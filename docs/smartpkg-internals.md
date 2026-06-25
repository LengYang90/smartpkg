# smartpkg 技术实现详解

> 本文档从架构和实现细节层面解释 smartpkg 每一项功能是如何工作的，以及为什么这样设计。

---

## 目录

1. [整体架构](#1-整体架构)
2. [包来源智能识别](#2-包来源智能识别)
3. [CRAN 镜像探测引擎](#3-cran-镜像探测引擎)
4. [Bioconductor 镜像探测](#4-bioconductor-镜像探测)
5. [24 小时缓存机制](#5-24-小时缓存机制)
6. [多来源安装路由](#6-多来源安装路由)
7. [CRAN→Bioc 自动降级](#7-cranbioc-自动降级)
8. [测试策略](#8-测试策略)

---

## 1. 整体架构

smartpkg 采用**三层架构**，每层职责清晰，层与层之间通过明确的函数接口通信：

```
┌──────────────────────────────────────────────────┐
│                   用户接口层                       │
│  smart_install(pkg, ...)                         │
│  refresh_mirror_cache()                          │
├──────────────────────────────────────────────────┤
│                   路由调度层                       │
│  detect_pkg_source() → 识别包来源                 │
│  warm_package_cache() → 预装 CRAN + Bioc 包索引   │
│  .smartpkg_cache$cran_pkgs / $bioc_pkgs          │
│  CRAN→Bioc 自动降级逻辑                          │
│  参数透传至安装后端                               │
├──────────────────────────────────────────────────┤
│                   镜像探测层                       │
│  get_mirror_list() / get_bioc_mirror_list()      │
│  probe_mirrors_curl_multi()  ← 并发探测核心      │
│  verify_candidates_concurrent() ← 下载验证       │
│  write_cache() / read_cache() / is_cache_valid() │
└──────────────────────────────────────────────────┘
```

**为什么这么分层？**

- **替换的代价**：用户只需要知道一个函数 `smart_install()`，三个原始安装入口 `install.packages()` / `BiocManager::install()` / `remotes::install_github()` 的内部差异被路由层屏蔽。
- **可测试性**：探测引擎可以独立于路由逻辑测试；缓存可以脱离网络测试。
- **可扩展性**：增加新的包来源（如 GitLab）只需在来源识别层加一条规则、在路由层加一个 backend 函数，不需要动探测引擎。

---

## 2. 包来源智能识别

### 功能

根据用户输入的字符串，自动判断包属于哪种来源：CRAN、Bioconductor、GitHub、本地包、或未知。

### 实现代码（`R/source_detect.R`）

```r
detect_pkg_source <- function(pkg) {
  # 1. 空值检查
  if (is.null(pkg) || is.na(pkg) || nchar(trimws(pkg)) == 0) {
    stop("Package name is empty")
  }
  pkg <- trimws(pkg)

  # 2. 检查显式命名空间 (CRAN::, Bioc::, GitHub::)
  if (grepl("::", pkg, fixed = TRUE)) {
    parts <- strsplit(pkg, "::", fixed = TRUE)[[1]]
    namespace <- tolower(trimws(parts[1]))
    pkg_name <- if (length(parts) >= 2) trimws(parts[2]) else ""
    # ... 路由到对应的 namespace
  }

  # 3. 检查本地路径
  if (grepl("\\.tar\\.gz$", pkg) || file.exists(pkg)) {
    return(list(source = "local", pkg = pkg))
  }

  # 4. 检查 GitHub 格式 (username/repo)
  if (grepl("^[A-Za-z0-9._-]+/[A-Za-z0-9._-]+$", pkg)) {
    return(parse_github(pkg))
  }

  # 5. 纯包名 → 默认走 CRAN
  list(source = "cran", pkg = pkg)
}
```

### 判断优先级

```
输入字符串
  │
  ├── 空/NA → ❌ 报错
  │
  ├── 包含 "::" → 显式命名空间
  │     ├── "CRAN::pkg" → CRAN
  │     ├── "Bioc::pkg" → Bioconductor
  │     ├── "GitHub::user/repo" → GitHub
  │     └── "其他::pkg" → unknown
  │
  ├── .tar.gz 结尾 或 本地文件/目录存在 → local
  │
  ├── "username/repo" 格式 → GitHub
  │     └── parse_github 解析出 username 和 repo
  │
  └── 纯文本 → CRAN（默认）
```

### 关键设计决策

**为什么先检查 `::` 再检查格式，而不是反过来？**
显式命名空间是最明确的信号，不依赖模式匹配的启发式判断。`Bioc::limma` 明确告诉系统这是 Bioc 包，不需要猜测。纯包名的启发式判断放在最后作为默认值。

**为什么 `strsplit("CRAN::", "::")` 在 R 中只返回一个元素？**
R 的 `strsplit()` 默认不保留尾部分隔符后的空字符串，所以 `strsplit("CRAN::", "::")[[1]]` 返回 `["CRAN"]` 而非 `["CRAN", ""]`。如果直接取 `parts[2]` 会得到 `NA`。修复：加上 `if (length(parts) >= 2)` 判断。

**为什么 GitHub 路径支持多级（`org/team/repo`）？**
部分 GitHub 仓库在子路径下，`paste(parts[-1], collapse = "/")` 确保所有 `/` 之后的内容都被视为 repo 名称。

---

## 3. CRAN 镜像探测引擎

### 功能

从 CRAN 官方约 96 个镜像中，自动选择响应速度最快的镜像，用于后续的 `install.packages()`。

### 两步探测法

```
第一步：并发 HEAD 探测（所有镜像）
    │
    ├── 对 96 个镜像同时发送 HTTP HEAD 请求
    ├── 超时 3 秒
    ├── 取响应时间最短的 TOP 10
    │
    └── 第二步：真实下载验证（TOP 10）
        │
        ├── 对候选的 10 个镜像同时下载 index.html
        ├── 超时 5 秒
        ├── 选下载速度最快的
        │
        └── 返回最快的镜像 URL
```

### 核心代码（`R/mirror_detect.R`）

```r
get_fastest_mirror <- function(mirrors, top_n = 10) {
  urls <- mirrors$URL

  # 第一步：并发 HEAD 探测
  probe_results <- probe_mirrors_concurrent(urls)
  probe_results <- probe_results[is.finite(probe_results$response_time), ]
  probe_results <- probe_results[order(probe_results$response_time), ]

  if (nrow(probe_results) == 0) {
    return("https://cloud.r-project.org")  # ← 全部失败时的逃生舱
  }

  candidates <- head(probe_results, min(top_n, nrow(probe_results)))

  # 第二步：真实下载验证
  if (nrow(candidates) > 1) {
    candidates <- verify_candidates_concurrent(candidates, timeout = 5)
    candidates <- candidates[is.finite(candidates$download_time), ]
    if (nrow(candidates) == 0) {
      return("https://cloud.r-project.org")
    }
    candidates <- candidates[order(candidates$download_time), ]
  }

  candidates$URL[1]
}
```

### 为什么用两步而不是一步？

- **只做 HEAD 请求**：约 1KB 的网络传输，96 个镜像并发 3 秒就能完成。如果直接对所有镜像做真实下载，带宽会成为瓶颈。
- **仅对 TOP 10 做下载验证**：HEAD 请求能测网络延迟但测不了实际吞吐量。有些镜像响应快但带宽低。两步法兼顾了**覆盖范围**和**准确性**。
- **并发探测是关键**：用 `curl::multi_run()` 而非串行 `vapply`，将 96×3s = 288s 降至约 3s。

### 并发探测的实现（`probe_mirrors_curl_multi`）

```r
probe_mirrors_curl_multi <- function(mirrors, timeout = 3) {
  pool <- curl::new_pool(total_con = 100, host_con = 6)
  results <- new.env(hash = FALSE, parent = emptyenv())

  for (i in seq_along(mirrors)) {
    url <- gsub("/?$", "/", mirrors[i])
    h <- curl::new_handle()
    curl::handle_setopt(h,
      customrequest = "HEAD", nobody = TRUE,
      timeout_ms = timeout * 1000, connecttimeout_ms = timeout * 1000
    )

    # 用 local() 捕获 i 的当前值，避免 R 闭包陷阱
    local({
      idx <- i
      curl::curl_fetch_multi(url, handle = h, pool = pool,
        done = function(resp) {
          assign(as.character(idx), as.numeric(resp$times[["total"]]),
                 envir = results)
        },
        fail = function(msg) {
          assign(as.character(idx), Inf, envir = results)
        }
      )
    })
  }

  curl::multi_run(pool = pool)
  # ...
}
```

**关键细节**：

1. **`curl::new_pool(total_con = 100)`** — 允许最多 100 个并发连接，覆盖全部镜像。
2. **`local({ idx <- i; ...})`** — R 的 `for` 循环不创建新的作用域，闭包中引用 `i` 会捕获循环结束后的最终值。用 `local()` 为每次迭代创建一个新环境，将 `i` 的当前值绑定到 `idx`。
3. **`assign(as.character(idx), ..., envir = results)`** — 将每个镜像的探测结果写入共享环境。用字符索引避免 R 环境的数字索引歧义。
4. **`curl::handle_setopt(h, customrequest = "HEAD", nobody = TRUE)`** — 只获取 HTTP 头，不下载正文，最大化探测速度。

### 为什么选择 curl 而非其他 HTTP 工具？

| 方式 | 并发性 | R 版本依赖 | 性能 |
|------|--------|-----------|------|
| `curl::multi_run()` | ✅ 真正并发 | R 3.0+ | 3s 完成 96 个请求 |
| `parallel::mclapply` | ⚠️ 仅 Unix | R 2.14+ | 进程开销大 |
| `httr::GET()` | ❌ 串行 | R 3.0+ | 96×3s = 288s |
| `utils::download.file()` | ❌ 串行 | R 内置 | 96×3s = 288s |

`curl` 包的 multi 接口是 R 生态中最成熟的异步 HTTP 方案。作为 Suggests 依赖，没有 `curl` 时自动降级为串行探测（虽然慢，但功能不受损）。

### 逃生舱机制

所有镜像都失败时（比如完全断网），回退到 `https://cloud.r-project.org` — RStudio 的 CDN 镜像，全球可用且可靠性最高。这是最低保障线。

---

## 4. Bioconductor 镜像探测

### 功能

从 ~5 个已知的 Bioconductor 镜像中选择最快的，用于 `BiocManager::install()` 中的 Bioc 包下载。

### 实现代码

```r
known_bioc_mirrors <- c(
  "https://bioconductor.org",                          # Main (US)
  "https://mirrors.ustc.edu.cn/bioc/",                  # USTC (China)
  "https://mirrors.tuna.tsinghua.edu.cn/bioconductor/", # Tsinghua (China)
  "https://bioconductor.statistik.tu-dortmund.de/",     # Dortmund (Germany)
  "https://bioconductor.mirrors.ustc.edu.cn/"           # USTC alt (China)
]

get_bioc_mirror_list <- function() {
  # 尝试从 BiocManager 获取完整镜像表
  if (requireNamespace("BiocManager", quietly = TRUE)) {
    mirror_table <- tryCatch(
      BiocManager:::.getMirrorTable(),
      error = function(e) NULL
    )
    if (is.data.frame(mirror_table) && "URL" %in% names(mirror_table)) {
      return(mirror_table$URL)
    }
  }
  known_bioc_mirrors  # fallback
}
```

### 为什么 Bioc 只有单步探测？

Bioc 镜像数量少（5 个左右），数量级远小于 CRAN 的 96 个。少到不需要先筛选再验证——直接并行 HEAD 探测，耗时约 2 秒，精度已经足够。两步法的额外复杂度不值得。

### 为什么 Bioc 镜像列表是硬编码的？

- BiocManager 官方没有提供便捷的镜像列表 API（`:.getMirrorTable()` 是内部函数，不保证稳定）。
- 全球 Bioconductor 镜像站点数量很少且稳定（长期 5-6 个），不像 CRAN 那样频繁变动。
- 维护成本低：即使长期不更新，主站 `https://bioconductor.org` 始终可用。

### Bioconductor 镜像在安装中的角色

`BiocManager::install()` 内部使用 `getOption("BioC_mirror")` 构建 Bioc 仓库 URL。smartpkg 在调用前设置：

```r
options(BioC_mirror = detect_fastest_bioc_mirror())
```

同时设置 CRAN 镜像用于安装 CRAN 依赖：

```r
options(repos = c(CRAN = detect_fastest_mirror()))
```

两个镜像的设置被包裹在 `old/new + on.exit` 中，确保调用结束后恢复用户原有的配置。

---

## 5. 两层缓存机制

smartpkg 有两层独立缓存，解决不同维度的问题：

### 5.1 文件级缓存（跨会话）

将镜像探测结果（最快 CRAN URL + 最快 Bioc URL）缓存到本地文件，
避免每次启动 R 都重新探测所有镜像。

### 实现代码（`R/cache.R`）

```r
CACHE_TTL <- 86400  # 24 小时（秒）

write_cache <- function(mirror_data) {
  saveRDS(mirror_data, file = cache_path())
}

read_cache <- function() {
  if (file.exists(cache_path())) readRDS(cache_path()) else NULL
}

is_cache_valid <- function() {
  cached <- read_cache()
  if (is.null(cached)) return(FALSE)
  if (is.null(cached$timestamp)) return(FALSE)
  elapsed <- difftime(Sys.time(), cached$timestamp, units = "secs")
  as.numeric(elapsed) < CACHE_TTL
}
```

### 缓存文件格式

```r
list(
  mirror_url = "https://mirrors.sustech.edu.cn/CRAN/",  # CRAN 镜像
  bioc_mirror_url = "https://mirrors.ustc.edu.cn/bioc",  # Bioc 镜像
  timestamp = "2026-06-25 14:26:56 UTC",                  # 探测时间
  all_mirrors_tested = 96,                                # 探测的镜像总数
  candidate_count = 10                                     # 候选镜像数
)
```

### 为什么选择文件缓存而非 R 选项/环境变量？

- **进程持久性**：R 选项和环境变量在 R 会话结束后消失。文件缓存跨会话、跨重启持续有效。
- **位置**：`~/.R/smartpkg_mirror_cache` — 在用户主目录的 `.R` 目录下，遵循 R 生态惯例（类似 `.Rprofile`、`.Rhistory`）。
- **格式**：`saveRDS()` / `readRDS()` — R 原生序列化，无需额外依赖，支持任意 R 数据结构。
- **为什么是 24 小时？** 镜像的响应速度变化不频繁（除非用户跨区域移动或镜像下线），24 小时足够覆盖日常使用，同时在网络环境变化时不会让用户等待太久。

### 缓存一致性

CRAN 和 Bioc 镜像共享同一个缓存文件、同一个时间戳。当 `is_cache_valid()` 返回 `FALSE` 时，两个镜像都会重新探测：

```
┌─────────┐     ┌─────────────────┐
│ 缓存有效 │ YES │ 读取缓存的镜像 URL │
│  ？      │────→│ (CRAN + Bioc)    │
└─────────┘     └─────────────────┘
    │ NO
    ▼
┌─────────────────┐
│ 重新探测两个镜像  │
│ （各自独立探测） │
└─────────────────┘
```

这种设计简化了逻辑：不需要为两个镜像分别维护过期时间。代价是 Bioc 镜像会随 CRAN 镜像一起过期，但 Bioc 探测仅需 2 秒，可以接受。

### 5.2 会话级包索引缓存（R 会话内）

首次调用 `smart_install()` 时，将 CRAN 和 Bioconductor 的**全量包名列表**加载到内存中，
后续判断包属于哪个源时直接查内存表，无需网络请求。

#### 实现代码（`R/smart_install.R`）

```r
.smartpkg_cache <- new.env(parent = emptyenv())

warm_package_cache <- function(mirror) {
  if (isTRUE(.smartpkg_cache$warmed)) return()

  # 预加载 CRAN 包索引（24,000+ 个包）
  contrib <- utils::contrib.url(mirror)
  all <- utils::available.packages(contriburl = contrib)
  .smartpkg_cache$cran_pkgs <- rownames(all)

  # 预加载 Bioc 包索引（27,000+ 个包）
  bioc_mirror <- detect_fastest_bioc_mirror()
  options(BioC_mirror = bioc_mirror)
  repos <- BiocManager::repositories()
  all_bioc <- character(0)
  for (r in repos) {
    found <- rownames(available.packages(contriburl = contrib.url(r)))
    all_bioc <- c(all_bioc, found)
  }
  .smartpkg_cache$bioc_pkgs <- unique(all_bioc)

  .smartpkg_cache$warmed <- TRUE
}
```

#### 包判断逻辑

```r
# 不联网，纯内存查表，≈0.001s
on_cran <- info$pkg %in% .smartpkg_cache$cran_pkgs
on_bioc <- info$pkg %in% .smartpkg_cache$bioc_pkgs
```

| 包 | ∈ CRAN | ∈ Bioc | 结论 |
|----|--------|--------|------|
| `dplyr` | ✅ | ✅ | CRAN（优先） |
| `ggplot2` | ✅ | ✅ | CRAN（优先） |
| `limma` | ❌ | ✅ | Bioc |
| `clusterProfiler` | ❌ | ✅ | Bioc |
| `nonexistent` | ❌ | ❌ | 报错 |

#### 为什么需要两层缓存？

- **文件缓存**解决镜像探测的跨会话复用（每 24 小时一次全量探测）
- **会话缓存**解决包来源判断的即时性（首次 ~7s 加载，后续 0.002s 判断）

`BiocManager::available(pkg)` 每次调用都独立下载 Bioc PACKAGES.gz（在中国连美国服务器约 6s）。
通过一次遍历所有 Bioc 子仓库并存入 `.smartpkg_cache$bioc_pkgs`，将 O(n) 网络请求降为 O(1) 内存查询。

---

## 6. 多来源安装路由

### 功能

根据来源识别结果，将安装请求路由到正确的安装后端。

### 实现代码（`R/smart_install.R`）

```r
smart_install <- function(pkg, ..., dry_run = FALSE) {
  info <- detect_pkg_source(pkg)
  args <- list(...)

  # 纯包名 + 实际安装：CRAN → Bioc 自动 fallback（见第 7 节）
  if (info$source == "cran" && !dry_run) { ... }

  # dry_run 或显式命名空间：直接路由
  switch(info$source,
    cran = install_cran(info$pkg, args, dry_run),
    bioc = install_bioc(info$pkg, args, dry_run),
    github = install_github(info$pkg, info$username, info$repo, args, dry_run),
    local = install_local(info$pkg, args, dry_run),
    stop("Unknown package source: ", info$source)
  )
}
```

### 各后端使用的安装命令

| 来源 | dry_run 返回 | 实际安装命令 | 镜像策略 |
|------|-------------|------------|---------|
| CRAN | 列出计划 | `install.packages(repos = 最快CRAN)` | 最快 CRAN 镜像 |
| Bioc | 列出计划 | `options(BioC_mirror=最快Bioc) + BiocManager::repositories() + repos["CRAN"]=最快CRAN` + `BiocManager::install()` | 双镜像，用 `BiocManager::repositories()` 构建完整仓库列表 |
| GitHub | 列出计划 | `remotes::install_github()` | 不需要镜像 |
| 本地 | 列出计划 | `install.packages(repos=NULL, type="source")` | 不需要镜像 |

### Bioc 安装的 repos 策略（重要）

初始实现用 `options(repos = c(CRAN = cran_mirror))`，但这会**覆盖** BiocManager 自行注册的
Bioconductor 各子仓库（BioCsoft、BioCann、BioCexp 等），导致 `BiocManager::install()` 回退到
`bioconductor.org` 下载 Bioc 包，忽略了我们探测到的最快 Bioc 镜像。

修复方案：先通过 `BiocManager::repositories()` 构建完整仓库列表（使用已设置的 `BioC_mirror` 选项），
再只替换其中的 CRAN 条目：

```r
options(BioC_mirror = detect_fastest_bioc_mirror())
repos <- BiocManager::repositories()  # ← 包含所有 Bioc 子仓库
repos["CRAN"] <- detect_fastest_mirror()  # ← 只替换 CRAN 条目
options(repos = repos)
```

此外，会话缓存预热时也会全局设置 `options(BioC_mirror)`，该设置在整个 R 会话中持续生效，
这样即使用户直接调用 `BiocManager::install()` 不走 smartpkg，也会从最快 Bioc 镜像下载。

### dry_run 机制

`dry_run = TRUE` 返回一个 list，包含安装计划的所有信息，但**不实际执行安装**。这是为测试设计的：

```r
smart_install("dplyr", dry_run = TRUE)
# → list(source = "cran", pkg = "dplyr",
#        mirror = "https://...", backend = "install.packages",
#        args = list())
```

测试时用 `dry_run = TRUE` 验证路由逻辑，用 `dry_run = FALSE`（默认）做实际安装。这样单元测试不需要网络和 sudo 权限。

### 参数透传

`...` 参数通过 `do.call()` 透传给后端：

```r
smart_install("dplyr", dependencies = TRUE, quiet = TRUE)
# → install.packages("dplyr", repos = "https://...",
#                     dependencies = TRUE, quiet = TRUE)
```

---

## 7. CRAN→Bioc 自动降级

### 功能

用户输入纯包名（如 `smart_install("clusterProfiler")` 或 `smart_install("limma")`）时，
自动判断该包属于 CRAN 还是 Bioconductor，路由到正确的安装后端。CRAN 优先，Bioc 作为降级。

### 实现代码（`R/smart_install.R`）

```r
if (info$source == "cran" && !dry_run) {
  mirror <- detect_fastest_mirror()

  # 会话级预热：首次加载 CRAN + Bioc 全量包名（约 7s，之后秒回）
  warm_package_cache(mirror)

  # 查内存表判断归属（≈0.001s，无需网络）
  on_cran <- info$pkg %in% .smartpkg_cache$cran_pkgs

  if (on_cran) {
    return(invisible(install_cran(info$pkg, args, dry_run = FALSE)))
  }

  # CRAN 上不存在 → 查 Bioc 内存表
  if (info$pkg %in% .smartpkg_cache$bioc_pkgs) {
    message("Not found on CRAN, installing from Bioconductor instead...")
    return(invisible(install_bioc(info$pkg, args, dry_run = FALSE)))
  }

  # 两个源都没有
  stop("Package '", info$pkg, "' is not available on CRAN or Bioconductor.")
}
```

### 为什么不用 tryCatch 捕获 `install.packages()` 的错误？

`install.packages()` 对不存在的包只发出 warning（"not available"），**不抛 error**。
`tryCatch(error = ...)` 根本捕获不到。早期实现尝试了三种方案：

| 方案 | 问题 | 结论 |
|------|------|------|
| `tryCatch` 捕获 error | `install.packages()` 只 warning 不 error | ❌ 无法触发 |
| HEAD 请求检查 web 页面 | 归档包（`limma`）返回 200（页面存在但不是当前版本） | ❌ 误判 |
| HEAD + HTML 前 15 行检查 removed | 需连 `cloud.r-project.org`（美国），中国用户慢 | ❌ 跨网慢 |

最终方案：**用会话缓存代替运行时判断**——预加载全量包列表到内存，查表判断。

### 为什么 dry_run 不做 fallback？

`dry_run` 的目的是**不联网、不下载**地快速验证路由逻辑。做 fallback 需要加载 CRAN + Bioc 的 PACKAGES 索引（~7s），
违背了 dry_run 的设计目标。所以 dry_run 对纯包名始终返回 source="cran"。

### 为什么不在 BiocManager::install() 之后再备降？

BiocManager 自身已经能处理自己的包。对于纯包名的 CRAN→Bioc 降级，我们只做一次判断：
是 CRAN 包就走 CRAN 安装，是 Bioc 包就走 Bioc 安装。不需要链式降级（再降 GitHub 等），
因为纯包名的语义本身就是"请自动找到这个包"——我们只在 CRAN 和 Bioc 之间自动选择。

### 会话缓存的性能

| 阶段 | 耗时 | 说明 |
|------|------|------|
| 首次预热（下载 PACKAGES.gz） | ~7 秒 | CRAN 24,061 包 + Bioc 27,844 包 |
| 后续每个包判断 | **~0.002 秒** | 纯内存字符串 %in% 查表 |
| 5 个包连续判断 | **~0.004 秒** | 全部缓存命中 |

Bioc 缓存的重要性：`BiocManager::available(pkg)` 每次调用都独立下载 Bioc 的 PACKAGES.gz，
在中国从 `bioconductor.org`（美国）下载需要约 6 秒。通过一次遍历所有 Bioc 子仓库
(BioCsoft / BioCann / BioCexp / BioCworkflows) 并存入内存表，将单次 6s 降为 0.001s。

---

## 8. 测试策略

### 测试文件结构

```
tests/testthat/
├── test-cache.R           # 缓存读写、有效期、过期、刷新
├── test-source_detect.R   # 来源识别所有格式和边界
├── test-mirror_detect.R   # 镜像探测（含真实网络探测）
├── test-smart_install.R   # 路由逻辑、参数透传、错误处理
└── test-integration.R     # 全量集成测试（64+ 断言）
```

### 测试覆盖

| 模块 | 测试数 | 测试内容 |
|------|--------|---------|
| 缓存 | 8 | 读写回环、有效/过期/无缓存判断、手动刷新、无缓存时刷新 |
| 来源识别 | 21 | CRAN/Bioc/GitHub/本地/未知、大小写、空格、空值、特殊字符、多级路径 |
| 镜像探测 | 8 | 镜像列表获取、单个探测、并发探测、两步选择、缓存命中、fallback |
| smart_install | 16 | 四种来源路由、dry_run、参数透传、错误处理 |
| 集成测试 | 64+ | 全部模块联合测试、端到端流程、边界情况、CRAN→Bioc fallback |

### 测试设计原则

1. **dry_run 隔离网络依赖**：不需要实际安装或网络访问就能验证路由逻辑。
2. **预埋缓存加速**：镜像探测测试前预埋缓存，避免每次运行都做真实的 5 秒网络探测。
3. **预期警告**：不可达镜像会触发正常警告（graceful degradation），测试中标记为预期。
4. **skip_if_not_installed**：需要 BiocManager 或 remotes 的测试用 `skip_if_not_installed()` 保护。

### 已知的测试耗时

| 测试 | 耗时 | 原因 |
|------|------|------|
| `test-cache.R` | < 1s | 纯本地文件操作 |
| `test-source_detect.R` | < 1s | 纯字符串解析 |
| `test-smart_install.R` | < 1s | 缓存命中时无网络请求 |
| `test-mirror_detect.R`（测 6） | ~10s | 真实全量镜像探测 |
| `test-integration.R` | 含网络探测测试时 ~10s | 镜像探测 + CRAN/Bioc 检查 |
| 会话缓存预热 | ~7s | 下载 CRAN + Bioc 全量 PACKAGES（仅首次） |
| 缓存命中后包判断 | **~0.002s** | 纯内存 %in% 查表 |

---

## 附录：关键性能数据

| 操作 | 优化前 | 优化后 | 加速比 |
|------|--------|--------|--------|
| CRAN 镜像探测（96 个） | 288s（串行） | 5s（并发） | **57×** |
| Bioc 镜像探测（5 个） | 15s（串行） | 2s（并发） | **7.5×** |
| CRAN 包存在判断（首次） | N/A | ~0.5s（从最快镜像下载 PACKAGES.gz） | - |
| CRAN 包存在判断（后续） | N/A | **~0.001s**（内存查表） | - |
| Bioc 包存在判断（每次 BiocManager::available） | ~6s（连美国服务器） | **~0.001s**（内存查表） | **6000×** |
| 首次 `smart_install()` | 5 分钟 | ~10 秒（镜像探测）+ ~7 秒（包索引加载） | **20×** |

## 附录：依赖关系图

```
smartpkg
├── Imports: utils (R 内置)
├── Suggests:
│   ├── curl         → 并发 HTTP 探测（有则加速，无则降级）
│   ├── testthat     → 单元测试
│   ├── BiocManager  → Bioconductor 安装（按需安装）
│   └── remotes      → GitHub 安装（按需安装）
```

核心功能仅依赖 `utils`（R 基础包）。`curl` 提供加速但非必需。`BiocManager` 和 `remotes` 仅在安装对应来源的包时才需要，不会影响 `smart_install()` 的基本可用性。
