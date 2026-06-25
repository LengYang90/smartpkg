# smartpkg: 智能 R 包安装工具设计文档

## 概述

`smartpkg` 是一个 R 包，提供 `smart_install()` 函数**统一替代** `install.packages()`、`BiocManager::install()` 和 `remotes::install_github()` 三个安装入口，自动为 R 包安装选择最快的 CRAN 镜像，并智能识别包来源路由到对应的安装后端（CRAN / Bioconductor / GitHub），全程无需用户手动选择镜像或设置 country。

## 设计目标

1. **全球通用** — 不限于中国，任何国家的用户都能自动找到最快的镜像
2. **无感体验** — 无需设置 country、无需手动选镜像，即装即用
3. **多来源支持** — 自动识别并路由 CRAN / Bioconductor / GitHub 包到正确的安装后端
4. **缓存加速** — 镜像测速结果缓存 24 小时，避免每次安装都重新探测
5. **零核心依赖** — 包的核心功能只依赖 R 基础包

## 架构设计

```
┌─────────────────────────────────────────┐
│           用户接口层                      │
│   smart_install()                       │
│   refresh_mirror_cache()                │
├─────────────────────────────────────────┤
│           镜像调度层                      │
│   ① 读缓存 → ② 无缓存/过期 → ③ 探测     │
│   ④ 选最快 → ⑤ 路由到安装后端            │
├─────────────────────────────────────────┤
│           探测引擎层                      │
│   镜像列表获取 → 并发 HEAD 测速 →        │
│   候选集筛选 → 真实验证测速 → 写入缓存    │
├─────────────────────────────────────────┤
│           安装后端层                      │
│   CRAN → install.packages()             │
│   Bioc → BiocManager::install()         │
│   GitHub → remotes::install_github()    │
└─────────────────────────────────────────┘
```

## 智能来源识别逻辑

`smart_install()` 接收一个包名称字符串，自动判断来源：

```
输入字符串
    │
    ├── 包含 "::" → 显式指定来源
    │     ├── "CRAN::dplyr" → 走 CRAN
    │     ├── "Bioc::limma" → 走 Bioconductor
    │     └── "GitHub::tidyverse/dplyr" → 走 GitHub
    │
    ├── 包含 "/" 且不是本地路径 → GitHub (username/repo)
    │     └── 不存在本地文件/目录 → GitHub
    │
    ├── 以 .tar.gz 结尾或本地路径 → 本地安装
    │
    └── 无特殊标记 → 自动检测
          ├── CRAN 有该包 → 走 CRAN（带最快镜像）
          ├── CRAN 没有 → 查 Bioconductor
          │     ├── Bioc 有 → 走 Bioc 安装
          │     └── 都没有 → 报错并建议检查包名
          └── 报错
```

## 探测引擎（两步探测法）

### 步骤 1：快速筛选（并发 HEAD 请求）

1. 调用 `getCRANmirrors()` 获取 CRAN 官方镜像列表（约 100+ 镜像）
2. 用 `curl` 多句柄或 `http::HEAD()` + `parallel` 对每个镜像发起并发 HTTP HEAD 请求
3. 超时设置为 3 秒
4. 取响应时间最短的 10 个镜像作为候选集

### 步骤 2：真实测速验证

1. 对候选的 10 个镜像，各下载一个极小文件（如 CRAN 上的 `1line` 包描述或 `index.html`）
2. 记录实际下载时间
3. 选择最快的镜像作为本会话的默认镜像

### 缓存机制

- 测速结果（最快镜像 URL + 探测时间戳）写入本地文件 `~/.R/smartpkg_mirror_cache`
- 缓存有效期：24 小时
- 过期后下次 `smart_install()` 自动触发重新探测
- 提供 `refresh_mirror_cache()` 函数供用户手动刷新

### 缓存文件格式（RDS）

```r
list(
  mirror_url = "https://cloud.r-project.org",
  timestamp = "2026-06-25 10:30:00 UTC",
  all_mirrors_tested = 120,
  candidate_count = 10
)
```

## 函数接口设计

### 主要函数

```r
#' 智能安装 R 包
#' @param pkg 包名，自动识别来源
#' @param ... 传递给安装后端的额外参数
#' @examples
#' smart_install("dplyr")                    # CRAN
#' smart_install("Bioc::limma")              # Bioconductor
#' smart_install("tidyverse/dplyr")          # GitHub
#' smart_install("./local/pkg_1.0.tar.gz")   # 本地
smart_install <- function(pkg, ...) {
    # 1. 识别来源
    # 2. 如果是 CRAN → 获取最快镜像
    # 3. 路由到对应的安装后端
    # 4. 传参并执行安装
}

#' 手动刷新镜像缓存
refresh_mirror_cache <- function() {
    # 强制重新探测并更新缓存
}
```

### 辅助函数（内部/导出可选）

```r
# 获取最快的 CRAN 镜像 URL
detect_fastest_mirror <- function() { ... }

# 探测所有镜像的响应时间
probe_mirrors <- function(mirrors, timeout = 3) { ... }

# 读取/写入缓存
read_cache <- function() { ... }
write_cache <- function(mirror_url) { ... }

# 识别包来源
detect_pkg_source <- function(pkg) { ... }
```

## 包结构

```
smartpkg/
├── DESCRIPTION
├── NAMESPACE
├── R/
│   ├── smart_install.R        # 主函数
│   ├── mirror_detect.R         # 探测引擎
│   ├── cache.R                 # 缓存读写
│   ├── source_detect.R         # 来源识别
│   └── utils.R                 # 工具函数
├── tests/
│   └── testthat/
│       └── ...
└── README.md
```

## NAMESPACE 依赖

```
Imports:
    utils（R 内置，getCRANmirrors, install.packages）
Suggests:
    BiocManager（Bioconductor 安装）
    remotes（GitHub 安装）
    curl（可选，更高效的并发 HTTP 请求）
```

## 错误处理策略

| 场景 | 行为 |
|------|------|
| 网络不可用 | 回退到 `cloud.r-project.org`（CDN，全球可用）作为默认镜像 |
| 测速全部超时 | 回退到 CDN，缓存一个失败标记，下次重试 |
| BiocManager 未安装 | 提示安装 `install.packages("BiocManager")` 后重试 |
| remotes 未安装 | 提示安装 `install.packages("remotes")` 后重试 |
| 包名在所有源都找不到 | 报错并给出检查建议 |

## 未来可扩展点

- [ ] 支持自定义镜像列表（环境变量或配置文件）
- [ ] 支持 `pak` 作为可选的安装后端
- [ ] 批量安装时复用已探测的镜像结果
- [ ] 更智能的镜像健康检测（失败自动切换到次优镜像）
- [ ] Windows/macOS/Linux 下的多平台测试
