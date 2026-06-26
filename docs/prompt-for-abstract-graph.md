# Prompt: smartpkg R 包架构图

将以下 prompt 输入 GPT-4（或其他支持画图的 AI 工具）生成 smartpkg 的架构示意图。

---

## 英文版 Prompt

```
Create an architectural diagram for "smartpkg", an R package that provides a unified package installer.

The diagram should show:

**Center**: A large box labeled "smart_install()" — this is the single entry point that replaces install.packages(), BiocManager::install(), and remotes::install_github().

**Left side (Input)**: Four input types flowing into smart_install():
1. "dplyr" (plain name → auto-detected as CRAN)
2. "clusterProfiler" (plain name → auto-detected as Bioconductor when CRAN not found)
3. "tidyverse/dplyr" (username/repo → GitHub)
4. "Bioc::limma" (explicit namespace → Bioconductor)

**Below Center (Internal mechanism)**: Three sub-sections:
1. "Source Detection" — identifies whether input is CRAN, Bioc, GitHub, or local
2. "Mirror Selection" — probes all CRAN mirrors (~96) concurrently with HEAD requests, then verifies top 10 with real downloads. Also detects fastest Bioconductor mirror (~5 mirrors)
3. "Session Cache" — warms up once per R session, stores 24,000+ CRAN package names and 27,000+ Bioc package names in memory for instant lookup

**Right side (Output/Backend)**: Three installation backends branching out:
1. "install.packages(repos = fastest_CRAN)" — for CRAN packages
2. "BiocManager::install()" — for Bioconductor packages (uses both fastest CRAN and fastest Bioc mirrors)
3. "remotes::install_github()" — for GitHub packages

**Bottom**:
1. A "24h File Cache" box that stores mirror probe results (~/.R/smartpkg_mirror_cache), shared across R sessions.
2. A "refresh_mirror_cache()" button/trigger pointing at the cache, representing the manual cache-clearing function.

Style: Clean, modern, with a blue/teal color scheme. Use icons if possible (R logo, database icon, server icon, package icon). The diagram should be readable at 1200px wide. Use arrows to show data flow direction.
```

## 中文版 Prompt

```
为 "smartpkg" R 包绘制一张架构示意图。

smartpkg 提供 smart_install() 一个函数，统一替代 install.packages()、BiocManager::install() 和 remotes::install_github()。

图中应包含以下元素：

**中心**：一个大框 "smart_install()" — 唯一的用户入口

**左侧（输入）**：四种输入流向 smart_install()：
1. "dplyr"（纯包名 → 自动识别为 CRAN）
2. "clusterProfiler"（纯包名 → CRAN 找不到时自动走 Bioconductor）
3. "tidyverse/dplyr"（username/repo → GitHub）
4. "Bioc::limma"（显式命名空间 → Bioconductor）

**中心下方（内部机制）**：三个子模块：
1. "来源识别" — 识别输入属于 CRAN / Bioc / GitHub / 本地
2. "镜像选择" — 并发 HEAD 探测约 96 个 CRAN 镜像，取最快 10 个做真实下载验证；同时探测约 5 个 Bioc 镜像
3. "会话缓存" — 首次调用时预热，内存中缓存 24000+ CRAN 包名和 27000+ Bioc 包名，后续判断秒回

**右侧（输出/后端）**：三个安装后端分支：
1. "install.packages(最快CRAN镜像)" — CRAN 包
2. "BiocManager::install(最快CRAN + 最快Bioc镜像)" — Bioc 包
3. "remotes::install_github()" — GitHub 包

**底部**：
1. "24小时文件缓存" — 镜像探测结果存储在 ~/.R/smartpkg_mirror_cache，跨 R 会话共享
2. "refresh_mirror_cache()" — 清除缓存按钮/触发点，指向文件缓存，手动触发重新探测

风格：简洁现代，蓝/青色系。使用箭头表示数据流向。宽度约 1200px。
```

---

将上面的 prompt 发给 GPT-4 或其他支持 DALL-E / Mermaid / Draw.io 的工具即可生成。
