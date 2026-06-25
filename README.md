# smartpkg

> Smart R Package Installer — 自动选择最快的 CRAN 镜像，支持 CRAN / Bioconductor / GitHub。

`smartpkg` 提供 `smart_install()` 函数，统一替代 `install.packages()`、`BiocManager::install()` 和 `remotes::install_github()`，核心能力：

- 🚀 **自动选最快镜像** — 两步探测法：并发 HEAD 测试 + 真实下载验证，结果缓存 24 小时
- 🧠 **智能来源识别** — 自动判断包来自 CRAN / Bioconductor / GitHub / 本地
- 🌍 **全球通用** — 不限地区，任何国家的用户都能自动找到最快的镜像
- 🔌 **即装即用** — 安装后无需任何配置，`smart_install()` 直接使用

## 安装

```r
# 从 GitHub 安装 smartpkg
remotes::install_github("yourusername/smartpkg")
```

## 使用

```r
library(smartpkg)

# CRAN 包 — 自动选最快镜像
smart_install("dplyr")

# Bioconductor 包
smart_install("Bioc::limma")

# GitHub 包
smart_install("tidyverse/dplyr")

# 本地包
smart_install("./mypkg_1.0.tar.gz")

# 传递额外参数
smart_install("ggplot2", dependencies = TRUE, quiet = TRUE)

# 手动刷新镜像缓存
refresh_mirror_cache()
```

## 工作原理

1. **第一步**：获取 CRAN 官方镜像列表（约 100+ 镜像）
2. **第二步**：对所有镜像并发发送 HEAD 请求，超时 3 秒，取最快 10 个
3. **第三步**：对这 10 个镜像下载极小文件测速，选最快的
4. **第四步**：结果缓存到 `~/.R/smartpkg_mirror_cache`，有效期 24 小时

## 依赖

- CRAN 安装：`install.packages()`（R 内置）
- Bioconductor：需要 `BiocManager` 包（首次使用时提示安装）
- GitHub：需要 `remotes` 包（首次使用时提示安装）
