# smartpkg

> Smart R Package Installer — 一个函数搞定 CRAN / Bioconductor / GitHub 包的安装，自动选最快镜像。

安装 R 包最烦人的就是选镜像、切源、翻来覆去配置。`smart_install()` 一个函数通吃所有来源，自动为你选择最快的 CRAN 镜像，Bioconductor 包也不必额外配置——纯包名自动识别，找不到 CRAN 就找 Bioc。

![smartpkg 架构图](man/figures/smartpkg-architecture.png)

```r
library(smartpkg)
```

## 支持的安装场景

CRAN 包——自动选择最快的镜像下载：

```r
smart_install("dplyr")
smart_install("ggplot2", dependencies = TRUE)
```

Bioconductor 包——纯包名即可，自动识别并路由：

```r
smart_install("limma")             # Automatic: CRAN miss -> Bioc
smart_install("clusterProfiler")   # Automatic: CRAN miss -> Bioc
smart_install("org.Hs.eg.db")      # Automatic: CRAN miss -> Bioc
smart_install("Bioc::limma")       # Explicitly use Bioc
```

GitHub 包——直接写 `username/repo`：

```r
smart_install("tidyverse/dplyr")
smart_install("GitHub::tidyverse/dplyr")
```

本地包——支持 `.tar.gz` 路径：

```r
smart_install("./mypkg_1.0.tar.gz")
```

## 工作原理

首次调用 `smart_install()` 时，程序会：

1. 从 CRAN 官方列表中自动探测响应最快的镜像（并发检测所有镜像，耗时约 5 秒）
2. 同时检测最快的 Bioconductor 镜像
3. 结果缓存到本地，**每 24 小时自动刷新一次**

后续安装直接走缓存镜像，无需重复探测。你也可以随时手动刷新缓存：

```r
refresh_mirror_cache()
```

## 安装 smartpkg

```r
remotes::install_github("yourusername/smartpkg")
```

安装后无需任何配置即可使用。
