# smartpkg

> Smart R Package Installer — Install CRAN, Bioconductor, and GitHub packages with one function. Automatically selects the fastest mirror.

Tired of configuring CRAN mirrors, switching repositories, and manually figuring out package sources? `smart_install()` handles everything in one call: it automatically detects the fastest CRAN mirror, intelligently routes plain package names to CRAN or Bioconductor, and supports GitHub and local packages out of the box.

![smartpkg architecture](man/figures/smartpkg-architecture.png)

```r
library(smartpkg)
```

## Usage

**CRAN packages** — automatically downloads from the fastest mirror:

```r
smart_install("dplyr")
smart_install("ggplot2", dependencies = TRUE)
```

**Bioconductor packages** — plain names work, auto-detected and routed:

```r
smart_install("limma")             # auto: not on CRAN → installs from Bioc
smart_install("clusterProfiler")   # auto: not on CRAN → installs from Bioc
smart_install("org.Hs.eg.db")     # auto: not on CRAN → installs from Bioc
smart_install("Bioc::limma")       # explicit Bioc namespace
```

Bioconductor installs are non-interactive by default, so batch installs do not
stop at `Update all/some/none? [a/s/n]`:

```r
smart_install("limma")             # ask = FALSE, update = FALSE by default
smart_install("limma", ask = TRUE) # Still allowed when you want prompts
```

**GitHub packages** — just use `username/repo`:

```r
smart_install("tidyverse/dplyr")
smart_install("GitHub::tidyverse/dplyr")
```

**Local packages** — supports `.tar.gz` paths:

```r
smart_install("./mypkg_1.0.tar.gz")
```

## How It Works

On the first call, `smart_install()`:

1. Probes all official CRAN mirrors concurrently to find the fastest one (~5 seconds)
2. Also detects the fastest Bioconductor mirror
3. Caches the results locally — **auto-refreshes every 24 hours**

Subsequent calls use the cached mirrors instantly. You can also manually refresh at any time:

```r
smart_cache_info()
refresh_mirror_cache()
```

## Installation

```r
remotes::install_github("yourusername/smartpkg")
```

No configuration needed after installation.

---

**中文文档**：[README.md](README.md)
