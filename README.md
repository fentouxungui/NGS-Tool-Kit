# NGSToolKit: RNA-Seq Analysis Platform and JBrowse Visualization

[![CRAN status](https.www.r-pkg.org/badges/version/NGSToolKit)](https://cran.r-project.org/package=NGSToolKit)
[![License: GPL v3](https://img.shields.org/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

`NGSToolKit` is an R Shiny application for comprehensive RNA-Seq data exploration, quality control, differential expression analysis (DESeq2), clustering, dimensionality reduction (t-SNE, UMAP, PCA), and integrated genomic visualization using JBrowse 2.

## Installation

You can install `NGSToolKit` directly from CRAN or GitHub:

```r
# Install from CRAN (once published):
install.packages("NGSToolKit")

# Or install development version from GitHub:
# install.packages("remotes")
remotes::install_github("bigfacilityiisr/NGS-Tool-Kit")
```

## Quick Start

Launch the application using:

```r
library(NGSToolKit)

# Launch the Shiny interface in your default browser:
run_ngs_toolkit()
```

## Features

- **Data QC & Exploration**: Summary statistics, library sizes, and count distributions.
- **Differential Expression**: Powered by DESeq2 with customizable thresholds, volcano plots, and MA plots.
- **Dimensionality Reduction**: PCA, t-SNE, and UMAP interactive visual projections.
- **Clustering & Heatmaps**: Hierarchical clustering, correlation matrices, and custom heatmaps.
- **Integrated JBrowse 2**: Interactive genome browser supporting FASTA, GFF3, BAM, and VCF files.

## Authors & Institution

- **ICAR – Indian Institute of Spices Research**, Kozhikode, Kerala, India
- **Contact**: sonacharlesicar@gmail.com, nabeelfebin@gmail.com
