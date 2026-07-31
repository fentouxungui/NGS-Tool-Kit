#' Imports and Global Variables
#'
#' @import shiny
#' @import shinydashboard
#' @import shinycssloaders
#' @importFrom shinyjs useShinyjs hidden toggle show hide enable disable
#' @importFrom DT DTOutput renderDT datatable
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_col geom_jitter labs theme theme_bw element_text facet_wrap last_plot
#' @importFrom DESeq2 vst DESeqDataSetFromMatrix DESeq results
#' @importFrom SummarizedExperiment assay
#' @importFrom Rtsne Rtsne
#' @importFrom pheatmap pheatmap
#' @importFrom Rsamtools indexFa bgzip indexTabix
#' @importFrom grDevices dev.off png rainbow
#' @importFrom graphics abline barplot boxplot legend plot.new text
#' @importFrom stats complete.cases cor dist hclust kmeans median prcomp rect.hclust rnorm runif sd var
#' @importFrom utils head read.csv read.delim str write.csv
#' @noRd
NULL

# Quiet R CMD check warnings about ggplot2 unquoted column names
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "condition", "Expression", "Sample", "PC1", "PC2",
    "tSNE1", "tSNE2", "UMAP1", "UMAP2", "Gene", "log2FoldChange",
    "pvalue", "padj", "baseMean", "value", "variable"
  ))
}
