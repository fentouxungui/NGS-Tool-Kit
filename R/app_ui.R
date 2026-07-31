#' NGSToolKit Shiny UI
#' @return Shiny UI object
#' @noRd
app_ui <- function() {
  dashboardPage(
    dashboardHeader(
      title = tagList(
        icon("leaf", class = "fa-fw", style = "color: #34C759;"),
        span("NGS Tool Kit", style = "font-weight: 600; font-size: 18px;")
      ),
      titleWidth = 300,
      tags$li(class = "dropdown", style = "margin-right: 15px;",
              tags$span(style = "color: #8E8E93; font-size: 13px;",
                        icon("table"), " RNA-Seq Analysis Platform"))
    ),
    
    dashboardSidebar(
      width = 300,
      sidebarMenu(
        id = "tabs",
        menuItem("Home", tabName = "home", icon = icon("house")),
        menuItem("Data QC", tabName = "qc", icon = icon("check-square")),
        menuItem("Correlation", tabName = "corr", icon = icon("random")),
        menuItem("Clustering", tabName = "cluster"),
        menuItem("PCA", tabName = "pca", icon = icon("chart-line")),
        menuItem("Heatmap", tabName = "heatmap", icon = icon("th")),
        menuItem("Gene Expression", tabName = "gene"),
        menuItem("DE Analysis", tabName = "de", icon = icon("flask")),
        menuItem("Top_Genes", tabName = "topgenes"),
        menuItem("MA Plot", tabName = "ma"),
        menuItem("Volcano", tabName = "volcano", icon = icon("mountain")),
        menuItem("t-SNE", tabName = "tsne", icon = icon("braille")),
        menuItem("UMAP", tabName = "umap", icon = icon("project-diagram")),
        menuItem("JBrowse", tabName = "jbrowse", icon = icon("dna")),
        tags$hr(),
        div(
          style = "padding:15px; text-align:center; font-size:13px; line-height:1.8; color:#444;",
          tags$h4(icon("address-card"), " Contact"),
          tags$b("NGS Tool Kit"), br(),
          "ICAR - Indian Institute of Spices Research", br(),
          "Kozhikode, Kerala, India", br(), br(),
          tags$a(href = "mailto:sonacharlesicar@gmail.com", icon("envelope"), " sonacharlesicar@gmail.com", style = "color:#007AFF;text-decoration:none;"), br(),
          tags$a(href = "mailto:nabeelfebin@gmail.com", icon("envelope"), " nabeelfebin@gmail.com", style = "color:#007AFF;text-decoration:none;"), br(),
          tags$a(href = "mailto:arifshahana03@gmail.com", icon("envelope"), " arifshahana03@gmail.com", style = "color:#007AFF;text-decoration:none;"), br(),
          tags$a(href = "mailto:tesheeja@gmail.com", icon("envelope"), " tesheeja@gmail.com", style = "color:#007AFF;text-decoration:none;"), br(), br(),
          icon("building"), " ICAR - Indian Institute of Spices Research", br(), br(),
          tags$small("Version 0.1.0")
        )
      )
    ),
    
    dashboardBody(
      useShinyjs(),
      tags$head(
        tags$style(HTML("
          :root {
            --ios-blue: #007AFF;
            --ios-green: #34C759;
            --ios-orange: #FF9F0A;
            --ios-red: #FF3B30;
            --ios-gray: #8E8E93;
            --ios-light-gray: #F2F2F7;
            --ios-dark-gray: #1C1C1E;
            --ios-background: #F9F9FB;
            --ios-card: #FFFFFF;
            --ios-border: #E5E5EA;
          }
          .content-wrapper, .right-side {
            background: var(--ios-background);
            font-family: -apple-system, BlinkMacSystemFont, 'SF Pro Text', 'SF Pro Display', 'Helvetica Neue', sans-serif;
          }
          .main-header {
            backdrop-filter: blur(20px) saturate(180%);
            background-color: rgba(255, 255, 255, 0.95) !important;
            border-bottom: 0.5px solid var(--ios-border) !important;
            box-shadow: 0 2px 10px rgba(0,0,0,0.03);
          }
          .main-header .logo { background-color: transparent !important; border-right: 0.5px solid var(--ios-border) !important; }
          .main-header .navbar { background-color: transparent !important; }
          .main-sidebar { background-color: rgba(255, 255, 255, 0.95) !important; border-right: 0.5px solid var(--ios-border) !important; }
          .sidebar-menu > li > a { color: var(--ios-dark-gray) !important; font-weight: 500; font-size: 15px; padding: 14px 20px; border-radius: 12px; margin: 4px 12px; }
          .sidebar-menu > li > a:hover { background: rgba(0, 122, 255, 0.08) !important; color: var(--ios-blue) !important; }
          .sidebar-menu > li.active > a { background: rgba(0, 122, 255, 0.12) !important; color: var(--ios-blue) !important; font-weight: 600; }
          .box { border-radius: 18px !important; border: 0.5px solid var(--ios-border) !important; background: var(--ios-card) !important; box-shadow: 0 2px 8px rgba(0,0,0,0.04) !important; margin-bottom: 24px; overflow: hidden; }
          .box-header { border-bottom: 0.5px solid var(--ios-border) !important; background: transparent !important; padding: 18px 20px; }
          .box-title { font-weight: 600; font-size: 17px; color: var(--ios-dark-gray); }
          .box-body { padding: 20px; }
          .btn { border-radius: 12px !important; font-weight: 500; font-size: 15px; padding: 8px 18px !important; }
          .btn-default, .btn-primary { background: var(--ios-blue); border: none; color: white; }
          .btn-default:hover, .btn-primary:hover { background: #005FC5; transform: scale(0.98); }
          .form-control, .selectize-input { border-radius: 12px !important; border: 0.5px solid var(--ios-border) !important; padding: 10px 14px !important; font-size: 15px; }
          .stat-card { background: linear-gradient(135deg, #4F6DF5 0%, #8B5CF6 100%); border-radius: 20px; padding: 28px 32px; color: white; box-shadow: 0 8px 20px rgba(79,109,245,0.18); }
          .stat-card h2 { color: #FFFFFF; margin: 0 0 6px 0; }
          .stat-card p { color: rgba(255,255,255,0.9); font-size: 15px; margin: 0; }
        "))
      ),
      tags$script(HTML('
        Shiny.addCustomMessageHandler("reloadJBrowse", function(message){
          var frame = document.getElementById("jb1-browser");
          if(frame){
            frame.src = "about:blank";
            setTimeout(function(){
              frame.src = "jbrowse/index.html?ts=" + new Date().getTime();
            }, 300);
          }
        });
      ')),
      
      tabItems(
        tabItem(tabName = "home",
          fluidRow(
            column(12,
              div(class = "stat-card",
                h2(icon("leaf"), " NGS Tool Kit"),
                p("An interactive RNA-Seq analysis platform for exploring gene expression and biological insights")
              )
            )
          ),
          br(),
          fluidRow(
            box(width = 12, title = tagList(icon("circle-info"), "What does this app do?"),
              h4("Overview"),
              p("NGS Tool Kit is an end-to-end RNA-Seq analysis dashboard that allows users to perform differential gene expression analysis, visualize high-dimensional data, and explore gene-level patterns interactively."),
              tags$ul(
                tags$li("Differential Expression Analysis (DESeq2)"),
                tags$li("Dimensionality Reduction (PCA, t-SNE, UMAP)"),
                tags$li("Gene-level exploration and visualization"),
                tags$li("Clustering and correlation analysis"),
                tags$li("Interactive plots and downloadable results"),
                tags$li("Interactive genome exploration using JBrowse")
              )
            )
          )
        ),
        tabItem(tabName = "qc", qcUI("qc1")),
        tabItem(tabName = "corr", corrUI("corr1")),
        tabItem(tabName = "cluster", clusterUI("cl1")),
        tabItem(tabName = "pca", pcaUI("pca1")),
        tabItem(tabName = "heatmap", heatmapUI("heat1")),
        tabItem(tabName = "gene", geneUI("gene1")),
        tabItem(tabName = "de", deUI("de1")),
        tabItem(tabName = "topgenes", topgenesUI("tg1")),
        tabItem(tabName = "ma", maUI("ma1")),
        tabItem(tabName = "volcano", volcanoUI("vol1")),
        tabItem(tabName = "tsne", tsneUI("tsne1")),
        tabItem(tabName = "umap", umapUI("umap1")),
        tabItem(tabName = "jbrowse", jbrowseUI("jb1"))
      )
    )
  )
}
