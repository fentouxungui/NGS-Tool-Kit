app_ui <- function() { dashboardPage(
  
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
      
      menuItem(
        "JBrowse",
        tabName = "jbrowse",
        icon = icon("dna")
      ),
      
      tags$hr(style = "margin: 15px 0; border-top: 0.5px solid var(--ios-border);"),
      
      div(
        style = "
          padding: 16px 20px;
          text-align: center;
          font-size: 12.5px;
          color: var(--ios-gray);
        ",
        div(
          style = "font-weight: 600; color: var(--ios-dark-gray); margin-bottom: 4px;",
          icon("building-columns"), " ICAR  IISR"
        ),
        div(
          style = "font-size: 11.5px;",
          "Kozhikode, Kerala, India"
        ),
        br(),
        tags$span(class = "badge", style = "background: rgba(0,122,255,0.1); color: #007AFF; font-weight: 600;", "v2.0")
      )
      
    )
  ),
  
  
  dashboardBody(
    useShinyjs(),
    
    # =========================
    # KEEP YOUR FULL iOS CSS HERE (UNCHANGED)
    # =========================
    tags$head(
      tags$style(HTML("/*  iOS 15+ STYLE SYSTEM */
                       /* Base iOS colors */
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
                       /* Main background */
        .content-wrapper, .right-side {
          background: var(--ios-background);
          font-family: -apple-system, BlinkMacSystemFont, 'SF Pro Text', 'SF Pro Display', 'Helvetica Neue', sans-serif;
        }
        /* Header styling */
        .main-header {
          backdrop-filter: blur(20px) saturate(180%);
          -webkit-backdrop-filter: blur(20px) saturate(180%);
          background-color: rgba(255, 255, 255, 0.95) !important;
          border-bottom: 0.5px solid var(--ios-border) !important;
          box-shadow: 0 2px 10px rgba(0,0,0,0.03);
        }
        .main-header .logo {
          background-color: transparent !important;
          border-right: 0.5px solid var(--ios-border) !important;
        }
        
        .main-header .navbar {
          background-color: transparent !important;
        }
        .main-header .logo span, 
        .main-header .logo i,
        .main-header .navbar .dropdown,
        .main-header .navbar .dropdown span,
        .main-header .navbar .dropdown i,
        .main-header .sidebar-toggle {
          color: var(--ios-dark-gray) !important;
        }
         /* Sidebar iOS styling */
        .main-sidebar {
          background-color: rgba(255, 255, 255, 0.95) !important;
          backdrop-filter: blur(10px);
          -webkit-backdrop-filter: blur(10px);
          border-right: 0.5px solid var(--ios-border) !important;
          box-shadow: none;
        }
        .sidebar-menu > li > a {
          color: var(--ios-dark-gray) !important;
          font-weight: 500;
          font-size: 15px;
          padding: 14px 20px;
          transition: all 0.2s ease;
          border-radius: 12px;
          margin: 4px 12px;
        }
        .sidebar-menu > li > a:hover {
          background: rgba(0, 122, 255, 0.08) !important;
          color: var(--ios-blue) !important;
        }
        
        .sidebar-menu > li.active > a {
          background: rgba(0, 122, 255, 0.12) !important;
          color: var(--ios-blue) !important;
          font-weight: 600;
        }
        .sidebar-menu > li.active > a > i {
          color: var(--ios-blue) !important;
        }
        
        /* iOS Card Style */
        .box {
          border-radius: 18px !important;
          border: 0.5px solid var(--ios-border) !important;
          background: var(--ios-card) !important;
          box-shadow: 0 2px 8px rgba(0,0,0,0.04), 0 0 0 0.5px rgba(0,0,0,0.02) !important;
          margin-bottom: 24px;
          overflow: hidden;
        }
        .box-header {
          border-bottom: 0.5px solid var(--ios-border) !important;
          background: transparent !important;
          padding: 18px 20px;
        }.box-title {
          font-weight: 600;
          font-size: 17px;
          color: var(--ios-dark-gray);
          letter-spacing: -0.2px;
        }
        
        .box-body {
          padding: 20px;
        }
        /* iOS Button Styles */
        .btn {
          border-radius: 12px !important;
          font-weight: 500;
          font-size: 15px;
          padding: 8px 18px !important;
          transition: all 0.2s ease;
          letter-spacing: -0.2px;
        }
        .btn-default, .btn-primary {
          background: var(--ios-blue);
          border: none;
          color: white;
        }
        
        .btn-default:hover, .btn-primary:hover {
          background: #005FC5;
          transform: scale(0.98);
        }
        
        .btn-success {
          background: var(--ios-green);
        }
        .btn-warning {
          background: var(--ios-orange);
        }
        
        .btn-danger {
          background: var(--ios-red);
        }
         /* Form controls */
        .form-control, .selectize-input {
          border-radius: 12px !important;
          border: 0.5px solid var(--ios-border) !important;
          padding: 10px 14px !important;
          font-size: 15px;
          transition: all 0.2s;
        }
        
        .form-control:focus, .selectize-input.focus {
          border-color: var(--ios-blue) !important;
          box-shadow: 0 0 0 3px rgba(0, 122, 255, 0.1) !important;
          outline: none;
        }
        /* iOS Table Style */
        .table {
          font-size: 14px;
        }
        
        .table thead th {
          background: var(--ios-light-gray);
          font-weight: 600;
          color: var(--ios-dark-gray);
          border-bottom: 0.5px solid var(--ios-border);
          text-transform: uppercase;
          font-size: 12px;
          letter-spacing: 0.3px;
        }
        .table tbody tr:hover {
          background: rgba(0, 122, 255, 0.04);
        }
        
        /* Info boxes */
        .info-box {
          border-radius: 16px;
          border: 0.5px solid var(--ios-border);
          box-shadow: 0 2px 8px rgba(0,0,0,0.04);
        }
        
        .info-box-icon {
          border-radius: 16px 0 0 16px;
        }
         /* Progress bars */
        .progress {
          border-radius: 10px;
          background: var(--ios-light-gray);
          height: 8px;
        }
        
        .progress-bar {
          border-radius: 10px;
          background: linear-gradient(90deg, var(--ios-blue), #005FC5);
        }
         /* iOS Alerts */
        .alert {
          border-radius: 14px;
          border: none;
          background: var(--ios-light-gray);
          color: var(--ios-dark-gray);
          padding: 14px 18px;
        }
        
        .alert-info {
          background: rgba(0, 122, 255, 0.1);
          color: var(--ios-blue);
        }
        .alert-success {
          background: rgba(52, 199, 89, 0.1);
          color: var(--ios-green);
        }
        
        /* Tabs styling */
        .nav-tabs {
          border-bottom: 0.5px solid var(--ios-border);
        }
        .nav-tabs > li > a {
          color: var(--ios-gray);
          font-weight: 500;
          border: none;
          background: transparent;
          padding: 12px 18px;
          transition: all 0.2s;
        }
        
        .nav-tabs > li.active > a {
          color: var(--ios-blue);
          border-bottom: 2px solid var(--ios-blue);
          background: transparent;
        }
        .nav-tabs > li > a:hover {
          color: var(--ios-blue);
          background: rgba(0, 122, 255, 0.04);
        }
        
        /* Custom iOS switches */
        .ios-switch {
          display: inline-block;
          position: relative;
          width: 51px;
          height: 31px;
        }
        .ios-switch input {
          opacity: 0;
          width: 0;
          height: 0;
        }
        .slider {
          position: absolute;
          cursor: pointer;
          top: 0;
          left: 0;
          right: 0;
          bottom: 0;
          background-color: #E5E5EA;
          transition: .3s;
          border-radius: 34px;
        }
         .slider:before {
          position: absolute;
          content: \"\";
          height: 27px;
          width: 27px;
          left: 2px;
          bottom: 2px;
          background-color: white;
          transition: .3s;
          border-radius: 50%;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
         }
         input:checked + .slider {
          background-color: var(--ios-green);
        }
        
        input:checked + .slider:before {
          transform: translateX(20px);
        }
        
        /* Loading spinner iOS style */
        @keyframes spin {
          0% { transform: rotate(0deg); }
          100% { transform: rotate(360deg); }
        }
         /* Scrollbar iOS style */
        ::-webkit-scrollbar {
          width: 8px;
          height: 8px;
        }
        
        ::-webkit-scrollbar-track {
          background: transparent;
        }
        
        ::-webkit-scrollbar-thumb {
          background: var(--ios-gray);
          border-radius: 10px;
        }
        ::-webkit-scrollbar-thumb:hover {
          background: #6C6C70;
        }
        
        /* Headers */
        h2, h3, h4 {
          font-weight: 600;
          letter-spacing: -0.3px;
          color: var(--ios-dark-gray);
        }
        
        h2 {
          font-size: 28px;
        }
        h4 {
          font-size: 17px;
          margin-bottom: 12px;
        }
        
        /* Stats cards */
        .stat-card {
          background: linear-gradient(135deg, #4F6DF5 0%, #8B5CF6 100%);
          border-radius: 20px;
          padding: 28px 32px;
          color: white;
          box-shadow: 0 8px 20px rgba(79,109,245,0.18);
        }
        .stat-card h2 {
          color: #FFFFFF;
          margin: 0 0 6px 0;
        }
        .stat-card p {
          color: rgba(255,255,255,0.9);
          font-size: 15px;
          margin: 0;
          text-align: left;
        }

        /* =========================
           GENERAL ALIGNMENT / JUSTIFY
           ========================= */
        .box-body p {
          text-align: justify;
          text-justify: inter-word;
          color: #3A3A3C;
          line-height: 1.6;
        }
        .box-body ul, .box-body ol {
          text-align: left;
          padding-left: 20px;
          line-height: 1.9;
          color: #3A3A3C;
        }
        .box-title {
          display: flex;
          align-items: center;
          gap: 8px;
        }
        .home-equal-row {
          display: flex;
          flex-wrap: wrap;
        }
        .home-equal-row > .col-sm-6 {
          display: flex;
        }
        .home-equal-row .box {
          width: 100%;
          margin-bottom: 24px;
        }

        /* =========================
           OVERALL WORKFLOW - STEP FLOW
           ========================= */
        .workflow-flow {
          display: flex;
          align-items: stretch;
          justify-content: space-between;
          flex-wrap: wrap;
          gap: 10px;
          margin: 20px 0 24px 0;
        }
        .workflow-step {
          flex: 1 1 0;
          min-width: 120px;
          border-radius: 16px;
          padding: 18px 10px;
          text-align: center;
          color: #1C1C1E;
          font-weight: 600;
          font-size: 14px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.04);
        }
        .workflow-step .workflow-icon {
          font-size: 22px;
          margin-bottom: 10px;
        }
        .workflow-step .workflow-label {
          letter-spacing: -0.1px;
        }
        .workflow-arrow {
          display: flex;
          align-items: center;
          justify-content: center;
          color: var(--ios-gray);
          font-size: 16px;
          flex: 0 0 auto;
          padding: 0 2px;
        }
        .wf-upload   { background: #E7EEFF; }
        .wf-upload   .workflow-icon { color: #3B6FF6; }
        .wf-qc       { background: #E6F8EC; }
        .wf-qc       .workflow-icon { color: #2FAE60; }
        .wf-de       { background: #FFF3E0; }
        .wf-de       .workflow-icon { color: #F0A020; }
        .wf-viz      { background: #F1E9FF; }
        .wf-viz      .workflow-icon { color: #8B5CF6; }
        .wf-explore  { background: #FFE9E4; }
        .wf-explore  .workflow-icon { color: #F0603C; }
        .wf-browser{ background:#EAF3FF; }
        .wf-browser .workflow-icon{ color:#007AFF; }
        .dot-blue{  background:#007AFF; }
        .wf-cluster  { background: #E1F6F4; }
        .wf-cluster  .workflow-icon { color: #16A398; }

        .workflow-desc-row {
          display: flex;
          flex-wrap: wrap;
          gap: 18px;
          border-top: 0.5px solid var(--ios-border);
          padding-top: 18px;
        }
        .workflow-desc-item {
          flex: 1 1 0;
          min-width: 150px;
          font-size: 13.5px;
          color: #3A3A3C;
          text-align: left;
          line-height: 1.6;
        }
        .workflow-dot {
          display: inline-block;
          width: 8px;
          height: 8px;
          border-radius: 50%;
          margin-right: 6px;
        }
        .dot-green  { background: #2FAE60; }
        .dot-orange { background: #F0A020; }
        .dot-purple { background: #8B5CF6; }
        .dot-red    { background: #F0603C; }
        .dot-teal   { background: #16A398; }

        @media (max-width: 900px) {
          .workflow-flow { flex-direction: column; }
          .workflow-arrow { transform: rotate(90deg); padding: 4px 0; }
        }

        /* Responsive adjustments */
        @media (max-width: 768px) {
          .box-body {
            padding: 12px;
          }
          
          .btn {
            padding: 6px 14px !important;
            font-size: 14px;
          }
        }
        
        /* =========================
           TEAM & CONTACT CARDS
           ========================= */
        .team-section-header {
          margin-top: 5px;
          margin-bottom: 24px;
          text-align: center;
        }
        .team-institution-badge {
          display: inline-block;
          background: rgba(0, 122, 255, 0.08);
          color: var(--ios-blue);
          font-weight: 600;
          font-size: 13.5px;
          padding: 7px 18px;
          border-radius: 20px;
          margin-bottom: 10px;
          border: 0.5px solid rgba(0, 122, 255, 0.2);
        }
        .team-grid {
          display: flex;
          flex-wrap: wrap;
          gap: 20px;
          margin-top: 15px;
        }
        .team-card-col {
          flex: 1 1 calc(50% - 20px);
          min-width: 280px;
        }
        @media (max-width: 768px) {
          .team-card-col { flex: 1 1 100%; }
        }
        .team-card {
          background: #FFFFFF;
          border-radius: 18px;
          border: 0.5px solid var(--ios-border);
          box-shadow: 0 4px 15px rgba(0,0,0,0.03);
          padding: 22px 24px;
          transition: all 0.25s ease-in-out;
          display: flex;
          flex-direction: column;
          justify-content: space-between;
          height: 100%;
          position: relative;
          overflow: hidden;
        }
        .team-card::before {
          content: '';
          position: absolute;
          top: 0;
          left: 0;
          width: 5px;
          height: 100%;
          background: linear-gradient(180deg, var(--ios-blue), #34C759);
          border-radius: 18px 0 0 18px;
        }
        .team-card:hover {
          transform: translateY(-3px);
          box-shadow: 0 8px 24px rgba(0, 122, 255, 0.12);
          border-color: rgba(0, 122, 255, 0.3);
        }
        .team-avatar-box {
          display: flex;
          align-items: center;
          gap: 16px;
          margin-bottom: 14px;
        }
        .team-avatar {
          width: 52px;
          height: 52px;
          border-radius: 16px;
          background: linear-gradient(135deg, #007AFF 0%, #34C759 100%);
          color: white;
          display: flex;
          align-items: center;
          justify-content: center;
          font-size: 20px;
          font-weight: 700;
          box-shadow: 0 4px 10px rgba(0,122,255,0.25);
          flex-shrink: 0;
        }
        .team-name {
          font-size: 18px;
          font-weight: 700;
          color: #1C1C1E;
          margin: 0 0 4px 0;
          letter-spacing: -0.3px;
        }
        .team-role {
          font-size: 13.5px;
          color: #6C6C70;
          font-weight: 500;
          line-height: 1.4;
        }
        .team-badge-role {
          display: inline-block;
          font-size: 12px;
          font-weight: 600;
          background: rgba(142, 142, 147, 0.12);
          color: #3A3A3C;
          padding: 3px 10px;
          border-radius: 12px;
          margin-top: 4px;
          line-height: 1.4;
        }
        .team-links {
          display: flex;
          gap: 10px;
          margin-top: 16px;
          padding-top: 14px;
          border-top: 0.5px solid var(--ios-border);
          flex-wrap: wrap;
        }
        .team-btn {
          display: inline-flex;
          align-items: center;
          gap: 6px;
          font-size: 13px;
          font-weight: 600;
          padding: 7px 14px;
          border-radius: 10px;
          text-decoration: none !important;
          transition: all 0.2s ease;
        }
        .team-btn-email {
          background: rgba(0, 122, 255, 0.08);
          color: var(--ios-blue) !important;
        }
        .team-btn-email:hover {
          background: var(--ios-blue);
          color: #FFFFFF !important;
        }
        .team-btn-profile {
          background: rgba(52, 199, 89, 0.08);
          color: #2FAE60 !important;
        }
        .team-btn-profile:hover {
          background: #2FAE60;
          color: #FFFFFF !important;
        }
        .team-btn-linkedin {
          background: rgba(10, 102, 194, 0.08);
          color: #0A66C2 !important;
        }
        .team-btn-linkedin:hover {
          background: #0A66C2;
          color: #FFFFFF !important;
        }
        
        /* =========================
           VISITOR COUNTER CARD
           ========================= */
        .visitor-counter-card {
          background: linear-gradient(135deg, #1C1C1E 0%, #2C2C2E 100%);
          border-radius: 20px;
          padding: 22px 24px;
          color: white;
          box-shadow: 0 8px 20px rgba(0,0,0,0.12);
          display: flex;
          flex-direction: column;
          justify-content: center;
          align-items: center;
          height: 100%;
          text-align: center;
          border: 0.5px solid rgba(255,255,255,0.12);
          min-height: 110px;
        }
        .visitor-count-number {
          font-size: 32px;
          font-weight: 800;
          color: #34C759;
          letter-spacing: -0.5px;
          line-height: 1.1;
          margin-bottom: 4px;
          font-family: -apple-system, BlinkMacSystemFont, 'SF Pro Display', sans-serif;
        }
        .visitor-count-label {
          font-size: 13px;
          font-weight: 600;
          color: rgba(255,255,255,0.85);
          display: flex;
          align-items: center;
          gap: 6px;
          text-transform: uppercase;
          letter-spacing: 0.5px;
        }
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
              
              # =========================
              # HERO BANNER & VISITOR COUNTER
              # =========================
              fluidRow(
                column(8,
                       div(class = "stat-card",
                           h2(icon("leaf"), " NGS Tool Kit"),
                           p("An interactive RNA-Seq analysis platform for exploring gene expression and biological insights")
                       )
                ),
                column(4,
                       uiOutput("visitor_counter_card")
                )
              ),
              
              br(),
              
              # =========================
              # WHAT DOES APP DO
              # =========================
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
              ),
              
              # =========================
              # REQUIRED INPUTS + STEP BY STEP (equal height row)
              # =========================
              fluidRow(
                class = "home-equal-row",
                column(6,
                       box(width = 12, title = tagList(icon("database"), "Required Inputs"),
                           tags$ul(
                             tags$li(strong("Count Matrix:"), " Genes as rows, samples as columns"),
                             tags$li(strong("Metadata:"), " Contains sample and condition information"),
                             tags$li("Optional: Upload custom datasets for visualization modules")
                           )
                       )
                ),
                column(6,
                       box(width = 12, title = tagList(icon("list-ol"), "Step-by-Step Workflow"),
                           tags$ol(
                             tags$li("Upload count matrix and metadata"),
                             tags$li("Perform Data Quality Control (QC)"),
                             tags$li("Run Differential Expression (DE) analysis"),
                             tags$li("Visualize results using MA and Volcano plots"),
                             tags$li("Explore top genes and gene expression"),
                             tags$li("Apply PCA, t-SNE, or UMAP for clustering"),
                             tags$li("Analyze patterns using heatmaps and correlation")
                           )
                       )
                )
              ),
              
              # =========================
              # OVERALL WORKFLOW - VISUAL STEP FLOW
              # =========================
              fluidRow(
                box(width = 12, title = tagList(icon("project-diagram"), "Overall Workflow"),
                    
                    p("The analysis pipeline follows a structured flow from raw data to biological interpretation:"),
                    
                    div(class = "workflow-flow",
                        
                        div(class = "workflow-step wf-upload",
                            div(class = "workflow-icon", icon("cloud-arrow-up")),
                            div(class = "workflow-label", "Data Upload")
                        ),
                        div(class = "workflow-arrow", icon("chevron-right")),
                        
                        div(class = "workflow-step wf-qc",
                            div(class = "workflow-icon", icon("shield-halved")),
                            div(class = "workflow-label", "QC")
                        ),
                        div(class = "workflow-arrow", icon("chevron-right")),
                        
                        div(class = "workflow-step wf-de",
                            div(class = "workflow-icon", icon("magnifying-glass")),
                            div(class = "workflow-label", "DE Analysis")
                        ),
                        div(class = "workflow-arrow", icon("chevron-right")),
                        
                        div(class = "workflow-step wf-viz",
                            div(class = "workflow-icon", icon("chart-simple")),
                            div(class = "workflow-label", "Visualization")
                        ),
                        div(class = "workflow-step wf-explore",
                            div(class = "workflow-icon", icon("magnifying-glass-chart")),
                            div(class = "workflow-label", "Gene Exploration")
                        ),
                        
                        div(class = "workflow-arrow", icon("chevron-right")),
                        
                        # NEW STEP
                        div(class = "workflow-step wf-browser",
                            div(class = "workflow-icon", icon("dna")),
                            div(class = "workflow-label", "Genome Browser")
                        ),
                        
                        div(class = "workflow-arrow", icon("chevron-right")),
                        
                        div(class = "workflow-step wf-cluster",
                            div(class = "workflow-icon", icon("people-group")),
                            div(class = "workflow-label", "Clustering")
                        )
                    ),
                    
                    div(class = "workflow-desc-row",
                        div(class = "workflow-desc-item",
                            span(class = "workflow-dot dot-green"),
                            strong("QC:"), " Ensures data quality before analysis"
                        ),
                        div(class = "workflow-desc-item",
                            span(class = "workflow-dot dot-orange"),
                            strong("DE Analysis:"), " Identifies significant genes"
                        ),
                        div(class = "workflow-desc-item",
                            span(class = "workflow-dot dot-purple"),
                            strong("Visualization:"), " PCA, UMAP, Volcano plots"
                        ),
                        div(class = "workflow-desc-item",
                            span(class = "workflow-dot dot-red"),
                            strong("Exploration:"), " Top genes and expression patterns"
                        ),
                        div(class = "workflow-desc-item",
                            span(class = "workflow-dot dot-blue"),
                            strong("Genome Browser:"),
                            " Explore genomic regions, genes and annotations interactively using JBrowse."
                        ),
                        div(class = "workflow-desc-item",
                            span(class = "workflow-dot dot-teal"),
                            strong("Clustering:"), " Groups similar samples"
                        )
                    )
                )
              ),
              
              # =========================
              # PROJECT TEAM & CONTACT INFORMATION
              # =========================
              fluidRow(
                box(
                  width = 12,
                  title = tagList(icon("user-group"), "Project Team & Contacts"),
                  
                  div(class = "team-section-header",
                      div(class = "team-institution-badge",
                          icon("building-columns"), " ICAR  Indian Institute of Spices Research, Kozhikode, Kerala, India"
                      ),
                      h4("Development & Research Team", style = "margin-top: 6px; font-weight: 700; color: #1C1C1E;")
                  ),
                  
                  div(class = "team-grid",
                      
                      # Dr. Sona Charles
                      div(class = "team-card-col",
                          div(class = "team-card",
                              div(class = "team-avatar-box",
                                  div(class = "team-avatar", "SC"),
                                  div(
                                    div(class = "team-name", "Dr. Sona Charles"),
                                    div(class = "team-badge-role", "Scientist (Agricultural Bioinformatics)")
                                  )
                              ),
                              div(class = "team-links",
                                  tags$a(class = "team-btn team-btn-email",
                                         href = "mailto:sonacharlesicar@gmail.com",
                                         icon("envelope"), "Email"),
                                  tags$a(class = "team-btn team-btn-profile",
                                         href = "https://spices.res.in/staff/detail/dr-sona-charles",
                                         target = "_blank",
                                         icon("arrow-up-right-from-square"), "Institute Profile")
                              )
                          )
                      ),
                      
                      # Febin T Nabeel
                      div(class = "team-card-col",
                          div(class = "team-card",
                              div(class = "team-avatar-box",
                                  div(class = "team-avatar", style = "background: linear-gradient(135deg, #FF9F0A 0%, #FF3B30 100%);", "FN"),
                                  div(
                                    div(class = "team-name", "Febin T Nabeel"),
                                    div(class = "team-badge-role", "Project Trainee")
                                  )
                              ),
                              div(class = "team-links",
                                  tags$a(class = "team-btn team-btn-email",
                                         href = "mailto:nabeelfebin@gmail.com",
                                         icon("envelope"), "Email"),
                                  tags$a(class = "team-btn team-btn-linkedin",
                                         href = "https://www.linkedin.com/in/febin-t-nabeel-a50222274/",
                                         target = "_blank",
                                         icon("linkedin"), "LinkedIn")
                              )
                          )
                      ),
                      
                      # Shahana Arif
                      div(class = "team-card-col",
                          div(class = "team-card",
                              div(class = "team-avatar-box",
                                  div(class = "team-avatar", style = "background: linear-gradient(135deg, #8B5CF6 0%, #EC4899 100%);", "SA"),
                                  div(
                                    div(class = "team-name", "Shahana Arif"),
                                    div(class = "team-badge-role", "Project Associate-1")
                                  )
                              ),
                              div(class = "team-links",
                                  tags$a(class = "team-btn team-btn-email",
                                         href = "mailto:arifshahana03@gmail.com",
                                         icon("envelope"), "Email"),
                                  tags$a(class = "team-btn team-btn-linkedin",
                                         href = "https://www.linkedin.com/in/shahana-arif-899587228/",
                                         target = "_blank",
                                         icon("linkedin"), "LinkedIn")
                              )
                          )
                      ),
                      
                      # Dr. T. E. Sheeja
                      div(class = "team-card-col",
                          div(class = "team-card",
                              div(class = "team-avatar-box",
                                  div(class = "team-avatar", style = "background: linear-gradient(135deg, #10B981 0%, #3B82F6 100%);", "TS"),
                                  div(
                                    div(class = "team-name", "Dr. T. E. Sheeja"),
                                    div(class = "team-badge-role", "HOD, Crop Improvement & Biotech | Principal Scientist")
                                  )
                              ),
                              div(class = "team-links",
                                  tags$a(class = "team-btn team-btn-email",
                                         href = "mailto:tesheeja@gmail.com",
                                         icon("envelope"), "Email"),
                                  tags$a(class = "team-btn team-btn-profile",
                                         href = "https://spices.res.in/staff/detail/dr-t-e-sheeja",
                                         target = "_blank",
                                         icon("arrow-up-right-from-square"), "Institute Profile")
                              )
                          )
                      )
                      
                  )
                )
              )
      ),
      tabItem(tabName = "qc", qcUI("qc1")),
      tabItem(tabName = "de", deUI("de1")),
      tabItem(tabName = "ma",
              maUI("ma1")
      ),
      tabItem(tabName = "pca", pcaUI("pca1")),
      tabItem(tabName = "heatmap", heatmapUI("heat1")),
      tabItem(tabName = "corr", corrUI("corr1")),
      tabItem(tabName = "volcano", volcanoUI("vol1")),
      tabItem(tabName = "tsne", tsneUI("tsne1")),
      tabItem(tabName = "umap", umapUI("umap1")),
      tabItem(tabName = "gene",
              geneUI("gene1")
      ),
      tabItem(tabName = "topgenes",
              topgenesUI("tg1")
      ),
      tabItem(tabName = "cluster", clusterUI("cl1")),
      
      
      tabItem(tabName = "jbrowse",jbrowseUI("jb1"))
    )
  )
)





# =========================
# SERVER
# =========================
}

