# NGSToolKit desktop entry: build the app object and return it (no server start).
library(NGSToolKit)
try(NGSToolKit:::setup_jbrowse(), silent = TRUE)
jb <- system.file("www/jbrowse", package = "NGSToolKit")
if (nzchar(jb) && dir.exists(jb)) shiny::addResourcePath("jbrowse", jb)
shiny::shinyApp(ui = NGSToolKit:::app_ui(), server = NGSToolKit:::app_server)