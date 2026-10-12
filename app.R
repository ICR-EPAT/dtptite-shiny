library(shiny)
library(bslib)

# Helpers in R/ are sourced automatically by shiny::runApp().
shinyApp(app_ui(), app_server)
