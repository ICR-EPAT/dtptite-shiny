library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "DTP-TITE-CRM",
  sidebar = sidebar(),
  card(card_body("Under construction."))
)

server <- function(input, output, session) {}

shinyApp(ui, server)
