library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Comparision of digital PCR experiments"),
  sidebarPanel("Bla"),
  mainPanel(
    uiOutput("dynamic_tabset") 
  )
))

