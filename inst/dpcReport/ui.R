library(shiny)

shinyUI(navbarPage(title = "dpcReport",
                   id= "navbar", windowTitle = "dpcraporter", collapsible=TRUE,
                   tabPanel("Input file",
                            fileInput("input_file", "Choose CSV File (input should contain Cq data)",
                                      accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                            br(),
                            p("Lost? Use button below to see an example:"),
                            actionButton("run_example", "Run example")),
                   tabPanel("Adjust data",
                            fluidRow(
                              column(3, h3("Experiment name"), htmlOutput("exp_choice")),
                              column(3, h3("Repeat id"), htmlOutput("rep_choice"))
                            )),
                   tabPanel("Input data2", verbatimTextOutput("read_exp_names")),
                   tabPanel("Input data", tableOutput("input_data"))
))

