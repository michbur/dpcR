library(shiny)

shinyUI(navbarPage(title = "dpcReport",
                   id= "navbar", windowTitle = "dpcReport", collapsible=TRUE,
                   tabPanel("Input file",
                            includeMarkdown("input_file1.md"),
                            fluidRow(
                              column(2, fileInput("input_file", 
                                                  "Choose dPCR data")),
                              column(3, htmlOutput("input_information"))
                            ),
                            includeMarkdown("input_file2.md"),
                            fluidRow(
                              column(3, h3("Experiment name"), htmlOutput("exp_choice")),
                              column(3, h3("Repeat id"), htmlOutput("rep_choice"))
                            )
                   ),
                   tabPanel("Data summary table", dataTableOutput("summary_input"),
                            includeMarkdown("data_summary_table1.md")),
                   tabPanel("Input data", tableOutput("input_data"))
))

