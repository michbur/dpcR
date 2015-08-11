library(shiny)
library(shinythemes)

shinyUI(navbarPage(title = "dpcReport",
                   theme = shinytheme("cerulean"),
                   id= "navbar", windowTitle = "dpcReport", collapsible=TRUE,
                   tabPanel("Input file",
                            includeMarkdown("input_file1.md"),
                            fluidRow(
                              column(2, fileInput("input_file", 
                                                  h4("Choose dPCR data"))),
                              column(2, selectInput("input_type", label = h4("Select data format"), 
                                                    choices = list("Raw data" = "raw", "Choice 2" = 2))),
                              column(3, htmlOutput("input_information"))
                            ),
                            includeMarkdown("input_file2.md"),
                            fluidRow(
                              column(3, h4("Experiment name"), htmlOutput("exp_choice")),
                              column(3, h4("Technical repeat ID"), htmlOutput("rep_choice"))
                            )
                   ),
                   tabPanel("Data summary table", 
                            dataTableOutput("summary_input"),
                            includeMarkdown("./data_summary/data_summary_table1.md")),
                   #summary boxplot and stripchart
                   tabPanel("Data summary scatter charts", 
                            plotOutput("summary_plot", 
                                       dblclick = dblclickOpts(id = "summary_plot_dbl")),
                            br(),
                            htmlOutput("summary_plot_dbl"),
                            includeMarkdown("./summary_plots/data_summary_scatterchart1.md"),
                            plotOutput("summary_exprep_plot", 
                                       dblclick = dblclickOpts(id = "summary_exprep_plot_dbl")),
                            htmlOutput("summary_exprep_plot_dbl")),
                   #test counts
                   tabPanel("Compare runs", 
                            includeMarkdown("./test_counts/test_counts1.md"),
                            dataTableOutput("test_counts_res"),
                            includeMarkdown("./test_counts/test_counts2.md"),
                            dataTableOutput("test_counts_groups"),
                            plotOutput("test_counts_plot", 
                                       dblclick = dblclickOpts(id = "test_count_dbl")),
                            htmlOutput("test_count_dbl"),
                            includeMarkdown("./test_counts/test_counts3.md")),
                   tabPanel("Array visualization", 
                            htmlOutput("array_choice"),
                            plotOutput("plot_panel", height = 600,
                                       brush  = brushOpts(id = "plot_panel_brush")),
                            br(),
                            htmlOutput("plot_panel_brush"),
                            dataTableOutput("plot_panel_region_summary")),
                   tabPanel("Download report", 
                            includeMarkdown("download_report1.md"),
                            checkboxInput("data_summary_table_rep", "Data summary table", value = TRUE),
                            checkboxInput("data_summary_scatter_rep", "Data summary scatter charts", 
                                          value = TRUE),
                            checkboxInput("data_summary_test_counts", "Compare runs", 
                                          value = TRUE),
                            checkboxInput("plot_panel", "Visualise individually each array", 
                                          value = FALSE),
                            downloadButton("report_download_button", 
                                           "Download report")),
                   tabPanel("About", 
                            includeMarkdown("about.md")),
                   tabPanel("Input data", 
                            tableOutput("input_data"))
))

