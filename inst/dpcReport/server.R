library(shiny)
library(dpcR)
library(ggplot2)
library(shinythemes)

source("server_data.R")


shinyServer(function(input, output) {
  
  # Input file panel --------------------------------
  
  #read and process data from different vendors
  input_dat <- reactive({
    #after loading any file it would be possible to start an example
    if(is.null(input[["input_file"]])) {
      read_dpcr("example_data.csv", format = "raw_adpcr")
    } else {
      read_dpcr(input[["input_file"]][["datapath"]], format = input[["input_type"]])
    }
  })
  
  exp_names <- reactive(slot(input_dat(), "exper"))
  
  rep_names <- reactive(slot(input_dat(), "replicate"))
  
  exp_names_new <- reactive(sapply(1L:length(exp_names()), function(single_exp_id)
    input[[paste0("experiment_name", single_exp_id)]]))
  
  rep_names_new <- reactive(sapply(1L:length(rep_names()), function(single_rep_id)
    input[[paste0("rep_name", single_rep_id)]]))
  
  output[["exp_choice"]] <- renderUI({
    lapply(1L:length(exp_names()), function(single_exp_id)
      textInput(inputId = paste0("experiment_name", single_exp_id), 
                label = paste0("Column", single_exp_id), value = exp_names()[single_exp_id]))
  })
  
  
  output[["rep_choice"]] <- renderUI({
    lapply(1L:length(rep_names()), function(single_rep_id)
      textInput(inputId = paste0("rep_name", single_rep_id), 
                label = paste0("Column", single_rep_id), value = rep_names()[single_rep_id]))
  })
  
  #information if input file is loaded
  output[["input_information"]] <- renderPrint({
    if(is.null(input[["input_file"]])) {
      p("No input detected. Example data loaded.")
    } else {
      p("Detected input file: ", strong(input[["input_file"]][["name"]]), ".")
    }
  })
  
  
  # Data summary table panel --------------------------------
  output[["summary_input"]] <- renderDataTable({
    source("./data_summary/summary_input.R", local = TRUE)
    res
  }, escape = FALSE)
  
  
  # Data summary scatter chart panel --------------------------------
  summary_plot_dat <- reactive({
    new_dat <- change_data(input_dat(), as.factor(rep_names_new()), as.factor(exp_names_new()))
    
    summ <- summary(new_dat, print = FALSE)[["summary"]]
    summ[summ[["method"]] == input[["CI_method"]], ]
  })
  
  
  #clicking a point in the summary boxplot
  summary_point <- reactiveValues(
    selected = NULL
  )
  
  observeEvent(input[["summary_plot_dbl"]], {
    summary_point[["selected"]] <- summary_plot_dbl()[["row"]]
  })
  
  summary_plot_dbl <- reactive({
    choose_xy_point(input[["summary_plot_dbl"]], 
                    data = summary_plot_dat()[, c("lambda", "experiment")])
  })
  
  output[["summary_plot"]] <- renderPlot({
    source("./summary_plots/summary_plot.R", local = TRUE)
    p
  })
  
  output[["summary_plot_dbl"]] <- renderPrint({
    summ <- summary_plot_dat()
    dat <- cbind(summ, selected = rep(FALSE, nrow(summary_plot_dat())))
    dat[as.numeric(summary_point[["selected"]]), "selected"] <- TRUE
    
    epilogue <- list(strong("Double-click"), "point on the chart to learn its properties.", br()) 
    
    prologue <- if(is.null(summary_point[["selected"]])) {
      list()
    } else {
      dat <- dat[dat[["selected"]] == TRUE, ]
      list("Experiment name: ", as.character(dat[["experiment"]]), br(), 
           HTML("&lambda;"), ":", round(dat[["lambda"]], app_digits), br())
    }
    
    do.call(p, c(prologue, epilogue))
  })
  
  # Data summary experiment-replicate scatter chart panel --------------------------------
  summary_exprep_plot_dat <- reactive({
    summ <- summary_plot_dat()
    summ[["exprep"]] <- factor(paste0(summ[["experiment"]], "\n", summ[["replicate"]]))
    summ
  })
  
  
  #clicking a point in the summary experiment-replicate scatter chart
  summary_exprep_point <- reactiveValues(
    selected = NULL
  )
  
  observeEvent(input[["summary_exprep_plot_dbl"]], {
    summary_exprep_point[["selected"]] <- summary_exprep_plot_dbl()[["row"]]
  })
  
  summary_exprep_plot_dbl <- reactive({
    choose_xy_point(input[["summary_exprep_plot_dbl"]], 
                    data = summary_exprep_plot_dat()[, c("exprep", "lambda")])
  })
  
  output[["summary_exprep_plot"]] <- renderPlot({
    source("./summary_plots/summary_exprep_plot.R", local = TRUE)
    p
  })
  
  output[["summary_exprep_plot_dbl"]] <- renderPrint({
    summ <- summary_exprep_plot_dat()
    dat <- cbind(summ, selected = rep(FALSE, nrow(summary_exprep_plot_dat())))
    dat[as.numeric(summary_exprep_point[["selected"]]), "selected"] <- TRUE
    
    epilogue <- list(strong("Double-click"), "point on the chart to learn its properties.", br()) 
    
    prologue <- if(is.null(summary_exprep_point[["selected"]])) {
      list()
    } else {
      dat <- dat[dat[["selected"]] == TRUE, ]
      list("Experiment name: ", as.character(dat[["experiment"]]), br(), 
           "Replicate ID: ", as.character(dat[["replicate"]]), br(),
           HTML("&lambda;"), ": ", round(dat[["lambda"]], app_digits), br(),
           HTML("&lambda;"), "(lower confidence interval): ", round(dat[["lambda.low"]], app_digits), br(),
           HTML("&lambda;"), "(upper confidence interval): ", round(dat[["lambda.up"]], app_digits), br())
    }
    
    do.call(p, c(prologue, epilogue))
  })
  
  # Test counts (compare experiments) --------------------- 
  test_counts_dat <- reactive({
    new_dat <- change_data(input_dat(), as.factor(rep_names_new()), as.factor(exp_names_new()))
    test_counts(new_dat, model = "ratio")
  })
  
  test_counts_groups_summary <- reactive({
    dat <- slot(test_counts_dat(), "group_coef")
    dat[["run"]] <- as.factor(rownames(dat))
    rownames(dat) <- NULL
    dat <- cbind(dat, summary_exprep_plot_dat()[, c("experiment", "replicate", "k", "n")])
    dat <- dat[, c("run", "experiment", "replicate", "group", "lambda", 
                   "lambda.low", "lambda.up", "k", "n")]
    dat
  })
  
  output[["test_counts_groups"]] <- renderDataTable({
    source("./test_counts/test_counts_group.R", local = TRUE)
    dat
  }, escape = FALSE)
  
  
  output[["test_counts_res"]] <- renderDataTable({
    source("./test_counts/test_counts_res.R", local = TRUE)
    res
  })
  
  #clicking a point in the summary experiment-replicate scatter chart
  test_count_point <- reactiveValues(
    selected = NULL
  )
  
  observeEvent(input[["test_count_dbl"]], {
    test_count_point[["selected"]] <- test_count_dbl()[["row"]]
  })
  
  test_count_dbl <- reactive({
    choose_xy_point(input[["test_count_dbl"]], 
                    data = test_counts_groups_summary()[, c("run", "lambda")])
  })
  
  
  output[["test_counts_plot"]] <- renderPlot({
    source("./test_counts/test_counts_plot.R", local = TRUE)
    p
  })
  
  output[["test_count_dbl"]] <- renderPrint({
    dat <- test_counts_groups_summary()
    dat[["selected"]] <- rep(FALSE, nrow(dat))
    dat[as.numeric(test_count_point[["selected"]]), "selected"] <- TRUE
    
    epilogue <- list(strong("Double-click"), "point on the chart to learn its properties.", br()) 
    
    prologue <- if(is.null(test_count_point[["selected"]])) {
      list()
    } else {
      dat <- dat[dat[["selected"]] == TRUE, ]
      list("Experiment name: ", as.character(dat[["experiment"]]), br(), 
           "Replicate ID: ", as.character(dat[["replicate"]]), br(),
           "Assigned group: ", as.character(dat[["group"]]), br(),
           HTML("&lambda;"), ": ", round(dat[["lambda"]], app_digits), br(),
           HTML("&lambda;"), "(lower confidence interval): ", round(dat[["lambda.low"]], app_digits), br(),
           HTML("&lambda;"), "(upper confidence interval): ", round(dat[["lambda.up"]], app_digits), br())
    }
    
    do.call(p, c(prologue, epilogue))
  })
  
  # plot panel --------------------------
  
  output[["plot_panel_tab"]] <- renderUI({
    if(class(input_dat()) == "adpcr") {
      list(includeMarkdown("./plot_panel/plot_panel1.md"),
           htmlOutput("array_choice"),
           htmlOutput("plot_panel_stat"),
           numericInput("nx", "Numbers of quadrats in the x direction:", 
                        5, min = 1, max = NA, step = NA),
           numericInput("ny", "Numbers of quadrats in the y direction:", 
                        5, min = 1, max = NA, step = 1),
           plotOutput("plot_panel", height = 600,
                      brush  = brushOpts(id = "plot_panel_brush")),
           br(),
           includeMarkdown("./plot_panel/plot_panel2.md"),
           htmlOutput("plot_panel_brush"),
           dataTableOutput("plot_panel_region_summary"))
    } else {
      includeMarkdown("./plot_panel/plot_panel0.md")
    }
  })
  
  
  output[["array_choice"]] <- renderUI({
    new_dat <- change_data(input_dat(), as.factor(rep_names_new()), as.factor(exp_names_new()))
    array_names <- colnames(new_dat)
    names(array_names) <- array_names
    selectInput("array_choice", label = h4("Select array"), 
                choices = as.list(array_names))
  })
  
  
  nx_a <- reactive({length(slot(input_dat(), "col_names"))})
  ny_a <- reactive({length(slot(input_dat(), "row_names"))})
  
  plot_panel_dat <- reactive({
    new_dat <- change_data(input_dat(), as.factor(rep_names_new()), as.factor(exp_names_new()))
    
    exp_run <- input[["array_choice"]]
    
    source("./plot_panel/adpcr2panel.R", local = TRUE)
    
    df
  })
  
  
  plot_panel_region <- reactiveValues(
    selected = NULL
  )
  
  observeEvent(input[["plot_panel_brush"]], {
    plot_panel_region[["selected"]] <- plot_panel_brush()
  })
  
  plot_panel_brush <- reactive({
    choose_xy_region(input[["plot_panel_brush"]], 
                     data = plot_panel_dat()[, c("col", "row")])
  })
  
  
  output[["plot_panel"]] <- renderPlot({
    df <- plot_panel_dat()
    
    if(!is.null(plot_panel_brush())) 
      df[plot_panel_region[["selected"]], "selected"] <- TRUE
    
    source("./plot_panel/plot_panel.R", local = TRUE)
    p + ggtitle(df[["exp_run"]][1])
  })
  
  output[["plot_panel_brush"]] <- renderPrint({
    dat <- plot_panel_dat()
    
    dat[plot_panel_region[["selected"]], "selected"] <- TRUE
    
    epilogue <- list(strong("Click and sweep"), "over the partitions to select them.", br()) 
    
    prologue <- if(is.null(plot_panel_brush())) {
      list()
    } else {
      dat <- dat[dat[["selected"]] == TRUE, ]
      list("Number of partitions selected: ", as.character(sum(plot_panel_region[["selected"]])), br())
    }
    do.call(p, c(prologue, epilogue))
  })
  
  output[["plot_panel_stat"]] <- renderPrint({
    new_dat <- change_data(input_dat(), as.factor(rep_names_new()), as.factor(exp_names_new()))
    roi <- extract_dpcr(new_dat, input[["array_choice"]])
    res <- test_panel(roi, 
                      length(slot(new_dat, "col_names")), 
                      length(slot(new_dat, "row_names")), 
                      nx = input[["nx"]], ny = input[["ny"]])[[1]]
    
    prologue <- list("Experiment name: ", as.character(slot(roi, "exper")), br(), 
                     "Replicate ID: ", as.character(slot(roi, "replicate")), br(),
                     "Complete Spatial Randomness test statistic (", HTML("&Chi;"), "): ", 
                     round(res[["statistic"]], app_digits), br(),
                     "Df: ", res[["parameter"]], br(),
                     "Complete Spatial Randomness test p-value: ", round(res[["p.value"]], app_digits), br(),
                     "Method: ", res[["method"]], br(),
                     "Alternative: ", res[["alternative"]], br())
    
    do.call(p, prologue)
  })
  
  output[["plot_panel_region_summary"]] <- renderDataTable({
    new_dat <- change_data(input_dat(), as.factor(rep_names_new()), as.factor(exp_names_new()))
    roi <- extract_dpcr(new_dat, input[["array_choice"]])
    
    summs <- summary(roi, print = FALSE)[["summary"]]
    summs <- cbind(region = rep("Whole array", nrow(summs)), summs)
    
    if(!is.null(plot_panel_brush())) {
      slot(roi, ".Data") <- slot(roi, ".Data")[plot_panel_region[["selected"]], , drop = FALSE]
      slot(roi, "n") <- sum(plot_panel_region[["selected"]])
      summs <- rbind(summs, cbind(region = rep("Selected region", nrow(summs)), 
                                  summary(roi, print = FALSE)[["summary"]]))
    }
    
    colnames(summs) <- c("Region", "Experiment name", "Replicate ID", "Method", "&lambda;", 
                         "&lambda; (lower CI)", "&lambda; (upper CI)", "m", 
                         "m (lower CI)", "m (upper CI)", "k", "n")
    
    summs
  }, escape = FALSE)
  
  
  
  #report download ---------------------------------------------------
  output[["report_download_button"]] <- downloadHandler(
    filename  = "dpcReport.html",
    content = function(file) {
      knitr::knit(input = "report_template.Rmd", 
                  output = "dpcReport.md", quiet = TRUE)
      on.exit(unlink(c("dpcReport.md", "figure"), recursive = TRUE))
      markdown::markdownToHTML("dpcReport.md", file, stylesheet = "report.css", 
                               options = c('toc', markdown::markdownHTMLOptions(TRUE)))
    })
  
  
})
