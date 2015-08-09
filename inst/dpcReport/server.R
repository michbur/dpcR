library(shiny)
library(dpcR)
library(ggplot2)

size_mod <- 4
cool_theme <- theme(plot.background=element_rect(fill = "transparent",
                                                 colour = "transparent"),
                    panel.grid.major = element_line(colour="lightgrey", linetype = "dashed"),
                    panel.background = element_rect(fill = "white", colour = "black"),
                    legend.background = element_rect(fill="NA"),
                    legend.position = "bottom",
                    axis.text = element_text(size=12 + size_mod),
                    axis.title.x = element_text(size=15 + size_mod, vjust = -0.1), 
                    axis.title.y = element_text(size=15 + size_mod, vjust = 1),
                    strip.text = element_text(size=15 + size_mod, face = "bold"),
                    legend.text = element_text(size=12 + size_mod), 
                    legend.title = element_text(size=15 + size_mod),
                    plot.title = element_text(size=20 + size_mod))

app_digits <- 4

change_data <- function(input_dat, rep_names_new, exp_names_new) {
  new_dat <- input_dat
  slot(new_dat, "replicate") <- rep_names_new
  slot(new_dat, "exper") <- exp_names_new
  colnames(new_dat) <- paste0(exp_names_new, ".", rep_names_new)
  new_dat
}

#the convention: data is a data frame, this first column is x, the second is y
choose_xy_point <- function(db_id, data) {
  if(!is.null(db_id)) {
    if(is.factor(data[[1]])) {
      #which experiment was chosen
      chosen_x <- levels(data[[1]])[round(db_id[["x"]], 0)]
      #which lambda was chosen
      #clicked lambda 
      clicked_y <- db_id[["y"]]
      diff_order <- order(abs(data[[2]] - clicked_y))
      row_id <- diff_order[which.max(data[[1]][diff_order] == chosen_x)]
      chosen_y <- data[row_id, 2]
      #indirect row_id, because we need the exact location, not relative id of the row
      #in the subset
    } else {
      #x and y countinous
      #not implemented yetl, maybe nearPoints
    }
    c(x = chosen_x, y = chosen_y, row = row_id)
  } else {
    NULL
  }
}


shinyServer(function(input, output) {
  
  # Input file panel --------------------------------
  #check if no data is loaded or no example used
  null_input <- reactive({
    is.null(input[["input_file"]]) && input[["run_example"]] == 0
  })
  
  #read and process data from different vendors
  input_dat <- reactive({
    #after loading any file it would be possible to start an example
    if(is.null(input[["input_file"]])) {
      read_dpcr("example_data.csv", format = "raw")
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
    new_dat <- change_data(input_dat(), as.factor(rep_names_new()), as.factor(exp_names_new()))
    #new_dat <- input_dat()
    res <- summary(new_dat, print = FALSE)[["summary"]]
    res <- cbind(run = paste0(res[["experiment"]], ".", res[["replicate"]]), res)
    colnames(res) <- c("Run", "Experiment name", "Replicate ID", "Method", "&lambda;", "&lambda; (lower CI)",
                       "&lambda; (upper CI)", "m", "m (lower CI)", "m (upper CI)", "k", "n")
    res
  }, escape = FALSE)
  
  
  # Data summary scatter chart panel --------------------------------
  summary_plot_dat <- reactive({
    new_dat <- change_data(input_dat(), as.factor(rep_names_new()), as.factor(exp_names_new()))
    
    summ <- summary(new_dat, print = FALSE)[["summary"]]
    summ[summ[["method"]] == "bhat", ]
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
                    data = summary_plot_dat()[, c("experiment", "lambda")])
  })
  
  output[["summary_plot"]] <- renderPlot({
    summ <- summary_plot_dat()
    dat <- cbind(summ, selected = rep(FALSE, nrow(summary_plot_dat())))
    dat[as.numeric(summary_point[["selected"]]), "selected"] <- TRUE
    
    ggplot(dat, aes(x = experiment, y = lambda, shape = selected,
                    ymax = lambda.up, ymin = lambda.low)) +
      geom_point(size = 4, alpha = 0.6, lty = 2, colour = "blue") + cool_theme +
      geom_boxplot(outlier.colour = NA, fill = adjustcolor("lightgrey", alpha.f = 0.25), shape = 15) + 
      ggtitle("Experiment boxplot") +
      scale_x_discrete("Experiment name") +
      scale_y_continuous(expression(lambda)) + 
      scale_shape_manual(guide = FALSE, values = c(15, 18)) 
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
    summ <- summary_exprep_plot_dat()
    dat <- cbind(summ, selected = rep(FALSE, nrow(summary_exprep_plot_dat())))
    dat[as.numeric(summary_exprep_point[["selected"]]), "selected"] <- TRUE
    
    ggplot(dat, aes(x = exprep, y = lambda, shape = selected, colour = experiment,
                    ymax = lambda.up, ymin = lambda.low, linetype = selected)) +
      geom_point(size = 4) + cool_theme +
      ggtitle("Experiment/replicate scatter chart") +
      scale_x_discrete("Replicate id", labels = dat[["replicate"]] ) +
      scale_y_continuous(expression(lambda)) + 
      scale_color_discrete("Experiment name") +
      scale_linetype_manual(guide = FALSE, values = c("solid", "dashed")) + 
      scale_shape_manual(guide = FALSE, values = c(15, 18)) + 
      geom_errorbar(size = 1.2, width = nlevels(dat[["exprep"]])/80)
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
    dat <- test_counts_groups_summary()
    colnames(dat) <- c("Run", "Experiment name", "Replicate ID", "Assigned group",
                       "&lambda;", "&lambda; (lower CI)", "&lambda; (upper CI)", "k", "n")
    dat
  }, escape = FALSE)
  
  
  output[["test_counts_res"]] <- renderDataTable({
    object <- test_counts_dat()
    signif_stars <- symnum(slot(object, "test_res")[, "p_value"], corr = FALSE, na = FALSE, 
                           cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                           symbols = c("***", "**", "*", ".", " "))
    res <- data.frame(runs = rownames(slot(object, "test_res")), 
                      slot(object, "test_res"), 
                      signif = as.vector(signif_stars))
    colnames(res) <- c("Compared pair of runs", "p-value", "Significance")
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
    dat <- test_counts_groups_summary()
    dat[["selected"]] <- rep(FALSE, nrow(dat))
    dat[as.numeric(test_count_point[["selected"]]), "selected"] <- TRUE
    
    ggplot(dat, aes(x = run, y = lambda, shape = selected, colour = experiment,
                    ymax = lambda.up, ymin = lambda.low, linetype = selected, label = group)) +
      geom_point(size = 4) + cool_theme +
      geom_text(aes(x = run, y = lambda.up), size = 7, vjust = -0.5, show_guide = FALSE) +
      ggtitle("Grouped experiments") +
      scale_x_discrete("Replicate id", labels = dat[["replicate"]] ) +
      scale_y_continuous(expression(lambda)) + 
      coord_cartesian(ylim = c(ifelse(min(dat[["lambda.low"]]) > 0,
                                      min(dat[["lambda.low"]]) * 0.95, 
                                      min(dat[["lambda.low"]]) * 1.05),
                               ifelse(max(dat[["lambda.up"]]) < 0,
                                      max(dat[["lambda.up"]]) * 0.95, 
                                      max(dat[["lambda.up"]]) * 1.15))) +
      scale_color_discrete("Experiment name") +
      scale_linetype_manual(guide = FALSE, values = c("solid", "dashed")) + 
      scale_shape_manual(guide = FALSE, values = c(15, 18)) + 
      geom_errorbar(size = 1.2, width = nlevels(dat[["run"]])/80)
    
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
  
  #input data table, may be scrapped ----------------------------------
  output[["input_data"]] <- renderTable({
    new_dat <- change_data(input_dat(), as.factor(rep_names_new()), as.factor(exp_names_new()))
    #new_dat <- input_dat
    storage.mode(new_dat) <- "integer"
    #colnames(new_dat) <- paste0(exp_names_new(), "; ", rep_names_new())
    slot(new_dat, ".Data")
  })
  
  
  output[["report_download_button"]] <- downloadHandler(
    filename  = "dpcReport.html",
    content = function(file) {
      knitr::knit(input = "report_template.Rmd", 
                  output = "dpcReport.md", quiet = TRUE)
      markdown::markdownToHTML("dpcReport.md", file, stylesheet = "report.css", 
                               options = c('toc', markdown::markdownHTMLOptions(TRUE)))
    }
  )
  
})