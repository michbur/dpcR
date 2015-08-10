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
                    strip.background = element_rect(fill = "#9ecae1", colour = "black"),
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
      if(is.factor(data[[2]])) {
        #which experiment was chosen
        chosen_x <- levels(data[[1]])[round(db_id[["x"]], 0)]
        chosen_y <- levels(data[[2]])[round(db_id[["y"]], 0)]
        
        row_id <- which(data[[1]] == chosen_x & data[[2]] == chosen_y)
        
        #indirect row_id, because we need the exact location, not relative id of the row
        #in the subset
      } else {
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
      }
      
    } else {
      #x and y countinous
      #not implemented yetl, maybe nearPoints
    }
    c(x = chosen_x, y = chosen_y, row = row_id)
  } else {
    NULL
  }
}


choose_xy_region <- function(brush_id, data) {
  if(is.null(brush_id)) {
    NULL
  } else {
    xmin <- levels(data[[1]])[round(brush_id[["xmin"]], 0)]
    xmax <- levels(data[[1]])[round(brush_id[["xmax"]], 0)]
    ymin <- levels(data[[2]])[round(brush_id[["ymin"]], 0)]
    ymax <- levels(data[[2]])[round(brush_id[["ymax"]], 0)]
    x_range <- as.numeric(xmin):as.numeric(xmax)
    y_range <- as.numeric(ymin):as.numeric(ymax)
    x <- data[[1]] %in% as.factor(x_range)
    y <- data[[2]] %in% as.factor(y_range)
    x & y
  }
}


shinyServer(function(input, output) {
  
  # Input file panel --------------------------------
  
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
    source("./data_summary/summary_input.R", local = TRUE)
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
  
  output[["array_choice"]] <- renderUI({
    new_dat <- change_data(input_dat(), as.factor(rep_names_new()), as.factor(exp_names_new()))
    array_names <- colnames(new_dat)
    names(array_names) <- array_names
    selectInput("array_choice", label = h4("Select array"), 
                choices = as.list(array_names))
  })
  
  plot_panel_dat <- reactive({
    new_dat <- change_data(input_dat(), as.factor(rep_names_new()), as.factor(exp_names_new()))
    
    ny_a <- 17
    nx_a <- 45
    
    id_df <- data.frame(which(matrix(TRUE, nrow = ny_a, ncol = nx_a), arr.ind = TRUE))
    
    id_df[["col"]] <- as.factor(id_df[["col"]])
    id_df[["row"]] <- as.factor(id_df[["row"]])
    
    
    exp_run <- input[["array_choice"]]
    cbind(id_df, value = as.factor(new_dat[, exp_run]), selected = rep(FALSE, ny_a * nx_a),
          exp_run = rep(exp_run, ny_a * nx_a))
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
    df[plot_panel_region[["selected"]], "selected"] <- TRUE
    
    ggplot(df, aes(x = col, y = row , fill = value, shape = selected)) +
      geom_tile(colour = "black", linetype = 2) + cool_theme + ggtitle(df[["exp_run"]][1]) +
      geom_point(size = 6) +
      scale_x_discrete("Column") + scale_y_discrete("Row") +
      scale_fill_discrete("Value") +
      scale_shape_manual(guide = FALSE, values = c(NA, 18)) +
      guides(fill = guide_legend(override.aes = list(shape = NA))) +
      theme(panel.border = element_blank(),
            panel.background = element_blank())
  })
  
  output[["plot_panel_brush"]] <- renderPrint({
    dat <- plot_panel_dat()
    if(!is.null(plot_panel_region[["selected"]]))
      dat[as.numeric(plot_panel_region[["selected"]]), "selected"] <- TRUE
    
    epilogue <- list(strong("Double-click"), "partition on the chart to learn its properties.", br()) 
    
    prologue <- if(is.null(plot_panel_region[["selected"]])) {
      list()
    } else {
      dat <- dat[dat[["selected"]] == TRUE, ]
      list("Row: ", as.character(dat[["row"]]), br(), 
           "Column: ", as.character(dat[["col"]]), br())
    }
    
    do.call(p, c(prologue, epilogue))
  })
  
  
  #report download ---------------------------------------------------
  output[["report_download_button"]] <- downloadHandler(
    filename  = "dpcReport.html",
    content = function(file) {
      knitr::knit(input = "report_template.Rmd", 
                  output = "dpcReport.md", quiet = TRUE)
      markdown::markdownToHTML("dpcReport.md", file, stylesheet = "report.css", 
                               options = c('toc', markdown::markdownHTMLOptions(TRUE)))
    })
  
  
  
  #input data table, may be scrapped ----------------------------------
  output[["input_data"]] <- renderTable({
    new_dat <- change_data(input_dat(), as.factor(rep_names_new()), as.factor(exp_names_new()))
    #new_dat <- input_dat
    storage.mode(new_dat) <- "integer"
    #colnames(new_dat) <- paste0(exp_names_new(), "; ", rep_names_new())
    slot(new_dat, ".Data")
  })
  
  
})