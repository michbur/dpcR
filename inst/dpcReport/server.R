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
  
  #information if input file is loaded
  output[["input_information"]] <- renderPrint({
    if(is.null(input[["input_file"]])) {
      p("No input detected. Example data loaded.")
    } else {
      p("Detected input file: ", strong(input[["input_file"]][["name"]]), ".")
    }
  })
  
  
  output[["input_data"]] <- renderTable({
    new_dat <- change_data(input_dat(), as.factor(rep_names_new()), as.factor(exp_names_new()))
    #new_dat <- input_dat
    storage.mode(new_dat) <- "integer"
    #colnames(new_dat) <- paste0(exp_names_new(), "; ", rep_names_new())
    slot(new_dat, ".Data")
  })
  
  output[["summary_input"]] <- renderDataTable({
    new_dat <- change_data(input_dat(), as.factor(rep_names_new()), as.factor(exp_names_new()))
    #new_dat <- input_dat()
    summary(new_dat, print = FALSE)[["summary"]]
  })
  
  summary_plot_dat <- reactive({
    new_dat <- change_data(input_dat(), as.factor(rep_names_new()), as.factor(exp_names_new()))
    
    summ <- summary(new_dat, print = FALSE)[["summary"]]
    summ[summ[["method"]] == "bhat", ]
  })
  
  
  output[["summary_plot"]] <- renderPlot({
    summ <- summary_plot_dat()
    dat <- cbind(summ, selected = rep(FALSE, nrow(summary_plot_dat())))
    dat[as.numeric(summary_point[["selected"]]), "selected"] <- TRUE
    
    print(dat)
    ggplot(dat, aes(x = experiment, y = lambda, colour = selected, shape = selected,
                    ymax = lambda.up, ymin = lambda.low)) +
      geom_point(size = 5, alpha = 0.5) + cool_theme +
      scale_x_discrete("Experiment name") +
      scale_y_continuous(expression(lambda)) + 
      scale_color_discrete(guide = FALSE) +
      scale_shape_discrete(guide = FALSE) #+ geom_errorbar(width = nlevels(dat[["experiment"]])/100)
  })
  
  # summary_point <- NULL
  
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
  
  output[["summary_plot_dbl"]] <- renderPrint({
    summ <- summary_plot_dat()
    dat <- cbind(summ, selected = rep(FALSE, nrow(summary_plot_dat())))
    dat[as.numeric(summary_point[["selected"]]), "selected"] <- TRUE
    
    if(is.null(summary_point[["selected"]])) {
      p("Double-click point to learn its properties.")
    } else {
      dat <- dat[dat[["selected"]] == TRUE, ]
      p("Experiment name: ", as.character(dat[["experiment"]]), ".", br(), 
        "Lambda: ", round(dat[["lambda"]], app_digits), ".")
    }
    
  })
  
  
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
  
})