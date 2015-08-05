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
                    axis.title.x = element_text(size=16 + size_mod, vjust = -1), 
                    axis.title.y = element_text(size=16 + size_mod, vjust = 1),
                    strip.text = element_text(size=17 + size_mod, face = "bold"),
                    legend.text = element_text(size=13 + size_mod), 
                    legend.title = element_text(size=17 + size_mod),
                    plot.title = element_text(size=20 + size_mod))


change_data <- function(input_dat, rep_names_new, exp_names_new) {
  new_dat <- input_dat
  slot(new_dat, "replicate") <- rep_names_new
  slot(new_dat, "exper") <- exp_names_new
  colnames(new_dat) <- paste0(exp_names_new, ".", rep_names_new)
  new_dat
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
  
  output[["summary_plot"]] <- renderPlot({
    new_dat <- change_data(input_dat(), as.factor(rep_names_new()), as.factor(exp_names_new()))
    
    summ <- summary(new_dat)[["summary"]]
    summ <- summ[summ[["method"]] == "bhat", c("experiment", "replicate", "method", "lambda", "k", "n")]
    ggplot(summ, aes(x = experiment, y = lambda, colour = replicate)) +
      geom_point(size = 4, position = "dodge") + cool_theme +
      scale_x_discrete("Experiment name") +
      scale_y_continuous(expression(lambda)) +
      scale_color_discrete("Replicate ID")
  })
  
  output[["dbl_info"]] <- renderPrint({
    str(input$plot_dbl)
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