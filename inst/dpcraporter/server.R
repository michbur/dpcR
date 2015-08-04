library(shiny)
library(dpcR)

input_dat <- sim_adpcr(m = 456, n = 765, times = 100, 
                 pos_sums = FALSE, n_panels = 5)

exp_names <- slot(input_dat, "exper")

rep_names <- slot(input_dat, "replicate")


exp_rep_choice <- c(list(title = "Experiment(s) names"), 
                lapply(1L:length(exp_names), function(single_exp_id)
                  textInput(inputId = paste0("experiment_name", single_exp_id), 
                            label = paste0("Column", single_exp_id), value = exp_names[single_exp_id])),
                lapply(1L:length(rep_names), function(single_rep_id)
                  textInput(inputId = paste0("rep_name", single_rep_id), 
                            label = paste0("Column", single_rep_id), value = rep_names[single_rep_id])))



change_data <- function(input_dat, rep_names_new, exp_names_new) {
  new_dat <- input_dat
  slot(new_dat, "replicate") <- rep_names_new
  slot(new_dat, "exper") <- exp_names_new
  colnames(new_dat) <- paste0(exp_names_new, "; ", rep_names_new)
  new_dat
}



shinyServer(function(input, output) {
  
  exp_names_new <- reactive(sapply(1L:length(exp_names), function(single_exp_id)
    input[[paste0("experiment_name", single_exp_id)]]))
  
  rep_names_new <- reactive(sapply(1L:length(rep_names), function(single_rep_id)
    input[[paste0("rep_name", single_rep_id)]]))
  
  dat <- reactive({
  })
  
  reactive(colnames(dat) <- paste0(exp_names_new(), "; ", rep_names_new()))
  
  output[["input_data"]] <- renderTable({
    new_dat <- change_data(input_dat, as.factor(rep_names_new()), as.factor(exp_names_new()))
    storage.mode(new_dat) <- "integer"
    colnames(new_dat) <- paste0(exp_names_new(), "; ", rep_names_new())
    slot(new_dat, ".Data")
  })
  
  output[["read_exp_names"]] <- renderPrint({
    new_dat <- change_data(input_dat, as.factor(rep_names_new()), as.factor(exp_names_new()))
    summary(new_dat)
  })
  
  
  output[["dynamic_tabset"]] <- renderUI({
    tabsetPanel(
      tabPanel("Input data", tableOutput("input_data")),
      tabPanel("Input data2", verbatimTextOutput("read_exp_names")),
      do.call(tabPanel, exp_rep_choice)
    )
  })
  
})