#functions and constants for server

size_mod <- 2
cool_theme <- theme(plot.background=element_rect(fill = "transparent",
                                                 colour = "transparent"),
                    panel.grid.major = element_line(colour="lightgrey", linetype = "dashed"),
                    panel.background = element_rect(fill = "white", colour = "black"),
                    legend.background = element_rect(fill="NA"),
                    legend.position = "bottom",
                    axis.text = element_text(size = 12 + size_mod),
                    axis.title.x = element_text(size = 15 + size_mod, vjust = -0.1), 
                    axis.title.y = element_text(size = 15 + size_mod, vjust = 1),
                    strip.text = element_text(size = 15 + size_mod, face = "bold"),
                    strip.background = element_rect(fill = "#9ecae1", colour = "black"),
                    legend.text = element_text(size = 12 + size_mod), 
                    legend.title = element_text(size = 15 + size_mod),
                    legend.key = element_rect(fill = "white", colour = "black", linetype = "dashed", size = 0.5),
                    plot.title = element_text(size = 20 + size_mod))

app_digits <- 4
# nx_a <- 45
# ny_a <- 17

change_data <- function(input_dat, rep_names_new, exp_names_new) {
  new_dat <- input_dat
  slot(new_dat, "replicate") <- rep_names_new
  slot(new_dat, "exper") <- exp_names_new
  colnames(new_dat) <- paste0(exp_names_new, ".", rep_names_new)
  new_dat
}

#capitalize first letter
cap1L <- function(x)
  paste0(toupper(substr(x, 0, 1)), substr(x, 2, nchar(x)))

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
        chosen_y <- levels(data[[1]])[round(db_id[["y"]], 0)]
        #which lambda was chosen
        #clicked lambda 
        clicked_x <- db_id[["x"]]
        diff_order <- order(abs(data[[2]] - clicked_x))
        row_id <- diff_order[which.max(data[[1]][diff_order] == chosen_y)]
        chosen_x <- data[row_id, 2]
        #indirect row_id, because we need the exact location, not relative id of the row
        #in the subset
      }
      
    } else {
      if(is.factor(data[[2]])) {
        #which experiment was chosen
        chosen_x <- levels(data[[2]])[round(db_id[["x"]], 0)]
        #which lambda was chosen
        #clicked lambda 
        clicked_y <- db_id[["y"]]
        diff_order <- order(abs(data[[1]] - clicked_y))
        row_id <- diff_order[which.max(data[[2]][diff_order] == chosen_x)]
        chosen_y <- data[row_id, 2]
        #indirect row_id, because we need the exact location, not relative id of the row
        #in the subset
      }
    }
    c(x = chosen_x, y = chosen_y, row = row_id)
  } else {
    NULL
  }
}

carefully_round <- function(x_id, x_range) {
  len_range <- length(x_range)
  rx <- round(x_id, 0)
  if(rx == 0)
    rx <- 1
  if(rx == len_range + 1)
    rx <- len_range
  x_range[rx]
}

choose_xy_region <- function(brush_id, data) {
  if(is.null(brush_id)) {
    NULL
  } else {
    xmin <- carefully_round(brush_id[["xmin"]], levels(data[[1]]))
    xmax <- carefully_round(brush_id[["xmax"]], levels(data[[1]]))
    ymin <- carefully_round(brush_id[["ymin"]], levels(data[[2]]))
    ymax <- carefully_round(brush_id[["ymax"]], levels(data[[2]]))
    x_range <- levels(data[[1]])[char2numeric(xmin):char2numeric(xmax)]
    y_range <- levels(data[[2]])[char2numeric(ymin):char2numeric(ymax)]
    x <- data[[1]] %in% as.factor(x_range)
    y <- data[[2]] %in% as.factor(y_range)
    x & y
  }
}

char2numeric <- function(x) 
  if(is.na(as.numeric(x))) {
    which(letters == tolower(x))
  } else {
    as.numeric(x)
  }


options(DT.options = list(dom = "Brtip",
                          buttons = c("copy", "csv", "excel", "print")
))

my_DT <- function(x)
  datatable(x, escape = FALSE, extensions = 'Buttons', 
            filter = "top", rownames = FALSE)

merge_dpcr <- function(a, b) {
  slot_names <- slotNames(a)
  slot_names <- slot_names[!(slot_names %in% c(".Data", "type", 
                                               "col_names", "row_names",
                                               "col_id", "row_id"))]
  for(i in slot_names)
    slot(a, i) <- slot(b, i)
  #update names of runs if exper or run were changed
  colnames(a) <- colnames(b)
  a
}

unorder_df <- function(df)
  data.frame(lapply(df, function(i)
    if(is.ordered(i)) {
      factor(i, ordered = FALSE)
    } else {
      i
    }))
