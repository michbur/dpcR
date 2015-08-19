ppp_data <- dpcR:::create_ppp(data_vector = single_array, nx_a = ncol(single_array), 
                              ny_a = nrow(single_array), marks = TRUE, plot = FALSE)

res <- spatstat::quadrat.test(ppp_data, nx = input[["nx"]], ny = input[["ny"]],
                              "two.sided", "Chisq", TRUE, nsim = 1999)
