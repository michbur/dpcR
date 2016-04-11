#calculate values of lambda (average number of molecules per partition)
calc_lambda <- function(k, n, v = 1, uv = 0) {
  p <- k/n
  p_conf <- sqrt(p * (1 - p)/n) #dube
  u_lambda <- sqrt(p/(n * (1 - p))) #bhat
  l <- fl(p)
  lower_dube <- fl(p - qnorm(0.975)*p_conf)
  upper_dube <- fl(p + qnorm(0.975)*p_conf)
  c_lower_dube <- lower_dube/v
  c_upper_dube <- upper_dube/v
  
  lower_bhat <- l - u_lambda
  upper_bhat <- l + u_lambda
  c_upper_bhat <- l/v + l/v * sqrt((1/l * sqrt(p/(n - k)))^2 + (uv/v)^2)
  c_lower_bhat <- l/v - l/v * sqrt((1/l * sqrt(p/(n - k)))^2 + (uv/v)^2)

  res <- data.frame(method = c(rep("dube", length(p)), rep("bhat", length(p))), 
                    lambda = c(l, l), 
                    lambda.low = c(lower_dube, lower_bhat), 
                    lambda.up = c(upper_dube, upper_bhat), 
                    m = l*n, m.low = c(lower_dube, lower_bhat)*n, m.up = c(upper_dube, upper_bhat)*n, 
                    c = l/v,
                    c.low = c(c_lower_dube, c_lower_bhat), 
                    c.up = c(c_upper_dube, c_upper_bhat), 
                    k = rep(k, 2), n = rep(n, 2), row.names = 1L:(length(l)*2))
  #reorder
  res[unlist(lapply(1L:(nrow(res)/2), function(i) c(i, i + nrow(res)/2))), ]
}
