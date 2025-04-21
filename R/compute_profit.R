#' Compute profit generation from almond yield
#' @param yield (num) Almond yield outputted from previous function in tons/acre 
#' @param  price (num) Current almond price ($/ton)
#' @param  year (num) Year of almond yield 
#' @param discount (num) rate (default 0.12)
#' @return data frame with estimate of profit
compute_profit_from_almond <- function(yield, year, price, discount = 0.12) {
  
  # ADD EQUIVALENT UNIT TESTS FOR ALMONDS??
  # make sure values are reasonable
  # if (length(energy) < 1) {
  #   return(NA)
  # }
  # 
  # # energy cannot be negative
  # if (min(energy) < 0) {
  #   return(NA)
  # }
  
  
  # generate a unique identifier or scenario number for each yield and year
  scen <- seq(from = 1, to = length(yield))
  yearprofit <- data.frame(scen = scen, yield = yield, year = year)
  yearprofit$net_profit <- yearprofit$yield * price
  
  # Discounting using the NPV function
  # We normalize the year to start year (the first year)
  yearprofit <- yearprofit %>%
    mutate(netpresent_profit = compute_NPV(value = net_profit, time = year - year[1], discount = discount))
  
  return(yearprofit)
}
