#' Run Almond Profit Model
#'
#' @param data (data frame) climate data containing yearly almond yield in ton/acre
#' @param price almond price ($/ton)
#' @param discount discount rate
#' @param data a data frame containing minimum temperature in degrees C, precipitation in millimeters, month, and year
#'  @param btemp (num) Regression coefficient for average minimum temperature in degrees celsius in February
#'  @param btemp2 (num) Regression coefficient for average monthly minimum temperature squared in degrees celsius in February
#'  @param bprecip (num) Regression coefficient for total monthly precipitation in millimeters in January
#'  @param bprecip2 (num) Regression coefficient for total monthly precipitation squared in millimeters in January
#'  @param b (num) Regression coefficient intercept 
#' @return data frame with year and discounted profit
run_almond_profit_model <- function(data, price = 4000, discount = 0.12, 
                                    btemp = -0.015, btemp2 = -0.0046, 
                                    bprecip = -0.07, bprecip2 = 0.0043, 
                                    b = 0.28) {
  yield_df <- compute_almond_yield(data, btemp = btemp, btemp2 = btemp2, 
                           bprecip = bprecip, bprecip2 = bprecip2, 
                           b = b)
  profit_df <- compute_profit_from_almond(
    yield = yield_df$almond_yield,
    year = yield_df$year,
    price = price,
    discount = discount
  )
  return(profit_df)
}
