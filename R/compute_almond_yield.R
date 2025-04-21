#' Calculate the Almond Yield Anomaly given Climate Variables
#'
#' @param data a data frame containing minimum temperature in degrees C, precipitation in millimeters, month, and year
#'  @param btemp (num) Regression coefficient for average minimum temperature in degrees celsius in February
#'  @param btemp2 (num) Regression coefficient for average monthly minimum temperature squared in degrees celsius in February
#'  @param bprecip (num) Regression coefficient for total monthly precipitation in millimeters in January
#'  @param bprecip2 (num) Regression coefficient for total monthly precipitation squared in millimeters in January
#'  @param b (num) Regression coefficient intercept 
#'
#' @returns The minimum, maximum, and average almond yield in ton/acre. 
#' @export 
#'
#' @examples almond_yield(data = climate)
compute_almond_yield <- function(data, btemp = -0.015, btemp2 = -0.0046, 
                         bprecip = -0.07, bprecip2 = 0.0043, 
                         b = 0.28) {
  
  # Turn off messages from summarize
  options(dplyr.summarise.inform = FALSE)
  
  # Group by year and month to get monthly averages
  monthly <- data %>% 
    group_by(year, month) %>% 
    summarise(tmin_avg = mean(tmin_c),  
              precip_total = sum(precip)) %>% # you don't want average precip, you want total for the month!
    ungroup()
  
  # Filter to january and february because those are the months used in the paper
  jan_data <- monthly %>% 
    filter(month == 1) %>% 
    select(year, precip_total)
  
  feb_data <- monthly %>% 
    filter(month == 2) %>% 
    select(year, tmin_avg)
  
  # Join just these two together by year
  clean_monthly <- left_join(feb_data, jan_data, by = "year")
  
  # Clean the names 
  names(clean_monthly) <- c("year", "tmin", "precip")
  
  # Function for calculating almond yield
  yield <- function(tmin, precip) {
    y <- (btemp * tmin + btemp2 * tmin^2 + bprecip * precip + bprecip2 * precip^2 + b)
    return(y)
  }
  
  # Apply almond field function across input data frame
  clean_monthly$almond_yield <- with(clean_monthly, yield(tmin, precip))
  clean_monthly <- clean_monthly %>% 
    select(year, almond_yield)
  return(clean_monthly)
  
  # Summarize data
  # return(c(round(min(clean_monthly$almond_yield), 2), 
  #          round(mean(clean_monthly$almond_yield), 2), 
  #          round(max(clean_monthly$almond_yield),2)))
}

