#' Habitat Suitability Index calculation
#'
#' Calculates the Habitat Suitability Index (HSI) for a species of interest
#' @param std.data Forest stand, regime, and outcome information
#' @param equation User-defined equation for calculating HSI
#' @param logistic TRUE/FALSE - if True, runs a logistic function on values (assigns HSI values between 0-1)
#' @return The input stand data with an additional HSI column of values
#' @examples
#' #Read in stand and regime information
#' std.data <- read_csv("./fvs_results.csv")
#'
#' #Create function to apply equation for HSI
#' Define the slope and y-intercept of the linear equation
#' hsi.func <- function(x = std.data$BA, m = -0.02, b = 2) {
#' return(m * x + b)
#' }
#'
#' The function will create a new column with the HSI values
#' new.data <- HSIcalc(std.data, hsi.func)
#'
#' #Summarise the HSI data
#' summary(new.data$HSI)
#' #Plot data against BA to show relationship
#' ggplot(data = new.data) +
#' geom_point(aes(x = BA, y = HSI, color = HSI),
#'           size = 3) +
#'           scale_color_viridis_c(option = "plasma") +
#'           theme_classic()
#'
#' @export

HSIcalc <- function(std.data, equation, logistic = T){

  if(logistic == T){
  # Define the logistic function (setting the HSI between 0-1)
  log.func <- function(x) {
    return(1 / (1 + exp(-x)))
  }
  #Run the function
  y <- log.func(equation())
  } else{
    y <- equation()
  }
  #y <- equation()
  #convert to data frame
  y <- data.frame(y)
  #rename for HSI
  colnames(y) <- "HSI"
  #bind stand data and HSI values
  std.data.new <- cbind(std.data, y)

  return(std.data.new)

}
