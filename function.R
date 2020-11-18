library(ggplot2)
library(devtools)
library(usethis)
library(roxygen2)
airtemps <- c(212, 30.3, 78, 32)

#long method
#a good sign of when to use a function is when you are doing a lot of copying and pasting 
celsius1 <- (airtemps [1] - 32) * 5/9
celsius2 <- (airtemps [2] - 32) * 5/9
celsius3 <- (airtemps [3] - 32) * 5/9

fahr_to_celsius <- function(fahr) {
  (fahr - 32) * 5/9
}

#things included in the curly brackets is the function 

celsius4 <- fahr_to_celsius(airtemps[1])
celsius1 == celsius4

airtemps_c <- fahr_to_celsius(airtemps)

#' Title
#'
#' @param cel Temperature in Celsius
#' @keywords temperature conversions
#' @return Temperature in Fahrenheit
#' @export 
#'
#' @examples
#' fahr_to_celsius(32)
#' fahr to celsius(c(32,313,76))

cel_to_fahr <- function(cel) {
  (cel * 9/5) + 32
}

airtemps_f <- cel_to_fahr(airtemps_c)
airtemps_f == airtemps

#Roxygen skeleton - under the wand button

#Building a longer more complicated function that can convert temps to Kelvin, Celsius, and Faherenheit

convert_temps <- function(fahr) {
  celsius <- (fahr -32) * 5/9
  kelvin <- celsius + 273.15
  
  list("fahr" = fahr, "celsius" = celsius, "kelvin" = kelvin)
}


temp_series <- convert_temps(seq(-100, 100, 10))
temps_df <- data.frame(temp_series)


custom_theme <- function(base_size = 9) {
  ggplot2::theme(
    axis.ticks       = ggplot2::element_blank(),
    text             = ggplot2::element_text(family = 'Helvetica', color = 'gray30', size = base_size),
    plot.title       = ggplot2::element_text(size = ggplot2::rel(1.25), hjust = 0.5, face = 'bold'),
    panel.background = ggplot2::element_blank(),
    legend.position  = 'right',
    panel.border     = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(colour = 'grey90', size = .25),
    legend.key       = ggplot2::element_rect(colour = NA, fill = NA),
    axis.line        = ggplot2::element_blank()
  )
}

ggplot(temps_df, mapping=aes(x=fahr, y=celsius, color=kelvin)) +
  geom_point() +
  custom_theme(10)

scatterplot<- function (df, point_size = 2, font_size = 9) {
  ggplot(df, mapping = aes(x- fahr, y = celsius , color = kelvin))
}

scatterplot(temps_df)
scatterplot(temps_df, 10, 20)


