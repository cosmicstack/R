#Data
data <- read.csv('/Users/krishanth/Desktop/CO2_emission.csv')
x <- data$X1990[!is.na(data$X1990)]
y <- data$X2000[!is.na(data$X1990)]
plot(x, y)