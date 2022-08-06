set.seed(73)
# Data Generation
x <- rnorm(100) 
y <- rnorm(100, sin(x)/exp(0.25*x), 0.1) + 0.1*rnorm(1)
plot(x, y)

#Kernel Definitions
uniform_kernel <- function(u) {
  0.5 * c(abs(u) < 1)
}

gaussian_kernal <- function(u) {
  sqrt(1/(2*pi))*exp(-0.5*u^2)
}

epanechnikov_kernel <- function(u) {
  0.75*(1-u^2) * c(abs(u) < 1)
}

biweight_kernel <- function(u) {
  (15/16)*(1-u^2)^2 * c(abs(u) < 1)
}

#Weight Function
mask <- function(x_val, x_vec, h, kernal) {
  kernal((x_val - x_vec)/h)
}

#Nadraya-Watson Estimator
nw_est <- function(y, x_val, x_vec, h) {
  sum(y*mask(x[i], x_vec, 0.1, gaussian_kernal))/sum(mask(x[i], x_vec, 0.1, gaussian_kernal))
}

#Trial
n <- length(x)
mx_hat <- matrix(nrow = n, ncol = 1)
for (i in 1:n) {
  mx_hat[i] <- nw_est(y, x[i], x, 0.1)
}

#Plotting estimated curve
plot(x, y, col='grey')
lines(x[order(x)], mx_hat[order(x)], type='l', col='blue', lty=2)

#leave-one-out cross validation for the best h
hs <- seq(0.01,1,by=0.01)
rh <- matrix(nrow = length(hs), ncol = 1)
for (i in 1:length(hs)) {
  mx_i <- matrix(nrow = n, ncol = 1)
  for (j in 1:n) {
    if (j == 1) {
      mx_i[j] <- nw_est(y[2:n], x[j], x[2:n], hs[i])
    } else if (j == n) {
      mx_i[j] <- nw_est(y[1:(n-1)], x[j], x[1:(n-1)], hs[i])
    } else {
      mx_i[j] <- nw_est(y[c(c(seq(1, (j-1))), c(seq((j+1), n)))], x[j], x[c(c(seq(1, (j-1))), c(seq((j+1), n)))], hs[i])
    }
  }
  mx_i[is.na(mx_i)] <- 0 
  rh[i] <- sum((y - mx_i)^2)
}
plot(hs, rh, type='l')
h_new <- hs[which(rh == min(rh))]

#Find new fitted curve with new h
mx_hat_new <- matrix(nrow = n, ncol = 1)
wsq <- matrix(nrow = n, ncol = 1)
for (i in 1:n) {
  wsq[i] <- sum(mask(x[i], x, h_new, gaussian_kernal)^2)
  mx_hat_new[i] <- nw_est(y, x[i], x, h_new)
}

#Plot new fitted curve
plot(x, y, col='grey')
lines(x[order(x)], mx_hat_new[order(x)], type='l', col='blue', lty=2)

#Residual plot
res <- y - mx_hat_new
plot(x, res)

#Prediction with confidence interval
s <- sum(res^2)/(length(res)-1)
var_mx <- s*wsq
lowband <- mx_hat_new + qnorm(0.025)*sqrt(var_mx)
highband <- mx_hat_new + qnorm(0.975)*sqrt(var_mx)

#Plotting the predicted band
plot(x, y, col='grey')
lines(x[order(x)], lowband[order(x)], type='l', lty=2)
lines(x[order(x)], mx_hat_new[order(x)], type='l')
lines(x[order(x)], highband[order(x)], type='l', lty=2)