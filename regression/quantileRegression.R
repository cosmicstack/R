# generate data with non-constant variance

x <- seq(0,100,length.out = 100)        # independent variable
sig <- 0.1 + 0.05*x                     # non-constant variance
b_0 <- 6                                # true intercept
b_1 <- 0.1                              # true slope
set.seed(1)                             # make the next line reproducible
e <- rnorm(100,mean = 0, sd = sig)      # normal random error with non-constant variance
y <- b_0 + b_1*x + e                    # dependent variable
plot(x, y)

lmodel <- lm(y ~ x)
lmodel$coefficients
lines()