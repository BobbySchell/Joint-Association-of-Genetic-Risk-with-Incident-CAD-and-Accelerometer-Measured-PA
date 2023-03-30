library(rms)

# Create a sample dataset
set.seed(123)
x <- rnorm(100, mean = 50, sd = 10)
y <- rnorm(100, mean = 5 + 0.1 * x + sin(x), sd = 1)
data <- data.frame(x = x, y = y)

# Create a restricted cubic spline with 3 knots using rcs()
spline <- rcs(x, 3)

# Add the spline to the dataset
data$spline <- spline

# Plot the data and the spline
plot(y ~ x, data = data)
lines(spline, col = "red")



# Create a sample dataset
set.seed(123)
x <- rnorm(100, mean = 50, sd = 10)
y <- rnorm(100, mean = 5 + 0.1 * x + sin(x), sd = 1)
data <- data.frame(x = x, y = y)

# Create three knots evenly spaced along the range of x
knots <- seq(min(x), max(x), length.out = 4)[2:3]

# Create a matrix of cubic spline basis functions for the knots
basis <- outer(x, knots, FUN = function(x, knot) pmax(x - knot, 0)^3)

# Fit a linear regression model using the basis functions
fit <- lm(y ~ basis)

# Predict the values of y using the fitted model
pred <- predict(fit)

# Plot the data and the spline
plot(y ~ x, data = data)
lines(x, pred, col = "red")




library(rms)

# Create a sample dataset
set.seed(123)
x <- rnorm(100, mean = 50, sd = 10)
z <- rbinom(100, size = 1, prob = 0.5)
y <- rnorm(100, mean = 5 + 0.1 * x + 2 * z + sin(x), sd = 1)
data <- data.frame(x = x, z = z, y = y)

# Create a restricted cubic spline with 3 knots interacted with z using rcs()
spline <- rcs(x, 3, interact = "z")

# Add the spline to the dataset
data$spline <- spline

# Fit a linear regression model using the spline and z as predictors
fit <- lm(y ~ spline + z, data = data)

# Predict the values of y using the fitted model
pred <- predict(fit, newdata = data)

# Plot the data and the spline
plot(y ~ x, data = data)
lines(x, pred, col = "red")


