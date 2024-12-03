# Set seed for reproducibility
set.seed(123)

# Simulate X
n <- 1000  # number of observations
X <- rnorm(n, mean=0, sd=1)  # X ~ N(0,1)

#plot x graph to show the normal distribution
plot(X)
hist(X, main = "Histogram of X", xlab = "X", col = "lightblue", border = "black")

# Define beta coefficients
beta0 <- 0  # intercept
beta1 <- 1  # slope

# Calculate probabilities using logistic function
pi_x <- 1 / (1 + exp(-(beta0 + beta1 * X)))

# Simulate Y based on probabilities pi_x
Y <- rbinom(n, 1, pi_x)

# Fit logistic regression model
model <- glm(Y ~ X, family = binomial(link = "logit"))

# Summary of the model
summary(model)

# Plot the data and the fitted probabilities
plot(X, Y, col = "red", pch = 20, cex = 0.6, main = "Logistic Regression Fit",
     xlab = "X", ylab = "Probability of Y = 1")
curve( 1 / (1 + exp(-(beta0 + beta1 * x))), add = TRUE, col = "green", lwd = 2)
curve(1 / (1 + exp(-(coef(model)[1] + coef(model)[2] * x))), add = TRUE, col = "blue", lwd = 2)