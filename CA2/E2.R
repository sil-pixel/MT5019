library("knitr")
library("ggplot2")
library("dplyr")




logdos<-c(-7.60,-6.22,-4.60,-3.00,-1.39,0.92)
n<-c(18,19,28,32,28,40)
x<-c(1,2,4,9,12,32)
data22 <- data.frame(logdos,x,n)
data22


# Data
log_dose <- c(-7.60, -6.22, -4.60, -3.00, -1.39, 0.92)
tumor_count <- c(1, 2, 4, 9, 12, 32)
no_tumor_count <- c(17, 17, 24, 23, 16, 8)
total_count <- tumor_count + no_tumor_count

# Compute risk (probability of tumor)
risk <- tumor_count / total_count

# Compute log-odds
log_odds <- log(risk / (1 - risk))

# Create a data frame
data <- data.frame(
  log_dose = log_dose,
  risk = risk,
  log_odds = log_odds
)

# Plot risk vs. log(dose)
ggplot(data, aes(x = log_dose, y = risk)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 3) +
  ggtitle("Risk vs. Log(Dose)") +
  xlab("Log(Dose)") +
  ylab("Risk") +
  theme_minimal()

# Plot log-odds vs. log(dose)
ggplot(data, aes(x = log_dose, y = log_odds)) +
  geom_line(color = "red", size = 1) +
  geom_point(color = "red", size = 3) +
  ggtitle("Log-Odds vs. Log(Dose)") +
  xlab("Log(Dose)") +
  ylab("Log-Odds") +
  theme_minimal()

