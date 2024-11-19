install.packages("epitools")
install.packages("MASS")
library(epitools) # the epitools package must have been installed
library(MASS) # the MASS package must have been installed

tab1<- as.table(rbind(c(309, 191), c(319, 281)))
dimnames(tab1) <- list(gender = c("women", "men"),opinion = c("favor","against"))
addmargins(tab1)
addmargins(prop.table(tab1,1),2)


chisq.test(tab1,correct=FALSE)
loglm(~gender+opinion,tab1)
# The function loglm fits a loglinear model, but we leave the details about that.
# Loglinear models will be handled in assignment 3 but with the function glm instead of loglm.

oddsratio(tab1, method = "wald", rev="neither")
riskratio(tab1,rev="neither")
# To obtain the intended odds/risk ratio, you may need to reverse the rows or columns:
oddsratio(tab1, method = "wald", rev="row")
oddsratio(tab1, method = "wald", rev="col")
oddsratio(tab1, method = "wald", rev="both")


# exercise 2
# Load the epitools package
library(epitools)
library(MASS)

# 1) ------
tab2 <- as.table(rbind(c(1198, 1493), c(557, 1278)))
dimnames(tab2) <- list(
  gender = c("men", "women"),
  admission = c("admitted", "rejected")
)
addmargins(tab2)
addmargins(prop.table(tab2,1),2)

# tests
# Perform Chi-square test
chisq_test_result2 <- chisq.test(tab2, correct = FALSE)
chisq_test_result2

# Perform the likelihood ratio test
G2_result2 <- loglm(~ gender + admission, data = tab2)
G2_result2

# Calculate odds ratio
odds_ratio_result2 <- oddsratio(tab2, method = "wald", rev = "row")
odds_ratio_result2

# Calculate risk ratio
risk_ratio_result2 <- riskratio(tab2, rev = "row")
risk_ratio_result2
# 2) ------
tab3 <- round(tab2/10)

# Perform Chi-square test
chisq_test_result3 <- chisq.test(tab3, correct = FALSE)
chisq_test_result3

# Perform the likelihood ratio test
G2_result3 <- loglm(~ gender + admission, data = tab3)
G2_result3

# Calculate odds ratio
odds_ratio_result3 <- oddsratio(tab3, method = "wald", rev = "row")
odds_ratio_result3

# Calculate risk ratio
risk_ratio_result3 <- riskratio(tab3, rev = "row")
risk_ratio_result3

tab4 <- round(tab2/100)

# Perform Chi-square test
chisq_test_result4 <- chisq.test(tab4, correct = FALSE)
chisq_test_result4

# Perform the likelihood ratio test
G2_result4 <- loglm(~ gender + admission, data = tab4)
G2_result4

# Calculate odds ratio
odds_ratio_result4 <- oddsratio(tab4, method = "wald", rev = "row")
odds_ratio_result4

# Calculate risk ratio
risk_ratio_result4 <- riskratio(tab4, rev = "row")
risk_ratio_result4
# 3) ------

