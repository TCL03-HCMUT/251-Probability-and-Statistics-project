library(caTools)
library(pscl)
library(caret)
library(PRROC)
library(pROC)
library(ggplot2)



# Make this reproducible
set.seed(1)

data <- read.csv(file = "preprocessed.csv", header = TRUE, sep = ",")


split <- sample.split(data, SplitRatio = 0.8)

training <- subset(data, split == "TRUE")
testing <- subset(data, split == "FALSE")


# Fitting models
model1 <- glm(Is_Ad ~ Height + Width, training,
    family = binomial()
) # only Height + Width

model2 <- glm(Is_Ad ~ Height + Width + Aspect_ratio, training,
    family = binomial()
) # adding aspect ratio

model3 <- glm(Is_Ad ~ Height + Width + Aspect_ratio + Local, training,
    family = binomial()
) # adding local


summary(model1)
summary(model2)
summary(model3)


# Compare models using anova
# Does Aspect_ratio improve the Height+Width model?
anova(model1, model2)

# Does Local improve the Height+Width+Aspect_ratio model?
anova(model2, model3)

# Choose final model as model2
model_final <- model2

# Use vif to check for multicollinearity:
library(car)
vif(model_final)

# Assess importance of each predictor by 
# dropping one at a time and measure how worse the model is
drop1(model_final, test = "Chisq")

# Confidence intervals for the coefficients of predictors
confint(model_final)

# Get odd ratios of each predictor
# OR > 1: increases odds of positive outcome
# OR < 1: decreases odds of positive outcome
exp(coef(model_final))


# Make a prediction object
probs <- predict(model_final, newdata = testing, type = "response")


library(caret)

predicted <- as.factor(ifelse(probs > 0.5, 1, 0))
actual <- as.factor(testing$Is_Ad)

# Print confusion matrix with metrics
cm <- confusionMatrix(predicted, actual, positive = "1")
print(cm)

# Evaluate ROC curve
roc_curve <- roc(testing$Is_Ad, probs)

library(ggplot2)
ggplot(data = data.frame(FPR = 1 - roc_curve$specificities, 
                         TPR = roc_curve$sensitivities), aes(x = FPR, y = TPR)) +
  geom_line(color = "blue", size = 1.2) +
  geom_abline(linetype = "dashed", color = "gray") +
  labs(title = "ROC Curve", x = "False Positive Rate", y = "True Positive Rate") +
  theme_minimal()

# Area under ROC curve
auc(roc_curve)

# Predict a few variables
new <- data.frame(
    Height = c(100, 300),
    Width = c(600, 600),
    Aspect_ratio = c(6, 2)
)

pred_link <- predict(model_final, newdata = new, type = "link", se.fit = TRUE)
eta <- as.numeric(pred_link$fit)
se_eta <- as.numeric(pred_link$se.fit)
z <- qnorm(0.975)

# CI on link (eta) scale
eta_ci_lower <- eta - z * se_eta
eta_ci_upper <- eta + z * se_eta

# Probability (p) and CI by transforming eta CI (recommended)
p <- plogis(eta)
p_ci_by_transformation <- cbind(
  lower = plogis(eta_ci_lower),
  upper = plogis(eta_ci_upper)
)

# Put results together
results <- data.frame(
    new = new,
    eta = eta,
    eta_lower = eta_ci_lower,
    eta_upper = eta_ci_upper,
    p = p,
    p_lower = p_ci_by_transformation[, "lower"],
    p_upper = p_ci_by_transformation[, "upper"]
)

print(results)