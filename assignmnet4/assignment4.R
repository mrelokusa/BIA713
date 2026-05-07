# ============================================================
# BIA713 Assignment 4 - 2026
# Student: Zondi, ENZ | Student Number: 4346549
# Section A only
# ============================================================

dir.create("output", showWarnings = FALSE)

results_path <- file.path("output", "assignment4_results.txt")
sink(results_path, split = TRUE)

cat("BIA713 Assignment 4 Results\n")
cat("===========================\n\n")

# Load StudentPerformance population and recreate the 25% sample
sp <- read.csv("../StudentPerformance.csv")

set.seed(4346549)
sample_sp <- sp[sample(nrow(sp), 250), ]
sample_sp$prep_completed <- ifelse(
  sample_sp$Test_preparation_course == "completed",
  1,
  0
)

write.csv(
  sample_sp,
  file.path("output", "BIA713_assignment4_sample_A_4346549.csv"),
  row.names = FALSE
)

cat("Sample information\n")
cat("------------------\n")
cat(sprintf("Population size: %d\n", nrow(sp)))
cat(sprintf("Sample size: %d\n", nrow(sample_sp)))
cat("Test preparation count:\n")
print(table(sample_sp$Test_preparation_course))
cat("\nMath score summary by test preparation group:\n")
print(tapply(sample_sp$Math_score, sample_sp$Test_preparation_course, summary))
cat("\n\n")

# Question 1 ------------------------------------------------------------------
# Relationship between test preparation course and mathematics score
q1_model <- lm(Math_score ~ prep_completed, data = sample_sp)

png(file.path("output", "A4_Q1.1_testprep_math_lm.png"), width = 900, height = 560)
x_jit <- jitter(sample_sp$prep_completed, amount = 0.06)
plot(
  x_jit,
  sample_sp$Math_score,
  xaxt = "n",
  xlim = c(-0.25, 1.25),
  xlab = "Test preparation course",
  ylab = "Mathematics score",
  main = "Test Preparation Course and Mathematics Score",
  pch = 16,
  col = rgb(0.10, 0.23, 0.36, 0.45)
)
axis(1, at = c(0, 1), labels = c("none", "completed"))
abline(q1_model, col = "#b3202a", lwd = 2)
group_means <- tapply(sample_sp$Math_score, sample_sp$prep_completed, mean)
points(c(0, 1), group_means, pch = 18, cex = 1.8, col = "#1b7f4b")
legend(
  "topleft",
  legend = c("Sample student", "Least squares line", "Group mean"),
  col = c(rgb(0.10, 0.23, 0.36, 0.45), "#b3202a", "#1b7f4b"),
  pch = c(16, NA, 18),
  lty = c(NA, 1, NA),
  lwd = c(NA, 2, NA),
  bty = "n"
)
dev.off()

cat("Question 1 model: Math_score ~ prep_completed\n")
cat("-----------------------------------------------\n")
print(summary(q1_model))
cat("\nConfidence intervals for Question 1 coefficients:\n")
print(confint(q1_model))
cat("\n\n")

# Question 2 ------------------------------------------------------------------
# Logistic regression predicting completion of the test preparation course
q2_full <- glm(
  prep_completed ~ Math_score + Reading_score + Writing_score,
  family = binomial,
  data = sample_sp
)

vif_calc <- function(data, predictors) {
  values <- numeric(length(predictors))
  names(values) <- predictors
  for (v in predictors) {
    other_vars <- setdiff(predictors, v)
    f <- as.formula(paste(v, "~", paste(other_vars, collapse = " + ")))
    r2 <- summary(lm(f, data = data))$r.squared
    values[v] <- 1 / (1 - r2)
  }
  values
}

score_vars <- c("Math_score", "Reading_score", "Writing_score")
score_cor <- cor(sample_sp[, score_vars])
score_vif <- vif_calc(sample_sp, score_vars)

cat("Question 2 full logistic model\n")
cat("------------------------------\n")
print(summary(q2_full))
cat("\nCorrelations among score predictors:\n")
print(round(score_cor, 4))
cat("\nVIF values for full model predictors:\n")
print(round(score_vif, 4))
cat("\n\n")

# Reading and writing are highly correlated. Keep writing because it gives the
# stronger reduced model with mathematics, and remove reading from the final model.
q2_final <- glm(
  prep_completed ~ Math_score + Writing_score,
  family = binomial,
  data = sample_sp
)

final_vars <- c("Math_score", "Writing_score")
final_vif <- vif_calc(sample_sp, final_vars)

cat("Question 2 final logistic model\n")
cat("-------------------------------\n")
print(summary(q2_final))
cat("\nVIF values for final model predictors:\n")
print(round(final_vif, 4))
cat("\nTransformed final model coefficients (odds ratios):\n")
print(round(exp(coef(q2_final)), 4))
cat("\n95% confidence intervals for odds ratios:\n")
print(round(exp(confint.default(q2_final)), 4))
cat("\n\n")

roc_curve <- function(actual, probability) {
  thresholds <- c(Inf, sort(unique(probability), decreasing = TRUE), -Inf)
  positives <- sum(actual == 1)
  negatives <- sum(actual == 0)
  tpr <- sapply(thresholds, function(t) {
    sum(probability >= t & actual == 1) / positives
  })
  fpr <- sapply(thresholds, function(t) {
    sum(probability >= t & actual == 0) / negatives
  })
  data.frame(threshold = thresholds, tpr = tpr, fpr = fpr)
}

auc_value <- function(actual, probability) {
  positives <- sum(actual == 1)
  negatives <- sum(actual == 0)
  ranks <- rank(probability)
  (sum(ranks[actual == 1]) - positives * (positives + 1) / 2) /
    (positives * negatives)
}

final_prob <- fitted(q2_final)
final_pred <- ifelse(final_prob >= 0.5, 1, 0)
confusion <- table(
  Actual = sample_sp$prep_completed,
  Predicted = final_pred
)
roc_final <- roc_curve(sample_sp$prep_completed, final_prob)
auc_final <- auc_value(sample_sp$prep_completed, final_prob)

png(file.path("output", "A4_Q2.5_roc_curve.png"), width = 760, height = 620)
plot(
  roc_final$fpr,
  roc_final$tpr,
  type = "l",
  lwd = 2,
  col = "#1a3a5c",
  xlim = c(0, 1),
  ylim = c(0, 1),
  xlab = "False positive rate",
  ylab = "True positive rate",
  main = "ROC Curve for Final Logistic Regression Model"
)
abline(0, 1, lty = 2, col = "grey50")
text(0.62, 0.20, labels = sprintf("AUC = %.3f", auc_final), cex = 1.1)
dev.off()

cat("ROC and classification information\n")
cat("----------------------------------\n")
cat(sprintf("AUC: %.4f\n", auc_final))
cat("Confusion matrix using a 0.50 cut-off:\n")
print(confusion)
cat(sprintf("Accuracy: %.4f\n", mean(final_pred == sample_sp$prep_completed)))
cat(sprintf(
  "Sensitivity: %.4f\n",
  sum(final_pred == 1 & sample_sp$prep_completed == 1) /
    sum(sample_sp$prep_completed == 1)
))
cat(sprintf(
  "Specificity: %.4f\n",
  sum(final_pred == 0 & sample_sp$prep_completed == 0) /
    sum(sample_sp$prep_completed == 0)
))

sink()
