dir.create("output", showWarnings = FALSE)

results_path <- file.path("output", "assignment3_results.txt")
sink(results_path, split = TRUE)

cat("BIA713 Assignment 3 Results\n")
cat("===========================\n\n")

# Section A -------------------------------------------------------------------
sp <- read.csv("StudentPerformance.csv")
set.seed(4346549)
sample_sp <- sp[sample(nrow(sp), 250), ]

male_math <- sample_sp$Math_score[sample_sp$Gender == "male"]
female_math <- sample_sp$Math_score[sample_sp$Gender == "female"]

var_test_a <- var.test(male_math, female_math)
ttest_a <- t.test(
  female_math,
  male_math,
  alternative = "two.sided",
  var.equal = var_test_a$p.value > 0.05,
  conf.level = 0.95
)

cat("SECTION A\n")
cat("---------\n")
cat(sprintf("Male sample:   n=%d, mean=%.4f, variance=%.4f, sd=%.4f\n",
            length(male_math), mean(male_math), var(male_math), sd(male_math)))
cat(sprintf("Female sample: n=%d, mean=%.4f, variance=%.4f, sd=%.4f\n",
            length(female_math), mean(female_math), var(female_math), sd(female_math)))
cat(sprintf("Difference (female - male) = %.4f\n\n",
            mean(female_math) - mean(male_math)))

cat("F test for equality of variances:\n")
print(var_test_a)
cat("\nTwo-sample t test for mean difference (female - male):\n")
print(ttest_a)
cat("\n\n")

# Section B -------------------------------------------------------------------
hh <- read.csv("Household.csv")
wc_pop_df <- hh[hh$PROVINCE == "WC", ]
ec_pop_df <- hh[hh$PROVINCE == "EC", ]
wc_pop <- wc_pop_df$HEADINC
ec_pop <- ec_pop_df$HEADINC

png(file.path("output", "A3_B1_variance_comparison.png"), width = 1200, height = 800)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
hist(
  wc_pop,
  main = "WC HEADINC (Population)",
  xlab = "Monthly Income (R)",
  col = "#89CFF0",
  border = "white",
  breaks = 16
)
hist(
  ec_pop,
  main = "EC HEADINC (Population)",
  xlab = "Monthly Income (R)",
  col = "#A8D5A2",
  border = "white",
  breaks = 16
)
boxplot(
  list("Western Cape" = wc_pop, "Eastern Cape" = ec_pop),
  main = "HEADINC by Province",
  ylab = "Monthly Income (R)",
  col = c("#89CFF0", "#A8D5A2"),
  border = "navy"
)
plot(
  density(wc_pop),
  col = "navy",
  lwd = 2,
  main = "Density Comparison",
  xlab = "Monthly Income (R)"
)
lines(density(ec_pop), col = "darkgreen", lwd = 2)
legend(
  "topright",
  legend = c("Western Cape", "Eastern Cape"),
  col = c("navy", "darkgreen"),
  lwd = 2,
  bty = "n"
)
dev.off()

set.seed(4346549)
wc_sample <- wc_pop_df[sample(nrow(wc_pop_df), 24), ]
ec_sample <- ec_pop_df[sample(nrow(ec_pop_df), 26), ]

ttest_b_ci <- t.test(
  wc_sample$HEADINC,
  ec_sample$HEADINC,
  alternative = "two.sided",
  var.equal = FALSE,
  conf.level = 0.95
)
ttest_b_gt <- t.test(
  wc_sample$HEADINC,
  ec_sample$HEADINC,
  alternative = "greater",
  var.equal = FALSE,
  conf.level = 0.95
)

cat("SECTION B\n")
cat("---------\n")
cat(sprintf("WC population: n=%d, mean=%.4f, variance=%.4f, sd=%.4f, IQR=%.4f, range=%.4f\n",
            length(wc_pop), mean(wc_pop), var(wc_pop), sd(wc_pop), IQR(wc_pop), diff(range(wc_pop))))
cat(sprintf("EC population: n=%d, mean=%.4f, variance=%.4f, sd=%.4f, IQR=%.4f, range=%.4f\n",
            length(ec_pop), mean(ec_pop), var(ec_pop), sd(ec_pop), IQR(ec_pop), diff(range(ec_pop))))
cat(sprintf("Variance ratio (WC / EC) = %.4f\n\n", var(wc_pop) / var(ec_pop)))

cat(sprintf("WC sample: n=%d, mean=%.4f, variance=%.4f, sd=%.4f\n",
            nrow(wc_sample), mean(wc_sample$HEADINC), var(wc_sample$HEADINC), sd(wc_sample$HEADINC)))
cat(sprintf("EC sample: n=%d, mean=%.4f, variance=%.4f, sd=%.4f\n\n",
            nrow(ec_sample), mean(ec_sample$HEADINC), var(ec_sample$HEADINC), sd(ec_sample$HEADINC)))

cat("Welch t interval for mean difference (WC - EC):\n")
print(ttest_b_ci)
cat("\nOne-sided Welch t test for WC > EC:\n")
print(ttest_b_gt)
cat("\n\n")

# Section C -------------------------------------------------------------------
er <- read.csv("Employee_Retention.csv")
years_left_pop <- er$time_spend_company[er$left == 1]
years_stayed_pop <- er$time_spend_company[er$left == 0]

f_test_c <- var.test(years_stayed_pop, years_left_pop)

set.seed(4346549)
er_sample <- er[sample(nrow(er), round(nrow(er) * 0.25)), ]
years_left_sample <- er_sample$time_spend_company[er_sample$left == 1]
years_stayed_sample <- er_sample$time_spend_company[er_sample$left == 0]

ttest_c <- t.test(
  years_stayed_sample,
  years_left_sample,
  alternative = "two.sided",
  var.equal = FALSE,
  conf.level = 0.99
)

anova_sample <- data.frame(
  time_spend_company = er_sample$time_spend_company,
  left_group = factor(er_sample$left, levels = c(0, 1), labels = c("Stayed", "Left"))
)
anova_model <- aov(time_spend_company ~ left_group, data = anova_sample)
anova_summary <- summary(anova_model)

promo_pop_table <- table(
  Left = factor(er$left, levels = c(0, 1), labels = c("Stayed", "Left")),
  Promotion = factor(
    er$promotion_last_5years,
    levels = c(0, 1),
    labels = c("Not promoted", "Promoted")
  )
)
promo_pop_chisq <- chisq.test(promo_pop_table)

promo_sample_table <- table(
  Left = factor(er_sample$left, levels = c(0, 1), labels = c("Stayed", "Left")),
  Promotion = factor(
    er_sample$promotion_last_5years,
    levels = c(0, 1),
    labels = c("Not promoted", "Promoted")
  )
)
promo_sample_chisq <- chisq.test(promo_sample_table)

png(file.path("output", "A3_C1_time_spend_boxplot.png"), width = 850, height = 520)
boxplot(
  time_spend_company ~ left_group,
  data = anova_sample,
  main = "Time Spent at Company by Employment Status (Sample)",
  xlab = "Employee status",
  ylab = "Years at company",
  col = c("#89CFF0", "#F4A7B9"),
  border = "navy"
)
dev.off()

png(file.path("output", "A3_C2_left_promo_sample.png"), width = 850, height = 520)
barplot(
  t(promo_sample_table),
  beside = TRUE,
  main = "Sample: Left by Promotion Status",
  xlab = "Promotion status",
  ylab = "Number of employees",
  col = c("#89CFF0", "#F4A7B9"),
  legend.text = rownames(promo_sample_table),
  args.legend = list(x = "topright", bty = "n"),
  border = "white"
)
dev.off()

cat("SECTION C\n")
cat("---------\n")
cat(sprintf("Population stayed group: n=%d, mean=%.4f, variance=%.4f, sd=%.4f\n",
            length(years_stayed_pop), mean(years_stayed_pop), var(years_stayed_pop), sd(years_stayed_pop)))
cat(sprintf("Population left group:   n=%d, mean=%.4f, variance=%.4f, sd=%.4f\n\n",
            length(years_left_pop), mean(years_left_pop), var(years_left_pop), sd(years_left_pop)))

cat("F test for equality of variances (population groups):\n")
print(f_test_c)
cat("\n")

cat(sprintf("Employee sample size = %d\n", nrow(er_sample)))
cat(sprintf("Sample stayed group: n=%d, mean=%.4f, variance=%.4f, sd=%.4f\n",
            length(years_stayed_sample), mean(years_stayed_sample), var(years_stayed_sample), sd(years_stayed_sample)))
cat(sprintf("Sample left group:   n=%d, mean=%.4f, variance=%.4f, sd=%.4f\n\n",
            length(years_left_sample), mean(years_left_sample), var(years_left_sample), sd(years_left_sample)))

cat("Welch t test for mean difference in time spent (stayed - left):\n")
print(ttest_c)
cat("\nANOVA table:\n")
print(anova_summary)
cat("\nPopulation contingency table: left by promotion\n")
print(addmargins(promo_pop_table))
cat("\nExpected counts for full-data chi-square test:\n")
print(round(promo_pop_chisq$expected, 4))
cat("\nFull-data chi-square test:\n")
print(promo_pop_chisq)

cat("\nSample contingency table: left by promotion\n")
print(addmargins(promo_sample_table))
cat("\nExpected counts for sample chi-square test:\n")
print(round(promo_sample_chisq$expected, 4))
cat("\nSample chi-square test:\n")
print(promo_sample_chisq)
cat("\n")

sink()
