library(e1071)

setwd('/Users/elokusa/Desktop/4346549_BIA713')

# ── QUESTION 1 ────────────────────────────────────────────────────────────────
# Recreate Assignment 1 Section A sample (set.seed matches section_a.R)
sp <- read.csv('StudentPerformance.csv')
set.seed(4346549)
sample_sp <- sp[sample(nrow(sp), 250), ]

male_math   <- sample_sp$Math_score[sample_sp$Gender == "male"]
female_math <- sample_sp$Math_score[sample_sp$Gender == "female"]

# Q1.1 – Histogram and QQ plot for each gender group
png('output/A2_Q1.1_male.png', width=900, height=450)
par(mfrow=c(1,2), mar=c(4,4,3,1))
hist(male_math, main="Male Math Scores - Histogram", xlab="Math Score",
     col="#89CFF0", border="white", breaks=12)
abline(v=mean(male_math), col="red", lwd=2, lty=2)
legend("topright", legend="Mean", col="red", lty=2, lwd=2, cex=0.9)
qqnorm(male_math, main="Male Math Scores - QQ Plot",
       pch=16, col="navy", cex=0.7)
qqline(male_math, col="red", lwd=2)
dev.off()

png('output/A2_Q1.1_female.png', width=900, height=450)
par(mfrow=c(1,2), mar=c(4,4,3,1))
hist(female_math, main="Female Math Scores - Histogram", xlab="Math Score",
     col="#F4A7B9", border="white", breaks=12)
abline(v=mean(female_math), col="darkred", lwd=2, lty=2)
legend("topright", legend="Mean", col="darkred", lty=2, lwd=2, cex=0.9)
qqnorm(female_math, main="Female Math Scores - QQ Plot",
       pch=16, col="darkred", cex=0.7)
qqline(female_math, col="red", lwd=2)
dev.off()

# Shapiro-Wilk normality test and shape measures
sw_male   <- shapiro.test(male_math)
sw_female <- shapiro.test(female_math)

cat(sprintf("Male   Shapiro-Wilk: W=%.4f, p=%.4f\n", sw_male$statistic,   sw_male$p.value))
cat(sprintf("Female Shapiro-Wilk: W=%.4f, p=%.4f\n", sw_female$statistic, sw_female$p.value))
cat(sprintf("Male   skewness=%.4f, kurtosis=%.4f\n", skewness(male_math),   kurtosis(male_math)))
cat(sprintf("Female skewness=%.4f, kurtosis=%.4f\n", skewness(female_math), kurtosis(female_math)))

# Q1.2 – Side-by-side boxplots and 1.5*IQR outlier detection
png('output/A2_Q1.2_boxplots.png', width=700, height=520)
par(mar=c(5,5,4,2))
boxplot(list(Male=male_math, Female=female_math),
        main="Math Score Distribution by Gender",
        ylab="Math Score (out of 100)",
        col=c("#89CFF0","#F4A7B9"), border="navy",
        notch=FALSE, outline=TRUE)
dev.off()

for (grp in list(Male=male_math, Female=female_math)) {
  nm  <- names(which(sapply(list(Male=male_math, Female=female_math), identical, grp)))
  q1  <- quantile(grp, 0.25); q3 <- quantile(grp, 0.75); iqr <- q3 - q1
  lo  <- q1 - 1.5*iqr;  hi <- q3 + 1.5*iqr
  out <- grp[grp < lo | grp > hi]
  cat(sprintf("%s: n=%d, Min=%.0f, Q1=%.2f, Med=%.0f, Mean=%.2f, Q3=%.0f, Max=%.0f\n",
              nm, length(grp), min(grp), q1, median(grp), mean(grp), q3, max(grp)))
  cat(sprintf("       IQR=%.2f, Fences=[%.2f, %.2f], Outliers=%d",
              iqr, lo, hi, length(out)))
  if (length(out) > 0) cat(sprintf("  values: %s", paste(sort(out), collapse=", ")))
  cat("\n")
}

# ── QUESTION 2 ────────────────────────────────────────────────────────────────
# Split Household population into WC and EC provincial subsets
hh     <- read.csv('Household.csv')
wc_pop <- hh[hh$PROVINCE == "WC", ]
ec_pop <- hh[hh$PROVINCE == "EC", ]

# Q2.1 – Histogram and QQ plot to assess normality of HEADINC graphically
png('output/A2_Q2.1_WC.png', width=900, height=450)
par(mfrow=c(1,2), mar=c(4,4,3,1))
hist(wc_pop$HEADINC, main="WC HEADINC - Histogram",
     xlab="Monthly Income (R)", col="#89CFF0", border="white", breaks=20)
abline(v=mean(wc_pop$HEADINC), col="red", lwd=2, lty=2)
legend("topright", legend="Mean", col="red", lty=2, lwd=2, cex=0.9)
qqnorm(wc_pop$HEADINC, main="WC HEADINC - QQ Plot",
       pch=16, col="navy", cex=0.5)
qqline(wc_pop$HEADINC, col="red", lwd=2)
dev.off()

png('output/A2_Q2.1_EC.png', width=900, height=450)
par(mfrow=c(1,2), mar=c(4,4,3,1))
hist(ec_pop$HEADINC, main="EC HEADINC - Histogram",
     xlab="Monthly Income (R)", col="#A8D5A2", border="white", breaks=20)
abline(v=mean(ec_pop$HEADINC), col="red", lwd=2, lty=2)
legend("topright", legend="Mean", col="red", lty=2, lwd=2, cex=0.9)
qqnorm(ec_pop$HEADINC, main="EC HEADINC - QQ Plot",
       pch=16, col="darkgreen", cex=0.5)
qqline(ec_pop$HEADINC, col="red", lwd=2)
dev.off()

# Q2.2 – Numerical normality measures (mean, median, SD, skewness, kurtosis, Shapiro-Wilk)
# Shapiro-Wilk is limited to n <= 5000; populations here are within that limit
for (pv in c("WC","EC")) {
  pop   <- hh[hh$PROVINCE == pv, "HEADINC"]
  pname <- ifelse(pv=="WC","Western Cape","Eastern Cape")
  cat(sprintf("\n%s (n=%d)\n", pname, length(pop)))
  cat(sprintf("  Mean:     R%10.2f\n", mean(pop)))
  cat(sprintf("  Median:   R%10.2f\n", median(pop)))
  cat(sprintf("  SD:       R%10.2f\n", sd(pop)))
  cat(sprintf("  Skewness: %10.4f\n",  skewness(pop)))
  cat(sprintf("  Kurtosis: %10.4f\n",  kurtosis(pop)))
  set.seed(1)
  sw_x <- if (length(pop) > 5000) sample(pop, 5000) else pop
  sw   <- shapiro.test(sw_x)
  cat(sprintf("  Shapiro-Wilk (n=%d): W=%.4f, p=%.4e\n", length(sw_x), sw$statistic, sw$p.value))
}

# Q2.4 / Q2.5 – Normal probability calculations using population mu and sigma
for (pv in c("WC","EC")) {
  pop   <- hh[hh$PROVINCE == pv, "HEADINC"]
  mu    <- mean(pop); sigma <- sd(pop)
  pname <- ifelse(pv=="WC","Western Cape","Eastern Cape")
  p1    <- pnorm(20000, mu, sigma)
  p3    <- pnorm(25000, mu, sigma) - pnorm(20000, mu, sigma)
  p2    <- 1 - pnorm(25000, mu, sigma)
  cat(sprintf("\n%s: mu=R%.2f, sigma=R%.2f\n", pname, mu, sigma))
  cat(sprintf("  P(X < 20000)          = %.4f  (%.2f%%)\n", p1, p1*100))
  cat(sprintf("  P(X > 25000)          = %.4f  (%.2f%%)\n", p2, p2*100))
  cat(sprintf("  P(20000 <= X <= 25000) = %.4f  (%.2f%%)\n", p3, p3*100))
}

# ── QUESTION 3 ────────────────────────────────────────────────────────────────
er <- read.csv('Employee_Retention.csv')

# Q3.1.1 – Split average_montly_hours by departure status (left=1, stayed=0)
hrs_left   <- er$average_montly_hours[er$left == 1]
hrs_stayed <- er$average_montly_hours[er$left == 0]

cat(sprintf("Hours Left   (n=%d): Mean=%.2f, Median=%.2f, SD=%.2f\n",
            length(hrs_left),   mean(hrs_left),   median(hrs_left),   sd(hrs_left)))
cat(sprintf("Hours Stayed (n=%d): Mean=%.2f, Median=%.2f, SD=%.2f\n",
            length(hrs_stayed), mean(hrs_stayed), median(hrs_stayed), sd(hrs_stayed)))

# Q3.1.2 – Normality verification: shape measures and Shapiro-Wilk (sampled at n=5000)
cat(sprintf("Hours Left:   Skewness=%.4f, Kurtosis=%.4f\n", skewness(hrs_left),   kurtosis(hrs_left)))
cat(sprintf("Hours Stayed: Skewness=%.4f, Kurtosis=%.4f\n", skewness(hrs_stayed), kurtosis(hrs_stayed)))

set.seed(1)
sw_hl <- shapiro.test(sample(hrs_left,   min(5000, length(hrs_left))))
sw_hs <- shapiro.test(sample(hrs_stayed, min(5000, length(hrs_stayed))))
cat(sprintf("Hours Left   Shapiro-Wilk: W=%.4f, p=%.4e\n", sw_hl$statistic, sw_hl$p.value))
cat(sprintf("Hours Stayed Shapiro-Wilk: W=%.4f, p=%.4e\n", sw_hs$statistic, sw_hs$p.value))

png('output/A2_Q3.1_hours.png', width=1000, height=500)
par(mfrow=c(2,2), mar=c(4,4,3,1))
hist(hrs_left,   main="Monthly Hours - Left (Histogram)",   xlab="Avg Monthly Hours",
     col="#F4A7B9", border="white", breaks=20)
abline(v=mean(hrs_left), col="red", lwd=2, lty=2)
qqnorm(hrs_left,   main="Monthly Hours - Left (QQ Plot)",
       pch=16, col="darkred", cex=0.5)
qqline(hrs_left, col="red", lwd=2)
hist(hrs_stayed, main="Monthly Hours - Stayed (Histogram)", xlab="Avg Monthly Hours",
     col="#89CFF0", border="white", breaks=20)
abline(v=mean(hrs_stayed), col="navy", lwd=2, lty=2)
qqnorm(hrs_stayed, main="Monthly Hours - Stayed (QQ Plot)",
       pch=16, col="navy", cex=0.5)
qqline(hrs_stayed, col="red", lwd=2)
dev.off()

# Q3.2.1 – Split last_evaluation by departure status
eval_left   <- er$last_evaluation[er$left == 1]
eval_stayed <- er$last_evaluation[er$left == 0]

cat(sprintf("Eval Left   (n=%d): Mean=%.4f, Median=%.4f, SD=%.4f\n",
            length(eval_left),   mean(eval_left),   median(eval_left),   sd(eval_left)))
cat(sprintf("Eval Stayed (n=%d): Mean=%.4f, Median=%.4f, SD=%.4f\n",
            length(eval_stayed), mean(eval_stayed), median(eval_stayed), sd(eval_stayed)))

# Q3.2.2 – Normality verification: shape measures and Shapiro-Wilk
cat(sprintf("Eval Left:   Skewness=%.4f, Kurtosis=%.4f\n", skewness(eval_left),   kurtosis(eval_left)))
cat(sprintf("Eval Stayed: Skewness=%.4f, Kurtosis=%.4f\n", skewness(eval_stayed), kurtosis(eval_stayed)))

set.seed(1)
sw_el <- shapiro.test(sample(eval_left,   min(5000, length(eval_left))))
sw_es <- shapiro.test(sample(eval_stayed, min(5000, length(eval_stayed))))
cat(sprintf("Eval Left   Shapiro-Wilk: W=%.4f, p=%.4e\n", sw_el$statistic, sw_el$p.value))
cat(sprintf("Eval Stayed Shapiro-Wilk: W=%.4f, p=%.4e\n", sw_es$statistic, sw_es$p.value))

png('output/A2_Q3.2_eval.png', width=1000, height=500)
par(mfrow=c(2,2), mar=c(4,4,3,1))
hist(eval_left,   main="Last Evaluation - Left (Histogram)",   xlab="Evaluation Score",
     col="#F4A7B9", border="white", breaks=20)
abline(v=mean(eval_left), col="red", lwd=2, lty=2)
qqnorm(eval_left,   main="Last Evaluation - Left (QQ Plot)",
       pch=16, col="darkred", cex=0.5)
qqline(eval_left, col="red", lwd=2)
hist(eval_stayed, main="Last Evaluation - Stayed (Histogram)", xlab="Evaluation Score",
     col="#89CFF0", border="white", breaks=20)
abline(v=mean(eval_stayed), col="navy", lwd=2, lty=2)
qqnorm(eval_stayed, main="Last Evaluation - Stayed (QQ Plot)",
       pch=16, col="navy", cex=0.5)
qqline(eval_stayed, col="red", lwd=2)
dev.off()
