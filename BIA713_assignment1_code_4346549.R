# ============================================================
# BIA713 Assignment 1 – 2026
# Student: Zondi, ENZ | Student Number: 4346549
# ============================================================

library(e1071)

# ============================================================
# SECTION A – Student Performance
# ============================================================

# Load population
sp <- read.csv('StudentPerformance.csv')

# Q1.3 – 25% random sample
set.seed(4346549)
sample_size <- round(nrow(sp) * 0.25)  # 250
sample_sp <- sp[sample(nrow(sp), sample_size), ]
write.csv(sample_sp, '~/Desktop/BIA713_assignment1_sample_A.csv', row.names=FALSE)

# Q1.4 – Categorical variable exploration
# Gender
gender_freq <- table(sample_sp$Gender)
gender_pct  <- prop.table(gender_freq) * 100
barplot(gender_freq,
        main="Gender Distribution (Sample n=250)",
        xlab="Gender", ylab="Frequency",
        col=c("#F4A7B9","#89CFF0"), border="white",
        ylim=c(0, max(gender_freq)*1.2))

# Test preparation course
prep_freq <- table(sample_sp$Test_preparation_course)
prep_pct  <- prop.table(prep_freq) * 100
barplot(prep_freq,
        main="Test Preparation Course (Sample n=250)",
        xlab="Test Preparation", ylab="Frequency",
        col=c("#A8D5A2","#FFD580"), border="white",
        ylim=c(0, max(prep_freq)*1.2))

# Q1.5 – Numerical descriptive statistics
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

num_vars <- c("Math_score", "Reading_score", "Writing_score")
for (v in num_vars) {
  x <- sample_sp[[v]]
  cat(v, "\n")
  cat("Min:", min(x), "| Max:", max(x), "| Mean:", round(mean(x),2),
      "| Median:", median(x), "| Mode:", get_mode(x), "\n")
  cat("SD:", round(sd(x),2), "| CV:", round((sd(x)/mean(x))*100,2), "%\n")
  cat("Q1:", quantile(x,0.25), "| Q3:", quantile(x,0.75),
      "| Skewness:", round(skewness(x),4), "\n\n")
  # Histogram + Boxplot
  par(mfrow=c(1,2))
  hist(x, main=paste("Histogram of", v), xlab=v,
       col="#89CFF0", border="white", breaks=12)
  abline(v=mean(x), col="red", lwd=2, lty=2)
  boxplot(x, main=paste("Boxplot of", v), ylab=v,
          col="#89CFF0", border="navy")
  par(mfrow=c(1,1))
}

# ============================================================
# SECTION B – Household Income & Expenditure
# ============================================================

hh <- read.csv('Household.csv')

# Q1.3 – Stratified sample: 24 WC + 26 EC
set.seed(4346549)
wc_pop <- hh[hh$PROVINCE == "WC", ]
ec_pop <- hh[hh$PROVINCE == "EC", ]
wc_sample <- wc_pop[sample(nrow(wc_pop), 24), ]
ec_sample <- ec_pop[sample(nrow(ec_pop), 26), ]
hh_sample <- rbind(wc_sample, ec_sample)

# Q2.1 – Frequency table for HEADINC by province
make_freq_table <- function(data, prov_name) {
  x <- data$HEADINC
  k <- ceiling(1 + 3.322 * log10(length(x)))
  breaks <- pretty(x, n=k)
  cuts <- cut(x, breaks=breaks, include.lowest=TRUE, right=FALSE)
  f   <- table(cuts)
  f_pct <- round(prop.table(f)*100, 2)
  F_cum <- cumsum(f)
  F_pct_cum <- round(cumsum(prop.table(f))*100, 2)
  data.frame(Interval=names(f), f=as.integer(f),
             `f%`=as.numeric(f_pct), F=as.integer(F_cum),
             `F%`=as.numeric(F_pct_cum))
}

wc_s <- hh_sample[hh_sample$PROVINCE=="WC",]
ec_s <- hh_sample[hh_sample$PROVINCE=="EC",]
print(make_freq_table(wc_s, "Western Cape"))
print(make_freq_table(ec_s, "Eastern Cape"))

# Q2.2 – Relative frequency histograms
par(mfrow=c(1,2))
hist(wc_s$HEADINC, freq=FALSE, breaks=pretty(range(wc_s$HEADINC),n=8),
     main="Western Cape – HEADINC (f%)", xlab="Monthly Income (R)",
     ylab="Relative Frequency", col="#89CFF0", border="white")
hist(ec_s$HEADINC, freq=FALSE, breaks=pretty(range(ec_s$HEADINC),n=8),
     main="Eastern Cape – HEADINC (f%)", xlab="Monthly Income (R)",
     ylab="Relative Frequency", col="#A8D5A2", border="white")
par(mfrow=c(1,1))

# Q2.3 – Boxplots
boxplot(HEADINC ~ PROVINCE, data=hh_sample,
        main="HEADINC by Province", xlab="Province", ylab="Monthly Income (R)",
        col=c("#A8D5A2","#89CFF0"), border="navy",
        names=c("Eastern Cape","Western Cape"))

# Q3.1 – Gender × Race two-way tables
for (prov in c("WC","EC")) {
  d <- hh_sample[hh_sample$PROVINCE==prov,]
  print(addmargins(table(Gender=d$GENDER, Race=d$RACE)))
}

# Q3.2 – Gender-by-race bar charts
par(mfrow=c(1,2))
for (prov in c("WC","EC")) {
  pname <- ifelse(prov=="WC","Western Cape","Eastern Cape")
  d <- hh_sample[hh_sample$PROVINCE==prov,]
  tbl <- table(Race=d$RACE, Gender=d$GENDER)
  barplot(tbl, beside=TRUE, main=paste(pname,"– Gender by Race"),
          xlab="Gender", ylab="Frequency",
          col=c("#FFD580","#F4A7B9","#89CFF0","#A8D5A2"),
          legend.text=paste("Race",rownames(tbl)),
          args.legend=list(x="topright",cex=0.8), border="white")
}
par(mfrow=c(1,1))

# Q3.3 – Gender × EDUC_CAT relative frequency tables
for (prov in c("WC","EC")) {
  d <- hh_sample[hh_sample$PROVINCE==prov,]
  tbl <- table(Gender=d$GENDER, Educ=d$EDUC_CAT)
  print(addmargins(round(prop.table(tbl)*100, 2)))
}

# Q3.4 – Gender-by-education bar charts
par(mfrow=c(1,2))
educ_labels <- c("1=None","2=Gr1-6","3=Gr7","4=Gr8-9","5=Gr10-11","6=Gr12","7=Post-12")
for (prov in c("WC","EC")) {
  pname <- ifelse(prov=="WC","Western Cape","Eastern Cape")
  d <- hh_sample[hh_sample$PROVINCE==prov,]
  tbl <- table(Educ=d$EDUC_CAT, Gender=d$GENDER)
  cols <- colorRampPalette(c("#89CFF0","#F4A7B9","#A8D5A2","#FFD580","#DDA0DD","#FFB347","#90EE90"))(nrow(tbl))
  barplot(tbl, beside=TRUE, main=paste(pname,"– Gender by Education"),
          xlab="Gender", ylab="Count",
          col=cols,
          legend.text=educ_labels[1:nrow(tbl)],
          args.legend=list(x="topright",cex=0.7), border="white")
}
par(mfrow=c(1,1))

# ============================================================
# SECTION C – Employee Retention
# ============================================================

er <- read.csv('Employee_Retension.csv')  # Note: original filename spelling

# Step 3 – Contingency table: left × promotion_last_5years
er$left_label  <- ifelse(er$left==1,"Left","Stayed")
er$promo_label <- ifelse(er$promotion_last_5years==1,"Promoted","Not Promoted")
tbl3 <- table(Left=er$left_label, Promoted=er$promo_label)
print(addmargins(tbl3))
print(round(prop.table(tbl3, margin=2)*100, 2))

# Bar chart
tbl3_plot <- table(Promoted=er$promo_label, Left=er$left_label)
barplot(tbl3_plot, beside=TRUE,
        main="Employee Leaving by Promotion Status",
        xlab="Employment Status", ylab="Number of Employees",
        col=c("#A8D5A2","#F4A7B9"),
        legend.text=c("Not Promoted","Promoted"),
        args.legend=list(x="topright"), border="white")

# Step 4 – Employees who left without promotion
left_no_promo <- tbl3["Left","Not Promoted"]
cat("Employees who left without promotion:", left_no_promo, "\n")

# Step 5 – % stayed after promotion
stayed_promo <- tbl3["Stayed","Promoted"]
total_promo  <- sum(tbl3[,"Promoted"])
cat("% stayed after promotion:", round((stayed_promo/total_promo)*100,2), "%\n")

# Step 6 – left × time_spend_company
tbl6 <- table(Left=er$left_label, Years=er$time_spend_company)
print(addmargins(tbl6))
print(round(prop.table(tbl6, margin=2)["Left",]*100, 2))

tbl6_plot <- prop.table(table(Left=er$left_label, Years=er$time_spend_company), margin=2)*100
barplot(tbl6_plot,
        main="Proportion Leaving by Years at Company",
        xlab="Years at Company", ylab="Percentage (%)",
        col=c("#F4A7B9","#89CFF0"),
        legend.text=c("Left","Stayed"),
        args.legend=list(x="topright"), border="white")

# Step 7 – Ogive of average_montly_hours
hrs <- er$average_montly_hours
breaks_h <- pretty(hrs, n=15)
cuts_h   <- cut(hrs, breaks=breaks_h, include.lowest=TRUE, right=FALSE)
F_h      <- cumsum(table(cuts_h))
F_pct_h  <- F_h / length(hrs) * 100
upper_bounds <- breaks_h[-1]

plot(upper_bounds, F_pct_h, type="b", pch=16, col="navy",
     main="Ogive – Average Monthly Working Hours",
     xlab="Average Monthly Hours", ylab="Cumulative Frequency (%)",
     ylim=c(0,105), lwd=2)
abline(v=160, col="red",       lty=2, lwd=1.5)
abline(v=200, col="darkgreen", lty=2, lwd=1.5)

pct_lt160   <- round(mean(hrs < 160)*100, 2)
pct_160_200 <- round(mean(hrs >= 160 & hrs <= 200)*100, 2)
pct_gt200   <- round(mean(hrs > 200)*100, 2)
cat("< 160 hrs:", pct_lt160, "%\n")
cat("160-200 hrs:", pct_160_200, "%\n")
cat("> 200 hrs:", pct_gt200, "%\n")
