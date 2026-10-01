## =====================================================================
## 5 NONPARAMETRIC TESTS - R PRACTICE EXAMPLES (ECONOMICS THEMES)
## =====================================================================
## Each example has: research question -> simulate data (skewed/with
## outliers so it's clear why a parametric test wouldn't fit) -> run
## the test -> interpret the result -> chart

set.seed(2026)

## ---------------------------------------------------------------------
## 1) MANN-WHITNEY U TEST  (nonparametric alternative to Independent t-test)
## Question: Does household income (million VND/month) differ between
## two provinces? Income data is usually RIGHT-SKEWED (a few very
## wealthy households pull a long tail).
## ---------------------------------------------------------------------

income_provinceA <- rlnorm(40, meanlog = log(15), sdlog = 0.5)  # million VND
income_provinceB <- rlnorm(40, meanlog = log(17), sdlog = 0.6)

# Quick normality check (why we don't just use a t-test)
shapiro.test(income_provinceA)   # p < 0.05 -> not normal

mw_test <- wilcox.test(income_provinceA, income_provinceB, conf.int = TRUE)
print(mw_test)
# Interpretation: p-value < 0.05 -> reject H0, the (rank-based) income
# distributions of the two provinces differ significantly.
# Note: Mann-Whitney compares "overall tendency to be higher/lower",
# NOT means -> report medians, not means, when using this test.
tapply(c(income_provinceA, income_provinceB),
       rep(c("ProvinceA", "ProvinceB"), each = 40), median)

# --- Chart: boxplot showing the right skew + median, to visualize why
# Mann-Whitney is used instead of a t-test (data isn't symmetric, has
# a long tail) ---
boxplot(income_provinceA, income_provinceB,
        names = c("Province A", "Province B"),
        col = c("#a6cee3", "#fdbf6f"),
        main = "Household Income: Province A vs Province B\n(Mann-Whitney U test)",
        ylab = "Income (million VND/month)")
points(c(1, 2), c(median(income_provinceA), median(income_provinceB)),
       pch = 18, col = "red", cex = 1.5)
legend("topleft", legend = "median", pch = 18, col = "red", bty = "n")


## ---------------------------------------------------------------------
## 2) WILCOXON SIGNED-RANK TEST  (nonparametric alternative to Paired t-test)
## Question: After a loan-support policy, did revenue (million VND/month)
## increase for the SAME group of 25 small businesses? This is
## before/after data on the same subjects -> must be "paired".
## ---------------------------------------------------------------------

  n_firms <- 25
revenue_before <- rlnorm(n_firms, meanlog = log(80), sdlog = 0.4)
# simulate the policy effect: most firms improve, a few drop sharply
policy_effect <- rnorm(n_firms, mean = 8, sd = 15)
revenue_after <- pmax(revenue_before + policy_effect, 5)

wilcoxon_test <- wilcox.test(revenue_after, revenue_before, paired = TRUE)
print(wilcoxon_test)
# Interpretation: p-value < 0.05 -> positive changes (revenue increases)
# clearly outweigh negative changes -> the policy has a statistically
# significant effect (on direction of change, not the average magnitude).

# --- Chart: a "before-after" line for each firm -> you can immediately
# see more firms trending up (green) than down (red) ---
matplot(rbind(revenue_before, revenue_after), type = "l", lty = 1,
        col = ifelse(revenue_after > revenue_before, "#33a02c", "#e31a1c"),
        xaxt = "n", xlim = c(0.8, 2.2),
        xlab = "", ylab = "Revenue (million VND/month)",
        main = "Firm Revenue Before vs After Policy\n(Wilcoxon Signed-Rank test)")
axis(1, at = c(1, 2), labels = c("Before", "After"))
points(rep(1, n_firms), revenue_before, pch = 16, col = "gray30")
points(rep(2, n_firms), revenue_after, pch = 16, col = "gray30")


## ---------------------------------------------------------------------
## 3) KRUSKAL-WALLIS TEST  (nonparametric alternative to One-way ANOVA)
## Question: Does starting salary (million VND/month) differ across 3
## industries: Manufacturing, Tech, Finance? Salary data usually has
## outliers (a few very high-paying positions).
## ---------------------------------------------------------------------

salary_manufacturing <- rlnorm(30, meanlog = log(9), sdlog = 0.3)
salary_tech <- rlnorm(30, meanlog = log(13), sdlog = 0.45)
salary_finance <- rlnorm(30, meanlog = log(11), sdlog = 0.5)

industry <- factor(rep(c("Manufacturing", "Tech", "Finance"), each = 30))
salary <- c(salary_manufacturing, salary_tech, salary_finance)

kw_test <- kruskal.test(salary ~ industry)
print(kw_test)
# Interpretation: p-value < 0.05 -> at least one industry differs from
# the others. We DON'T yet know which pair differs -> run a post-hoc
# Dunn test:

# install.packages("dunn.test")  # run once
dunn.test::dunn.test(salary, industry, method = "bonferroni")

# --- Chart: boxplot of the 3 groups side by side, to see which group
# stands out most -> compare against the Dunn test results above ---
boxplot(salary ~ industry,
        col = c("#a6cee3", "#b2df8a", "#fdbf6f"),
        main = "Starting Salary by Industry\n(Kruskal-Wallis test)",
        xlab = "Industry", ylab = "Salary (million VND/month)")


## ---------------------------------------------------------------------
## 4) SPEARMAN RANK CORRELATION  (nonparametric alternative to Pearson)
## Question: Is the Provincial Competitiveness Index (PCI, ordinal-like
## or a nonlinear relationship) correlated with the number of newly
## registered businesses?
## ---------------------------------------------------------------------

n_provinces <- 30
pci_score <- round(runif(n_provinces, 50, 90), 1)
# a MONOTONIC but nonlinear relationship (saturates at high PCI)
new_firms <- round(200 * log(pci_score - 40) + rnorm(n_provinces, 0, 20))

spearman_test <- cor.test(pci_score, new_firms, method = "spearman")
print(spearman_test)
# Compare with Pearson to see the difference:
cor.test(pci_score, new_firms, method = "pearson")
# Interpretation: Spearman's rho is usually HIGHER than Pearson's r
# here, because the relationship is monotonic but concave, not linear.

# --- Chart: raw values (left, curved/nonlinear relationship) vs ranks
# (right, closer to linear) -> illustrates why Spearman fits better ---
par(mfrow = c(1, 2))
plot(pci_score, new_firms, pch = 16, col = "#1f78b4",
     main = "Raw Values (Pearson)",
     xlab = "PCI", ylab = "New Registered Firms")
plot(rank(pci_score), rank(new_firms), pch = 16, col = "#e31a1c",
     main = "Ranks (Spearman)",
     xlab = "Rank of PCI", ylab = "Rank of New Firms")
par(mfrow = c(1, 1))


## ---------------------------------------------------------------------
## 5) FRIEDMAN TEST  (nonparametric alternative to Repeated Measures ANOVA)
## Question: The unemployment rate (%) of the SAME group of 20
## provinces/cities is measured at 3 time points: pre-pandemic,
## during pandemic, post-pandemic recovery. Is there a significant
## change across the 3 time points?
## ---------------------------------------------------------------------

n_provinces2 <- 20
before_pandemic <- rlnorm(n_provinces2, meanlog = log(3), sdlog = 0.3)
during_pandemic <- before_pandemic + rlnorm(n_provinces2, meanlog = log(3), sdlog = 0.6)  # sharp, uneven rise
after_pandemic <- during_pandemic - rlnorm(n_provinces2, meanlog = log(2), sdlog = 0.5)   # falls, but unevenly

unemployment_data <- matrix(c(before_pandemic, during_pandemic, after_pandemic), ncol = 3,
                             dimnames = list(NULL, c("Before", "During", "After")))

friedman_test <- friedman.test(unemployment_data)
print(friedman_test)
# Interpretation: p-value < 0.05 -> unemployment rate changes
# significantly across at least one of the 3 time points. To find out
# which pair of time points differs, run a pairwise Wilcoxon post-hoc
# test with Bonferroni correction:

pairwise.wilcox.test(as.vector(unemployment_data),
                      rep(c("Before", "During", "After"), each = n_provinces2),
                      paired = TRUE, p.adjust.method = "bonferroni")

# --- Chart: one line per province across the 3 time points -> clearly
# shows the common pattern of "sharp rise during pandemic, then fall" ---
matplot(t(unemployment_data), type = "l", lty = 1, col = "#33a02c",
        xaxt = "n", xlab = "", ylab = "Unemployment Rate (%)",
        main = "Unemployment Rate, 20 Provinces, 3 Time Points\n(Friedman test)")
axis(1, at = 1:3, labels = c("Before", "During", "After"))
lines(1:3, colMeans(unemployment_data), lwd = 3, col = "black")
legend("topright", legend = "mean across provinces", lwd = 3, col = "black", bty = "n")

## =====================================================================
## GENERAL NOTES
## - For every test, plot a boxplot first to "see" the skew/outliers:
##   boxplot(income_provinceA, income_provinceB)
## - Use shapiro.test() to check normality before deciding between a
##   parametric or nonparametric test.
## - Report median + IQR (not mean + SD) when using nonparametric tests.
## =====================================================================
