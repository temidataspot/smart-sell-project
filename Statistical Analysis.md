```r
*# Descriptive stat*
library(dplyr)

smartsell %>%
  group_by(Store, Dept) %>%
  summarise(
    Mean = mean(Weekly_Sales, na.rm = TRUE),
    Median = median(Weekly_Sales, na.rm = TRUE),
    Mode = as.numeric(names(sort(table(Weekly_Sales), decreasing = TRUE)[1])),
    SD = sd(Weekly_Sales, na.rm = TRUE)
  )
# store size vs avg sales'
smartsell %>%
  group_by(Store) %>%
  summarise(
    Store_Size = first(Size),
    Avg_Sales = mean(Weekly_Sales, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = Store_Size, y = Avg_Sales)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Store Size vs Average Sales", x = "Store Size", y = "Average Weekly Sales")

*# Inferential Statistics*

*# T-test:* Holidays vs. Non-Holidays'
t.test(Weekly_Sales ~ IsHoliday, data = smartsell)

*# One-way ANOVA:* Sales by Store Type
anova1 <- aov(Weekly_Sales ~ Type, data = smartsell)
summary(anova1)

*# Run Tukey HSD test* to find specific store types differ:
TukeyHSD(anova1)

*# Two-way ANOVA:* Store Type & Holiday
anova2 <- aov(Weekly_Sales ~ Type * IsHoliday, data = smartsell)
summary(anova2)

*# bayesian inference*
install.packages("bayesAB")
library(bayesAB)

*# bayesian A/B test:* holiday uplift

library(dplyr)
library(bayesAB)

# Prepare the data
holiday_data <- smartsell %>%
  filter(IsHoliday %in% c(TRUE, FALSE)) %>%
  select(IsHoliday, Weekly_Sales)

A <- holiday_data$Weekly_Sales[holiday_data$IsHoliday == FALSE]
B <- holiday_data$Weekly_Sales[holiday_data$IsHoliday == TRUE]

# Run Bayesian A/B Test with correctly named priors
result <- bayesTest(
  A, B,
  priors = c(mu_mean = 0, mu_sd = 100, sigma_shape = 1, sigma_rate = 1),
  distribution = "normal"
)

# Summarize and plot results
summary(result)
plot(result)

# Install if needed
install.packages("BayesFactor")

# Load library
library(BayesFactor)

*# Run Bayesian t-test*
bf_result <- ttestBF(
  x = holiday_data$Weekly_Sales[holiday_data$IsHoliday == TRUE],
  y = holiday_data$Weekly_Sales[holiday_data$IsHoliday == FALSE],
  paired = FALSE
)

# Show result
bf_result

# Get posterior samples
posterior_samples <- posterior(bf_result, iterations = 10000)

# Convert to data frame
posterior_df <- as.data.frame(posterior_samples)

# Load ggplot2
library(ggplot2)

# Plot posterior of the mean difference
ggplot(posterior_df, aes(x = mu)) +
  geom_density(fill = "steelblue", alpha = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Posterior Distribution of the Mean Difference",
    x = "Mean Difference (Holiday - Non-Holiday)",
    y = "Density"
  )

*# Linear Regression: Markdowns & Economic Indicators*
model <- lm(Weekly_Sales ~ Fuel_Price + Temperature + CPI + Unemployment, data = smartsell)
summary(model)


# plot reg cf
library(ggplot2)

*# Get coefficients and confidence intervals*
model <- lm(Weekly_Sales ~ Fuel_Price + Temperature + CPI + Unemployment, data = smartsell)
coefs <- summary(model)$coefficients

# Prepare data frame for plotting
coef_df <- data.frame(
  Term = rownames(coefs),
  Estimate = coefs[, "Estimate"],
  Lower = coefs[, "Estimate"] - 1.96 * coefs[, "Std. Error"],
  Upper = coefs[, "Estimate"] + 1.96 * coefs[, "Std. Error"]
)

# Remove intercept for clarity
coef_df <- coef_df[coef_df$Term != "(Intercept)", ]

# Plot
ggplot(coef_df, aes(x = reorder(Term, Estimate), y = Estimate, fill = Estimate > 0)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
  coord_flip() +
  scale_fill_manual(values = c("red", "green")) +
  labs(title = "Effect of Economic Indicators on Weekly Sales",
       x = "Predictor", y = "Coefficient Estimate") +
  theme_minimal()

*# Regression with Interaction Terms*
model_interaction <- lm(Weekly_Sales ~ Type * CPI + IsHoliday * Dept, data = smartsell)
summary(model_interaction)

install.packages("patchwork")
library(patchwork)
# Load necessary libraries
library(ggplot2)
library(patchwork)

# Plot 1: CPI x Store Type interaction
plot1 <- ggplot(smartsell, aes(x = CPI, y = Weekly_Sales, color = Type)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Interaction: CPI & Store Type",
    x = "Consumer Price Index (CPI)",
    y = "Weekly Sales"
  ) +
  theme_minimal()

# Plot 2: Dept x Holiday interaction
plot2 <- ggplot(smartsell, aes(x = Dept, y = Weekly_Sales, color = IsHoliday)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Interaction: Department & Holiday",
    x = "Department",
    y = "Weekly Sales"
  ) +
  theme_minimal()

# Combine plots side-by-side
plot1 + plot2
```
