```r
# independent contribution of unemployment and cpi to weekly sales

# multiple linear regression plot
# bar chart of regression coefficients

# Fit linear model

model <- lm(Weekly_Sales ~ Unemployment + CPI, data = smartsell)

# Extract coefficients (excluding intercept)
coefs <- summary(model)$coefficients[-1, ]

# Create a data frame
coef_df <- data.frame(
  Variable = rownames(coefs),
  Estimate = coefs[, "Estimate"]
)

# Plot
library(ggplot2)

ggplot(coef_df, aes(x = Variable, y = Estimate, fill = Estimate > 0)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(Estimate, 2)), vjust = -0.5) +
  scale_fill_manual(values = c("red", "blue")) +
  labs(title = "Effect of Unemployment & CPI on Weekly Sales",
       y = "Effect on Sales (per unit change)",
       x = NULL) +
  theme_minimal()

# 3d plot showing how both affect sales together

install.packages("plotly")
library(plotly)

# Filter & remove NAs
plot_data <- smartsell %>%
  select(Weekly_Sales, CPI, Unemployment) %>%
  na.omit()

# 3D scatter
plot_ly(plot_data, 
        x = ~CPI, 
        y = ~Unemployment, 
        z = ~Weekly_Sales, 
        type = "scatter3d", 
        mode = "markers",
        marker = list(size = 3, color = ~Weekly_Sales, colorscale = "Viridis")) %>%
  layout(title = "Weekly Sales vs CPI & Unemployment")
```
