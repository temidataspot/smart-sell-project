```r
# Ensure Month is a factor with correct month order
smartsell$Month <- factor(smartsell$Month)

# Weekly Sales Trends
weekly_sales_trend <- smartsell %>%
  group_by(Month) %>%
  summarise(Total_Sales = sum(Weekly_Sales, na.rm = TRUE))

library(ggplot2)
library(scales)  # for labels like 60M

plot1 <- ggplot(weekly_sales_trend, aes(x = Month, y = Total_Sales)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +  # Optional: nice small dots on the line
  geom_text(aes(label = paste0("$", label_number(scale = 1e-6, suffix = "M")(Total_Sales))),
            vjust = -0.5, size = 3, color = "black") +
  labs(title = "Monthly Sales Trends", x = "Month", y = NULL) +
  scale_y_continuous(labels = NULL) +
  theme_minimal(base_size = 12)+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )
plot1
# markdown vs sales correlation. do markdown boost sales?

# Step 1: Create Total Markdown
smartsell <- smartsell %>%
  mutate(Total_Markdown = rowSums(select(., starts_with("Markdown")), na.rm = TRUE))

filtered_data <- smartsell %>%
  filter(Total_Markdown > 0, Weekly_Sales > 0)

# Step 2: Plot Markdown vs Sales
plot2 <- ggplot(filtered_data, aes(x = Total_Markdown, y = Weekly_Sales)) +
  geom_point(alpha = 0.5, color = "steelblue") +  # scatter points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # linear trend line
  scale_x_continuous(labels = label_dollar()) +
  scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  labs(
    title = "Impact of Total Markdown on Weekly Sales",
    x = "Total Markdown ($)",
    y = "Weekly Sales ($)"
  ) +
  theme_minimal(base_size = 12)

plot2

# Impact of Holidays on Sales Trends
library(ggplot2)
library(scales)
library(dplyr)

plot3 <- smartsell %>%
  group_by(Holiday_Label) %>%
  summarise(Avg_Sales = mean(Weekly_Sales, na.rm = TRUE)) %>%
  ggplot(aes(x = Holiday_Label, y = Avg_Sales, fill = Holiday_Label)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0("$", label_number(scale_cut = cut_short_scale())(Avg_Sales))),
            vjust = -0.5, fontface = "bold", size = 4, color = "black") +
  scale_y_continuous(labels = NULL, expand = expansion(mult = c(0, 0.2))) +
  scale_fill_manual(values = c("gray70", "midnightblue")) +
  labs(
    title = "Holiday vs Non-Holiday Sales",
    x = "Holiday Status",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "none"  # <-- now we remove legend again
  )


# Sales Trend Over Time by Store Type
library(ggplot2)
library(scales)

plot4 <- ggplot(smartsell, aes(x = Type, y = Weekly_Sales, fill = Type)) +
  geom_violin(trim = FALSE, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
  scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  labs(
    title = "Sales Distribution by Store Type",
    x = "Store Type",
    y = "Weekly Sales"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  scale_fill_manual(values = c("steelblue", "firebrick", "darkgreen"))

# ---- Combine Plots with a General Title ----
combined_plot <- (plot1 + plot2) /
  (plot3 + plot4) +
  plot_layout(heights = c(1, 1),  # control row height ratios
              guides = "collect") +  # share legends if needed
  plot_annotation(
    title = "📊 Exploratory Data Analysis Overview: Sales Trends & Patterns",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  ) &
  theme(plot.margin = margin(10, 10, 10, 10),  # margin around each plot
        panel.spacing = unit(2, "lines"))      # space *between* the plots

# Show the plot
print(combined_plot)
```
