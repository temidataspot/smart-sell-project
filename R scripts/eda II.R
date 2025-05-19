# PHASE 2B: EDA II
install.packages(c("tidyverse", "dplyr", "utils", "ggplot2", "lubridate", "readxl"))
library(readxl)
library(tidyverse)
library(ggplot2)
library(utils)
library(dplyr)
library(lubridate)
install.packages("openxlsx")
library(openxlsx)
install.packages("readr")
library(readr)
retail <- read_csv("retail.csv")
View(retail)
str(retail)

# Seasonal trends and holiday spikes
# Average sales by week number (to see seasonal weekly trends)
retail %>%
  mutate(Week = isoweek(Date)) %>%
  group_by(Week) %>%
  summarise(Average_Sales = mean(Weekly_Sales, na.rm = TRUE)) %>%
  ggplot(aes(x = Week, y = Average_Sales)) +
  geom_line(color = "blue") +
  labs(title = "Seasonal Weekly Sales Trend", x = "Week of Year", y = "Average Weekly Sales")

# Highlight holiday spikes
retail %>%
  group_by(Date, IsHoliday) %>%
  summarise(Sales = sum(Weekly_Sales)) %>%
  ggplot(aes(x = Date, y = Sales, color = IsHoliday)) +
  geom_line() +
  labs(title = "Sales Trend Highlighting Holidays", y = "Total Sales")
install.packages("scales")
retail %>%
  group_by(Month_Name, IsHoliday) %>%
  summarise(Sales = sum(Weekly_Sales, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(Month_Name, 
                        levels = month.name),  # Ensure Jan to Dec order
             y = Sales, 
             group = IsHoliday, 
             color = IsHoliday)) +
  geom_line(size = 1) +
  labs(title = "Sales Trend Highlighting Holidays", x = "Month", y = "Total Sales") +
  scale_y_continuous(labels = label_number_si()) +  # Makes 7e+07 into "70M", etc.
  theme_minimal(base_size = 12) +
  theme(panel.background = element_rect(fill = "white"))

# Boxplot for each MarkDown variable vs Sales
markdown_vars <- c("MarkDown1", "MarkDown2", "MarkDown3", "MarkDown4", "MarkDown5")

# Melt data (reshape)
library(reshape2)
md_data <- retail %>%
  select(all_of(markdown_vars), Weekly_Sales) %>%
  melt(id.vars = "Weekly_Sales")

# Boxplot
ggplot(md_data, aes(x = variable, y = value)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Distribution of MarkDown Values", x = "MarkDown Type", y = "Value")

# Time trends of markdowns
retail %>%
  select(Date, all_of(markdown_vars)) %>%
  pivot_longer(-Date, names_to = "MarkDown", values_to = "Value") %>%
  group_by(Date, MarkDown) %>%
  summarise(Average = mean(Value, na.rm = TRUE)) %>%
  ggplot(aes(x = Date, y = Average, color = MarkDown)) +
  geom_line() +
  labs(title = "MarkDown Value Trends Over Time")

# Correlation analysis: macroeconomic factors and sales
# Select relevant variables
cor_data <- retail %>%
  select(Weekly_Sales, Temperature, Fuel_Price, CPI, Unemployment)

# Remove NAs
cor_data <- na.omit(cor_data)

# Correlation matrix
cor_matrix <- cor(cor_data)

# Heatmap
install.packages("corrplot")
library(corrplot)
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black")

# PHASE 3: STATISTICAL ANALYSIS
# Descriptive Stats by store type
type_summary <- retail %>%
  group_by(Type) %>%
  summarise(
    Mean_Sales = mean(Weekly_Sales, na.rm = TRUE),
    Median_Sales = median(Weekly_Sales, na.rm = TRUE),
    SD_Sales = sd(Weekly_Sales, na.rm = TRUE),
    Min_Sales = min(Weekly_Sales, na.rm = TRUE),
    Max_Sales = max(Weekly_Sales, na.rm = TRUE),
    N = n()
  )

install.packages("kableExtra")
library(kableExtra)
kable(type_summary, caption = "Summary of Weekly Sales by Store Type") %>%
  kable_styling(full_width = FALSE)

library(ggplot2)

# violin viz
ggplot(retail, aes(x = Type, y = Weekly_Sales, fill = Type)) +
  geom_violin(trim = FALSE, color = "black", alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +  # Optional boxplot inside
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Distribution of Weekly Sales by Store Type",
    x = "Store Type",
    y = "Weekly Sales"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

# store size vs average sales
# Step 1: Calculate average weekly sales per store
store_avg_sales <- retail %>%
  group_by(Store, Size) %>%
  summarise(Average_Sales = mean(Weekly_Sales, na.rm = TRUE), .groups = 'drop')

# Step 2: Plot
ggplot(store_avg_sales, aes(x = Size, y = Average_Sales)) +
  geom_point(aes(color = Average_Sales), size = 3, alpha = 0.7) +
  geom_smooth(method = "loess", se = TRUE, color = "black", linetype = "dashed") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Store Size vs Average Weekly Sales",
    x = "Store Size (sq ft)",
    y = "Average Weekly Sales"
  ) +
  theme_minimal(base_size = 14)

#Markdown1-5 over time

# Prepare long-format data
markdown_long <- retail %>%
  select(Date, IsHoliday, starts_with("MarkDown")) %>%
  pivot_longer(cols = starts_with("MarkDown"), names_to = "MarkDownType", values_to = "Value")

# Plot
ggplot(markdown_long, aes(x = Date, y = Value, color = IsHoliday)) +
  geom_line(alpha = 0.5, size = 0.7) +
  facet_wrap(~ MarkDownType, scales = "free_y") +
  scale_color_manual(values = c("FALSE" = "#999999", "TRUE" = "#E41A1C")) +
  labs(
    title = "MarkDown1–5 Trends Over Time (Holiday Weeks Highlighted)",
    subtitle = "Red lines indicate holiday weeks",
    x = "Date",
    y = "MarkDown Value",
    color = "Holiday"
  ) +
  theme_minimal(base_size = 14)

# Impact analysis (cont'd): how various factors influence weekly sales

