# BLOCK 1 - packages
install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
install.packages("tidyverse" )
install.packages("scales")
install.packages("gridExtra")
install.packages("gt")
install.packages("lubricate")
search()
install.packages("readr")

getwd()
list.files()

setwd("C:/Users/ADMIN/OneDrive/Documents")


# BLOCK 2 - load data
install.packages("readxl")
library(readxl)

sales <- read_excel("Supermarket Sales.csv.xlsx")
head(sales)
str(sales)
colnames(sales)
head(sales$order_date)
# BLOCK 2- loading remaining packages
library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)
# BLOCK 4 - clean data
sales <- sales %>%
  rename(
    order_no      = `Order No`,
    order_date    = `Order Date`,
    customer_name = `Customer Name`,
    ship_date     = `Ship Date`,
    retail_price  = `Retail Price (USD)`,
    quantity      = `Order Quantity`,
    tax           = `Tax (USD)`,
    net_revenue   = `Net Revenue`,
    total         = `Total (USD)`
  ) %>%
  mutate(
    Month = floor_date(as.Date(ship_date), "month")
  )

# Confirm it worked
head(sales$Month)

# Fix the Month column
sales <- sales %>%
  mutate(
    Month = floor_date(as.Date(ship_date), "month")
  )

# BLOCK 5 - theme
portfolio_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", size = 15),
    plot.subtitle      = element_text(color = "grey50"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position    = "top",
    legend.title       = element_blank()
  )
brand_colors <- c("#2E86AB","#E84855","#F9A03F","#A23B72","#3BB273","#6C4F77")

# BLOCK 6 - bar chart top 10 customers
library(forcats)
top_customers <- sales %>%
  group_by(customer_name) %>%
  summarise(total_revenue = sum(total), .groups = "drop") %>%
  arrange(desc(total_revenue)) %>%
  slice_head(n = 10) %>%
  mutate(customer_name = fct_reorder(customer_name, total_revenue))

bar_chart <- ggplot(top_customers,
                    aes(x = customer_name, y = total_revenue, fill = customer_name)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(aes(label = dollar(total_revenue, prefix = "$")),
            hjust = -0.1, size = 3.5, color = "grey30") +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(prefix = "$"),
                     expand = expansion(mult = c(0, 0.2))) +
  scale_fill_manual(values = colorRampPalette(brand_colors)(10)) +
  labs(title    = "Top 10 Customers by Revenue",
       subtitle = "Ranked by total amount spent",
       x = NULL, y = "Total Revenue (USD)") +
  portfolio_theme

print(bar_chart)

# BLOCK 7- Line chart monthly trend
monthly_revenue <- df %>%
  group_by(Month) %>%
  summarise(total_revenue = sum(total), .groups = "drop")

line_chart <- ggplot(monthly_revenue, aes(x = Month, y = total_revenue)) +
  geom_line(color = "#2E86AB", linewidth = 1.3) +
  geom_point(size = 3.5, shape = 21, fill = "white",
             color = "#2E86AB", stroke = 2) +
  geom_area(fill = "#2E86AB", alpha = 0.08) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  labs(title    = "Monthly Revenue Trend",
       subtitle = "Total sales over time",
       x = NULL, y = "Total Revenue (USD)") +
  portfolio_theme +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

print(line_chart)