library(tidyverse)
library(jsonlite)
library(lubridate)

# Fetch the data
df <- read.csv("https://ourworldindata.org/grapher/excess-mortality-p-scores-average-baseline.csv?v=1&csvType=full&useColumnShortNames=true")

# Fetch the metadata
metadata <- fromJSON("https://ourworldindata.org/grapher/excess-mortality-p-scores-average-baseline.metadata.json?v=1&csvType=full&useColumnShortNames=true")

df$Entity
str(df)
df$Day <- as.Date(df$Day, format = "%Y-%m-%d")

df |> 
  filter(Entity == c("Netherlands", "Belgium", "France", "Germany")) |> 
  ggplot(aes(x=Day, y=p_avg_all_ages, color = Entity)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4, alpha = 0.7)

df |> 
  filter(Entity == "Netherlands") |> 
  ggplot(aes(x = Day, y = p_avg_all_ages, color = Entity)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2, alpha = 0.7) +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_hline(yintercept = c(10, 20, 30, 40, 50), 
             linetype = "dashed", color = "grey40") +
  labs(
    title = "Excess mortality: Deaths from all causes compared to average over previous years",
    subtitle = "Percentage difference between the reported weekly or monthly deaths in 2020–2024\nand the average deaths in the same period in 2015–2019.",
    caption = "Data source: Human Mortality Database (2024); World Mortality Dataset (2024); Karlinsky and Kobak (2021) – Learn more about this data\n\nNote: The reported number of deaths might not count all deaths that occurred due to incomplete coverage and delays in reporting.\n\nOurWorldinData.org/coronavirus | CC BY",
    x = NULL,
    y = NULL
  ) +
  scale_y_continuous(
    limits = c(-10, 60),
    breaks = seq(-10, 60, by = 10),
    labels = function(x) paste0(x, "%")
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 9, hjust = 0)
  )
  



df[df$Entity == "Netherlands", ]




library(ggplot2)
library(dplyr)

# Define your custom date breaks
custom_dates <- as.Date(c(
  "2020-01-05", "2021-02-24", "2021-09-12", "2022-03-31",
  "2022-10-17", "2023-05-05", "2023-11-21", "2024-06-08", "2025-03-16"
))

df |> 
  filter(Entity == "Netherlands") |> 
  ggplot(aes(x = Day, y = p_avg_all_ages, color = Entity)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2, alpha = 0.7) +
  geom_hline(yintercept = -20, linetype = "dashed", color = "black") +  # This acts as the x-axis
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_hline(yintercept = c(-10, 10, 20, 30, 40, 50), 
             linetype = "dashed", color = "grey40") +
  scale_y_continuous(
    limits = c(-20, 60),
    breaks = seq(-20, 60, by = 10),
    labels = function(x) paste0(x, "%")
  ) +
  scale_x_date(
    breaks = custom_dates,
    labels = format(custom_dates, "%b %d, %Y")
  ) +
  labs(
    title = "Excess mortality: Deaths from all causes compared to average over previous years",
    subtitle = "Percentage difference between the reported weekly or monthly deaths in 2020–2024\nand the average deaths in the same period in 2015–2019.",
    caption = "Data source: Human Mortality Database (2024); World Mortality Dataset (2024); Karlinsky and Kobak (2021) – Learn more about this data\n\nNote: The reported number of deaths might not count all deaths that occurred due to incomplete coverage and delays in reporting.\n\nOurWorldinData.org/coronavirus | CC BY",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 9, hjust = 0)
  )




# Define custom date breaks
custom_dates <- as.Date(c(
  "2020-01-05", "2021-02-24", "2021-09-12", "2022-03-31",
  "2022-10-17", "2023-05-05", "2023-11-21", "2024-06-08", "2025-03-16"
))

# Create a vector of formatted date labels
date_labels <- format(custom_dates, "%b %d, %Y")

df |> 
  filter(Entity == "Netherlands") |> 
  ggplot(aes(x = Day, y = p_avg_all_ages, color = Entity)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2, alpha = 0.7) +
  # X-axis-style line at -20%
  geom_hline(yintercept = -20, linetype = "dashed", color = "black") +
  # Reference lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_hline(yintercept = c(-10, 10, 20, 30, 40, 50, 60), 
             linetype = "dashed", color = "grey40") +
  # Vertical tick marks at each date point on the -20% axis
  geom_segment(
    data = data.frame(x = custom_dates),
    aes(x = x, xend = x, y = -20.5, yend = -19.5),
    inherit.aes = FALSE,
    color = "black"
  ) +
  # Manual x-axis labels with bigger, bold font
  annotate("text", x = custom_dates, y = -22.5, label = date_labels,
           angle = 0, size = 4, fontface = "bold", hjust = 0.5) +
  scale_x_date(
    breaks = custom_dates,
    labels = NULL
  ) +
  # Y-axis formatting
  scale_y_continuous(
    limits = c(-25, 60),
    breaks = seq(-20, 60, by = 10),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Excess mortality in The Netherlands: Deaths from all causes compared to average over previous years",
    subtitle = "Percentage difference between the reported weekly or monthly deaths in 2020–2024\nand the average deaths in the same period in 2015–2019.",
    caption = "Data source: Human Mortality Database (2024); World Mortality Dataset (2024); Karlinsky and Kobak (2021)\nNote: The reported number of deaths might not count all deaths that occurred due to incomplete coverage and delays in reporting.\n\nOurWorldinData.org/coronavirus | CC BY",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 17),
    plot.caption = element_text(size = 14, hjust = 0),
    axis.text.x = element_blank(),  # Remove default x-axis text
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.ticks.x = element_blank(),
    legend.position = "none"
  )

country_excess_deaths <- read_csv("D:/Onderzoek/Article/Sterfte COVID/Excess mortality COVID/country_excess_deaths.csv")
df2 <- country_excess_deaths

colnames(df2)
library(janitor)
df2 <- clean_names(df2)
str(df2)
df2 = df2[,c(1:5,8:10)]
df2
df2$data_until <- mdy(df2$data_until)

num_cols <- c("excess", "undercount", "per_100k", "increase")
df2 <- df2 %>%
  mutate(across(all_of(num_cols), parse_number))

df2$type <- factor(df2$type)

library(dplyr)

df2 <- df2 %>% arrange(desc(increase))
head(df2, 25)

df2$country
df2$country = factor(df2$country)
library(forcats)

df2 |> 
  filter(country %in% c("Belgium", "Netherlands", "France", "Germany")) |> 
  mutate(country = fct_reorder(country, increase)) |> 
  ggplot(aes(x = country, y = increase, color = country, fill = country)) +
  geom_col() +
  theme_minimal()
