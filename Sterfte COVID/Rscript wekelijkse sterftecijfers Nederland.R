library(tidyverse)
library(showtext)
library(ggtext)
library(lubridate)

library(readr)
X70895ned_UntypedDataSet_02052025_160412 <- read_delim("D:/Onderzoek/Article/Sterfte COVID/70895ned_UntypedDataSet_02052025_160412.csv", 
                                                       delim = ";", escape_double = FALSE, trim_ws = TRUE)

df <- X70895ned_UntypedDataSet_02052025_160412 

df_national <- df %>%
  filter(Geslacht == "T001038", LeeftijdOp31December == 10000)

library(stringr)
library(ISOweek)
df_national <- df_national %>%
  filter(str_detect(Perioden, "^\\d{4}W\\d{3}$")) %>%
  mutate(
    year = as.numeric(str_sub(Perioden, 1, 4)),
    week = as.numeric(str_sub(Perioden, 6, 7)),
    iso_week_str = paste0(year, "-W", sprintf("%02d", week), "-1"),
    date = ISOweek2date(iso_week_str)
  ) %>%
  filter(!is.na(week), week >= 1, week <= 53)

df_covid_summary <- df_national %>%
  select(year, week, deaths = Overledenen_1, date)

deaths_by_week <- df_covid_summary %>%
  group_by(date) %>%
  summarize(deaths = sum(deaths, na.rm = TRUE), .groups = "drop") %>%
  filter(deaths >= 1500) %>%
  mutate(pre_post = if_else(date < as.Date("2020-03-01"), "pre", "post"))

pre_deaths <- deaths_by_week %>%
  filter(pre_post == "pre") %>%
  summarize(mean = mean(deaths, na.rm = TRUE)) %>%
  pull(mean)

post_deaths <- deaths_by_week %>%
  filter(pre_post == "post") %>%
  summarize(mean = mean(deaths, na.rm = TRUE)) %>%
  pull(mean)



df_covid_summary %>%
  count(date) %>%
  filter(n > 1)











deaths_by_week$pre_post <- as.factor(deaths_by_week$pre_post)
str(deaths_by_week)
deaths_by_week$year

table(deaths_by_week$week)

deaths_by_week %>%
  ggplot(aes(x = date, y = deaths, color = pre_post)) +
  geom_point(show.legend = FALSE, size = 1.5)

deaths_by_week %>%
  filter(deaths >= 1500) %>%
  ggplot(aes(x = date, y = deaths, color = pre_post)) +
  geom_point(size = 1.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  scale_color_manual(values = c("pre" = "#58595B", "post" = "#F05A27"),
                     labels = c("pre" = "Pre-COVID", "post" = "Post-COVID")) +
  labs(x = "Date", y = "Deaths per week", 
       title = "Weekly deaths in the Netherlands (weeks 10-15)",
       color = "Period") +
  theme_minimal()

library(dplyr)

df_line <- deaths_by_week %>%
  filter(deaths >= 1500) %>%
  group_by(year, week, date, pre_post) %>%
  summarize(deaths = mean(deaths), .groups = "drop")  # or sum(deaths) if appropriate


deaths_by_week %>%
  ggplot(aes(x = date, y = deaths, color = pre_post)) +
  geom_line(show.legend = FALSE, linewidth = 0.75)
  

library(ggplot2)

ggplot(df_line, aes(x = date, y = deaths, color = pre_post)) +
  geom_line(linewidth = 0.75, show.legend = FALSE) +
  geom_point(size = 1.5, alpha = 0.7) +
  scale_color_manual(values = c("pre" = "#58595B", "post" = "#F05A27")) +
  labs(x = "Date", y = "Deaths per week",
       title = "Weekly deaths in the Netherlands (weeks 10-15, outliers removed)") +
  theme_minimal()



  
  # geom_rect(aes(xmin = ymd("2020-03-01"), xmax = ymd("2026-03-01"),
  #               ymin = 0, ymax = 6000),
  #           fill = "#FCF0E9", color = NA, inherit.aes = FALSE) +
  geom_hline(yintercept = c(3000, 4000, pre_deaths),
             linewidth = c(0.2, 0.2, 0.4),
             color = c("gray80", "gray80", "black"),
             linetype = c(1, 1, 2)) +
  geom_line(show.legend = FALSE, linewidth = 0.75) +
  annotate(geom = "text", hjust = 0, vjust = -0.2,
           size = 3.5,
           x = ymd("2015-03-01"),
           y = c(3000, 4000),
           label = c("3,000", "4,000 deaths per week")) +
  annotate(geom = "text",
           x = ymd("2022-09-01"),
           y = pre_deaths * 0.98,
           label = "2015-19 average") +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_y_continuous(breaks = c(3000, 4000),
                     limits = c(2000, 5000)) +
  scale_x_date(breaks = ymd(c("2016-01-01", "2018-01-01", "2020-03-01",
                              "2022-01-01", "2024-01-01")),
               labels = c("2016", "2018", "March 2020", "2022", "2024")) +
  scale_color_manual(breaks = c("pre", "post"),
                     values = c("#58595B", "#F05A27")) +
  labs(x = NULL,
       y = NULL,
       title = "Total deaths per week in the Netherlands") +
  theme(
    plot.background = element_rect(fill = "#EEEEEE"),
    plot.title = element_textbox_simple(face = "bold", size = 11.5,
                                        fill = "white", width = NULL,
                                        padding = margin(5, 4, 5, 4),
                                        hjust = 0),
    plot.margin = margin(4, 15, 10, 10),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 8.5, margin = margin(t = 3),
                               color = "black"),
    axis.ticks.length.x = unit(4, "pt"),
    axis.ticks.x = element_line(linewidth = 0.4),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_rect(fill = NA)
  )





