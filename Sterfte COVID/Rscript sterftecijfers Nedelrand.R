library(tidyverse)
library(showtext)
library(ggtext)
library(lubridate)


font_add_google("Libre Franklin", "franklin")
showtext_opts(dpi = 300)
showtext_auto()


library(readr)
Overledenen_kerncijfers_02052025_133132 <- read_delim("D:/Onderzoek/Article/Sterfte COVID/Overledenen__kerncijfers_02052025_133132.csv", 
      delim = ";", escape_double = FALSE, trim_ws = TRUE)
str(df)

ggsave("pre_post_covid.png", width = 6, height = 4.26)
View(df)
df <- df[,2:4]
colnames(df)
colnames(df) <- c("period", "total_deaths", "deathrate")

# Calculating the Death Rate
# To determine the death rate (number of deaths per 1,000 inhabitants),
# you'll need the total number of deaths and the population size for the 
# corresponding period.
# 
# For 2024:
# 
# Total deaths: 172,051
# 
# Estimated population: 17.5 million
# 
# Death rate: Approximately 9.83 deaths per 1,000 inhabitants
# Centraal Bureau voor de Statistiek
# 
# Centraal Bureau voor de Statistiek
# 
# 
# For 2025 (up to week 8):
# 
# Total deaths: Approximately 30,186 (sum of weekly
# deaths from weeks 1 to 8)
# 
# Estimated population: 17.6 million
# 
# Annualized death rate: Approximately 9.9 deaths per 1,000 inhabitants
# Centraal Bureau voor de Statistiek
# 
# Note: The 2025 figures are provisional and subject to revision
# as more data becomes available.

library(tibble)

# New data for 2024 and 2025 (estimated)
new_data <- tibble(
  period = c("2024", "2025*"),
  total_deaths = c(172051, 30186 * (52 / 8)),  # 2025 estimate extrapolated from week 1–8
  deathrate = c(9.83, (30186 * (52 / 8)) / 17600000 * 1000)  # Assuming population ~17.6 million
)

# View the new data
print(new_data)

library(tibble)
library(dplyr)

# Original df
df <- tibble(
  period = c("1950", "1960", "1970", "1980", "1990", "2000", "2010", 
             "2015", "2020", "2023"),
  total_deaths = c(75929, 87825, 109619, 114279, 128824, 140527, 
                   136058, 147134, 168678, 169521),
  deathrate = c(7.5, 7.6, 8.4, 8.1, 8.6, 8.8, 8.2, 8.7, 9.7, 9.5)
)

# New data for 2024 and 2025
new_data <- tibble(
  period = c("2024", "2025*"),
  total_deaths = c(172051, 30186 * (52 / 8)),  # Estimate for 2025
  deathrate = c(9.83, (30186 * (52 / 8)) / 17600000 * 1000)  # Estimate using population ~17.6 million
)

# Combine both tibbles
df_combined <- bind_rows(df, new_data)

# View the result
print(df_combined)

covid_pre_post <- df_combined |> 
  select(period, deathrate) %>%
  mutate(pre_post = if_else(period < "2020", "pre", "post"))

library(dplyr)
library(ggplot2)
library(lubridate)

# Clean up 'period' (remove "*" and convert to numeric)
covid_pre_post <- df_combined %>%
  mutate(period = as.numeric(gsub("\\*", "", period))) %>%
  select(period, deathrate) %>%
  mutate(pre_post = if_else(period < 2020, "pre", "post"))

# Calculate means
pre_deaths <- covid_pre_post %>%
  filter(pre_post == "pre") %>%
  summarize(mean = mean(deathrate)) %>%
  pull(mean)

post_deaths <- covid_pre_post %>%
  filter(pre_post == "post") %>%
  summarize(mean = mean(deathrate)) %>%
  pull(mean)

# Plot
covid_pre_post %>%
  ggplot(aes(x = period, y = deathrate, color = pre_post)) +
  geom_rect(aes(xmin = 2020, xmax = 2025,
                ymin = -Inf, ymax = Inf),
            fill = "#FCF0E9", color = NA, inherit.aes = FALSE) +
  geom_hline(yintercept = c(pre_deaths, post_deaths),
             linewidth = c(0.4, 0.4),
             color = c("gray60", "black"),
             linetype = c(2, 1)) +
  geom_line(linewidth = 0.75, show.legend = FALSE) +
  labs(x = "Year", y = "Death rate",
       title = "Death Rate in the Netherlands: Pre- and Post-COVID") +
  theme_minimal()

ggplot(covid_pre_post, aes(x = period, y = deathrate, color = pre_post, group = 1)) +
  geom_rect(aes(xmin = 2020, xmax = 2025, ymin = -Inf, ymax = Inf),
            fill = "#FCF0E9", color = NA, inherit.aes = FALSE) +
  geom_hline(yintercept = c(pre_deaths, post_deaths),
             linewidth = c(0.4, 0.4),
             color = c("gray60", "black"),
             linetype = c(2, 1)) +
  geom_line(linewidth = 0.75) +
  scale_x_continuous(breaks = covid_pre_post$period) +
  labs(x = "Year", y = "Death rate",
       color = "Period",
       title = "Death Rate in the Netherlands: Pre- and Post-COVID") +
  theme_minimal()




library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)  # Load the scales package

# Clean and prepare data
covid_pre_post <- df_combined %>%
  mutate(period = as.numeric(gsub("\\*", "", period))) %>%
  select(period, deathrate) %>%
  mutate(pre_post = if_else(period < 2020, "pre", "post"))

# Calculate means
pre_deaths <- covid_pre_post %>%
  filter(pre_post == "pre") %>%
  summarize(mean = mean(deathrate)) %>%
  pull(mean)

post_deaths <- covid_pre_post %>%
  filter(pre_post == "post") %>%
  summarize(mean = mean(deathrate)) %>%
  pull(mean)

# Plot with annotations and style
covid_pre_post %>%
  ggplot(aes(x = period, y = deathrate, color = pre_post, group = 1)) +
  geom_rect(aes(xmin = 2020, xmax = 2025, ymin = -Inf, ymax = Inf),
            fill = "#FCF0E9", color = "white", inherit.aes = FALSE) +
  geom_hline(yintercept = c(pre_deaths, post_deaths),
             linewidth = c(0.4, 0.4),
             color = c("gray80", "darkred"),
             linetype = c(2, 1)) +
  geom_line(linewidth = 0.75) +
  annotate(geom = "text", hjust = 0, vjust = -0.2,
           family = "franklin", size = 8.5, size.unit = "pt",
           x = 2015, y = pre_deaths * 0.98,
           label = paste0(round(pre_deaths,1), " deaths pre-COVID")) +
  annotate(geom = "text", family = "franklin",
           x = 2022, y = post_deaths * 1.02,
           label = paste0(round(post_deaths,1), " deaths post-COVID")) +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_y_continuous(breaks = c(8, 9, 10),
                     limits = c(7, 10.5)) +
  scale_x_continuous(breaks = seq(1950, 2025, by = 5),  # All years shown
                     labels = function(x) ifelse(x %in% c(2015, 2020, 2025), as.character(x), ""),
                     expand = c(0, 0)) +  # Custom x-axis labels
  scale_color_manual(breaks = c("pre", "post"),
                     values = c("#58595B", "#F05A27")) +
  labs(x = NULL,
       y = "Death rate per 1,000 inhabitants",
       title = "Death Rate in the Netherlands: Pre- and Post-COVID") +
  theme(
    text = element_text(family = "franklin"),
    plot.background = element_rect(fill = "#EEEEEE"),
    plot.title = element_textbox_simple(face = "bold", size = 11.5,
                                        fill = "white", width = NULL,
                                        padding = margin(5, 4, 5, 4),
                                        hjust = 0),
    plot.margin = margin(4, 15, 10, 10),
    axis.text.y = element_text(size = 8.5, color = "black"),
    axis.ticks.y = element_line(linewidth = 0.4),
    axis.text.x = element_text(size = 8.5, color = "black"),
    axis.ticks.length.x = unit(4, "pt"),
    axis.ticks.x = element_line(linewidth = 0.4),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_rect(fill = NA)
  )



library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

# Your original covid_pre_post dataset already contains 2025
covid_pre_post <- tibble(
  period = c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2015, 2020, 2023, 2024, 2025),
  deathrate = c(7.5, 7.6, 8.4, 8.1, 8.6, 8.8, 8.2, 8.7, 9.7, 9.5, 9.83, 11.1),
  pre_post = c("pre", "pre", "pre", "pre", "pre", "pre", "pre", "pre", "post", "post", "post", "post")
)

# Calculate means for pre and post periods
pre_deaths <- covid_pre_post %>%
  filter(pre_post == "pre") %>%
  summarize(mean = mean(deathrate)) %>%
  pull(mean)

post_deaths <- covid_pre_post %>%
  filter(pre_post == "post") %>%
  summarize(mean = mean(deathrate)) %>%
  pull(mean)

# Plot with the 2025 data point correctly reflected
covid_pre_post %>%
  ggplot(aes(x = period, y = deathrate, color = pre_post, group = 1)) +
  geom_rect(aes(xmin = 2020, xmax = 2025, ymin = -Inf, ymax = Inf),
            fill = "#FCF0E9", color = "white", inherit.aes = FALSE) +
  geom_hline(yintercept = c(pre_deaths, post_deaths),
             linewidth = c(0.4, 0.4),
             color = c("gray20", "darkred"),
             linetype = c(2, 2)) +
  geom_line(linewidth = 0.75) +  # Line graph with the new 2025 data point
  annotate(geom = "text", hjust = 0, vjust = -0.2,
           family = "franklin", size = 8.5, size.unit = "pt",
           x = 2000, y = pre_deaths * 0.98,
           label = paste0("Average Death Rate pre-COVID")) +
  annotate(geom = "text", family = "franklin",
           x = 2005, y = post_deaths * 1.02,
           label = paste0("Average Death Rate post-COVID")) +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_y_continuous(breaks = c(8, 9, 10, 11),
                     limits = c(7, 11.5)) +
  scale_x_continuous(breaks = seq(1950, 2025, by = 5),  # Show all years on x-axis
                     labels = seq(1950, 2025, by = 5),  # Label all years
                     expand = c(0, 0)) +  # Custom x-axis labels
  scale_color_manual(breaks = c("pre", "post"),
                     values = c("#58595B", "#F05A27")) +
  labs(x = "Year",
       y = "Death rate per 1,000 inhabitants",
       title = "Death Rate in the Netherlands: Pre- and Post-COVID",
       caption = paste("Data source: CBS, the Netherlands\n",
                       "Links to data sources:\n",
                       "Population statistics: https://www.cbs.nl/en-gb\n",
                       "Death rates data: https://opendata.cbs.nl/statline\n",
                       "Plot created by Edward F. Hillenaar, Psychologist and Data Scientist")) +
  theme(
    legend.position = "none",
    text = element_text(family = "franklin"),
    plot.background = element_rect(fill = "#EEEEEE"),
    plot.title = element_textbox_simple(face = "bold", size = 20,
                                        fill = "white", width = NULL,
                                        padding = margin(5, 4, 5, 4),
                                        hjust = 0),
    plot.margin = margin(4, 15, 10, 10),
    axis.text.y = element_text(size = 8.5, color = "black"),
    axis.ticks.y = element_line(linewidth = 0.4),
    axis.text.x = element_text(size = 8.5, color = "black"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text((margin = margin(r = 15))),
    axis.ticks.length.x = unit(4, "pt"),
    axis.ticks.x = element_line(linewidth = 0.4),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_rect(fill = NA),
    plot.caption = element_text(hjust = 0, size = 10, 
      color = "black", face = "italic", margin = margin(t = 10))
  )


library(ggstatsplot)
library(dplyr)

# Ensure 'pre' comes before 'post' on the x-axis
covid_pre_post <- covid_pre_post %>%
  mutate(pre_post = factor(pre_post, levels = c("pre", "post")))

ggbetweenstats(
  data = covid_pre_post,
  x = pre_post,
  y = deathrate,
  title = "Pre vs. Post-COVID Death Rate Comparison",
  xlab = "Period",
  ylab = "Death Rate",
  plot.type = "boxviolin",
  type = "parametric",  # Use nonparametric if data isn't normally distributed
  centrality.plotting = TRUE,
  centrality.type = "mean",
  point.args = list(size = 4),
  ggtheme = ggplot2::theme_minimal()
)

library(effects)
library(effectsize)
library(report)

interpret_hedges_g(-2.38)
# [1] "large"
# (Rules: cohen1988)


t.test(deathrate ~ pre_post, data = covid_pre_post) |> 
  report()

# Effect sizes were labelled following Cohen's (1988)
# recommendations.
# 
# The Welch Two Sample t-test testing the difference of deathrate
# by pre_post (mean in group pre = 8.24, mean in group post =
# 10.03) suggests that the effect is negative, statistically
# significant, and large (difference = -1.79, 95% CI [-2.87,
# -0.72], t(4.41) = -4.48, p = 0.009; Cohen's d = -4.26, 95% CI
# [-7.50, -0.91])


