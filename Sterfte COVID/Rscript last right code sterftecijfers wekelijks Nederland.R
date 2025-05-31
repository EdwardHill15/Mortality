library(readr)
library(dplyr)
library(stringr)
library(ISOweek)
library(ggplot2)

# Adjust the path if needed
df <- read_delim("D:/Onderzoek/Article/Sterfte COVID/70895ned_UntypedDataSet_02052025_160412.csv", 
                 delim = ";", escape_double = FALSE, trim_ws = TRUE)
colnames(df)
df_filtered <- df %>%
  filter(
    Geslacht == "T001038",
    LeeftijdOp31December == 10000,
    
  )
colnames(df_filtered)
df_weeks <- df_filtered %>%
  filter(str_detect(Perioden, "^\\d{4}W\\d{3}$")) %>%
  mutate(
    year = as.integer(substr(Perioden, 1, 4)),
    week = as.integer(str_sub(Perioden, -2, -1)), # always last two digits
    iso_week = sprintf("%d-W%02d", year, week)
  ) %>%
  filter(week >= 1 & week <= 53) %>%
  mutate(week_date = ISOweek2date(paste0(iso_week, "-1")))

colnames(df_weeks)
df_weeks_2015 <- df_weeks |> 
  filter(year > 2015)

ggplot(df_weeks, aes(x = week_date, y = Overledenen_1)) +
  geom_line(color = "steelblue") +
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed", color = "red") +
  annotate("text", x = as.Date("2020-03-15"), y = max(df_weeks$Overledenen_1, na.rm = TRUE),
           label = "COVID-19 start", color = "red", hjust = 0, vjust = 1.2, size = 3.5) +
  labs(
    title = "Weekly Deaths in the Netherlands (All Ages, Total Sexes)",
    x = "Week",
    y = "Number of Deaths"
  ) +
  theme_minimal()



df_weeks <- df_weeks %>%
  mutate(
    covid_period = ifelse(week_date < as.Date("2020-03-01"), "Pre-COVID", "Post-COVID")
  )
colnames(df_weeks)
df_weeks |> 
filter(Overledenen_1 > 1900) |> 
ggplot(aes(x = week_date, y = Overledenen_1, color = covid_period)) +
  geom_line() +
  labs(
    title = "Weekly Deaths in the Netherlands",
    x = "Week",
    y = "Number of Deaths",
    color = ""
  ) +
  scale_color_manual(values = c("Pre-COVID" = "steelblue", "Post-COVID" = "firebrick")) +
  theme_minimal()



# 2. Gemiddelde voor pre- en post-covid berekenen
pre_deaths <- df_weeks_2015 %>%
  filter(covid_period == "Pre-COVID", year > 2015) %>%
  summarise(mean = mean(Overledenen_1)) %>%
  pull(mean)

post_deaths <- df_weeks_2015 %>%
  filter(covid_period == "Post-COVID") %>%
  summarise(mean = mean(Overledenen_1)) %>%
  pull(mean)

cat("Gemiddelde sterfte per week vóór COVID:", round(pre_deaths, 0), "\n")
cat("Gemiddelde sterfte per week na COVID:", round(post_deaths, 0), "\n")

# Example: deaths_by_week already has columns: date, deaths, pre_post

library(stringr)

# Wrap your caption at 100 characters (adjust as needed)
my_caption <- str_wrap(
  paste("Data source: CBS, the Netherlands",
        "Links to data sources:",
        "1. Population statistics: https://www.cbs.nl/en-gb",
        "2. Death rates data: https://opendata.cbs.nl/statline",
        sep = "\n"),
  width = 100
)
str(df_weeks)
colnames(df_weeks)
df_weeks$year

df_weeks <- df_weeks %>%
  mutate(
    covid_period = ifelse(week_date < as.Date("2020-03-01"), "pre", "post")
  )



df_weeks |>
  filter(year >= 2015) |> 
  ggplot(aes(x = week_date, y = Overledenen_1, color = covid_period, 
             group = covid_period)) +
  geom_rect(aes(xmin = as.Date("2020-03-01"), xmax = as.Date("2025-03-10"),
                ymin = 1800, ymax = 5000),
            fill = "#F8D7C3", color = "white", alpha = 0.3, inherit.aes = FALSE) +
  geom_hline(yintercept = c(2877, 3267, pre_deaths),
             linewidth = c(0.2, 0.2, 0.4),
             color = c("gray20", "darkred", "black"),
             linetype = c(2, 2, 2)) +
  geom_line(linewidth = 1, alpha = 0.85) +
  labs(
    x = "Year",
    y = "Deaths per week",
    title = "Weekly Deaths in the Netherlands",
    color = "Period",
    caption = my_caption
  ) +
  annotate(geom = "text", hjust = 0, vjust = -0.2,
           size = 2,
           x = as.Date(c("2017-03-01", "2022-01-01")),
           y = c(2877, 3267),
           label = c("2,877 deaths", "3,267 deaths")) +
  # Add Pre-COVID and Post-COVID annotations
  annotate(geom = "text", x = as.Date("2017-06-01"), y = 4700,
           label = "Pre-COVID", color = "#009EBC", 
           size = 3, fontface = "bold") +
  annotate(geom = "text", x = as.Date("2022-09-01"), y = 4700,
           label = "Post-COVID", color = "#F05A27", 
           size = 3, fontface = "bold") +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_color_manual(breaks = c("pre", "post"),
                     values = c("pre" = "#009EBC", "post" = "#F05A27"),
                     labels = c("pre" = "Pre-COVID", "post" = "Post-COVID")) +
  scale_y_continuous(limits = c(1800, 5000)) +
  scale_x_date(
    breaks = as.Date(c("2015-01-01", "2017-01-01", "2019-01-01", "2021-01-01", "2023-01-01", "2025-01-01")),
    labels = c("2015", "2017", "2019", "2021", "2023", "2025"),
    limits = c(as.Date("2015-01-01"), as.Date("2025-12-31"))
  ) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#EEEEEE"),
    plot.title = element_textbox_simple(face = "bold", size = 10,
                                        fill = "white", width = NULL,
                                        padding = margin(5, 4, 5, 4),
                                        hjust = 0,
                                        margin = margin(b = 10)),
    axis.title.x = element_text(margin = margin(t = 8), size = 7),
    axis.title.y = element_text(margin = margin(r = 8), size = 7),
    plot.margin = margin(4, 15, 50, 10), # bottom margin increased!
    axis.text.y = element_text(size = 6),
    axis.ticks.y = element_line(linewidth = 0.3),
    axis.text.x = element_text(size = 6, margin = margin(t = 3),
                               color = "black"),
    axis.ticks.length.x = unit(4, "pt"),
    axis.ticks.x = element_line(linewidth = 0.4),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_rect(fill = NA),
    plot.caption = element_text(
      hjust = 0, size = 5, color = "black", face = "italic",
      margin = margin(t = 10, r = 0, b = 0, l = 0),
      lineheight = 2.5
    )
  )


library(ggstatsplot)
library(dplyr)
df_weeks$covid_period
# Ensure 'pre' comes before 'post' on the x-axis
df_weeks <- df_weeks %>%
  mutate(covid_period = factor(covid_period, 
                  levels = c("Pre-COVID", "Post-COVID")))

colnames(df_weeks)


ggbetweenstats(
  data = df_weeks_2015,
  x = covid_period,
  y = Overledenen_1,
  title = "Pre vs. Post-COVID Deaths per week Comparison",
  xlab = "Period",
  ylab = "Deaths per week",
  plot.type = "boxviolin",
  type = "parametric",  # Use nonparametric if data isn't normally distributed
  centrality.plotting = TRUE,
  centrality.type = "mean",
  point.args = list(size = 4),
  ggtheme = ggplot2::theme_minimal(),
  ggplot.component = list(
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 10, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 6),
      plot.caption = element_text(size = 5, hjust = 0),
      axis.title = ggplot2::element_text(size = 5),
      axis.text = ggplot2::element_text(size = 4),
      legend.title = ggplot2::element_text(size = 2),
      legend.text = ggplot2::element_text(size = 2),
      strip.text = ggplot2::element_text(size = 2)
    )
  )
)

library(effects)
library(effectsize)
library(report)

interpret_hedges_g(-0.96)
# interpret_hedges_g(-0.96)
# [1] "large"
# (Rules: cohen1988)


t.test(Overledenen_1 ~ covid_period, 
       data = df_weeks_2015) |> 
  report()

# Effect sizes were labelled following Cohen's (1988) recommendations.
# 
# The Welch Two Sample t-test testing the difference of Overledenen_1 by
# covid_period (mean in group Pre-COVID = 2877.42, mean in group Post-COVID
# = 3266.90) suggests that the effect is negative, statistically
# significant, and large (difference = -389.48, 95% CI [-460.89, -318.08],
# t(485.73) = -10.72, p < .001; Cohen's d = -0.97, 95% CI [-1.16, -0.78])
# 



library(dplyr)
library(zoo)  # for rollmean

df_weeks_2015 <- df_weeks_2015 %>%
  arrange(week_date) %>%
  group_by(covid_period) %>%
  mutate(
    deaths_ma3 = zoo::rollmean(Overledenen_1, k = 3, fill = NA, align = "center"),
    deaths_ma5 = zoo::rollmean(Overledenen_1, k = 5, fill = NA, align = "center")
  ) %>%
  ungroup()

df_weeks_2015 |>
  ggplot(aes(x = week_date, y = deaths_ma3, color = covid_period, group = covid_period)) +
  geom_rect(aes(xmin = as.Date("2020-03-01"), xmax = as.Date("2025-03-10"),
                ymin = 1800, ymax = 5000),
            fill = "#FCF0E9", color = "white", inherit.aes = FALSE) +
  geom_hline(yintercept = c(2877, 3267, pre_deaths),
             linewidth = c(0.2, 0.2, 0.4),
             color = c("gray20", "darkred", "black"),
             linetype = c(2, 2, 2)) +
  geom_line(linewidth = 1, alpha = 0.85) +
  # ... rest of your code unchanged ...
  scale_color_manual(
    breaks = c("pre", "post"),
    values = c("pre" = "#009EBC", "post" = "#F05A27"),
    labels = c("pre" = "Pre-COVID", "post" = "Post-COVID")
  )



