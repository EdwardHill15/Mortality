library(tidyverse)
library(showtext)
library(ggtext)
library(lubridate)
library(stringr)
library(ISOweek)


font_add_google("Libre Franklin", "franklin")
showtext_opts(dpi = 300)
showtext_auto()


X70895ned_UntypedDataSet_02052025_160412 <- read_delim("D:/Onderzoek/Article/Sterfte COVID/70895ned_UntypedDataSet_02052025_160412.csv", 
                                                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
df <- X70895ned_UntypedDataSet_02052025_160412
str(df)


# 2. Gemiddelde voor pre- en post-covid berekenen
pre_deaths <- deaths_by_week %>%
  filter(pre_post == "pre") %>%
  summarise(mean = mean(deaths)) %>%
  pull(mean)

post_deaths <- deaths_by_week %>%
  filter(pre_post == "post") %>%
  summarise(mean = mean(deaths)) %>%
  pull(mean)

cat("Gemiddelde sterfte per week vóór COVID:", round(pre_deaths, 0), "\n")
cat("Gemiddelde sterfte per week na COVID:", round(post_deaths, 0), "\n")

# Example: deaths_by_week already has columns: date, deaths, pre_post

library(stringr)

# Wrap your caption at 100 characters (adjust as needed)
my_caption <- str_wrap(
  paste("Data source: CBS, the Netherlands",
        "Links to data sources:",
        "Population statistics: https://www.cbs.nl/en-gb",
        "Death rates data: https://opendata.cbs.nl/statline",
        "Plot created by Edward F. Hillenaar, Psychologist and Data Scientist",
        sep = "\n"),
  width = 100
)


deaths_by_week |>
  filter(deaths >= 4000) |> 
  ggplot(aes(x = date, y = deaths, color = pre_post)) +
  geom_rect(aes(xmin = as.Date("2020-03-01"), xmax = as.Date("2025-03-10"),
                ymin = 0, ymax = 10000),
            fill = "#FCF0E9", color = NA, inherit.aes = FALSE) +
  geom_hline(yintercept = c(5337, 6534, pre_deaths),
             linewidth = c(0.2, 0.2, 0.4),
             color = c("gray20", "darkred", "black"),
             linetype = c(2, 2, 2)) +
  geom_line(linewidth = 0.7, alpha = 0.85) +
  labs(
    x = "Year",
    y = "Deaths per week",
    title = "Weekly Deaths in the Netherlands",
    color = "Period",
    caption = my_caption
  ) +
  annotate(geom = "text", hjust = 0, vjust = -0.2,
           size = 2,
           x = as.Date(c("2015-03-01", "2020-03-01")),
           y = c(5337, 6534),
           label = c("5,337 deaths", "6,534 deaths")) +
  coord_cartesian(expand = FALSE, clip = "off") +
  scale_color_manual(breaks = c("pre", "post"),
                     values = c("pre" = "#009EBC", "post" = "#F05A27"),
                     labels = c("pre" = "Pre-COVID", "post" = "Post-COVID")) +
  scale_y_continuous(limits = c(3000, 10000)) +
  scale_x_date(
    breaks = as.Date(c("1995-01-01", "2000-01-01", "2005-01-01", "2010-01-01", "2015-01-01", "2020-01-01", "2025-01-01")),
    labels = c("1995", "2000", "2005", "2010", "2015", "2020", "2025"),
    limits = c(as.Date("1995-01-01"), as.Date("2025-12-31"))
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
      hjust = 0, size = 4, color = "black", face = "italic",
      margin = margin(t = 10, r = 0, b = 0, l = 0),
      lineheight = 2.5
    )
  )


library(ggstatsplot)
library(dplyr)

# Ensure 'pre' comes before 'post' on the x-axis
covid_pre_post <- covid_pre_post %>%
  mutate(pre_post = factor(pre_post, levels = c("pre", "post")))

colnames(deaths_by_week)


# Ensure 'pre' comes before 'post' on the x-axis
deaths_by_week <- deaths_by_week %>%
  mutate(pre_post = factor(pre_post, levels = c("pre", "post")))



ggbetweenstats(
  data = deaths_by_week,
  x = pre_post,
  y = deaths,
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

