# Start with your existing code to load and filter the data
library(tidyverse)
library(showtext)
library(ggtext)

font_add_google("Libre Franklin", "franklin")
showtext_opts(dpi = 300)
showtext_auto()


library(readr)
library(dplyr)
library(stringr)
library(ISOweek)
library(ggplot2)

# Load and prepare the data as you've done
X70895ned_UntypedDataSet_02052025_160412 <- read_delim("D:/Onderzoek/Article/Sterfte COVID/70895ned_UntypedDataSet_02052025_160412.csv", 
                                                       delim = ";", escape_double = FALSE, trim_ws = TRUE)

df <- X70895ned_UntypedDataSet_02052025_160412 

# Filter for national totals
df_national <- df %>%
  filter(Geslacht == "T001038", LeeftijdOp31December == 10000)

# Extract year and week information
df_national <- df_national %>%
  filter(str_detect(Perioden, "^\\d{4}W\\d{3}$")) %>%
  mutate(
    year = as.numeric(str_sub(Perioden, 1, 4)),
    week = as.numeric(str_sub(Perioden, 6, 7)),
    iso_week_str = paste0(year, "-W", sprintf("%02d", week), "-1"),
    date = ISOweek2date(iso_week_str)
  ) %>%
  filter(!is.na(week), week >= 1, week <= 53)


df_national <- df %>%
  filter(
    Geslacht == "T001038",
    LeeftijdOp31December == 10000,
    # Add similar filters for any other subgroup columns:
    # e.g., Regio == "Total", Herkomst == "Total", etc.
  )


deaths_by_week <- df_national %>%
  filter(str_detect(Perioden, "^\\d{4}W\\d{3}$")) %>%
  mutate(
    year = as.numeric(str_sub(Perioden, 1, 4)),
    week = as.numeric(str_sub(Perioden, 6, 7)),
    iso_week_str = paste0(year, "-W", sprintf("%02d", week), "-1"),
    date = ISOweek2date(iso_week_str),
    pre_post = if_else(date < as.Date("2020-03-01"), "pre", "post")
  ) %>%
  filter(!is.na(week), week >= 1, week <= 53, Overledenen_1 >= 1500) %>%
  select(year, week, deaths = Overledenen_1, date, pre_post)


# Calculate means for pre and post periods
pre_deaths <- deaths_by_week %>%
  filter(pre_post == "pre") %>%
  summarize(mean = mean(deaths)) %>%
  pull(mean)

# pre_deaths
# [1] 2486.986

post_deaths <- deaths_by_week %>%
  filter(pre_post == "post") %>%
  summarize(mean = mean(deaths)) %>%
  pull(mean)

# post_deaths
# [1] 3267.895


# Filter to 1995-2025
deaths_by_week_filtered <- deaths_by_week %>%
  filter(date >= as.Date("1995-01-01"), date <= as.Date("2025-12-31"))


# Example: deaths_by_week already has columns: date, deaths, pre_post
deaths_by_week_filtered |> 
  ggplot(aes(x = date, y = deaths, color = pre_post)) +
  # Highlight post-COVID period
  geom_rect(aes(xmin = as.Date("2020-03-01"), xmax = as.Date("2025-03-10"),
                ymin = 0, ymax = 5000),
            fill = "#FCF0E9", color = NA, inherit.aes = FALSE) +
  # Mean lines
  geom_hline(yintercept = c(2487, 3268, pre_deaths),
             linewidth = c(0.2, 0.2, 0.4),
             color = c("gray20", "darkred", "black"),
             linetype = c(2, 2, 2)) +
  # Points for each week
  geom_point(size = 1.5, alpha = 0.8) +
  # GAM smoothers for each period
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"),
              se = FALSE, linewidth = 1.2) +
  # Annotations for mean lines
  annotate(geom = "text", hjust = 0, vjust = -0.2,
           size = 2,
           x = as.Date(c("2015-03-01","2007-03-01")),
           y = c(2350, 3300),
           label = c("2,487 deaths", "3,268 deaths")) +
  # annotate(geom = "text",
  #          x = as.Date("2022-09-01"),
  #          y = pre_deaths * 0.98,
  #          label = "2015-19 average") +
  coord_cartesian(expand = FALSE, clip = "off") +
  # scale_y_continuous(breaks = c(2500, 3500),
  #                    limits = c(1500, 5000)) +
  # scale_x_date(breaks = ymd(c("2016-01-01", "2018-01-01", "2020-03-01",
  #                             "2022-01-01", "2024-01-01")),
  #              labels = c("2016", "2018", "March 2020", "2022", "2024")) +
  scale_color_manual(breaks = c("pre", "post"),
                     values = c("pre" = "#009EBC", "post" = "#F05A27"),
                     labels = c("pre" = "Pre-COVID", "post" = "Post-COVID")) +
  scale_y_continuous(limits = c(1500, 5000)) +
  # Use years as x-axis labels
  scale_x_date(
    breaks = as.Date(c("1995-01-01", "2000-01-01", "2005-01-01", "2010-01-01", "2015-01-01", "2020-01-01", "2025-01-01")),
    labels = c("1995", "2000", "2005", "2010", "2015", "2020", "2025"),
    limits = c(as.Date("1995-01-01"), as.Date("2025-12-31"))
  ) +
  labs(x = "Year",
       y = "Deaths per week",
       title = "Weekly Deaths in the Netherlands",
       # subtitle = "Pre and post COVID-19 (March 2020)",
       color = "Period",
       caption = paste("Data source: CBS, the Netherlands\n",
                       "Links to data sources:\n",
                       "Population statistics: https://www.cbs.nl/en-gb\n",
                       "Death rates data: https://opendata.cbs.nl/statline\n",
                       "Plot created by Edward F. Hillenaar, Psychologist and Data Scientist")) +
  theme(
    legend.position = c(0.1,0.9),
    plot.background = element_rect(fill = "#EEEEEE"),
    plot.title = element_textbox_simple(face = "bold", size = 20,
                                        fill = "white", width = NULL,
                                        padding = margin(5, 4, 5, 4),
                                        hjust = 0,
                                        margin = margin(b = 10)),
    # plot.subtitle = element_text(margin = margin(b = 10), size = 15),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.margin = margin(4, 15, 10, 10),
    axis.text.y = element_text(size = 9),
    axis.ticks.y = element_line(linewidth = 0.3),
    axis.text.x = element_text(size = 9, margin = margin(t = 3),
                               color = "black"),
    axis.ticks.length.x = unit(4, "pt"),
    axis.ticks.x = element_line(linewidth = 0.4),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_rect(fill = NA),
    plot.caption = element_text(hjust = 0, size = 8, 
    color = "black", face = "italic", margin = margin(t = 10)))


library(ggstatsplot)
library(dplyr)

# Ensure 'pre' comes before 'post' on the x-axis
deaths_by_week_filtered <- deaths_by_week_filtered %>%
  mutate(pre_post = factor(pre_post, levels = c("pre", "post")))
colnames(deaths_by_week_filtered)

library(ggstatsplot)
ggbetweenstats(
  data = deaths_by_week_filtered,
  x = pre_post,
  y = deaths,
  title = "Pre vs. Post-COVID Deaths per week Comparison",
  xlab = "Period",
  ylab = "Deaths",
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

interpret_hedges_g(-1.70)
# interpret_hedges_g(-1.70)
# [1] "large"
# (Rules: cohen1988)

t.test(deaths ~ pre_post, data = deaths_by_week_filtered) |> 
  report()
# Effect sizes were labelled following Cohen's (1988)
# recommendations.
# 
# The Welch Two Sample t-test testing the difference of
# deaths by pre_post (mean in group pre = 2675.97, mean in
# group post = 3267.90) suggests that the effect is negative,
# statistically significant, and large (difference = -591.93,
# 95% CI [-643.08, -540.78], t(325.18) = -22.77, p < .001;
# Cohen's d = -2.53, 95% CI [-2.82, -2.23])


