library(tidyverse)
library(tibble)
library(showtext)
library(ggtext)
library(lubridate)
library(janitor)

font_add_google("Libre Franklin", "franklin")
showtext_opts(dpi = 300)
showtext_auto()

# Date sequence: Jan 2021 to Mar 2023
dates <- seq(ymd("2021-01-01"), ymd("2023-03-01"), by = "1 month")


percent_vaccinated <- c(1, 2, 3, 6, 10, 18, 30, 45, 55, 65, 70, 75,
                        77, 79, 80, 81, 82, 83, 84, 84, 85, 85, 85, 85,
                        85, 85, 85)


# Approximated values for each group based on visual inspection of Panel B
conceptions_all <- c(
  6.3, 6.2, 6.1, 6.0, 5.7, 5.9, 5.8, 6.3, 6.4, 6.4, 6.2, 6.1,  # 2021
  5.9, 5.7, 5.7, 5.7, 4.8, 5.7, 5.0, 5.8, 5.7, 5.9, 5.8, 5.6,  # 2022
  4.8, 4.6, 4.4                                                # 2023
)

conceptions_vaccinated <- c(
  0.5, 2.8, 2.4, 2.0, 1.8, 0.8, 1.2, 2.8, 3.8, 4.2, 4.0, 4.1,  # 2021
  4.1, 4.2, 4.2, 4.2, 4.0, 4.4, 4.1, 4.4, 4.4, 4.7, 4.6, 4.2,  # 2022
  4.2, 4.1, 4.0                                                # 2023
)

conceptions_unvaccinated <- c(
  6.4, 6.3, 6.2, 6.2, 6.0, 9.0, 9.9, 10.2, 10.1, 9.9, 11.0, 10.8, # 2021
  8.6, 7.0, 6.8, 7.0, 6.0, 6.8, 6.4, 6.5, 6.4, 7.1, 7.0, 6.2,     # 2022
  6.0, 6.0, 5.9                                                   # 2023
)

# Create the tibble
data <- tibble(
  Date = dates,
  Percent_Vaccinated = percent_vaccinated,
  Conceptions_All = conceptions_all,
  Conceptions_Vaccinated = conceptions_vaccinated,
  Conceptions_Unvaccinated = conceptions_unvaccinated
)

# View the tibble
print(data)
setwd("D:/Onderzoek/Article/Sterfte COVID")
# Write to CSV
write_csv(data, "vaccination_conception_data.csv")

glimpse(data)
data_long <- pivot_longer(data, 3:5, names_to = "conceptions", 
                          values_to = "succes")

data_long <- data_long |> 
  mutate(conceptions = factor(conceptions,
                              levels = c("Conceptions_All", 
                                         "Conceptions_Vaccinated", 
                                         "Conceptions_Unvaccinated")))

glimpse(data_long)
data_long <- clean_names(data_long)

library(tidyverse)
library(janitor)
library(scales)

# Ensure data is properly cleaned and in long format
data_long <- data_long |> 
  mutate(
    year = factor(lubridate::year(date)),
    month = lubridate::month(date)
  )


# Load necessary libraries
library(tidyverse)
library(janitor)
library(scales)
library(lubridate)
library(showtext)
library(ggtext)

# Font settings
font_add_google("Libre Franklin", "franklin")
showtext_opts(dpi = 300)
showtext_auto()

font_add_google("Oswald", "sans-serif")
showtext_opts(dpi = 300)
showtext_auto()

font_add_google("Ubuntu", "sans-serif")
showtext_opts(dpi = 300)
showtext_auto()

font_add_google("News Cycle", "sans-serif")
showtext_opts(dpi = 300)
showtext_auto()


str(data_long)



# 
# # Plot on full date axis to maintain consistent spacing
# p <- ggplot(data_long, aes(x = date, y = succes, group = conceptions, 
#                            color = conceptions)) + 
#   geom_line(linewidth = 1.2) +
#   scale_x_date(
#     name = NULL,
#     date_breaks = "1 month",
#     date_labels = "%m\n%Y",  # Month on top, year underneath
#     expand = expansion(mult = c(0.01, 0.01))
#   ) +
#   scale_y_continuous(
#     name = "Number of successful conceptions\nper 1,000 women",
#     limits = c(0, 11),
#     breaks = seq(0, 11, 2)
#   ) +
#   # scale_color_manual(
#   #   values = c(
#   #     "conceptions_all" = "darkred",
#   #     "conceptions_vaccinated" = "black",
#   #     "conceptions_unvaccinated" = "dodgerblue"
#   #   ),
#   #   labels = c("All women", "Vaccinated women", "Unvaccinated women"),
#   #   name = NULL
#   # ) +
#   labs(
#     title = "Successful Conceptions per 1,000 Women (Aged 18–39) by COVID-19 Vaccination Status"
#   ) +
#   theme_minimal(base_family = "franklin", base_size = 12) +
#   theme(
#     plot.background = element_rect(fill = "white", color = NA),
#     panel.background = element_rect(fill = "white", color = NA),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor = element_blank(),
#     legend.position = "bottom",
#     legend.text = element_text(size = 10),
#     plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
#     axis.text.x = element_text(size = 9, angle = 0, vjust = 0.5),
#     axis.text.y = element_text(size = 10),
#     axis.title.y = element_text(size = 12)
#   )
# 
# p <- ggplot(data_long, aes(x = date, y = succes, group = conceptions, 
#                            color = conceptions)) + 
#   geom_line(linewidth = 1.2) +
#   scale_x_date(
#     name = NULL,
#     date_breaks = "1 month",
#     date_labels = "%m\n%Y",
#     expand = expansion(mult = c(0.01, 0.01))
#   ) +
#   scale_y_continuous(
#     name = "Number of successful conceptions\nper 1,000 women",
#     limits = c(0, 11),
#     breaks = seq(0, 11, 2)
#   ) +
#   scale_color_manual(
#     values = c(
#       "conceptions_all" = "darkred",
#       "conceptions_vaccinated" = "black",
#       "conceptions_unvaccinated" = "dodgerblue"
#     ),
#     labels = c(
#       "conceptions_all" = "All women",
#       "conceptions_vaccinated" = "Vaccinated women",
#       "conceptions_unvaccinated" = "Unvaccinated women"
#     ),
#     name = NULL
#   ) +
#   labs(
#     title = "Successful Conceptions per 1,000 Women (Aged 18–39) by COVID-19 Vaccination Status"
#   ) +
#   theme_minimal(base_family = "franklin", base_size = 12) +
#   theme(
#     plot.background = element_rect(fill = "white", color = NA),
#     panel.background = element_rect(fill = "white", color = NA),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor = element_blank(),
#     legend.position = "bottom",
#     legend.text = element_text(size = 10),
#     plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
#     axis.text.x = element_text(size = 9, angle = 0, vjust = 0.5),
#     axis.text.y = element_text(size = 10),
#     axis.title.y = element_text(size = 12)
#   )
# 
# levels(data_long$conceptions)


library(lubridate)

# Define midpoints of each year for label positioning
mid_2021 <- as.Date("2021-07-01")
mid_2022 <- as.Date("2022-07-01")
mid_2023 <- as.Date("2023-02-01")

# Build the plot and include everything at once
p <- ggplot(data_long, aes(x = date, y = succes, group = conceptions, 
                           color = conceptions)) + 
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = as.numeric(as.Date(c("2021-01-01", "2022-01-01", "2023-01-01"))),
             linetype = "dashed", color = "grey30") +
  scale_x_date(
    name = NULL,
    breaks = seq.Date(from = as.Date("2021-01-01"), to = as.Date("2023-03-01"), by = "1 month"),
    labels = function(x) format(x, "%m"),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    name = "Number of successful conceptions\nper 1,000 women",
    breaks = seq(0, 11, 2)   # Remove limits=!
  ) +
  scale_color_manual(
    values = c(
      "Conceptions_All" = "darkred",
      "Conceptions_Vaccinated" = "black",
      "Conceptions_Unvaccinated" = "dodgerblue"
    ),
    labels = c(
      "Conceptions_All" = "All women",
      "Conceptions_Vaccinated" = "Vaccinated women",
      "Conceptions_Unvaccinated" = "Unvaccinated women"
    ),
    name = NULL
  ) +
  labs(
    title = "Successful Conceptions per 1,000 Women (Aged 18–39) by COVID-19 Vaccination Status"
  ) +
  annotate("text", x = mid_2021, y = -2.0, label = "2021", size = 4, fontface = "bold") +
  annotate("text", x = mid_2022, y = -2.0, label = "2022", size = 4, fontface = "bold") +
  annotate("text", x = mid_2023, y = -2.0, label = "2023", size = 4, fontface = "bold") +
  coord_cartesian(ylim = c(0, 11), clip = "off") +
  theme_minimal(base_family = "News Cycle", base_size = 12) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12),
    plot.margin = margin(20, 10, 60, 10)  # Extra space at bottom
  )


# Print the plot
print(p)


setwd("D:/Onderzoek/Article/Sterfte COVID")
# Optional: Save plot
ggsave("lineplot_conceptions_by_vaccination_status.png", 
       p, width = 12, height = 5, dpi = 300, bg = "white")

