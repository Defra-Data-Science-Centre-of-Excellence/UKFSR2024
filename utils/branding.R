library(dplyr)
library(ggplot2)
library(tidyr)

psp <- ukfsr::prodsupply |> pivot_wider(names_from = type)


psp |>
  mutate(gap = indigenous_food - all_food,
         top = 140 - indigenous_food) |>
  select(year, all_food, gap, top) |>
  pivot_longer(cols = all_food:top, names_to = "type") |>
  mutate(type = factor(type,
                       levels = c("all_food", "gap", "top"),
                       labels = c("All food", "gap", "top"))) |>
  ggplot() +
  geom_area(aes(x = year, y = value, fill = rev(type))) +
  annotate("text", x = 1990, y = 125, label = "\nFood Security", size= 18, colour= "#12436D", family = "Arial Bold" ) +
  annotate("text", x = 1990, y = 35, label = "is\nNational Security", size= 18, colour= "#F46A25", family = "Arial Bold") +
  # annotate("text", x = 1990, y = 135, label = "UKFSR2024", size= 16, colour= "#F46A25", family = "GDS Transport Website" ) +
  scale_fill_manual(values = rev(c("#12436D", "grey90", "#F46A25"))) +
  theme_void() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"), plot.margin = margin(0,0,0,0), plot.background = element_blank())
