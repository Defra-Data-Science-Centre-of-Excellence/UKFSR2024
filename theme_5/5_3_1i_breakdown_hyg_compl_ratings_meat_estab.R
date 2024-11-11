library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)
library(stringr)
library(patchwork)
library(grid)
library(gridtext)
library(gtable)
library(ggtext)
library(data.table)
library(ggalluvial)

source(here("utils", "load-font.R"))

t5_3_1i <- aws.s3::s3read_using(FUN = read_csv,
                                bucket = ukfsr::s3_bucket(),
                                object = "theme_5/t5_3_1/output/csv/5_3_1i_breakdown_hyg_compl_ratings_meat_estab.csv")

t5_3_1i$country <- factor(t5_3_1i$country, levels = c("England and Wales 2023","England and Wales 2022","Northern Ireland 2023","Northern Ireland 2022","Scotland 2023","Scotland 2022"))

t5_3_1i_long <- t5_3_1i %>% 
  group_by(country) %>%
  pivot_longer(cols=c("Good","Generally Satisfactory","Improvement Necessary","Urgent Improvement Necessary"),
               names_to="rating",
               values_to="value")

t5_3_1i_long$rating <- factor(t5_3_1i_long$rating, levels = c("Good","Generally Satisfactory","Improvement Necessary","Urgent Improvement Necessary"))

level_order <- c("Good","Generally Satisfactory","Improvement Necessary","Urgent Improvement Necessary")

af_categorical_colours <- afcolours::af_colours("categorical", n = 4)
names(af_categorical_colours)=levels(t5_3_1i_long$rating)

t5_3_1i_long$country_wrap = str_wrap(t5_3_1i_long$country, width = 10)

# # https://stackoverflow.com/questions/6644997/showing-data-values-on-stacked-bar-chart-in-ggplot2
# # ensure question axis matches original vector
# # https://stackoverflow.com/questions/12774210/how-do-you-specifically-order-ggplot2-x-axis-instead-of-alphabetical-order
# # Turn question column into a character vector
# # Then turn it back into a factor with the levels in the correct order

t5_3_1i_long1 <- t5_3_1i_long

# stacked bar chart with bespoke small number labels
# https://stackoverflow.com/questions/27831384/ggplot2-stacked-bar-chart-labels-with-leader-lines

t5_3_1i_long1 <- t5_3_1i_long

t5_3_1i_long3 <- mutate(t5_3_1i_long1, foo = ifelse(value < 1.5, NA, value))
t5_3_1i_long4 <- mutate(t5_3_1i_long1, foo = ifelse(value < 1.5, value, NA))

t5_3_1i_plot <- ggplot(t5_3_1i_long3,aes(x=country_wrap, y=value, label = round(value,0), group=rating)) +
  geom_bar(stat="identity", aes(fill = rating)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  scale_fill_manual(values = af_categorical_colours) +
  scale_y_continuous(limits = c(0,100.02),breaks = seq(0,100.02, 10))+
  labs(y = "Hygiene compliance rating (%)") +
  theme(axis.text.x = element_text(size=20)) +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=18, face = "italic", margin = margin(r = 1, unit = 'cm'))) + 
  theme(axis.title.x = element_blank()) +
  geom_text(stat = "stratum", aes(stratum = rating, label = ifelse(is.na(foo),"",sprintf("%.1f", foo))), color="white", fontface = "bold", size=6) # +

t5_3_1ii_plot <- ggplot(t5_3_1i_long4,aes(x=country_wrap, y=value, label = round(value,0), group=rating)) +
  geom_bar(stat="identity", aes(fill = rating)) +
  theme_ukfsr(base_family = "GDS Transport Website", base_size = 14) +
  scale_fill_manual(values = af_categorical_colours) +
  scale_y_continuous(limits = c(0,100.02),breaks = seq(0,100.02, 10))+
  labs(y = "Hygiene compliance rating (%)") +
  theme(axis.text.x = element_text(size=20)) +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        legend.text = element_text(size=18, face = "italic", margin = margin(r = 1, unit = 'cm'))) + 
  theme(axis.title.x = element_blank()) +
  geom_text(stat = "stratum", aes(stratum = rating, label = ifelse(is.na(foo),"",sprintf("%.1f", foo))), color="white", fontface = "bold", size=6) # +

# t5_3_1ii_plot

# Label for left hand side of bar
left1 <- filter(ggplot_build(t5_3_1ii_plot)$data[[2]],
                stratum %in% c("Improvement Necessary")) %>%
  mutate(x = 1.35) %>%
  mutate(y = y+5) 

row_to_keep = c(TRUE, TRUE, FALSE, FALSE)
left1 = left1[row_to_keep,]

left1 <- left1 %>%
  mutate(y = ifelse(y > 5.5, y+5, y)) 

left1$x <- ifelse(left1$y == 5.45, left1$x-0.7, left1$x)
left1$x <- ifelse(left1$y == 10.8, left1$x+0.3, left1$x)
left1$y[!is.na(left1$y)] <- 10

# Label for right hand side of bar
right1 <- filter(ggplot_build(t5_3_1ii_plot)$data[[2]],
                 stratum %in% c("Urgent Improvement Necessary")) %>%
  mutate(x = 0.65) %>%
  mutate(y = y+5)

right1 <- right1 %>%
  mutate(y = ifelse(y > 5.1, y+5, y))

right1$x <- ifelse(right1$label == 0.6, right1$x+1, right1$x)
right1$x <- ifelse(right1$y == 5.1, right1$x+0.7, right1$x)
right1$x <- ifelse(right1$y == 10.3, right1$x+0.7, right1$x)

right1$y[!is.na(right1$y)] <- 10

t5_3_1iii_plot <- t5_3_1i_plot +
  annotate("text", x = left1$x, y = left1$y, label = left1$count, colour = "white", fontface = "bold", size = 6) +
  annotate("text", x = right1$x, y = right1$y, label = right1$count, colour = "white", fontface = "bold", size = 6) 

t5_3_1iii_plot

r.seg <- data.frame(x = rep(1.2, times = 2),
                    xend = rep(1.25, times = 2),
                    y = left1$y,
                    yend = left1$y) 

l.seg <- data.frame(x = rep(0.76, times = 2),
                    xend = rep(0.8, times = 2),
                    y = right1$y,
                    yend = right1$y)

# Draw the segments                                        
row_to_keep = c(FALSE,TRUE)
r1.seg <- r.seg[row_to_keep,]

t5_3_1iv_plot <- t5_3_1iii_plot +
  annotate("segment", x = (r.seg$x)+0.1, xend = (r1.seg$xend)+0.1, y = 0, yend = (r1.seg$yend)-2, colour = "#F46A25", linewidth=1) 

t5_3_1v_plot <- t5_3_1iv_plot +
  annotate("segment", x = (l.seg$x-0.05), xend = (l.seg$xend-0.15), y = 0, yend = (l.seg$yend-2), colour = "#801650", linewidth=1) 

t5_3_1vi_plot <- t5_3_1v_plot +
  annotate("segment", x = (r.seg$x)+1.1, xend = (r1.seg$xend)+1.1, y = 0, yend = (r1.seg$yend)-2, colour = "#F46A25", linewidth=1) 

t5_3_1vii_plot <- t5_3_1vi_plot +
  annotate("segment", x = (l.seg$x+0.95), xend = (l.seg$xend+0.85), y = 0.55, yend = (l.seg$yend-2), colour = "#801650", linewidth=1) 

t5_3_1vii_plot 


save_graphic(t5_3_1vii_plot, "5.3.1i", "breakdown hyg compl ratings meat estab")

save_csv(t5_3_1i, "5.3.1i", "breakdown hyg compl ratings meat estab")
