library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)
library(forcats)
library(readODS)

source(here::here("utils", "load-font.R"))

tmp <- tempfile()
auk_9 <- "https://assets.publishing.service.gov.uk/media/665999c416cf36f4d63ebcb7/AUK-chapter9-06jun24.ods"

download.file(auk_9, destfile = tmp)

rawdata <- read_ods(path = tmp, sheet = "Table_9_1", skip = 2, col_types = NA) |> 
  select(-Notes) |> 
  pivot_longer(cols = !starts_with("Type"), names_to = "year") |> 
  mutate(value = as.numeric(case_when(value == "[x]" ~ NA, .default = value)),
         year = as.numeric(year))


chtdata <- rawdata |> 
  filter(Type %in% c("Total compounds plus imports less exports",
                     "Straight concentrates",
                     "Non-concentrates",
                     "Feed produced and used on farm or purchased from other farms"
  ), year >= 1990) |> 
  mutate(Type = factor(Type, levels = c("Total compounds plus imports less exports",
                                     "Straight concentrates",
                                     "Non-concentrates",
                                     "Feed produced and used on farm or purchased from other farms"
  ), labels = c("Total compounds plus\n imports less exports",
                "Straight concentrates",
                "Non-concentrates",
                "Inter/intra farm transfer"
  )))

cht <- chtdata|> 
  ggplot() +
  geom_line(aes(x = year, y= value, colour = Type)) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_colour_manual(values = af_colours("categorical")) + 
  guides(col = guide_legend(nrow=2)) +
  labs(y = "thousand tonnes", x = NULL) +
  theme_ukfsr(base_family = "GDS Transport Website") 

save_csv(chtdata, "3.1.1d", "animal feed use auk ch9")
save_graphic(cht, "3.1.1d", "animal feed use auk ch9")
