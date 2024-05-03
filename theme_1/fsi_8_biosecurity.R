library(aws.s3)
library(aws.ec2metadata)
library(here)
library(dplyr)
library(ggplot2)
library(readr)
library(ukfsr)
library(afcolours)

source(here::here("utils", "load-font.R"))

colspec <- cols(
  year = col_integer(),
  status = col_character(),
  value = col_double()
)

cases <- aws.s3::s3read_using(FUN = read_csv,
                                    bucket = ukfsr::s3_bucket(),
                                    object = "theme_fsi/tfsi_8_1/output/csv/fsi_8_1_apha_cases.csv", 
                              col_types = colspec)


chart <- cases |>
  filter(year<2024) |> 
  mutate(status = factor(status, levels = c("not_confirmed", "confirmed"), labels = c("Not confirmed", "Confirmed"), ordered = TRUE)) |> 
  ggplot() +
  geom_col(aes(x = year, y = value, fill = status)) +
  scale_fill_manual(values = af_colours(type = "duo")) +
  labs(x = NULL, y = NULL) +
  theme_ukfsr()


for(i in c(14, 16,22)) {
  
  cht <- chart + theme_ukfsr(base_family = "GDS Transport Website",
                             base_size = i,
                             chart_line_size = 2) +
    theme(plot.margin = margin(5,50,5,5,unit = "pt"))
  
  save_graphic(cht, "fsi.8.1", paste("biosecurity fsi base", i))
  
}

