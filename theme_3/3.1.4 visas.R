library('ukfsr')
library('afcolours')
library('ggplot2')
library('dplyr')
library('tidyr')
library('aws.s3')
library('lubridate')

af_colours_1 <- c(
  "#12436D" # Dark blue
)

source(here::here("utils", "load-font.R"))

FSR_visas <- aws.s3::s3read_using(FUN = readr::read_csv,
                                  bucket = "s3-ranch-054",
                                  object = "theme_3/t3_1_3/output/csv/3_1_3a_seasonal_worker_visas.csv")
#to add commas to numbers
marks_sci <- function(`Seasonal worker visas issued`) format(`Seasonal worker visas issued`, big.mark = ",", scientific = FALSE)

chart <- 
  ggplot(data=FSR_visas) +
  geom_col(aes(x = Year, y = `Seasonal worker visas issued`), fill = af_colours_1) +
  scale_y_continuous(labels = marks_sci)+
  theme_ukfsr(base_family = "GDS Transport Website")

print(chart)

save_graphic(chart, "3.1.3a", "Seasonal worker visas chart")
