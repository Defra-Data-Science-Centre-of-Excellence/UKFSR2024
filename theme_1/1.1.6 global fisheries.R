### Data
library(dplyr)
library(tidyr)
library(aws.ec2metadata)
library(aws.s3)
library(stringi)
library(stringr)
library(readr)
library(ukfsr)
library(afcolours)
library(here)


#FAOSTAT

source(here("utils", "load-font.R"))

# Fisheries and aquaculture production------------------------------------------

fish_species <- aws.s3::s3read_using(FUN = read_csv,
                                     bucket = ukfsr::s3_bucket(),
                                     object = "theme_1/input_data/t1_1_6/CL_FI_SPECIES_GROUPS.csv")%>%
  filter(Yearbook_Group_En=="Fish, crustaceans and molluscs, etc.")

aquaculture_quantity <- aws.s3::s3read_using(FUN = read_csv,
                                    bucket = ukfsr::s3_bucket(),
                                    object = "theme_1/input_data/t1_1_6/Aquaculture_Quantity.csv")


aquaculture<-fish_species%>%
  left_join(aquaculture_quantity,by=c("3A_Code"="SPECIES.ALPHA_3_CODE"))%>%
  mutate(TYPE=if_else(ENVIRONMENT.ALPHA_2_CODE=="IN","Aquaculture-freshwater",if_else(ENVIRONMENT.ALPHA_2_CODE=="BW","Aquaculture-brackishwater","Aquaculture-marine areas")))%>%
  select(PERIOD,TYPE,VALUE)



capture_quantity <- aws.s3::s3read_using(FUN = read_csv,
                                  bucket = ukfsr::s3_bucket(),
                                  object = "theme_1/input_data/t1_1_6/Capture_Quantity.csv")

capture<-fish_species%>%
  left_join(capture_quantity,by=c("3A_Code"="SPECIES.ALPHA_3_CODE"))%>%
  mutate(ENVIRONMENT.ALPHA_2_CODE=if_else(as.numeric(AREA.CODE)<9,"IN","MA"))%>%
  mutate(ENVIRONMENT.ALPHA_2_CODE=if_else(ENVIRONMENT.ALPHA_2_CODE=="BW","IN",ENVIRONMENT.ALPHA_2_CODE))%>%
  mutate(TYPE=if_else(ENVIRONMENT.ALPHA_2_CODE=="IN","Capture-inland waters","Capture-marine areas"))%>%
  select(PERIOD,TYPE,VALUE)

fisheries<-rbind(aquaculture,capture)%>%
  group_by(PERIOD,TYPE)%>%
  summarise(VALUE=sum(VALUE,na.rm=TRUE)/1E6)%>%
  rename(period="PERIOD")%>%
  rename(type="TYPE")%>%
  rename(value="VALUE")


fisheries_chart <- fisheries|>
  ggplot() +
  geom_area(aes(x = period, y = value, fill = type), lwd = 1) +
  scale_x_continuous(limits = c(1950,2022),breaks =seq(1950,2022,10)) +
  scale_y_continuous(limits = c(0,200), expand = expansion(mult = c(0,0.05))) +
  scale_fill_manual(values = af_colours("categorical"))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  guides(fill=guide_legend(nrow=3,byrow=TRUE))+
  labs(x = NULL,
       y = "Million tonnes")

save_graphic(fisheries_chart, "1.1.6a", "global fish production")
save_csv(fisheries, "1.1.6a", "global fish production")


# Sustainable fisheries --------------------------------------------------------

sustainable_fisheries <- aws.s3::s3read_using(FUN = read_csv,
                                         bucket = ukfsr::s3_bucket(),
                                         object = "theme_1/input_data/t1_1_6/sustainable_fisheries.csv")

global_sustainable_fisheries<-sustainable_fisheries%>%
  filter(Area=="World")%>%
  rename(area=Area)%>%
  rename(value=Value)%>%
  rename(year=Year)%>%
  select(year,area,value)

global_sustainable_fisheries_chart <- global_sustainable_fisheries|>
  ggplot() +
  geom_line(aes(x = year, y = value/100,), colour = af_colours(n=1)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1, by = 0.1),labels = scales::percent, expand = expansion(mult = c(0, 0.05))) +
  # scale_color_manual(values = af_colours("categorical", n = 1))+
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "")

save_graphic(global_sustainable_fisheries_chart, "1.1.6b", "global sustainable fisheries production")
save_csv(global_sustainable_fisheries, "1.1.6b", "global sustainable fisheries production")
