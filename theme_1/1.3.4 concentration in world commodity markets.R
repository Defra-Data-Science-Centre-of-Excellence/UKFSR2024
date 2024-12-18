### Data
library(dplyr)
library(tidyr)
library(ggplot2)
library(aws.ec2metadata)
library(aws.s3)
library(readr)
library(ukfsr)
library(afcolours)
library(here)
library(readxl)
library(scales)
library(zoo)

source(here("utils", "load-font.R"))

# HHI export concentration -----------------------------------------------------
export_shares <- aws.s3::s3read_using(FUN = read_csv,
                                        bucket = ukfsr::s3_bucket(),
                                        object = "theme_1/input_data/t1_3_4/hi141120241526.csv")%>%
  pivot_longer(cols=4:23,names_to="years",values_to = "value")%>%
  mutate(Commodity=if_else(Commodity=="Corn","Maize",Commodity))
  
export_shares_world <-export_shares%>%
  filter(Country=="World")

export_shares_not_world <-export_shares%>%
  filter(!Country=="World")

export_shares_global<-export_shares_not_world%>%
  left_join(export_shares_world,by=c("Commodity"="Commodity","Attribute"="Attribute","Unit Description"="Unit Description","years"="years"))%>%
  mutate(HI=(value.x/value.y)^2)%>%
  group_by(Commodity,years)%>%
  summarise(HI=sum(HI,na.rm=TRUE))%>%
  mutate(year=as.numeric(substr(years,1,4)))%>%
  arrange(desc(year))%>%
  rename(commodity=Commodity)%>%
  rename(hi=HI)

export_shares_global_a<-export_shares_not_world%>%
  left_join(export_shares_world,by=c("Commodity"="Commodity","Attribute"="Attribute","Unit Description"="Unit Description","years"="years"))%>%
  mutate(HI=(value.x/value.y)^2)%>%
  mutate(year=as.numeric(substr(years,1,4)))%>%
  arrange(desc(year))


herfindhal_indices_chart<-ggplot()+
  geom_line(data=export_shares_global,aes(x = year, y = signif(hi,digits=3),group=commodity, colour = commodity)) +
  geom_point(data=export_shares_global,aes(x = year, y = signif(hi,digits=3),group=commodity, colour = commodity, shape = commodity),size=3) +
  scale_y_continuous(limits = c(0,0.5),breaks=c(0, 0.1,0.2,0.3,0.4,0.5),labels=c(0, 0.1,0.2,0.3,0.4,0.5), expand = expansion(mult = c(0, 0.05))) +
  scale_x_continuous(limits = c(2004,2024),breaks=c(seq(2004,2020,4),2023),labels=c("04/05","08/09","12/14","16/17","20/21","23/24"))+
  scale_colour_manual(values = af_colours("categorical",n=6)) +
  theme_ukfsr(base_family = "GDS Transport Website") +
  labs(x = NULL,
       y = "HHI(export share squared)",colour="",linetype="",shape="",size="")

export_shares_global<-export_shares_global%>%
  select(-year)%>%
  rename(year=years)

save_graphic(herfindhal_indices_chart, "1.3.4a", "hhi export concentration")
save_csv(export_shares_global, "1.3.4a", "hhi export concentration")




