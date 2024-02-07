library(showtext)
library(ukfsr)
library(ggplot2)

regfont <- tempfile()
boldfont <- tempfile()

aws.s3::save_object(object = "assets/font/GDSTransportWebsite.ttf", 
                    bucket = s3_bucket(), 
                    file = regfont, 
                    headers = list("x-amz-acl" = "bucket-owner-full-control"))


aws.s3::save_object(object = "assets/font/GDSTransportWebsite-Bold.ttf", 
                    bucket = s3_bucket(), 
                    file = boldfont, 
                    headers = list("x-amz-acl" = "bucket-owner-full-control"))

font_add(family = "GDS Transport Website",
         regular = regfont,
         bold = boldfont)

showtext_auto()
