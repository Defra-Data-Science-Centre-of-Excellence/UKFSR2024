# Some example code to bulk download a set of files from the bucket

# collect FSI graphics ---------------------------------------------------------
svgs <- ukfsr::bucket_manifest(file_ext = "svg")
pngs <- ukfsr::bucket_manifest(file_ext = "png")

charts <- dplyr::bind_rows(svgs, pngs) |> 
  dplyr::filter(stringr::str_starts(folder, "theme_fsi"))

x <- charts$path
y <- charts$file

purrr::map2(x, y, \(x, y) {
  aws.s3::save_object(object = x, 
                      bucket = ukfsr::s3_bucket(), 
                      file = paste0("~/fsi/",y), 
                      headers = list("x-amz-acl" = "bucket-owner-full-control"))
}
)


zip(zipfile = "~/fsi.zip", files = list.files("~/fsi/", full.names = TRUE))

# test code to make an ODS file -----------------------------------------------
library(ukfsr)
library(readODS)
library(readr)

csvs <- bucket_manifest(file_ext = "csv") |> 
  dplyr::filter(stringr::str_starts(folder, "theme_3") & stringr::str_ends(folder, "output/csv"))

path <- csvs$path
title <- csvs$title

purrr::map2(path, title, \(path, title) {
  x <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = path)
  write_ods(x, path = "~/work/test.ods", sheet = title, append = TRUE)
}
)
