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

data <- purrr::map(path, \(path) {
  x <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = path)
  write_ods(x, path = "~/work/test.ods", sheet = title, append = TRUE)
}
)

# Experimenting with 'rapid.spreadsheets' and xlsx ----------------------------- 
library(rapid.spreadsheets)
library(openxlsx)
library(ukfsr)
library(dplyr)
library(readODS)
library(stringr)
library(readr)

csvs <- bucket_manifest(file_ext = "csv") |> 
  dplyr::filter(stringr::str_starts(folder, "theme_1") & stringr::str_ends(folder, "output/csv")) |> 
  mutate(sheet_name = str_replace_all(paste0(row_number(), "_", indicator_id,"_", str_sub(title, 1, 19)), " ", "_"))

path <- csvs$path
title <- csvs$title
sheet_names <- csvs$sheet_name

data <- purrr::map(path, \(path) {
  x <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = path)
})

names(data) <- sheet_names

wb <- openxlsx::createWorkbook()
purrr::pmap(list(data, sheet_names, title), \(data, sheet_names, title) {
  create_data_table_tab(wb, data,tab_name = sheet_names, heading = title)
  
})

contents <- tibble(sheet_names, title)
create_contents_notes(wb, contents)


cover <- c("UK Food Security Report 2024",
           "Theme 1",
           "Description",
           "This dataset contains the underlying data for the indicators in Theme 1",
           "Last update",
           "[date]",
           "Contact details",
           "foodsecurityreport@defra.gov.uk",
           "Copyright",
           "© Crown copyright 2024"
           )

create_cover_sheet(wb, text_df = as_tibble(cover), subheadings = c(2,3,5,7,9))

saveWorkbook(wb, "~/test.xlsx", overwrite = TRUE)


