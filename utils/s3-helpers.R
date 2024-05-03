# Returns a data frame with contents of the bucket for a specified file
# extension. Hopefully helpful to generate a vector of filenames for multiple
# download
bucket_manifest <- function(file_ext = "png") {

  manifest <- aws.s3::get_bucket_df(ukfsr::s3_bucket())
  
  files <- manifest |> 
      dplyr::select(Key) |>
      dplyr::filter(stringr::str_ends(Key, file_ext)) |> 
      dplyr::mutate(path = dirname(Key),
             file = basename(Key))

  return(files)  
}

# Heres an example of some code to use the function to download multiple files
# into  folder and zip it up.

# files <- bucket_manifest(file_ext = "png")
# 
# x <- files$Key
# y <- files$file
# 
# purrr::map2(x, y, \(x, y) {
#   aws.s3::save_object(object = x,
#                       bucket = ukfsr::s3_bucket(),
#                       file = paste0("myfolderpath/",y),
#                       headers = list("x-amz-acl" = "bucket-owner-full-control"))
# }
# )
# 
# 
# utils::zip(zipfile = "myfiles.zip", files = list.files("myfolderpath/", full.names = TRUE))



# collect FSI graphics ---------------------------------------------------------
svgs <- bucket_manifest(file_ext = "svg")
pngs <- bucket_manifest(file_ext = "png")

charts <- dplyr::bind_rows(svgs, pngs) |> 
  filter(stringr::str_starts(path, "theme_fsi"))

x <- charts$Key
y <- charts$file

purrr::map2(x, y, \(x, y) {
  aws.s3::save_object(object = x, 
                      bucket = ukfsr::s3_bucket(), 
                      file = paste0("~/fsi/",y), 
                      headers = list("x-amz-acl" = "bucket-owner-full-control"))
}
)


zip(zipfile = "~/fsi.zip", files = list.files("~/fsi/", full.names = TRUE))
