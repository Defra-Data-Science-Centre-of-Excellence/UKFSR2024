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
