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
