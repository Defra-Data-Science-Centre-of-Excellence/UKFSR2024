library(rapid.spreadsheets)
library(openxlsx)
library(ukfsr)
library(dplyr)
library(readODS)
library(stringr)
library(readr)
library(janitor)
library(here)

#Theme 1 dataset

wb <- openxlsx::createWorkbook()


cover <- c("UK Food Security Report 2024",
           "Theme 1",
           "Description",
           "This dataset contains the underlying data for the indicators in Theme 1",
           "Last update",
           "11 December 2024",
           "Next update",
           "December 2027",
           "Contact details",
           "foodsecurityreport@defra.gov.uk",
           "Shorthand",
           "[x] indicates values that are missing due to the information not being applicable or the data being unavailable",
           "Source",
           "Department for Environment, Food and Rural Affairs",
           "Copyright",
           "© Crown copyright 2024")

create_cover_sheet(wb, text_df = as_tibble(cover), ,tab_name = "Cover_Sheet",subheadings = c(2,3,5,7,9,11,13,15))


# appending row number because I was getting duplicate sheet names - may not be
# needed with final data. Plus there is a 31 char limit for sheet names.
csvs <- bucket_manifest(file_ext = "csv") |> 
  dplyr::filter(stringr::str_starts(folder, "theme_1") & stringr::str_ends(folder, "output/csv")) |> 
  #mutate(sheet_name = str_replace_all(paste0(indicator_id,"_", str_sub(title, 1, 19)), " ", "_"))
  mutate(sheet_name = indicator_id) #|>
#mutate(title = toupper(title)	)

path <- csvs$path
title <- csvs$title
sheet_names <- csvs$sheet_name

data <- purrr::map(path, \(path) {
  x <- aws.s3::s3read_using(FUN = read_csv,
                            bucket = ukfsr::s3_bucket(),
                            object = path) %>% 
    clean_names()
})

names(data) <- sheet_names

contents <- tibble(sheet_names,title)%>% 
  dplyr::rename("Sheet names"=sheet_names,"Title"=title)

create_contents_notes(wb, contents,column_width = c(20, 60)) # set column width - first column wide cos of long names then using rep to duplicate for remaining columns)




#contents<-contents %>% 
#rename(sheet_names=`Sheet Names`,title=`Title`)


# Notes page --------------------------------------------------------------

##
# now we create our notes dataframe. The structure is similar to contents page.
# e.g. one column for note number, another for the actual note.

# create notes data frame - needs updating wit new notes
notes_df <- data.frame(
  "Note number" = c("Note 1", "Note 2", "Note 3"),
  "Note text" = c("Figures that are Tables in the report provide a full view of the data used and so do not have an accompanying datasheet.",
                  "Instances where charts do not have disclosed datasheets due to the terms agreed on datasharing.",
                  "Some datasheets provide data for multiple Figures on the UKFSR")
) %>%   
  clean_names(case = "sentence") 



# Now we add the notes page using the same function as the contents page. Note we have to change the heading
# to "Notes" this time. We set contents_links to false to stop it trying to link the first column with 
# matching tab names (as we don't have a tab called Note 1 etc)


# add notes
create_contents_notes(
  wb, # workbook to update 
  df = notes_df, # notes df we made
  tab_name = "Notes", # tab name
  heading = "Notes", # heading
  column_width = c(30, 80), # set column width - first column wide cos of long names then using rep to duplicate for remaining columns
  contents_links = FALSE # need this so as not to link to tab names. 
)



purrr::pmap(list(data, sheet_names, title), \(data, sheet_names, title) {
  create_data_table_tab(wb, data,tab_name = sheet_names, heading = title,column_width = c(30, 30)) #heading already specified? Will get year to be leading category so 20 would do then can I add as many 30s?
  
})

### remove borders 
##
# rown is the rows to apply to. For some reason this has to be set to start at "0" to remove the
# borders on the column headers. But weirdly need "+3" on the end to cover whole table.
# coln is the columns to apply to. 

# remove borders and right align year headers 
format_borders(
  wb,
  sheet = "Contents",
  rown = c(0:nrow(contents)+3),
  coln = c(1:ncol(contents))
)

format_borders(
  wb,
  sheet = "Notes",
  rown = c(0:nrow(contents)+3),
  coln = c(1:ncol(contents))
) 





saveWorkbook(wb, 
             here("datasets","fsr_theme1.xlsx"),
             overwrite = TRUE)


#export and save as ODS
