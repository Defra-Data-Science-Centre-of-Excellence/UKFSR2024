#' Theme for CPI brief charts
#'
#' @return
#' @export
#'
#' @examples
theme_mm23 <- function() {
  font <- "Arial"
  
  ggplot2::theme(
    
    #Text format:
    #This sets the font, size, type and colour of text for the chart's title
    plot.title = ggplot2::element_text(family=font,
                                       size=12,
                                       face="bold",
                                       color="#222222"),
    #This sets the font, size, type and colour of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
    plot.subtitle = ggplot2::element_text(family=font,
                                          size=10),
    #   margin=ggplot2::margin(9,0,9,0)),
    # size=10,
    # margin=ggplot2::margin(9,0,9,0)),
    plot.caption = ggplot2::element_blank(),
    #This leaves the caption text element empty, because it is set elsewhere in the finalise plot function
    
    #Legend format
    #This sets the position and alignment of the legend, removes a title and backround for it and sets the requirements for any text within the legend. The legend may often need some more manual tweaking when it comes to its exact position based on the plot coordinates.
    legend.position = "right",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family=font,
                                        size=10,
                                        color="#222222"),
    
    #Axis format
    #This sets the text font, size and colour for the axis test, as well as setting the margins and removes lines and ticks. In some cases, axis lines and axis ticks are things we would want to have in the chart - the cookbook shows examples of how to do so.
    axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(family=font,
                                      size=10,
                                      color="#222222"),
    axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    
    #Grid lines
    #This removes all minor gridlines and adds major y gridlines. In many cases you will want to change this to remove y gridlines and add x gridlines. The cookbook shows you examples for doing so
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
    panel.grid.major.x = ggplot2::element_blank(),
    
    #Blank background
    #This sets the panel background as blank, removing the standard grey ggplot background colour from the plot
    panel.background = ggplot2::element_blank(),
    
    #Strip background (#This sets the panel background for facet-wrapped plots to white, removing the standard grey ggplot background colour and sets the title size of the facet-wrap title to font size 22)
    strip.background = ggplot2::element_rect(fill="white"),
    strip.text = ggplot2::element_text(size  = 12,  hjust = 0)
  )
}






#' Get the value for a series and date
#'
#' @param df a dataframe of cpi values created by mm23::get_mm23_*
#' @param cdidcode a CDID
#' @param series_date a date
#'
#' @return series value
#' @export
#'
#' @examples
get_val <- function(df, cdidcode, series_date){
  
  val <- df |>  
    dplyr::filter(cdid == cdidcode & date == series_date) |>  
    dplyr::pull(value)
  
  val
  
}


#' Text that describes the change in two numbers
#'
#' @param now the current value
#' @param previous the previous value
#'
#' @return "up", "down" or "unchanged"
#' @export
#'
#' @examples
eval_change <- function(now, previous) {
  text <-  ""
  
  if (now == previous){
    text = "unchanged"
  }
  
  if(now < previous){
    text =  "down"
  }
  
  if(now > previous){
    text =  "up"
  }
  return(text)
}





#' Standard line chart for CPI brief
#'
#' @param df a dataframe of cpi values created by mm23::get_mm23_*
#' @param cdids series CDIDs to plot
#' @param start_date start date of x axis
#' @param labels optional labels for chart legend
#'
#' @return
#' @export
#'
#' @examples
line_chart <- function(df, cdids, start_date = "2014-03-01", labels){
  
palette <- c("#414487FF", # DEFAULT BLUE
             "#FDE725FF") # YELLOW

  if(missing(labels))(labels = cdids)
  names(df)[names(df) == 'value'] <- 'CPI value'
  
  data <- df  |> 
    filter(cdid %in% cdids & `CPI value` != is.na(`CPI value`) & date >= start_date) |> 
    ggplot() +
    geom_line(aes(x = date, y = `CPI value`, colour = cdid), size = 1) +
    scale_colour_manual(values=palette,breaks = cdids, labels = labels) +
    scale_y_continuous(labels = label_percent(scale = 1,accuracy = 0.1),breaks = breaks_extended(10)) +
    scale_x_date(date_labels = "%b %Y") +
    theme_mm23() 
  
  return(data)
}








#' Standard bar chart for CPI brief
#'
#' @param df a dataframe of cpi values created by mm23::get_mm23_*
#' @param cdids series CDIDs to plot
#' @param start_date start date of x axis
#' @param labels optional labels for chart legend
#' 
#' @return
#' @export
#'
#' @examples
bar_chart <- function(df, cdids, start_date = "2020-01-01", labels){
  
  palette <- c("#414487FF", # DEFAULT BLUE
               "#FDE725FF") # YELLOW
  
  
  if(missing(labels))(labels = cdids)
  names(df)[names(df) == 'value'] <- 'CPI value'
  
  data <- df |> 
    filter(cdid %in% cdids & `CPI value` != is.na(`CPI value`) & date >= start_date) |> 
    ggplot() +
    geom_bar(aes(x = date, y = `CPI value`, fill = cdid),
             stat = "identity",
             position = position_dodge(preserve = "single")) +
    scale_fill_manual(values=palette,breaks = cdids, labels = labels) +
    scale_y_continuous(labels = label_percent(scale = 1,accuracy = 0.1),breaks = breaks_extended(5)) +
    scale_x_date(date_labels = "%b %Y") +
    theme_mm23()
  
  return(data)
}



#' Get a list of metadata for a CPI series
#'
#' @param df a metadata dataframe created with mm23::get_mm23_metadata()
#' @param seriesid a CDID 
#'
#' @return
#' @export
#'
#' @examples
get_series_metadata <- function(df, seriesid){
  
  series <- df %>% 
    dplyr::filter(cdid == seriesid)
  # browser()
  if(nrow(series) != 1){
    stop("Error: CDID not found")
  }
  
  out <- list(
    cdid = series$cdid,
    title = series$title,
    pre_unit = series$pre_unit,
    unit = series$unit,
    release_date = series$release_date,
    next_release = series$next_release,
    important_notes = series$important_notes
  )
  
  return(out)
  
}




#' Extract a useful title from the series name. Use a regex to strip stuff from
#' beginning and end of string. defaults of "" will fail, need to fix this
#'
#' @param string 
#' @param start 
#' @param end 
#'
#' @return
#' @export
#'
#' @examples
tidy_series_name <- function(string, start = "", end = ""){
  string1 <- str_remove(string, start)
  string2 <- str_remove(string1, end)
  string2 <- str_to_lower(string2)
  return(string2)
}


#' Get structure of an OECD dataset
#' 
#' function copied from https://github.com/expersso/OECD. For some reason the
#' CRAN package function didnt work but this code does. Gets metadata detail from
#' OECD.
#'
#' @param dataset an OECD dataset
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' metadata <- get_data_structure("PRICES_CPI")
#' vars <- metadata$SUBJECT
#' locations <- metadata$LOCATION
#' }
get_data_structure <- function(dataset) {
  
  url <- paste0("https://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/",
                dataset)
  data_structure <- rsdmx::readSDMX(url)
  
  # First data frame in returned list: data frame with full variables names
  variable_desc <- data.frame(data_structure@concepts)
  
  # Clean up names (keeping only id and English description)
  variable_desc[] <- lapply(variable_desc, as.character)
  
  variable_desc$en[is.na(variable_desc$en)] <- 
    variable_desc$Name.en[!is.na(variable_desc$Name.en)]
  
  names(variable_desc)[length(names(variable_desc))] <- "description"
  variable_desc <- variable_desc[, c("id", "description")]
  
  # List of data frames in returned list: descriptions of factor levels
  code_names <- data_structure@codelists@codelists
  code_names <- vapply(code_names, function(x) x@id, "character")
  
  code_list <-  lapply(code_names, function(x) {
    df <- as.data.frame(data_structure@codelists, codelistId = x)
    try({
      df <- df[, c("id", "label.en")]
      names(df)[2] <- "label"
    }, silent = TRUE)
    df
  })
  
  names(code_list) <- gsub(paste0("CL_", dataset, "_"), "", code_names)
  
  full_df_list <- c("VAR_DESC" = list(variable_desc), code_list)
  full_df_list
}
