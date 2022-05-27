library(tidyverse)
library(fuzzyjoin)
library(rhandsontable)
library(shiny)

Fuzzy_Matcher <- function(df1, df2, focus_term){ #Focus term is a string that makes the filtering more lenient - use to catch more items with this term in them. Default is "raw".
  
  print(paste0(deparse(substitute(df1)), "_name"))
  
  new_column_names <- c("ID", "Pseudo ID", #Creates a list of new column names to be applied to the table
                        paste0(deparse(substitute(df1)), "_code"),
                        paste0(deparse(substitute(df1)), "_name"),
                        paste0(deparse(substitute(df2)), "_code"),
                        paste0(deparse(substitute(df2)), "_name"),
                        "Correct Match", "Confidence")
  
  print(length(new_column_names))
  print(new_column_names)
  
  # Data input checking ----
  
  #These checks are run on the inputs to make sure the data frames are data frames and the correct length, and that the string input is just a string
  
  stopifnot("df1 is not a data frame - please input a data frame consisting of an id/code column and an item name column." = is.data.frame(df1))
  stopifnot("df2 is not a data frame - please input a data frame consisting of an id/code column and an item name column." = is.data.frame(df2))
  stopifnot("df1 is too long - please make sure the input dataframes are two columns in length." = (length(df1) == 2))
  stopifnot("df2 is too long - please make sure the input dataframes are two columns in length." = (length(df2) == 2))
  stopifnot("The focus term is not a character or string - please input a character or string, e.g. 'raw'" = is.character(focus_term))
  
  
  new_column_names <- c("ID", "Pseudo ID", #Creates a list of new column names to be applied to the table
                        paste0(deparse(substitute(df1)), "_code"),
                        paste0(deparse(substitute(df1)), "_name"),
                        paste0(deparse(substitute(df2)), "_code"),
                        paste0(deparse(substitute(df2)), "_name"),
                        "Correct Match", "Confidence")
  
  print(length(new_column_names))
  
  # Data pre-processing ----
  
  #Starting checks - the timer is started, dataframe metadata is gathered, and columns are renamed
  
  start_time <- Sys.time() #Start time for the timer is set
  
  df1_item_number <- nrow(df1) #number of rows from the primary df is found
  
  df1_names <- colnames(df1) #original column names are taken for preservation
  df2_names <- colnames(df2)
  
  colnames(df1)[1] <- "item_code" #column names are reset - only columns with the same name can be matched with stringdist_join
  colnames(df2)[1] <- "item_code"
  colnames(df1)[2] <- "item_name"
  colnames(df2)[2] <- "item_name"
  
  new_column_names <- c("ID", "Pseudo ID", #Creates a list of new column names to be applied to the table
                        paste0(deparse(substitute(df1)), "_code"),
                        paste0(deparse(substitute(df1)), "_name"),
                        paste0(deparse(substitute(df2)), "_code"),
                        paste0(deparse(substitute(df2)), "_name"),
                        "Correct Match", "Confidence")
  
  print(length(new_column_names))
  
  # Fuzzy Matching ----
  
  #This is the actual fuzzy matching, where the closest entries are found for each entry
  
  fuzzy_output <- stringdist_join(df1, df2, #This selects the two lists to check matches against
                                  by = "item_name", #This allows you to select the field by which the search will be done
                                  mode = "left",
                                  method = "jw", #the fuzzy search method - more info here, need to do some research
                                  ignore_case=TRUE,
                                  max_dist = 0.3, #The maximum distance between the two strings - I believe this varies dependent on the method
                                  distance_col = "dist") #This lists the distances and sets the column name they should be listed under - a perfect match should be 0
  
  new_column_names <- c("ID", "Pseudo ID", #Creates a list of new column names to be applied to the table
                        paste0(deparse(substitute(df1)), "_code"),
                        paste0(deparse(substitute(df1)), "_name"),
                        paste0(deparse(substitute(df2)), "_code"),
                        paste0(deparse(substitute(df2)), "_name"),
                        "Correct Match", "Confidence")
  
  print(length(new_column_names))
  
  # Fuzzy Results processing ----
  
  #Results are grouped and sorted
  
  fuzzy_output_selection <- fuzzy_output %>% 
    group_by(item_name.x) %>% #output formatting - this makes it so that the output is organised by the item_name.x, (x being the first list item at the start of the tool)
    slice_min(dist, n = 5) %>% #This means only the closest 5 matches are listed per item on the second dataframe
    filter(grepl(focus_term, item_name.x) | dist<=0.225) #This introduces a filter. By combining this with the max_dist in the fuzzy search,  the end
  # result is that any items with the focus term in their name are listed if their distance is under 0.3, 
  # along with anything with a distance of 0.225 or lower. This makes the distance more forgiving for items with that focus term in them.
  
  new_column_names <- c("ID", "Pseudo ID", #Creates a list of new column names to be applied to the table
                        paste0(deparse(substitute(df1)), "_code"),
                        paste0(deparse(substitute(df1)), "_name"),
                        paste0(deparse(substitute(df2)), "_code"),
                        paste0(deparse(substitute(df2)), "_name"),
                        "Correct Match", "Confidence")
  
  print(length(new_column_names))
  
  # Prep work for match confirmations ----
  
  #Tables are set up and sorted for the Shiny interface and functionality
  
  fuzzy_output_selection$cor_match <- FALSE #Correct Match column is created and populated with False
  
  fuzzy_output_selection$Item_min_dist <- "" #Item minimum distance column is created
  unique_entries <- unique(fuzzy_output_selection$item_code.x) #unique entries are listed for items from df1
  
  for (i in 1:length(unique_entries)){ #This for loop finds all potential matches for a particular item, finds the closest one, and populates the item_min_dist column for all of those items with that minimum for sorting
    i_subsection <- fuzzy_output_selection %>%
      filter(item_code.x == unique_entries[i])
    i_min <- min(i_subsection$dist)
    fuzzy_output_selection <- fuzzy_output_selection %>% mutate(Item_min_dist = replace(Item_min_dist, item_code.x == unique_entries[i], i_min))
  }
  
  fuzzy_output_selection <- fuzzy_output_selection[order(fuzzy_output_selection$Item_min_dist),] #This sorts items based on the item min dist
  
  fuzzy_output_selection <- tibble::rowid_to_column(fuzzy_output_selection, "ID") #This creates a column with the current row numer as the value
  fuzzy_output_selection$Pseudo_ID <- fuzzy_output_selection$ID #This duplicates this value into a Pseudo_ID column
  fuzzy_output_selection$Confidence <- "" #This creates the Confidence column 
  fuzzy_output_selection <- fuzzy_output_selection %>% #This moves columns around for ease of reading in the output table
    relocate(Pseudo_ID, .after = ID) %>% 
    relocate(Confidence, .after = cor_match)
  fuzzy_output_selection <- fuzzy_output_selection[,-c(7,10)] #This removes certain rows no longer needed - dist and item_min_dist
  
  print(length(fuzzy_output_selection))

  new_column_names <- c("ID", "Pseudo ID", #Creates a list of new column names to be applied to the table
                        paste0(deparse(substitute(df1)), "_code"),
                        paste0(deparse(substitute(df1)), "_name"),
                        paste0(deparse(substitute(df2)), "_code"),
                        paste0(deparse(substitute(df2)), "_name"),
                        "Correct Match", "Confidence")
  
  print(length(new_column_names))
  
}
  