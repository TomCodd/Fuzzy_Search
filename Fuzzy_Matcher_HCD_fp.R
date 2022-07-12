library(tidyverse)
library(fuzzyjoin)
library(rhandsontable)
library(shiny)
library (docstring)

#Currently, the input is two dataframes, stripped down to their ID and item name 
#columns, in that order

Fuzzy_Matcher_HCD_fp <- function(df1, df2, focus_term){ #Focus term is a string that 
  #makes the filtering more lenient - use to catch more items with this term in 
  #them. Default is "raw".
  
  #' A GUI interface to match rows in two dataframes to each other via a fuzzy 
  #' string search
  #' 
  #' @description This function reads in two dataframes, both comprised of an ID
  #' row and a name row. The name rows are matched based on fuzzy search 
  #' suggestions and human confirmation using the GUI interface.
  #' 
  #' 
  #' @param df1 Required - The primary data frame, with items that need matches.
  #' The first column must be the ID column, the second must be the item names.
  #' @param df2 Required - The secondary data frame, with a list of potential
  #' items to match the contents of df1 against. The first column must be the ID
  #' column, the second must be the item names.
  #' @param focus_term Optional - Specify a string. If the string is contained 
  #' in the item name, then the fuzzy matcher opens a wider potential list of 
  #' matches to that item.
  #' @return An R object of csv that contains items from \code{df1} and their 
  #' counterparts from \code{df2} in the same row.
  
  
  # Data input checking ----
  
  #These checks are run on the inputs to make sure the data frames are data frames and the correct length, and that the string input is just a string
  
  stopifnot("df1 is not a data frame - please input a data frame consisting of an id/code column and an item name column." = is.data.frame(df1))
  stopifnot("df2 is not a data frame - please input a data frame consisting of an id/code column and an item name column." = is.data.frame(df2))
  stopifnot("df1 is too long - please make sure the input dataframes are two columns in length." = (length(df1) == 2))
  stopifnot("df2 is too long - please make sure the input dataframes are two columns in length." = (length(df2) == 2))
  
  if(!missing(focus_term)){
    stopifnot("The focus term is not a character or string - please input a character or string, e.g. 'raw'" = is.character(focus_term))
  }
  
  
  
  # Data pre-processing ----
  
  #Starting checks - the timer is started, dataframe metadata is gathered, and columns are renamed
  #Also column name creation, as some quirk means it doesn't work when its wanted later on
  
  start_time <- Sys.time() #Start time for the timer is set
  
  new_column_names <- c("ID", "Pseudo ID", #Creates a list of new column names to be applied to the table
                        paste0(deparse(substitute(df1)), "_code"),
                        paste0(deparse(substitute(df1)), "_name"),
                        paste0(deparse(substitute(df2)), "_code"),
                        paste0(deparse(substitute(df2)), "_name"),
                        "Correct Match", "Confidence", "Term Complexity")
  
  df1_item_number <- nrow(df1) #number of rows from the primary df is found
  
  df1_names <- colnames(df1) #original column names are taken for preservation
  df2_names <- colnames(df2)
  
  colnames(df1)[1] <- "item_code" #column names are reset - only columns with the same name can be matched with stringdist_join
  colnames(df2)[1] <- "item_code"
  colnames(df1)[2] <- "item_name"
  colnames(df2)[2] <- "item_name"
  
  
  
  # Fuzzy Matching ----
  
  #This is the actual fuzzy matching, where the closest entries are found for each entry
  
  fuzzy_output <- stringdist_join(df1, df2, #This selects the two lists to check matches against
                                  by = "item_name", #This allows you to select the field by which the search will be done
                                  mode = "left",
                                  method = "jw", #the fuzzy search method - more info here, need to do some research
                                  ignore_case=TRUE,
                                  max_dist = 0.3, #The maximum distance between the two strings - I believe this varies dependent on the method
                                  distance_col = "dist") #This lists the distances and sets the column name they should be listed under - a perfect match should be 0
  
  
  
  # Fuzzy Results processing ----
  
  #Results are grouped and sorted
  
  fuzzy_output_selection <- fuzzy_output %>% 
    group_by(item_name.x) %>% #output formatting - this makes it so that the output is organised by the item_name.x, (x being the first list item at the start of the tool)
    slice_min(dist, n = 5) #This means only the closest 5 matches are listed per item on the second dataframe
  
  
  if(!missing(focus_term)){
    fuzzy_output_selection <- fuzzy_output_selection %>%
      filter(grepl(focus_term, item_name.x) | dist<=0.225) #This introduces a filter. By combining this with the max_dist in the fuzzy search,  the end
  }# result is that any items with the focus term in their name are listed if their distance is under 0.3, 
  # along with anything with a distance of 0.225 or lower. This makes the distance more forgiving for items with that focus term in them.
  
  
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
  fuzzy_output_selection$TermComp <- "" #This creates the Term Complexity column
  fuzzy_output_selection <- fuzzy_output_selection %>% #This moves columns around for ease of reading in the output table
    relocate(Pseudo_ID, .after = ID) %>% 
    relocate(Confidence, .after = cor_match) %>% 
    relocate(TermComp, .after = Confidence)
  fuzzy_output_selection <- fuzzy_output_selection[,-c(7,11)] #This removes certain rows no longer needed - dist and item_min_dist
  
  colnames(fuzzy_output_selection) <- new_column_names
  
  
  
  # RShiny - Match confirmation ----
  
  DF <- fuzzy_output_selection
  
  ui <- (fluidPage( #this outlines the main page design
    fluidRow(
      column(12,
             h1("food item potential matches", align = "center"))),
    fluidRow(
      column(12,
             actionButton("saveBtn", "All matches identified"))),
    fluidRow(
      column(12,
             br())),
    fluidRow(
      column(12,
             rHandsontableOutput("table", height = "500px"))),
  )
  )
  
  server <- (function(input, output, session){
    
    values <- reactiveValues(data = DF) #Imports the data as reactiveValues
    
    observeEvent(input$table,{
      input_table<-as.data.frame(hot_to_r(input$table)) #Makes the table "hot" - i.e. interactable with rhandsontable
      
      matched_df2_codes <- input_table[,3][input_table[,7] == TRUE] #Creates a list of list A codes that have been successfully matched
      matched_df1_codes <- input_table[,5][input_table[,7] == TRUE] #creates a list of matched df1 codes
      incorrect_matched_codes <- input_table[,1][input_table[,3] %in% matched_df2_codes & input_table[,7] == FALSE | input_table[,5] %in% matched_df1_codes & input_table[,7] == FALSE] #creates the list of codes that are incorrect matches
      input_table[,2] <- input_table[,1] #resets pseudo_ID to ID
      input_table[,2][which(input_table[,1] %in% incorrect_matched_codes)]<-NA #Sets PseudoID to NA if the row contains an incorrect match
      input_table<-input_table[order(input_table[,2], na.last=TRUE),] #resorts the table based on pseudotable, putting NA matches at the bottom
      values$data<-input_table #Resets the data values to match the edited table
      
    })
    
    output$table <- renderRHandsontable({
      rhandsontable(values$data)%>% #outputs the data table
        hot_col(1:6, readOnly = TRUE) %>% #Outputs the table, and makes it so that only the True/False column is editable
        hot_col(1:2, width = 0.5) %>% #sets the ID and PseudoID columns to be very narrow, so they don't appear visible
        hot_col(col="Confidence", type = "dropdown", source = c("","high", "medium", "low")) %>% #Creates the confidence dropdown for that column
        hot_col(col="Term Complexity", type = "dropdown", source = c("","Too Complex")) %>% #Creates the confidence dropdown for that column
        
                
        #These renderers colour the incorrect matches pink, and make them uneditable - different renderers for the different type of columns
        hot_col(1:6, renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.TextRenderer.apply(this, arguments);
             var ID = instance.getData()[row][0]
             var pseudoID = instance.getData()[row][1]
             if (ID !== pseudoID) {
              td.style.background = 'pink';
             }
             
           }") %>%
        hot_col(7, renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
             var ID = instance.getData()[row][0]
             var pseudoID = instance.getData()[row][1]
             if (ID !== pseudoID) {
              td.style.background = 'pink';
              cellProperties.readOnly = true;
             }
           }") %>%
        hot_col(8, renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.DropdownRenderer.apply(this, arguments);
             var ID = instance.getData()[row][0]
             var pseudoID = instance.getData()[row][1]
             if (ID !== pseudoID) {
              td.style.background = 'pink';
              cellProperties.readOnly = true;
             }
           }") %>%
        hot_col(9, renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.DropdownRenderer.apply(this, arguments);
             var ID = instance.getData()[row][0]
             var pseudoID = instance.getData()[row][1]
             if (ID !== pseudoID) {
              td.style.background = 'pink';
              cellProperties.readOnly = true;
             }
           }")
    })
    
    observeEvent(input$saveBtn, { #Controls what happens when the save button is pressed
      output_table <- as.data.frame(hot_to_r(input$table)) #Creates an output table from the current data table
      matches <- output_table[,1][output_table[,7] == TRUE] #Creates a list of the match row ID's
      true_matches <- output_table%>%
        filter (ID %in% matches) #Creates a subset for those row ID's
      percent_completed <- round((nrow(true_matches)/df1_item_number), digits = 2)*100 #matching metadata is added - how many rows from df1 were matched
      true_matches_without_confidence <- true_matches %>% #Checks all matches have a confidence value
        filter (Confidence == "")
      match_IDs_without_confidence <- true_matches_without_confidence$ID
      output_matches <- true_matches[-c(1,2,7)]
      if (nrow(true_matches_without_confidence)>0){ #If true matches don't have confidence values, this Modal flags this for attention and fixing
        showModal(modalDialog(
          title = "Please select confidence values for these rows:",
          str_c(match_IDs_without_confidence, collapse = ", "),
          easyClose = TRUE
        ))
      } else { #Otherwise, the save options Modal appears
        showModal(modalDialog(
          title = "Please select save options",
          radioButtons("outputoption", h4("Output options"),
                       choices = list("R dataframe" = 1, "CSV file" = 2)),
          textInput("FileName", "Choose file/dataframe name",
                    value = "fuzzy_match_output"),
          footer = actionButton("outputcontinue", "Continue")
        ))
      }
      
      observeEvent(input$outputcontinue, { #Once the save buttons are selected and confirmed, the summary screen appears
        end_time <- Sys.time()
        time_taken <- round((end_time-start_time), digits = 2)
        if (input$outputoption == 1){
          assign(paste0(input$FileName), output_matches, envir = .GlobalEnv)
          showModal(modalDialog(
            title = str_c("You have matched ", nrow(true_matches), " items!"),
            str_c("Thats ", percent_completed, "% of the FCT (", df1_item_number, " items), and took ",  time_taken, " ", units(time_taken)),
            footer = actionButton("closeButton", "Close tool"),
            easyClose = TRUE
          ))
        } else { #With different outcomes if the R object or CSV outputs have been selected.
          write.csv(output_matches, file = paste0(input$FileName, ".csv"), row.names = FALSE)
          showModal(modalDialog(
            title = str_c("You have matched ", nrow(true_matches), " items!"),
            str_c("Thats ", percent_completed, "% of the FCT (", df1_item_number, " items), and took ",  time_taken, " ", units(time_taken)),
            footer = actionButton("closeButton", "Close tool"),
            easyClose = TRUE
          ))
        }
      })
    })
    observeEvent(input$closeButton, { #Controls the closing of the app
      stopApp()
    })
  })
  
  shinyApp(ui, server)
}
