library(tidyverse)
library(fuzzyjoin)
library(rhandsontable)
library(shiny)
library (docstring)

Column_Creator <- function(df, name){
  new_column_name <- paste0("separate by '", name, "'?")
  df$x <- ""
  colnames(df)[colnames(df) == "x"] <- new_column_name
  return(df)
}


#Currently, the input is two dataframes, stripped down to their ID and item name 
#columns, in that order

Term_Separator <- function(df, separation_terms = c(",", "and")){ #Focus term is a string that 
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
  #' @param df Required - The primary data frame, with items that need matches.
  #' The first column must be the ID column, the second must be the item names.
  #' @param separation_terms Optional - Specify a list of strings. The terms in 
  #' the list are used as potential separation points for the names in df1. 
  #' Defaults to "," and "and".
  #' @return A dataframe consisting of the contents of \code{df} with the 
  #' specified terms seperated.
  
  
  # Data input checking ----
  
  #These checks are run on the inputs to make sure the data frames are data frames and the correct length, and that the string input is just a string
  
  stopifnot("df is not a data frame - please input a data frame consisting of an id/code column and an item name column." = is.data.frame(df))
  stopifnot("df is too long - please make sure the input dataframes are two columns in length." = (length(df) == 2))

  if(!missing(separation_terms)){
    stopifnot("The separation terms are not a character or string - please input a character or string, e.g. c(',', 'and')" = is.character(separation_terms))
  }
  
  
  
  # Data pre-processing ----
  
  #Starting checks - the timer is started, dataframe metadata is gathered, and columns are renamed
  #Also column name creation, as some quirk means it doesn't work when its wanted later on
  
  start_time <- Sys.time() #Start time for the timer is set
  
  
  df1_names <- colnames(df1) #original column names are taken for preservation
  df2_names <- colnames(df2)
  
  for (i in (1:length(separation_terms))){
    df <- Column_Creator(df, separation_terms[i])
  }
  
  new_column_names <- c("order", #Creates a list of new column names to be applied to the table
                        paste0(deparse(substitute(df)), "_code"),
                        paste0(deparse(substitute(df)), "_name"))
  
  for (i in (4:ncol(df))){
    additional_name <- colnames(df)[i]
    new_column_names <- c(new_column_names, additional_name)
  }

  
  
  
  colnames(fuzzy_output_selection) <- new_column_names
  
  
  
  # RShiny - Match confirmation ----
  
  DF <- fuzzy_output_selection
  
  ui <- (fluidPage( #this outlines the main page design
    fluidRow(
      column(12,
             h1("Select terms to separate", align = "center"))),
    fluidRow(
      column(12,
             actionButton("saveBtn", "All terms separated"))),
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
