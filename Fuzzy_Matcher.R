library(tidyverse)
library(fuzzyjoin)
library(rhandsontable)
library(shiny)

#Currently, the input is two dataframes, stripped down to their ID and item name columns, in that order

Fuzzy_Matcher <- function(df1, df2, focus_term){ #Focus term is a string that makes the filtering more lenient - use to catch more items with this term in them. Default is "raw".
  
  stopifnot("df1 is not a data frame - please input a data frame consisting of an id/code column and an item name column." = is.data.frame(df1))
  stopifnot("df2 is not a data frame - please input a data frame consisting of an id/code column and an item name column." = is.data.frame(df2))
  stopifnot("df1 is too long - please make sure the input dataframes are two columns in length." = (length(df1) == 2))
  stopifnot("df2 is too long - please make sure the input dataframes are two columns in length." = (length(df2) == 2))
  stopifnot("The focus term is not a character or string - please input a character or string, e.g. 'raw'" = is.character(focus_term))
  
  
  start_time <- Sys.time()
  
  df1_item_number <- nrow(df1)
  
  df1_names <- colnames(df1)
  df2_names <- colnames(df2)
  
  colnames(df1)[1] <- "item_code"
  colnames(df2)[1] <- "item_code"
  colnames(df1)[2] <- "item_name"
  colnames(df2)[2] <- "item_name"
  
  fuzzy_output <- stringdist_join(df1, df2, #This selects the two lists to check matches against
                                  by = "item_name", #This allows you to select the field by which the search will be done
                                  mode = "left",
                                  method = "jw", #the fuzzy search method - more info here, need to do some research
                                  ignore_case=TRUE,
                                  max_dist = 0.3, #The maximum distance between the two strings - I believe this varies dependent on the method
                                  distance_col = "dist") #This lists the distances and sets the column name they should be listed under - a perfect match should be 0
  
  fuzzy_output_selection <- fuzzy_output %>% 
    group_by(item_name.x) %>% #output formatting - this makes it so that the output is organised by the item_name.x, (x being the first list item at the start of the tool)
    slice_min(dist, n = 5) %>% #This means only the closest 5 matches are listed per item on the second dataframe
    filter(grepl(focus_term, item_name.x) | dist<=0.225) #This introduces a filter. By combining this with the max_dist in the fuzzy search,  the end
  # result is that any items with the focus term in their name are listed if their distance is under 0.3, 
  # along with anything with a distance of 0.225 or lower. This makes the distance more forgiving for items with that focus term in them.
  
  fuzzy_output_selection$cor_match<-FALSE
  
  fuzzy_output_selection$Item_min_dist <- ""
  unique_entries <- unique(fuzzy_output_selection$item_code.x)
  
  for (i in 1:length(unique_entries)){
    i_subsection <- fuzzy_output_selection %>%
      filter(item_code.x == unique_entries[i])
    i_min <- min(i_subsection$dist)
    
    fuzzy_output_selection <- fuzzy_output_selection %>% mutate(Item_min_dist = replace(Item_min_dist, item_code.x == unique_entries[i], i_min))
  }
  
  fuzzy_output_selection <- fuzzy_output_selection[order(fuzzy_output_selection$Item_min_dist),]
  
  fuzzy_output_selection <- tibble::rowid_to_column(fuzzy_output_selection, "ID")
  fuzzy_output_selection$Pseudo_ID <- fuzzy_output_selection$ID
  fuzzy_output_selection$Confidence <- ""
  fuzzy_output_selection <- fuzzy_output_selection %>% 
    relocate(Pseudo_ID, .after = ID) %>% 
    relocate(Confidence, .after = cor_match)
  fuzzy_output_selection <- fuzzy_output_selection[,-c(7,10)]
  
  names(fuzzy_output_selection) = c("ID", "Pseudo ID" , paste0(deparse(substitute(df1)), "_code"), paste0(deparse(substitute(df1)), "_name"), paste0(deparse(substitute(df2)), "_code"), paste0(deparse(substitute(df2)), "_name"),  "Correct Match", "Confidence")
  
  DF <- fuzzy_output_selection
  
  
  ui <- (fluidPage(
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
    
    values <- reactiveValues(data = DF)
    
    observeEvent(input$table,{
      input_table<-as.data.frame(hot_to_r(input$table))
      
      matched_dict_codes <- input_table[,3][input_table[,7] == TRUE] #Creates a list of list A codes that have been successfully matched
      matched_FCT_codes <- input_table[,5][input_table[,7] == TRUE]
      incorrect_matched_codes <- input_table[,1][input_table[,3] %in% matched_dict_codes & input_table[,7] == FALSE | input_table[,5] %in% matched_FCT_codes & input_table[,7] == FALSE]
      input_table[,2] <- input_table[,1]
      input_table[,2][which(input_table[,1] %in% incorrect_matched_codes)]<-NA
      input_table<-input_table[order(input_table[,2], na.last=TRUE),]
      values$data<-input_table
      
    })
    
    output$table <- renderRHandsontable({
      rhandsontable(values$data)%>%
        hot_col(1:6, readOnly = TRUE) %>% #Outputs the table, and makes it so that only the True/False column is editable
        hot_col(1:2, width = 0.5) %>%
        hot_col(col="Confidence", type = "dropdown", source = c("","high", "medium", "low")) %>%
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
    
    observeEvent(input$saveBtn, {
      output_table <- as.data.frame(hot_to_r(input$table))
      matches <- output_table[,1][output_table[,7] == TRUE]
      true_matches <- output_table%>%
        filter (ID %in% matches)
      percent_completed <- round((nrow(true_matches)/df1_item_number), digits = 2)
      true_matches_without_confidence <- true_matches %>%
        filter (Confidence == "")
      match_IDs_without_confidence <- true_matches_without_confidence$ID
      output_matches <- true_matches[-c(1,2,7)]
      if (nrow(true_matches_without_confidence)>0){
        showModal(modalDialog(
          title = "Please select confidence values for these rows:",
          str_c(match_IDs_without_confidence, collapse = ", "),
          easyClose = TRUE
        ))
      } else {
        showModal(modalDialog(
          title = "Please select save options",
          radioButtons("outputoption", h4("Output options"),
                       choices = list("R dataframe" = 1, "CSV file" = 2)),
          textInput("FileName", "Choose file/dataframe name",
                    value = "fuzzy_match_output"),
          footer = actionButton("outputcontinue", "Continue")
        ))
      }
      
      observeEvent(input$outputcontinue, {
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
        } else {
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
    observeEvent(input$closeButton, {
      stopApp()
    })
  })
  
  shinyApp(ui, server)
}
