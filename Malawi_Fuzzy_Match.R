library(tidyverse)
library(fuzzyjoin)
library(rhandsontable)
library(shiny)

# Reading in the Kenya Food Composition Table
MAFoods_FCT<- read.csv(here::here('output', 'MAPS_MAFOODS_v1.2.csv'))

testsample = MAFoods_FCT[, 1:2] %>%
  rename(fooditem = original_food_name) #Limits the FCT to the ID and the food item name - this is all that is needed for the Fuzzy match

FCT_item_number <- nrow(testsample)
start_time <- Sys.time()
#Reading from the Dictionary

dictionary <- read.csv("https://raw.githubusercontent.com/LuciaSegovia/MAPS_fct/main/metadata/MAPS_Dictionary_v2.5.csv", encoding = "latin1", na.strings=c("","NA")) %>%
  rename(fooditem = FoodName_3) #This rename is here as the fuzzy search requires the columns being checked to be the same name

dict_testsample = dictionary[,c(9,11)]   #Limits the Dictionary to the ID and the food item name - this is all that is needed for the Fuzzy match     

fuzzy_output <-stringdist_join(dict_testsample, testsample, #This selects the two lists to check matches against
                               by = "fooditem", #This allows you to select the field by which the search will be done
                               mode = "left",
                               method = "jw", #the fuzzy search method - more info here, need to do some research
                               ignore_case=TRUE,
                               max_dist = 0.3, #The maximum distance between the two strings - I believe this varies dependent on the method
                               distance_col = "dist")#This lists the distances and sets the column name they should be listed under - a perfect match should be 0

fuzzy_output_selection <- fuzzy_output %>% 
  group_by(fooditem.x) %>% #output formatting - this makes it so that the output is organised by the fooditem.x, (x being the first list item at the start of the tool, Kenfct)
  slice_min(dist, n = 5) %>% #This means only the closest 5 matches are listed per food item on the dictionary
  filter(grepl("raw", fooditem.x) | dist<=0.225) #This introduces a filter. By combining this with the max_dist in the fuzzy search,  the end
# result is that any items with "raw" in their name are listed if their distance is under 0.3, 
# along with anything with a distance of 0.225 or lower. This makes the distance more forgiving for raw items.

fuzzy_output_selection$cor_match<-FALSE

fuzzy_output_selection$Item_min_dist <- ""
unique_entries <- unique(fuzzy_output_selection$ID_3)

for (i in 1:length(unique_entries)){
  i_subsection <- fuzzy_output_selection %>%
    filter(ID_3 == unique_entries[i])
  i_min <- min(i_subsection$dist)
  
  fuzzy_output_selection <- fuzzy_output_selection %>% mutate(Item_min_dist = replace(Item_min_dist, ID_3 == unique_entries[i], i_min))
}

fuzzy_output_selection <- fuzzy_output_selection[order(fuzzy_output_selection$Item_min_dist),]

fuzzy_output_selection <- tibble::rowid_to_column(fuzzy_output_selection, "ID")
fuzzy_output_selection$Pseudo_ID <- fuzzy_output_selection$ID
fuzzy_output_selection$Confidence <- ""
fuzzy_output_selection <- fuzzy_output_selection %>% 
  relocate(Pseudo_ID, .after = ID) %>% 
  relocate(Confidence, .after = cor_match)
fuzzy_output_selection <- fuzzy_output_selection[,-c(7,10)]


names(fuzzy_output_selection)=c("ID", "Pseudo ID" , "MAPS ID code", "MAPS dictionary name", "FCT code", "FCT food item",  "Correct Match", "Confidence")

DF <- fuzzy_output_selection


ui<-(fluidPage(
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

server<-(function(input,output,session){
  
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
    output_table<-as.data.frame(hot_to_r(input$table))
    matches <- output_table[,1][output_table[,7] == TRUE]
    true_matches <- output_table%>%
      filter (ID %in% matches)
    percent_completed <- round((nrow(true_matches)/FCT_item_number), digits = 2)
    true_matches_without_confidence <- true_matches %>%
      filter (Confidence == "")
    match_IDs_without_confidence <- true_matches_without_confidence$ID
    end_time <- Sys.time()
    time_taken <- round((end_time-start_time)/60, digits = 2)
    if (nrow(true_matches_without_confidence)>0){
      showModal(modalDialog(
        title = "Please select confidence values for these rows:",
        str_c(match_IDs_without_confidence, collapse = ", "),
        easyClose = TRUE
      ))
    } else {
      write.csv(true_matches, file = "MAFoods_Fuzzy_search_matches.csv", row.names = FALSE)
      showModal(modalDialog(
        title = str_c("You have matched ", nrow(true_matches), " items!"),
        str_c("Thats ", percent_completed, "% of the FCT (", FCT_item_number, " items), and took ",  time_taken, " minutes."),
        footer = actionButton("closeButton", "Close tool"),
        easyClose = TRUE
      ))
    }
  })
  observeEvent(input$closeButton, {
    stopApp()
  })
})

shinyApp(ui, server)