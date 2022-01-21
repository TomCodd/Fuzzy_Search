library(tidyverse)
library(fuzzyjoin)
library(rhandsontable)
library(shiny)

dictionary <- read.csv("https://raw.githubusercontent.com/LuciaSegovia/MAPS_fct/main/metadata/MAPS_Dictionary_v2.5.csv", encoding = "latin1", na.strings=c("","NA")) %>%
  rename(fooditem = FoodName_3)

dict_stripdown = dictionary[,c(9,11)]   #Limits the Dictionary to the ID and the food item name - this is all that is needed for the Fuzzy match     

fuzzy_output <-stringdist_join(testsample, dict_stripdown, #This selects the two lists to check matches against
                               by = "fooditem", #This allows you to select the field by which the search will be done
                               mode = "left",
                               method = "jw", #the fuzzy search method - more info here, need to do some research
                               ignore_case=TRUE,
                               max_dist = 0.3, #The maximum distance between the two strings - I believe this varies dependent on the method
                               distance_col = "dist")#This lists the distances and sets the column name they should be listed under - a perfect match should be 0

fuzzy_output_selection <- fuzzy_output %>% 
  group_by(fooditem.x) %>% #output formatting - this makes it so that the output is organised by the fooditem.x, (x being the first list item at the start of the tool, Kenfct)
  slice_min(dist, n = 5) %>% #This means only the closest 5 matches are listed per food item on the FCT
  filter(grepl("raw", fooditem.x) | dist<=0.225) #This introduces a filter. By combining this with the max_dist in the fuzzy search,  the end
# result is that any items with "raw" in their name are listed if their distance is under 0.3, 
# along with anything with a distance of 0.225 or lower. This makes the distance more forgiving for raw items.


names(fuzzy_output_selection)=c("FCT code", "FCT food item", "MAPS ID code", "MAPS dictionary name", "Fuzzy distance")
fuzzy_output_selection$"Correct match"<-FALSE

#Order - want to add a column which shows the highest result match for that FCT food item, and then alphabetical. So need a new column which shows the highest result for that item, then sort by that, secondary sort by name



DF <- fuzzy_output_selection #renames the selection going through matches for ease in the form

ui<-(fluidPage(
  fluidRow(
    titlePanel(
      h1("FCT-Dictionary food item potential matches", align = "center")),
    sidebarPanel(
      actionButton("saveBtn", "All matches identified")),
    mainPanel(
      rHandsontableOutput("table", height = "500px"),
      br()
      
      
    )
  )
))
server<-(function(input,output,session){
  
  # returns rhandsontable type object - editable excel type grid data
  output$table <- renderRHandsontable({
    rhandsontable(DF) %>%
      hot_col(1:5, readOnly = TRUE)
  })
  
  # on click of button the file will be saved to the working directory
  observeEvent(input$saveBtn, {
    write.csv(isolate(hot_to_r(input$table)), file = "Fuzzy_matches.csv", row.names = FALSE)
    print("requirements met")
    stopApp()
  })
  # hot_to_r() converts the rhandsontable object to r data object
})

shinyApp(ui, server)