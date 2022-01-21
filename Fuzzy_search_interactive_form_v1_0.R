library(tidyverse)
library(fuzzyjoin)
library(rhandsontable)
library(shiny)

# Reading in the Kenya Food Composition Table
kenfct <- readxl::read_excel(here::here('data', "MOH-KENFCT_2018.xlsx"),
                             sheet = 4, skip = 2) %>%
  mutate(FCT = 'KENFCT') %>% #adding a column with the FCT short-name
  slice(1:1240)

ken_names <- c('code', 'fooditem', 'EDIBLE', 'ENERC2', 'ENERC1', 'WATER', 
               'PROTCNT', 'FAT',  'CHOAVLDF', 'FIBTG', 'ASH', 
               'CA', 'FE', 'MG', 'P', 'K', 'NA.', 'ZN', 'SE',
               'VITA_RAE', 'VITA', 'RETOL', 'CARBEQ', 
               'THIA', 'RIBF', 'NIA', 'FOLDFE', 'FOLFD',
               'VITB12', 'VITC', 'CHOLE', 'OXALAC', 'PHYTCPPD', 'IP3', 'IP4',
               'IP5', 'IP6','FASAT', "FAMS","FAPU", 'FCT')

kenfct <- kenfct %>% rename_at(vars(1:37, 60:62, 320),  ~ken_names) 

#creating variable 'foodgroups'

kenfg <- kenfct %>%  filter(code %in% c(1:15)) %>% pull(fooditem)

kenfct <- kenfct %>% mutate(foodgroup = case_when(
  str_detect(code, "[:digit:]{5}") & str_starts(code, '10') ~ kenfg[10],
  str_starts(code, '10') ~ kenfg[1],
  str_starts(code, '20') ~ kenfg[2],
  str_starts(code, '30') ~ kenfg[3],
  str_starts(code, '40') ~ kenfg[4],
  str_starts(code, '50') ~ kenfg[5],
  str_starts(code, '60') ~ kenfg[6],
  str_starts(code, '70') ~ kenfg[7],
  str_starts(code, '80') ~ kenfg[8],
  str_starts(code, '90') ~ kenfg[9],
  str_starts(code, '11') ~ kenfg[11],
  str_starts(code, '12') ~ kenfg[12],
  str_starts(code, '13') ~ kenfg[13],
  str_starts(code, '14') ~ kenfg[14],
  str_starts(code, '15') ~ kenfg[15])) %>% 
  filter(!is.na(ENERC1), !is.na(fooditem)) #Removing NA, SD/min-max

testsample = kenfct[, 1:2] #Limits the FCT to the ID and the food item name - this is all that is needed for the Fuzzy match

#Reading from the Dictionary

dictionary <- read.csv("https://raw.githubusercontent.com/LuciaSegovia/MAPS_fct/main/metadata/MAPS_Dictionary_v2.5.csv", encoding = "latin1", na.strings=c("","NA")) %>%
  rename(fooditem = FoodName_3)

#dictionary <- read_csv(here::here('metadata', "MAPS_Dictionary_v2.5.csv", encoding = "latin1")) %>%
#  rename(fooditem = FoodName_3) #This rename is here as the fuzzy search requires the columns being checked to be the same name

#Encoding(dictionary$fooditem)<-"latin1" #This helps tidy up any special characters into a format that stringdist_join can read. Its setting the column encoding

dict_testsample = dictionary[,c(9,11)]   #Limits the Dictionary to the ID and the food item name - this is all that is needed for the Fuzzy match     

fuzzy_output <-stringdist_join(testsample, dict_testsample, #This selects the two lists to check matches against
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

DF <- fuzzy_output_selection

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







# Plan for this - output to a shiny form, self hosted (so no internet connection or server needed)
# The form would go through the tickbox options, allowing the user to confirm whether the fuzzy search has returned a match
# Once the user has selected a match, ideally any other potential matches for that item will be collapsed/greyed out
# Once done, the form should return the matches in a format that follows the next step of the fct import, allowing seamless matching

#Useful link: http://stla.github.io/stlapblog/posts/shiny_editTable.html
