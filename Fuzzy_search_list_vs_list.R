library(tidyverse)
library(fuzzyjoin)
library(stringi)

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
dictionary <- read_csv(here::here('metadata', "MAPS_Dictionary_v2.5.csv")) %>%
  rename(fooditem = FoodName_3) #This rename is here as the fuzzy search requires the columns being checked to be the same name

Encoding(dictionary$fooditem)<-"latin1" #This helps tidy up any special characters into a format that stringdist_join can read. Its setting the column encoding

dict_testsample = dictionary[,c(9,11)]   #Limits the Dictionary to the ID and the food item name - this is all that is needed for the Fuzzy match     
#dict_testsample = dictionary[411,c(9,11)] #This entry was the problematic one - the accented e in rosé caused Fuzzy search to break

fuzzy_output <-stringdist_join(testsample, dict_testsample, #This selects the two lists to check matches against
                by = "fooditem", #This allows you to select the field by which the search will be done
                mode = "left",
                method = "jw", #the fuzzy search method - more info here, need to do some research
                ignore_case=TRUE,
                max_dist = 10, #The maximum distance between the two strings - I believe this varies dependent on the method
                distance_col = "dist") %>% #This lists the distances and sets the column name they should be listed under - a perfect match should be 0
  group_by(fooditem.x) %>% #output formatting - this makes it so that the output is organised by the fooditem.x, (x being the first list item at the start of the tool, Kenfct)
  slice_min(order_by = dist, n = 5) #This means only the closest 5 matches are listed per food item on the FCT

fuzzy_output %>%
  readr::write_excel_csv(., 
                         here::here('output', 'fuzzy_matches.csv'))