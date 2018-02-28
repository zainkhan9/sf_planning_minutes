library(rjson)
library(stringr)
library(tidyverse)
library(magrittr)
library(lubridate)
library(RJSONIO)
library(RCurl)
library(sp)
library(rgdal)
library(mapview)
library(geosphere)

json_data <- rjson::fromJSON(file="/Users/Alexander/Downloads/test2.json")
  
projects <- NULL
speakers <- NULL
speakers_list <- NULL
errors <- 0
for(i in 1:length(json_data)){
#for(i in 1:100){
    id <- json_data[i][[1]]$ID
    item <- json_data[i][[1]]$NUMBER
    name <- json_data[i][[1]]$NAME
    adr <- json_data[i][[1]]$ADDRESS
    prelim_rec <- json_data[i][[1]]$PRELIM_REC
    cont_from <- json_data[i][[1]]$CONT_FROM
    desc <- json_data[i][[1]]$DESC
    action <- ifelse(is.null(json_data[i][[1]]$ACTION), NA, json_data[i][[1]]$ACTION)
    ayes <- ifelse(is.null(json_data[i][[1]]$AYES), NA, json_data[i][[1]]$AYES)
    absent <- ifelse(is.null(json_data[i][[1]]$ABSENT), NA, json_data[i][[1]]$ABSENT)
    file <- json_data[i][[1]]$file_name
    
    out <- c(id, item, name, adr, prelim_rec, cont_from, desc, action, ayes, absent, file)
    #names(out) <- c("id", "name", "adr", "prelim_rec", "cont_from", "desc", "action", "ayes", "absent", "file")
    projects <- rbind(projects, out)
    
    speakers_num <- length(json_data[i][[1]]$SPEAKERS)
    speakers_names <- names(json_data[i][[1]]$SPEAKERS)
    
    if(speakers_num==0){
      errors <- errors + 1
      next
    }
    
    for(j in 1:speakers_num){
      if(is.null(speakers_names)){
        temp <- unlist(str_split(paste0(json_data[i][[1]]$SPEAKERS[j][[1]], collapse=""), " – "))
        temp2 <- unlist(str_split(str_trim(str_replace(temp[1], "\\+|\\-|\\=|\\(|\\)", "")), ", | - | – "))
        name <- temp2[1]
        title <- temp2[2]
        tone <- str_extract(temp[1], "\\+|\\-|\\=")
        comment <- temp[2]
      }
      else{
        tone <- str_extract(speakers_names[j], "\\+|\\-|\\=")
        temp <- unlist(str_split(speakers_names[j], ", | - | – "))
        name <- str_trim(str_replace_all(temp[1], "\\+|\\-|\\=|\\(|\\)", ""))
        title <- temp[2]
        comment <- paste0(unlist(json_data[i][[1]]$SPEAKERS[j]), collapse=" ")
      }
      speakers_list <- rbind(speakers_list, 
                             c(id, item, file, name, title, tone, comment))
    }
}
speakers_list <- as.data.frame(speakers_list)
names(speakers_list) <- c("id", "item", "file", "name", "title", "tone", "comment")
speakers_list %<>% filter(!name=="NA") %>%
  mutate(name_merge = toupper(str_replace(str_replace(name, '/',''),' ','')))

# need to manually fix multiple names
#name_fix <- speakers_list %>% filter(lname_length>1)

# summary of speakers by project
speakers_summary <- speakers_list %>% group_by(id, item, file) %>%
  summarise(comments = n(), positive = sum(tone=="+"), negative = sum(tone=="-"), 
            neutral = sum(tone=="="))

# summary of speakers
repeat_speakers <- speakers_list %>% group_by(name_merge) %>% summarise(n=n()) #%>% filter(n>1)
ggplot(data=repeat_speakers %>% filter(!(name_merge %in% c("NANA", "(M)SPEAKER", "(F)SPEAKER", "REPRESENTINGPROJECT SPONSOR", "PROJECTSPONSOR")))) + 
  geom_histogram(aes(n)) + theme_classic() + labs(x='number of meetings a speaker appears at')

# voter file
vf <- read.csv("/Users/Alexander/Downloads/sf_voter_file.csv", stringsAsFactors = F) 
vf %<>% mutate(name_merge = paste0(FirstName, LastName)) %>%
  group_by(name_merge) %>% mutate(same_name=n())
speakers_vf <- inner_join(repeat_speakers, 
                          vf %>% filter(same_name==1))

#google maps latlong lookup
prefix <- "https://maps.googleapis.com/maps/api/geocode/json?address="
suffix <- "&key=AIzaSyB7VlT2zn5ZWCGQ4LEIT1V3B8MbmoHukDI"
gmaps.code <- function(adr, city, state){
  
  URL <- paste0(prefix, gsub(" ", "%20", adr), "%20",  gsub(" ", "%20", city), "%20", gsub(" ", "%20", state), suffix)
  try(JSON <- fromJSON(URL))
  
  if(JSON$status=="OK"){
    # extracts info
    address <- JSON$results[[1]]$formatted_address
    coord.lat <- JSON$results[[1]]$geometry$location[1]
    coord.long <- JSON$results[[1]]$geometry$location[2]
    type <- paste0(JSON$results[[1]]$types, collapse=", ")
    temp <- length(JSON$results[[1]]$address_components)
    county <- NA
    for(i in 1:temp){
      if(JSON$results[[1]]$address_components[[i]]$types[1] == "administrative_area_level_2"){
        county <- JSON$results[[1]]$address_components[[i]]$long_name
        break
      }
    }
    
    out <- c(address, county, coord.lat, coord.long, type)
    names(out) <- c("address", "county", "coord.lat", "coord.long", "type")
  }else {
    out <- rep(NA, 5)
    names(out) <- c("address", "county", "coord.lat", "coord.long", "type")
  }
  
  return(out)
}

# geocoding the speakers addresses matched to the vf
speakers_coded <- mapply(gmaps.code, adr=paste(speakers_vf$AddressNumber, speakers_vf$StreetName, speakers_vf$StreetType), city='san francisco', state='ca')
speakers_coded2 <- data.frame(t(speakers_coded))
speakers_coded3 <- cbind(as.data.frame(speakers_vf), speakers_coded2)
#  filter(!is.na(long))
  
# merging back into project stuff
speakers_list2 <- inner_join(speakers_list %>% mutate(name_merge=paste0(fname, lname)), 
                             speakers_coded3)

# merged
projects <- as.data.frame(projects)
names(projects) <- c("id", "item", "name", "adr", "prelim_rec", "cont_from", "desc", "action", "ayes", "absent", "file")
projects$adr <- as.character(projects$adr)
projects$desc <- as.character(projects$desc)
  
# number of unique projects that appear
projects_sum <- projects_merged %>% group_by(id) %>% summarise(count=n())
ggplot(data=projects_sum) + geom_histogram(aes(count)) + theme_classic() + labs(x='number of meetings a project is discussed at')

projects %<>% mutate(staff_rec = ifelse(str_detect(prelim_rec, fixed("condition|modification", ignore_case=T)), "approve with conditions",
                                 ifelse(str_detect(prelim_rec, fixed("disapprov", ignore_case=T)), "dissaprove",
                                 ifelse(str_detect(prelim_rec, fixed("approv", ignore_case=T)), "approve",
                                 ifelse(str_detect(prelim_rec, fixed("pending", ignore_case=T)), "pending",
                                 ifelse(str_detect(prelim_rec, fixed("Mitigated Negative Declaration", ignore_case=T)), "uphold pmnd",
                                 ifelse(str_detect(prelim_rec, fixed("environmental impact report|eir", ignore_case=T)), "certify eir",
                                 ifelse(str_detect(prelim_rec, fixed("California Environmental Quality Act|ceqa", ignore_case=T)), "adopt ceqa findings", NA
                                 ))))))),
                     action_sum = ifelse(str_detect(action, fixed("continued", ignore_case=T)), "continued",
                                        ifelse(str_detect(action, fixed("approved", ignore_case=T)), " approved",
                                        ifelse(str_detect(action, fixed("do not take discretionary review|no discretionary review|did not take discretionary review", ignore_case=T)), "did not take dr",
                                        ifelse(str_detect(action, fixed("information", ignore_case=T)), "informational",
                                        ifelse(str_detect(action, fixed("adopted", ignore_case=T)) & str_detect(action, fixed("California Environmental Quality Act|ceqa", ignore_case=T)), "adopted ceqa findings", 
                                        ifelse(str_detect(action, fixed("adopted", ignore_case=T)) & str_detect(action, fixed("modification|condition|amendment|amended", ignore_case=T)), "adopted with modifications", 
                                        ifelse(str_detect(action, fixed("withdrawn", ignore_case=T)), "withdrawn", 
                                        ifelse(str_detect(action, fixed("eir", ignore_case=T)) & str_detect(action, fixed("certified", ignore_case=T)), "eir certified",
                                        ifelse(str_detect(action, fixed("took dr|took discretionary review|take discretionary review", ignore_case=T)), "took dr", NA
                                        ))))))))),
                     cont_from_date = as.Date(paste0(str_extract(cont_from, "\\d{4}"), 
                                                     "-",
                                                     match(substr(str_extract(cont_from, "([a-zA-Z]*\\s)(\\d+)"), 1,3), month.abb),
                                                     "-",
                                                     str_extract(cont_from, "(\\d+)"))),
                     cont_to_date = ifelse(action_sum=="continued", as.Date(paste0(str_extract(action, "\\d{4}"), 
                                                   "-",
                                                   match(substr(str_extract(action, "([a-zA-Z]*\\s)(\\d+)"), 1,3), month.abb),
                                                   "-",
                                                   str_extract(action, "(\\d+)"))), NA),
                     adr_clean = ifelse(!str_detect(adr, "[a-zA-Z]+") & str_detect(desc, "-"), str_split(desc, "-")[[1]][1], adr))
projects$cont_to_date <- as.Date(projects_merged$cont_to_date, origin="1970-01-01")

# merging with geocoded speakers
projects_merged_geo <- inner_join(projects, 
                                  speakers_list2,
                                  by = c("id", "item", "file"))
#projects_merged_geo %<>% mutate(comments = ifelse(is.na(comments), 0 , comments),
#                                positive = ifelse(is.na(positive), 0 , positive),
#                                negative = ifelse(is.na(negative), 0 , negative),
#                                neutral = ifelse(is.na(neutral), 0 , neutral))

# unique projects with geocoded speakers
projects_geo <- projects_merged_geo %>% group_by(adr_clean) %>% summarise() %>% filter(str_detect(adr_clean, "[a-zA-Z]+"))
projects_geo <- as.data.frame(projects_geo)
# merging with everything
projects_merged <- left_join(projects, speakers_summary) %>% 

# geocoding project addresses
projects_coded <- mapply(gmaps.code, adr=projects_geo$adr_clean, city='san francisco', state='ca')

projects_coded2 <- data.frame(t(projects_coded))
projects_coded3 <- cbind(projects_geo, projects_coded2)
rownames(projects_coded3) <- NULL

projects_merged_geocoded <- inner_join(projects_merged_geo %>% rename(speaker_lat = coord.lat, speaker_long = coord.long), 
                                       projects_coded3 %>% rename(project_lat = coord.lat, project_long = coord.long),
                                       by="adr_clean")
projects_merged_geocoded$speaker_lat <- as.numeric(as.character(projects_merged_geocoded$speaker_lat))
projects_merged_geocoded$speaker_long <- as.numeric(as.character(projects_merged_geocoded$speaker_long))
projects_merged_geocoded$project_lat <- as.numeric(as.character(projects_merged_geocoded$project_lat))
projects_merged_geocoded$project_long <- as.numeric(as.character(projects_merged_geocoded$project_long))

projects_merged_geocoded %<>% rowwise() %>% mutate(dist = as.numeric(distm(c(speaker_long, speaker_lat), c(project_long, project_lat))))

ggplot(data=projects_merged_geocoded) + geom_histogram(aes(dist)) + theme_classic() + labs(x='distance from project (meters)')
ggplot(data=projects_merged_geocoded) + geom_histogram(aes(dist)) + theme_classic() + labs(x='distance from project (meters)') + 
  facet_wrap(~tone, ncol=1)

# how to plot the map
map_data <- as.data.frame(projects_merged_geocoded)
coordinates(map_data) <- ~ speaker_long + speaker_lat
proj4string(map_data) <- CRS("+proj=longlat +ellps=WGS84")

project_data <- as.data.frame(projects_merged_geocoded) %>% filter(!is.na(project_long))
coordinates(project_data) <- ~ project_long + project_lat
proj4string(project_data) <- CRS("+proj=longlat +ellps=WGS84")

mapView(map_data, cex=1, color='black') + project_data



