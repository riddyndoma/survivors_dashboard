## clean_locations

## Adding locations

## Unfortunately at present, the locations ids attached to the follow ups
## and persons store only the lowest level of location. 
## In order to merge on the location data at multiple levels we need to do 
## multiple joins and filter the data. 

## Process

## 1. start from locations mongodb export (if hierarchy changes, this should be updated) 
## 2. join on the lowest area
## 3. add that area to the follow up uuids
## 3. add that area to the case / contact uuids

## Just select the two variables needed from contacts, contact uuid & location_id
## location_id should be ID of RECO at level 6 but often not at right level 

locations_join <- cleaned_contacts %>% 
  distinct(location_id)

### Name 6
# Join on name_6 area
n_6 <- left_join(locations_join, cleaned_locations, by = "location_id") %>%
  mutate(original_location_id = location_id) %>%
  select(original_location_id,
         id_a = location_id,
         admin_level_a = admin_level,
         name_a = name,
         parent_a = parent_location_id)

n_5 <- left_join(n_6,cleaned_locations, by = c("parent_a" = "location_id")) %>%
  select(original_location_id,
         id_a,
         admin_level_a,
         name_a,
         id_b = parent_a,
         admin_level_b = admin_level,
         name_b = name,
         parent_b = parent_location_id)

n_4 <- left_join(n_5,cleaned_locations, by = c("parent_b" = "location_id")) %>%
  select(original_location_id,
         id_a,
         admin_level_a,
         name_a,
         id_b,
         admin_level_b,
         name_b,
         id_c = parent_b,
         admin_level_c = admin_level,
         name_c = name,
         parent_c = parent_location_id)

n_3 <- left_join(n_4,cleaned_locations, by = c("parent_c" = "location_id")) %>%
  select(original_location_id,
         id_a,
         admin_level_a,
         name_a,
         id_b,
         admin_level_b,
         name_b,
         id_c,
         admin_level_c,
         name_c,
         id_d = parent_c,
         admin_level_d = admin_level,
         name_d = name,
         parent_d = parent_location_id)

n_2 <- left_join(n_3,cleaned_locations, by = c("parent_d" = "location_id")) %>%
  select(original_location_id,
         id_a,
         admin_level_a,
         name_a,
         id_b,
         admin_level_b,
         name_b,
         id_c,
         admin_level_c,
         name_c,
         id_d,
         admin_level_d,
         name_d,
         id_e = parent_d,
         admin_level_e = admin_level,
         name_e = name,
         parent_e = parent_location_id)


# hierarchy_join_renamed <- n_2 %>%
#   mutate(admin_6 = case_when(
#                     str_detect(name_a, "MVE") ~ name_a,
#                     admin_level_a == "lng_reference_data_category_location_geographical_level_admin_level_6" ~ name_a
#                   ),
#          id_6 = case_when(
#                 str_detect(name_a, "MVE") ~ id_a,
#                 admin_level_a == "lng_reference_data_category_location_geographical_level_admin_level_6" ~ id_a
#                   ),
#          admin_5 = case_when(
#                 admin_level_b == "lng_reference_data_category_location_geographical_level_admin_level_5" ~ name_b,
#                 admin_level_a == "lng_reference_data_category_location_geographical_level_admin_level_5" ~ name_a
#                   ),
#          id_5 = case_when(
#            admin_level_b == "lng_reference_data_category_location_geographical_level_admin_level_5" ~ id_b,
#            admin_level_a == "lng_reference_data_category_location_geographical_level_admin_level_5" ~ id_a
#                   ),
#          admin_4 = case_when(
#                 admin_level_c == "lng_reference_data_category_location_geographical_level_admin_level_4" ~ name_c,
#                 admin_level_b == "lng_reference_data_category_location_geographical_level_admin_level_4" ~ name_b,
#                 admin_level_a == "lng_reference_data_category_location_geographical_level_admin_level_4" ~ name_a
#                   ),
#          id_4 = case_when(
#            admin_level_c == "lng_reference_data_category_location_geographical_level_admin_level_4" ~ id_c,
#            admin_level_b == "lng_reference_data_category_location_geographical_level_admin_level_4" ~ id_b,
#            admin_level_a == "lng_reference_data_category_location_geographical_level_admin_level_4" ~ id_a
#                   ),
#          admin_3 = case_when(
#                 admin_level_d == "lng_reference_data_category_location_geographical_level_admin_level_3" ~ name_d,
#                 admin_level_c == "lng_reference_data_category_location_geographical_level_admin_level_3" ~ name_c,
#                 admin_level_b == "lng_reference_data_category_location_geographical_level_admin_level_3" ~ name_b,
#                 admin_level_a == "lng_reference_data_category_location_geographical_level_admin_level_3" ~ name_a
#                   ),
#          id_3 = case_when(
#            admin_level_d == "lng_reference_data_category_location_geographical_level_admin_level_3" ~ id_d,
#            admin_level_c == "lng_reference_data_category_location_geographical_level_admin_level_3" ~ id_c,
#            admin_level_b == "lng_reference_data_category_location_geographical_level_admin_level_3" ~ id_b,
#            admin_level_a == "lng_reference_data_category_location_geographical_level_admin_level_3" ~ id_a
#                   ),
#          admin_2 = case_when(
#                 admin_level_e == "lng_reference_data_category_location_geographical_level_admin_level_2" ~ name_e,
#                 admin_level_d == "lng_reference_data_category_location_geographical_level_admin_level_2" ~ name_d,
#                 admin_level_c == "lng_reference_data_category_location_geographical_level_admin_level_2" ~ name_c,
#                 admin_level_b == "lng_reference_data_category_location_geographical_level_admin_level_2" ~ name_b,
#                 admin_level_a == "lng_reference_data_category_location_geographical_level_admin_level_2" ~ name_a
#                   ),
#          id_2 = case_when(
#            admin_level_e == "lng_reference_data_category_location_geographical_level_admin_level_2" ~ id_e,
#            admin_level_d == "lng_reference_data_category_location_geographical_level_admin_level_2" ~ id_d,
#            admin_level_c == "lng_reference_data_category_location_geographical_level_admin_level_2" ~ id_c,
#            admin_level_b == "lng_reference_data_category_location_geographical_level_admin_level_2" ~ id_b,
#            admin_level_a == "lng_reference_data_category_location_geographical_level_admin_level_2" ~ id_a
#          
#                   )) 
# 
# hierarchy_join_renamed <- hierarchy_join_renamed %>%
#  
hierarchy_join_renamed =n_2 %>%
select(original_location_id,
         prestataire = name_a,
         #prestataire_id = id_6,
         programme = name_b,
         programme_id = id_b,
         Zone = name_c,
         zone_id = id_c,
         province = name_d,
         province_id = id_d,
         pays = name_e,
         pays_id = id_e)


         
## now we can attach this location data to the cases, contacts and follow ups         

followups_join <- left_join(cleaned_followups, hierarchy_join_renamed, by = c("location_id" = "original_location_id")) 

cases_join <- left_join(cleaned_cases, hierarchy_join_renamed, by = c("location_id" = "original_location_id"))

contacts_join <- left_join(cleaned_contacts, hierarchy_join_renamed, by = c("location_id" = "original_location_id"))



## give us a list of supervisors and their corresponding user IDs
## concatenate first and last name
## join using sql package bc could not find an easier way without getting errors

library(sqldf)
sup <- cleaned_users %>%
  mutate(name = paste(firstname, lastname, sep = "_")) %>%
  dplyr::filter(grepl("sup", name)) %>%
  select(
    uuid,
    name) 

sup_join <- sqldf('select 
                  sup.uuid AS sup_id,
                  sup.name AS sup,
                  cleaned_teams.uuid AS team_id,
                  cleaned_teams.name AS team,
                  cleaned_teams.locationids AS location_id_team
                from sup
                  inner join cleaned_teams on sup.uuid = cleaned_teams.userids 
                  ') 

contacts_visual_id <- contacts_join %>%
  select(person_id = uuid, visual_id = visualid)

sup_join_subset <- sup_join %>%
  distinct(location_id_team, .keep_all = TRUE)


followups_join <- left_join(followups_join, sup_join_subset, by = "team_id") 
followups_join <- left_join(followups_join, contacts_visual_id, by = "person_id") 
cases_join <- left_join(cases_join, sup_join_subset, by = c("location_id" = "location_id_team"))
contacts_join <- left_join(contacts_join, sup_join_subset, by = c("location_id" = "location_id_team"))
hierarchy_join_renamed <- left_join(hierarchy_join_renamed, sup_join_subset, by = c("original_location_id" = "location_id_team"))
