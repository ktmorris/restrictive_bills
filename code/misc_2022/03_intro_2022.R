

files <- list.files("raw_data/bulk_legiscan_data/", full.names = T,
                    pattern = "^bills.csv", recursive = T)


all_bills <- rbindlist(lapply(files, function(f){
  j <- fread(f) %>% 
    mutate(status_date = as.Date(status_date),
           file = f)
}), fill = T) %>% 
  mutate(state = substring(url, 22, 23),
         year = year(status_date))

ses <- fread("raw_data/sessions.csv")

all_bills <- left_join(all_bills, ses)

all_bills <- all_bills %>% 
  mutate(bill_number = ifelse(state %in% c("FL", "ID"),
                              paste0(substring(bill_number, 1, 1), "B",
                                     as.integer(substring(bill_number, 2))),
                              bill_number),
         bill_number = ifelse(state %in% c("IN", "UT", "WY", "MI"),
                              paste0(substring(bill_number, 1, 2),
                                     as.integer(substring(bill_number, 3))),
                              bill_number),
         bill_number = ifelse(state %in% c("NY"),
                              paste0(substring(bill_number, 1, 1),
                                     "B",
                                     as.integer(substring(bill_number, 2))),
                              bill_number),
         bill_merge = paste(state, bill_number),
         year = trimws(paste(year, session))) %>% 
  group_by(bill_merge, year) %>% 
  arrange(session_id) %>% 
  filter(row_number() == 1)

#######

restric <- fread("raw_data/intro_2022.csv", sep = ",")
colnames(restric) <- c("state", "bill", "year", "url")

restric <- restric %>% 
  mutate(bill_merge = trimws(paste(substring(bill, 1, 2),
                                   gsub(" ", "", substring(bill, 3)))),
         year = as.character(year),
         bill_merge = ifelse(bill_merge == "IA SSB1204/IASF531", "IA SSB1204", bill_merge),
         bill_merge = ifelse(bill_merge %in% c("NC SB326"), paste0(substring(bill_merge, 1, 4),
                                                                   substring(bill_merge, 6)),
                             bill_merge),
         bill_merge = ifelse(bill_merge == "MA H3878/MAHD1349", "MA H3878", bill_merge),
         bill_merge = ifelse(bill_merge == "MA HD2559", "MA H762", bill_merge),
         bill_merge = ifelse(substring(bill_merge, 1, 2) == "PA", gsub("C", "", bill_merge),
                             bill_merge),
         bill_merge = ifelse(substring(bill_merge, 1, 2) %in% c("SC", "NJ"), gsub("B", "", bill_merge),
                             bill_merge),
         bill_merge = ifelse(substring(bill_merge, 1, 2) == "PA", gsub("JR", "B", bill_merge),
                             bill_merge),
         bill_merge = ifelse(bill_merge %in% c("MA H812/MAHD1393",
                                               "MA H762/MAHD642",
                                               "MA H792/MAHD503",
                                               "MA S468/MASD1096",
                                               "MA H816/MAHD2703"),
                             substring(bill_merge, 1, 7), bill_merge),
         year = ifelse(bill_merge %in% c("CA SB511", "CA SB597"), "2022", year),
         year = ifelse(bill_merge %in% c("NH HB1153",
                                         "NH HB1522",
                                         "NH HB1543",
                                         "NH HB1542",
                                         "NH CACR36",
                                         "VA HB34",
                                         "VA HB35",
                                         "VA HB36",
                                         "VA HB39",
                                         "VA HB46",
                                         "VA HB24",
                                         "NH SB418"), "2021", year))


restric <- left_join(restric,
                     all_bills %>% 
                       select(bill_id,
                              title,
                              file,
                              bill_merge,
                              year))

mi <- filter(restric, is.na(bill_id))


###########################################

files <- list.files("raw_data/bulk_legiscan_data/", full.names = T,
                    pattern = "^sponsors.csv", recursive = T)


sponsors <- rbindlist(lapply(files, function(f){
  j <- fread(f) %>% 
    mutate(file = gsub("sponsors", "bills", f))
}), fill = T)

sponsors <- left_join(restric, sponsors)

###########################################

files <- list.files("raw_data/bulk_legiscan_data/", full.names = T,
                    pattern = "^people.csv", recursive = T)


people <- rbindlist(lapply(files, function(f){
  j <- fread(f) %>% 
    mutate(file = gsub("people", "bills", f))
}), fill = T)

sponsors <- left_join(sponsors, people)

slim <- sponsors %>% 
  select(state,
         bill,
         title,
         name,
         party,
         district,
         position)

fwrite(slim, "temp/2021_sponsors.csv")
saveRDS(slim, "temp/sponsors_intro_2022.rds")
#############
