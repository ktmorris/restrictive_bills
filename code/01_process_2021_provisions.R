

files <- list.files("raw_data/bulk_legiscan_data/", full.names = T,
                    pattern = "^bills.csv", recursive = T)


all_bills <- rbindlist(lapply(files, function(f){
  j <- fread(f) %>% 
    mutate(status_date = as.Date(status_date),
           file = f)
}), fill = T) %>% 
  mutate(state = substring(url, 22, 23),
         year = year(status_date),
         year = ifelse(is.na(year), as.integer(substring(last_action_date, 1, 4)), year))

ses <- fread("raw_data/sessions.csv")

all_bills <- left_join(all_bills, ses)

all_bills <- all_bills %>% 
  mutate(bill_number = ifelse(state %in% c("FL", "ID"),
                              paste0(substring(bill_number, 1, 1), "B",
                                     as.integer(substring(bill_number, 2))),
                              bill_number),
         bill_number = ifelse(state %in% c("IN", "UT", "WY", "TN", "CT", "IL",
                                           "MI") & !(state %in% c("MI", "IL") &
                                                       substring(bill_number, 1, 3) %in%
                                                       c("HCR", "HJR")),
                              paste0(substring(bill_number, 1, 2),
                                     as.integer(substring(bill_number, 3))),
                                     bill_number),
         bill_number = ifelse(state %in% c("SC", "RI"),
                              paste0(substring(bill_number, 1, 1),
                                     as.integer(substring(bill_number, 2))),
                              bill_number),
         bill_number = ifelse(state %in% c("NY"),
                              paste0(substring(bill_number, 1, 1),
                                     "B",
                                     as.integer(substring(bill_number, 2))),
                              bill_number),
         bill_number = ifelse(state %in% c("NC"),
                              gsub("B", "", bill_number),
                              bill_number),
         bill_merge = paste(state, bill_number),
         year = trimws(paste(year, session))) %>% 
  group_by(bill_merge, year) %>% 
  arrange(session_id) %>% 
  filter(row_number() == 1)
slall <- select(all_bills, bill_id, bill_number, year, bill_merge, title)
#######

provs <- fread("raw_data/2021_provisions.csv")[,c(1, 2, 3, 4, 18, 9, 11)]
colnames(provs) <- c("state", "bill", "subject", "impact", "year", "date_added", "url")

one_per <- provs %>% 
  mutate(date_added = as.Date(date_added, "%m/%d/%Y")) %>% 
  group_by(state, bill, year) %>% 
  summarize(date_added = min(date_added)) %>% 
  filter(year != "")

one_per <- one_per %>% 
  mutate(bill_merge = trimws(paste(substring(bill, 1, 2),
                                   gsub(" ", "", substring(bill, 3)))),
         bill_merge = ifelse(state == "Rhode Island", gsub("B", "", bill_merge),
                             bill_merge),
         bill_merge = ifelse(substring(bill_merge, 1, 3) == "PA ",
                             gsub("C", "", bill_merge), bill_merge),
         bill_merge = ifelse(substring(bill_merge, 1, 2) %in% c("SC", "NC"),
                             gsub("B", "", bill_merge), bill_merge),
         bill_merge = ifelse(substring(bill_merge, 1, 3) == "NJ ",
                             gsub("B", "", bill_merge), bill_merge),
         bill_merge = ifelse(bill_merge == "FL HJR61", "FL HB61", bill_merge),
         bill_merge = ifelse(substring(bill_merge, 1, 3) == "ND ",
                             paste0(substring(bill_merge, 1, 3),
                                    substring(bill_merge, 6)),
                             bill_merge),
         bill_merge = ifelse(bill_merge == "VT SB15", "VT S0015", bill_merge),
         bill_merge = ifelse(bill_merge == "ME HP104/LD148", "ME LD148", bill_merge),
         bill_merge = ifelse(bill_merge == "ME HP68/LD102", "ME LD102", bill_merge),
         bill_merge = ifelse(bill_merge == "ME HP78/LD112", "ME LD112", bill_merge),
         bill_merge = ifelse(bill_merge == "PA SJR30", "PA SB30", bill_merge),
         bill_merge = ifelse(bill_merge == "MS SCR508", "MS SC508", bill_merge),
         bill_merge = ifelse(bill_merge == "MS SCR520", "MS SC520", bill_merge),
         bill_merge = ifelse(bill_merge == "MS HCR17", "MS HC17", bill_merge),
         bill_merge = ifelse(bill_merge == "NH HB2", "NH SB2", bill_merge),
         bill_merge = ifelse(bill_merge == "TN SB1178", "TN HB1178", bill_merge),
         bill_merge = ifelse(bill_merge == "TX HB237", "TX HR237", bill_merge),
         bill_merge = ifelse(bill_merge == "ME HP143", "ME LD208", bill_merge),
         bill_merge = ifelse(bill_merge == "ME LD253/HP174", "ME LD253", bill_merge),
         bill_merge = ifelse(bill_merge == "ME LD253/HP174", "ME LD253", bill_merge),
         bill_merge = ifelse(bill_merge == "IA SJR9/IASSB1083", "IA SSB1083", bill_merge),
         bill_merge = ifelse(bill_merge == "ME SR91", "ME LD202", bill_merge),
         bill_merge = ifelse(bill_merge == "VT SB50", "VT S0050", bill_merge),
         bill_merge = ifelse(bill_merge == "MA SD241", "MA S485", bill_merge),
         bill_merge = ifelse(bill_merge == "NC HJR330", "NC H330", bill_merge),
         bill_merge = ifelse(bill_merge == "NC HB119", "NC H119", bill_merge),
         bill_merge = ifelse(bill_merge == "MA HD652", "MA H825", bill_merge),
         bill_merge = ifelse(bill_merge == "WV SB516", "WV SB561", bill_merge),
         bill_merge = ifelse(bill_merge == "MA HD1393", "MA H812", bill_merge),
         bill_merge = ifelse(bill_merge == "MA HD237", "MA H788", bill_merge),
         bill_merge = ifelse(bill_merge == "MA SD39", "MA S484", bill_merge),
         bill_merge = ifelse(bill_merge == "MA SD189", "MA S457", bill_merge),
         bill_merge = ifelse(bill_merge == "MA HD449", "MA H838", bill_merge),
         bill_merge = ifelse(substring(bill_merge, 1, 2) == "CT",
                             gsub("R", "", bill_merge), bill_merge),
         bill_merge = ifelse(bill_merge == "ME HR73", "ME LD107", bill_merge),
         year = ifelse(bill_merge %in% c("NJ S2820",
                                         "NJ A4655",
                                         "NJ A4814",
                                         "NJ S3203",
                                         "TX SB287",
                                         "TX HR237"), "2021", year),
         year = ifelse(bill_merge %in% c("TN SB2",
                                         "TX HB76",
                                         "TX HB400",
                                         "TX HB221",
                                         "TX HB583",
                                         "TX SB95",
                                         "TX HB230",
                                         "TX SB100",
                                         "VA SB1101",
                                         "VA HB1746",
                                         "DE HB15",
                                         "FL HB61",
                                         "NJ AB 5171",
                                         "NJ S3297",
                                         "TN SB18",
                                         "VA HB1758",
                                         "VA SB1118",
                                         "NJ A5165",
                                         "NJ S3300",
                                         "NJ A5166",
                                         "TN SB23",
                                         "VA HB1800",
                                         "VA SB1100",
                                         "NJ A5171",
                                         "CA AJR3",
                                         "FL SB402"), "2020", year),
         year = ifelse(year == "2021" & state == "Texas", "2020", year),
         year = ifelse(year == "2020" & bill_merge %in% c("TX HB304",
                                                        "TX HB530",
                                                        "TX HB574",
                                                        "TX HB740",
                                                        "TX SB287",
                                                        "TX HB1031",
                                                        "TX HB1026",
                                                        "TX SB303",
                                                        "TX SB342",
                                                        "TX HB1128",
                                                        "TX HB1056",
                                                        "TX SB377",
                                                        "TX SB379",
                                                        "TX SB378",
                                                        "TX SB381",
                                                        "TX HB1174",
                                                        "TX HB1176",
                                                        "TX HB1175",
                                                        "TX HB1170",
                                                        "TX HB1179",
                                                        "TX HB1232",
                                                        "TX HB1244",
                                                        "TX HB1242",
                                                        "TX SB426",
                                                        "TX HB1243",
                                                        "TX HB1245",
                                                        "TX SB408",
                                                        "TX HB1366",
                                                        "TX HB1385",
                                                        "TX HB1375",
                                                        "TX HB1382",
                                                        "TX HB3424",
                                                        "AR SB557"), "2021", year),
         year = ifelse(state == "Texas" & date_added >= "2021-02-03" & year == "2020", "2021", year),
         year = ifelse(bill_merge %in% c("CA SB34",
                                         "CA AB53",
                                         "CA AB372",
                                         "NC H605"), "2022", year),
         year = ifelse(bill_merge %in% c("MN SF2", "TX HB82"), "2021 (S)", year)) %>% 
  filter(substring(bill_merge, 1, 5) != "KY BR",
         bill_merge != "MA H2559",
         state != "")


one_per <- left_join(one_per,
                     fread("raw_data/cleaner.csv") %>% 
                       mutate(date_added = as.Date(date_added, "%m/%d/%Y"))) %>% 
  mutate(bill_merge = ifelse(!is.na(bill_merge2) & bill_merge2 != "", bill_merge2, bill_merge),
         year = ifelse(!is.na(year2) & year2 != "", year2, year))

one_per <- left_join(one_per,
                     all_bills %>% 
                       select(bill_id,
                              title,
                              file,
                              bill_merge,
                              year, description)) %>% 
  group_by(state, bill, year, title, bill_id) %>% 
  filter(row_number() == 1)

provs <- left_join(provs, select(ungroup(one_per), bill, bill_id, file, year, state)) %>% 
  mutate(impact = case_when(tolower(substring(impact, 1, 1)) == "r" ~ "R",
                            tolower(substring(impact, 1, 1)) == "n" ~ "N",
                            tolower(substring(impact, 1, 1)) %in% c("e", "p") ~ "E"),
         subject = tolower(subject))

provs <- left_join(provs,
                   fread("raw_data/groupings.csv")) %>% 
  select(-subject) %>% 
  filter(!is.na(group),
         !is.na(impact))

passage <- fread("raw_data/2021 Restrictive Bills - passed 2021.csv") %>% 
  mutate(`BILL NUMBER` = ifelse(`BILL NUMBER` == "IA SF 413", "IA SF 413/IA SSB 1199", `BILL NUMBER`),
         `BILL NUMBER` = ifelse(`BILL NUMBER` == "IA SF 568", "IA SSB 1237/IA SF 568", `BILL NUMBER`))

provs$pass <- provs$bill %in% passage$`BILL NUMBER`

saveRDS(provs, "temp/clean_provisions_2021.rds")

###########################################

files <- list.files("raw_data/bulk_legiscan_data/", full.names = T,
                    pattern = "^sponsors.csv", recursive = T)


sponsors <- rbindlist(lapply(files, function(f){
  j <- fread(f) %>% 
    mutate(file = gsub("sponsors", "bills", f))
}), fill = T)

sponsors <- left_join(provs, sponsors)

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
         name,
         party,
         district,
         position,
         impact,
         group,
         pass)

saveRDS(slim, "temp/sponsor_intro_2021.rds")
