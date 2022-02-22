

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
         bill_number = ifelse(state %in% c("IN", "UT", "WY"),
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

restric <- fread("raw_data/2021 Restrictive Bills - passed 2021.csv")
colnames(restric) <- c("state", "bill", "year")

restric <- restric %>% 
  mutate(bill_merge = trimws(paste(substring(bill, 1, 2),
                            gsub(" ", "", substring(bill, 3)))))


restric <- left_join(restric,
                     all_bills %>% 
                       select(bill_id,
                              title,
                              file,
                              bill_merge,
                              year))
##########################################
final_votes <- fread("raw_data/_Final Vote_ Analysis - 2021 Restrictive Bills.csv")
colnames(final_votes) <- clean_names(final_votes)

final_votes <- select(final_votes, bill, house_url, senate_url) %>% 
  pivot_longer(ends_with("url")) %>% 
  rename(chamber = name) %>% 
  mutate(chamber = gsub("_url", "", chamber),
         roll_call_id = as.integer(sub(".*/", "", value)),
         bill = ifelse(bill == "IA SF 413/IA SSB 1199", "IA SF 413", bill),
         roll_call_id = ifelse(bill == "OK HB 2663" & chamber == "house", 1073681, roll_call_id),
         roll_call_id = ifelse(bill == "OK HB 2663" & chamber == "senate", 1060047, roll_call_id)) %>% 
  select(-value)

final_votes <- left_join(restric, final_votes)

files <- list.files("raw_data/bulk_legiscan_data/", full.names = T,
                    pattern = "^rollcalls.csv", recursive = T)


rcs <- rbindlist(lapply(files, function(f){
  j <- fread(f) %>% 
    mutate(date = as.Date(date),
           file = gsub("rollcalls", "bills", f))
}), fill = T) %>% 
  select(-chamber)

rcs <- left_join(final_votes, rcs)

##########################################
files <- list.files("raw_data/bulk_legiscan_data/", full.names = T,
                    pattern = "^votes.csv", recursive = T)


votes <- rbindlist(lapply(files, function(f){
  j <- fread(f) %>% 
    mutate(file = gsub("votes", "bills", f))
}), fill = T) 

rcs <- left_join(rcs, votes)

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

fwrite(slim, "temp/2021_passed_sponsors.csv")

#############

rcs2 <- left_join(rcs,
                 select(people,
                        file,
                        people_id,
                        party)) %>% 
  filter(vote_desc %in% c("Nay", "Yea")) %>% 
  group_by(bill,
           chamber, party, vote_desc) %>% 
  tally()

rcs2 <- left_join(restric %>% 
                    select(bill),
                  rcs2)

rcs2 <- rcs2 %>% 
  pivot_wider(id_cols = c(bill,
                          chamber, party), names_from = vote_desc, values_from = n,
              values_fill = 0) %>% 
  mutate(n = paste(Yea, "/", (Nay + Yea)),
         n = ifelse(Yea + Nay == 0, NA, n)) %>% 
  select(-`NA`, -Nay, -Yea) %>% 
  pivot_wider(id_cols = c(bill, chamber), names_from = party, values_from = n,
              values_fill = "0 / 0") %>% 
  select(-`NA`) %>% 
  pivot_wider(id_cols = c(bill), names_from = chamber,
              values_from = c(D, R, I, L), values_fill = "Voice Vote") %>% 
  select(Bill = bill, R_house, D_house, I_house, L_house,
         R_senate, D_senate, I_senate, L_senate) %>% 
  mutate_all(~ ifelse(. == "0 / 0", NA, .),
             ~ paste0("`", .))

fwrite(rcs2, "temp/votes_2021.csv")
#######################

