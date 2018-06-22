library(readr)
library(tidyverse)

#Import state voter reg db last modified 6.16.2018 at 6:44am
#You can download the state voter db at https://s3.amazonaws.com/dl.ncsbe.gov/data/ncvoter_Statewide.zip
ncvoter_Statewide <- read_delim("ncvoter_Statewide.txt", 
                                "\t", escape_double = FALSE, trim_ws = TRUE)

#Select county and demographic information
ncvr_select <- ncvoter_Statewide %>%
  select(county_desc, voter_status_desc, voter_status_reason_desc, race_code, ethnic_code, party_cd, gender_code, birth_year)
          
#Filter for active/verified voters 
voter_verified <- ncvr_select %>%
  filter(voter_status_reason_desc == "VERIFIED")

#Create a dataframe for party affiliation
voter_party <- voter_verified %>%
  group_by(county_desc, party_cd) %>%
  count(county_desc, party_cd) %>%
  spread(party_cd, n)

#Create a dataframe for gender
voter_gender <- voter_verified %>%
  group_by(county_desc, gender_code) %>%
  count(county_desc, gender_code) %>%
  spread(gender_code, n) %>%
  rename(Male = "M", Female = "F", Undisclosed = "U", gender_na = `<NA>`)

#Create a dataframe for race
voter_race <- voter_verified %>%
  group_by(county_desc, race_code) %>%
  count(county_desc, race_code) %>%
  spread(race_code, n) %>%
  rename(race_na = `<NA>`)

#Create a dataframe for ethnicity
voter_eth <- voter_verified %>%
  group_by(county_desc, ethnic_code) %>%
  count(county_desc, ethnic_code) %>%
  spread(ethnic_code, n) 

#Join the 3 databases into one
#First, inner join party and gender
ij_party_gender <- inner_join(voter_party, voter_gender, county_desc = "county_desc")

#Then create an inner join with race and ethnicity
ij_race_eth <-  inner_join(voter_race, voter_eth, county_desc = "county_desc")

#Now join the two inner joins
nc_voter_demographics <- inner_join(ij_party_gender, ij_race_eth, county_desc = county_desc)
  
#Write csv file 
write.csv(nc_voter_demographics, "nc_voter_demographics.csv")



