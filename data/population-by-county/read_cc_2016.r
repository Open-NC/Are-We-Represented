# #################################################
#
# David Hopp
# vivariumwest@gmail.com
# 7 Feb 2018
#
# #################################################
# 
# R requires package dplyr
#
# #################################################
# 
# produce a csv file of county voting-age population counts
# 
# data source:
# https://www.census.gov/data/tables/2016/demo/popest/counties-detail.html
# to retrieve cc-est2016-alldata-37.csv
# from "Annual County Resident Population Estimates by Age, Sex, Race, and Hispanic Origin: April 1, 2010 to July 1, 2016"
#
# COUNTY is the FIPS code
#
# we want 
# YEAR=9 estimate 2016
# AGEGRP >= 5 ages 20 and higher
#   note: 4 is ages 15-19
#   take 2/5 of AGEGRP 4 as an estimate of ages 18-19
#
# ####################################
# read cc-est2016-alldata-37.csv into data frame
df_cc_est2016_alldata_37 <- read.csv(  
  "data_source/cc-est2016-alldata-37.csv",
  sep=",",
  header=TRUE,
  stringsAsFactors=FALSE
)
#
str(df_cc_est2016_alldata_37)
# ###################################
# AGEGRP >= 5
df_cc_est2016_20 <-
  df_cc_est2016_alldata_37 %>%
  dplyr::filter(
    YEAR == 6 &
    AGEGRP >= 5
  ) %>%  
  dplyr::select(
    -SUMLEV,
    -STATE,
    -STNAME,
    -CTYNAME,
    -YEAR
  )
#
str(df_cc_est2016_20)
# ###############################
# AGEGRP == 4
df_cc_est2016_ <-
  df_cc_est2016_alldata_37 %>%
  dplyr::filter(
    YEAR == 6 &
    AGEGRP == 4
  ) %>%  
  dplyr::select(
    -SUMLEV,
    -STATE,
    -STNAME,
    -CTYNAME,
    -YEAR
  )
#
str(df_cc_est2016_)
#
# this just computes 2/5 of ages 15-19 population
df_cc_est2016_tmp <- df_cc_est2016_ %>%
  dplyr::select(
    -COUNTY,
    -AGEGRP
  ) 
df_cc_est2016_tmp[] <- round(0.4*df_cc_est2016_tmp[])
str(df_cc_est2016_tmp)
#
# paste back together into a single data frame
df_cc_est2016_18 <-
  cbind(
    df_cc_est2016_ %>%
    dplyr::select(
      COUNTY,
      AGEGRP
    ),
    df_cc_est2016_tmp
  )
str(df_cc_est2016_18)
# #############################
# combine to get all ages in one data frame
#
df_cc_est2016_voteage <-
  rbind(
    df_cc_est2016_20,
    df_cc_est2016_18
  ) %>%
  dplyr::arrange(
    COUNTY,
    AGEGRP
  )
str(df_cc_est2016_voteage)
# ############################
# roll up to get total per county
df_cc_est2016_voteagetot <-
  df_cc_est2016_voteage %>%
  dplyr::select(
    -AGEGRP
  ) %>%
  dplyr::group_by(
    COUNTY
  ) %>%
  dplyr::summarize_all(
    funs(sum)
  ) %>%
  dplyr::ungroup()
str(df_cc_est2016_voteagetot)
# ############################
# write as a csv file
write.csv( 
  df_cc_est2016_voteagetot, 
  file="df_cc_est2016_voteagetot.csv", 
  row.names=FALSE
)
# ############################

