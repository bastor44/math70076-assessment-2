require(ggplot2)
require(tidyverse)

source(knitr::purl("filter_schools.R", output=tempfile(), quiet=TRUE))
institutions <- read.csv("Most-Recent-Cohorts-Institution.csv")
type_pp_map 

## COA vs. Inflation 
###### Load inflation data #####
inflation_df <- readxl::read_xlsx("SeriesReport-20250422082250_691115.xlsx", 
                                  range="A12:P42", col_names=TRUE)

###### Load institutions data #####
year_ids <- c("1996_97", "1997_98", "1998_99", "1999_00", "2000_01", "2001_02", 
              "2002_03", "2003_04", "2004_05", "2005_06", "2006_07", "2007_08", 
              "2008_09", "2009_10", "2010_11", "2011_12", "2012_13", "2013_14", 
              "2014_15", "2015_16", "2016_17", "2017_18", "2018_19", "2019_20", 
              "2020_21", "2021_22", "2022_23")
n_years <- length(year_ids) 

# load all tuition data for all schools for all years
coa_dfs <- lapply(year_ids, function(y){
  read.csv(paste0("MERGED", y, "_PP.csv")) |>
    select(INSTNM, TUITIONFEE_IN, TUITIONFEE_OUT, CONTROL) |>
    mutate(CONTROL = type_pp_map$LABEL[type_pp_map$VALUE==CONTROL]) |>
    rename(!!paste0("TUITION_IN_", y) := TUITIONFEE_IN, 
           !!paste0("TUITION_OUT_", y) := TUITIONFEE_OUT)|>
    distinct(INSTNM)
})

coa_data <- coa_dfs[[1]]
for (i in 2:length(coa_dfs)) {
  coa_data <- coa_data |>
    merge(coa_dfs[[i]], by=c("INSTNM", "CONTROL"), all=TRUE)
}


## EDA 
