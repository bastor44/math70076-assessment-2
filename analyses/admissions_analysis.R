## Admissions Rates 

require(ggplot2)
require(tidyverse)

source(knitr::purl("R/filter_schools.R", output=tempfile(), quiet=TRUE))
institutions <- read.csv("data/Most-Recent-Cohorts-Institution.csv")

##### Load data 1996/97 - 2022/23 #####
year_ids <- c("1996_97", "1997_98", "1998_99", "1999_00", "2000_01", "2001_02", 
              "2002_03", "2003_04", "2004_05", "2005_06", "2006_07", "2007_08", 
              "2008_09", "2009_10", "2010_11", "2011_12", "2012_13", "2013_14", 
              "2014_15", "2015_16", "2016_17", "2017_18", "2018_19", "2019_20", 
              "2020_21", "2021_22", "2022_23")
n_years <- length(year_ids) 


# load all admissions and test data
if (file.exists("data/adm_test_data.csv")){
  adm_test_data <- read.csv("data/adm_test_data.csv")
} else {
  adm_test_dfs <- lapply(year_ids, function(y){
    file_name <- paste0("data/MERGED", y, "_PP.csv")
    read.csv(file_name) |>
      select(INSTNM, ADM_RATE, SATVRMID, SATMTMID, SATWRMID, 
             ACTCMMID, ACTENMID, ACTMTMID, ACTWRMID) |>
      rename(!!paste0("ADM_RATE_", y):=ADM_RATE, 
             !!paste0("SATVR_", y):=SATVRMID,
             !!paste0("SATMT_", y) := SATMTMID, 
             !!paste0("SATWR_", y) := SATWRMID, 
             !!paste0("ACTCM_", y) := ACTCMMID, 
             !!paste0("ACTEN_", y) := ACTENMID, 
             !!paste0("ACTMT_", y) := ACTMTMID, 
             !!paste0("ACTWR_", y) := ACTWRMID) |>
      distinct(INSTNM, .keep_all=TRUE)
  })
  
  # combine all years - only keep schools that are included in all years 
  adm_test_data <- adm_test_dfs[[1]]
  for (i in 2:length(year_ids)) {
    adm_test_data <- adm_test_data |>
      merge(adm_test_dfs[[i]], by="INSTNM")
  }
  
  # combine with current (23/24) data 
  current_adm_test <- institutions |>
    select(INSTNM, ADM_RATE, starts_with("SAT"), starts_with("ACT")) |>
    rename(ADM_RATE_2023_24 = ADM_RATE, 
           SATVR_2023_24 = SATVRMID, 
           SATMT_2023_24 = SATMTMID, 
           SATWR_2023_24 = SATWRMID, 
           ACTCM_2023_24 = ACTCMMID, 
           ACTEN_2023_24 = ACTENMID, 
           ACTMT_2023_24 = ACTMTMID, 
           ACTWR_2023_24 = ACTWRMID) |>
    distinct(INSTNM, .keep_all=TRUE)
  
  adm_test_data <- adm_test_data |>
    merge(current_adm_test, by="INSTNM")
  
  # save data frame
  write.csv(adm_test_data, "data/adm_test_data.csv", row.names=FALSE)
  
  adm_test_data <- read.csv("data/adm_test_data.csv")
}


##### EDA #####
# see if there are any patterns 

## Admissions ##
# reformat 
admissions_data <- adm_test_data |>
  select(INSTNM, starts_with("ADM_RATE")) 

# some years are missing all admissions rates --> remove
n_missing <- apply(admissions_data, 2, function(x){sum(is.na(x))})
years_to_remove <- colnames(admissions_data)[n_missing==nrow(admissions_data)]
admissions_data <- admissions_data |>
  select(-all_of(years_to_remove))

# some schools are missing admissions rates for all years --> remove 
schools_missing <- apply(admissions_data, 1, function(x) {sum(is.na(x)) == 23})
schools_missing <- admissions_data$INSTNM[schools_missing==TRUE]
admissions_data <- admissions_data |>
  filter(!INSTNM %in% schools_missing)

# pivot longer
adm_rates <- admissions_data |>
  pivot_longer(
    cols=starts_with("ADM_RATE"), 
    names_to="year", 
    names_prefix="ADM_RATE", 
    values_to="ADM_RATE"
  )

# add an average for each year 
adm_rates <- adm_rates |>
  group_by(year) |>
  mutate(AVG = mean(ADM_RATE, na.rm=TRUE)*100,,
         ADM_RATE = ADM_RATE*100, 
         year = gsub("_(\\d{4})_(\\d{2})", "\\1-\\2", year))

# plot
adm_plot <- ggplot(adm_rates, aes(x=year, y=ADM_RATE, group=INSTNM)) +
  geom_line(alpha=0.05) + 
  theme_minimal() + 
  labs(x="Year", 
       y="Admission Rate (%)", 
       title="Admissions rates of all U.S. schools from \n 2001/02-2023/24", 
       caption="Admissions rates of U.S. colleges and universities from 
       1996/97-2023/24 for which College Scorecard has data. 
       The pink line shows the average (mean) admission rate across all 
       schools each year.") +
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  # add a line for the average 
  geom_line(aes(x=year, y=AVG), color="hotpink", linewidth=1.2) + 
  theme(title=element_text(size=14), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=14), 
        panel.background = element_rect(fill=NA))
  

adm_plot

# save as image to put into final app 
ggsave("images/admissions_plot.png", plot=adm_plot, height=5, width=6)




## Most selective schools 
most_selective <- slice_min(admissions_data, n=100, 
                            order_by=ADM_RATE_2023_24)

selective_schools <- most_selective$INSTNM
selective_schools

most_selective_rates <- most_selective |>
  pivot_longer(
    cols=starts_with("ADM_RATE"), 
    names_to="year", 
    names_prefix="ADM_RATE", 
    values_to="ADM_RATE"
  ) 
most_selective_rates <- most_selective_rates |>
  group_by(year) |>
  mutate(AVG = mean(ADM_RATE, na.rm=TRUE)*100,,
         ADM_RATE = ADM_RATE*100, 
         year = gsub("_(\\d{4})_(\\d{2})", "\\1-\\2", year))

selective_plot <- ggplot(most_selective_rates, 
                         aes(x=year, y=ADM_RATE, group=INSTNM)) +
  geom_line(alpha=0.2) + 
  theme_minimal() + 
  labs(x="Year", 
       y="Admission Rate (%)", 
       title="Admissions rates of the top 100 most-selective \n colleges and universities in the U.S., 2001-2024.", 
       caption="The admissions rates of the most selective colleges and universities 
       in the U.S. have been steadily declining since 2001/02. The average of the 
       top 100 most selective schools is indicated by the red line.") +
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  # add a line for the average 
  geom_line(aes(x=year, y=AVG), color="firebrick", linewidth=1.2) + 
  theme(title=element_text(size=14), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=14), 
        panel.background = element_rect(fill=NA))
selective_plot

ggsave("images/selective_admissions.png", plot=selective_plot, height=5, width=6)

