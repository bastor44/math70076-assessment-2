require(ggplot2)
require(tidyverse)

source(knitr::purl("filter_schools.R", output=tempfile(), quiet=TRUE))
institutions <- read.csv("data/Most-Recent-Cohorts-Institution.csv")

# type pub/priv
type_pp_map <- data_dictionary[which(data_dictionary$VARIABLE.NAME=="CONTROL"):
                                 (which(data_dictionary$VARIABLE.NAME=="CONTROL")+2), 
                               c("VALUE", "LABEL")]
type_pp_map <- type_pp_map |>
  mutate(LABEL=gsub("\\s.+", "", LABEL))
type_pp_map 

## COA vs. Inflation 
###### Load inflation data #####
inflation_df <- readxl::read_xlsx("data/SeriesReport-20250422082250_691115.xlsx", 
                                  range="A12:P42", col_names=TRUE)

###### Load institutions data #####
year_ids <- c("1996_97", "1997_98", "1998_99", "1999_00", "2000_01", "2001_02", 
              "2002_03", "2003_04", "2004_05", "2005_06", "2006_07", "2007_08", 
              "2008_09", "2009_10", "2010_11", "2011_12", "2012_13", "2013_14", 
              "2014_15", "2015_16", "2016_17", "2017_18", "2018_19", "2019_20", 
              "2020_21", "2021_22", "2022_23")
n_years <- length(year_ids) 

# load all tuition data for all schools for all years
if (!file.exists("data/coa_data.csv")) {
  coa_dfs <- lapply(year_ids, function(y){
    read.csv(paste0("data/MERGED", y, "_PP.csv")) |>
      select(INSTNM, TUITIONFEE_IN, TUITIONFEE_OUT, CONTROL) |>
      mutate(CONTROL = type_pp_map$LABEL[match(CONTROL, type_pp_map$VALUE)]) |>
      rename(!!paste0("TUITION_IN_", y) := TUITIONFEE_IN, 
             !!paste0("TUITION_OUT_", y) := TUITIONFEE_OUT)|>
      distinct(INSTNM, .keep_all = TRUE)
  })
  
  # merge all years
  coa_data <- coa_dfs[[1]]
  for (i in 2:length(coa_dfs)) {
    coa_data <- coa_data |>
      merge(coa_dfs[[i]], by=c("INSTNM", "CONTROL"), all=TRUE)
  }
  
  # remove years with no observed costs
  n_missing <- apply(coa_data, 2, function(x){sum(is.na(x))})
  years_to_remove <- colnames(coa_data)[n_missing==nrow(coa_data)]
  coa_data <- coa_data |>
    select(-all_of(years_to_remove))
  
  # remove schools with no observed costs 
  rows_missing <- apply(coa_data, 1, function(x){sum(is.na(x)) == 46})
  schools_missing <- coa_data$INSTNM[rows_missing==TRUE]
  coa_data <- coa_data |>
    filter(!INSTNM %in% schools_missing)
    
  write.csv(coa_data, "data/coa_data.csv", row.names=FALSE)
  coa_data <- read.csv("data/coa_data.csv")
} else {
  coa_data <- read.csv("data/coa_data.csv")
}


# All schools - out of state tuition 
coa_out <- coa_data |>
  select(INSTNM, starts_with("TUITION_OUT_"))
coa_out <- coa_out |>
  pivot_longer(
    cols=starts_with("TUITION_OUT_"), 
    names_to="year", 
    names_prefix = "TUITION_OUT_", 
    values_to="TUITION_OUT"
  )

# add average 
coa_out <- coa_out |>
  group_by(year) |>
  mutate(AVG = mean(TUITION_OUT, na.rm=TRUE), 
         year = gsub("_(\\d{4})_(\\d{2})", "\\1-\\2", year))

# plot
coa_plot <- ggplot(coa_out, aes(x=year, y=TUITION_OUT, group=INSTNM)) +
  geom_line(alpha=0.05) +
  theme_minimal() +
  labs(x="Year", 
       y="Out-of-State Tuition ($)", 
       title="Out-of-state tuition for U.S. colleges and universities, 
                2000-2024", 
       subtitle="Not adjusted for inflation", 
       caption="Cost of attending college in the U.S. has been steadily rising since 2000.
       All schools for which data was available from College Scorecard are shown in black, 
       and the average across all schools is shown in red.") +
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  # add a line for the average 
  geom_line(aes(x=year, y=AVG), color="firebrick", linewidth=1.2) + 
  theme(title=element_text(size=14), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=14), 
        panel.background = element_rect(fill=NA))
coa_plot

ggsave("images/coa_plot.png", coa_plot, height=5, width=6)



# group by public/private
coa_pp <- coa_data |>
  select(INSTNM, starts_with("TUITION_OUT_"), CONTROL) |>
  pivot_longer(
    cols=starts_with("TUITION_OUT_"), 
    names_to="year", 
    names_prefix = "TUITION_OUT_", 
    values_to="TUITION_OUT"
  )
coa_pp <- coa_pp |>
  group_by(CONTROL, year) |>
  summarise(AVG = mean(TUITION_OUT, na.rm=TRUE)) |>
  filter(!is.na(CONTROL))

pp_plot <- ggplot(coa_pp, aes(x=year, y=AVG, group=CONTROL, color=CONTROL)) +
  geom_line(linewidth=1.2) +
  theme_minimal() + 
  theme(title=element_text(size=14), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=14), 
        panel.background = element_rect(fill=NA)) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  theme(legend.position = "none")+
  labs(x="Year", 
       y="Out-of-State Tuition ($)", 
       title="Out-of-state tuition for U.S. colleges and 
       universities, 2000-2024, stratified by Public/Private", 
       subtitle="Not adjusted for inflation", 
       caption="Cost of attending college in the U.S. has been steadily rising for 
       both public and private institutions since 2000. The average cost for each group is shown.
       The cost of private institutions has been rising at a faster rate than their public counterparts.")

pp_plot
ggsave("images/pp_plot.png", pp_plot, height=5, width=6)


# adjust for inflation 
