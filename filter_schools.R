
require(tidyverse)
require(roxygen2)

##### Load Data #####
institutions <- read.csv("data/Most-Recent-Cohorts-Institution.csv")
data_dictionary <- read.csv("data/institution_data_dictionary.csv")

##### School Search Request #####
# Filter data from institutions dataset to match search criteria 

# Filter on: 
# Region -  REGION takes values 0-9 corresponding to each of the choices
# State - ST_FIPS takes values 1-78 corresponding to each state & outlying terr
#         or STABBR (AL, AK, ..., WY, AS, ..., MH) 59 options
# Urbanicity - LOACLE (int 11-43)
# City - CITY (strings)
# School Name - INSTNM (each also has unique UNITID), ALIAS (nicknames e.g. UAB)
# School Size - CCSIZSET (int -2, 0-18; 2/4yr and size)
# School Type (public/private) - CONTROL (1,2,3 - pub, priv, priv)
# Institution Type (2yr/4yr) - CCSIZSET (int -2, 0-18; 2/4yr and size)
# Maximum Cost - TUITIONFEE_IN and TUITIONFEE_OUT
# Degree Type - HIGHDEG (highest degree awarded; 0,1,2,3,4 w 4=graduate)


# Return: 
# school name - INSTNM
# city - CITY
# state - ST_FIPS
# address - ADDR
# size category - CCSIZSET  
# actual size - UGDS (enrollment of all undergrad students)
# type - CCSIZSET and CONTROL                          
# acceptance rate - ADM_RATE/ADM_RATE_SUPP (suppressed for n<30)
# SAT - SATVRMID, SATMTMID, SATWRMID (midpoints for reading, math, writing) or 
#       SATVR50, SATMT50 (50th percentile)
# ACT - ACTCMMID, ACTENMID, ACTMTMID, ACTWRMID (midpoints cumulative, English, 
#       math, writing) or ACTCM50, ACTEN50, ACTMT50
# grad rate - C150_4, C150_L4 (completion rate for first-time full-time students 
#             @ 4yr and less than 4yr inst)
# cost - TUITIONFEE_IN and TUITIONFEE_OUT
# student/faculty ratio - STUFACR (undergrad to instructional faculty ratio)
# gender split - FEMALE (fraction of female students)
# URL - INSTURL


##### Mappings #####
# region 
region_map <- data_dictionary[which(data_dictionary$VARIABLE.NAME=="REGION"):
                                (which(data_dictionary$VARIABLE.NAME=="REGION")+9), 
                              c("VALUE", "LABEL")] 
region_map <- region_map |>
  mutate(LABEL=gsub("\\s\\(.+\\)", "", x=LABEL))

# states 
states_map <- data_dictionary[which(data_dictionary$VARIABLE.NAME=="ST_FIPS"):
                                (which(data_dictionary$VARIABLE.NAME=="ST_FIPS")+57), 
                              c("VALUE", "LABEL")]
state_choices <- unique(states_map$LABEL)

# urbanicity 
locale_map <- data_dictionary[which(data_dictionary$VARIABLE.NAME=="LOCALE"):
                                (which(data_dictionary$VARIABLE.NAME=="LOCALE")+11), 
                              c("VALUE", "LABEL")]
locale_map <- locale_map |>
  mutate(LABEL=gsub(":\\s.+\\(.+\\)", "", LABEL))
locale_map <- list(
  "City" = 11:13, 
  "Suburb" = 21:23, 
  "Town" = 31:33, 
  "Rural" = 41:43
)

# size & type  
cc_map <- data_dictionary[which(data_dictionary$VARIABLE.NAME=="CCSIZSET"):
                            (which(data_dictionary$VARIABLE.NAME=="CCSIZSET")+19), 
                          c("VALUE", "LABEL")] 
cc_map <- cc_map |>
  separate(LABEL, into=c("TYPE", "SIZE", "RES"), sep=", ")

size_map <- list(
  "Very small (<1,000 students)" = cc_map$VALUE[cc_map$SIZE=="very small"], 
  "Small (1,000-2,999 students)" = cc_map$VALUE[cc_map$SIZE=="small"],  
  "Medium (3,000–9,999 students)" = cc_map$VALUE[cc_map$SIZE=="medium"], 
  "Large (10,000+ students)" = cc_map$VALUE[cc_map$SIZE=="large"]
)

type_24_map <- list(
  "2-year" = cc_map$VALUE[cc_map$TYPE=="Two-year"], 
  "4-year" = cc_map$VALUE[cc_map$TYPE=="Four-year"]
)

# type pub/priv
type_pp_map <- data_dictionary[which(data_dictionary$VARIABLE.NAME=="CONTROL"):
                                 (which(data_dictionary$VARIABLE.NAME=="CONTROL")+2), 
                               c("VALUE", "LABEL")]
type_pp_map <- type_pp_map |>
  mutate(LABEL=gsub("\\s.+", "", LABEL))


# degree type 
degree_map <- data_dictionary[which(data_dictionary$VARIABLE.NAME=="HIGHDEG"):
                                (which(data_dictionary$VARIABLE.NAME=="HIGHDEG")+4), 
                              c("VALUE", "LABEL")]
degree_map <- degree_map |>
  mutate(LABEL = gsub("\\s+degree", "", LABEL))
degree_map <- list(
  "Certificate" = 1, 
  "Associate" = 2, 
  "Bachelor's" = 3, 
  "Graduate" = 4
)



##### FUNCTION TO FILTER SCHOOLS #####
#' filter_schools
#'
#' @param data - institutions
#' @param inputs - school search criteria input by the user
#'
#' @returns data - data frame of schools matching the search criteria 
#' @export
#'
#' @examples
#' filter_schools(institutions, user_input=list(state_select="Wisconsin""))
filter_schools <- function(data, user_input) {
  # regions
  if (!is.null(user_input$region_select) && length(user_input$region_select)>0) {
    data <- data[data$REGION %in% 
                   region_map$VALUE[region_map$LABEL 
                                    %in% user_input$region_select], ]
  }
  
  # states 
  if (!is.null(user_input$state_select) && length(user_input$state_select)>0) {
    data <- data[data$ST_FIPS %in% 
                   states_map$VALUE[states_map$LABEL %in%
                                      user_input$state_select], ]
  }
  
  # urbanicity/locale
  if (!is.null(user_input$urban_select) && length(user_input$urban_select)>0) {
    selected_locales <- unlist(locale_map[user_input$urban_select])
    data <- data[data$LOCALE %in% selected_locales, ]
  }

  # city 
  if (!is.null(user_input$city_select) && nzchar(user_input$city_select)) {
    data <- data[
      !is.na(data$CITY) &
        grepl(user_input$city_select, data$CITY, ignore.case = TRUE),
    ]
  }
  
  # school name (text user_input)
  if (length(user_input$school_name_search)>0) {
    name <- user_input$school_name_search 
    data <- data[
      # check full name and "alias"
      grepl(name, data$INSTNM, ignore.case = TRUE) |
        grepl(name, data$ALIAS, ignore.case = TRUE), ]
  }
  
  # size
  if (!is.null(user_input$size) && length(user_input$size)>0) {
    selected_sizes <- unlist(size_map[user_input$size])
    data <- data[data$CCSIZSET %in% selected_sizes, ]
  }
  
  # type (2/4 year)
  if (!is.null(user_input$school_type) && length(user_input$school_type)>0) {
    selected_types <- unlist(type_24_map[user_input$school_type])
    data <- data[data$CCSIZSET %in% selected_types, ]
  }
  
  # type (public/private)
  if (!is.null(user_input$pub_priv) && length(user_input$pub_priv)>0) {
    data<- data[data$CONTROL %in%
                  type_pp_map$VALUE[type_pp_map$LABEL %in% user_input$pub_priv], ]
  }
  
  # cost 
  if (!is.null(user_input$cost) && length(user_input$cost)>0) {
    data <- data[data$TUITIONFEE_IN < user_input$cost | 
                   data$TUITIONFEE_OUT < user_input$cost, ]
  }
  
  # degree type 
  if (!is.null(user_input$degree) && length(user_input$degree)>0) {
    selected_degrees <- unlist(degree_map[user_input$degree])
    data <- data[data$HIGHDEG <= selected_degrees, ]
  }
  
  # desired columns of filtered data 
  data <- data |>
    select(INSTNM, CITY, ST_FIPS, ADDR, CCSIZSET, UGDS, CONTROL, ADM_RATE, 
           SATVRMID, SATMTMID, SATWRMID, SAT_AVG, ACTCMMID, ACTENMID, ACTMTMID, 
           ACTWRMID, C150_4, C150_L4, TUITIONFEE_IN, TUITIONFEE_OUT, STUFACR, 
           FEMALE, INSTURL, LATITUDE, LONGITUDE) |>
    filter(!is.na(INSTNM))
  
  # add https to urls so they open properly when clicked on 
  data <- data |>
    mutate(INSTURL = ifelse(!str_detect(INSTURL, "https://"), 
      paste0("https://", INSTURL), INSTURL))
  
  return(data)
}


