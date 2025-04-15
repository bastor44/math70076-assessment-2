# load libraries 
library(shiny)
library(tidyverse)
library(bslib)
library(RSQLite)


# load functions from other scripts
# source(knitr::purl("filename.qmd", output=tempfile(), quiet=TRUE))
source(knitr::purl("school_search_filter.R", output=tempfile(), quiet=TRUE))

#### UI ####
# set up User Interface 
ui <- fluidPage(
  theme = bs_theme(version=5), 
  
  titlePanel("College Scorecard Shiny App"),
  tabsetPanel(
    id="tabset",
    
    ##### School Search #####
    # Page for filtering schools 
    tabPanel("School Search",
             sidebarLayout(
               sidebarPanel(
                 # inputs
                 # select region
                 selectInput(
                   inputId="region_select", label="Filter by Region", 
                   choices=c("U.S. Service Schools",
                             "New England", "Mid East", 
                             "Great Lakes", "Plains", "Southeast", 
                             "Southwest", "Rocky Mountains", "Far West", 
                             "Outlying Areas"
                   ),
                   multiple=TRUE
                 ),
                 
                 # select state
                 selectInput(
                   inputId="state_select", label="Filter by State",
                   choices=state_choices,
                   multiple=TRUE
                 ),
                 
                 # urbanicity 
                 checkboxGroupInput(
                   inputId="urban_select", label="Filter by Urbanicity",
                   choices=c("City", "Suburb", "Town", "Rural")
                 ),
                 
                 # input city 
                 textInput(
                   inputId = "city_select", label="Filter by City",
                   placeholder="Enter city name (optional)"
                 ),
                 
                 # enter school name
                 textInput(
                   inputId="school_name_search", label="School Name", 
                   placeholder="Enter school name (optional)"
                 ),
                 
                 
                 # size (small, medium, large)
                 checkboxGroupInput(
                   inputId="size", 
                   label="School Size", 
                   choices=c("Very small (<1,000 students)", 
                             "Small (1,000-2,999 students)", 
                             "Medium (3,000–9,999 students)", 
                             "Large (10,000+ students)")
                 ),
                 
                 # type of school (public, private, 2-year, 4-year)
                 checkboxGroupInput(
                   inputId="pub_priv",
                   label="School Type (Public/Private)",
                   choices=c("Public", "Private")
                 ),
                 checkboxGroupInput(
                   inputId="school_type",
                   label="Institution Type",
                   choices=c("2-year", "4-year")
                 ),
                 
                 # maximum cost
                 sliderInput(
                   inputId="cost",
                   label="Maximum Cost ($)",
                   min=0,
                   max=110000,
                   value=50000,
                   step=100
                 ),
                 
                 # academic field
                 # degree type 
                 checkboxGroupInput(
                   inputId="degree",
                   label="Degree Type", 
                   choices=c("Certificate", "Associate", "Bachelor's", "Graduate")
                 ),
                 
                 # search button
                 actionButton(inputId="search", label="Find Schools"),
                 
                 # clear button
                 actionButton(inputId="clear", label="Clear Search Criteria")
                 
               ),
               
               mainPanel(
                 uiOutput("school_search_output")
               )
             )
    ), 
    
    
    ##### School Compare #####
    # Page for comparing schools - some analysis of comparisons 
    tabPanel("School Compare",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   inputId="sort", 
                   label="Sort By",
                   selected=NA,
                   choices=c("Cost", "Size", "Acceptance Rate", 
                             "Graduation Rate", "Median Income After Graduation")
                 ),
                 
                 # reset comparison list
                 actionButton(inputId="reset_comparison", 
                              label="Reset List of Schools")
               ), 
               
               mainPanel(
                 # uiOutput(outputId="sort_output")
                 uiOutput(outputId="compare_output")
               )
             )
    ), 
    # add a card for each of the schools that were selected to add to comparison
    # on School Search page (cards appear the same as on that page)
    # enable ordering by cost, size, acceptance rate, grad rate, income after grad
    # Also, say at top how many results there are
    
    ##### User Compare #####
    # Page for user to compare themselves to the school(s)
    tabPanel("How do I stack up?",
             sidebarLayout(
               sidebarPanel(
                 # inputs
                 # School Name
                 textInput(
                   inputId="school-name",
                   label="School Name",
                   placeholder="Enter school name"
                 ),
                 
                 # GPA
                 numericInput(
                   inputId="gpa", 
                   label="GPA", 
                   value=0.0, 
                   min=0.0, 
                   max=5.0,
                   step=0.01
                 ),
                 
                 # SAT scores 
                 layout_columns(
                   numericInput(
                     inputId="sat-rw", 
                     label="SAT Score (Reading and Writing)",
                     value=NA, 
                     min=200,
                     max=800,
                     step=1
                   ),
                   numericInput(
                     inputId="sat-m", 
                     label="SAT Score (Math)", 
                     value=NA,
                     min=200,
                     max=800,
                     step=1
                   )
                 ),
                 
                 # ACT Scores 
                 layout_columns(
                   numericInput(
                     inputId="act-m",
                     label="ACT Math Score",
                     value=NA, 
                     min=1,
                     max=36,
                     step=1
                   ),
                   numericInput(
                     inputId="act-s",
                     label="ACT Science Score",
                     value=NA,
                     min=1,
                     max=36, 
                     step=1
                   ),
                   numericInput(
                     inputId="act-e",
                     label="ACT English Score",
                     value=NA, 
                     min=1,
                     max=36,
                     step=1
                   ), 
                   numericInput(
                     inputId="act-r",
                     label="ACT Reading Score",
                     value=NA, 
                     min=1,
                     max=36,
                     step=1
                   )
                 ),
                 numericInput(
                   inputId="act-comp",
                   label="ACT Composite Score",
                   value=NA,
                   min=1,
                   max=36,
                   step=1
                 ), 
                 
                 # include demographic info?
                 checkboxInput(
                   inputId="demo-indicator", 
                   label="View Demographic Information"
                 ),
                 # if this is checked, add additional field for gender, race/ethnicity, socioeconomic background
                 
                 actionButton(
                   inputId="compare",
                   label="Show me how I compare"
                 )
               ), 
               
               mainPanel(
                 uiOutput("mystats_output")
                 # want to create distributions of gpa/scores if possible and add a line + percentile for input value
                 # if demographics included, add pie charts, highlighting which demo user falls into
                 # add somewhere analysis of how good a match the user is for the school and how "likely" they are to 
                 #      be accepted (*make sure there is a note saying there is no guarantee)
               )
             ))
  )
)






# run app
shinyApp(ui, server)
