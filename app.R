# load libraries 
library(shiny)
library(tidyverse)
library(bslib)
library(RSQLite)
library(leaflet)

# load functions from other scripts
# source(knitr::purl("filename.qmd", output=tempfile(), quiet=TRUE))
source(knitr::purl("filter_schools.R", output=tempfile(), quiet=TRUE))

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
                 #textInput(
                 selectizeInput(
                   inputId="school_name",
                   label="School Name",
                   choices=NULL,
                   multiple=FALSE,
                   selected=NULL, 
                   options=list(
                     placeholder="Enter school name", 
                     maxOptions=15
                   )
                   #placeholder="Enter school name"
                 ),
                 
                 # SAT scores 
                 layout_columns(
                   numericInput(
                     inputId="sat_vr", 
                     label="SAT Score (Reading)",
                     value=500, 
                     min=200,
                     max=800,
                     step=1
                   ),
                   numericInput(
                     inputId="sat_wr", 
                     label="SAT Score (Writing)", 
                     value=500, 
                     min=200, 
                     max=800, 
                     step=1
                   ), 
                   numericInput(
                     inputId="sat_m", 
                     label="SAT Score (Math)", 
                     value=500,
                     min=200,
                     max=800,
                     step=1
                   )
                 ),
                 
                 # ACT Scores 
                 layout_columns(
                   numericInput(
                     inputId="act_m",
                     label="ACT Math Score",
                     value=18, 
                     min=1,
                     max=36,
                     step=1
                   ),
                  
                   numericInput(
                     inputId="act_e",
                     label="ACT English Score",
                     value=18, 
                     min=1,
                     max=36,
                     step=1
                   ), 
                   numericInput(
                     inputId="act_r",
                     label="ACT Writing Score",
                     value=6, 
                     min=2,
                     max=12,
                     step=1
                   )
                 ),
                 numericInput(
                   inputId="act_comp",
                   label="ACT Composite Score",
                   value=18,
                   min=1,
                   max=36,
                   step=1
                 ), 
                 
                 actionButton(
                   inputId="compare",
                   label="Show me how I compare"
                 )
               ), 
               
               mainPanel(
                 #leafletOutput(),
                 uiOutput("scores_plots"),
                 #textOutput("quantiles")
                 #uiOutput("mystats_output")
                 # want to create distributions of scores and add a line + percentile for input value
                 # add somewhere analysis of how good a match the user is for the school and how "likely" they are to 
                 #      be accepted (*make sure there is a note saying there is no guarantee)
                 
                 # ideally a map with the schools cooridinates and user's (if input)
               )
             ))
  )
)



#### Server ####
# server to run app
server <- function(input, output, session) {
  ##### School Search tab server #####
  # reactive for search button
  # design card layout here - card for each school
  # include a checkbox to add to school compare page 
  
  # filter schools based on criteria in input$region-select, etc
  filtered_schools <- reactive({
    filter_inputs <- list(region_select = input$region_select, 
                          state_select = input$state_select, 
                          urban_select = input$urban_select, 
                          city_select = input$city_select, 
                          school_name_search = input$school_name_search, 
                          size = input$size,
                          pub_priv = input$pub_priv, 
                          input$school_type, 
                          cost = input$cost, 
                          degree= input$degree)
    
    req(input$search) # only filter when search button is pressed
    filter_schools(institutions, filter_inputs)
  })
  
  # store schools for comparison
  compare_list <- reactiveValues(selected=character()) 
  
  # only show results when search button is pressed
  observeEvent(input$search, { 
    schools_data <- filtered_schools()
    n_results <- nrow(schools_data)
    
    output$school_search_output <- renderUI({
      if (nrow(schools_data)==0) {
        return(h4("No schools found matching your criteria."))
      }
      
      tagList(
        h3(paste(n_results, "Schools matching your criteria")), 
        
        fluidRow(
          lapply(1:nrow(schools_data), function(i) {
            school <- schools_data[i, ]
            checkbox_id <- paste0("compare_", i)
            
            # observe adds to comparison list
            observeEvent(input[[checkbox_id]], {
              if (isTRUE(input[[checkbox_id]])) {
                if (!(school$INSTNM %in% compare_list$selected)) {
                  compare_list$selected <- c(compare_list$selected, school$INSTNM)
                } else {
                  compare_list$selected <- (compare_list$selected[compare_list$selected != school$INSTM])
                }
              }
            })
            
            column(
              width=4, 
              card(
                # school name 
                card_header(h4(school$INSTNM)),
                
                # location 
                p(strong("Location: "), ifelse(is.na(school$CITY), "Unavailable", 
                                               paste0(school$CITY, ", ", 
                                                      states_map$LABEL[states_map$VALUE == school$ST_FIPS]))),
                p(strong("Address: "), ifelse(is.na(school$ADDR), 
                                              "Address unavailable", school$ADDR)), 
                
                # additional info 
                p(strong("Undergraduate Enrollment: "), school$UG), 
                p(strong("Type: "), 
                  type_pp_map$LABEL[type_pp_map$VALUE == school$CONTROL]), 
                p(strong("Acceptance Rate: "), 
                  ifelse(is.na(school$ADM_RATE), 
                         "Unavailable",  
                         paste0(round(school$ADM_RATE*100), "%"))),
                
                # test scores 
                p(strong("Median SAT (Math): "),
                  ifelse(is.na(school$SATMTMID), 
                         "Unavailable", 
                         school$SATMTMID)), 
                p(strong("Median SAT (Reading): "),
                  ifelse(is.na(school$SATVRMID), 
                         "Unavailable", 
                         school$SATVRMID)),
                p(strong("Median ACT Composite: "), 
                  ifelse(is.na(school$ACTCMMID), 
                         "Unavailable", 
                         school$ACTCMMID)),
                
                # graduation rate
                p(strong("Graduation Rate"), 
                  # look at different columns for 4yr vs 2yr 
                  ifelse(is.na(school$C150_4), 
                         paste0(round(school$C150_L4*100), "%"), 
                         paste0(round(school$C150_4*100), "%"))
                ),
                
                # cost
                p(strong("Tuition"), " (In-State): ", 
                  ifelse(is.na(school$TUITIONFEE_IN), 
                         "Unavailable", 
                         paste0("$", school$TUITIONFEE_IN, ".00"))), 
                p(strong("Tuition"), " (Out-of-State): ", 
                  ifelse(is.na(school$TUITIONFEE_OUT), 
                         "Unavailable", 
                         paste0("$", school$TUITIONFEE_OUT, ".00"))),
                
                # student-faculty ratio
                p(strong("Student-Faculty Ratio: "), 
                  ifelse(is.na(school$STUFACR), 
                         "Unavailable", 
                         round(school$STUFACR, 1))), 
                
                # gender split
                p(strong("Percent Female: "), 
                  ifelse((school$FEMALE=="PS" | is.na(school$FEMALE)),
                         "Unavailable", 
                         paste0(round(as.numeric(school$FEMALE) * 100), "%"))),
                
                # URL 
                p(a(href=school$INSTURL, target="_blank", "Visit School Website"), 
                  "(Opens in new window)"),
                
                # check box to add to comparison 
                checkboxInput(
                  inputId=checkbox_id,
                  label= "Add to Comparison",
                  value=FALSE)
              )
            )
          })
        )
      )
    })
  })
  
  # clear search criteria and results when clear button pressed
  observeEvent(input$clear, { 
    
    updateSelectInput(session, inputId="region_select", selected=character(0))
    updateSelectInput(session, "state_select", selected=character(0))
    updateCheckboxGroupInput(session, "urban_select", selected=character(0))
    updateTextInput(session, "city_select", value="")
    updateTextInput(session,"school_name_search", value="")
    updateSelectInput(session,"size", selected=character(0))
    updateSelectInput(session,"pub_priv", selected=character(0))
    updateSelectInput(session,"school_type", selected=character(0))
    updateSliderInput(session,"cost", value=50000)
    updateCheckboxGroupInput(session,"degree", selected=character(0))
    
    output$school_search_output <- renderUI({})
  })
  
  ##### School Compare tab server#####
  # schools_to_compare <- reactive({
  #   
  # })
  
  output$compare_output <- renderUI({
    if (length(compare_list$selected)==0){
      return(h3("No schools selected for comparison"))
    } 
    
    tagList(
      h3("Schools selected for comparison:"), 
      lapply(compare_list$selected, function(name) {
        p(name)
      })
    )
  })
  
  # clear search with button 
  observeEvent(input$reset_comparison, {
    compare_list$selected <- character()
  })
  
  # should have cards for each of those that were selected to compare on School Search page
  # reactive order by select (results should appear whether or not )
  # selected_schools <- reactive({
  #   selected <- sapply(1:nrow(filtered_schools), function(i) {
  #     input[[paste0("compare_",i)]]
  #   })
  #   
  #   filtered_schools[selected==TRUE, ]
  # })
  # 
  # sort_criteria <- reactive({
  #   sort_by <- input$sort
  # })
  # 
  # output$sort_output <- renderUI({
  #   schools_to_compare <- selected_schools() |>
  #     sort_by(sort_criteria)
  #   
  #   if (nrow(selected_schools==0)) {
  #     return("No schools to compare")
  #   }
  #   
  #   renderDataTable({
  #     schools_to_compare |>
  #       select(school_name, city, state, size, cost, acceptance_rate, 
  #              graduation_rate, median_income, url, percent_f, median_sat,
  #              median_act, median_gpa)
  #   })
  # })
  
  ##### User Compare tab server #####
  
  # update school choices in drop down as user types
  school_choices <- reactive({
    unique(institutions$INSTNM)
  })
  observe({
    updateSelectizeInput(session,
                         "school_name", 
                         choices=school_choices(), 
                         server=TRUE)
  })
  
  # store user scores for tests as they are input
  user_scores <- reactive({
    score_inputs <- list(
      SAT_Math = input$sat_m, 
      SAT_Reading = input$sat_vr, 
      SAT_Writing = input$sat_wr,
      ACT_Composite = input$act_comp,
      ACT_Math = input$act_m, 
      ACT_English = input$act_e, 
      ACT_Writing = input$act_r 
    )
    return(score_inputs)
  })
  
  # create graphs based on school name
  test_plots <- reactive({
    req(input$school_name)
    
    all_plots <- lapply(test_names, function(test){
      plotId <- paste0("plot_", test)
      user_score <- user_scores()[[test]]
      
      result <- plot_test_score(
        data=institutions, 
        school=input$school_name, 
        test=test, 
        #user_score=user_scores()[[test]]
        user_score=user_score
      )
      
      if (!is.null(result$plot)) {
        result_plot <- result$plot
        result_quant <- result$quantile
      } else {
        result_plot <- ggplot() + 
          labs(title=paste("No data available for", test))
        result_quant <- "No data available"
      }
       
      return(list(test=test, plotId=plotId, result_plot=result_plot, 
                  result_quant=result_quant, avg=result$avg, q25=result$q25,
                  q75=result$q75))
    })
    
    return(all_plots)
  })

  # only show graphs when button clicked 
  observeEvent(input$compare, {
    req(test_plots())
    results <- test_plots()
    
    # create unique output Id for each plot
    lapply(results, function(p){
      plot_id <- p$plotId
      plot <- p$result_plot
      output[[plot_id]] <- renderPlot({
        plot
      })
    })
    
    output$scores_plots <- renderUI({
      tagList(
        h3(input$school_name), 
        fluidRow(
            lapply(results, function(p){
              column(
                width=6, 
                card(
                plotOutput(outputId = p$plotId),
                # add info below each plot
                p(strong("Average", p$test, "Score:"), p$avg),
                p(strong("25th percentile:"), p$q25),
                p(strong("75th percentile:"), p$q75),
                p(ifelse(p$result_quant=="No data available", 
                         "", 
                         paste0("You likely scored in the ", p$result_quant, "th percentile")))
                )
              )
            })
          )
        )
      })
    })
  
}


# run app
shinyApp(ui, server)
