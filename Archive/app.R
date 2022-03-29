#-----------------------------------
# 
#-----------------------------------
library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(readxl)
# install.packages('rsconnect')

load('ma.rda')
load('poptot.rda')

# This begins the app
############### Server ####################
###########################################
server <- function(input, output) {

  #-----------------------------------------------------------------------
  # Tab 1: Population Characteristics
  #-----------------------------------------------------------------------
  output$plot1 <- renderPlot({
    
    # make plot of the population
    popsummary <- poptot %>%
      group_by(County.Size) %>%
      summarise(Current.65plus=sum(Total.65plus)/1000000,
                MA.Autoenrollees.10yrs = sum(Pop.eligible)/1000000) %>%
      gather(key=Pop.Type, value=Popsize, -County.Size)
    
    popsummary %>%
      ggplot(aes(x = County.Size, y = Popsize, fill=County.Size)) +
      geom_bar(position="dodge", stat="identity")+
      facet_wrap(~Pop.Type, scales = "free_y") +
      theme_bw()+
      labs(title= 'Current 65+ and future eligible auto enrollees (10 years)', 
           x = "County.Size", y = "Million")
    
  })
  #-------------------
  # Data Table
  #-------------------
  
  # Using DT package, we're making a data table
  output$table1 <- DT::renderDataTable(DT::datatable({
    # Choosing just a few columns to display
    data <- poptot %>%
      group_by(County.Size) %>%
      summarise(Current.65plus=sum(Total.65plus),
                MA.Autoenrollees.10yrs = round(sum(Pop.eligible)))
    data
  },
  options = list(searching = FALSE, pageLength = 5,scrollX = T)
  # lengthMenu = c(5, 10, 15, 20), 
  ))
  
  #-----------------------------------------------------------------------
  # Tab 2: Number of eligible MA plans based on given choices of 
  #-----------------------------------------------------------------------
  finaldf <- reactive({
    # input$rating c(2,2.5,3,3.5,4,4.5,5)
    # input$premium c("0", "Below 25pct", "Below Median","Below Mean", "No Restriction")
    # input$MOOP c("Minimum", "Below 25pct", "Below Median","Below Mean", "No Restriction")
    # input$plottype c("# MA plans", "# states w/o plans")
    # input$ratingrangemax c(2,2.5,3,3.5,4,4.5,5)
    
    # calculate the mean and median premium among these plans
    df_ma <- data.frame()
    df_cnty <- data.frame()
    for (max_rating in c(2, 2.5, 3, 3.5, 4, 4.5, 5)) {
      if (max_rating >= input$rating) {
        df = ma[ma$Overall.Star.Rating>=input$rating & ma$Overall.Star.Rating<=max_rating,]
        df <- df %>%
          group_by(FIPSCode) %>%
          mutate(Premium.mean = mean(PlanPremium),
                 Premium.median = median(PlanPremium),
                 Premium.25pct = quantile(PlanPremium, 0.25))
        
        if (input$premium=="0") {
          df = df[df$PlanPremium==0, ]
        } else if (input$premium=="Below Mean") {
          df = df[df$PlanPremium<=df$Premium.mean,]
        } else if (input$premium=="Below Median") {
          df = df[df$PlanPremium<=df$Premium.median,]
        } else if (input$premium=="Below 25percentile") {
          df = df[df$PlanPremium<=df$Premium.25pct,]
        }
        
        # calculate the MOOP percentiles to for filtering
        df <- df %>%
          group_by(FIPSCode) %>%
          mutate(MOOP.min = min(InNetworkMOOP),
                 MOOP.mean = mean(InNetworkMOOP),
                 MOOP.median = median(InNetworkMOOP),
                 MOOP.25pct = quantile(InNetworkMOOP, 0.25))
        
        if (input$MOOP=="Minimum") {
          df = df[df$InNetworkMOOP==df$MOOP.min,]
        } else if (input$MOOP=="Below Mean") {
          df = df[df$InNetworkMOOP<=df$MOOP.mean,]
        } else if (input$MOOP=="Below Median") {
          df = df[df$InNetworkMOOP<=df$MOOP.median,]
        } else if (input$MOOP=="Below 25percentile") {
          df = df[df$InNetworkMOOP<=df$MOOP.25pct,]
        }
        
        df <- df %>%
          right_join(poptot)
        
        # 1) Number of eligible MA plans
        agg <- df %>%
          group_by(County.Size, Plan.Type) %>%
          summarise(EligibleMA = n()) # this would gives us the number of eligible MA plans
        agg[is.na(agg$Plan.Type), 'Plan.Type'] = 'No Plans'
        
        # 2) Number of counties that with or without eligible MA plans
        df['CountyN'] = 1
        df['County.Type'] = "With.Plan"
        df[is.na(df$Plan.Type), 'County.Type'] = 'Without.Plan'
        
        cntyN <- df %>%
          select(c(County.Size, State, County, CountyN, County.Type)) %>%
          distinct() %>%
          group_by(County.Size, County.Type) %>%
          summarise(No.Counties = sum(CountyN))
        
        # colnames(cntyN) <- c('County.Size', 'States.without.Plans')
        agg['max.rating'] <- max_rating
        cntyN['max.rating'] <- max_rating
        
        if (max_rating==input$rating) {
          agg['rating.range'] <- paste0('rating: ', input$rating)
          cntyN['rating.range'] <- paste0('rating: ',input$rating)
        } else{
          agg['rating.range'] <- paste0('rating: ',input$rating,'-', max_rating)
          cntyN['rating.range'] <- paste0('rating: ',input$rating,'-', max_rating)
        }
        
        # Calculate the share of counties with or without plans
        cntyN <- cntyN %>% 
          group_by(County.Size) %>%
          mutate(Total.County = sum(No.Counties),
                 Share = round(No.Counties/Total.County*100))
        
        df_ma <- rbind(df_ma, agg[agg$Plan.Type != 'No Plans',])
        df_cnty <- rbind(df_cnty, cntyN)
      }
      
    }
    return (c(df_ma, df_cnty))
  })
  
  
  output$plot2_1 <- renderPlot({
    

    
    # df_ma$max.rating==5,
    if (input$plottype=='# MA plans') {
      p <- df_ma[df_ma$max.rating %in% input$ratingrange,] %>%
        ggplot(aes(x = Plan.Type, y = EligibleMA, fill=County.Size)) +
        geom_bar(position="dodge", stat="identity")+
        facet_wrap(~rating.range) +
        theme_bw()+
        theme(axis.text.x = element_text(angle = 25))
      labs(title= 'Eligible MA plans',
           x = "Year", y = "Percent")
    } else if (input$plottype=='# states w/o plans') {
      p <- df_cnty[df_cnty$max.rating %in% input$ratingrange,] %>%
        ggplot(aes(x = County.Size, y = No.Counties, fill=County.Type)) +
        geom_bar(position="dodge", stat="identity")+
        geom_text(aes(label = paste0(Share, '%')), size = 3,
                  position = position_dodge(width=0.9), vjust=-0.25) +
        facet_wrap(~rating.range) +
        theme_bw()+
        labs(title= 'Number of counties w/o eligible plans',
             x = "Year", y = "Percent")
    }
    p
  })
   
  output$checkbox2 <- renderUI({
      choice <-  seq(input$rating,5,0.5)
      # choice <- c(2,2.5,3,3.5,4,4.5,5)
      checkboxGroupInput("ratingrange", paste0("Maximum Rating"),
                         choices = choice, selected = choice)
      })
  
  output$text <- renderText({
    paste0("<f><font size = 4>",
           "<strong>Notes: </strong>",
           "Under each payment source, the population universe includes only people who have positive payment paid by that source"
           )
  })
      
}
############### UI ####################
#######################################
ui <- shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  # This applies preset aesthetic decisions to the entire app
  navbarPage(
    "MA Auto-Enrollment Project",
    # navbarPage lets us set up a navigation bar to toggle between tabPanels
    
    #-------------------------
    # Welcome Tab
    #-------------------------  
    tabPanel("Welcome", # tabPanel() creates the tabs
             tags$head(
               tags$style("h2 {color: #04B4AE; }
               h1 {color: #04B4AE}; }")), # this is setting the color palette for our tab headers 1 and 2
             headerPanel("About the App"), # I haven't created a title for this page, try adding one!
             h4("This App visualizes health insurance coverage and the health expenditures patterns. With the visualization,
                we can compare health insurance coverage and cost by different groups of people. We can look into more details of 
                the cost paid by different payment sources and for different service types."),
             # br(), #a break creates a space
             h2("How to Use This App"), # the number following h corresponds to size
             h4(tags$ul(
               tags$li("Health Insurance: Display the enrollment rate of different insurance and the uninsured rate by different characteristics"), #bullet point
               tags$li("Out-of-pocket Share: Present the out-of-pocket share for different health services by different characteristics"), #bullet point
               tags$li("Health Expenditure Distribution: Visualize the distribution by different characteristics of population as well as the corresponding spending for certain health service under different payment sources") #bullet point
             )),
             h4("To begin, select \"Health Insurance\" on the navigation bar. You will be asked to choose 
                the demographic or social characteristics and the corresponding groups that you are interested in, the plots will display the enrollment rate of 
                Medicare, Medicaid and Private Health Insurance, as well as the uninsurance rate over time."),
             h4("The second tab \"Out-of-pocket Share\" on the navigation bar will again allow you to choose the demographic or social characteristics
             and the corresponding groups, once you set your selection, the main panel will 
             present the out-of-pocket (OOP) share paid for different health services by the groups of people that you are interested in."),
             h4("In the third tab \"Health Expenditure Distribution\", other than selection of the characteristics and the corresponding groups, users can also
             select the year and health service type. The plots display the distribution of population by different characteristic groups, and the share of health spending 
                associated with each group. The parallel distributions are plotted for different payments sources simultaneously. Note that, for each payment source, the population 
                universe is the people who have positive payment made by that source."),
             h2("The Data"),
             h4("This app uses MEPS data. The data includes all different person-level characteristics, such as
                age, gender, race, marital status, education, employment status, poverty status and region. The data also includes genderal health insurance coverage information
                as well as the expenditures of different health services that paid by different payment sources. Health services include: Hospital inpatient, 
                Outpatient (including office-based medical providers), Emergency Room (ER), Dental, Prescription Drugs (Rx), Homehealth (either Agency or non-Agency), 
                Vision and other medical equipment. Payment source include Medicare, Medicaid, Private Health Insurance (including Tricare), Other Insurance, and Out-of-pocket (OOP)")),
    
    #-------------------------
    # Population Tab
    #-------------------------
    tabPanel("Population",
             tags$head(tags$style("h2 {color: #ee5500; } h1 {color: #04B4AE}; }")),
             headerPanel( "Plotting Population"),
             #create a 1st tab panel
             tabsetPanel(type = "tabs", 
                         #first panel shows regression table
                         tabPanel("Population Plot",
                                  plotOutput("plot1"),
                                  helpText("")),
                         tabPanel("Population Table",
                                  div(DT::dataTableOutput("table1"), style = "font-size: 100%; width: 75%"),
                                  helpText(""))
             )
    ),
    
    #-------------------------
    # MA plans tab
    #-------------------------
    
    tabPanel(
      # Second tab
      "MA Plans",
      headerPanel("Plotting Eligible MA plans"),
      # Side bar layout
      sidebarLayout(position = "left",
                    # Side panel
                    sidebarPanel(width = 2,
                                 selectInput("rating",
                                             "Minimum Rating:",
                                             c(2,2.5,3,3.5,4,4.5,5), 
                                             selected = (4)),
                                 selectInput("premium",
                                             "Premium choice:",
                                             c("0", "Below 25pct", 
                                               "Below Median","Below Mean", 
                                               "No Restriction"), 
                                             selected = ("0")),
                                 selectInput("MOOP",
                                             "OOP Maximum:",
                                             c("Minimum", "Below 25pct", 
                                               "Below Median","Below Mean", 
                                               "No Restriction"), 
                                             selected = ("Minimum")),
                                 selectInput("plottype",
                                             "What to plot",
                                             c("# MA plans", "# states w/o plans"), 
                                             selected = ("# MA plans")),
                                 uiOutput("checkbox2")
                                 # helpText("Red dots are Elementary school, Blue dots are Middle school or above")
                    ),
                    # Main panel
                    mainPanel(
                      # Create the plot.
                      plotOutput("plot2"),
                      htmlOutput('text')
                    ))
    )# 2nd Graph Tab
  )
))
shinyApp(ui = ui, server = server)
