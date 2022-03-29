#-----------------------------------
# MA auto-enrollment
#-----------------------------------
library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(readxl)
library(leaflet)
library(rgdal)
# install.packages('rsconnect')
# https://data.library.virginia.edu/data-scientist-as-cartographer-an-introduction-to-making-interactive-maps-in-r-with-leaflet/
# This begins the app
############### Server ####################
###########################################
server <- function(input, output) {
  
  # load some global data
  load('ma.rda')
  load('poptot.rda')
  poptot$Pop.eligible <- round(poptot$Pop.eligible)
  county_shape <- readOGR("cb_2019_us_county_20m")
  county_shape$FIPSCode <- as.numeric(county_shape$GEOID)

  #-----------------------------------------------------------------------
  # TAB 1: Population Characteristics
  #-----------------------------------------------------------------------
  #---------------------------------
  # Subtab 1: Population barchart
  #---------------------------------
  output$plot1 <- renderPlot({
    
    # make plot of the population (65+ and eligible pop) by county.size
    popsummary <- poptot %>%
      group_by(County.Size) %>%
      summarise(Current.65plus=sum(Total.65plus)/1000000,
                MA.Autoenrollees.10yrs = sum(Pop.eligible)/1000000) %>%
      gather(key=Pop.Type, value=Popsize, -County.Size)
    
    # plot
    popsummary %>%
      ggplot(aes(x = County.Size, y = Popsize, fill=County.Size)) +
      geom_bar(position="dodge", stat="identity")+
      facet_wrap(~Pop.Type, scales = "free_y") +
      theme_bw()+
      labs(title= 'Current 65+ and future eligible auto enrollees (10 years)', 
           x = "County.Size", y = "Million")
    
  })
  
  #---------------------------------
  # Subtab 2: Population summary table
  #---------------------------------
  # Using DT package, we're making a data table
  output$table1_1 <- DT::renderDataTable(DT::datatable({
    
    data <- poptot %>%
      group_by(County.Size) %>%
      summarise(Current.65plus=sum(Total.65plus),
                MA.Autoenrollees.10yrs = round(sum(Pop.eligible)))
    data
  },
  rownames = F,
  options = list(searching = FALSE, pageLength = 5, scrollX = T,
                 list(autoWidth = TRUE))
  ))
  
  #---------------------------------
  # Subtab 3: present detailed county level population data
  #--------------------------------- 
  output$table1_2 <- DT::renderDataTable(DT::datatable({
    # Choosing just a few columns to display
    data <- poptot[, c('State', 'County', 'FIPSCode', 'County.Size',
                       'TotalPop', "Total.65plus", "Pop.eligible")]
    data
  },
  rownames = F,
  options = list(searching = T, pageLength = 50, scrollX = T,
                 list(autoWidth = TRUE))
  ))
  
  #---------------------------------
  # Subtab 4: Map 1 to map county population
  #---------------------------------
  output$map1 <- renderLeaflet({
    
    # merge the counties data with the outcome table
    counties <- county_shape
    counties@data <- data.frame(counties@data, poptot[match(counties@data$FIPSCode, poptot$FIPSCode),])
    counties$var.interest = counties@data[,input$map1_var]/1000
    
    county_popup <- paste0("<strong>State: </strong>", 
                           counties@data$State,
                           "<br><strong>County: </strong>", 
                           counties@data$County,
                           "<br><strong>Fips Code: </strong>", 
                           counties@data$FIPSCode,
                           "<br><strong>County Size: </strong>",
                           counties@data$County.Size,
                           paste0("<br><strong>", input$map1_var, ": </strong>", 
                                  counties@data$var.interest))
    
    
    paletteNum <- colorQuantile('Blues', domain = counties$var.interest, n=10)
    # paletteNum <- colorNumeric(
    #   palette = colorRampPalette(c('lightblue', 'darkblue'))(length(counties$FIPSCode)), 
    #   domain = counties$var.interest)
    
    m <- leaflet() %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      setView(lng = -96.25, lat = 39.50, zoom = 3.5) %>%
      addPolygons(data = county_shape,
                  color = 'white',
                  weight = 1,
                  smoothFactor = .3,
                  fillOpacity = .75,
                  fillColor = ~paletteNum(counties$var.interest),
                  # label = ~stateLabels,
                  # labelOptions = labelOptions(
                  #   style = list(color = 'gray30'),
                  #   textsize = '10px'),
                  popup = county_popup,
                  highlightOptions = highlightOptions(
                    weight = 3,
                    color = 'dodgerblue'
                  )
      ) %>%
      addLegend(pal = paletteNum, values = counties$var.interest, 
                title = 'Level', position = 'bottomright')
    m
  })
  
  
  #-----------------------------------------------------------------------
  # TAB 2: Number of eligible MA plans based on given choices of rating/premium/MOOP
  #-----------------------------------------------------------------------
  
  ### First create a reactive data for future use in this tab
  finaldf <- reactive({
    # input$rating c(2,2.5,3,3.5,4,4.5,5)
    # input$premium c("0", "Below 25pct", "Below Median","Below Mean", "No Restriction")
    # input$MOOP c("Minimum", "Below 25pct", "Below Median","Below Mean", "No Restriction")
    # input$ratingrangemax c(2,2.5,3,3.5,4,4.5,5)
    
    # 1) calculate the number of eligible plans by different county size
    # 2) calculate the number of states with/without eligible plans by different county size
    df_ma <- data.frame() 
    df_cnty <- data.frame()
    # for each minimum rating choice, we present data under different maximum rating choice
    for (max_rating in c(2, 2.5, 3, 3.5, 4, 4.5, 5)) {
      if (max_rating >= input$rating) {
        # within certain rating range
        df = ma[ma$Overall.Star.Rating>=input$rating & ma$Overall.Star.Rating<=max_rating,]
        
        # calculate the mean/median/25pct among selected plans within each county
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
        } else if (input$premium=="Below 20") {
          df = df[df$PlanPremium<=20,]
        } else if (input$premium=="Below 50") {
          df = df[df$PlanPremium<=50,]
        } else if (input$premium=="Below 100") {
          df = df[df$PlanPremium<=100,]
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
        
        # merge with population data to get the county characteristics
        df <- df %>%
          right_join(poptot)
        df['rating.range'] = paste(input$rating, max_rating, sep='-')
        ###################################
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
          distinct() %>% # drop duplicates
          group_by(County.Size, County.Type) %>%
          summarise(No.Counties = sum(CountyN))
        
        # Calculate the share of counties with or without plans
        cntyN <- cntyN %>% 
          group_by(County.Size) %>%
          mutate(Total.County = sum(No.Counties),
                 Share = round(No.Counties/Total.County*100))
        
        # create variable indicating the rating range
        agg['max.rating'] <- max_rating
        cntyN['max.rating'] <- max_rating
        
        if (max_rating==input$rating) {
          agg['rating.range'] <- paste0('rating: ', input$rating)
          cntyN['rating.range'] <- paste0('rating: ',input$rating)
        } else{
          agg['rating.range'] <- paste0('rating: ',input$rating,'-', max_rating)
          cntyN['rating.range'] <- paste0('rating: ',input$rating,'-', max_rating)
        }
        
        # append the data 
        df_ma <- rbind(df_ma, agg)
        df_cnty <- rbind(df_cnty, cntyN)
        
      }
      
    }
    
    ##############################
    # prepare the data for mapping
    # 1) total eligible plans
    # 2) mean/median premium/MOOP/annual cost of eligible plans
    ## NOTE: Here the df is always the df when the rating range is min-max
    df['HavePlan'] <- 1
    df[is.na(df$Plan.Type), 'HavePlan'] = 0
    summary <- df %>%
      group_by(State, County, FIPSCode, rating.range) %>%
      summarise(eligible.plans.count = sum(HavePlan),
                mean.premium = round(mean(PlanPremium),2),
                median.premium = median(PlanPremium),
                mean.MOOP = round(mean(InNetworkMOOP),2),
                median.MOOP = median(InNetworkMOOP),
                mean.annualcost = round(mean(MA_annual_cost),2),
                median.annualcost = median(MA_annual_cost))
    
    # put all resulting data into a list for future reference (in server)
    df_return <- list('MA' = df_ma, 'county' = df_cnty, 'countysummary'=summary)
    return (df_return)
  })
  
  #---------------------------------
  # Subtab 1: Plot 1 to plot the eligible MA plans
  #---------------------------------
  output$plot2_1 <- renderPlot({
    
    df_ma <- finaldf()['MA'][[1]]
    df_ma <- df_ma[df_ma$Plan.Type != 'No Plans',]
  
    df_ma[df_ma$max.rating %in% input$ratingrange,] %>%
      ggplot(aes(x = Plan.Type, y = EligibleMA, fill=County.Size)) +
      geom_bar(position="dodge", stat="identity")+
      facet_wrap(~rating.range) +
      theme_bw()+
      theme(axis.text.x = element_text(angle = 25))+
      labs(title= 'Number of Eligible MA plans',
         x = "Plan.Type", y = "N")
  })
  
  #---------------------------------
  # Subtab 2: Plot 2 to plot the number of states with/without eligible MA plans
  #---------------------------------
  output$plot2_2 <- renderPlot({
    
    df_cnty <- finaldf()['county'][[1]]
    
    df_cnty[df_cnty$max.rating %in% input$ratingrange,] %>%
      ggplot(aes(x = County.Size, y = No.Counties, fill=County.Type)) +
      geom_bar(position="dodge", stat="identity")+
      geom_text(aes(label = paste0(Share, '%')), size = 3,
                position = position_dodge(width=0.9), vjust=-0.25) +
      facet_wrap(~rating.range) +
      theme_bw()+
      labs(title= 'Number of counties w/o eligible plans',
           x = "County.Size", y = "N")
  
  })
  
  #---------------------------------
  # Subtab 3: Table to present the MA plan summary data
  #---------------------------------
  # create the choice selection for the rating range
  output$table2_rating_choice <- renderUI({
    # create the values for the rating range
    r <- seq(input$rating,5,0.5)
    for (i in seq(1,length(r))) {
      if (r[i]>input$rating) {
        r[i] <- paste(input$rating,r[i],sep = '-')
      }
    }
    
    selectInput('rating.range', 'rating.range', append(c('All'), r))
  })

  output$table2_1 <- DT::renderDataTable(DT::datatable({
    
    # reshape from wide to long
    data <- finaldf()['MA'][[1]] %>%
      spread(County.Size, EligibleMA) %>%
      replace(is.na(.), 0) %>%
      select(-max.rating) %>%
      rowwise() %>% 
      mutate(Total = sum(`Bottom 25%`,`Middle 50%`, `Top 25%`,na.rm=TRUE))
    
    # filter choice based on the selection
    data$rating.range <- gsub("rating: ","",as.character(data$rating.range))
    if (input$PlanType != 'All') {
      data <- data %>%
        filter(Plan.Type==input$PlanType)
    }
    if (input$rating.range != 'All') {
      data <- data %>%
        filter(rating.range==input$rating.range)
    }
    data
  },
  rownames = F,
  options = list(searching = T, pageLength = 50,scrollX = T,
                 list(autoWidth = TRUE))
  
  ))
  
  #---------------------------------
  # Subtab 4: Mapping the detailed county data
  #---------------------------------
  ### MAP####
  output$map2_var_choice <- renderUI({
    # create the values for the rating range
    var <- c("eligible.plans.count", "mean.annualcost","median.annualcost", 
             "mean.MOOP","median.MOOP")
    if (input$premium != '0') {
      var <- append(var, c("mean.premium","median.premium"))
    }
    
    selectInput("map2_var","Cost/Premium/MOOP: ", var)
  })

  output$map2 <- renderLeaflet({
    
    dftouse = finaldf()['countysummary'][[1]]
    
    # merge the counties data with the outcome table
    counties <- county_shape
    counties@data <- data.frame(counties@data, dftouse[match(counties@data$FIPSCode, dftouse$FIPSCode),])

    # counties <- merge(county_shape, finaldf()['countysummary'][[1]], 
    #                   by = 'FIPSCode', all.x = F)
    counties$var.interest = counties@data[,input$map2_var]
    
    county_popup <- paste0("<strong>Fips Code: </strong>", 
                           counties@data$FIPSCode,
                           "<br><strong>State: </strong>", 
                           counties@data$State,
                           "<br><strong>County: </strong>", 
                           counties@data$County,
                           paste0("<br><strong>", input$map2_var, ": </strong>", 
                                  counties@data$var.interest))
    
    
    # paletteNum <- colorNumeric('Blues', domain = counties$median.annualcost)
    paletteNum <- colorNumeric(
      palette = colorRampPalette(c('lightblue', 'darkblue'))(length(counties$FIPSCode)), 
      domain = counties$var.interest)
    
    m <- leaflet() %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      setView(lng = -96.25, lat = 39.50, zoom = 3.5) %>%
      addPolygons(data = county_shape,
                  color = 'white',
                  weight = 1,
                  smoothFactor = .3,
                  fillOpacity = .75,
                  fillColor = ~paletteNum(counties$var.interest),
                  # label = ~stateLabels,
                  # labelOptions = labelOptions(
                  #   style = list(color = 'gray30'),
                  #   textsize = '10px'),
                  popup = county_popup,
                  highlightOptions = highlightOptions(
                    weight = 3,
                    color = 'dodgerblue'
                  )
      ) %>%
      addLegend(pal = paletteNum, values = counties$var.interest, 
                title = 'Level', position = 'bottomright')
    m
  })
  
  #---------------------------------
  # Subtab 5: Table to present detailed MA plan data county level
  #---------------------------------
  output$table2_2 <- DT::renderDataTable(DT::datatable({
    # Choosing just a few columns to display
    data <- finaldf()['countysummary'][[1]]
    data
  },
  rownames = F,
  options = list(searching = T, pageLength = 50,scrollX = T,
                 list(autoWidth = TRUE))
  ))
  
  ####################################
  # Other
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
  theme = shinytheme("yeti"),
  # This applies preset aesthetic decisions to the entire app
  navbarPage(
    "MA Auto-Enrollment",
    # navbarPage lets us set up a navigation bar to toggle between tabPanels
    
    #-------------------------
    # Welcome Tab
    #-------------------------  
    tabPanel("Welcome", # tabPanel() creates the tabs
             tags$head(tags$style("h2 {color: #b4044d; } h1 {color: #047cb4}; }")), # this is setting the color palette for our tab headers 1 and 2
             #headerPanel("About the App"), # I haven't created a title for this page, try adding one!
             h4("This App visualizes the county population and the county level eligible Medicare Advantage (MA) plans (for auto-enrollment) 
             given the illustrative policy choices. Visualization including table, bar charts and mapping "),
             # br(), #a break creates a space
             h2("How to Use This App"), # the number following h corresponds to size
             h4(tags$ul(
               tags$li(tags$b("County Population:"), "Display summary and county level population, including CURRENT total population and 65+ elderly, as well as potential 
                       MA eligible population in the FUTURE 10 years. Each county is categorized as 'Bottom 25%', 'Middle 50%' and 'Top 25%' based on
                       on the county population size."
                       ),
               tags$br(), # add a line break
               tags$li(tags$b("MA Plan selection:"), "Display summary and detailed county level MA plan data under different policy selections"))), #bullet point"),
             h4(""),
             h4(""),
             h2("The Data"),
             h4("ACS, CMS, PWBM Microsim")),
    
    #-------------------------
    # Population Tab
    #-------------------------
    tabPanel("County Population",
             tags$head(tags$style("h2 {color: #b4044d; } h1 {color: #047cb4}; }")),
             # headerPanel( "Plotting Population"),
             #create a 1st tab panel
             tabsetPanel(type = "tabs", 
                         #first panel shows regression table
                         tabPanel("Population Plot",
                                  plotOutput("plot1"),
                                  helpText("")),
                         tabPanel("Population Table",
                                  div(DT::dataTableOutput("table1_1"), style = "font-size: 100%; width: 75%"),
                                  helpText("")),
                         tabPanel("Population Mapping",
                                  selectInput("map1_var",
                                              "Population (Thousands):",
                                              c('TotalPop', 'Total.65plus', 'Pop.eligible')),
                                  leafletOutput("map1", width = "100%", height = "800px"),
                                  helpText("")),
                         tabPanel("Deatiled County Population",
                                  div(DT::dataTableOutput("table1_2"), style = "font-size: 100%; width: 75%"),
                                  helpText(""))
             )
    ),
    
    #-------------------------
    # MA plans tab
    #-------------------------
    tabPanel(
      # Second tab
      "MA Plan selection",
      # headerPanel("Plotting Eligible MA plans"),
      # Side bar layout
      sidebarLayout(position = "left",
                    # Side panel
                    sidebarPanel(width = 2,
                                 h2("Choices"),
                                 selectInput("rating",
                                             "Minimum Rating:",
                                             c(2,2.5,3,3.5,4,4.5,5), 
                                             selected = (4)),
                                 uiOutput("checkbox2"),
                                 helpText(""),
                                 selectInput("premium",
                                             "Premium choice:",
                                             c("0", "Below 25percentile", 
                                               "Below Median","Below Mean", 
                                               "No Restriction", 'Below 20', 
                                               'Below 50', 'Below 100'), 
                                             selected = ("0")),
                                 selectInput("MOOP",
                                             "OOP Maximum:",
                                             c("Minimum", "Below 25percentile", 
                                               "Below Median","Below Mean", 
                                               "No Restriction"), 
                                             selected = ("Minimum"))
                                 
                                 # helpText("Red dots are Elementary school, Blue dots are Middle school or above")
                    ),
                    # Main panel
                    mainPanel(
                      tabsetPanel(type = "tabs", 
                                  #first panel shows regression table
                                  tabPanel("MA Plans",
                                           plotOutput("plot2_1"),
                                           helpText("")),
                                  tabPanel("States w/o eligible plans",
                                           plotOutput("plot2_2"),
                                           helpText("")),
                                  tabPanel("Plan Table",
                                           
                                           # headerPanel("Examine the Data"),
                                           # Create a drop down
                                           div(style="display: inline-block;vertical-align:top; width: 150px;",
                                               selectInput(
                                             "PlanType",
                                             "Plan.Type:",
                                             c(
                                               "All",
                                               "HMO",
                                               "Local PPO",
                                               "MSA",
                                               "PFFS",
                                               "Regional PPO",
                                               "No Plans"
                                             )
                                           )),
                                           div(style="display: inline-block;vertical-align:top; width: 150px;",
                                               uiOutput("table2_rating_choice")),
                                           div(DT::dataTableOutput("table2_1"), style = "font-size: 100%; width: 100%")),
                                  
                                  tabPanel("County Mapping",
                                           uiOutput('map2_var_choice'),
                                           helpText('Note: This map displays characteristics of the eligible plans that 
                                               are ABOVE the selected minimum rating, and satisfy the choice of premium and MOOP.
                                                    Clicking the county will show its FIPS code and the corresponding statistics'),
                                           leafletOutput("map2", width = "100%", height = "800px"),
                                           helpText("")),
                                  tabPanel("Detailed County Data",
                                           div(DT::dataTableOutput("table2_2"), style = "font-size: 100%; width: 100%"),
                                           helpText(""))
                      )
                    ))
    )
  )
))
shinyApp(ui = ui, server = server)
