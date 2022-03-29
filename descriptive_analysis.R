library(readxl)
library(tidyverse)
library(ggplot2)

# reference: https://yanheupenn.shinyapps.io/healthfacts/

# type = 'All' # 
type = c('HMO', 'Local PPO', 'MSA', 'PFFS', 'Regional PPO')
rating = 2
premium ='0' #'No Restriction' # c("0", "Below Mean", "Below Median")
MOOP = "Minimum" # c("No Restriction", "Below Mean", "Below Median")

otherdtdir = "F:/.cache/Auto-enrollment-MA/"
setwd(otherdtdir)
pop =read.csv("F:/.cache/Auto-enrollment-MA/county_population.csv")
fips <- read_excel("F:/.cache/Auto-enrollment-MA/state_county_fips.xlsx")

workdir = "E:/Repositories/MA-auto-enrollment/"
setwd(workdir)

ma <- read.csv("3_Output/MA_Plan_Data.csv")

states <- read.csv("1_Input/states.csv")

fips <- fips %>%
  full_join(states, by=c("StateAbb" = "Abbreviation")) %>%
  filter(!is.na(FIPSCode))

ma <- ma %>%
  left_join(fips)

# pop is used to categorize the counties into different size groups
pop['FIPSCode'] = pop$State*1000+pop$County
# AgeGroup 0 means total population, 14+ indicates age 65+

# categorize
poptot = pop[pop$AgeGroup==0 & pop$Year==12,]
poptot = poptot %>%
  select(c(FIPSCode, TotalPop)) %>%
  inner_join(fips) %>%
  mutate(Pop.25pct = quantile(TotalPop, 0.25),
         Pop.75pct = quantile(TotalPop, 0.75)) 

poptot['County.Size'] = "Bottom 25%"
poptot[poptot$TotalPop>poptot$Pop.25pct & poptot$TotalPop<=poptot$Pop.75pct, 'County.Size'] = "Middle 50%"
poptot[poptot$TotalPop>poptot$Pop.75pct, 'County.Size'] = "Top 25%"

# calculate the total 65+ in each county
pop65 <- pop[pop$AgeGroup>=14 & pop$Year==12,]
pop65 <- pop65 %>%
  group_by(FIPSCode) %>%
  summarise(Total.65plus =sum(TotalPop))

# calculate the people entering the MA in the next 10 years
popelig <- read.csv("1_Input/county_eligible_population.csv")
popelig <- popelig %>%
  filter(Year<=2031) %>%
  group_by(FIPSCode) %>%
  summarise(Pop.eligible =sum(CountyPopulation))

poptot <- poptot %>%
  left_join(pop65) %>%
  left_join(popelig)

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

#######################
# calculate the mean and median premium among these plans
df_ma <- data.frame()
df_cnty <- data.frame()
for (max_rating in c(2, 2.5, 3, 3.5, 4, 4.5, 5)) {
  if (max_rating >= rating) {
    df = ma[ma$Overall.Star.Rating>=rating & ma$Overall.Star.Rating<=max_rating,]
    df <- df %>%
      group_by(FIPSCode) %>%
      mutate(Premium.mean = mean(PlanPremium),
             Premium.median = median(PlanPremium),
             Premium.25pct = quantile(PlanPremium, 0.25))
    
    if (premium=="0") {
      df = df[df$PlanPremium==0, ]
    } else if (premium=="Below Mean") {
      df = df[df$PlanPremium<=df$Premium.mean,]
    } else if (premium=="Below Median") {
      df = df[df$PlanPremium<=df$Premium.median,]
    } else if (premium=="Below 25percentile") {
      df = df[df$PlanPremium<=df$Premium.25pct,]
    }
    
    # calculate the MOOP percentiles to for filtering
    df <- df %>%
      group_by(FIPSCode) %>%
      mutate(MOOP.min = min(InNetworkMOOP),
             MOOP.mean = mean(InNetworkMOOP),
             MOOP.median = median(InNetworkMOOP),
             MOOP.25pct = quantile(InNetworkMOOP, 0.25))
    
    if (MOOP=="Minimum") {
      df = df[df$InNetworkMOOP==df$MOOP.min,]
    } else if (MOOP=="Below Mean") {
      df = df[df$InNetworkMOOP<=df$MOOP.mean,]
    } else if (MOOP=="Below Median") {
      df = df[df$InNetworkMOOP<=df$MOOP.median,]
    } else if (MOOP=="Below 25percentile") {
      df = df[df$InNetworkMOOP<=df$MOOP.25pct,]
    }
    
    df <- df %>%
      right_join(poptot)
    
    agg <- df %>%
      group_by(County.Size, Plan.Type) %>%
      summarise(EligibleMA = n()) # this would gives us the number of eligible MA plans
    agg[is.na(agg$Plan.Type), 'Plan.Type'] = 'No Plans'
    
    # data of counties that do not have eligible MA plans
    
    # 2) Number of counties that with or without eligible MA plans
    df['CountyN'] = 1
    df['County.Type'] = "With.Plan"
    df[is.na(df$Plan.Type), 'County.Type'] = 'Without.Plan'
    
    cntyN <- df %>%
      select(c(County.Size, State, County, CountyN, County.Type)) %>%
      distinct() %>%
      group_by(County.Size, County.Type) %>%
      summarise(No.Counties = sum(CountyN))
  }
  
  agg['max.rating'] <- max_rating
  cntyN['max.rating'] <- max_rating
  
  if (max_rating==rating) {
    agg['rating.range'] <- paste0('rating: ', rating)
    cntyN['rating.range'] <- paste0('rating: ',rating)
  } else{
    agg['rating.range'] <- paste0('rating: ',rating,'-', max_rating)
    cntyN['rating.range'] <- paste0('rating: ',rating,'-', max_rating)
  }
  
  df_ma <- rbind(df_ma, agg)
  df_cnty <- rbind(df_cnty, cntyN)
}

#df_ma$max.rating==5,
df_ma %>%
  ggplot(aes(x = Plan.Type, y = EligibleMA, fill=County.Size)) +
  geom_bar(position="dodge", stat="identity")+
  facet_wrap(~rating.range, scales = "free_y") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 25))
  labs(title= 'Medicare/Medicaid/PHI coverage Rate and Uninsurance Rate', 
       x = "Year", y = "Percent")

# df_cnty %>%
#   ggplot(aes(x = County.Size, y = States.without.Plans, fill=County.Size)) +
#   geom_bar(position="dodge", stat="identity")+
#   facet_wrap(~rating.range, scales = "free_y") +
#   theme_bw()+
#   labs(title= 'Medicare/Medicaid/PHI coverage Rate and Uninsurance Rate', 
#      x = "Year", y = "Percent")

  
# tables 
a=df_ma %>%
  spread(County.Size, EligibleMA) %>%
  replace(is.na(.), 0) %>%
  select(-max.rating) %>%
  rowwise() %>% 
  mutate(Total = sum(`Bottom 25%`,`Middle 50%`, `Top 25%`,na.rm=TRUE))
  
  # mutate(Total=`Bottom 25%`+`Middle 50%`+ `Top 25%`,na.rm=TRUE)

####################
# data for mapping

df['HavePlan'] <- 1
df[is.na(df$Plan.Type), 'HavePlan'] = 0
summary <- df %>%
  group_by(FIPSCode) %>%
  summarise(eligible.plans.count = sum(HavePlan),
            mean.premium = mean(PlanPremium),
            median.premium = median(PlanPremium),
            mean.MOOP = mean(InNetworkMOOP),
            median.MOOP = median(InNetworkMOOP),
            mean.annualcost = mean(MA_annual_cost),
            median.annualcost = median(MA_annual_cost))

# filter data for mapping
filteredData <- reactive({
  sch['var.interest'] <- sch[,input$map_var]
  place <- sch %>% 
    filter(School_level %in% input$sch_lvl) %>%
    group_by(SCHOOL_ZIP) %>%
    summarise(avg.map = mean(var.interest),
              White = round(mean(White),2),
              Black = round(mean(African_American),2),
              Asian = round(mean(Asian),2),
              Latino = round(mean(Latino),2),
              Other.Race = round(mean(Other),2),
              Pacific.Islander = round(mean(Pacific_Islander),2))
})

# output the map
################################
library(leaflet)
library(rgdal)
county_shape <- readOGR("E:/Repositories/MA-auto-enrollment/2_Code/descriptive_analysis/shinyApp/cb_2019_us_county_20m")
county_shape$FIPSCode = as.numeric(county_shape$GEOID)

counties <- merge(county_shape, summary, by = 'FIPSCode', all.x = F)

counties <- county_shape
counties@data <- data.frame(counties@data, poptot[match(counties@data$FIPSCode, poptot$FIPSCode),])



# counties['var.interest'] = round(counties$avg.map,2)

county_popup <- paste0("<strong>Fips Code: </strong>", 
                       counties$FIPSCode, 
                       "<br>",
                       "<br><strong>value: </strong>", 
                       paste0(counties@data['FIPSCode'], '%'))

# paletteNum <- colorNumeric('Blues', domain = counties$median.annualcost)
paletteNum <- colorNumeric(
  palette = colorRampPalette(c('lightblue', 'darkblue'))(length(counties$FIPSCode)), 
  domain = counties$median.annualcost)

m <- leaflet() %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
  setView(lng = -96.25, lat = 39.50, zoom = 3.5) %>%
  addPolygons(data = county_shape,
              color = 'white',
              weight = 1,
              smoothFactor = .3,
              fillOpacity = .75,
              fillColor = ~paletteNum(counties$median.annualcost),
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
  addLegend(pal = paletteNum, values = counties$median.annualcost, 
            title = 'test', position = 'bottomright')



