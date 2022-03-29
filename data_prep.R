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

save(ma, file='2_Code/descriptive_analysis/ma.rda')
save(poptot, file='2_Code/descriptive_analysis/poptot.rda')

# ####################
# # data for mapping
# df['HavePlan'] = 1
# df[is.na(df$Plan.Type), 'HavePlan'] = 0 #TODO: see whether use 0 or NA
# df_map <- df %>%
#   group_by(State, County) %>%
#   summarise(EligibleMA = sum(HavePlan))
