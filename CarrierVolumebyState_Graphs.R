##data is generated from proprietary database using mysql
##Raw data is all shipments by carrier 

##This code will run analysis on the data set and spit out a graph of each state 
#showing volume of shipments delivered to that state by carrier by month

##Load packages##
library("zoo")
library("lubridate")
library("dplyr")
library("data.table")
library("ggplot2")
library("plyr")
library("sqldf")

------------------
## set working directly appropriately
------------------

#read data appropriately (my proprietary example below:)
A_S14 <- read.csv("Aug13_Sept14.csv", header = TRUE)
J_D14 <- read.csv("Jul_Dec_14.csv", header = TRUE)
M_J15 <- read.csv("Mar_Jan_15.csv", header = TRUE)

#combine data
data <- rbind(A_S14, J_D14, M_J15)
#convert to table dataframe
dfdata <- tbl_df(data)
##to use on your own data, substitute column names appropriately as below
dfdata$carrier <-  tolower(dfdata$carrier)
dfdata$state <- toupper(dfdata$state)
dfdata$created_at <- mdy_hms(dfdata$created_at)
dfdata$created_at <- as.yearmon(dfdata$created_at)
dfdata$created_at <- as.Date(dfdata$created_at)
  
##subsets and summarized columns by state
plotdata <- dfdata %>%
  select(carrier, state, created_at) %>%
  filter(carrier ==  "fedex"|carrier == "newgistics"|carrier == "usps") %>%
  filter(state == 'AK'|state == 'AL'|state == 'AR'|state == 'AZ'|state == 'CA'|state == 'CO'|state == 'CT'|state == 'DE'|state == 'FL'|state == 'GA'|state == 'HI'|state == 'IA'|state == 'ID'|state == 'IL'|state == 'IN'|state == 'KS'| state =='KY'| state =='LA'|state == 'MA'|state == 'MD'| state =='ME'|state == 'MI'|state == 'MN'|state == 'MO'|state == 'MS'|state == 'MT'|state == 'NC'|state == 'ND'|state == 'NE'|state == 'NH'|state == 'NJ'|state == 'NM'|state == 'NV'|state == 'NY'|state == 'OH'|state == 'OK'|state == 'OR'|state == 'PA'|state == 'RI'|state == 'SC'|state == 'SD'|state == 'TN'|state == 'TX'|state == 'UT'|state == 'VA'|state == 'VT'|state == 'WA'|state == 'WI'|state == 'WV'|state == 'WY') %>%
  filter(created_at != "2015-03-01") %>%
  transform( count = 1) %>%
  arrange(created_at, state, carrier) %>%
  ddply(c("created_at", "state", "carrier"), summarise, tot = sum(count)) %>%
  mutate(percent = prop.table(tot)) %>%
  arrange(percent)


##Plot it
qplot(created_at, tot, data = plotdata, geom = "line", color = carrier, facets = ~ state)

---------------------------------------------------------------------------------
---------------------------------------------------------------------------------
##This code will group the shipment data by fedex region (population) and plot via choropleth map
  
##uses plotdata from above
##Filter data by FedEx region using states in each region
Western <- filter(plotdata, state =="MT"|state =="HI"|state =="AK"|state =="WA"| state =="OR"| state =="CA"| state == "ID"| state == "UT"| state == "NV"| state == "AZ"| state == "NM"| state == "WY"| state == "CO")
Southern <- filter(plotdata,state =="TX"| state =="OK"| state =="LA"| state =="AR"| state =="MS"| state =="AL"| state =="GA"| state =="FL"| state =="SC"| state =="NC"| state =="TN")
Central <- filter(plotdata,state == "MO"| state =="IA"| state =="MN"| state =="WI"| state =="IL"| state == "IN"| state =="MI"| state =="KY"| state =="VA"| state =="WV"| state =="OH"|state == "NE"| state == "KS"|state == "SD"| state == "ND")
Northeast <- filter(plotdata,state =="PA"| state =="MD"| state =="DE"| state =="NJ"| state =="CT"| state =="NY"| state =="NJ"| state =="RI"| state =="MA"| state =="NH"| state =="VT"| state =="ME")
## add region name to corresponding data
Western <- mutate(Western, region = "Western")
Southern <- mutate(Southern, region = "Southern")
Central <- mutate(Central, region = "Central")
Northeast <- mutate(Northeast, region = "Northeast")
## combine regions
plotregion <- rbind(Western, Southern, Central, Northeast)
## add count
plotregion <- mutate(plotregion, count = 1)
region <- select(plotregion, state, count, region)
## summarise over region
regsum <- ddply(region, c("region"), summarise, tot = sum(count))
regstate <- ddply(region, c("region", "state"), summarise, del = sum(count))
colnames(regsum) <- c("fdxregion", "value")
colnames(regstate) <- c("fdxregion", "abbr", "del")
comb <- sqldf("SELECT * FROM regstate LEFT JOIN regsum using (fdxregion)")

##Must make the region names match what data(state.map) contains (from choropleth.Maps Package)
#Load our csv of state names and abbreviations
stabb <- read.csv("State_Abbreviations.csv", header=FALSE)
colnames(stabb) <- c("region", "abbr")
comb <- sqldf("SELECT * FROM comb LEFT JOIN stabb using (abbr)")
comb <- select(comb, region, value)
comb$region <- tolower(comb$region)


##map the data
state_choropleth(comb, title = "Total shipments by state all time", buckets = 1)

---------------------------------------------------------------------------------