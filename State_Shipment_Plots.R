#carrier shipments by state
##packages 
library("zoo")
library("lubridate")
library("dplyr")
library("data.table")
library("ggplot2")


#read data
A_S14 <- read.csv("Aug13_Sept14.csv", header = TRUE)
J_D14 <- read.csv("Jul_Dec_14.csv", header = TRUE)
Mar_Jan_15 <- read.csv("Mar_Jan_15.csv", header = TRUE)
stabb <- read.csv("stabb.csv", header=FALSE)
colnames(stabb) <- c("name", "abbr")

#combine data
data <- rbind(A_S14, J_D14, Mar_Jan_15)
#convert to table dataframe and format
dfdata <- tbl_df(data)
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
  ddply(c("state"), summarise, tot = sum(count)) %>%
  mutate(percent = prop.table(tot)) %>%
  arrange(percent)



##Plot it
qplot(created_at, tot, data = plotdata, geom = "line", color = carrier, facets = ~ state)
