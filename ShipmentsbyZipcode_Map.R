## Choropleth zip code
setwd("~/Desktop/R Files/zipcode/zipdata")

library("maps")
library("dplyr")
library("zipcode")

# read data
x <- read.csv("zip1.csv", header=TRUE)
y <- read.csv("zip2.csv", header=TRUE)
z <- read.csv("zip3.csv", header=TRUE)
#combine table
zipdata <- rbind(x, y, z)
#turn into df table
zipdata <- tbl_df(zipdata)

#select relevant columns
##colnames(zipdata) for reference: [1] "id"  "created_at"  "created_at1" "carrier"     "service"    "country"     "state"       "address1"    "zip" 
dfzip <- select(zipdata, carrier, state, zip)
dfzip <- filter(dfzip, carrier == "Newgistics"|carrier == "FedEx"|carrier == "USPS")
dfzip$state <- toupper(dfzip$state)
dfzipclean <- filter(dfzip, state == 'AK'|state == 'AL'|state == 'AR'|state == 'AZ'|state == 'CA'|state == 'CO'|state == 'CT'|state == 'DE'|state == 'FL'|state == 'GA'|state == 'HI'|state == 'IA'|state == 'ID'|state == 'IL'|state == 'IN'|state == 'KS'| state =='KY'| state =='LA'|state == 'MA'|state == 'MD'| state =='ME'|state == 'MI'|state == 'MN'|state == 'MO'|state == 'MS'|state == 'MT'|state == 'NC'|state == 'ND'|state == 'NE'|state == 'NH'|state == 'NJ'|state == 'NM'|state == 'NV'|state == 'NY'|state == 'OH'|state == 'OK'|state == 'OR'|state == 'PA'|state == 'RI'|state == 'SC'|state == 'SD'|state == 'TN'|state == 'TX'|state == 'UT'|state == 'VA'|state == 'VT'|state == 'WA'|state == 'WI'|state == 'WV'|state == 'WY')
##enter carrier specific filter call here if desired
dfzipcleaner <- select(dfzipclean, zip, state)
dfzipcleaner$zip <- clean.zipcodes(dfzipcleaner$zip)
dfzipcleaner <- transform(dfzipcleaner, count = 1)
##to select carrier specific
#dfzipcleaner <- filter(dfzipclean, carrier == "Newgistics")
#dfzipcleaner <- filter(dfzipclean, carrier == "FedEx")
#dfzipcleaner <- filter(dfzipclean, carrier == "USPS")

zipcounts <- ddply(dfzipcleaner, c("zip"), summarise, value = sum(count))

# heat map code

zipcounts$zip <- clean.zipcodes(zipcounts$zip)
sqlzip <- sqldf("SELECT * FROM zipcounts LEFT JOIN zipcode using (zip)")
vallatlon <- select(sqlzip, value, latitude, longitude)

#Prepare Map
map(database='state',col="black", lwd=.5, resolution=.1, legend = "topright")
title = "Shipments by Zipcode -- Inception to March 1st 2015"

## Map Points
#cex value creates circles around lat lon of zipcode corresponding to the number of shipments sent there/100
points(vallatlon$longitude, vallatlon$latitude, pch=21, cex=vallatlon$value/100, col= "black")


## Rough Warehouse optimization - find the central point
## for all shipments:
class(sqldf)

y<- as.numeric(sqlzip$latitude)
x <- as.numeric(sqlzip$longitude)
y <- y[!is.na(y)]
x <- x[!is.na(x)]
y <- mean(y)
x <- mean(x)
points(x, y, cex = 2, pch = 19, col = "blue")
