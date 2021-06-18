install.packages(c("ggplot2"), dep=TRUE)
install.packages(c("corrplot"), dep=TRUE)
install.packages(c("mapdata"), dep=TRUE)
install.packages(c("oz"), dep=TRUE)
install.packages(c("usmap"), dep=TRUE)
install.packages(c("grid"), dep=TRUE)
install.packages(c("moments"), dep=TRUE)
install.packages(c("distr"), dep=TRUE)
install.packages(c("ggpubr"), dep=TRUE)
install.packages(c("Hmisc"), dep=TRUE)
install.packages(c("readxl"), dep=TRUE)
install.packages("readxl")
install.packages("stargazer")
install.packages("sp")
install.packages(c("rgdal"), dep=TRUE)
install.packages(c("socviz"), dep=TRUE)
install.packages(c("tigris"), dep=TRUE)
install.packages(c("dplyr"), dep=TRUE)
install.packages("ggmap")
install.packages("tidyverse")
install.packages(c("cowplot", "googleway", "ggrepel", "ggspatial", "libwgeom"), dep=TRUE)
install.packages("sf", dep=TRUE)
install.packages("rnaturalearth", dep=TRUE)
install.packages("rnaturalearthdata", dep=TRUE)
install.packages("nycflights13")
install.packages("fmsb")
install.packages("ROCR")
install.packages("reshape2")
install.packages(c("vcd"), dep=TRUE)
install.packages("gmodels")

library(ggplot2)
library(corrplot)
library(maps)
library(maptools)
library(mapdata)
library(oz)
library(usmap)
library(grid)
library(moments)
library(distr)
library(ggpubr)
library(Hmisc)
library(readxl)
library(stargazer)
library(sp)
library(rgdal)
library(socviz)
#library(tigris)
library(dplyr)
library(ggmap)
library(tidyverse)
library(sf)
library("rnaturalearth")
library("rnaturalearthdata")
library(tidyverse)
library(lubridate)
library(nycflights13)
library(fmsb)
library(car)
library(ROCR)
library(reshape2)
library(vcd)
library(gmodels)

# setwd
getwd()
setwd("/Users/sannul/Google Drive(sannul@hanyang.ac.kr)/Mathematics/Math Capstone PBL (Data Analysis)/Project 2")

# rm
rm(list = ls(all.names = TRUE))
gc()

### Raw data Handling ###
### DLDTE to year
funda0020 <- read.csv("funda0020.csv", na = c("", "NA", "N/A")) # import this
funda0020$dldte <- year(as.Date(as.character(funda0020$dldte), "%Y%m%d"))
table(subset(funda0020[!duplicated(funda0020[,c("gvkey")]),], dlrsn %in% c(2,3))$dldte)
20+27+34+20+24+19+53+125+81+50+118+88+93+87+103

# STALT variable from NA and TL to 0 and 1
table(funda0020$stalt)
t(subset(funda0020, stalt=="TO"))
funda0020$stalt[is.na(funda0020$stalt)] <- "0"
funda0020$stalt[funda0020$stalt=="TO"] <- "0"
funda0020$stalt[funda0020$stalt=="TL"] <- "1"
table(funda0020$stalt)
funda0020$stalt <- as.integer(funda0020$stalt)

# refined data
funda2010 <- read.csv("funda2010.csv", na = c("", "NA", "N/A"))
funda2010na <- read.csv("funda2010na.csv", na = c("", "NA", "N/A"))
funda2010_vif <- read.csv("funda2010_vif.csv", na = c("", "NA", "N/A"))
funda2000uscf <- read.csv("funda2000uscf.csv", na = c("", "NA", "N/A"))
funda2000all <- read.csv("funda2000all.csv", na = c("", "NA", "N/A"))
funda2010_ttest <- read.csv("funda2010_ttest.csv", na = c("", "NA", "N/A"))
funda2010_44 <- read.csv("funda2010_44.csv", na = c("", "NA", "N/A"))

# Data for Counties and Cities
uscities <- read.csv("uscities.csv", na = c("", "NA", "N/A"))

# Data save
write.csv(funda2010_ttest, 'funda2010_ttest.csv', row.names=F)
write.csv(funda2010, 'funda2010.csv', row.names=F)
write.csv(funda2000na, 'funda2000na.csv', row.names=F)
write.csv(county_full, 'county_full.csv', row.names=F)
write.csv(funda2000uscf, 'funda2000uscf.csv', row.names=F)

# Model 1 dataset
funda2010 <- subset(funda0020, fyear==2010)
rownames(funda2010) <- 1:dim(funda2010)[1]

# Names of bankruptcy or liquidation
subset(funda2000, stalt=="TL")[,"conml"]
subset(funda2001, stalt=="TL")[,"conml"]
subset(funda2002, stalt=="TL")[,"conml"]
subset(funda2003, stalt=="TL")[,"conml"]
subset(funda2004, stalt=="TL")[,"conml"]
subset(funda2003, conml=="Wheeling Pittsburgh Corp")[,"stalt"]

# view and indexing
View(funda)
summary(funda)
stargazer(funda)
str(funda)
colnames(funda)
unique(funda$aqi)
table(funda$loc)
funda[funda$loc=="KOR","conml"]
idcs<-which(funda$state=='NY')
funda$state[idcs]
funda[idcs,]

### Column deletion (later)
# NA count of all
table(is.na(funda2010))

# NA count Column wise
sapply(funda2010,function(x) sum(is.na(x)))
colSums(is.na(funda2010))

# NA count Row wise
rowSums((is.na(funda2010)))

### Row deletion
## Cut loc!="USA"
dim(subset(funda2010, loc!="USA"))
funda2010 <- subset(funda2010, loc=="USA")

## Cut is.na($state)
sum(is.na(funda2010$state))
funda2010[which(is.na(funda2010$state)),c("gvkey","conml","city","state","add1","dlrsn","dldte")]
funda2010 <- funda2010[-which(is.na(funda2010$state)),]

## Cut dldte<2010
dim(subset(funda2010, dldte<=2010))
funda2010 <- funda2010[-which(funda2010$dldte<=2010),]
rownames(funda2010) <- 1:dim(funda2010)[1]

### Table1 number of bankruptcy and liquidation companies
x1 <- as.data.frame(table(funda2010$dldte))
colnames(x1) <- c("year","All deletion")
x2 <- as.data.frame(table(subset(funda2010, dlrsn==2)$dldte))
colnames(x2) <- c("year","Bankruptcy")
x3 <- as.data.frame(table(subset(funda2010, dlrsn==3)$dldte))
colnames(x3) <- c("year","Liquidation")
x4 <- as.data.frame(table(subset(funda2010, dlrsn %in% c(2,3))$dldte))
colnames(x4) <- c("year","B + L")
table1 <- merge(merge(merge(x1, x2), x3, all.x=TRUE), x4)
table1$Liquidation[is.na(table1$Liquidation)] <- 0
stargazer(as.matrix(table1))
table(funda2010$dldte)
table(funda2010$dlrsn)
help(CrossTable)
CrossTable(funda2010$dldte, funda2010$dlrsn, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)[1]
stargazer(CrossTable(funda2010$dldte, funda2010$dlrsn, prop.r = FALSE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)[1])

### Adding county fips and census region by merging with uscities
uscities <- read.csv("uscities.csv", na = c("", "NA", "N/A"))
colnames(uscities)[c(3,8)] <- c("state","long")
#uscities$county_fips <- as.character(uscities$county_fips)
mfu <- merge(funda2010, uscities[,c("city","state","county_fips","county_name","long","lat","state_name")], all.x=TRUE)
duplcity <- mfu[duplicated(mfu[,c('city','state','gvkey')]),]
mfu[duplicated(mfu[,"gvkey"]),]
table(duplcity$state)
table(duplcity$city)


### check cities whose name and state name be overlaped
## https://www.whatcountyamiin.com/
subset(mfu, city=="Burbank")[,c("gvkey","city","state","county_fips","county_name","add1")]
subset(mfu, city=="Fairview")[,c("gvkey","city","state","county_fips","county_name","add1")]
subset(mfu, city=="Mesquite")[,c("gvkey","city","state","county_fips","county_name","add1")]
subset(mfu, city=="Milton")[,c("gvkey","city","state","county_fips","county_name","add1")]
subset(mfu, city=="Mountain View")[,c("gvkey","city","state","county_fips","county_name","add1")]
subset(mfu, city=="Newtown")[,c("gvkey","city","state","county_fips","county_name","add1")]
subset(mfu, city=="Pewaukee")[,c("gvkey","city","state","county_fips","county_name","add1")]
subset(mfu, city=="Plantation")[,c("gvkey","city","state","county_fips","county_name","add1")]
subset(mfu, city=="Rolling Hills")[,c("gvkey","city","state","county_fips","county_name","add1")]
subset(mfu, city=="Woodbury")[,c("gvkey","city","state","county_fips","county_name","add1")]

## exclude 31 companies from umm (7986 -> 7955)
funda2010 <- mfu[-c(1053,1055,2509,4112,4207,4381,4383,4385,4387,4389,4391,4393,4395,4397,4399,4401,
                        4403,4405,4407,4409,4411,5225,5227,5557,5702,5704,5706,5708,6072,7939,7941),]
rownames(funda2010) <- 1:dim(funda2010)[1]

### Adding census region by merging with county_data
sum(is.na(funda2010$state))
census_region <- county_data[!duplicated(county_data[,"state"]),c("state","census_region")]
funda2010 <- merge(funda2010, census_region, all.x=TRUE)
subset(funda2010, is.na(census_region))[,c("city","state","county_fips","county_name","state_name","census_region","dlrsn","dldte")]

# exclude 7 companies in PR (Puerto Rico) or GU (Guam)
funda2010 <- subset(funda2010, is.na(funda2010$census_region)==FALSE)
rownames(funda2010) <- 1:dim(funda2010)[1]
table(funda2010$census_region)

# reason for blank of county map
sum(is.na(funda2010$county_fips))
sum(is.na(funda2010$county_name))
subset(funda2010, is.na(funda2010$county_fips))[,c("gvkey","conml","city","state")]
length(table(funda2010$county_fips))
length(table(county_data$id))

### Dependent variable == BL in 2011~2013 ###
`%notin%` <- Negate(`%in%`)
funda2010$BL <- 0
funda2010[which((funda2010$dlrsn %in% c(2,3)) & (funda2010$dldte %in% c(2011,2012,2013))),"BL"] <- 1
dim(subset(funda2010, BL==1)[,c("dlrsn","dldte","BL")])
dim(subset(funda2010, BL==0)[,c("dlrsn","dldte","BL")])

## Not in operator
`%notin%` <- Negate(`%in%`)

### Column deletion
funda2010 <- funda2010[, apply(funda2010, 2, function(x) length(table(x))!=1)]
dim(funda2010[, apply(funda2010, 2, function(x) sum(is.na(x))<dim(funda2010)[1]*0.8)])
funda2010 <- funda2010[, apply(funda2010, 2, function(x) sum(is.na(x))<dim(funda2010)[1]*0.8)]

# Data Save to csv file funda2010
write.csv(funda2010, 'funda2010.csv', row.names=F)

# apply
dim(funda2010[, apply(funda2010, 2, function(x) !any(is.na(x)))])
dim(funda2010[, apply(funda2010, 1, function(x) !any(is.na(x)))])

# Proportion of BL
prop.table(table(funda2010$BL))

# table for variable names
stargazer(array(sort(colnames(funda2010)), dim=c(32,13)))

### --------------------------------------------------------------------------------------------###

#### Model 2 Data frame generating

# dataframe %>% group_by(factor_var) %>% sample_n(size) : random sampling by group 
Cars93[ , c("Manufacturer", "Model", "Origin")] %>% group_by(Origin) %>% sample_n(10)

funda0009[,c("gvkey","dlrsn","dldte")] %>% group_by(gvkey) %>% sample_n(1)

gg <- funda0009 %>% sample_n(10)
subset(funda0009, gvkey %in% gg$gvkey)[,c("gvkey","fyear","conml","dlrsn","dldte")]

gg2 <- subset(funda0009, gvkey %in% gg$gvkey) %>% group_by(gvkey) %>% sample_n(1)
gg2[,c("gvkey","fyear","conml","dlrsn","dldte")]

gg$gvkey

gg<-(funda0009[,c("gvkey","dlrsn","dldte")] %>% sample_n(10))$gvkey
"029894" %in% gg



### --------------------------------- Data Save completed --------------------------------------###


### --------------------------------- for US county map --------------------------------------- ###

### functions ###

# Multiplot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

### Define theme_map() function
# added it here vs use ggthemes since jlev14 was having issues with the pkg
theme_map <- function(base_size = 9, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
          axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(),
          panel.grid = element_blank(), panel.spacing = unit(0, "lines"), plot.background = element_blank(),
          legend.justification = c(0,0), legend.position = c(0, 0))
}

##############
### US map ###
##############

# US County map (non filled)
ggplot(data = county_map, mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "gray90", size = 0.05) + coord_equal() +
  theme_map()

### county_full ###
county_full <- merge(county_map, county_data, by = "id")
colnames(county_full)[1] <- "county_fips"
county_full$county_fips <- as.integer(county_full$county_fips) # because county_fips of uscities is integer

### example: population density per square mile
ggplot(data = county_full, mapping = aes(x = long, y = lat, fill = pop_dens, group = group)) +
  geom_polygon(color = "black", size = 0.05) + coord_equal() +
  scale_fill_brewer(palette="Blues", 
                    labels = c("0-10", "10-50", "50-100", "100-500", "500-1,000", "1,000-5,000", ">5,000")) +
  labs(fill = "Population per\nsquare mile") +
  theme_map() +
  guides(fill = guide_legend(nrow = 1)) + 
  theme(legend.position = "bottom")

### example: female percent
ggplot(data = county_full, mapping = aes(x = long, y = lat, fill = female, group = group)) +
  geom_polygon(color = "black", size = 0.05) + coord_equal() +
  #scale_fill_gradient(palette="Blues") +
  scale_fill_continuous(low = "white", high = "#CB454A") +
  labs(fill = "Female per square mile") +
  theme_map() +
  #guides(fill = guide_legend(nrow = 1)) + 
  theme(legend.position = "bottom")



### Color output
RColorBrewer::brewer.pal(n = 7, name = "Blues")

### Number of companies map by state and county ###

# State
statelength <- aggregate(gvkey~state, funda2010, length)
colnames(statelength) <- c("abbr", "number")
statelength <- merge(fips_info(), statelength, by="abbr", all.x=TRUE)
plot_usmap(data=statelength, values="number", color="#3182BD", size=0.1, labels=TRUE) + 
  scale_fill_continuous(low = "#EFF3FF", high="#4292C6", name = "count", label=scales::comma) + 
  theme(legend.position = "right")#, plot.title = element_text(size=30, hjust = 0.5), legend.title = element_text(size=15))
  
# County (continuous fill)
countylength <- aggregate(gvkey~county_fips, funda2010, length)
colnames(countylength) <- c("fips", "number")
plot_usmap(data=countylength, values="number", color="black", size=0.05, labels=TRUE) + 
  scale_fill_continuous(low = "#DEEBF7", high="#084594", name = "count", label=scales::comma) + 
  theme(legend.position = "right", plot.title = element_text(size=30, hjust = 0.5), legend.title = element_text(size=15))

### Factor levels
countylenz <- county_data[is.na(county_data$flipped)==FALSE,c("id","name","state","pop","female","white","flipped")]
colnames(countylenz)[1] <- "fips"
countylenz[,1] <- as.integer(countylenz[,1])
countylenz <- merge(countylenz, countylength, all.x=TRUE)
countylenz$number <- cut(countylenz$number, c(1,2,3,6,11,51,101,640), right=FALSE) # integer to factor
table(countylenz$number)

RColorBrewer::brewer.pal(n = 9, name = "Blues")

### County (factor level fill)
plot_usmap(data=countylenz, values="number", color="Black", size=0.13)+#, labels=TRUE) + 
  scale_fill_brewer(palette="Blues", 
                    labels = c("1", "2", "3-5", "6-10", "11-50", "51-100","101-600")) +
  labs(fill = "Count") +
  theme_map() +
  guides(fill = guide_legend(ncol = 1)) + 
  theme(legend.position = "right")

### Plotting cities on US map
citycoord <- usmap_transform(funda2010[is.na(funda2010$long)==FALSE,c("long","lat")])
plot_usmap(regions="state", color="Black", size=0.18) +
  geom_point(data = citycoord, aes(x = long.1, y = lat.1), size=1.5, alpha = 0.6, color="#2171B5") #+
  scale_size_area()
table(funda2010$city)

# Census region
table(census_region$census_region)
# Western States
plot_usmap(data=countylength, include=subset(census_region, census_region=="West")[,"state"], values="number", color="black", size=0.05, labels=TRUE) + 
  scale_fill_continuous(low = "#DEEBF7", high="#084594", name = "count", label=scales::comma) + 
  theme(legend.position = "right", plot.title = element_text(size=30, hjust = 0.5), legend.title = element_text(size=15)) +
  labs(title = "Western States")

# Northeastern States
plot_usmap(data=countylength, include=subset(census_region, census_region=="Northeast")[,"state"], values="number", color="black", size=0.05, labels=TRUE) + 
  scale_fill_continuous(low = "#DEEBF7", high="#084594", name = "count", label=scales::comma) + 
  theme(legend.position = "right", plot.title = element_text(size=30, hjust = 0.5), legend.title = element_text(size=15)) +
  labs(title = "Northeastern States")

# Southern States
plot_usmap(data=countylength, include=subset(census_region, census_region=="South")[,"state"], values="number", color="black", size=0.05, labels=TRUE) + 
  scale_fill_continuous(low = "#DEEBF7", high="#084594", name = "count", label=scales::comma) + 
  theme(legend.position = "right", plot.title = element_text(size=30, hjust = 0.5), legend.title = element_text(size=15)) +
  labs(title = "Southern States")

# Midwestern States
plot_usmap(data=countylength, include=subset(census_region, census_region=="Midwest")[,"state"], values="number", color="black", size=0.05, labels=TRUE) + 
  scale_fill_continuous(low = "#DEEBF7", high="#084594", name = "count", label=scales::comma) + 
  theme(legend.position = "right", plot.title = element_text(size=30, hjust = 0.5), legend.title = element_text(size=15)) +
  labs(title = "Midwestern States")

# California States
plot_usmap(data=countylength, include="CA", values="number", color="black", size=0.05) + 
  scale_fill_continuous(low = "#DEEBF7", high="#084594", name = "count", label=scales::comma) + 
  theme(legend.position = "right", plot.title = element_text(size=30, hjust = 0.5), legend.title = element_text(size=15)) +
  labs(title = "California States")

### ----------------------------------------------------------------------------------------- ###

### Median of continuous variables

### countymedian by county
countymedian <- aggregate(cbind(prcc_c, xacc)~county_fips, funda2000uscf, median, na.action=na.pass)
colnames(countymedian)[1] <- "fips"
countymedian$xacc <- cut(countymedian$xacc, 7)
countymedian$prcc_c <- cut(countymedian$prcc_c, 7)
plot_usmap(data=countymedian, values="prcc_c", color="Black", size=0.13) +#, labels=TRUE) + 
  scale_fill_brewer(palette="Blues") +
  labs(fill = "prcc_c") +
  theme_map() +
  guides(fill = guide_legend(ncol = 1)) + 
  theme(legend.position = "right")

### ----------------------------- Find state by longitude and latitude ---------------------------- ###

## Find state by longitude and latitude
lonlat_to_state_sp <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, states_sp)
  
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}

# Test the function using points in Wisconsin and Oregon.
testPoints <- data.frame(x = c(-90, -120), y = c(44, 44))

lonlat_to_state_sp(testPoints)

### ----------------------------------- Handling missing value ---------------------------------------- ###

## Numeric columns
colnames(funda2010)
colSums(is.na(funda2010))
str(funda2010)

funda2010$BL <- as.integer(funda2010$BL)
#dim(select_if(funda2010, is.numeric)) # include integer
funda2010cont <- funda2010[, sapply(funda2010, class) == "numeric"]
colSums(is.na(funda2010cont))

### SIC (Standard Industrial Classification)
sum(is.na(funda2010$sic))
table(funda2010$sic)

##########
### NAICS (North American Industrial Classification System)
sum(is.na(funda2010$naics))
colSums(is.na(subset(funda2010, is.na(funda2010$naics))))
table(funda2010$naics)
subset(funda2010, is.na(naics))[,c("gvkey","sic","gind","dlrsn")]
table(subset(funda2010, is.na(naics))$sic)

stargazer(as.matrix(data.frame(Sector, Description)))

Sector = c("11","21","22","23","31-33","42","44-45","48-49","51","52","53","54","55","56","61","62","71","72","81","92")
Description = c("Agriculture, Forestry, Fishing and Hunting","Mining, Quarrying, and Oil and Gas Extraction",
"Utilities","Construction","Manufacturing","Wholesale Trade","Retail Trade","Transportation and Warehousing",
"Information","Finance and Insurance","Real Estate and Rental and Leasing","Professional, Scientific, and Technical Services",
"Management of Companies and Enterprises","Administrative and Support and Waste Management and Remediation Services",
"Educational Services","Health Care and Social Assistance","Arts, Entertainment, and Recreation",
"Accommodation and Food Services","Other Services (except Public Administration)","Public Administration")

funda2010$naics
table(substr(funda2010$naics, 1,2))
subset(funda2010, substr(funda2010$naics,1,2)=="99")[,c("gvkey","state","dlrsn","sic","naics")]
table(subset(funda2010, substr(funda2010$naics,1,2)=="99")$sic)
table(subset(funda2010, substr(funda2010$naics,1,2)=="99")$naics)
subset(funda2010, substr(funda2010$naics,1,2)=="73")[,c("gvkey","state","dlrsn","sic","naics")]


### GICS (Global Industry Classification Standard)
## gind
sum(is.na(funda2010$gind))
colSums(is.na(subset(funda2010, is.na(funda2010$gind))))
table(funda2010$gind)
## gsubind
sum(is.na(funda2010$gsubind))
colSums(is.na(subset(funda2010, is.na(funda2010$gind))))
table(funda2010$gsubind)


##
table(funda2010$naics)
sum(is.na(funda2010cont)) #1013744

colnames(funda2010)[393] <- "naics6"
funda2010[,"naics5"] <- substr(funda2010$naics,1,5)
funda2010[,"naics4"] <- substr(funda2010$naics,1,4)
funda2010[,"naics3"] <- substr(funda2010$naics,1,3)
funda2010[,"naics2"] <- substr(funda2010$naics,1,2)
sum(is.na(funda2010))
## 6 digit mean
naics6_mean <- aggregate(.~funda2010$naics6, funda2010cont, mean, na.rm=TRUE, na.action=na.pass)
colnames(naics6_mean)[1] <- "naics6"
cont_xy6 <- funda2010 %>% left_join(naics6_mean, by="naics6")
write.csv(cont_xy6, 'cont_xy6.csv', row.names=F)
funda2010_naics6 <- read.csv("funda2010_naics6.csv", na = c("", "NA", "N/A"))
funda2010_naics6 <- funda2010_naics6[,-1]
sum(is.na(funda2010_naics6[,colnames(funda2010cont)])) # 1085687 / 267381 

## 5 digit mean
naics5_mean <- aggregate(.~funda2010$naics5, funda2010cont, mean, na.rm=TRUE, na.action=na.pass)
colnames(naics5_mean)[1] <- "naics5"
funda2010_naics6$naics5 <- as.character(funda2010_naics6$naics5)
cont_xy5 <- funda2010_naics6 %>% left_join(naics5_mean, by="naics5")
write.csv(cont_xy5, 'cont_xy5.csv', row.names=F)
funda2010_naics5 <- read.csv("funda2010_naics5.csv", na = c("", "NA", "N/A"))
funda2010_naics5 <- funda2010_naics5[,-1]
sum(is.na(funda2010_naics5[,colnames(funda2010cont)])) # 253864

## 4 digit mean
naics4_mean <- aggregate(.~funda2010$naics4, funda2010cont, mean, na.rm=TRUE, na.action=na.pass)
colnames(naics4_mean)[1] <- "naics4"
funda2010_naics5$naics4 <- as.character(funda2010_naics5$naics4)
cont_xy4 <- funda2010_naics5 %>% left_join(naics4_mean, by="naics4")
write.csv(cont_xy4, 'cont_xy4.csv', row.names=F)
funda2010_naics4 <- read.csv("funda2010_naics4.csv", na = c("", "NA", "N/A"))
funda2010_naics4 <- funda2010_naics4[,-1]
sum(is.na(funda2010_naics4[,colnames(funda2010cont)])) # 199897

## 3 digit mean
naics3_mean <- aggregate(.~funda2010$naics3, funda2010cont, mean, na.rm=TRUE, na.action=na.pass)
colnames(naics3_mean)[1] <- "naics3"
funda2010_naics4$naics3 <- as.character(funda2010_naics4$naics3)
cont_xy3 <- funda2010_naics4 %>% left_join(naics3_mean, by="naics3")
write.csv(cont_xy3, 'cont_xy3.csv', row.names=F)
funda2010_naics3 <- read.csv("funda2010_naics3.csv", na = c("", "NA", "N/A"))
funda2010_naics3 <- funda2010_naics3[,-1]
sum(is.na(funda2010_naics3[,colnames(funda2010cont)])) # 187878

## 2 digit mean
naics2_mean <- aggregate(.~funda2010$naics2, funda2010cont, mean, na.rm=TRUE, na.action=na.pass)
colnames(naics2_mean)[1] <- "naics2"
funda2010_naics3$naics2 <- as.character(funda2010_naics3$naics2)
cont_xy2 <- funda2010_naics3 %>% left_join(naics2_mean, by="naics2")
write.csv(cont_xy2, 'cont_xy2.csv', row.names=F)
funda2010_naics2 <- read.csv("funda2010_naics2.csv", na = c("", "NA", "N/A"))
funda2010_naics2 <- funda2010_naics2[,-1]
sum(is.na(funda2010_naics2[,colnames(funda2010cont)])) # 184688

### Now 560 companies NA handling with SIC code
## 4 digit mean
sic4_mean <- aggregate(.~funda2010$sic, funda2010cont, mean, na.rm=TRUE, na.action=na.pass)
colnames(sic4_mean)[1] <- "sic"
sic_xy4 <- funda2010_naics2 %>% left_join(sic4_mean, by="sic")
write.csv(sic_xy4, 'sic_xy4.csv', row.names=F)
funda2010_sic4 <- read.csv("funda2010_sic4.csv", na = c("", "NA", "N/A"))
funda2010_sic4 <- funda2010_sic4[,-1]
sum(is.na(funda2010_sic4[,colnames(funda2010cont)])) # 161845

table(funda2010$sic)

funda2010na <- funda2010_sic4

funda2010na$naics2 <- as.character(funda2010_naics2$naics2)
funda2010na$naics3 <- as.character(funda2010_naics2$naics3)
funda2010na$naics4 <- as.character(funda2010_naics2$naics4)
funda2010na$naics5 <- as.character(funda2010_naics2$naics5)
funda2010na$naics6 <- as.character(funda2010_naics2$naics6)

funda2010na <- funda2010na[apply(funda2010na, 1, function(x) sum(is.na(x))<dim(funda2010na)[2]*0.15),]
rownames(funda2010na) <- 1:dim(funda2010na)[1]

sum(is.na(funda2010na)) # 58409
sum(is.na(funda2010na[,colnames(funda2010cont)])) # 1622
colSums(is.na(funda2010na))

write.csv(funda2010na, "funda2010na.csv", row.names=F)

stargazer(array(sort(colnames(funda2010na)), dim=c(35,12)))
stargazer(funda2010na)

### --------------------------------------------- t-test ------------------------------------------------ ###

colnames(funda2010cont)
var.test(xacc~BL, funda2010na)
t.test(funda2010na$"xacc"~BL, funda2010na)

var.test(xacc~BL, funda2010na)$p.value
t.test(xacc~BL, funda2010na)$p.value

ttest_func <- function(colname){
  F_pvalue <- var.test(eval(as.symbol(colname))~BL, funda2010na)$p.value
  if (F_pvalue < 0.05){
    VE <- FALSE
  } else {
    VE <- TRUE
  }
  t_pvalue <- t.test(eval(as.symbol(colname))~BL, funda2010na, var.equal=VE)$p.value
  if (t_pvalue < 0.05){
    #print(paste0(colname,": select"))
    append = TRUE
  } else {
    #print(paste0(colname,": remove"))
    append = FALSE
  }
  return(append)
}

selection_list <- c()
remove_list <- c()
for (i in colnames(funda2010cont)) {
  if (ttest_func(i)) {
    selection_list <- c(selection_list, i)
  } else {
    remove_list <- c(remove_list, i)
  }
}

selection_list #245
remove_list # 105
selection_list %notin% sort(colnames(funda2010_ttest))

stargazer(array(sort(selection_list), dim=c(21,12)))
stargazer(array(sort(remove_list), dim=c(9,12)))


funda2010_ttest <- funda2010na[,selection_list] # set of selected continuous variables

funda2010na[,colnames(funda2010na) %notin% remove_list] # set of categorical and selected continuous variables


### ---------------------------------------------- VIF ------------------------------------------------ ###

# multicolleniarity check
VIF(lm(wcap ~ .,data=funda2010_ttest))
  
# Multi-collinearity check and remove the highly correlated variables step by step
# UDF of stepwise VIF function with preallocated vectors
# code source: https://beckmw.wordpress.com/2013/02/05/collinearity-and-stepwise-vif-selection/

vif_func <- function(in_frame,thresh=10, trace=F,...){
  require(fmsb)
  if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
  #get initial vif value for all comparisons of variables
  vif_init <- vector('list', length = ncol(in_frame))
  names(vif_init) <- names(in_frame)
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val,' ~ .'))
    vif_init[[val]] <- VIF(lm(form_in,data=in_frame,...))
  }
  vif_max<-max(unlist(vif_init))
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('', times = nrow(vif_init) ),quote=F)
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(names(in_frame))
  }
  else{
    in_dat<-in_frame
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      vif_vals <- vector('list', length = ncol(in_dat))
      names(vif_vals) <- names(in_dat)
      var_names <- names(in_dat)
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val,' ~ .'))
        vif_add <- VIF(lm(form_in,data=in_dat,...))
        vif_vals[[val]] <- vif_add
      }
      max_row <- which.max(vif_vals)
      #max_row <- which( as.vector(vif_vals) == max(as.vector(vif_vals)) )
      vif_max<-vif_vals[max_row]
      if(vif_max<thresh) break
      if(trace==T){ #print output of each iteration
        vif_vals <- do.call('rbind', vif_vals)
        vif_vals
        prmatrix(vif_vals,collab='vif',rowlab=row.names(vif_vals),quote=F)
        cat('\n')
        cat('removed: ', names(vif_max),unlist(vif_max),'\n\n')
        flush.console()
      }
      in_dat<-in_dat[,!names(in_dat) %in% names(vif_max)]
    }
    return(names(in_dat))
  }
}
sum(is.na(funda2010_ttest))
subset(funda2010_ttest, any(is.na(x)))
vif_func(funda2010_ttest, thresh=10, trace=T)

X_independent <- vif_func(funda2010_ttest, thresh=10, trace=T)

funda2010_ttest

dim(funda2010_ttest[apply(funda2010_ttest, 1, function(x) !any(is.na(x))),])

funda2010_ttestna <- funda2010_ttest[apply(funda2010_ttest, 1, function(x) !any(is.na(x))),]


write.csv(funda2010_ttestna, 'funda2010_ttestna.csv', row.names=F)
funda2010na[apply(funda2010_ttest, 1, function(x) any(is.na(x))),c("gvkey","city","state","BL")]

sum(is.na(funda2010_ttest))
sum(is.na(funda2010_ttest[apply(funda2010_ttest, 1, function(x) any(is.na(x))),]))


sum(is.na(funda2010na[apply(funda2010_ttest, 1, function(x) any(is.na(x))),]))
rowSums(is.na(funda2010na[apply(funda2010_ttest, 1, function(x) any(is.na(x))),]))

funda2010_vif <- read.csv("funda2010_vif.csv", na = c("", "NA", "N/A"))
funda2010_vif <- funda2010_vif[,-1]
vif_cont_list <- sort(colnames(funda2010_vif))

stargazer(array(sort(remove_list), dim=c(9,12)))
stargazer(array(sort(selection_list[selection_list %notin% vif_cont_list]), dim=c(13,12)))
stargazer(array(sort(vif_cont_list), dim=c(8,12)))
funda2010na$naics
subset(funda2010na, naics2==31)$conml
subset(funda2010, conml=="Kellogg Co")$naics
### ------------------------------------------- GVIF ---------------------------------------------- ###

colnames(funda2010_vif)
VIF(lm(txdbca ~ ., data=funda2010_vif))

for(val in var_names){
  regressors <- var_names[-which(var_names == val)]
  form <- paste(regressors, collapse = '+')
  form_in <- formula(paste(val,' ~ .'))
  vif_init[[val]] <- VIF(lm(form_in,data=in_frame,...))
}

remove_list2 <- colnames(funda2010cont)[colnames(funda2010cont) %notin% colnames(funda2010_vif)]
remove_list2 <- remove_list2[-260][-259]

dim(funda2010na)
dim(funda2010na[rownames(funda2010_ttestna),])

funda2010_156 <- funda2010na[rownames(funda2010_ttestna),colnames(funda2010na) %notin% remove_list2]
rownames(funda2010_156) <- 1:dim(funda2010_156)[1]

prop.table(table(funda2010_156$BL))

sum(is.na(funda2010_156))
colSums(is.na(funda2010_156))
funda2010_156$acctstd
sum(is.na(funda2010_156[,colnames(funda2010_156) %notin% colnames(funda2010cont)]))


table(funda2010_156$stko)

funda2010_130 <- funda2010_156[,apply(funda2010_156, 2, function(x) sum(is.na(x))<1029)]
funda2010_130 <- funda2010_130[,sort(colnames(funda2010_130))]
colSums(is.na(funda2010_130))

VIF(lm(wcap ~ ., data=funda2010_51[,colnames(funda2010_51)[colnames(funda2010_51) %in% colnames(funda2010_vif)]]))
vif(lm(wcap ~ ., data=funda2010_51[,colnames(funda2010_51)[colnames(funda2010_51) %in% colnames(funda2010_vif)]]))


### -------------------------------------- fitting logistic regression -------------------------------------- ###

write.csv(funda2010_156, 'funda2010_156.csv', row.names=F)
write.csv(funda2010_130, 'funda2010_130.csv', row.names=F)

manyzerocol <- colnames(funda2010[,sapply(funda2010, function(x) nrow(subset(funda2010, x==0)) + sum(is.na(x)) > 6000)])
meanlesscol <- manyzerocol[manyzerocol %in% colnames(funda2010_130)]
meanlesscol <- meanlesscol[-57][-56][-1]

table(subset(funda2010, is.na(naics2))$sic)

funda2010_76 <- funda2010_130[,colnames(funda2010_130) %notin% meanlesscol]
write.csv(funda2010_76, 'funda2010_76.csv', row.names=F)
table(funda2010_76$exchg)

#### Final 51 columns
funda2010_51 <- read.csv("funda2010_51.csv", na = c("", "NA", "N/A"))
funda2010_46 <- read.csv("funda2010_46.csv", na = c("", "NA", "N/A"))
sum(is.na(funda2010_51))
colSums(is.na(funda2010_51))
str(funda2010_51)

colnames(funda2010_51[, sapply(funda2010_51, class) == "numeric"]) # long and lat included
colnames(funda2010_51[, sapply(funda2010_51, class) == "integer"])
colnames(funda2010_51[, sapply(funda2010_51, class) == "character"])

colnames(funda2010_46[, sapply(funda2010_46, class) == "numeric"]) # long and lat included
colnames(funda2010_46[, sapply(funda2010_46, class) == "integer"])
colnames(funda2010_46[, sapply(funda2010_46, class) == "character"])

funda2010_46$BL <- as.factor(funda2010_46$BL)
funda2010_46$exchg <- as.factor(funda2010_46$exchg)
funda2010_46$naics2 <- as.factor(funda2010_46$naics2)
funda2010_46$stalt <- as.factor(funda2010_46$stalt)
funda2010_46$stko <- as.factor(funda2010_46$stko)
funda2010_46$census_region <- as.factor(funda2010_46$census_region)
funda2010_46$fic <- as.factor(funda2010_46$fic)
funda2010_46$idbflag <- as.factor(funda2010_46$idbflag)
funda2010_46$state <- as.factor(funda2010_46$state)

# Scale to numerical variables
funda2010_46[, sapply(funda2010_46, function(x) class(x) == "numeric")] <- scale(funda2010_46[, sapply(funda2010_46, function(x) class(x) == "numeric")])
num_list <- colnames(funda2010_46[, sapply(funda2010_46, function(x) class(x) == "numeric")])
num_list[37] <- "BL"
# data set with 12 input variable and Y variable
colnames(funda2010_45)
str(funda2010_46)
colSums(is.na(funda2010_46[,-39]))
subset(funda2010_46, is.na(exchg))

funda2010_45 <- funda2010_46[-3043,c(-39)] # stko col and exchg==NA row deletion
write.csv(funda2010_45, 'funda2010_45.csv', row.names=F)

funda2010_45 <- read.csv("funda2010_45.csv", na = c("", "NA", "N/A"))
colnames(funda2010_46)
funda2010_45$BL <- as.factor(funda2010_45$BL)
funda2010_45$exchg <- as.factor(funda2010_45$exchg)
funda2010_45$naics2 <- as.factor(funda2010_45$naics2)
funda2010_45$stalt <- as.factor(funda2010_45$stalt)
funda2010_45$census_region <- as.factor(funda2010_45$census_region)
funda2010_45$fic <- as.factor(funda2010_45$fic)
funda2010_45$idbflag <- as.factor(funda2010_45$idbflag)
funda2010_45$state <- as.factor(funda2010_45$state)
str(funda2010_45)
colSums(is.na(funda2010_45))

## training set vs test set
str(funda2010_44)
funda2010_44 <- funda2010_46[,c(-14,-39)] # exchg, stko cols deletion
write.csv(funda2010_44, 'funda2010_44.csv', row.names=F)
funda2010_44 <- read.csv("funda2010_44.csv", na = c("", "NA", "N/A"))

str(funda2010_44)
funda2010_44$BL <- as.factor(funda2010_44$BL)
funda2010_44$census_region <- as.factor(funda2010_44$census_region)
funda2010_44$fic <- as.factor(funda2010_44$fic)
funda2010_44$idbflag <- as.factor(funda2010_44$idbflag)
funda2010_44$naics2 <- as.factor(funda2010_44$naics2)
funda2010_44$stalt <- as.factor(funda2010_44$stalt)
funda2010_44$state <- as.factor(funda2010_44$state)

colnames(funda2010_44)
fitting_df <- funda2010_44[,-18]
str(fitting_df)
table(fitting_df$BL)

# split train(0.8) and test set(0.2)
set.seed(123) # for reproducibility
train <- as.integer(c(sample(rownames(subset(fitting_df, BL==1)), size=60, replace=F), sample(rownames(subset(fitting_df, BL==0)), size=600, replace=F)))
test <- as.integer(c(rownames(subset(fitting_df[-train,], BL==1)), sample(rownames(subset(fitting_df[-train,], BL==0)), size=160, replace=F)))


dim(fitting_df[train,])
dim(fitting_df[test,])

table(fitting_df$BL)
table(fitting_df[train,]$BL)
table(fitting_df[test,]$BL)
prop.table(table(fitting_df[train,]$BL))
prop.table(table(fitting_df[test,]$BL))

# train with training set
## Stepsiwse
glm.fit <- glm(BL ~ ., data = fitting_df[train,], family = binomial(link = "logit"))
step_glm <- step(glm.fit, direction = "both")
summary(glm.fit)

# selected variables
glm2.fit <- glm(BL ~ aco + aqpl1 + caps + csho + cstk + glcea + idbflag + optfvgr + spced + stalt + stkcpa,
               data = fitting_df[train,], family = binomial(link = "logit"))
step_glm2 <- step(glm2.fit, direction = "both")
summary(glm2.fit)

## table
coefficients(summary(glm2.fit))
round(coefficients(summary(glm2.fit)), 2)
stargazer(round(coefficients(summary(glm2.fit)), 3))

stepwise_detail <- read.csv("stepwise_detail.csv", na = c("", "NA", "N/A"))
stargazer(as.matrix(stepwise_detail))

str(fitting_df)
stargazer(array(sort(colnames(fitting_df)), dim=c(11,4)))

# likelihood ratio test
LR_statistic = 402.12 - 311.41
LR_statistic
pchisq(LR_statistic, df = (659-648), lower.tail = F)
summary(glm2.fit)

### ------------------------------------ test set ------------------------------------ ###
# calculate the probabilities on test set
glm.probs <- predict(glm2.fit, fitting_df[test,], type="response")

names(glm.probs)
# compute the predictions with the threshold of 0.5
glm.pred <- rep(0, nrow(fitting_df[test,]))
names(glm.pred) <- names(glm.probs)
glm.pred[glm.probs > .1267] = 1
table(glm.pred)

names(which(glm.pred == 1))
fitting_df[names(which(glm.pred == 1)),"BL"]
fitting_df[names(which(glm.pred == 0)),"BL"]

table(fitting_df[names(which(glm.pred == 1)),"BL"])
table(fitting_df[names(which(glm.pred == 0)),"BL"])

table(glm.pred[rownames(subset(fitting_df[test,], BL==1))])
table(glm.pred[rownames(subset(fitting_df[test,], BL==0))])

table(fitting_df[test,]$BL)
# Make predictions
probabilities <- glm2.fit %>% predict(fitting_df[test,], type = "response")
predicted.classes <- ifelse(probabilities > 0.1267, "1", "0")

# Model accuracy
mean(predicted.classes == fitting_df$BL)

# ROC curve
pr <- prediction(glm.probs, fitting_df$BL[test])
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf, main="ROC Curve")

# AUC (The Area Under an ROC Curve)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
detach(wdbc)



install.packages(c("Epi"), dep=TRUE)
install.packages(c("pROC"), dep=TRUE)
install.packages(c("ztable"), dep=TRUE)
install.packages(c("moonBook"), dep=TRUE)
library(Epi)
library(pROC)
library(ztable)
library(moonBook)


require(Epi)
require(pROC)
require(ztable)
require(moonBook)
source("ROC_sub.R")
a1 <- ROC(form=BL ~ aco + aqpl1 + caps + csho + cstk + glcea + idbflag + optfvgr + spced + stalt + stkcpa,
          data=samt, plot="ROC")

samt <- fitting_df[test,]
samt$stalt[1] <- 1
str(fitting_df[test,])
str(fitting_df[train,])

table(fitting_df[train,]$stalt)
table(fitting_df[test,]$stalt)
table(fitting_df[train,]$idbflag)
table(fitting_df[test,]$idbflag)

table(fitting_df$fic)
colnames(fitting_df[test,])

#----------------------------------------------------------------------------------------------------------------

train <- as.integer(c(sample(rownames(subset(fitting_df, BL==1)), size=60, replace=F), sample(rownames(subset(fitting_df, BL==0)), size=600, replace=F)))
test <- as.integer(c(rownames(subset(fitting_df[-train,], BL==1)), sample(rownames(subset(fitting_df[-train,], BL==0)), size=160, replace=F)))

write.csv(fitting_df[train,],'fitting_train.csv',row.names=F)
write.csv(fitting_df[test,],'fitting_test.csv',row.names=F)


#--------------------------------------------------------------------------------------------------------------

##---test with training set
# calculate the probabilities on test set
glm.probs <- predict(glm2.fit, fitting_df[train,], type="response")
glm.probs[1:20]

# compute the predictions with the threshold of 0.5
glm.pred <- rep(0, nrow(fitting_df[train,]))
glm.pred[glm.probs > .012] = 1
table(glm.pred)
fitting_df[which(glm.pred == 1),"BL"]
table(fitting_df[which(glm.pred == 1),"BL"])

table(fitting_df[train,]$BL)

# Make predictions
probabilities <- glm2.fit %>% predict(fitting_df[train,], type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "1", "0")

# Model accuracy
mean(predicted.classes == fitting_df$BL)
##---

library(tidyverse)
library(caret)
theme_set(theme_bw())


### ------------------------------------ scatter plot between cont. variables ------------------------------------ ###

colnames(funda2010_51)
VIF(lm(wcap ~ ., data=funda2010_51[,colnames(funda2010_51)[colnames(funda2010_51) %in% colnames(funda2010_vif)]]))
vif(lm(wcap ~ ., data=funda2010_51[,colnames(funda2010_51)[colnames(funda2010_51) %in% colnames(funda2010_vif)]]))

# scatter plot of x=concave.points_mean, y=area_worst
ggplot(data=funda2010_51, aes(x=optfvgr, y=stkcpa, colour=as.factor(BL), alpha=0.5)) +
  geom_point(shape=19, size=3) +
  #coord_cartesian(xlim = c(0,50), ylim = c(0,5000)) +
  ggtitle("Scatter Plot of fatn & dcs by BL")

### ------------------------------------ histogram ------------------------------------ ###

hist(fitting_df$aco)

(14+117)/176
14/16
117/160
14/57




ggplot(fitting_df, aes(x=aco, y=..density..)) +
  #geom_histogram(binwidth=800, fill = "orange", colour = "black") +
  geom_density(fill = "orange", colour=NA, alpha=.5) +
  geom_histogram(binwidth=1, fill = "orange", colour = "black", alpha=.7) +
  geom_line(stat="density") +
  expand_limits(y=0) +
  labs(x="aco", y="Density")

ggplot(fitting_df, aes(x=aco)) +
  geom_histogram(binwidth = 1, fill = "#4292C6", colour = "black", alpha=.7) +
  labs(x="aco", y="Count")

h1 <- ggplot(fitting_df[-as.integer(rownames(subset(fitting_df, (aco > 0.2)|(aco < -0.2)))),], aes(x=aco)) +
  geom_histogram(binwidth = 0.003, fill = "#4292C6", colour = "black", alpha=.7) +
  labs(x="aco", y=NULL) +
  theme_minimal(base_size = 13)
length(rownames(subset(fitting_df, (aco > 0.2)|(aco < -0.2))))

h2 <- ggplot(fitting_df[-as.integer(rownames(subset(fitting_df, (aqpl1 > 0.05)|(aqpl1 < -0.5)))),], aes(x=aqpl1)) +
  geom_histogram(binwidth = 0.002, fill = "#4292C6", colour = "black", alpha=.7) +
  labs(x="aqpl1", y=NULL) +
  theme_minimal(base_size = 13)
length(rownames(subset(fitting_df, (aqpl1 > 0.05)|(aqpl1 < -0.5))))

h3 <- ggplot(fitting_df[-as.integer(rownames(subset(fitting_df, (caps > 0.25)|(caps < -0.25)))),], aes(x=caps)) +
  geom_histogram(binwidth = 0.005, fill = "#4292C6", colour = "black", alpha=.7) +
  labs(x="caps", y=NULL) +
  theme_minimal(base_size = 13)
length(rownames(subset(fitting_df, (caps > 0.25)|(caps < -0.25))))

h4 <- ggplot(fitting_df[-as.integer(rownames(subset(fitting_df, (csho > 0.5)|(csho < -0.5)))),], aes(x=csho)) +
  geom_histogram(binwidth = 0.006, fill = "#4292C6", colour = "black", alpha=.7) +
  labs(x="csho", y=NULL) +
  theme_minimal(base_size = 13)
length(rownames(subset(fitting_df, (csho > 0.5)|(csho < -0.5))))

h5 <- ggplot(fitting_df[-as.integer(rownames(subset(fitting_df, (cstk > 0.2)|(cstk < -0.5)))),], aes(x=cstk)) +
  geom_histogram(binwidth = 0.003, fill = "#4292C6", colour = "black", alpha=.7) +
  labs(x="cstk", y=NULL) +
  theme_minimal(base_size = 13)
length(rownames(subset(fitting_df, (cstk > 0.2)|(cstk < -0.3))))

h6 <- ggplot(fitting_df[-as.integer(rownames(subset(fitting_df, (glcea > 0.3)|(glcea < -0.3)))),], aes(x=glcea)) +
  geom_histogram(binwidth = 0.005, fill = "#4292C6", colour = "black", alpha=.7) +
  labs(x="glcea", y=NULL) +
  theme_minimal(base_size = 13)
length(rownames(subset(fitting_df, (glcea > 0.3)|(glcea < -0.3))))

h7 <-ggplot(fitting_df[-as.integer(rownames(subset(fitting_df, (optfvgr > 0.1)|(optfvgr < -0.1)))),], aes(x=optfvgr)) +
  geom_histogram(binwidth = 0.0015, fill = "#4292C6", colour = "black", alpha=.7) +
  labs(x="optfvgr", y=NULL) +
  theme_minimal(base_size = 13)
length(rownames(subset(fitting_df, (optfvgr > 0.1)|(optfvgr < -0.1))))

h8 <- ggplot(fitting_df[-as.integer(rownames(subset(fitting_df, (spced > 0.3)|(spced < -0.25)  ))),], aes(x=spced)) +
  geom_histogram(binwidth = 0.005, fill = "#4292C6", colour = "black", alpha=.7) +
  labs(x="spced", y=NULL) +
  theme_minimal(base_size = 13)
length(rownames(subset(fitting_df, (spced > 0.3)|(spced < -0.25) )))

h9  <- ggplot(fitting_df[-as.integer(rownames(subset(fitting_df, (stkcpa > 0.5)|(stkcpa < -0.3)))),], aes(x=stkcpa)) +
  geom_histogram(binwidth = 0.006, fill = "#4292C6", colour = "black", alpha=.7) +
  labs(x="stkcpa", y=NULL) +
  theme_minimal(base_size = 13)
length(rownames(subset(fitting_df, (stkcpa > 0.5)|(stkcpa < -0.3) )))

multiplot(h1, h2, h3, h4, h5, h6, h7, h8, h9, cols=3)

stargazer(fitting_df[,c("aco","aqpl1","caps","csho","cstk","glcea","optfvgr","spced","stkcpa")])
stargazer(fitting_df[,c("BL","idbflag","stalt")])

fitting_df[, sapply(fitting_df, class)=="numeric"]
final_model_list <- c("aco","aqpl1","BL","caps","csho","cstk","glcea","idbflag","optfvgr","spced","stalt","stkcpa")

## Box plot
fitting_df_melt <- melt(fitting_df[,c("BL","aco","aqpl1","caps","csho","cstk","glcea","optfvgr","spced","stkcpa")], id.var = "BL")
ggplot(data = fitting_df_melt[(fitting_df_melt$value < 0.5)&(fitting_df_melt$value > -0.5),], aes(x=variable, y=value), fill=type) + 
  geom_boxplot(aes(fill=as.factor(BL))) +
  theme_minimal(base_size = 20) +
  coord_flip() +
  labs(x=NULL, y=NULL, fill="BL") +
  theme(legend.position = "bottom") +
  scale_fill_manual(values=c("#6BAED6", "#D6936B"))

stalt_melt <- melt(fitting_df[,c("stalt","aco","aqpl1","caps","csho","cstk","glcea","optfvgr","spced","stkcpa")], id.var = "stalt")
ggplot(data = stalt_melt[(stalt_melt$value < 0.5)&(stalt_melt$value > -0.5),], aes(x=variable, y=value), fill=type) + 
  geom_boxplot(aes(fill=as.factor(stalt))) +
  theme_minimal(base_size = 20) +
  coord_flip() +
  labs(x=NULL, y=NULL, fill="Status Alert") +
  theme(legend.position = "bottom") +
  scale_fill_manual(values=c("#6BAED6", "#D6936B"))

idbflag_melt <- melt(fitting_df[,c("idbflag","aco","aqpl1","caps","csho","cstk","glcea","optfvgr","spced","stkcpa")], id.var = "idbflag")
ggplot(data = idbflag_melt[(idbflag_melt$value < 0.5)&(idbflag_melt$value > -0.5),], aes(x=variable, y=value), fill=type) + 
  geom_boxplot(aes(fill=as.factor(idbflag))) +
  theme_minimal(base_size = 20) +
  coord_flip() +
  labs(x=NULL, y=NULL, fill="International, Domestic, Both Indicator") +
  theme(legend.position = "bottom") +
  scale_fill_manual(values=c("#6BAED6", "#D6936B"))


### Color output
RColorBrewer::brewer.pal(n = 9, name = "Blues")
RColorBrewer::brewer.pal(n = 9, name = "Reds")
install.packages("colortools")
library(colortools)
wheel("darkblue", num = 12)
analogous("darkblue")
analogous("#69b3a2")
analogous("#4292C6")
complementary("steelblue")
complementary("#9ECAE1")
complementary("#6BAED6")
complementary("darkslategrey")

complementary("#3182BD")
complementary("#4292C6")
#3182BD, #4292C6

complementary("#69b3a2")
#69b3a2


splitComp("steelblue")
splitComp("#4292C6")
#4292C6
tetradic("steelblue")
tetradic("#4292C6")

square("steelblue")
square("#4292C6")


sequential("steelblue")
sequential("#084594")

# Between independent variables  
cor.test(fitting_df$aco, fitting_df$aqpl1, method="spearman")

fitting_cont <- fitting_df[,c("aco","aqpl1","caps","csho","cstk","glcea","optfvgr","spced","stkcpa")]
cor_51 <- rcorr(as.matrix(fitting_cont), type = "spearman")
M1 <- cor_51$r
p_mat1 <- cor_51$P
par(mfrow=c(1,1))
corrplot(M1, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45,
         col = colorRampPalette(c("darkslategrey","white","orange"))(200),
         p.mat = p_mat1, sig.level = 0.05, diag = FALSE)

corrplot(M1, method = "color", col = colorRampPalette(c("#945708","white","#084594"))(200),
         type = "lower", order = "hclust",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 0, #Text label color and rotation
         # Combine with significance level
         p.mat = p_mat1, sig.level = 0.05,
         # hide correlation coefficient on the principal diagonal
         diag = FALSE)

corrplot(M1, method = "color", col = colorRampPalette(c("#B56521","white","#2171B5"))(200),
         type = "lower", order = "hclust", diag = FALSE, addgrid.col = "black",
         addCoef.col = "black", number.cex = 1.5, addCoefasPercent = FALSE,
         tl.col = "black", tl.srt = 0, tl.pos = "ld", tl.cex = 1.5, #Text label color and rotation
         p.mat = p_mat1, sig.level = 0.05, pch.cex = 5, cl.cex = 1.3)

corrplot(M1, method = "color", col = colorRampPalette(c("#9C5308","#D6936B","#FFFBEF","#EFF3FF","#6BAED6","#08519C"))(200),
         type = "lower", order = "hclust", number.cex = 1.5, #number.font = 2, #addCoefasPercent = "black",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 0, tl.pos = "ld", tl.cex = 1.5, 
         # Combine with significance level
         p.mat = p_mat1, sig.level = 0.05, cl.cex = 1.2, pch.cex=5,
         # hide correlation coefficient on the principal diagonal
         diag = FALSE)


RColorBrewer::brewer.pal(n = 5, name = "Blues")
complementary("#08519C")
complementary("#6BAED6")
complementary("#EFF3FF")

c("#9C5308","#D6936B","#FFFBEF","#EFF3FF","#6BAED6","#08519C")


#--------------------------------------------forecasting----------------------------------------------
#--------------------------------------------forecasting----------------------------------------------
#--------------------------------------------forecasting----------------------------------------------


funda2019 <- subset(funda0020, fyear==2019)
funda2020 <- subset(funda0020, fyear==2020)
rownames(funda2019) <- 1:nrow(funda2019)
rownames(funda2020) <- 1:nrow(funda2020)
funda2020 <- funda2020[,sort(colnames(funda2020))]
colnames(funda2020)

table(funda2010$dlrsn)

table(funda2020$idbflag)
funda2020$stalt <- as.factor(funda2020$stalt)
funda2020$idbflag <- as.factor(funda2020$idbflag)

colSums(is.na(funda2019[,c("aco","aqpl1","caps","csho","cstk","glcea","optfvgr","spced","stkcpa")]))
funda2020na <- funda2020[,c("aco","aqpl1","caps","csho","cstk","glcea","optfvgr","spced","stkcpa","stalt","idbflag","sic","naics")]
sum(is.na(funda2020cont))

#-------------------------------------------NA handling 2020----------------------------------------------

funda2020na
table(funda2020na$naics)
colSums(is.na(funda2020na)) #3768
subset(funda2020na, is.na(sic))
sort(colnames(funda2020))
colnames(funda2020na)[13] <- "naics6"
funda2020na[,"naics5"] <- substr(funda2020na$naics,1,5)
funda2020na[,"naics4"] <- substr(funda2020na$naics,1,4)
funda2020na[,"naics3"] <- substr(funda2020na$naics,1,3)
funda2020na[,"naics2"] <- substr(funda2020na$naics,1,2)

## 6 digit mean
n6_mean <- aggregate(.~funda2020na$naics6, funda2020cont, mean, na.rm=TRUE, na.action=na.pass)
colnames(n6_mean)[1] <- "naics6"
c6 <- funda2020na %>% left_join(n6_mean, by="naics6")
write.csv(c6, 'c6.csv', row.names=F)
funda2020_naics6 <- read.csv("funda2020_naics6.csv", na = c("", "NA", "N/A"))
funda2020_naics6 <- funda2020_naics6[,-1]
sum(is.na(funda2020_naics6[,colnames(funda2020cont)])) # 1085687 / 267381 

## 5 digit mean
n5_mean <- aggregate(.~funda2020na$naics5, funda2020cont, mean, na.rm=TRUE, na.action=na.pass)
colnames(n5_mean)[1] <- "naics5"
funda2020_naics6$naics5 <- as.character(funda2020_naics6$naics5)
c5 <- funda2020_naics6 %>% left_join(n5_mean, by="naics5")
write.csv(c5, 'c5.csv', row.names=F)
funda2020_naics5 <- read.csv("funda2020_naics5.csv", na = c("", "NA", "N/A"))
funda2020_naics5 <- funda2020_naics5[,-1]
sum(is.na(funda2020_naics5[,colnames(funda2020cont)])) # 253864

## 4 digit mean
naics4_mean <- aggregate(.~funda2010$naics4, funda2010cont, mean, na.rm=TRUE, na.action=na.pass)
colnames(naics4_mean)[1] <- "naics4"
funda2010_naics5$naics4 <- as.character(funda2010_naics5$naics4)
cont_xy4 <- funda2010_naics5 %>% left_join(naics4_mean, by="naics4")
write.csv(cont_xy4, 'cont_xy4.csv', row.names=F)
funda2010_naics4 <- read.csv("funda2010_naics4.csv", na = c("", "NA", "N/A"))
funda2010_naics4 <- funda2010_naics4[,-1]
sum(is.na(funda2010_naics4[,colnames(funda2010cont)])) # 199897

## 3 digit mean
naics3_mean <- aggregate(.~funda2010$naics3, funda2010cont, mean, na.rm=TRUE, na.action=na.pass)
colnames(naics3_mean)[1] <- "naics3"
funda2010_naics4$naics3 <- as.character(funda2010_naics4$naics3)
cont_xy3 <- funda2010_naics4 %>% left_join(naics3_mean, by="naics3")
write.csv(cont_xy3, 'cont_xy3.csv', row.names=F)
funda2010_naics3 <- read.csv("funda2010_naics3.csv", na = c("", "NA", "N/A"))
funda2010_naics3 <- funda2010_naics3[,-1]
sum(is.na(funda2010_naics3[,colnames(funda2010cont)])) # 187878

## 2 digit mean
naics2_mean <- aggregate(.~funda2010$naics2, funda2010cont, mean, na.rm=TRUE, na.action=na.pass)
colnames(naics2_mean)[1] <- "naics2"
funda2010_naics3$naics2 <- as.character(funda2010_naics3$naics2)
cont_xy2 <- funda2010_naics3 %>% left_join(naics2_mean, by="naics2")
write.csv(cont_xy2, 'cont_xy2.csv', row.names=F)
funda2010_naics2 <- read.csv("funda2010_naics2.csv", na = c("", "NA", "N/A"))
funda2010_naics2 <- funda2010_naics2[,-1]
sum(is.na(funda2010_naics2[,colnames(funda2010cont)])) # 184688

### Now 560 companies NA handling with SIC code
## 4 digit mean
sic4_mean <- aggregate(.~funda2010$sic, funda2010cont, mean, na.rm=TRUE, na.action=na.pass)
colnames(sic4_mean)[1] <- "sic"
sic_xy4 <- funda2010_naics2 %>% left_join(sic4_mean, by="sic")
write.csv(sic_xy4, 'sic_xy4.csv', row.names=F)
funda2010_sic4 <- read.csv("funda2010_sic4.csv", na = c("", "NA", "N/A"))
funda2010_sic4 <- funda2010_sic4[,-1]
sum(is.na(funda2010_sic4[,colnames(funda2010cont)])) # 161845

table(funda2010$sic)

funda2010na <- funda2010_sic4

funda2010na$naics2 <- as.character(funda2010_naics2$naics2)
funda2010na$naics3 <- as.character(funda2010_naics2$naics3)
funda2010na$naics4 <- as.character(funda2010_naics2$naics4)
funda2010na$naics5 <- as.character(funda2010_naics2$naics5)
funda2010na$naics6 <- as.character(funda2010_naics2$naics6)

funda2010na <- funda2010na[apply(funda2010na, 1, function(x) sum(is.na(x))<dim(funda2010na)[2]*0.15),]
rownames(funda2010na) <- 1:dim(funda2010na)[1]

sum(is.na(funda2010na)) # 58409
sum(is.na(funda2010na[,colnames(funda2010cont)])) # 1622
colSums(is.na(funda2010na))

write.csv(funda2010na, "funda2010na.scv", row.names=F)



##-------------------------------------------- Pie Chart ----------------------------------------------

RColorBrewer::brewer.pal(n = 5, name = "Blues")

p1 <- ggplot(aggregate(aco~BL, fitting_df, length), aes(x="", y=aco, fill=BL)) +
  geom_bar(stat="identity") + #, width=0.01, color="black") +
  scale_fill_manual(values = c("#EFF3FF","#3182BD")) +
  coord_polar("y", start=0) +
  theme_void(base_size = 21) + # remove background, grid, numeric labels
  labs(title="Response variable", x=NULL, y=NULL, fill="BL") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")

p2 <- ggplot(aggregate(aco~stalt, fitting_df, length), aes(x="", y=aco, fill=stalt)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("#EFF3FF","#3182BD")) +
  coord_polar("y", start=0) +
  theme_void(base_size = 21) + 
  labs(title="Status Alert", x=NULL, y=NULL, fill="STALT") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")

p3 <- ggplot(aggregate(aco~idbflag, fitting_df, length), aes(x="", y=aco, fill=idbflag)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("#3182BD","#EFF3FF")) +
  coord_polar("y", start=0) +
  theme_void(base_size = 21) +
  labs(title="International, Domestic, Both", x=NULL, y=NULL, fill="IDBFLAG") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")

multiplot(p1,p2,p3,cols=3)


##-------------------------------------------- chi-squared test ----------------------------------------------

table(fitting_df$BL, fitting_df$stalt)
chisq.test(table(fitting_df$BL, fitting_df$stalt))
fisher.test(table(fitting_df$BL, fitting_df$stalt))

table(fitting_df$BL, fitting_df$idbflag)
chisq.test(table(fitting_df$BL, fitting_df$idbflag))
fisher.test(table(fitting_df$BL, fitting_df$idbflag))

table(fitting_df$stalt, fitting_df$idbflag)
chisq.test(table(fitting_df$stalt, fitting_df$idbflag))
fisher.test(table(fitting_df$stalt, fitting_df$idbflag))

install.packages(c("vcd"), dep=TRUE)
library(vcd)
mosaic(~ stalt + BL, direction = c("v", "h"), data = fitting_df, shade = TRUE)

install.packages(c("oddsratio"), dep=TRUE)
library(oddsratio)
# Calculate OR for specific increment step of continuous variable
or_glm(data = fitting_df, model = glm2.fit, incr = list(gre = 380, gpa = 5))
or_glm(data = fitting_df, model = step_glm, incr = list(gre = 380, gpa = 5))

ORtable=function(x,digits=2){
  suppressMessages(a<-confint(x))
  result=data.frame(exp(coef(x)),exp(a))
  result=round(result, digits)
  result=cbind(result,round(summary(x)$coefficient[,4],3))
  colnames(result)=c("OR","2.5%","97.5%","p")
  result
}

odds_ratio = ORtable(glm2.fit)
odds_ratio = odds_ratio[2:nrow(odds_ratio),]
odds_ratio = odds_ratio[2:nrow(odds_ratio),]
odds_ratio[-7,]
HRplot(odds_ratio[c(-2,-7),], type=2, show.CI=TRUE, cex=2)


install.packages ( " reprex " )
devtools :: install_github ( " tidyverse / reprex " )

