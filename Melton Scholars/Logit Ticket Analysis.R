setwd("~/Melton Scholars")
# libraries to load in
library(lubridate)
library(regclass)
library(multcompView)
library(arules)
library(discretization)
library(rockchalk)
library(DescTools)
library(caret)
library(fitdistrplus)
library(SOIL)
library(Boruta)
library(tidyverse)
library(stringr)

library(mlogit)

load("~/Melton Scholars/logit model data.RData")
#

# generating and checking data
# mydata <- read.csv("Primary15-19_Cleaned.csv")
 
str(mydata)

table(factor(mydata$type))

table(factor(mydata$type[which(mydata$type %in% "Alumni - Season")]))
# let's try separating season tickets and single game tickets
levels(mydata$type)
x <- levels(mydata$type)
str_view(x, "Season*")
str_detect(x, "Season*"); sum(str_detect(x, "Season*"))
str_detect(x, "Season"); sum(str_detect(x, "Season"))
levels(mydata$type)[which(str_detect(x, "Season*") == T)]

season_tickets <- mydata[which(mydata$type %in% c( levels(mydata$type)[which(str_detect(x, "Season*") == T)] ) ),]
single_game <- mydata[-which(mydata$type %in% c( levels(mydata$type)[which(str_detect(x, "Season*") == T)] ) ),]
nrow(season_tickets) + nrow(single_game) == nrow(mydata) #true!!

str(season_tickets)
str(single_game)


# adding in order location
cons.disc <- c()
y <- season_tickets$order_state
for (i in 1:nrow(season_tickets)) {
  if(y[i] %in% c(levels(season_tickets$order_state)[170:190], "Knox") & season_tickets$order_city[i] == "Knoxville") { cons.disc[i] <- 0 }
  if(y[i] %in% c(levels(season_tickets$order_state)[170:190]) & season_tickets$order_city[i] != "Knoxville") { cons.disc[i] <- 1 }
  if(y[i] %in% c(levels(season_tickets$order_state)[-c(170:190)])) { cons.disc[i] <- 2 }
}
season_tickets$order_location <- factor(cons.disc)
summary(season_tickets$order_location)
season_tickets[1:20,]


# aggregate season tickets by quantity order then add total price paid
S <- aggregate(seat ~ price + section + row + price_zone + type + order_number + order_location + game + game_date_time,
               data = season_tickets, FUN = function(x){sum(length(x))})

S[1:20,]
S$game_price <- S$price * S$seat
S[1:20,]

S$year <- year(S$game_date_time)
summary(S$year)

#aggregate to match season payment
season <- aggregate(game_price ~ price_zone + type + order_number + year + seat + order_location, data = S, FUN = sum)
season[1:20,]


# st_data <- S[,c(1:3,8:13)]
# str(st_data)
# sg_data <- single_game[,c(1,3:6,12,19:20,10)]
# str(sg_data)

st_data <- season


# add in zone specific variables
# table(factor(st_data$price_zone))
# add in distance factor (4 levels (0: lower deck, 1: upper deck))
# summary(st_data$price_zone)
# levels(st_data$price_zone)
# cons.disc <- c()
# y <- st_data$price_zone
# for (i in 1:nrow(st_data)) {
#   if(y[i] == "Chairbacks") { st_data$distance.chairbacks[i] <- 1 } 
#   if(y[i] == "East Sideline") { cons.disc[i] <- 0 }
#   if(y[i] == "East Upper") { cons.disc[i] <- 1 }
#   if(y[i] == "North Lower") { cons.disc[i] <- 0 }
#   if(y[i] == "South Lower") { cons.disc[i] <- 0 }
#   if(y[i] == "North Upper") { cons.disc[i] <- 1 }
#   if(y[i] == "South Upper (1-15)") { cons.disc[i] <- 1 }
#   if(y[i] == "South Upper (16+)") { cons.disc[i] <- 1 }
#   if(y[i] == "West Sideline") { cons.disc[i] <- 0 }
#   if(y[i] == "West Upper") { cons.disc[i] <- 1 }
# }
# st_data$distance <- cons.disc
# summary(st_data$distance)
# table(factor(st_data$distance))
st_data$distance.chairbacks <- 1
st_data$distance.east_sideline <- 0
st_data$distance.east_upper <- 1
st_data$distance.north_lower <- 0
st_data$distance.south_lower <- 0
st_data$distance.north_upper <- 1
st_data$distance.south_upper_L15 <- 1
st_data$distance.south_upper_M15 <- 1
st_data$distance.west_sideline <- 0
st_data$distance.west_upper <- 1

# add in direction factor (North, South, East, West)
# cons.disc <- c()
# y <- st_data$price_zone
# for (i in 1:nrow(st_data)) {
#   if(y[i] == "Chairbacks") { cons.disc[i] <- "North" }
#   if(y[i] == "East Sideline") { cons.disc[i] <- "East" }
#   if(y[i] == "East Upper") { cons.disc[i] <- "East" }
#   if(y[i] == "North Lower") { cons.disc[i] <- "North" }
#   if(y[i] == "South Lower/North Upper") { cons.disc[i] <- "South" }
#   if(y[i] == "South Upper (1-15)") { cons.disc[i] <- "South" }
#   if(y[i] == "South Upper (16+)") { cons.disc[i] <- "South" }
#   if(y[i] == "West Sideline") { cons.disc[i] <- "West" }
#   if(y[i] == "West Upper") { cons.disc[i] <- "West" }
# }
# st_data$direction <- factor(cons.disc)
# summary(st_data$direction)
st_data$direction.chairbacks <- "North"
st_data$direction.east_sideline <- "East"
st_data$direction.east_upper <- "East"
st_data$direction.north_lower <- "North"
st_data$direction.south_lower <- "South"
st_data$direction.north_upper <- "North"
st_data$direction.south_upper_L15 <- "South"
st_data$direction.south_upper_M15 <- "South"
st_data$direction.west_sideline <- "West"
st_data$direction.west_upper <- "West"

# add in averae price by section by year
st_data$price.chairbacks <- ifelse(st_data$year == 2015, mean(S$price[which(S$price_zone == "Chairbacks" & str_detect(S$type, "Comp*") == F & S$year == 2015)]), 
                                   ifelse(st_data$year == 2016, mean(S$price[which(S$price_zone == "Chairbacks" & str_detect(S$type, "Comp*") == F & S$year == 2016)]), 
                                          ifelse(st_data$year == 2017, mean(S$price[which(S$price_zone == "Chairbacks" & str_detect(S$type, "Comp*") == F & S$year == 2017)]), 
                                                 ifelse(st_data$year == 2018, mean(S$price[which(S$price_zone == "Chairbacks" & str_detect(S$type, "Comp*") == F & S$year == 2018)]), 
                                                        ifelse(st_data$year == 2019, mean(S$price[which(S$price_zone == "Chairbacks" & str_detect(S$type, "Comp*") == F & S$year == 2019)]), 0)))))
st_data$price.east_sideline <- ifelse(st_data$year == 2015, mean(S$price[which(S$price_zone == "East Sideline" & str_detect(S$type, "Comp*") == F & S$year == 2015)]), 
                                      ifelse(st_data$year == 2016, mean(S$price[which(S$price_zone == "East Sideline" & str_detect(S$type, "Comp*") == F & S$year == 2016)]), 
                                             ifelse(st_data$year == 2017, mean(S$price[which(S$price_zone == "East Sideline" & str_detect(S$type, "Comp*") == F & S$year == 2017)]), 
                                                    ifelse(st_data$year == 2018, mean(S$price[which(S$price_zone == "East Sideline" & str_detect(S$type, "Comp*") == F & S$year == 2018)]), 
                                                           ifelse(st_data$year == 2019, mean(S$price[which(S$price_zone == "East Sideline" & str_detect(S$type, "Comp*") == F & S$year == 2019)]), 0)))))
st_data$price.east_upper <- ifelse(st_data$year == 2015, mean(S$price[which(S$price_zone == "East Upper" & str_detect(S$type, "Comp*") == F & S$year == 2015)]), 
                                   ifelse(st_data$year == 2016, mean(S$price[which(S$price_zone == "East Upper" & str_detect(S$type, "Comp*") == F & S$year == 2016)]), 
                                          ifelse(st_data$year == 2017, mean(S$price[which(S$price_zone == "East Upper" & str_detect(S$type, "Comp*") == F & S$year == 2017)]), 
                                                 ifelse(st_data$year == 2018, mean(S$price[which(S$price_zone == "East Upper" & str_detect(S$type, "Comp*") == F & S$year == 2018)]), 
                                                        ifelse(st_data$year == 2019, mean(S$price[which(S$price_zone == "East Upper" & str_detect(S$type, "Comp*") == F & S$year == 2019)]), 0)))))
st_data$price.north_lower <- ifelse(st_data$year == 2015, mean(S$price[which(S$price_zone == "North Lower" & str_detect(S$type, "Comp*") == F & S$year == 2015)]), 
                                    ifelse(st_data$year == 2016, mean(S$price[which(S$price_zone == "North Lower" & str_detect(S$type, "Comp*") == F & S$year == 2016)]), 
                                           ifelse(st_data$year == 2017, mean(S$price[which(S$price_zone == "North Lower" & str_detect(S$type, "Comp*") == F & S$year == 2017)]), 
                                                  ifelse(st_data$year == 2018, mean(S$price[which(S$price_zone == "North Lower" & str_detect(S$type, "Comp*") == F & S$year == 2018)]), 
                                                         ifelse(st_data$year == 2019, mean(S$price[which(S$price_zone == "North Lower" & str_detect(S$type, "Comp*") == F & S$year == 2019)]), 0)))))
st_data$price.south_lower <- ifelse(st_data$year == 2015, mean(S$price[which(S$price_zone == "South Lower" & str_detect(S$type, "Comp*") == F & S$year == 2015)]), 
                                    ifelse(st_data$year == 2016, mean(S$price[which(S$price_zone == "South Lower" & str_detect(S$type, "Comp*") == F & S$year == 2016)]), 
                                           ifelse(st_data$year == 2017, mean(S$price[which(S$price_zone == "South Lower" & str_detect(S$type, "Comp*") == F & S$year == 2017)]), 
                                                  ifelse(st_data$year == 2018, mean(S$price[which(S$price_zone == "South Lower" & str_detect(S$type, "Comp*") == F & S$year == 2018)]), 
                                                         ifelse(st_data$year == 2019, mean(S$price[which(S$price_zone == "South Lower" & str_detect(S$type, "Comp*") == F & S$year == 2019)]), 0)))))
st_data$price.north_upper <- ifelse(st_data$year == 2015, mean(S$price[which(S$price_zone == "North Upper" & str_detect(S$type, "Comp*") == F & S$year == 2015)]), 
                                    ifelse(st_data$year == 2016, mean(S$price[which(S$price_zone == "North Upper" & str_detect(S$type, "Comp*") == F & S$year == 2016)]), 
                                           ifelse(st_data$year == 2017, mean(S$price[which(S$price_zone == "North Upper" & str_detect(S$type, "Comp*") == F & S$year == 2017)]), 
                                                  ifelse(st_data$year == 2018, mean(S$price[which(S$price_zone == "North Upper" & str_detect(S$type, "Comp*") == F & S$year == 2018)]), 
                                                         ifelse(st_data$year == 2019, mean(S$price[which(S$price_zone == "North Upper" & str_detect(S$type, "Comp*") == F & S$year == 2019)]), 0)))))
st_data$price.south_upper_L15 <- ifelse(st_data$year == 2015, mean(S$price[which(S$price_zone == "South Upper (1-15)" & str_detect(S$type, "Comp*") == F & S$year == 2015)]), 
                                        ifelse(st_data$year == 2016, mean(S$price[which(S$price_zone == "South Upper (1-15)" & str_detect(S$type, "Comp*") == F & S$year == 2016)]), 
                                               ifelse(st_data$year == 2017, mean(S$price[which(S$price_zone == "South Upper (1-15)" & str_detect(S$type, "Comp*") == F & S$year == 2017)]), 
                                                      ifelse(st_data$year == 2018, mean(S$price[which(S$price_zone == "South Upper (1-15)" & str_detect(S$type, "Comp*") == F & S$year == 2018)]), 
                                                             ifelse(st_data$year == 2019, mean(S$price[which(S$price_zone == "South Upper (1-15)" & str_detect(S$type, "Comp*") == F & S$year == 2019)]), 0)))))
st_data$price.south_upper_M15 <- ifelse(st_data$year == 2015, mean(S$price[which(S$price_zone == "South Upper (16+)" & str_detect(S$type, "Comp*") == F & S$year == 2015)]), 
                                        ifelse(st_data$year == 2016, mean(S$price[which(S$price_zone == "South Upper (16+)" & str_detect(S$type, "Comp*") == F & S$year == 2016)]), 
                                               ifelse(st_data$year == 2017, mean(S$price[which(S$price_zone == "South Upper (16+)" & str_detect(S$type, "Comp*") == F & S$year == 2017)]), 
                                                      ifelse(st_data$year == 2018, mean(S$price[which(S$price_zone == "South Upper (16+)" & str_detect(S$type, "Comp*") == F & S$year == 2018)]), 
                                                             ifelse(st_data$year == 2019, mean(S$price[which(S$price_zone == "South Upper (16+)" & str_detect(S$type, "Comp*") == F & S$year == 2019)]), 0)))))
st_data$price.west_sideline <- ifelse(st_data$year == 2015, mean(S$price[which(S$price_zone == "West Sideline" & str_detect(S$type, "Comp*") == F & S$year == 2015)]), 
                                      ifelse(st_data$year == 2016, mean(S$price[which(S$price_zone == "West Sideline" & str_detect(S$type, "Comp*") == F & S$year == 2016)]), 
                                             ifelse(st_data$year == 2017, mean(S$price[which(S$price_zone == "West Sideline" & str_detect(S$type, "Comp*") == F & S$year == 2017)]), 
                                                    ifelse(st_data$year == 2018, mean(S$price[which(S$price_zone == "West Sideline" & str_detect(S$type, "Comp*") == F & S$year == 2018)]), 
                                                           ifelse(st_data$year == 2019, mean(S$price[which(S$price_zone == "West Sideline" & str_detect(S$type, "Comp*") == F & S$year == 2019)]), 0)))))
st_data$price.west_upper<- ifelse(st_data$year == 2015, mean(S$price[which(S$price_zone == "West Upper" & str_detect(S$type, "Comp*") == F & S$year == 2015)]), 
                                  ifelse(st_data$year == 2016, mean(S$price[which(S$price_zone == "West Upper" & str_detect(S$type, "Comp*") == F & S$year == 2016)]), 
                                         ifelse(st_data$year == 2017, mean(S$price[which(S$price_zone == "West Upper" & str_detect(S$type, "Comp*") == F & S$year == 2017)]), 
                                                ifelse(st_data$year == 2018, mean(S$price[which(S$price_zone == "West Upper" & str_detect(S$type, "Comp*") == F & S$year == 2018)]), 
                                                       ifelse(st_data$year == 2019, mean(S$price[which(S$price_zone == "West Upper" & str_detect(S$type, "Comp*") == F & S$year == 2019)]), 0)))))


#
# adjusting and checking data
# # potential segmentation
# # table(factor(st_data$type))
# # st_data$channel <- "Season Tickets"
# # st_data$segment <- c()
# #
# # for (i in 1:nrow(st_data)) {
# #   ifelse( st_data$type[i] == "Alumni - Season", st_data$segment[i] <- "Alumni",
# #           ifelse( st_data$type[i] == "Donor - Season", st_data$segment[i] <- "Donor", st_data$segment[i] <- "Public") )
# # }
# # table(st_data$segment)
# 
# # logit analysis attempt
# 
# attach(mydata)
# 
# table(price_zone)
# 
# # aggregate whole season price by seat(section, row, seat) and year
# 
# Sea_Tick <- aggregate( price  ~ (section + row + seat + price_zone + type + year) , data = st_data, FUN = "sum" )
# A <- aggregate(price~price_zone+type+year, data = Sea_Tick, FUN="mean"); A[which(A$price_zone == "Chairbacks"),]
# 
# 
# write.csv(st_data,"season tickets 2015-2019 by game.csv",row.names = F)
# write.csv(Sea_Tick,"season tickets 2015-2019 lump sum.csv",row.names = F)
# 
# Sea_Tick[which(Sea_Tick$section == "XX4" & Sea_Tick$row == 13 & Sea_Tick$seat == 1),] # just a check to make sure aggregate worked
# 
# my_st_analysis <- mlogit.data(Sea_Tick, choice = "price_zone", shape = "wide", alt.var = c(levels(Sea_Tick$price_zone)),
#                               varying = list("price", "section", "row", "seat") )
# 
# 
# 
# 
# y <- Sea_Tick$price_zone
# # for (i in 304404:nrow(Sea_Tick)) {
#   if(y[i] == "Chairbacks") { Sea_Tick$Chairbacks[i] <- 1 } else { Sea_Tick$Chairbacks[i] <- 0 }
#   if(y[i] == "East Sideline") { Sea_Tick$East_Sideline[i] <- 1 } else { Sea_Tick$East_Sideline[i] <- 0 }
#   if(y[i] == "East Upper") { Sea_Tick$East_Upper[i] <- 1 } else { Sea_Tick$East_Upper[i] <- 0 }
#   if(y[i] == "North Lower") { Sea_Tick$North_Lower[i] <- 1 } else { Sea_Tick$North_Lower[i] <- 0 }
#   if(y[i] == "South Lower/North Upper") { Sea_Tick$South_Lower_North_Upper[i] <- 1 } else { Sea_Tick$South_Lower_North_Upper[i] <- 0 }
#   if(y[i] == "South Upper (1-15)") { Sea_Tick$South_Upper_1_15[i] <- 1 } else { Sea_Tick$South_Upper_1_15[i] <- 0 }
#   if(y[i] == "South Upper (16+)") { Sea_Tick$South_Upper_16[i] <- 1 } else { Sea_Tick$South_Upper_16[i] <- 0 }
#   if(y[i] == "West Sideline") { Sea_Tick$West_Sideline[i] <- 1 } else { Sea_Tick$West_Sideline[i] <- 0 }
#   if(y[i] == "West Upper") { Sea_Tick$West_Upper[i] <- 1 } else { Sea_Tick$West_Upper[i] <- 0 }
# }
# 
# mldata <- mlogit.data( Sea_Tick, choice = "price_zone", shape = "wide", alt.var = "price_zone",
#                       varying = c(list("price", "section", "row", "seat"), 8:16) )
# 
# # names(Sea_Tick) <- c( paste("var", 1:7, sep = "") )
# # names(Sea_Tick) <- c("section", "row", "seat", "price_zone", "type", "year", "price")
# 
# # my_st_analysis <- uReshape(Sea_Tick, id.vars = c("type"))
# 
library(mlogit)
library(rstudioapi)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# # mydata <- read.csv("Primary15-19_Cleaned.csv")
# my_st_analysis <- mlogit.data(mydata, choice = "price_zone", shape = "wide" )
# my_st_analysis[1:20,]

# aggregate st_data by section, seat, row over the full season
# DATA <- aggregate( order_price  ~ (section + row + type + price_zone + year + distance.chairbacks + distance.east_sideline + distance.east_upper +
#                                      distance.north_lower + distance.south_lower + distance.north_upper + distance.south_upper_L15 + 
#                                      distance.south_upper_M15 + distance.west_sideline + distance.west_upper + direction.chairbacks + 
#                                      direction.east_sideline + direction.east_upper + direction.north_lower + direction.south_lower + 
#                                      direction.north_upper + direction.south_upper_L15 + direction.south_upper_M15 + direction.west_sideline + 
#                                      direction.west_upper + price.chairbacks + price.east_sideline + price.east_upper +
#                                      price.north_lower + price.south_lower + price.north_upper + price.south_upper_L15 + 
#                                      price.south_upper_M15 + price.west_sideline + price.west_upper + seat + order_location) , data = st_data, FUN = "sum" )
# summary(DATA)
# DATA[which(DATA$seat_quantity == 50),]
DATA <- st_data
names(DATA)[7] <- "order_price" 
names(DATA)[5] <- "seat_quantity"

# using all season ticket available levels (season_tickets)
# set.seed(494); sample.rows <- sample(1:nrow(season_tickets), size = 2000, replace = F)
donor.rows <- which(DATA$type == "Donor - Season")
alumni.rows <- which(DATA$type == "Alumni - Season")
faculty.rows <- which(DATA$type == "Faculty - Season")
donor.rows19 <- which(DATA$type == "Donor - Season" & DATA$year == "2019")
alumni.rows19 <- which(DATA$type == "Alumni - Season" & DATA$year == "2019")
faculty.rows19 <- which(DATA$type == "Faculty - Season" & DATA$year == "2019")
# mysample <- season_tickets[sample.rows, ]
donor_sample <- DATA[donor.rows, -c(2:3)]
alumni_sample <- DATA[alumni.rows, -c(2:3)]
faculty_sample <- DATA[faculty.rows, -c(2:3)]
other_sample <- DATA[-c(donor.rows,alumni.rows,faculty.rows), -c(2:3)]
donor_sample19 <- DATA[donor.rows19, -c(2:4)]
alumni_sample19 <- DATA[alumni.rows19, -c(2:4)]
faculty_sample19 <- DATA[faculty.rows19, -c(2:4)]
other_sample19 <- DATA[-c(donor.rows19,alumni.rows19,faculty.rows19), -c(2:4)]
#samp <- DATA[,-c(1:2,5)]

# my_analysis <- mlogit.data(mysample, choice = "price_zone", shape = "wide")
# my_analysis <- mlogit.data(samp, choice = "price_zone", shape = "wide", varying = 3:32, alt.levels = c(levels(samp$price_zone)))
alum_analysis <- mlogit.data(alumni_sample, choice = "price_zone", shape = "wide", varying = 6:35, alt.levels = c(levels(alumni_sample$price_zone)))
donor_analysis <- mlogit.data(donor_sample, choice = "price_zone", shape = "wide", varying = 6:35, alt.levels = c(levels(donor_sample$price_zone)))
faculty_analysis <- mlogit.data(faculty_sample, choice = "price_zone", shape = "wide", varying = 6:35, alt.levels = c(levels(faculty_sample$price_zone)))
other_analysis <- mlogit.data(other_sample, choice = "price_zone", shape = "wide", varying = 6:35, alt.levels = c(levels(other_sample$price_zone)))
# 2019
alum_analysis19 <- mlogit.data(alumni_sample19, choice = "price_zone", shape = "wide", varying = 5:34, alt.levels = c(levels(alumni_sample19$price_zone)))
donor_analysis19 <- mlogit.data(donor_sample19, choice = "price_zone", shape = "wide", varying = 5:34, alt.levels = c(levels(donor_sample19$price_zone)))
faculty_analysis19 <- mlogit.data(faculty_sample19, choice = "price_zone", shape = "wide", varying = 5:34, alt.levels = c(levels(faculty_sample19$price_zone)))
other_analysis19 <- mlogit.data(other_sample19, choice = "price_zone", shape = "wide", varying = 5:34, alt.levels = c(levels(other_sample19$price_zone)))
# my_analysis[1:20, ]
alum_analysis[1:20, ]
donor_analysis[1:20, ]
faculty_analysis[1:20, ]
other_analysis[1:20, ]

# analysis
formula <- mFormula( price_zone ~ distance + direction + price + 0  )
logit.model_1 <- mlogit(formula, data = alum_analysis)

logit.model_2 <- mlogit(formula, data = donor_analysis)

logit.model_3 <- mlogit(formula, data = faculty_analysis)

logit.model_4 <- mlogit(formula, data = other_analysis)

summary(logit.model_1)
summary(logit.model_2)
summary(logit.model_3)
summary(logit.model_4)

# 2019 analysis
logit.model_5 <- mlogit(formula, data = alum_analysis19)

logit.model_6 <- mlogit(formula, data = donor_analysis19)

logit.model_7 <- mlogit(formula, data = faculty_analysis19)

logit.model_8 <- mlogit(formula, data = other_analysis19)

summary(logit.model_5)
summary(logit.model_6)
summary(logit.model_7)
summary(logit.model_8)

save.image("~/Melton Scholars/logit model data.RData")
# using only season ticket relevant levels (st_data) with no new variables
# set.seed(494); sample.rows <- sample(1:nrow(st_data), size = 2000, replace = F)
# mysample <- st_data[sample.rows, ]
# 
# my_analysis <- mlogit.data(mysample, choice = "price_zone", shape = "wide")
# my_analysis[1:20,]
# 
# # analysis
# logit.model_4 <- mlogit(price_zone ~ 0 | price, data = my_analysis)
# summary(logit.model_4)
# 
# logit.model_5 <- mlogit(price_zone ~ 1, data = my_analysis)
# summary(logit.model_5)
# 
# logit.model_6 <- mlogit(price_zone ~ 1 | price + type + year, data = my_analysis)
# summary(logit.model_6)


### using multinom from nnet
DATA$price_zone2 <- relevel(DATA$price_zone, ref = "Chairbacks")
model_1 <- multinom(price_zone ~ ., data = alumni_sample)
summary(model_1)

#### need to update "Vol Pass - Single Game" price in mydata and sg_data before single game analysis####











