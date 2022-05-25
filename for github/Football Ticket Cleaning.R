setwd("~/Melton Scholars")

library(lubridate)
library(regclass)
library(multcompView)
library(arules)
library(discretization)
library(rockchalk)
library(DescTools)
library(caret)
library(fitdistrplus)
library(Boruta)

# running through newest code updates since we received additional data marked now by tranasaction ID (received late April)


## read in ticketing data for 2015-2019; source "Athletics"; no secondary market data at the moment
Tickets2015 <- read.csv("2015.csv")
Tickets2016 <- read.csv("2016.csv")
Tickets2017 <- read.csv("2017.csv")
Tickets2018 <- read.csv("2018.csv")
Tickets2019 <- read.csv("2019.csv")

## reading in VividSeats (secondary market) data, received 10/23/2019
VS2017 <- read.csv("2017.csv")
VS2018 <- read.csv("2018.csv")
VS2019 <- read.csv("2019.csv")

## add in tv information for 2015; need to verify 2017-18 for any issues but 2015-16 needed full update
summary(Tickets2015$tv)
levels(Tickets2015$game)
cons.disc <- c()
x <- Tickets2015$game
for (i in 1:nrow(Tickets2015)) { 
  if(x[i]=="Arkansas") { cons.disc[i] <- "ESPN2" }
  if(x[i]=="Georgia") { cons.disc[i] <- "CBS" }
  if(x[i]=="North Texas") { cons.disc[i] <- "SECN" }
  if(x[i]=="Oklahoma") { cons.disc[i] <- "ESPN" }
  if(x[i]=="South Carolina") { cons.disc[i] <- "SECN" }
  if(x[i]=="Vanderbilt") { cons.disc[i] <- "SECN" }
  if(x[i]=="Western Carolina") { cons.disc[i] <- "ESPNU" }
}
Tickets2015$tv <- factor(cons.disc)
summary(Tickets2015$tv)
levels(Tickets2015$tv)

summary(Tickets2016$tv)
levels(Tickets2016$game)
cons.disc <- c()
x <- Tickets2016$game
for (i in 1:nrow(Tickets2016)) { 
  if(x[i]=="FOOTBALL vs Alabama") { cons.disc[i] <- "CBS" }
  if(x[i]=="FOOTBALL vs Appalachian State") { cons.disc[i] <- "SECN" }
  if(x[i]=="FOOTBALL vs Florida") { cons.disc[i] <- "CBS" }
  if(x[i]=="FOOTBALL vs Kentucky") { cons.disc[i] <- "SECN" }
  if(x[i]=="FOOTBALL vs Missouri") { cons.disc[i] <- "CBS" }
  if(x[i]=="FOOTBALL vs Ohio University") { cons.disc[i] <- "SECN" }
  if(x[i]=="FOOTBALL vs Tennessee Tech") { cons.disc[i] <- "SECN" }
}
Tickets2016$tv <- factor(cons.disc)
summary(Tickets2016$tv)
levels(Tickets2016$tv)

## add in variable of "home team state", "away team state", "other"

# nlevels(Tickets2015$game); levels(Tickets2015$game)
# adjust opponent state information to correct incorrect levels
summary(Tickets2015$opponent_state)
levels(Tickets2015$opponent_state)
Tickets2015$opponent_state <- 
  combineLevels(Tickets2015$opponent_state, levs = levels(Tickets2015$opponent_state)[c(2,4)], newLabel = c("GA"))
levels(Tickets2015$opponent_state)
Tickets2015$opponent_state <- 
  combineLevels(Tickets2015$opponent_state, levs = levels(Tickets2015$opponent_state)[c(2,4)], newLabel = c("NC"))
levels(Tickets2015$opponent_state)
Tickets2015$opponent_state <- 
  combineLevels(Tickets2015$opponent_state, levs = levels(Tickets2015$opponent_state)[c(2,5)], newLabel = c("TN"))
nlevels(Tickets2015$opponent_state)
# double check/correct order state information; needs mass corrections
summary(Tickets2015$order_state)
levels(Tickets2015$order_state)

## now we can add in the variable
#summary(Tickets2015$opponent_state)
#levels(Tickets2015$opponent_state)
#cons.disc <- c()
#x <- Tickets2015$game
#for (i in 1:nrow(Tickets2015)) { 
#  if(x[i]=="Arkansas") { cons.disc[i] <- "ESPN2" }
#  if(x[i]=="Georgia") { cons.disc[i] <- "CBS" }
#  if(x[i]=="North Texas") { cons.disc[i] <- "SECN" }
#  if(x[i]=="Oklahoma") { cons.disc[i] <- "ESPN" }
#  if(x[i]=="South Carolina") { cons.disc[i] <- "SECN" }
#  if(x[i]=="Vanderbilt") { cons.disc[i] <- "SECN" }
#  if(x[i]=="Western Carolina") { cons.disc[i] <- "ESPNU" }
#}
#Tickets2015$order_state <- factor(cons.disc)
#summary(Tickets2015$order_state)
#levels(Tickets2015$order_state); levels(Tickets2016$order_state); 
#levels(Tickets2017$order_state); levels(Tickets2018$order_state)

## adding in home opponent's record on game day
Tickets2015$opponent_record_gameday 

## adding home opponent's record for previous season
Tickets2015$opponent_record_previous_year 

## adjust 2015 game information to match all seasons
summary(Tickets2015$game)
levels(Tickets2015$game)
cons.disc <- c()
x <- Tickets2015$game
for (i in 1:nrow(Tickets2015)) { 
  if(x[i]=="Arkansas") { cons.disc[i] <- "FOOTBALL vs Arkansas" }
  if(x[i]=="Georgia") { cons.disc[i] <- "FOOTBALL vs Georgia" }
  if(x[i]=="North Texas") { cons.disc[i] <- "FOOTBALL vs North Texas" }
  if(x[i]=="Oklahoma") { cons.disc[i] <- "FOOTBALL vs Oklahoma" }
  if(x[i]=="South Carolina") { cons.disc[i] <- "FOOTBALL vs South Carolina" }
  if(x[i]=="Vanderbilt") { cons.disc[i] <- "FOOTBALL vs Vanderbilt" }
  if(x[i]=="Western Carolina") { cons.disc[i] <- "FOOTBALL vs Western Carolina" }
}
Tickets2015$game <- factor(cons.disc)
summary(Tickets2015$game)
levels(Tickets2015$game)

## discritize sections into Tennessee's "price zones"
# link to price zones layout: 
    # https://static.utsports.com/custompages/pdf/Neyland_Stadium_Price_Zone_Chart.pdf
## verify section infromation, number and label

## ignore lines where price is equal to 0

#tennessee record on gameday appears to be fine, not sure why there is true/false values. Will check to see if they
#connect to particular gameday
table(Tickets2015$tennessee_record)
Tickets2015[which(Tickets2015$tennessee_record == "False"),] ## all values western carolina game
Tickets2015$tennessee_record <- 
  combineLevels(Tickets2015$tennessee_record, levs = levels(Tickets2015$tennessee_record)[c(2,8)], 
                newLabel = levels(Tickets2015$tennessee_record)[2])
table(Tickets2015$tennessee_record)
Tickets2015[which(Tickets2015$tennessee_record == "True"),] # some variables Vandy and Georgia, ugh




Tickets2015$id_year <- paste(Tickets2015$id,"2015", sep = "_")
Tickets2016$id_year <- paste(Tickets2016$id,"2016", sep = "_")
Tickets2017$id_year <- paste(Tickets2017$id,"2017", sep = "_")
Tickets2018$id_year <- paste(Tickets2018$id,"2018", sep = "_")
Tickets2019$id_year <- paste(Tickets2019$id,"2019", sep = "_")
VS2017$id_year <- paste(VS2017$id, "2017", sep = "_")
VS2018$id_year <- paste(VS2018$id, "2018", sep = "_")
VS2019$id_year <- paste(VS2019$id, "2019", sep = "_")


DATA <- rbind(Tickets2015,Tickets2016,Tickets2017,Tickets2018,Tickets2019)
View(DATA)
write.csv(tickets.orig, "TennesseeTickets15-19.csv", row.names = FALSE)
VSData <- rbind(VS2017, VS2018, VS2019)
View(VSData)
write.csv(VSData, "VividSeatsTickets17-19.csv", row.names = FALSE)


# tickets.orig <- DATA
# secondary.orig <- VSData

# DATA <- tickets.orig
# VSData <- secondary.orig


head(DATA$game_date_time)
DATA$game_date_time <- mdy_hm(DATA$game_date_time)
head(DATA$order_date_time)
DATA$order_date_time <- mdy_hm(DATA$order_date_time) # generates some NA's


summary(DATA)
summary(DATA$price)
DATA[which(is.na(DATA$price)),] # all rows entirely blank, remove
length(DATA[which(is.na(DATA$price)),])
DATA <- DATA[-which(is.na(DATA$price)),]

summary(DATA) # let's remove the incomplete cases, should only be a few
nrow(DATA) # 3196912 before removing incomplete cases
DATA <- DATA[complete.cases(DATA),]
nrow(DATA) # 3196817 final number of complete cases!

## null out identifying variables
DATA$id_year <- NULL
DATA$id <- NULL
# DATA$performance_code <- NULL

summary(DATA)

# why are some levels in primary data type listed as secondary markets
levels(DATA$type)
length(which(DATA$type %in% c("Vivid Seats", "Stubhub")))

# ################### analysis ###################
# set.seed(9523); train.rows <- sample(1:nrow(DATA),20000)
# TRAIN <- DATA[train.rows,]; HOLDOUT <- DATA[-train.rows,]
# 
# 
# # M <- lm(price~., data=TRAIN)
# # let's see a basic relationship between section and row
# M_1 <- lm(price~section+row, data=TRAIN)
# summary(M_1)
# plot(M_1)
# # now let's look at the relationship with game
# M_2 <- lm(price~game, data=TRAIN)
# summary(M_2)
# plot(M_2)
# # basic regressions without adjustments
# M_3 <- lm(resale_price ~ section+row, data = VSData)
# summary(M_3)
# M_4 <- lm(resale_price ~ row, data=VSData)
# summary(M_4)

# add in game number
summary(VSData)
table( as.numeric( substr(VSData$performance_code,start=6,stop=7) ) )
VSData$game_number <- as.numeric( substr(VSData$performance_code,start=6,stop=7) )

summary(DATA)
table( as.numeric( substr(DATA$performance_code,start=6,stop=7) ) )
DATA$game_number <- as.numeric( substr(DATA$performance_code,start=6,stop=7) )

# "fixing" row data
for( let in LETTERS) {
 VSData$row[ which( VSData$row == let) ] <- as.character( match(let,LETTERS) )
}
VSData$row[ which( VSData$row == "WC") ] <- as.character( 12 )
VSData <- VSData[-which( VSData$row == ""),]
levels(VSData$row)
table(VSData$row)
VSData$row <- factor(VSData$row)
levels(VSData$row)
table(VSData$row)
VSData$row <- as.numeric( as.character( VSData$row ) )

## adjust row data for original
for( let in LETTERS) {
  DATA$row[ which( DATA$row == let) ] <- as.character( match(let,LETTERS) )
}
DATA$row[ which( DATA$row == "WC") ] <- as.character( 12 )
# DATA <- DATA[-which( DATA$row == ""),]
levels(DATA$row)
table(DATA$row)
DATA$row <- factor(DATA$row)
levels(DATA$row)
table(DATA$row)
DATA$row <- as.numeric( as.character( DATA$row ) )


## let's add in price zones for the secondary market
# need to add in row contingency in order to hit all of the different "price zones" 
# can also just add in "section type" ie upper bowl, lower bowl, and sidelines (upper v lower)
# DATA <- read.csv("Primary15-19_Cleaned.csv")
# VSData <- read.csv("Secondary_Market_Adjusted.csv")
#
summary(VSData$section)
levels(VSData$section)
VSData$section <- factor(VSData$section)
summary(VSData$section)
levels(VSData$section)
cons.disc <- c()
x <- VSData$section
for (i in 1:nrow(VSData)) { 
  if(x[i]%in% c("R","S","T","U","V","W")) { cons.disc[i] <- "West Sideline" }
  if(x[i]%in% c("A","B","C","D","E","F")) { cons.disc[i] <- "East Sideline" }
  if(x[i]%in% c("G","H","I","J","K","L","M",
                "N","O","P","Q")) { cons.disc[i] <- "South Lower" }
  if(x[i]%in% c("XX1","XX2","XX3","XX4","XX5","YY6","YY7","YY8","YY9",
                "YY10","ZZ11","ZZ12","ZZ13","ZZ14","ZZ15")
     & VSData$row[i] <= 12) { cons.disc[i] <- "North Upper" }
  if(x[i]%in% c("X1","X2","X3","X4","X5","Y6","Y7","Y8","Y9",
                "Y10","Z11","Z12","Z13","Z14","Z15")) { cons.disc[i] <- "North Lower" }
  if(x[i]%in% c("AA","BB","CC","DD","EE","FF","EC4", "EAST")) { cons.disc[i] <- "East Upper" }
  if(x[i]%in% c("TER-1","TER-2","TER-3","TER-4","TER-5",
                "TER-6","TER-7", "WC7")) { cons.disc[i] <- "West Upper" }
  if(x[i]%in% c("GG","HH","II","JJ","KK","LL","MM","NN","OO","PP","QQ") 
     & VSData$row[i] <= 15) { cons.disc[i] <- "South Upper (1-15)" }
  if(x[i]%in% c("GG","HH","II","JJ","KK","LL","MM","NN","OO","PP","QQ") 
     & VSData$row[i] >= 16) { cons.disc[i] <- "South Upper (16+)" }
  if(x[i]%in% c("XX1","XX2","XX3","XX4","XX5","YY6","YY7","YY8","YY9",
                "YY10","ZZ11","ZZ12","ZZ13","ZZ14","ZZ15")
     & VSData$row[i] >= 13) { cons.disc[i] <- "Chairbacks" }
}
VSData$price_zone <- factor(cons.disc)
summary(VSData$price_zone)
levels(VSData$price_zone)

summary(VSData)

write.csv(VSData, "Secondary_Market_Adjusted.csv", row.names = F)
# run price_zone adjustments on original data too
summary(DATA$section)
levels(DATA$section)
DATA$section <- factor(DATA$section)
summary(DATA$section)
levels(DATA$section)
cons.disc <- c()
x <- DATA$section
for (i in 1:nrow(DATA)) { 
  if(x[i]%in% c("R","S","T","U","V","W")) { cons.disc[i] <- "West Sideline" }
  if(x[i]%in% c("A","B","C","D","E","F")) { cons.disc[i] <- "East Sideline" }
  if(x[i]%in% c("G","H","I","J","K","L","M",
                "N","O","P","Q")) { cons.disc[i] <- "South Lower" }
  if(x[i]%in% c("XX1","XX2","XX3","XX4","XX5","YY6","YY7","YY8","YY9",
                "YY10","ZZ11","ZZ12","ZZ13","ZZ14","ZZ15")
     & DATA$row[i] <= 12) { cons.disc[i] <- "North Upper" }
  if(x[i]%in% c("X1","X2","X3","X4","X5","Y6","Y7","Y8","Y9",
                "Y10","Z11","Z12","Z13","Z14","Z15", "16", "17", "18")) { cons.disc[i] <- "North Lower" }
  if(x[i]%in% c("AA","BB","CC","DD","EE","FF","EC1", "EC2", "EC3", "EC4", "EC5", "EC6", "EC7",
                "EAST", "UNIV.STE.")) { cons.disc[i] <- "East Upper" }
  if(x[i]%in% c("TER-1","TER-2","TER-3","TER-4","TER-5",
                "TER-6","TER-7", "WC1", "WC2", "WC3", "WC4", "WC5", "WC6", "WC7", "WC8", "WC9", "WEST")) { cons.disc[i] <- "West Upper" }
  if(x[i]%in% c("GG","HH","II","JJ","KK","LL","MM","NN","OO","PP","QQ") 
     & DATA$row[i] <= 15) { cons.disc[i] <- "South Upper (1-15)" }
  if(x[i]%in% c("GG","HH","II","JJ","KK","LL","MM","NN","OO","PP","QQ") 
     & DATA$row[i] >= 16) { cons.disc[i] <- "South Upper (16+)" }
  if(x[i]%in% c("XX1","XX2","XX3","XX4","XX5","YY6","YY7","YY8","YY9",
                "YY10","ZZ11","ZZ12","ZZ13","ZZ14","ZZ15")
     & DATA$row[i] >= 13) { cons.disc[i] <- "Chairbacks" }
}
DATA$price_zone <- factor(cons.disc)
summary(DATA$price_zone)
levels(DATA$price_zone)

summary(DATA)

write.csv(DATA, "Primary15-19_Cleaned.csv", row.names = F)
## run on both DATA and VSData frames; test on VSData as it is a much shorter data frame
## adjusting opponent classifaction for "conference", "rival", "non-conference_power5", "non-conference_other"
## opponent classification meanings
## conference - any SEC opponent that is not a rival
## rival - Florida, Georgia, Alabama, Vanderbilt, Kentucky
## non-conference_power5 - any opponent matchup that is from Big12, Big10, Pac12, and ACC
## non-conference_other - all other opponents
summary(VSData$game)
levels(VSData$game)
VSData$game <- factor(VSData$game)
summary(VSData$game)
levels(VSData$game)
cons.disc <- c()
x <- VSData$game
for (i in 1:nrow(VSData)) { 
  if(x[i]%in% c("FOOTBALL vs Georgia", "FOOTBALL vs Vanderbilt", "FOOTBALL vs Alabama",
                "FOOTBALL vs Florida", "FOOTBALL vs Kentucky")) { cons.disc[i] <- "rival" }
  if(x[i]%in% c("FOOTBALL vs South Carolina", "FOOTBALL vs LSU", "FOOTBALL vs Missouri", "FOOTBALL vs Mississippi State"
  ) ) { cons.disc[i] <- "conference" }
  if(x[i]%in% c("FOOTBALL vs Indiana St", "FOOTBALL vs Southern Miss", "FOOTBALL vs UMASS",
                "FOOTBALL vs Charlotte", "FOOTBALL vs ETSU", "FOOTBALL vs UTEP", "FOOTBALL vs UAB", "FOOTBALL vs BYU",
                "FOOTBALL vs Chattanooga", "FOOTBALL vs Georgia State"
  ) ) { cons.disc[i] <- "non-conference_other" }
  if(x[i]%in% c()) { cons.disc[i] <- "non-conference_power5" }
}
VSData$game_type <- factor(cons.disc)
summary(VSData$game_type)
levels(VSData$game_type)

# what about original
summary(DATA$game)
levels(DATA$game)
DATA$game <- factor(DATA$game)
summary(DATA$game)
levels(DATA$game)
cons.disc <- c()
x <- DATA$game
for (i in 1:nrow(DATA)) { 
  if(x[i]%in% c("FOOTBALL vs Georgia", "FOOTBALL vs Vanderbilt", "FOOTBALL vs Alabama",
                "FOOTBALL vs Florida", "FOOTBALL vs Kentucky", "Georgia", "Vanderbilt") ) { cons.disc[i] <- "rival" }
  if(x[i]%in% c("FOOTBALL vs South Carolina", "FOOTBALL vs LSU", "FOOTBALL vs Missouri", "FOOTBALL vs Mississippi State", 
                "Arkansas", "South Carolina" ) ) { cons.disc[i] <- "conference" }
  if(x[i]%in% c("FOOTBALL vs Indiana St", "FOOTBALL vs Southern Miss", "FOOTBALL vs UMASS",
                "FOOTBALL vs Charlotte", "FOOTBALL vs ETSU", "FOOTBALL vs UTEP", "FOOTBALL vs UAB", "FOOTBALL vs BYU",
                "FOOTBALL vs Chattanooga", "FOOTBALL vs Georgia State", "North Texas", "Western Carolina", "FOOTBALL vs Appalachian State",
                "FOOTBALL vs Ohio University", "FOOTBALL vs Tennessee Tech"  ) ) { cons.disc[i] <- "non-conference_other" }
  if(x[i]%in% c("Oklahoma" )) { cons.disc[i] <- "non-conference_power5" }
}
DATA$game_type <- factor(cons.disc)
summary(DATA$game_type)
levels(DATA$game_type)

# save cleaned original
write.csv(DATA, "Primary15-19_Cleaned.csv", row.names = FALSE)

## add in win percentage, one "true" category and one with a forgiveness factor
# create wins and losses columns
summary(VSData)
summary(VSData$tennessee_record)
class(VSData$tennessee_record)
VSData$tennessee_record[ which( VSData$tennessee_record == "False")] <- as.factor("3-5")
summary(VSData$tennessee_record)
VSData$tennessee_record <- factor(VSData$tennessee_record)
summary(VSData$tennessee_record)
table(  substr(VSData$tennessee_record,start=1,stop=1) )
VSData$tennessee_wins <-  as.numeric( substr(VSData$tennessee_record,start=1,stop=1) )
table(  substr(VSData$tennessee_record,start=3,stop=3) )
VSData$tennessee_losses <- as.numeric( substr(VSData$tennessee_record,start=3,stop=3) )
class(VSData$tennessee_losses)
class(VSData$tennessee_wins)
# develop "true" winning percentage
table( round(VSData$tennessee_wins / (VSData$tennessee_wins + VSData$tennessee_losses),digits = 4 ) )
VSData$ut_winning_percentage <- round(VSData$tennessee_wins / 
                                        (VSData$tennessee_wins + VSData$tennessee_losses),digits = 4 )
# develop "foregiveness" winning percentage
table( round( (VSData$tennessee_wins + 3) / 
                (VSData$tennessee_wins + VSData$tennessee_losses + 5),digits = 4 ) )
VSData$ut_foregiveness_wp <- round( (VSData$tennessee_wins + 3) / 
                                      (VSData$tennessee_wins + VSData$tennessee_losses + 5),digits = 4 )

# Will need to adjust in different way moving forward

# ## add in opponents record on game day
# summary(VSData$game)
# levels(VSData$game)
# cons.disc <- c()
# x <- VSData$game
# for (i in 1:nrow(VSData)) { 
#   if(x[i]== "FOOTBALL vs Georgia") { cons.disc[i] <- as.character( "4-0") }
#   if(x[i]== "FOOTBALL vs Indiana St") { cons.disc[i] <- as.character( "0-1") }
#   if(x[i]== "FOOTBALL vs LSU") { cons.disc[i] <- as.character( "8-2") }
#   if(x[i]== "FOOTBALL vs South Carolina") { cons.disc[i] <- as.character( "4-2") }
#   if(x[i]== "FOOTBALL vs Southern Miss") { cons.disc[i] <- as.character( "5-3") }
#   if(x[i]== "FOOTBALL vs UMASS") { cons.disc[i] <- as.character( "0-4") }
#   if(x[i]== "FOOTBALL vs Vanderbilt") { cons.disc[i] <- as.character( "4-7") }
#   if(x[i]== "FOOTBALL vs Alabama") { cons.disc[i] <- as.character( "7-0") }
#   if(x[i]== "FOOTBALL vs Charlotte") { cons.disc[i] <- as.character( "4-4") }
#   if(x[i]== "FOOTBALL vs ETSU") { cons.disc[i] <- as.character( "1-0") }
#   if(x[i]== "FOOTBALL vs Florida") { cons.disc[i] <- as.character( "2-1") }
#   if(x[i]== "FOOTBALL vs Kentucky") { cons.disc[i] <- as.character( "7-2") }
#   if(x[i]== "FOOTBALL vs Missouri") { cons.disc[i] <- as.character( "6-4") }
#   if(x[i]== "FOOTBALL vs UTEP") { cons.disc[i] <- as.character( "0-2") }
# }
# VSData$opponent_record <- factor(cons.disc)
# summary(VSData$opponent_record)
# levels(VSData$opponent_record)
# VSData$opponent_record <- as.factor(VSData$opponent_record)
# summary(VSData$opponent_record)
# # opponents wins and losses going into game inorder to create winning percentage
# table(  substr(VSData$opponent_record,start=1,stop=1) ) 
# VSData$opponent_wins <-  as.numeric( substr(VSData$opponent_record,start=1,stop=1) )
# table(  substr(VSData$opponent_record,start=3,stop=3) ) 
# VSData$opponent_losses <- as.numeric( substr(VSData$opponent_record,start=3,stop=3) )
# class(VSData$opponent_losses)
# class(VSData$opponent_wins)
# # now let's add in winning percentage
# table( round(VSData$opponent_wins / (VSData$opponent_wins + VSData$opponent_losses),digits = 4 ) )
# VSData$opponent_winning_percentage <- round(VSData$opponent_wins / 
#                                         (VSData$opponent_wins + VSData$opponent_losses),digits = 4 )

# Vivid_adjusted <- VSData
# VSData <- Vivid_adjusted
# write.csv(Vivid_adjusted, "Secondary_Market_Adjusted.csv", row.names = FALSE)
write.csv(VSData, "Secondary_Market_Adjusted.csv", row.names = FALSE)

# ## remove identifying variables
# VSData$id <- NULL
# VSData$id_year <- NULL
# # VSData$performance_code <- NULL
# 
# 
# ## let's look at some models to see variation!
# summary(VSData)
# 
# row_section_model <- lm(resale_price~section*row, data = VSData)
# summary(row_section_model)
# 
# row_model <- lm(resale_price~row, data = VSData)
# summary(row_model)
# 
# zone_model <- lm(resale_price~price_zone, VSData)
# summary(zone_model)
# 
# wp_model <- lm(resale_price~ut_winning_percentage, VSData)
# summary(wp_model)
# 
# foregiveness_wp_model <- lm(resale_price~ut_foregiveness_wp, VSData)
# summary(foregiveness_wp_model)
# 
# game_num_model <- lm(resale_price~game_number, VSData)
# summary(game_num_model)
# 
# opponent_model <- lm(resale_price~game, VSData)
# summary(opponent_model)
# 
# game_type_model <- lm(resale_price~game_type, VSData)
# summary(game_type_model)

# model <- lm(resale_price~ (. - order_city - order_zip - order_state
#                            - game_date_time - tennessee_wins - tennessee_losses 
#                            - opponent_wins - opponent_losses - tennessee_record 
#                            - opponent_record - opponent_city - opponent_state), VSData)
# summary(model)


## let's try to merge primary and secondary market data
primary17_18_19 <- rbind(Tickets2017, Tickets2018, Tickets2019)
summary(VSData)
summary(primary17_18_19)
# need to clean/fix some of the primary in order to facilitate merge
primary17_18_19 <- primary17_18_19[complete.cases(primary17_18_19),]
summary(primary17_18_19)
primary17_18_19$performance_code <- factor(primary17_18_19$performance_code)
# only keep needed rows from primary
primary_merge <- primary17_18_19[,c(4:8)]
# should be cleaned, let's try to merge
resale17_19 <- merge(primary_merge, VSData, by = c("performance_code", "section", "row", "seat"), all.y = TRUE)
nrow(VSData)                #93779
nrow(resale17_19)                   #93779
length(complete.cases(resale17_19) == T) #93779
# successfully merged primary and all secondary market data! Now we can analyze price v resale price!
write.csv(resale17_19, "Primary_Secondary_Matched.csv", row.names = FALSE)


# need to clean resale17_18 to make it easier for analysis but first simple model attempt
model_1 <- lm(resale_price~game.x+ut_winning_percentage+game_type+
                price_zone, resale17_19)
summary(model_1)

# clean merged data fram some more
summary(resale17_19)
# original.merged <- resale17_18 # save original just in case something goes wrong
# resale17_18 <- original.merged
resale17_18 <- resale17_18[,-c(5, 18, 20:22, 11, 27:35)]

model_data <- resale17_18[,-c(18:20, 7:9, 12, 13, 15, 16, 1)]

# building models to try and predict secondary market prices
M.ratio <- lm( (log(resale_price+2)/log(price+2))~., data = model_data )
# add two to both prices to avoid 0/0 and -inf/-inf
summary(M.ratio)
plot(M.ratio)

M.resale <- lm( log(resale_price+1)~.-price, data = model_data)
# add one to avoid -inf data point
summary(M.resale)
plot(M.resale)


nrow(resale17_19)
length(complete.cases(resale17_19) == T)

summary(resale17_19)
# remove identifying variables
resale17_19$performance_code <- NULL
resale17_19$id <- NULL
resale17_19$id_year <- NULL

# let's run a random forest to see variable classification, but we'll use a limited number of rows to save time!
set.seed(494); training_rows <- sample(1:nrow(resale17_19), 2000)
TRAIN_resale <- resale17_19[training_rows,]; HOLDOUT_resale <- resale17_19[-training_rows,] 

fitControl <- trainControl(method="cv",number=5)

forestGrid <- expand.grid(mtry=c(1,3,5,12))
set.seed(494); FOREST <- train( resale_price~., data = TRAIN_resale, method='rf',
                               tuneGrid=forestGrid, trControl=fitControl, preProc = c("center", "scale"), na.action = na.omit)
summary(FOREST)
varImp(FOREST)
# rf variable importance
# 
# only 20 most important variables shown (out of 9250)
# 
# Overall
# game_typerival                100.00
# conference_gameTrue            69.23
# opponent_rank1/1               66.65
# gameFOOTBALL vs Alabama        51.52
# opponent_ranknone              47.31
# tennessee_record3-3            45.98
# game_number                    43.94
# opponent_cityAthens            36.39
# opponent_cityGainesville       35.10
# gameFOOTBALL vs Florida        32.21
# game_date_time9/30/17 15:30    28.76
# game_date_time9/22/18 19:00    28.50
# opponent_stateFL               27.59
# tennessee_record2-1            26.51
# tvESPN                         26.33
# game_date_time10/20/18 15:30   22.30
# typeIMG Public - Season        19.89
# game_date_time10/5/19 19:00    17.98
# price_zoneSouth Upper (16+)    14.49
# tennessee_record1-1            14.42

set.seed(494); FOREST_log <- train( (log(resale_price))~., data = TRAIN_resale, method='rf',
                                    tuneGrid = forestGrid, trControl = fitControl, preProc = c("center","scale"), na.action = na.omit)
summary(FOREST_log)
varImp(FOREST_log)
# rf variable importance
# 
# only 20 most important variables shown (out of 9250)
# 
# Overall
# tennessee_record3-3            100.00
# opponent_cityGainesville        96.45
# opponent_rank1/1                94.52
# game_typerival                  93.78
# game_date_time11/3/18 16:00     92.36
# opponent_ranknone               86.95
# game_typenon-conference_other   84.23
# tennessee_record3-5             74.67
# tennessee_record2-1             73.93
# game_date_time9/22/18 19:00     69.96
# opponent_stateFL                63.87
# tvESPN                          63.70
# game_number                     58.12
# gameFOOTBALL vs Alabama         51.01
# ut_foregiveness_wp              48.51
# typeIMG Public - Season         47.66
# opponent_stateNC                42.33
# tennessee_losses                41.78
# game_date_time10/20/18 15:30    38.56
# game_date_time11/17/18 15:30    33.75

set.seed(494); FOREST_ratio <- train( (log(resale_price/(price+1)))~., data = TRAIN_resale, method='rf',
                                      tuneGrid = forestGrid, trControl = fitControl, preProc = c("center","scale"), na.action = na.omit)
summary(FOREST_ratio)
varImp(FOREST_ratio)
# rf variable importance
# 
# only 20 most important variables shown (out of 9249)
# 
# Overall
# typeMarketing Comp - Season  100.00
# typeVol Pass - Single Game    69.80
# price_zoneSouth Upper (16+)   38.15
# order_zip38139                29.00
# tennessee_record3-5           27.19
# sectionYY9                    25.73
# order_zip92860                25.30
# order_zip30646                21.69
# sectionC                      20.09
# game_date_time9/30/17 15:30   19.92
# typeIMG Public - Season       18.30
# price_zoneWest Sideline       18.14
# price_zoneEast Sideline       18.13
# gameFOOTBALL vs Florida       16.80
# gameFOOTBALL vs Missouri      16.59
# opponent_cityGainesville      16.36
# order_cityGermantown          14.95
# tennessee_record5-5           13.02
# order_cityNorco               12.62
# order_stateCA                 12.23


new_model <- lm((log(resale_price))~., data = TRAIN_resale)
summary(new_model)

ratio_model <- lm( ( log(resale_price/(price+1)) ) ~., data = TRAIN_resale)
summary(ratio_model)

# AOV <- aov(resale_price~.-seat-price, data = TRAIN_resale)
# TUKEY <- TukeyHSD(AOV)
# multcompLetters4(AOV,TUKEY)


# let's try using SOIL or Bortua package to help with classification
boruta_output <- Boruta( (log(resale_price))~., data = na.omit(TRAIN_resale), doTrace = 2)
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])
print(boruta_signif)
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance") 


boruta_ratio <- Boruta( ( log(resale_price/(price+1)) ) ~., data = na.omit(TRAIN_resale), doTrace = 2 )
boruta_signif_ratio <- names(boruta_ratio$finalDecision[boruta_ratio$finalDecision %in% c("Confirmed", "Tentative")])
print(boruta_signif_ratio)
plot(boruta_ratio, cex.axis=.7, las=2, xlab="", main="Variable Importance") 

boruta_output_norm <- Boruta( resale_price~., data = na.omit(TRAIN_resale), doTrace = 2)
boruta_signif_norm <- names(boruta_output_norm$finalDecision[boruta_output_norm$finalDecision %in% c("Confirmed", "Tentative")])
print(boruta_signif_norm)
plot(boruta_output_norm, cex.axis=.7, las=2, xlab="", main="Variable Importance") 


analysis.original <- resale17_19
# resale17_19 <- analysis.original # in case mistake use this line
# now add in binary variables for team performance, ticket in hand, adjust opponent city and state info and ignore price zones!
variable.names(resale17_19)

# adjust opponent city/state and remove
levels(resale17_19$opponent_city); levels(resale17_19$opponent_state)
summary(resale17_19$opponent_city); summary(resale17_19$opponent_state)
cons.disc <- c()
x <- resale17_19$opponent_city
for (i in 1:nrow(Tickets2015)) { 
  if(x[i]=="Amherst") { cons.disc[i] <- "UMASS" }
  if(x[i]=="Athens") { cons.disc[i] <- "GEORGIA" }
  if(x[i]=="Baton Rouge") { cons.disc[i] <- "LSU" }
  if(x[i]=="Columbia") { cons.disc[i] <- "SOUTH CAROLINA" }
  if(x[i]=="Hattiesburg") { cons.disc[i] <- "SOUTHERN MISS" }
  if(x[i]=="Nashville") { cons.disc[i] <- "VANDERBILT" }
  if(x[i] %in% c("Charlotte", "11/3/18 16:00")) { cons.disc[i] <- "CHARLOTTE" }
  if(x[i]=="Terre Haute") { cons.disc[i] <- "INDIANA ST" }
  if(x[i]=="El Paso") { cons.disc[i] <- "UTEP" }
  if(x[i]=="Gainesville") { cons.disc[i] <- "FLORIDA" }
  if(x[i]=="Johnson City") { cons.disc[i] <- "ETSU" }
  if(x[i]=="Lexington") { cons.disc[i] <- "KENTUCKY" }
  if(x[i]=="Tuscaloosa") { cons.disc[i] <- "ALABAMA" }
  if(x[i]=="Atlanta") { cons.disc[i] <- "GEORGIA STATE" }
  if(x[i]=="Birmingham") { cons.disc[i] <- "UAB" }
  if(x[i]=="Chattanooga") { cons.disc[i] <- "CHATTANOOGA" }
  if(x[i]=="Provo") { cons.disc[i] <- "BYU" }
  if(x[i]=="Starkville") { cons.disc[i] <- "MISSISSIPPI STATE" }
}
resale17_19$opponent <- factor(cons.disc)
summary(resale17_19$opponent)
levels(resale17_19$opponent)

# add in date between primary purchase and game
resale17_19$game_date_time[36003:36004] <- "11/3/18 16:00"
resale17_19$game_date_time <- mdy_hm(resale17_19$game_date_time)

resale17_19$conference_game[36003:36004] <- "False"
resale17_19$order_date_time <- mdy_hm(resale17_19$order_date_time)
resale17_19$ticket_in_hand <- as.duration(as.period( interval( resale17_19$order_date_time, resale17_19$game_date_time ) ))

# # adjust conference game to be T and F
# if( resale17_19$conference_game == "True" ) { resale17_19$conference_game <- T } else { resale17_19$conference_game <- F }


# add in recency performance metrics (previosu 1, 2, & 3 weeks and streak)
summary(resale17_19$performance_code)
x <- resale17_19$performance_code
wk1 <- c()
wk2 <- c()
wk <- c()
streak <- c()
for (i in 1:nrow(Tickets2015)) { 
  if(x[i]=="F17-F01") { wk1[i] <- 1 ; wk2[i] <- 0 ; wk[i] <- 0 ; streak[i] <- 1 }
  if(x[i]=="F17-F02") { wk1[i] <- 0 ; wk2[i] <- 1 ; wk[i] <- 1 ; streak[i] <- -1 }
  if(x[i]=="F17-F03") { wk1[i] <- 1 ; wk2[i] <- 0 ; wk[i] <- 1 ; streak[i] <- 1 }
  if(x[i]=="F17-F04") { wk1[i] <- 0 ; wk2[i] <- 1 ; wk[i] <- 0 ; streak[i] <- -1 }
  if(x[i]=="F17-F05") { wk1[i] <- 0 ; wk2[i] <- 0 ; wk[i] <- 0 ; streak[i] <- -4 }
  if(x[i]=="F17-F06") { wk1[i] <- 0 ; wk2[i] <- 1 ; wk[i] <- 0 ; streak[i] <- -1 }
  if(x[i]=="F17-F07") { wk1[i] <- 0 ; wk2[i] <- 0 ; wk[i] <- 1 ; streak[i] <- -2 }
  if(x[i]=="F18-F01") { wk1[i] <- 0 ; wk2[i] <- 0 ; wk[i] <- 0 ; streak[i] <- -1 }
  if(x[i]=="F18-F02") { wk1[i] <- 1 ; wk2[i] <- 0 ; wk[i] <- 0 ; streak[i] <- 1 }
  if(x[i]=="F18-F03") { wk1[i] <- 1 ; wk2[i] <- 1 ; wk[i] <- 0 ; streak[i] <- 2 }
  if(x[i]=="F18-F04") { wk1[i] <- 1 ; wk2[i] <- 0 ; wk[i] <- 0 ; streak[i] <- 1 }
  if(x[i]=="F18-F05") { wk1[i] <- 0 ; wk2[i] <- 0 ; wk[i] <- 1 ; streak[i] <- -2 }
  if(x[i]=="F18-F06") { wk1[i] <- 1 ; wk2[i] <- 0 ; wk[i] <- 0 ; streak[i] <- 1 }
  if(x[i]=="F18-F07") { wk1[i] <- 1 ; wk2[i] <- 1 ; wk[i] <- 0 ; streak[i] <- 2 }
  if(x[i]=="F19-F01") { wk1[i] <- 0 ; wk2[i] <- 0 ; wk[i] <- 0 ; streak[i] <- 0 }
  if(x[i]=="F19-F02") { wk1[i] <- 0 ; wk2[i] <- 0 ; wk[i] <- 0 ; streak[i] <- -1 }
  if(x[i]=="F19-F03") { wk1[i] <- 0 ; wk2[i] <- 0 ; wk[i] <- 0 ; streak[i] <- -2 }
  if(x[i]=="F19-F04") { wk1[i] <- 0 ; wk2[i] <- 1 ; wk[i] <- 0 ; streak[i] <- -1 }
  if(x[i]=="F19-F05") { wk1[i] <- 0 ; wk2[i] <- 0 ; wk[i] <- 1 ; streak[i] <- -2 }
  if(x[i]=="F19-F06") { wk1[i] <- 0 ; wk2[i] <- 1 ; wk[i] <- 0 ; streak[i] <- -1 }
  if(x[i]=="F19-F07") { wk1[i] <- 1 ; wk2[i] <- 0 ; wk[i] <- 1 ; streak[i] <- 1 }
  if(x[i]=="F19-F08") { wk1[i] <- 1 ; wk2[i] <- 1 ; wk[i] <- 1 ; streak[i] <- 4 }
}
# resale17_19$one_week_previous <- wk1
# resale17_19$two_week_previous <- wk2
# resale17_19$three_week_previous <- wk
resale17_19$streak <- streak
summary(resale17_19); summary(resale17_19$streak)

# remove NA price values
nrow(resale17_19); nrow(resale17_19[which(is.na(resale17_19$price) == T),])
resale17_19 <- resale17_19[-which(is.na(resale17_19$price) == T),]
nrow(resale17_19)

write.csv(resale17_19, "Primary_Secondary_Matched_Cleaned_just streak.csv", row.names = FALSE)
resale17_19$opponent_city <- NULL; resale17_19$opponent_state <- NULL; 
resale17_19$game <- NULL; resale17_19$order_date_time <- NULL
resale17_19$performance_code <- NULL; resale17_19$id_year <- NULL; resale17_19$id <- NULL

# add in bs variable to see if a a variable does worse than a random number
resale17_19$bs <- rnorm(nrow(resale17_19))

resale17_19 <- resale17_19[which(resale17_19$price > 0), ]


library(SOIL)
library(Boruta)
# now that we've cleaned and added let's resample
set.seed(494); training_rows <- sample(1:nrow(resale17_19), 2000)
TRAIN_resale <- resale17_19[training_rows,]; HOLDOUT_resale <- resale17_19[-training_rows,] 
# let's focus on the ratio of the prices instead of all three!
boruta_ratio <- Boruta( ( log(resale_price/(price+1)) ) ~.-price_zone, data = na.omit(TRAIN_resale), doTrace = 2 )
boruta_signif_ratio <- names(boruta_ratio$finalDecision[boruta_ratio$finalDecision %in% c("Confirmed", "Tentative")])
print(boruta_signif_ratio)
plot(boruta_ratio, cex.axis=.7, las=2, xlab="", main="Variable Importance")

soil_data <- resale17_19
r_price <- resale17_19$resale_price
p_price <- resale17_19$price
soil_y <- (log(r_price/(p_price+1)))

soil_x <- resale17_19[,-c(5,8)]
soil_x <- data.matrix(soil_x)

soil_ratio <- SOIL( soil_x, soil_y )
summary(soil_ratio); plot(soil_ratio)

head(soil_y)


# try and develep and potentially check price sensitivity
DEMAND <- data.frame(price_point=rep(0,19), resale_demand=rep(0,19),original_demand=rep(0,19))
DEMAND$price_point <- seq(0,900,50)
y <- DEMAND$price_point
for (i in 1:19) {
  DEMAND$resale_demand[i] <- sum(resale17_19$resale_price <= y[i])
  DEMAND$original_demand[i] <- sum(resale17_19$price <= y[i])
}
summary(DEMAND)
for (i in 2:19) {
  DEMAND$resale_PE[i] <- (DEMAND$resale_demand[i]-DEMAND$resale_demand[i-1])/(DEMAND$price_point[i]-DEMAND$price_point[i-1]) * 
                          (DEMAND$price_point[i]/DEMAND$resale_demand[i])
  DEMAND$original_PE[i] <- (DEMAND$original_demand[i]-DEMAND$original_demand[i-1])/(DEMAND$price_point[i]-DEMAND$price_point[i-1]) * 
                          (DEMAND$price_point[i]/DEMAND$original_demand[i])
}
summary(DEMAND)



# probability of purchase
x <- resale17_19$resale_price
x <- x[complete.cases(x)]
x <- x+1
hist(x)

hist( log(x) )
mean( log(x) )
sd( log(x) )

hist(x, freq = FALSE, breaks = seq(0,900,50))
curve( dlnorm(x,3.951746,0.7870164), add = T, col = 'red', lwd = 3)

# P(X <= 10) = F(10)
plnorm(10,3.951746,0.7870164)
# P(X > 250) = 1 - P(X <= 250)
1 - plnorm(250,3.951746,0.7870164)
# P(100 < X < 200) = P(X <= 200) - P(X <= 100)
plnorm(200,3.951746,0.7870164) - plnorm(100,3.951746,0.7870164)




# boruta without compensated tickets (price > 0)
boruta <- Boruta( ( log(resale_price/(price)) ) ~.-section, data = na.omit(TRAIN_resale), doTrace = 2 )
boruta_signif <- names(boruta_ratio$finalDecision[boruta_ratio$finalDecision %in% c("Confirmed", "Tentative")])
print(boruta_signif)
plot(boruta, cex.axis=.55, las=2, xlab="", main="Variable Importance")

levels(resale17_19$type)


View(resale17_19)

share_data <- resale17_19

for (i in 1:nlevels(share_data$opponent)) {
  levels(share_data$opponent)[i] <- paste("Opponent", i, sep = " ")
}

levels(share_data$opponent)

set.seed(2395); share_rows <- sample(1:nrow(share_data), size = 1000)
SHARE <- share_data[share_rows,]
write.csv(SHARE, "data_to_share.csv", row.names = FALSE)
