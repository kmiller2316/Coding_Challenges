Sys.setenv("plotly_username"="kmiller2316")
Sys.setenv("plotly_api_key"="pGkZyyJjtADSB9NJwVSl")

setwd("~/Job Applications 2019-20/Mariners Application")

library(regclass)
library(dplyr)
library(DescTools)
library(lubridate)

# read in initial data
TRAIN.Original <- read.csv("2020-train.csv")
TEST <- read.csv("2020-test.csv")

############### Coding Challenge Question 1 ##################
##############################################################
# Data cleaning
DATA <- TRAIN.Original
# check all  variables!
summary(DATA)
summary(DATA$inning)
summary(DATA$top_bottom)
summary(DATA$outs)
summary(DATA$balls)
summary(DATA$strikes)
summary(DATA$release_speed)
summary(DATA$vert_release_angle)
summary(DATA$horz_release_angle)
summary(DATA$spin_rate)
summary(DATA$spin_axis)
summary(DATA$tilt) # only issue but is redundant with spin_axis, remove
summary(DATA$rel_height)
summary(DATA$rel_side)
summary(DATA$extension)
summary(DATA$vert_break)
summary(DATA$induced_vert_break)
summary(DATA$horz_break)
summary(DATA$plate_height)
summary(DATA$plate_side)
summary(DATA$zone_speed)
summary(DATA$vert_approach_angle)
summary(DATA$horz_approach_angle)
summary(DATA$zone_time)
summary(DATA$x55)
summary(DATA$y55)
summary(DATA$z55)
summary(DATA$pitch_type)
summary(DATA$pitch_call)
summary(DATA$is_strike)

# create ball or strike variable
summary(DATA$pitch_call)
levels(DATA$pitch_call)
class(DATA$pitch_call)
cons.disc <- c()
x <- DATA$pitch_call
for (i in 1:nrow(DATA)) { 
  if(x[i]%in% c("FoulBall", "InPlay", "StrikeCalled", "StrikeSwinging")) { cons.disc[i] <- "Yes" }
  if(x[i]%in% c("BallCalled", "BallIntentional", "HitByPitch")) { cons.disc[i] <- "No" }
}
DATA$is_strike <- factor(cons.disc)
summary(DATA$is_strike)
levels(DATA$is_strike)

summary(TEST)
summary(DATA)

# clean test and train together so NAs are all replaced accurately
# need to remove pitch_call from training to merge
DATA$pitch_call <- NULL #identifying and not in "TEST"

ALL <- rbind(DATA,TEST)

# replace NA's
summary(ALL)
for (q in which( unlist(lapply(ALL,class) %in% c("numeric","integer")))) {
  ALL[which(is.na(ALL[,q])),q] <- median(ALL[,q],na.rm=TRUE)
}
summary(ALL)

# combine levels in pitcher/batter side
levels(ALL$pitcher_side)
names <- c("Left", "Left","Right", "Right")
levels(ALL$pitcher_side) <- names
levels(ALL$pitcher_side)

levels(ALL$batter_side)
levels(ALL$batter_side) <- names
levels(ALL$batter_side)

summary(ALL)

# break back into training and test
TRAIN.CLEAN <- ALL[1:nrow(DATA),]
TEST.CLEAN <- ALL[-(1:nrow(DATA)),]

# remove identifying and redundant and zero variance variables
TRAIN.CLEAN$pitch_id <- NULL #identifying
TRAIN.CLEAN$tilt <- NULL #redundant with spin_axis
TRAIN.CLEAN$pitcher_id <- NULL #zero variance variable
TRAIN.CLEAN$batter_id <- NULL #zero variance variable
TRAIN.CLEAN$stadium_id <- NULL #zero variance variable
TRAIN.CLEAN$pitch_type <- NULL #zero variance variable
TRAIN.CLEAN$catcher_id <- NULL #zero variance variable
TRAIN.CLEAN$umpire_id <- NULL # zero variance variable
TRAIN.CLEAN$y55 <- NULL #zero variance variable
summary(TRAIN.CLEAN)

# break TRAIN dataset into training and holdout samples
set.seed(9523); train.rows <- sample(1:nrow(TRAIN.CLEAN),5000)  #get 500 rows to play around with
TRAIN <- TRAIN.CLEAN[train.rows,] 
HOLDOUT <- TRAIN.CLEAN[-train.rows,] 

# model building time 
# (using basic parameters on all models, once identify best then will adjust to fit best model)
library(caret)
library(pROC)

# vanilla regression

set.seed(9523); GLM <- train(is_strike~.,data=TRAIN,method='glm',
                            trControl=fitControl,preProc=c("center", "scale") )
GLM$results
# ROC 0.5979194
varImp(GLM)


fitControl <- trainControl(method="cv",number=5, classProbs=TRUE, 
                           summaryFunction=twoClassSummary)

glmnetGrid <- expand.grid(alpha = seq(0,1,.05),lambda = 10^seq(-4,-1,length=10))

# Regularized Logistic Regression

set.seed(9523); GLMnet <- train(is_strike~., data=TRAIN,method='glmnet', trControl=fitControl, 
                                tuneGrid=glmnetGrid, preProc = c("center", "scale"))

GLMnet  
plot(GLMnet) 
GLMnet$bestTune 
GLMnet$results
GLMnet$results[rownames(GLMnet$bestTune),]  
postResample(predict(GLMnet,newdata=HOLDOUT),HOLDOUT$is_strike) 
# accuracy 0.65963046
mean(HOLDOUT$is_strike=="Yes")  #Accuracy of naive model that predicts every pitch is a strike
# 0.6372323

roc(HOLDOUT$is_strike,predict(GLMnet,newdata=HOLDOUT,type="prob")[,2])
# Area under the curve: 0.5894


# vanilla partition (trees looking like best option)
treeGrid <- expand.grid(cp=10^seq(-5,-1,length=25))

set.seed(9523); TREE <- train(is_strike~.,data=TRAIN,method='rpart', tuneGrid=treeGrid,
                             trControl=fitControl, preProc = c("center", "scale"))
TREE  
plot(TREE) 
TREE$bestTune 
TREE$results 
TREE$results[rownames(TREE$bestTune),] 

postResample(predict(TREE,newdata=HOLDOUT),HOLDOUT$is_strike)
# Accuracy of tree with cp=0.001467799   0.8404068


# random forest (looks very good as well; worried about overfit a little;
# will test with increased mtry)
# forestGrid <- expand.grid(mtry=c(1,3,5,12)) first iteration of mtry
forestGrid <- expand.grid(mtry=c(12,15,20))
set.seed(9523); FOREST <- train(is_strike~.,data=TRAIN,method='rf',tuneGrid=forestGrid,
                                trControl=fitControl, preProc = c("center", "scale"))
FOREST  
plot(FOREST) 
FOREST$bestTune 
FOREST$results 
FOREST$results[rownames(FOREST$bestTune),]

postResample(predict(FOREST,newdata=HOLDOUT),HOLDOUT$is_strike)
# Accuracy 0.8517407 with mtry=12
# with two trys varrying mtry, 12 is still best tune for random forest


# Boosted Tree Model (boosted looks best)
# gbmGrid <- expand.grid(n.trees=c(100,200,500),interaction.depth=1:4,
#                        shrinkage=c(.01,.001),n.minobsinnode=c(5,10))
# 
# set.seed(9523); GBM <- train(is_strike~.,data=TRAIN, method='gbm',tuneGrid=gbmGrid,verbose=FALSE,
#                             trControl=fitControl, preProc = c("center", "scale"))
# 
# GBM  
# plot(GBM) 
# GBM$bestTune 
# GBM$results 
# GBM$results[rownames(GBM$bestTune),]  
# library(gbm)
# varImp(GBM)
# 
# postResample(predict(GBM,newdata=HOLDOUT,n.trees=500),HOLDOUT$is_strike)
# # Accuracy 0.8538422
# 
# #Boosted tree testing adjustments!
# gbmGrid <- expand.grid(n.trees=seq(100,10000,50),interaction.depth=5,
#                        shrinkage=0.001,n.minobsinnode=5)
# set.seed(9523); GBM <- train(is_strike~.,data=TRAIN, method='gbm',tuneGrid=gbmGrid,verbose=FALSE,
#                              trControl=fitControl, preProc = c("center", "scale"))
# GBM  
# plot(GBM) 
# GBM$bestTune 
# GBM$results 
# GBM$results[rownames(GBM$bestTune),] 
# varImp(GBM)
# postResample(predict(GBM,newdata=HOLDOUT,n.trees=8950),HOLDOUT$is_strike)
# # Accuracy 0.8582739 leaving interaction @ 5, shrinkage @ 0.001, & nodes @ 5 
# 
# gbmGrid <- expand.grid(n.trees=8950,interaction.depth=1:6,
#                        shrinkage=0.001,n.minobsinnode=5)
# set.seed(9523); GBM <- train(is_strike~.,data=TRAIN, method='gbm',tuneGrid=gbmGrid,verbose=FALSE,
#                              trControl=fitControl, preProc = c("center", "scale"))
# GBM  
# plot(GBM) 
# GBM$bestTune 
# GBM$results 
# GBM$results[rownames(GBM$bestTune),] 
# varImp(GBM)
# postResample(predict(GBM,newdata=HOLDOUT,n.trees=8950),HOLDOUT$is_strike)
# # Accuracy 0.8582064 interaction depth @ 5, shrinkage @ 0.001, & nodes @ 5
# 
# gbmGrid <- expand.grid(n.trees=seq(8950,20050,100),interaction.depth=5,
#                        shrinkage=seq(0.0001,0.001,length = 20),n.minobsinnode=5)
# set.seed(9523); GBM <- train(is_strike~.,data=TRAIN, method='gbm',tuneGrid=gbmGrid,verbose=FALSE,
#                              trControl=fitControl, preProc = c("center", "scale"))
# GBM  
# plot(GBM) 
# GBM$bestTune 
# GBM$results 
# GBM$results[rownames(GBM$bestTune),] 
# varImp(GBM)
# postResample(predict(GBM,newdata=HOLDOUT,n.trees=10150),HOLDOUT$is_strike)
# Accuracy 0.8582964 with trees 10150, interaction depth @ 5, shrinkage @ 0.0009526316, & nodes @ 5

# FINAL Model specifications
gbmGrid <- expand.grid(n.trees=10150,interaction.depth=5,
                       shrinkage=0.0009526316,n.minobsinnode=5)
set.seed(9523); GBM <- train(is_strike~.,data=TRAIN, method='gbm',tuneGrid=gbmGrid,verbose=FALSE,
                             trControl=fitControl, preProc = c("center", "scale"))
GBM  
plot(GBM) 
GBM$bestTune 
GBM$results 
GBM$results[rownames(GBM$bestTune),] 
varImp(GBM)
postResample(predict(GBM,newdata=HOLDOUT,n.trees=10150),HOLDOUT$is_strike)
# Accuracy 0.8585373 with trees 10150, interaction depth @ 5, shrinkage @ 0.0009526316, & nodes @ 5

# Run predictions on the Test.Clean dataset for submission and change from Yes/No to 1s and 0s
finalpredictions <- predict(GBM,newdata=TEST.CLEAN)
length(finalpredictions)  #Should be 145552 items! Awesome it checks out
replacement <- c()
for (i in 1:length(finalpredictions)) {
  if (finalpredictions[i] == "Yes") { replacement[i] <- 1 }
  if (finalpredictions[i] == "No") { replacement[i] <- 0 }
}
TEST.CLEAN$is_strike <- replacement
summary(TEST.CLEAN)
summary(TEST.CLEAN$is_strike)
mean(complete.cases(TEST.CLEAN)) # 1, good to go
mean(TEST.CLEAN$is_strike == 1) # 0.6267863
write.csv(TEST.CLEAN,file="my_final_strike_predictions.csv", row.names = FALSE)


###################################################################################################


################# Coding Challenge Question 2 ####################
##################################################################
# catcher_id f06c9fdf
library(plotly)

# create subset of only our new catcher
# can still use DATA to review all catcher strike tendencies!
# add back in pitch_call
DATA$pitch_call <- TRAIN.Original[,35]
ACQUISITION <- subset(DATA, catcher_id == "f06c9fdf")
length(which(DATA$catcher_id == "f06c9fdf")) # 8822
nrow(ACQUISITION) # 8822
summary(ACQUISITION) # some NA's in catcher set but really only using and reviewing is_strike

RECEIVED <- subset(ACQUISITION, pitch_call %in% c("BallCalled", "StrikeCalled"))
RECEIVED$strike <- ifelse(RECEIVED$is_strike == "Yes", "Strike","Ball")

pal <- c("blue", "red")
p <- plot_ly(RECEIVED, type = "scatter", mode = "markers",
             x = ~plate_side, y = ~plate_height, color = ~strike, colors = pal)
 
skill <- layout(p, title = "New Catcher Pitch Receiving", 
                xaxis = list(title = "Plate Position", range = c(-3,3)),
                yaxis = list(title = "Height at Plate", range = c(0,6.5)),
                legend = list(font = list(family = "sans-serif",
                                          size = 20,
                                          color = "black")),
                shapes = list(
                  list(type = "rect",
                       line = list(color = "black"),
                       x0 = -8.5/12, x1 = 8.5/12, xref = "x",
                       y0 = 1.75, y1 = 3.42, yref= "y")
                ))
skill # creates plot with strike zone and ball/strike plot to see catcher"s receiving

zone_link <- api_create(skill, filename = "pitch_receiving", sharing = "public")
zone_link


# let's look at the his success of receiving inside and outside the zone
summary(RECEIVED)
# ifelse(RECEIVED$plate_side >= (-8.5/12) & RECEIVED$plate_side <= (8.5/12) &
#          RECEIVED$plate_height >= (1.75) & RECEIVED$plate_height <= (3.42), "In", "Out")
RECEIVED$in_out_zone <- as.factor(ifelse(RECEIVED$plate_side >= (-8.5/12) & 
                                           RECEIVED$plate_side <= (8.5/12) &
                                           RECEIVED$plate_height >= (1.75) & 
                                           RECEIVED$plate_height <= (3.42), "In", "Out"))
summary(RECEIVED$in_out_zone)
summary(RECEIVED$is_strike)

length(which(RECEIVED$in_out_zone == "In" & RECEIVED$is_strike == "No"))
# 61 pitches in the zone were called balls
length(which(RECEIVED$in_out_zone == "Out" & RECEIVED$is_strike == "Yes"))
# 462 pitches outside of the zone were called strikes

# percentage of pitches in the zone called balls  (Ball_In_Zone_Total)
round(((length(which(RECEIVED$in_out_zone == "In" & RECEIVED$is_strike == "No")))/
         (length(which(RECEIVED$in_out_zone == "In"))))*100, digits = 2)
# 6.22% of pitches in the zone will be called balls with catcher f06c9fdf behind the plate

# percentage of pitches outside the zone called strikes (Strike_Outside_Zone_Total)
round(((length(which(RECEIVED$in_out_zone == "Out" & RECEIVED$is_strike == "Yes")))/
         (length(which(RECEIVED$in_out_zone == "Out"))))*100, digits = 2)
# 11.99% of pitches outside the zone are called strikes with catcher f06c9fdf behind the plate

# percent of strikes called that are outside the zone  (Ks_Called_Outside)
round(((length(which(RECEIVED$in_out_zone == "Out" & RECEIVED$is_strike == "Yes")))/
         (length(which(RECEIVED$is_strike == "Yes"))))*100, digits = 2)
# 33.45% of all strikes called were initally out of the zone with catcher f06c9fdf behind the plate

# percent of balls called that are inside the zone  (Balls_Called_In)
round(((length(which(RECEIVED$in_out_zone == "In" & RECEIVED$is_strike == "No")))/
         (length(which(RECEIVED$is_strike == "No"))))*100, digits = 2)
# 1.77% of all balls were intially in the striek zone with catcher f06c9fdf behind the plate

levels(DATA$catcher_id)
nlevels(DATA$catcher_id)

PERF <- data.frame(Catcher=rep(0,119),Ball_In_Zone_Total=rep(0,119),
                   Strike_Outside_Zone_Total=rep(0,119),Ks_Called_Outside=rep(0,119),
                   Balls_Called_In=rep(0,119))

for (i in 1:nlevels(DATA$catcher_id)) {
  D <- subset(DATA, catcher_id == levels(DATA$catcher_id)[i])
  R <- subset(D, pitch_call %in% c("BallCalled", "StrikeCalled"))
  R$in_out_zone <- as.factor(ifelse(R$plate_side >= (-8.5/12) & 
                                      R$plate_side <= (8.5/12) & 
                                      R$plate_height >= (1.75) & 
                                      R$plate_height <= (3.42), "In", "Out"))
  PERF[i,1] <- levels(DATA$catcher_id)[i]
  PERF[i,2] <- round(((length(which(R$in_out_zone == "In" & R$is_strike == "No")))/
                        (length(which(R$in_out_zone == "In"))))*100, digits = 2)
  PERF[i,3] <- round(((length(which(R$in_out_zone == "Out" & R$is_strike == "Yes")))/
                        (length(which(R$in_out_zone == "Out"))))*100, digits = 2)
  PERF[i,4] <- round(((length(which(R$in_out_zone == "Out" & R$is_strike == "Yes")))/
                        (length(which(R$is_strike == "Yes"))))*100, digits = 2)
  PERF[i,5] <- round(((length(which(R$in_out_zone == "In" & R$is_strike == "No")))/
                        (length(which(R$is_strike == "No"))))*100, digits = 2)
  
}

PERF
PERF <- PERF[complete.cases(PERF),]
PERF
summary(PERF)
# Ball_In_Zone_Total Strike_Outside_Zone_Total Ks_Called_Outside Balls_Called_In
# Min.   : 0.000     Min.   : 0.00             Min.   : 0.00     Min.   :0.000  
# 1st Qu.: 3.680     1st Qu.:13.27             1st Qu.:34.16     1st Qu.:1.140  
# Median : 4.990     Median :14.44             Median :36.94     Median :1.500  
# Mean   : 4.713     Mean   :14.48             Mean   :36.73     Mean   :1.453  
# 3rd Qu.: 5.910     3rd Qu.:15.94             3rd Qu.:39.58     3rd Qu.:1.770  
# Max.   :11.110     Max.   :20.63             Max.   :57.89     Max.   :3.850  

# Catcher "f06c9fdf"
# Ball_In_Zone_Total Strike_Outside_Zone_Total Ks_Called_Outside Balls_Called_In
#       6.22                    11.99                33.45             1.77




