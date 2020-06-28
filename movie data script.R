
#import libraries
library(tidyverse)
library(janitor)
library(GGally)
library(readxl)
library(dummies)
library(dplyr)
library(tidyr)
library(ggplot2)
library(MASS)
library(summarytools)
library(randomForest)

#reading in the movie data through a csv file
movie_data <- read.csv(file.choose())

#modify headers
header_names <- c("Attendance",
                  "Importance_of_Movie_Going",
                  "Movies_Month",
                  "Food_Spent",
                  "I_Arcade",
                  "I_Food",
                  "I_Washroom",
                  "I_Chairs",
                  "I_Seating",
                  "I_Screen_Size",
                  "I_Sound2",
                  "I_Num_Screens",
                  "I_Clean_Washrooms",
                  "Drive_More",
                  "S_Newspaper",
                  "S_Internet",
                  "S_Phone_In",
                  "S_TV",
                  "S_Friends",
                  "S_Other",
                  "P_Internet",
                  "P_Movie_Now",
                  "P_Movie_Later",
                  "P_Other",
                  "Active",
                  "Social",
                  "Ethnicity",
                  "Gender",
                  "Uni",
                  "Age")
for (i in 1:30){
  names(movie_data)[i+1] <- header_names[i]
}

#cleaned up incorrect data
movie_data$Drive_More[movie_data$Drive_More>4] <- 4
movie_data$Active[movie_data$Active>4] <- 4
movie_data$Social[movie_data$Social>4] <- 4

#correlation
#not treating the variables as simply continuous variable, instead treating them as ranks
ggcorr(movie_data)

#turn continuous variables to numeric types and nominal variables to factor type
movie_data$Movies_Month <- as.numeric(movie_data$Movies_Month)
names <- c(2:3,5:31)
movie_data[,names] <- lapply(movie_data[,names],factor)
str(movie_data)


# separate the data into those who attend regularly and those who don't
q1_no <- filter(movie_data, Attendance == 0)
q1_yes <- filter(movie_data, Attendance == 1)


#omit rows with >1 NA values
q1_yes <- na.omit(q1_yes)

#filter the data between females and males for comparison later
female <- filter(q1_yes, Gender == 1)
male <- filter(q1_yes, Gender == 0)

#data frame summary
view(dfSummary(movie_data))
view(dfSummary(q1_yes))
view(dfSummary(q1_no))

#Chi-square test

# Test 1: Is ethnicity related to Movie attendance 
table1 <- table(movie_data$Ethnicity, movie_data$Attendance)
table1
chisq.test(table1)
fisher.test(table1)
# p value = .1705, ethnicity and movie attendance have no relationship 
  
# Test 2: Is University year related to Movie Attendance?
table2 <- table(movie_data$Uni, movie_data$Attendance)
table2
chisq.test(table2)
#p value = .000005858, there is a significant relationship between Uni year and Attendance
  
#Test 3: Is Gender related to Movie Attendance? 
table3 <- table(movie_data$Gender, movie_data$Attendance)
table3
chisq.test(table3)
#pvalue =.5056, there is no relationship between Gender and movie attendance 
  

#Two Sample T-test: Does # of movies watched per month differ for Males and females?
female_monthly_movies <- female$Movies_Month
male_monthly_movies <- male$Movies_Month
male_female_Movies_per_Month <- t.test(female_monthly_movies, male_monthly_movies, var.eqal = TRUE)
male_female_Movies_per_Month
# p = 0.8378 : There is no difference in # of movies watched

#Anova Test

# Anova Test 1: Is there a difference bw #movies/month and age groups
#q1_yes$Age <- as.factor(q1_yes$Age)
aggregate(Movies_Month ~ Age, q1_yes, mean)
anova1 <- aov(Movies_Month ~ Age, data = q1_yes)
summary(anova1)
TukeyHSD(anova1)
#P value less than .05, reject the null hypothesis= the groups are different
#the greatest difference is between age 5-1, 5-2 and 5-3

#Anova Test 2: Is there a difference bw #movies/month and Uni
#q1_yes$Uni <- as.factor(q1_yes$Uni)
aggregate(Movies_Month ~ Uni, q1_yes, mean)
anova2 <- aov(Movies_Month ~ Uni, data = q1_yes)
summary(anova2)
TukeyHSD(anova2)
#P value less than.05= Uni groups are different
# The greatest difference is between year 4-1, 4-2 and 5-2

#Anova Test 3: Difference in Ethnic group means and #movies/month
q1_yes$Ethnicity <- as.factor(q1_yes$Ethnicity)
aggregate(Movies_Month ~ Ethnicity, q1_yes, mean)
anova3 <- aov(Movies_Month ~ Ethnicity, data = q1_yes)
summary(anova3)
TukeyHSD(anova3)
#there is no difference in group means


# Anova test 4: Difference in Active group means and #Movies/Month
#q1_yes$Active <- as.factor(q1_yes$Active)
aggregate(Movies_Month ~ Active, q1_yes, mean)
anova4 <- aov(Movies_Month ~ Active, data = q1_yes)
summary(anova4)
TukeyHSD(anova4)
#there is no difference in group means

# Anova test 5 : Difference in Social group means and #Movies/Month
#q1_yes$Social <- as.factor(q1_yes$Social)
aggregate(Movies_Month ~ Social, q1_yes, mean)
anova5 <- aov(Movies_Month ~ Social, data = q1_yes)
summary(anova5)
TukeyHSD(anova5)
#there is no difference in group means

# Anova test 6 : Difference in Drive More group means and #Movies/Month
#q1_yes$Drive_More <- as.factor(q1_yes$Drive_More)
aggregate(Movies_Month ~ Drive_More, q1_yes, mean)
anova6 <- aov(Movies_Month ~ Drive_More, data = q1_yes)
summary(anova6)
TukeyHSD(anova6)
# There is a difference in group means, 
#The greatest difference is between group 3-2, 1-0, if you were willing to drive further, you would watch less movies


#Anova test 7 : Difference in Food Spend group means and #Movies/Month
#q1_yes$Food_Spent <- as.factor(q1_yes$Food_Spent)
aggregate(Movies_Month ~ Food_Spent, q1_yes, mean)
anova7 <- aov(Movies_Month ~ Food_Spent, data = q1_yes)
summary(anova7)
TukeyHSD(anova7)
# there is a difference in Food spent groups and #Movies/Month
# Particularly, those who spent $7.50-$15 watched less movies than those who spent $0 or 7.50


# Test 8: Does #Movies/month depend on importance of Movie going 
aggregate(Movies_Month ~ Importance_of_Movie_Going, q1_yes, mean)
anova8 <- aov(Movies_Month ~ Importance_of_Movie_Going, data =q1_yes)
summary(anova8)
TukeyHSD(anova8)
#there is a difference for importance of movie going groups
# particularily, those who voted movie going is very important go more frequently than those who voted somehat unimportant



#regression

q1_yes[,names] <- lapply(q1_yes[,names],as.integer)
str(q1_yes)

#create data file with dummy variables for question 11
BinaryEthnicity = cbind(q1_yes, dummy(q1_yes$Ethnicity))
#a warning message will appear here due to the package being used not being updated since 2012
# go to https://tinyurl.com/y74fm5te for more information

#q1_yes1 = caucasian
#q1_yes2 = asian
#q1_yes3 = aboriginal

#Regression Analysis 1 Monthly Attendance
Reg1 <- lm(Movies_Month~Active+Social+q1_yes1+q1_yes2+q1_yes3+Gender+Uni+Age, data=BinaryEthnicity)
summary(Reg1)
#delete Age
Reg1 <- lm(Movies_Month~Active+Social+q1_yes1+q1_yes2+q1_yes3+Gender+Uni, data=BinaryEthnicity)
summary(Reg1)
#delete q1_yes1
Reg1 <- lm(Movies_Month~Active+Social+q1_yes2+q1_yes3+Gender+Uni, data=BinaryEthnicity)
summary(Reg1)
#delete q1_yes2
Reg1 <- lm(Movies_Month~Active+Social+q1_yes3+Gender+Uni, data=BinaryEthnicity)
summary(Reg1)
#delete Gender
Reg1 <- lm(Movies_Month~Active+Social+q1_yes3+Uni, data=BinaryEthnicity)
summary(Reg1)
#delete q1_yes3
Reg1 <- lm(Movies_Month~Active+Social+Uni, data=BinaryEthnicity)
summary(Reg1)
#delete Social
Reg1 <- lm(Movies_Month~Active+Uni, data=BinaryEthnicity)
summary(Reg1)
#delete Active
Reg1 <- lm(Movies_Month~Uni, data=BinaryEthnicity)
summary(Reg1)

#Regression Analysis 2 Spending Habits
Reg2 <-lm(Food_Spent~Active+Social+q1_yes1+q1_yes2+q1_yes3+Gender+Uni+Age,data=BinaryEthnicity)
summary(Reg2)
#delete Active
Reg2 <-lm(Food_Spent~Social+q1_yes1+q1_yes2+q1_yes3+Gender+Uni+Age,data=BinaryEthnicity)
summary(Reg2)
#delete q1_yes3
Reg2 <-lm(Food_Spent~Social+q1_yes1+q1_yes2+Gender+Uni+Age,data=BinaryEthnicity)
summary(Reg2)
#delete q1_yes2
Reg2 <-lm(Food_Spent~Social+q1_yes1+Gender+Uni+Age,data=BinaryEthnicity)
summary(Reg2)
#delete Gender
Reg2 <-lm(Food_Spent~Social+q1_yes1+Uni+Age,data=BinaryEthnicity)
summary(Reg2)
#delete Social
Reg2 <-lm(Food_Spent~q1_yes1+Uni+Age,data=BinaryEthnicity)
summary(Reg2)
#delete Uni
Reg2 <-lm(Food_Spent~q1_yes1+Age,data=BinaryEthnicity)
summary(Reg2)
#delete q1_yes1
Reg2 <-lm(Food_Spent~Age,data=BinaryEthnicity)
summary(Reg2)

q1_yes[,names] <- lapply(q1_yes[,names],as.factor)
str(q1_yes)



#mode function
getMode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Create an empty data fame to store frequency data
#Created new variables for further analyze how each gender and uni year interacts with the data
#Importance_of_Movie_Going is the mode of how the group rated going to movies
#Average_Movie_Watched is the average number a person in the group would watch the movie in a month
#Food_Spent, I_Arcade, I_Food, ..., S_Other is the mode of the responses in that category from the group
#Food_Spent_Frequency_4, I_Arcade_Frequency_4, ..., S_Other_Frequency_4 looks at how frequent the group answered 4 to that category
frequency_data <- data.frame(Gender = factor(),
                             Uni=factor(),
                             Total_Individuals = integer(),
                             Importance_of_Movie_Going = factor(),
                             Movie_Month = integer(),
                             Average_Movie_Watched = double(),
                             Food_Spent = factor(),
                             Food_Spent_Frequency_4 = double(),
                             I_Arcade = factor(),
                             I_Arcade_Frequency_4 = double(),
                             I_Food = factor(),
                             I_Food_Frequency_4 = double(),
                             I_Washroom = factor(),
                             I_Washroom_Frequency_4 = double(),
                             I_Chairs = factor(),
                             I_Chairs_Frequency_4 = double(),
                             I_Seating = factor(),
                             I_Seating_Frequency_4 = double(),
                             I_Screen_Size = factor(),
                             I_Screen_Size_Frequency_4 = double(),
                             I_Sound2 = factor(),
                             I_Sound2_Frequency_4 = double(),
                             I_Num_Screens = factor(),
                             I_Num_Screens_Frequency_4 = double(),
                             I_Clean_Washrooms = factor(),
                             I_Clean_Washrooms_Frequency_4 = double(),
                             S_Newspaper = factor(),
                             S_Newspaper_Frequency_4 = double(),
                             S_Internet = factor(),
                             S_Internet_Frequency_4 = double(),
                             S_Phone_In = factor(),
                             S_Phone_In_Frequency_4 = double(),
                             S_TV = factor(),
                             S_TV_Frequency_4 = double(),
                             S_Friends = factor(),
                             S_Friends_Frequency_4 = double(),
                             S_Other = factor(),
                             S_Other_Frequency_4 = double())

# frequency analysis for how each gender in each year interacts with the movie theatre
for (i in 1:5){
  
  #-------------female------------------
  
  #find the values to the new variables
  female_year_filter <- filter(female, Uni == i)
  numOfIndividuals <- nrow(female_year_filter)
  modeOfImportanceOfMovieGoing <- getMode(female_year_filter$Importance_of_Movie_Going)
  MovieWatchedPerMonth <- sum(female_year_filter$Movies_Month)
  AverageMoviedWatched <-MovieWatchedPerMonth/numOfIndividuals
  modeOfFoodSpent <-getMode(female_year_filter$Food_Spent)
  FoodSpentFrequency4 <- sum(female_year_filter$Food_Spent == 4) / numOfIndividuals
  modeOfIArcade <- getMode(female_year_filter$I_Arcade)
  IArcadeFrequency4 <- sum(female_year_filter$I_Arcade == 4) / numOfIndividuals
  modeOfIFood <- getMode(female_year_filter$I_Food)
  IFoodFrequency4 <- sum(female_year_filter$I_Food == 4) / numOfIndividuals
  modeOfIWashroom <- getMode(female_year_filter$I_Washroom)
  IWashroomFrequency4 <- sum(female_year_filter$I_Washroom == 4) / numOfIndividuals
  modeOfIChairs <- getMode(female_year_filter$I_Chairs)
  IChairsFrequency4 <- sum(female_year_filter$I_Chairs == 4) / numOfIndividuals
  modeOfISeating <- getMode(female_year_filter$I_Seating)
  ISeatingFrequency4 <- sum(female_year_filter$I_Seating == 4) / numOfIndividuals
  modeOfIScreenSize <- getMode(female_year_filter$I_Screen_Size)
  IScreenSizeFrequency4 <-sum(female_year_filter$I_Screen_Size == 4) / numOfIndividuals
  modeOfISound2 <- getMode(female_year_filter$I_Sound2)
  ISound2Frequency4 <- sum(female_year_filter$I_Sound2 == 4) / numOfIndividuals
  modeOfINumScreens <- getMode(female_year_filter$I_Num_Screens)
  INumScreensFrequency4 <- sum(female_year_filter$I_Num_Screens == 4) / numOfIndividuals
  modeOfICleanWashrooms <- getMode(female_year_filter$I_Clean_Washrooms)
  ICleanWashroomsFrequency4 <- sum(female_year_filter$I_Clean_Washrooms == 4) / numOfIndividuals
  modeOfSNewspaper <- getMode(female_year_filter$S_Newspaper)
  SNewspaperFrequency4 <- sum(female_year_filter$S_Newspaper == 4) / numOfIndividuals
  modeOfSInternet <- getMode(female_year_filter$S_Internet)
  SInternetFrequency4 <- sum(female_year_filter$S_Internet == 4) / numOfIndividuals
  modeOfSPhoneIn <- getMode(female_year_filter$S_Phone_In)
  SPhoneInFrequency4 <- sum(female_year_filter$S_Phone_In == 4) / numOfIndividuals
  modeOfSTV <- getMode(female_year_filter$S_TV)
  STVFrequency4 <- sum(female_year_filter$S_TV == 4) / numOfIndividuals
  modeOfSFriends <- getMode(female_year_filter$S_Friends)
  SFriendsFrequency4 <- sum(female_year_filter$S_Friends == 4) / numOfIndividuals
  modeOfSOther <- getMode(female_year_filter$S_Other)
  SOtherFrequency4 <- sum(female_year_filter$S_Other == 4) / numOfIndividuals
  
  #creates a data frame from the new variables
  female_data   <- data.frame(1,
                              i,
                              numOfIndividuals,
                              modeOfImportanceOfMovieGoing,
                              MovieWatchedPerMonth,
                              AverageMoviedWatched,
                              modeOfFoodSpent,
                              FoodSpentFrequency4,
                              modeOfIArcade,
                              IArcadeFrequency4,
                              modeOfIFood,
                              IFoodFrequency4,
                              modeOfIWashroom,
                              IWashroomFrequency4,
                              modeOfIChairs,
                              IChairsFrequency4,
                              modeOfISeating,
                              ISeatingFrequency4,
                              modeOfIScreenSize,
                              IScreenSizeFrequency4,
                              modeOfISound2,
                              ISound2Frequency4,
                              modeOfINumScreens,
                              INumScreensFrequency4,
                              modeOfICleanWashrooms,
                              ICleanWashroomsFrequency4,
                              modeOfSNewspaper,
                              SNewspaperFrequency4,
                              modeOfSInternet,
                              SInternetFrequency4,
                              modeOfSPhoneIn,
                              SPhoneInFrequency4,
                              modeOfSTV,
                              STVFrequency4,
                              modeOfSFriends,
                              SFriendsFrequency4,
                              modeOfSOther,
                              SOtherFrequency4)
  
  #renames the new data frame
  frequency_names   <- c("Gender",
                         "Uni",
                         "Total_Individuals",
                         "Importance_of_Movie_Going",
                         "Movie_Month",
                         "Average_Movie_Watched",
                         "Food_Spent",
                         "Food_Spent_Frequency_4",
                         "I_Arcade",
                         "I_Arcade_Frequency_4",
                         "I_Food",
                         "I_Food_Frequency_4",
                         "I_Washroom",
                         "I_Washroom_Frequency_4",
                         "I_Chairs",
                         "I_Chairs_Frequency_4",
                         "I_Seating",
                         "I_Seating_Frequency_4",
                         "I_Screen_Size",
                         "I_Screen_Size_Frequency_4",
                         "I_Sound2",
                         "I_Sound2_Frequency_4",
                         "I_Num_Screens",
                         "I_Num_Screens_Frequency_4",
                         "I_Clean_Washrooms",
                         "I_Clean_Washrooms_Frequency_4",
                         "S_Newspaper",
                         "S_Newspaper_Frequency_4",
                         "S_Internet",
                         "S_Internet_Frequency_4",
                         "S_Phone_In",
                         "S_Phone_In_Frequency_4",
                         "S_TV",
                         "S_TV_Frequency_4",
                         "S_Friends",
                         "S_Friends_Frequency_4",
                         "S_Other",
                         "S_Other_Frequency_4")
  for (j in 1:38){
    names(female_data)[j] <- frequency_names[j]
  }
  
  #adds the new dataframe to frequency_data
  frequency_data <- rbind(frequency_data,female_data)
  
  #--------------------end of female -------------------------
  
  
  #-------------male------------------
  
  #find the values to the new variables
  male_year_filter <- filter(male, Uni == i)
  numOfIndividuals <- nrow(male_year_filter)
  modeOfImportanceOfMovieGoing <- getMode(male_year_filter$Importance_of_Movie_Going)
  MovieWatchedPerMonth <- sum(male_year_filter$Movies_Month)
  AverageMoviedWatched <-MovieWatchedPerMonth/numOfIndividuals
  modeOfFoodSpent <-getMode(male_year_filter$Food_Spent)
  FoodSpentFrequency4 <- sum(male_year_filter$Food_Spent == 4) / numOfIndividuals
  modeOfIArcade <- getMode(male_year_filter$I_Arcade)
  IArcadeFrequency4 <- sum(male_year_filter$I_Arcade == 4) / numOfIndividuals
  modeOfIFood <- getMode(male_year_filter$I_Food)
  IFoodFrequency4 <- sum(male_year_filter$I_Food == 4) / numOfIndividuals
  modeOfIWashroom <- getMode(male_year_filter$I_Washroom)
  IWashroomFrequency4 <- sum(male_year_filter$I_Washroom == 4) / numOfIndividuals
  modeOfIChairs <- getMode(male_year_filter$I_Chairs)
  IChairsFrequency4 <- sum(male_year_filter$I_Chairs == 4) / numOfIndividuals
  modeOfISeating <- getMode(male_year_filter$I_Seating)
  ISeatingFrequency4 <- sum(male_year_filter$I_Seating == 4) / numOfIndividuals
  modeOfIScreenSize <- getMode(male_year_filter$I_Screen_Size)
  IScreenSizeFrequency4 <-sum(male_year_filter$I_Screen_Size == 4) / numOfIndividuals
  modeOfISound2 <- getMode(male_year_filter$I_Sound2)
  ISound2Frequency4 <- sum(male_year_filter$I_Sound2 == 4) / numOfIndividuals
  modeOfINumScreens <- getMode(male_year_filter$I_Num_Screens)
  INumScreensFrequency4 <- sum(male_year_filter$I_Num_Screens == 4) / numOfIndividuals
  modeOfICleanWashrooms <- getMode(male_year_filter$I_Clean_Washrooms)
  ICleanWashroomsFrequency4 <- sum(male_year_filter$I_Clean_Washrooms == 4) / numOfIndividuals
  modeOfSNewspaper <- getMode(male_year_filter$S_Newspaper)
  SNewspaperFrequency4 <- sum(male_year_filter$S_Newspaper == 4) / numOfIndividuals
  modeOfSInternet <- getMode(male_year_filter$S_Internet)
  SInternetFrequency4 <- sum(male_year_filter$S_Internet == 4) / numOfIndividuals
  modeOfSPhoneIn <- getMode(male_year_filter$S_Phone_In)
  SPhoneInFrequency4 <- sum(male_year_filter$S_Phone_In == 4) / numOfIndividuals
  modeOfSTV <- getMode(male_year_filter$S_TV)
  STVFrequency4 <- sum(male_year_filter$S_TV == 4) / numOfIndividuals
  modeOfSFriends <- getMode(male_year_filter$S_Friends)
  SFriendsFrequency4 <- sum(male_year_filter$S_Friends == 4) / numOfIndividuals
  modeOfSOther <- getMode(male_year_filter$S_Other)
  SOtherFrequency4 <- sum(male_year_filter$S_Other == 4) / numOfIndividuals
  
  #creates a data frame from the new variables
  male_data   <- data.frame(0,
                            i,
                            numOfIndividuals,
                            modeOfImportanceOfMovieGoing,
                            MovieWatchedPerMonth,
                            AverageMoviedWatched,
                            modeOfFoodSpent,
                            FoodSpentFrequency4,
                            modeOfIArcade,
                            IArcadeFrequency4,
                            modeOfIFood,
                            IFoodFrequency4,
                            modeOfIWashroom,
                            IWashroomFrequency4,
                            modeOfIChairs,
                            IChairsFrequency4,
                            modeOfISeating,
                            ISeatingFrequency4,
                            modeOfIScreenSize,
                            IScreenSizeFrequency4,
                            modeOfISound2,
                            ISound2Frequency4,
                            modeOfINumScreens,
                            INumScreensFrequency4,
                            modeOfICleanWashrooms,
                            ICleanWashroomsFrequency4,
                            modeOfSNewspaper,
                            SNewspaperFrequency4,
                            modeOfSInternet,
                            SInternetFrequency4,
                            modeOfSPhoneIn,
                            SPhoneInFrequency4,
                            modeOfSTV,
                            STVFrequency4,
                            modeOfSFriends,
                            SFriendsFrequency4,
                            modeOfSOther,
                            SOtherFrequency4)
  
  #renames the new data frame
  for (j in 1:38){
    names(male_data)[j] <- frequency_names[j]
  }

  #adds the new dataframe to frequency_data
  frequency_data <- rbind(frequency_data,male_data)
  
  #--------------------end of male -------------------------
}

#Testing the validity of the final model 

#Random Forest Test

#Predicting # Movies per month using Uni, Importance of Movie going and willingness to drive

str(q1_yes)

rf.train1 <- q1_yes[1:419, c("Importance_of_Movie_Going","Uni", "Drive_More")]
rf.label <- q1_yes$Movies_Month
set.seed(1234)
rf.1<-randomForest(x=rf.train1, y=rf.label, importance =  TRUE, ntree =  1000)
rf.1
varImpPlot(rf.1)