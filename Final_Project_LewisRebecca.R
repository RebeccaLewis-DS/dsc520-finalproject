# Assignment: Final Project Section 3
# Name: LEWIS, REBECCA
# Date: 20 JANUARY 2019

#load packages and data
library(readr)
library(ggplot2)
library(lubridate)
library(stringr)
library(dplyr)
library(broom)
library(reshape2)
library(tidyr)

combined <- read.csv("aac_intakes_outcomes.csv", header = TRUE)

#create dataset with variables related to project
ProjectData <- combined[, c("outcome_type", "outcome_subtype", "outcome_datetime", "animal_type","dob_monthyear", "breed", "intake_condition", "intake_type", "intake_datetime", "sex_upon_intake","time_in_shelter_days")]

#add another variable that groups the outcome types to either Live Release or Euthanasia
#find the indexes of each category
liverelease_index <- as.character(ProjectData$outcome_type) %in% c("Adoption", "Return to Owner", "Rto-Adopt", "Transfer", "Relocate")
euthanasia_index <- as.character(ProjectData$outcome_type) == "Euthanasia"

#create a vector of the categories
outcome_category <- ifelse(liverelease_index, "Live Release", ifelse(euthanasia_index, "Euthanasia", NA))

#add the categories to the project data
ProjectData <- cbind(ProjectData, outcome_category)


##Clean up Dates
#DOB: modify year by adding day 1 and converting to date type
ProjectData$dob_monthyear <-as.character(ProjectData$dob_monthyear)
ProjectData$dob_monthyear <-paste(ProjectData$dob_monthyear, 1)
ProjectData$dob_monthyear <- str_replace(ProjectData$dob_monthyear," ", "-")

ProjectData$dob_monthyear <-ymd(ProjectData$dob_monthyear)

#Outcome Date
ProjectData$outcome_datetime <-as.character(ProjectData$outcome_datetime)
ProjectData$outcome_datetime <- ymd_hms(ProjectData$outcome_datetime)

#Intake Date
ProjectData$intake_datetime <-as.character(ProjectData$intake_datetime)
ProjectData$intake_datetime <- ymd_hms(ProjectData$intake_datetime)


#change null sex upon intake values to unknown
ProjectData$sex_upon_intake <- as.character((ProjectData$sex_upon_intake))
ProjectData$sex_upon_intake <- ifelse(ProjectData$sex_upon_intake == "", "Unknown", ProjectData$sex_upon_intake)
ProjectData$sex_upon_intake <- as.factor(ProjectData$sex_upon_intake )

#set baseline level for intake condition
print(levels(ProjectData$intake_condition))
ProjectData$intake_condition = relevel(ProjectData$intake_condition, ref = 4)


#set baseline level for gender
print(levels(ProjectData$sex_upon_intake))
ProjectData$sex_upon_intake = relevel(ProjectData$sex_upon_intake, ref = 5)

#set baseline level for intake type
print(levels(ProjectData$intake_type))
ProjectData$intake_type = relevel(ProjectData$intake_type, ref = 3)

ProjectData$outcome_type <- as.character(ProjectData$outcome_type)
ProjectData$outcome_type <- str_replace(ProjectData$outcome_type, "Rto-Adopt", "Return to Owner")
ProjectData$outcome_type <- as.factor(ProjectData$outcome_type)

#Final Cleaning
#Filter 
any(is.na(ProjectData$outcome_category))

#filter out null outcome categories (neither live release nor euthanasia)
Clean_ProjectData <- ProjectData %>% 
    filter(is.na(ProjectData$outcome_category) == FALSE) 

#filter out animals other than cats and dogs
Clean_ProjectData <- Clean_ProjectData %>%
    filter(as.character(Clean_ProjectData$animal_type) %in% c("Cat", "Dog"))

#filter out intake types that are not applicable: remove wildlife and euthanasia request
Clean_ProjectData <- Clean_ProjectData %>%
    filter(as.character(Clean_ProjectData$intake_type) %in% c("Public Assist", "Owner Surrender", "Stray"))

#age at intake
Clean_ProjectData$age_intake_days <- difftime(Clean_ProjectData$intake_datetime,Clean_ProjectData$dob_monthyear,units="days")

#age at outcome
Clean_ProjectData$age_outcome_days <- difftime(Clean_ProjectData$outcome_datetime,Clean_ProjectData$dob_monthyear,units="days")

Austin_Model <- glm(outcome_category ~ animal_type + intake_condition + intake_type + time_in_shelter_days + sex_upon_intake, data = Clean_ProjectData, family = binomial())

summary(Austin_Model)

modelChi <- Austin_Model$null.deviance - Austin_Model$deviance
chidf <- Austin_Model$df.null - Austin_Model$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)

Clean_ProjectData_Mod <- Clean_ProjectData

Clean_ProjectData_Mod$predicted.probabilities <- fitted(Austin_Model)
Clean_ProjectData_Mod$standardized.residuals <- rstandard(Austin_Model)
Clean_ProjectData_Mod$studentized.residuals <- rstudent(Austin_Model)
Clean_ProjectData_Mod$dfbeta <- dfbeta(Austin_Model)
Clean_ProjectData_Mod$dffit <- dffits(Austin_Model)
Clean_ProjectData_Mod$leverage <- hatvalues(Austin_Model)

head(Clean_ProjectData_Mod[ , c("outcome_category", "animal_type", "intake_condition", "intake_type", "time_in_shelter_days", "sex_upon_intake", "outcome_type", "predicted.probabilities")])

Clean_ProjectData_Mod$pred_Live <- ifelse(Clean_ProjectData_Mod$predicted.probabilities > .50, "T", "F")
Clean_ProjectData_Mod$actual_Live <- ifelse(as.character(Clean_ProjectData_Mod$outcome_category) == "Live Release", "T", "F")

Clean_ProjectData_Mod$accuracy <- Clean_ProjectData_Mod$actual_Live == Clean_ProjectData_Mod$pred_Live

sum(Clean_ProjectData_Mod$accuracy) / nrow(Clean_ProjectData_Mod) * 100

#Cats vs Dogs
Cat_Data <- Clean_ProjectData %>%
        filter(animal_type == "Cat")

Cat_Model <- glm(outcome_category ~ intake_condition + intake_type + time_in_shelter_days + sex_upon_intake, data = Cat_Data, family = binomial())

summary(Cat_Model)

cat_modelChi <- Cat_Model$null.deviance - Cat_Model$deviance
cat_chidf <- Cat_Model$df.null - Cat_Model$df.residual
cat_chisq.prob <- 1 - pchisq(cat_modelChi, cat_chidf)

Cat_Data_Mod <- Cat_Data
Cat_Data_Mod$predicted.probabilities <- fitted(Cat_Model)
Cat_Data_Mod$standardized.residuals <- rstandard(Cat_Model)
Cat_Data_Mod$studentized.residuals <- rstudent(Cat_Model)
Cat_Data_Mod$dfbeta <- dfbeta(Cat_Model)
Cat_Data_Mod$dffit <- dffits(Cat_Model)
Cat_Data_Mod$leverage <- hatvalues(Cat_Model)

Cat_Data_Mod$pred_Live <- ifelse(Cat_Data_Mod$predicted.probabilities > .50, "T", "F")
Cat_Data_Mod$actual_Live <- ifelse(as.character(Cat_Data_Mod$outcome_category) == "Live Release", "T", "F")

Cat_Data_Mod$accuracy <- Cat_Data_Mod$actual_Live == Cat_Data_Mod$pred_Live

sum(Cat_Data_Mod$accuracy) / nrow(Cat_Data_Mod) * 100

Dog_Data <- Clean_ProjectData %>%
    filter(animal_type == "Dog")

Dog_Model <- glm(outcome_category ~ intake_condition + intake_type + time_in_shelter_days + sex_upon_intake, data = Dog_Data, family = binomial())

summary(Dog_Model)

dog_modelChi <- Dog_Model$null.deviance - Dog_Model$deviance
dog_chidf <- Dog_Model$df.null - Dog_Model$df.residual
dog_chisq.prob <- 1 - pchisq(cat_modelChi, cat_chidf)

Dog_Data_Mod <- Dog_Data
Dog_Data_Mod$predicted.probabilities <- fitted(Dog_Model)
Dog_Data_Mod$standardized.residuals <- rstandard(Dog_Model)
Dog_Data_Mod$studentized.residuals <- rstudent(Dog_Model)
Dog_Data_Mod$dfbeta <- dfbeta(Dog_Model)
Dog_Data_Mod$dffit <- dffits(Dog_Model)
Dog_Data_Mod$leverage <- hatvalues(Dog_Model)


Dog_Data_Mod$pred_Live <- ifelse(Dog_Data_Mod$predicted.probabilities > .50, "T", "F")
Dog_Data_Mod$actual_Live <- ifelse(as.character(Dog_Data_Mod$outcome_category) == "Live Release", "T", "F")

Dog_Data_Mod$accuracy <- Dog_Data_Mod$actual_Live == Dog_Data_Mod$pred_Live

sum(Dog_Data_Mod$accuracy) / nrow(Dog_Data_Mod) * 100


#dogs by breed-----------------------------------------------------------------
Dog_by_breed <- Dog_Data %>%
    group_by(breed = as.character(breed)) %>%
    summarise(count = n()) %>%
    arrange(desc(count))

head(Dog_by_breed, 10)

top_10_breeds <- Dog_by_breed[1:10, 1]

Top_10_Breed_Data <- Dog_Data %>%
    filter(breed %in% pull(top_10_breeds)) %>%
    count(breed, outcome_category) 

ggplot(Top_10_Breed_Data, aes(x=breed, y=n, fill=breed)) + geom_bar(stat="identity") + 
    facet_wrap(~ outcome_category) +
    labs(title = "Outcome by Breed", y = "Count") + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())



Dog_Euth_Data <- Dog_Data %>%
    filter(outcome_category =="Euthanasia" & breed %in% pull(top_10_breeds)) %>%
    arrange(breed)

ggplot(data = Top_10_Breed_Data, aes(breed)) + geom_bar(aes(fill=breed)) + 
    labs(title = "Occurence by Breed") + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())

ggplot(data = Dog_Euth_Data, aes(breed)) + geom_bar(aes(fill=breed)) + 
    labs(title = "Euthanasia by Breed") + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())


Top_10_Wide <- spread(Top_10_Breed_Data, outcome_category, n)

Top_10_Wide$Percent_Live <- Top_10_Wide$`Live Release` / (Top_10_Wide$Euthanasia + Top_10_Wide$`Live Release`) * 100
Top_10_Wide$Percent_Euth <- Top_10_Wide$Euthanasia / (Top_10_Wide$Euthanasia + Top_10_Wide$`Live Release`) * 100

HiLow_Breed_Data <- Dog_Data %>%
    filter(breed %in% c("Dachshund Mix", "Miniature Poodle Mix", "Pit Bull Mix") & outcome_type != "Euthanasia") %>%
    count(breed, outcome_type) 


ggplot(HiLow_Breed_Data, aes(x=outcome_type, y=n, fill=outcome_type)) + geom_bar(stat="identity") + 
    facet_wrap(~ breed) +
    labs(title = "Live Release Method by Breed", y = "Count") + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())

HiLo_Wide <- spread(HiLow_Breed_Data, outcome_type, n)

HiLo_Wide$Percent_RTO <- (HiLo_Wide$`Return to Owner`) / (HiLo_Wide$Adoption + HiLo_Wide$`Return to Owner`  + HiLo_Wide$Transfer )* 100
HiLo_Wide$Percent_TXR <- HiLo_Wide$Transfer / (HiLo_Wide$Adoption + HiLo_Wide$`Return to Owner` + HiLo_Wide$Transfer )* 100

#intake condition--------------------------------------------------------------------
Cat_by_Cond <- Clean_ProjectData %>%
    filter(as.character(intake_condition) %in% c("Aged", "Injured", "Sick") & as.character(animal_type) == "Cat") %>%
    count(intake_condition, outcome_category)

ggplot(Cat_by_Cond, aes(x=intake_condition, y=n, fill=intake_condition)) + geom_bar(stat="identity") + 
    facet_wrap(~ outcome_category) +
    labs(title = "Cat Outcome by Intake Condition", y = "Count") + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())

Cat_Wide <- spread(Cat_by_Cond, outcome_category, n)

Cat_Wide$Percent_Live<- (Cat_Wide$`Live Release`) / (Cat_Wide$`Live Release` + Cat_Wide$Euthanasia )* 100
Cat_Wide$Percent_Euth <- Cat_Wide$Euthanasia / (Cat_Wide$`Live Release` + Cat_Wide$Euthanasia )* 100


Dog_by_Cond <- Clean_ProjectData %>%
    filter(as.character(intake_condition) %in% c("Aged", "Injured", "Sick") & as.character(animal_type) == "Dog") %>%
    count(intake_condition, outcome_category)

ggplot(Dog_by_Cond, aes(x=intake_condition, y=n, fill=intake_condition)) + geom_bar(stat="identity") + 
    facet_wrap(~ outcome_category) +
    labs(title = "Dog Outcome by Intake Condition", y = "Count") + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())

Dog_Wide <- spread(Dog_by_Cond, outcome_category, n)

Dog_Wide$Percent_Live<- (Dog_Wide$`Live Release`) / (Dog_Wide$`Live Release` + Dog_Wide$Euthanasia )* 100
Dog_Wide$Percent_Euth <- Dog_Wide$Euthanasia / (Dog_Wide$`Live Release` + Dog_Wide$Euthanasia )* 100

#Male vs Female - combine the genders and test accuracy of overall model
Gender_Data <- Clean_ProjectData 
Gender_Data$gender_category <- as.factor(ifelse(as.character(Gender_Data$sex_upon_intake) %in% c("Intact Male", "Neutered Male"), "Male",
                                      ifelse(as.character(Gender_Data$sex_upon_intake) %in% c("Intact Female", "Spayed Female"), "Female",
                                             "Unknown")))

print(levels(Gender_Data$gender_category))
Gender_Data$gender_category = relevel(Gender_Data$gender_category, ref = 3)

Gender_Model <- glm(outcome_category ~ animal_type + intake_condition + intake_type + time_in_shelter_days + gender_category, data = Gender_Data, family = binomial())

summary(Gender_Model)

#time of stay
Clean_ProjectData <- Clean_ProjectData %>% mutate(binary_live = ifelse(outcome_category == "Live Release", 1, 0))

ggplot(data = Clean_ProjectData, aes(x = time_in_shelter_days, y = binary_live)) + 
    geom_jitter(width = 0, height = .05, alpha = .5) +
    geom_smooth(method = "glm", se = 0, method.args = list(family = "binomial"))


Cat_Data <- Cat_Data %>% mutate(binary_live = ifelse(outcome_category == "Live Release", 1, 0))

ggplot(data = Cat_Data, aes(x = time_in_shelter_days, y = binary_live)) + 
    geom_jitter(width = 0, height = .05, alpha = .5) +
    geom_smooth(method = "glm", se = 0, method.args = list(family = "binomial")) +
    labs(title = "Live Release for Cats", x = "Days in Shelter", y = "Outcome (1 = Live Release)")


Dog_Data <- Dog_Data %>% mutate(binary_live = ifelse(outcome_category == "Live Release", 1, 0))

ggplot(data = Dog_Data, aes(x = time_in_shelter_days, y = binary_live)) + 
    geom_jitter(width = 0, height = .05, alpha = .5) +
    geom_smooth(method = "glm", se = 0, method.args = list(family = "binomial")) +
    labs(title = "Live Release for Dogs", x = "Days in Shelter", y = "Outcome (1 = Live Release)")
