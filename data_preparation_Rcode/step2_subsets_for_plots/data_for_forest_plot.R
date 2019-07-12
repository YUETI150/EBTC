library(openxlsx)
getwd()
setwd("/Users/dingle/Desktop/ALY6980/project A material/datasets")
intact_dta <- read.xlsx("intact_data.xlsx")

#import the useful data from og_dta
names(intact_dta)
dta_meta <- intact_dta[,c(1,2,3,4,13,22,23,24,31,25,26,27,28)]

#remove the rows which has no compared groups(control), cuz they don't have 
#function
combine <- dta_meta#create a empty df in advance
combine <- combine[0,]
names(dta_meta[9])

sort_logic <- function(studyID){
  subsets <<- dta_meta[dta_meta[,9] == studyID,]
  if(length(unique(subsets$DailyDose))>1){
    combine <<- rbind(combine,subsets)
  } else if(length(unique(subsets$Timepoint))>1){
    combine <<- rbind(combine,subsets)
  } else {
    cat("removed study", studyID," ")
  }
}


##loop to combine all the studies containing the control and treated groups
for(i in 1:143){
  sort_logic(studyID = i)
}

###########backup the removed studies which do not have control group
listofstudyID <- c(3,4,5,6,10,11,12,13,14,18,20,21,22,36,37,38,41,44,60,102,105,121,125,131)
backup_non_control <- subset(dta_meta, studyID %in% listofstudyID)
# write.xlsx(backup_non_control, file = "backup_non_control.xlsx", row.names = FALSE)


#remove the rows whose "varibility unit" value is "do not analyze"
which(grepl("do not analyze", combine$UnitofVariability))#none, skip

#unify the value of "Endpointname"
combine$Endpointname[combine$Endpointname == "Aspartate transaminase (AST)"] <- "AST"
combine$Endpointname[combine$Endpointname == "Alanine aminotransferase (ALT)"] <- "ALT"
combine$Endpointname[combine$Endpointname == "Alkaline phosphatase (ALP)"] <- "ALP"
#remove the rows whose study is "withdrawal due to abnormal LFT" in "Endpointname"
combine <- combine[!combine$Endpointname %in% "Study withdrawal due to abnormal LFT", ]
#########backup the rows whose study is "withdrawal due to abnormal LFT" in "Endpointname"
backup_withdrawal <- subset(combine, Endpointname %in% "Study withdrawal due to abnormal LFT")
#write.xlsx(backup_withdrawal, file = "backup_withdrawal.xlsx", row.names = FALSE)


##remove the rows whose "Unitofvaribility" is not SD,SE,OR SEM from combine
combine$UnitofVariability[combine$UnitofVariability == "SEM"] <- "SE"
df <- combine
combine <- combine[combine$UnitofVariability %in% c("SE","SD"),] # or combine <- combine[combine$UnitofVariability %in% "SE" | combine$UnitofVariability %in% "SD",]
##########backup the rows whose "Unitofvaribility" is not SD,SE,OR SEM
backup_Unitofvariability <- df[!row.names(df) %in% row.names(combine),]
#write.xlsx(backup_Unitofvariability, file = "backup_Unitofvariability.xlsx", row.names = FALSE)



#remove the rows whose "variability" or "effectvalue" is not numeric
df <- combine
combine <- combine[!is.na(as.numeric(as.character(combine$Effectvalue))),]
combine <- combine[!is.na(combine$Variability),]
########backup the rows whose "variability" or "effectvalue" is not numeric
backup_unusual_result <- df[!row.names(df) %in% row.names(combine),]
#write.xlsx(backup_unusual_result, file = "backup_unusual_result.xlsx", row.names = FALSE)


#questions：
#1.Do we need to separate the same study by "Species" and "Endpointname", cuz the same study also use different criteria (solved)
#2.is the SD value in the meta-analysis table the mean of SD or not? (solved)
#3. we find that there might be different control groups(baseline group) identified by  different "Dailydose"
#in the same study with the same species. shall we further group them by "dailydose"?(solved)
##得知要求后，将combine 分为两个子集（根据studyid链接）：
#step1：if语句create conrtol组，直接导入effect value， sd，total
#step2: groupby语句求mean， create treated组，一个studyid一个row,添加column “mean effectvalue” 计算平均值导入treated 子集
#step3: merge重新组合两个子集 by studyid



#convert the values of timepoint,whose prefix is "0", to Baseline
combine$Timepoint <- gsub("\\<0 weeks\\>", "Baseline", combine$Timepoint)
#"\<" is an escape sequence for the beginning of a word, and ">" is the end

#convert mixed dailydose into mean of them
combine$DailyDose[combine[,"DailyDose"] == "2 to 8"] <- (2+8)/2
combine$DailyDose[combine[,"DailyDose"] == "200-600"] <- (200+600)/2
combine$DailyDose[combine[,"DailyDose"] == "4.4+/-2"] <- 4.4

#reset the row names and unify the data 
row.names(combine) <- 1:nrow(combine)
#we change "0" to "400" in dailydose because its timepoint is "baseline", which means it actually never be used.
combine[12,"DailyDose"] <- 400
combine[14,"DailyDose"] <- 400
# from studyID 108 to 111, we need to convert dailydose 0 to default 200, and convert corresponding timepoint to "baseline"
# this action will not change the facts of data but significantly improve the convenience for data cleaning
combine[c(213,216,220,223,225,228,231,234,243),"DailyDose"] <- 200
combine[c(213,216,220,223,225,228,231,234,243),"Timepoint"] <- "Baseline"
combine[251,"Timepoint"] <- "3 weeks"
combine[254,"Timepoint"] <- "8 weeks"
#replace "single dose" with "single" to keep unified with treated group
combine[c(272,274), "Timepoint"] <- "single"
##########remove and Backup the study which doesn't have any comparing criteria.
backup_unusual_control <- combine[c(256,257),]
#write.xlsx(backup_unusual_control, file = "backup_unusual_control.xlsx", row.names = FALSE)
combine <- combine[-c(256,257),]


#again, reset the rowname after removing the rows
row.names(combine) <- 1:nrow(combine)


##添加新的column “id后缀”， 对比the combination of “dailydose” and “studyid” 若不同则产生新的后缀。等后缀都产生之后，在与studyid结合（1.1）
## 用多重 if 语句 和 for loop 完成目标

###create a new subset to figure out the specificity of each detailed experimental group
suffix_dta <- subset(unique(combine[,c("DailyDose","Timepoint","studyID")]))
suffix_dta$suffixID <- NA
# add new columns to identify the different combination of Dose/timepoint with studyID
suffix_dta$DoseandID <- paste(suffix_dta$DailyDose, suffix_dta$studyID, sep="")
suffix_dta$PointandID <- paste(suffix_dta$Timepoint, suffix_dta$studyID, sep="")
str(suffix_dta)





#create a empty df for rbind
suffix <<- suffix_dta[0,]

for(i in 1:143){
  x <- subset(suffix_dta,  suffix_dta$studyID == i)
  if((length(unique(x$DoseandID)) == 1) & (length(unique(x$PointandID)) != 1) ){
    x$suffixID <- cumsum(!duplicated(x$DoseandID)) ## set the suffix id for different combinations of Dose/timepoint with studyID
    suffix <<- rbind(suffix,x)
  } else if((length(unique(x$DoseandID)) != 1) & (length(unique(x$PointandID)) == 1)){
    x$suffixID <- cumsum(!duplicated(x$PointandID)) 
    suffix <<- rbind(suffix,x)
  } else if((length(unique(x$DoseandID)) != 1) & (length(unique(x$PointandID)) != 1)){
    x$suffixID <- cumsum(!duplicated(x$DoseandID)) 
    suffix <<- rbind(suffix,x)
  } else {
    cat("error! studyID", i, "please check the problem")
  }
}
#here study 68 is eliminated by this loop, because it does not have any comparing group, so its meaningless for our study



# now, we successfully create the column "combinedID" to identify the different experimental groups in the same studies.
suffix$combinedID <- paste(suffix$studyID, suffix$suffixID, sep=".")

# left join combine and suffix 
combine <- merge(x = combine, y = suffix, by = c("DailyDose","Timepoint","studyID"), all.x = TRUE)
#we remove the row representint the study 68 which is meaningless
combine <- combine[-c(301),]
any(is.na((combine$combinedID))) # no NA existing in combine$combinedID now





####step1####
control <- combine
control <- control[0,]

for(i in unique(combine$combinedID)){
  df <- combine[combine[,17] == i,]
  if(any(df$Timepoint == "Baseline")){
    control <<- rbind(df[df[,"Timepoint"] == "Baseline",], control)
  } else if(any(df$Timepoint == 0)) {  
    control <<- rbind(df[df[,"Timepoint"] == 0,], control)
  } else{
    control <<- rbind(df[df[,"DailyDose"] == min(df[,"DailyDose"]),], control)
  }
}

#check if there are some errors after creating control subset, directly go to "combine", then create control again
combine[c(15,252),"combinedID"] <- 58.1
combine[c(59,135),"combinedID"] <- 58.2
# do step1 again
control <- combine
control <- control[0,]

for(i in unique(combine$combinedID)){
  df <- combine[combine[,17] == i,]
  if(any(df$Timepoint == "Baseline")){
    control <<- rbind(df[df[,"Timepoint"] == "Baseline",], control)
  } else if(any(df$Timepoint == 0)) {  
    control <<- rbind(df[df[,"Timepoint"] == 0,], control)
  } else{
    control <<- rbind(df[df[,"DailyDose"] == min(df[,"DailyDose"]),], control)
  }
}


#Question: The study 103, as the treated group,does not have any control/compared group . The difference between each pair of daily dose is the gender of the subject. 
#Do we also regard one of the gender (male or female) as a control group? Or just remove them?




#create the treated group table
library(dplyr)
treated <- anti_join(combine, control)


#since the treated dataset has different compared/treated groups with the same "combinedID"
#we'd better to create a "comparedID" for the convenience of calculating the mean of effectvalue
k <- treated[,c(1,2,17)]
k$Doseandpoint <- paste(k$DailyDose,k$Timepoint, sep = " , ")
k$comparedID <- NA
compare <- k
compare <- compare[0,]

for(i in unique(treated$combinedID)){
  x <- subset(k, k$combinedID == i)
  x$comparedID <- cumsum(!duplicated(x$Doseandpoint)) 
  compare <<- rbind(compare, x)
}

compare$comparedID <- paste(compare$combinedID,compare$comparedID, sep = ".")

compare <- compare[!duplicated(compare$comparedID),]
length(unique(compare$comparedID))

treated <- merge(x = treated, y = compare, by = c("DailyDose","Timepoint","combinedID"), all.x = TRUE)
# the sum of the row numbers of two subsets should be equal to row number of combine.
nrow(treated)+nrow(control)
nrow(combine)

#Now, we have the complete subsets of treated group and control group. we can join/combine them by "combinedID" instead of "comparedID"
#since "comparedID" is only existing in treated group, and it is used to identify different treated conditions for the same control condition in a study






####################################################################################################################################################################################
####step2####
#calculate the means of each treated group and control group. 


###### unify the unit of variability and transfer the values of SE/SD


###for control subset
#calculate the median of sample size "Numberofexperimentalunits(animals,humans)".
library(stringr)
control$`Numberofexperimentalunits(animals,humans)` <- str_replace(control$`Numberofexperimentalunits(animals,humans)`, "6-10", "8")
control$`Numberofexperimentalunits(animals,humans)` <- str_replace(control$`Numberofexperimentalunits(animals,humans)`, "3-4", "3.5")
control$`Numberofexperimentalunits(animals,humans)` <- str_replace(control$`Numberofexperimentalunits(animals,humans)`, "6-9", "7.5")
control$`Numberofexperimentalunits(animals,humans)` <- str_replace(control$`Numberofexperimentalunits(animals,humans)`, "5-6", "5.5")

################## backup for the control groups whose sample size is "na"/"NA"
backup_control_samplesize <- control[grep("na", control$`Numberofexperimentalunits(animals,humans)`),]
backup_control_samplesize <- rbind(backup_control_samplesize,control[is.na(control$`Numberofexperimentalunits(animals,humans)`),])
#write.xlsx(backup_control_samplesize, file = "backup_control_samplesize.xlsx", row.names = FALSE)


#remove the na rows from control group
control <- control[!row.names(control) %in% row.names(backup_control_samplesize),]

#transfer SE value to SD value
control$Variability[control[,"UnitofVariability"] == "SE"] <- as.numeric(control$Variability[control[,"UnitofVariability"] == "SE"])*
  sqrt(as.numeric(control$`Numberofexperimentalunits(animals,humans)`[control[,"UnitofVariability"] == "SE"]))
#stransfer SE unit to SD unit
control$UnitofVariability[control[,"UnitofVariability"] == "SE"] <- "SD"


#compute the varibility for each unique control/treated group
meanofvar_control <- control %>% 
  group_by(combinedID) %>%
  summarize(
    Meanofvaribility_control = mean(as.numeric(Variability), na.rm = TRUE)
  )


### for treated group
#calculate the median of sample size "Numberofexperimentalunits(animals,humans)".
library(stringr)
treated$`Numberofexperimentalunits(animals,humans)` <- str_replace(treated$`Numberofexperimentalunits(animals,humans)`, "6-10", "8")
treated$`Numberofexperimentalunits(animals,humans)` <- str_replace(treated$`Numberofexperimentalunits(animals,humans)`, "3-4", "3.5")
treated$`Numberofexperimentalunits(animals,humans)` <- str_replace(treated$`Numberofexperimentalunits(animals,humans)`, "6-9", "7.5")
treated$`Numberofexperimentalunits(animals,humans)` <- str_replace(treated$`Numberofexperimentalunits(animals,humans)`, "5-6", "5.5")

################## backup for the treated groups whose sample size is "na"/"NA"
backup_treated_samplesize <- treated[grep("na", treated$`Numberofexperimentalunits(animals,humans)`),]
backup_treated_samplesize <- rbind(backup_treated_samplesize,treated[is.na(treated$`Numberofexperimentalunits(animals,humans)`),])
#write.xlsx(backup_treated_samplesize, file = "backup_treated_samplesize.xlsx", row.names = FALSE)


#remove the na rows from treated group
treated <- treated[!row.names(treated) %in% row.names(backup_treated_samplesize),]

#transfer SE value to SD value
treated$Variability[treated[,"UnitofVariability"] == "SE"] <- as.numeric(treated$Variability[treated[,"UnitofVariability"] == "SE"])*
  sqrt(as.numeric(treated$`Numberofexperimentalunits(animals,humans)`[treated[,"UnitofVariability"] == "SE"]))
#stransfer SE unit to SD unit
treated$UnitofVariability[treated[,"UnitofVariability"] == "SE"] <- "SD"

#there is a value of variability whose format is wrong ("o.150", should have been "0.150"). report it comparedID 77.1.2
treated$Variability[treated[,"Variability"] == "o.150"] <- 0.150

#compute the varibility for each unique treated/treated group
meanofvar_treated <- treated %>% 
  group_by(comparedID) %>%
  summarize(
    Meanofvaribility_treated = mean(as.numeric(Variability), na.rm = FALSE)
  )


###compute mean of effect values for control and treated datasets


meanofeffect_control <- control %>% 
  group_by(combinedID) %>% 
  summarize(
    MeanofEffectvalue_control = mean(as.numeric(Effectvalue), na.rm = TRUE)
  )



meanofeffect_treated <- treated %>% 
  group_by(comparedID) %>% 
  summarize(
    MeanofEffectvalue_treated = mean(as.numeric(Effectvalue), na.rm = TRUE)
  )



######join the subsets of "meanofvar" and "meanofeffect" with the control and treated datasets
###for control (join by "combinedID")
control <- merge(x = control, y = meanofeffect_control, by = "combinedID", all.x = TRUE)
control <- merge(x = control, y = meanofvar_control, by = "combinedID", all.x = TRUE)

###for treated (join by "comparedID")
treated <- merge(x = treated, y = meanofeffect_treated, by = "comparedID", all.x = TRUE)
treated <- merge(x = treated, y = meanofvar_treated, by = "comparedID", all.x = TRUE)



#make the rows of control and treated subsets unique
#first, drop the columns "effectvalue" and "variability",since the groups under totally same condition might still have different results
control <- control[,!(names(control) %in% c("Effectvalue", "Variability"))]
treated <- treated[,!(names(treated) %in% c("Effectvalue", "Variability"))]

#second, unify the unitofdose for control and treated, since the messy unit will make the totally same subgroup different
#question: they are two different control groups for treatment groups or just the mistakes of unitofdose? (solved)
#treated:79.2.1     control:76.1     79.1
rownames(treated) <- NULL
rownames(control) <- NULL
treated$UnitofDose[138] <- "mg/kg"
control$UnitofDose[c(70, 75)] <- "mg/kg"


#third, remove the totally same rows, and only remain unique ones
control <- control[!duplicated(control),]
treated <- treated[!duplicated(treated),]



#Question3: Some studies do not have the control group whose "dailydose" or "Timepoint" is not "0" or "baseline".(i.e. study 62,
#do we just regard the combinedID 62.1 as the control? or just remove relevant experiment data) (answer: we exclude but report them)
#####remove and backup the study whose "control group" is not "0" or "baseline" in "dailydose" or "Timepoint"
#back up study 2,63,64,65,66,69,103 
backup_non_control2 <- merge(y = control[control$studyID %in% c(62,63,64,65,66,69,103),], 
                             x = treated[treated$studyID %in% c(62,63,64,65,66,69,103),],
                             by = "studyID", all = TRUE)

#write.xlsx(backup_non_control2, file = "backup_non_control2.xlsx", row.names = FALSE)

#remove study 2,63,64,65,66,69,103 
control <- control[!control$studyID %in% c(62,63,64,65,66,69,103),]
treated <- treated[!treated$studyID %in% c(62,63,64,65,66,69,103),]


###create a brief one
control_merge <- control[,c(1,2,3,11,16,17,5,6,7,9,10,12)]
treated_merge <- treated[,c(1,2,3,4,12,18,19)]

combinedforplot <- merge(x = treated_merge,y = control_merge, by = "combinedID", all.x = TRUE)

any(is.na(combinedforplot)) #successfully create the dataset for forest plot
#write.xlsx(combinedforplot, file = "combinedforplot.xlsx", row.names = FALSE)


####add CI
df <- combinedforplot

df[,c("t","SE","Effectsize","UL","LL")] <- NA
colnames(df)[5] <- "samplesize_treated"
colnames(df)[10] <- "samplesize_control"
df$samplesize_treated <- as.numeric(df$samplesize_treated)
df$samplesize_control <- as.numeric(df$samplesize_control)

df$t <- abs(qt(0.05/2, df$samplesize_treated + df$samplesize_control-2))
df$Effectsize <- df$MeanofEffectvalue_treated - df$MeanofEffectvalue_control

# The pooled estimate of the variance is
#(df$samplesize_treated-1)*((df$Meanofvaribility_treated)^2)+(df$samplesize_control-1)*((df$Meanofvaribility_control)^2)/(df$samplesize_treated + df$samplesize_control-2)

df$SE <- sqrt((((df$samplesize_treated-1)*((df$Meanofvaribility_treated)^2)+(df$samplesize_control-1)*((df$Meanofvaribility_control)^2)/(df$samplesize_treated + df$samplesize_control-2)
)/df$samplesize_treated)+(((df$samplesize_treated-1)*((df$Meanofvaribility_treated)^2)+(df$samplesize_control-1)*((df$Meanofvaribility_control)^2)/(df$samplesize_treated + df$samplesize_control-2)
)/df$samplesize_control))

df$UL <- ((df$Effectsize) + (df$t)*(df$SE))  
df$LL <- ((df$Effectsize) - (df$t)*(df$SE))  
any(is.na(df))

library(openxlsx) 
#export the dataset for the forest plot in Part B.
#write.xlsx(df, file = "dta_partB.xlsx", row.names = FALSE)  


#########################################################convert dailydose unit for scatter, bubble and boxplot2
getwd()
setwd("/Users/dingle/Desktop/ALY6980/project A material/datasets")
df <- read.xlsx("dta_partB.xlsx")

df <- df[-c(80,81,82,83),]
df$DailyDose.x <- as.numeric(df$DailyDose.x)
str(df$DailyDose.x)
options(scipen=999)
#weight references:
#Human : 60 kg
#Mouse: 0.02 kg
#Rat: 0.15 kg
#Non-Human primate: 3 kg

table(df$UnitofDose)
#remove IU/L rows
df <- df[!df$UnitofDose == "IU/L",]
#convert g to mg/kg
df[df$UnitofDose == "g", "UnitofDose"] <- "mg" #only one row whose dailydose for rat is 10 g, it should be 10 mg, or its extremely unusual
df[df$UnitofDose == "mg" & df$Species == "Human", "DailyDose.x"] <- df[df$UnitofDose == "mg" & df$Species == "Human", "DailyDose.x"]/60
df[df$UnitofDose == "mg" & df$Species == "Rat", "DailyDose.x"] <- df[df$UnitofDose == "mg" & df$Species == "Rat", "DailyDose.x"]/0.15
df[df$UnitofDose == "mg" & df$Species == "Non-human primate", "DailyDose.x"] <-df[df$UnitofDose == "mg" & df$Species == "Non-human primate", "DailyDose.x"]/3
df[df$UnitofDose == "mg", "UnitofDose"] <- "mg/kg"
#convert mg/100g to mg/kg
df[df$UnitofDose == "mg/100g","DailyDose.x"] <- df[df$UnitofDose == "mg/100g","DailyDose.x"]*10
df[df$UnitofDose == "mg/100g", "UnitofDose"] <- "mg/kg"
#convert mg/kg/diet to mg/kg
df[df$UnitofDose == "mg/kg/diet", "UnitofDose"] <- "mg/kg"
#convert mg/day to mg/kg
df[df$UnitofDose == "mg/day" & df$Species == "Human", "DailyDose.x"] <- df[df$UnitofDose == "mg/day" & df$Species == "Human", "DailyDose.x"]/60
df[df$UnitofDose == "mg/day" & df$Species == "Rat", "DailyDose.x"] <- df[df$UnitofDose == "mg/day" & df$Species == "Rat", "DailyDose.x"]/0.15
df[df$UnitofDose == "mg/day" & df$Species == "Non-human primate", "DailyDose.x"] <-df[df$UnitofDose == "mg/day" & df$Species == "Non-human primate", "DailyDose.x"]/3
df[df$UnitofDose == "mg/day" & df$Species == "Mouse", "DailyDose.x"] <-df[df$UnitofDose == "mg/day" & df$Species == "Mouse", "DailyDose.x"]/0.02
df[df$UnitofDose == "mg/day", "UnitofDose"] <- "mg/kg"


df <- df[order(df$DailyDose.x, decreasing = FALSE),]
rownames(df) <- NULL

df$levels <- NA
df$levels[1:46] <- "Low"
df$levels[47:92] <- "Medium"
df$levels[93:137] <- "High"
table(df$levels)

df$Effectpercent <- (df$MeanofEffectvalue_treated)/(df$MeanofEffectvalue_control)*100
#export the data for boxplot2
#write.xlsx(df, file = "dta_boxplot2.xlsx", row.names = FALSE)





