library(tibble)
library(readxl)
library(openxlsx)


getwd()
setwd("/Users/dingle/desktop/ALY6980/project A material/datasets")
animal <- read_excel("original dataset.xlsx", sheet = "Animal")
human <- read_excel("original dataset.xlsx", sheet = "Human")

#unify the different column names
names(human)[names(human)=="...28"] <- "note"
animal["note"] <- NA
library(stringr)
names(animal) <- str_replace_all(names(animal), c(" " = "","\r\n" = ""))
names(human) <- str_replace_all(names(human), c(" " = "","\r\n" = ""))
names(animal)[c(22,27)] <- c("UnitofDose","UnitofVariability")
names(human)[c(22,27)] <- c("UnitofDose","UnitofVariability")

#add the columns that the other table doesnt have
#install.packages("tibble")
human <- add_column(human, Vehicle = NA, .after = "Control")
animal <- add_column(animal, Control = NA, .after = "Vehicle")

#change Endpointtypetype to Endpointtype
setdiff(names(human),names(animal))
names(animal)[18] <- "Endpointtype"

#create the original intact dataset
intact_dta <- rbind(human, animal) 
#set "studyID"by different groups of animals even in the same study. 
sub_for_ID <- unique(intact_dta[c("Bibliography", "Species", "Endpointname")])
sub_for_ID["studyID"] <- c(1:143)
intact_dta <- merge(intact_dta, sub_for_ID, by = c("Bibliography", "Species","Endpointname"), type = "inner", match = "all")


#export excel file.
write.xlsx(intact_dta, file = "intact_data.xlsx", row.names = FALSE)
getwd()
