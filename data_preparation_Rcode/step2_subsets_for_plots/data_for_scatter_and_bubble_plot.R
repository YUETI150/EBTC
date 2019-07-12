library(openxlsx)
getwd()
setwd("/Users/dingle/Desktop/ALY6980/project A material/datasets")
intact_dta <- read.xlsx("intact_data.xlsx")
summary(intact_dta)

#check if these two columns' values are totally the same
intact_dta$Drug[!(intact_dta$Drug.. %in% intact_dta$X.Compound..)] # yes, they totally the same.


#library(stringr)
#remove "x" and "." in columns names
#names(og_dta) <- str_replace_all(names(og_dta), c("X" = ""))
#names(og_dta) <- gsub("\\.", "", names(og_dta))
#rename the duplicated column names and 
#names(og_dta)[names(og_dta) == ""] <- "note"
#names(og_dta)[22] <- "Unit0"
#names(og_dta)[27] <- "Unit1"



library(data.table)
#create Excel_file A which only described for "ALT"
dta_ALT <- intact_dta[grep("ALT", intact_dta$Endpointname), ]

#store the data for other "Endpointname"
dta_without_ALT <- intact_dta[!row.names(intact_dta) %in% row.names(dta_ALT),]
#recover rows having LFT (either ALT or AST) > or = 10" and "LFT (either ALT or AST) > or = 3"
dta_without_ALT <- rbind(dta_without_ALT, dta_ALT[dta_ALT$Endpointname == "LFT (either ALT or AST) > or = 10",])
dta_without_ALT <- rbind(dta_without_ALT, dta_ALT[dta_ALT$Endpointname == "LFT (either ALT or AST) > or = 3",])
write.csv(dta_without_ALT, file = "dta_without_ALT.csv", row.names = FALSE)

#as Katya required, we should remove the rows whose "Endpointname" is "ALT > 3 ULN"
dta_ALT <- dta_ALT[!dta_ALT$Endpointname == "ALT >3 x ULN",]
dta_ALT <- dta_ALT[!dta_ALT$Endpointname == "ALT/AST > 3 ULN",]

##set the studyID for the table by "Bibliography" and "Species"
#find the unique study with the specific animal
uni_ALT <- unique(dta_ALT[c("Bibliography", "Species")])
row.names(uni_ALT) <- NULL
uni_ALT["study_ID"] <- c(1:25)
#insert the studyID to dta_ALT using inner-join mindset
dta_ALT <- merge(x = dta_ALT, y = uni_ALT, by = c("Bibliography", "Species"), all = TRUE)
#thus, Refid identifies the specific study name, studyID identifies the study with specific animals


write.csv(dta_ALT, file = "Excel file for ALT.csv", row.names = FALSE)
testdata <- read.csv("Excel file for ALT.csv")







####################################################################################################################################
