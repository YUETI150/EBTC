#Import the dataset
data <- read.csv("https://raw.githubusercontent.com/chang5307/test/master/data_forestplot.csv", sep = ",", encoding = "UTF-8")

#Data preprocessing 7 ~ 75
#Forest plot 78

#Adjust data contents
data$Endpointname[data$Endpointname == "liver weight, relative"] <- "Liver weight, relative"
data <- subset(data, UnitofDose != "IU/L")
datatp <- subset(data, Timepoint.y == "Baseline" | Timepoint.y == 0)

datatp$Timepoint.x <- as.character(datatp$Timepoint.x)
datatp$Timepoint.x[datatp$Timepoint.x == "Week 12"] <- "3 months"
datatp$Timepoint.x[datatp$Timepoint.x == "44 weeks (LETO)"] <- "11 months (LETO)"
datatp$Timepoint.x[datatp$Timepoint.x == "44 weeks (OLETF)"] <- "11 months (OLETF)"
datatp$Timepoint.x[datatp$Timepoint.x == "60 weeks (LETO)"] <- "15 months (LETO)"
datatp$Timepoint.x[datatp$Timepoint.x == "60 weeks (OLEFT)"] <- "15 months (OLETF)"
datatp$Timepoint.x[datatp$Timepoint.x == "60 weeks (OLETF)"] <- "15 months (OLETF)"
datatp$Timepoint.x[datatp$Timepoint.x == "20 weeks"] <- "5 months"
datatp$Timepoint.x[datatp$Timepoint.x == "52 weeks"] <- "13 months"
datatp$Timepoint.x[datatp$Timepoint.x == "12 month"] <- "12 months"
datatp$Timepoint.x[datatp$Timepoint.x == "Month 3"] <- "3 months"
datatp$Timepoint.x[datatp$Timepoint.x == "Month 8"] <- "8 months"

#############################################################################
#Subgroup by Species
data_human = datatp[datatp[, "Species"] == "Human", ]
data_rat = datatp[datatp[, "Species"] == "Rat", ]
data_nhp = datatp[datatp[, "Species"] == "Non-human primate", ]

##################################################################################################
#Subgroup by Species & Endpoint name
#Rat
#ALT
rat_alt = data_rat[data_rat[, "Endpointname"] == "ALT", ]
#AST
rat_ast = data_rat[data_rat[, "Endpointname"] == "AST", ]
#Liver weight, relative
rat_lwr = data_rat[data_rat[, "Endpointname"] == "Liver weight, relative", ]
#Liver weight, absolute
rat_lwa = data_rat[data_rat[, "Endpointname"] == "Liver weight, absolute", ]
#Bilirubin (total)
rat_bt = data_rat[data_rat[, "Endpointname"] == "Bilirubin (total)", ]

#Human
#ALT
human_alt = data_human[data_human[, "Endpointname"] == "ALT", ]
#AST
human_ast = data_human[data_human[, "Endpointname"] == "AST", ]
#Bilirubin (free)
human_bf = data_human[data_human[, "Endpointname"] == "Bilirubin (free)", ]

#Non-human primate
#ALT
nhp_alt = data_nhp[data_nhp[, "Endpointname"] == "ALT", ]
#ALP
nhp_alp = data_nhp[data_nhp[, "Endpointname"] == "ALP", ]
#Bilirubin (total)
nhp_bt = data_nhp[data_nhp[, "Endpointname"] == "Bilirubin (total)", ]

##########################################################################################
#Subgroup by different drugs
#Rat_T
rat_alt_t <- subset(rat_alt, Drug == "Troglitazone/Rezulin")
rat_ast_t <- subset(rat_ast, Drug == "Troglitazone/Rezulin")
rat_lwa_t <- subset(rat_lwa, Drug == "Troglitazone/Rezulin")
rat_lwr_t <- subset(rat_lwr, Drug == "Troglitazone/Rezulin")
rat_bt_t <- subset(rat_bt, Drug == "Troglitazone/Rezulin")

#Human_T
human_alt_t <- subset(human_alt, Drug == "Troglitazone/Rezulin")
human_ast_t <- subset(human_ast, Drug == "Troglitazone/Rezulin")
#Human_R
human_alt_r <- subset(human_alt, Drug == "Rosiglitazone/Avandia")
human_ast_r <- subset(human_ast, Drug == "Rosiglitazone/Avandia")
human_bf_r <- subset(human_bf, Drug == "Rosiglitazone/Avandia")

#NHP_T
nhp_alt_t <- subset(nhp_alt, Drug == "Troglitazone/Rezulin")
nhp_alp_t <- subset(nhp_alp, Drug == "Troglitazone/Rezulin")
nhp_bt_t <- subset(nhp_bt, Drug == "Troglitazone/Rezulin")

##########################################################################################
library("forestplot")

##########################################################################################
#Rat_T
df <- as.data.frame(rat_bt_t)
df <- df[order(df$Year),]

tabletext <- cbind(c("Bibliography (year)", paste0(gsub("([A-Za-z]+).*", "\\1", df$Bibliography)," (",df$Year,")"), NA, NA),
                   c("Study ID",as.character(df$comparedID), NA, "Summary"),
                   c("Drug", as.character(df$Drug), NA, NA),
                   c("Timepoint", as.character(df$Timepoint.x), NA, NA),
                   c("Lower Limit", round(df$LL,2), NA, round(mean(df$LL),2)),
                   c("Effect Size (Endpointunit)", paste0(df$Effectsize," (",df$Endpointunit,")"), NA, round(mean(df$Effectsize),2)),
                   c("Upper Limit", round(df$UL,2), NA, round(mean(df$UL),2)),
                   c("Effect Percent", as.character(df$Effectpercent), NA, NA))

cochrane_from_rmeta <- structure(list(
  mean  = c(NA, df$Effectsize, NA, mean(df$Effectsize)), 
  lower = c(NA, df$LL, NA, mean(df$LL)),
  upper = c(NA, df$UL, NA, mean(df$UL))),
  row.names = c(NA, nrow(df)+3),
  class = "data.frame")

forestplot(title = paste("Forest Plot with", unique(df$Endpointname), "&", unique(df$Species), "(Control on Timepoint)"),
           tabletext, 
           hrzl_lines = gpar(col="#444444"),
           cochrane_from_rmeta,new_page = TRUE,
           is.summary=c(TRUE,rep(FALSE,nrow(df)+1),TRUE),
           clip=c(round(min(df$LL),2),round(max(df$UL),2)), 
           xlog=F,
           xlab = paste("lower than control","               ","higher than control"),
           txt_gp = fpTxtGp(xlab = gpar(cex = 1)),
           graphwidth = unit(3,"inches"),
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue", 
                        text = c("black", "darkred", "darkred", "black")))

#Human_T
df <- as.data.frame(human_ast_t)
df <- df[order(df$Year),]

tabletext <- cbind(c("Bibliography (year)", paste0(gsub("([A-Za-z]+).*", "\\1", df$Bibliography)," (",df$Year,")"), NA, NA),
                   c("Study ID",as.character(df$comparedID), NA, "Summary"),
                   c("Drug", as.character(df$Drug), NA, NA),
                   c("Timepoint", as.character(df$Timepoint.x), NA, NA),
                   c("Lower Limit", round(df$LL,2), NA, round(mean(df$LL),2)),
                   c("Effect Size (Endpointunit)", paste0(df$Effectsize," (",df$Endpointunit,")"), NA, round(mean(df$Effectsize),2)),
                   c("Upper Limit", round(df$UL,2), NA, round(mean(df$UL),2)),
                   c("Effect Percent", as.character(df$Effectpercent), NA, NA))

cochrane_from_rmeta <- structure(list(
  mean  = c(NA, df$Effectsize, NA, mean(df$Effectsize)), 
  lower = c(NA, df$LL, NA, mean(df$LL)),
  upper = c(NA, df$UL, NA, mean(df$UL))),
  row.names = c(NA, nrow(df)+3),
  class = "data.frame")

forestplot(title = paste("Forest Plot with", unique(df$Endpointname), "&", unique(df$Species), "(Control on Timepoint)"),
           tabletext, 
           hrzl_lines = gpar(col="#444444"),
           cochrane_from_rmeta,new_page = TRUE,
           is.summary=c(TRUE,rep(FALSE,nrow(df)+1),TRUE),
           clip=c(round(min(df$LL),2),round(max(df$UL),2)), 
           xlog=F,
           xlab = paste("lower than control","               ","higher than control"),
           txt_gp = fpTxtGp(xlab = gpar(cex = 1)),
           graphwidth = unit(3,"inches"),
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue", 
                        text = c("black", "darkred", "darkred", "black")))

#Human_R
df <- as.data.frame(human_bf_r)
df <- df[order(df$Year),]

tabletext <- cbind(c("Bibliography (year)", paste0(gsub("([A-Za-z]+).*", "\\1", df$Bibliography)," (",df$Year,")"), NA, NA),
                   c("Study ID",as.character(df$comparedID), NA, "Summary"),
                   c("Drug", as.character(df$Drug), NA, NA),
                   c("Timepoint", as.character(df$Timepoint.x), NA, NA),
                   c("Lower Limit", round(df$LL,2), NA, round(mean(df$LL),2)),
                   c("Effect Size (Endpointunit)", paste0(df$Effectsize," (",df$Endpointunit,")"), NA, round(mean(df$Effectsize),2)),
                   c("Upper Limit", round(df$UL,2), NA, round(mean(df$UL),2)),
                   c("Effect Percent", as.character(df$Effectpercent), NA, NA))

cochrane_from_rmeta <- structure(list(
  mean  = c(NA, df$Effectsize, NA, mean(df$Effectsize)), 
  lower = c(NA, df$LL, NA, mean(df$LL)),
  upper = c(NA, df$UL, NA, mean(df$UL))),
  row.names = c(NA, nrow(df)+3),
  class = "data.frame")

forestplot(title = paste("Forest Plot with", unique(df$Endpointname), "&", unique(df$Species), "(Control on Timepoint)"),
           tabletext, 
           hrzl_lines = gpar(col="#444444"),
           cochrane_from_rmeta,new_page = TRUE,
           is.summary=c(TRUE,rep(FALSE,nrow(df)+1),TRUE),
           clip=c(round(min(df$LL),2),round(max(df$UL),2)), 
           xlog=F,
           xlab = paste("lower than control","               ","higher than control"),
           txt_gp = fpTxtGp(xlab = gpar(cex = 1)),
           graphwidth = unit(3,"inches"),
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue", 
                        text = c("black", "darkgreen",
                                 "black")))

#NHP_T
df <- as.data.frame(nhp_alp_t)
df <- df[order(df$Year),]

tabletext <- cbind(c("Bibliography (year)", paste0(gsub("([A-Za-z]+).*", "\\1", df$Bibliography)," (",df$Year,")"), NA, NA),
                   c("Study ID",as.character(df$comparedID), NA, "Summary"),
                   c("Drug", as.character(df$Drug), NA, NA),
                   c("Timepoint", as.character(df$Timepoint.x), NA, NA),
                   c("Lower Limit", round(df$LL,2), NA, round(mean(df$LL),2)),
                   c("Effect Size (Endpointunit)", paste0(df$Effectsize," (",df$Endpointunit,")"), NA, round(mean(df$Effectsize),2)),
                   c("Upper Limit", round(df$UL,2), NA, round(mean(df$UL),2)),
                   c("Effect Percent", as.character(df$Effectpercent), NA, NA))

cochrane_from_rmeta <- structure(list(
  mean  = c(NA, df$Effectsize, NA, mean(df$Effectsize)), 
  lower = c(NA, df$LL, NA, mean(df$LL)),
  upper = c(NA, df$UL, NA, mean(df$UL))),
  row.names = c(NA, nrow(df)+3),
  class = "data.frame")

forestplot(title = paste("Forest Plot with", unique(df$Endpointname), "&", unique(df$Species), "(Control on Timepoint)"),
           tabletext, 
           hrzl_lines = gpar(col="#444444"),
           cochrane_from_rmeta,new_page = TRUE,
           is.summary=c(TRUE,rep(FALSE,nrow(df)+1),TRUE),
           clip=c(round(min(df$LL),2),round(max(df$UL),2)), 
           xlog=F,
           xlab = paste("lower than control","               ","higher than control"),
           txt_gp = fpTxtGp(xlab = gpar(cex = 1)),
           graphwidth = unit(3,"inches"),
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue", 
                        text = c("black", "darkred", "darkred","darkred", "darkred",
                                 "darkred", "darkred","darkred", "darkred",
                                 "darkred", "darkred","darkred", "darkred",
                                 "black")))

#Multiple Drugs
#human_ast, human_alt
df <- as.data.frame(rat_bt)
df <- df[order(df$Drug, df$Year),]

tabletext <- cbind(c("Bibliography (year)", paste0(gsub("([A-Za-z]+).*", "\\1", df$Bibliography)," (",df$Year,")"), NA, NA),
                   c("Study ID",as.character(df$comparedID), NA, "Summary"),
                   c("Drug", as.character(df$Drug), NA, NA),
                   c("Daily Dose", paste0(as.character(df$DailyDose.x), " (",df$UnitofDose,")"), NA, NA),
                   c("Lower Limit", round(df$LL,2), NA, round(mean(df$LL),2)),
                   c("Effect Size (Endpointunit)", paste0(df$Effectsize," (",df$Endpointunit,")"), NA, round(mean(df$Effectsize),2)),
                   c("Upper Limit", round(df$UL,2), NA, round(mean(df$UL),2)),
                   c("Effect Percent", as.character(df$Effectpercent), NA, NA))

cochrane_from_rmeta <- structure(list(
  mean  = c(NA, df$Effectsize, NA, mean(df$Effectsize)), 
  lower = c(NA, df$LL, NA, mean(df$LL)),
  upper = c(NA, df$UL, NA, mean(df$UL))),
  row.names = c(NA, nrow(df)+3),
  class = "data.frame")

forestplot(title = paste("Forest Plot with", unique(df$Endpointname), "&", unique(df$Species), "(Control on Daily Dose)"),
           tabletext, 
           hrzl_lines = gpar(col="#444444"),
           cochrane_from_rmeta,new_page = TRUE,
           is.summary=c(TRUE,rep(FALSE,nrow(df)+1),TRUE),
           clip=c(round(min(df$LL),2),round(max(df$UL),2)), 
           xlog=F,
           xlab = paste("lower than control", "              ", "higher than control"),
           txt_gp = fpTxtGp(xlab = gpar(cex = 1)),
           graphwidth = unit(3,"inches"),
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue", 
                        text = c("black", 
                                 "darkgreen", "darkgreen", 
                                 "darkred", "darkred",
                                 "black")))