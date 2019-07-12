#import dataset
data <- read.csv("https://raw.githubusercontent.com/chang5307/test/master/data_forestplot.csv", sep = ",", encoding = "UTF-8")

#Data preprocessing 7 ~ 89
#Forest plot 92

#Adjust data contents
data$Endpointname[data$Endpointname == "liver weight, relative"] <- "Liver weight, relative"
data <- subset(data, UnitofDose != "IU/L")
datad <- subset(data, DailyDose.y == 0 & Timepoint.y != 0)

datad$UnitofDose[datad$UnitofDose == "mg/kg/diet"] <- "mg/kg"
datad$DailyDose.x[datad$comparedID == "141.1.1"] <- 66.67
datad$UnitofDose[datad$UnitofDose == "g"] <- "mg/kg"

###########################################################################################
#Subgroup by Drugs
data_T <- datad[datad[, "Drug"] == "Troglitazone/Rezulin", ]
data_R <- datad[datad[, "Drug"] == "Rosiglitazone/Avandia", ]

###########################################################################################
#Subgroup by Drug & Endpoint name
#Drug T
#ALT
drugT_alt <- data_T[data_T[, "Endpointname"] == "ALT", ]
#AST
drugT_ast <- data_T[data_T[, "Endpointname"] == "AST", ]
#ALP
drugT_alp <- data_T[data_T[, "Endpointname"] == "ALP", ]
#Bilirubin (total)
drugT_bt <- data_T[data_T[, "Endpointname"] == "Bilirubin (total)", ]
#Bilirubin (direct)
drugT_bd <- data_T[data_T[, "Endpointname"] == "Bilirubin (direct)", ]
#Liver weight, absolute
drugT_lwa <- data_T[data_T[, "Endpointname"] == "Liver weight, absolute", ]
#Liver weight, relative
drugT_lwr <- data_T[data_T[, "Endpointname"] == "Liver weight, relative", ]

#Drug R
#ALT
drugR_alt <- data_R[data_R[, "Endpointname"] == "ALT", ]
#AST
drugR_ast <- data_R[data_R[, "Endpointname"] == "AST", ]
#ALP
drugR_alp <- data_R[data_R[, "Endpointname"] == "ALP", ]
#Bilirubin (total)
drugR_bt <- data_R[data_R[, "Endpointname"] == "Bilirubin (total)", ]
#Gamma-glutamyl transferase (γGT)
drugR_ggt <- data_R[data_R[, "Endpointname"] == "Gamma-glutamyl transferase (γGT)", ]
#SGOT
drugR_sgot <- data_R[data_R[, "Endpointname"] == "SGOT", ]
#Trialylglycerol
drugR_tri <- data_R[data_R[, "Endpointname"] == "Trialylglycerol", ]
#Liver weight, absolute
drugR_lwa <- data_R[data_R[, "Endpointname"] == "Liver weight, absolute", ]
#Liver weight, relative
drugR_lwr <- data_R[data_R[, "Endpointname"] == "Liver weight, relative", ]

###########################################################################################
#Subgroup by different species
#T
#Rat
drugT_alt_rat <- subset(drugT_alt, Species == "Rat")
drugT_ast_rat <- subset(drugT_ast, Species == "Rat")
drugT_alp_rat <- subset(drugT_alp, Species == "Rat")
drugT_bt_rat <- subset(drugT_bt, Species == "Rat")
drugT_bd_rat <- subset(drugT_bd, Species == "Rat")
drugT_lwa_rat <- subset(drugT_lwa, Species == "Rat")
drugT_lwr_rat <- subset(drugT_lwr, Species == "Rat")
#Non-human primate
drugT_lwa_nhp <- subset(drugT_lwa, Species == "Non-human primate")
drugT_lwr_nhp <- subset(drugT_lwr, Species == "Non-human primate")

#R
#Rat
drugR_alt_rat <- subset(drugR_alt, Species == "Rat")
drugR_ast_rat <- subset(drugR_ast, Species == "Rat")
drugR_alp_rat <- subset(drugR_alp, Species == "Rat")
drugR_bt_rat <- subset(drugR_bt, Species == "Rat")
drugR_ggt_rat <- subset(drugR_ggt, Species == "Rat")
drugR_sgot_rat <- subset(drugR_sgot, Species == "Rat")
drugR_lwa_rat <- subset(drugR_lwa, Species == "Rat")
drugR_lwr_rat <- subset(drugR_lwr, Species == "Rat")
#Mouse
drugR_alt_mouse <- subset(drugR_alt, Species == "Mouse")
drugR_ast_mouse <- subset(drugR_ast, Species == "Mouse")
drugR_tri_mouse <- subset(drugR_tri, Species == "Mouse")
drugR_lwa_mouse <- subset(drugR_lwa, Species == "Mouse")
drugR_lwr_mouse <- subset(drugR_lwr, Species == "Mouse")

###########################################################################################
library("forestplot")

###########################################################################################
#R (Adjust df and text color for requested plot)
df <- as.data.frame(drugR_lwr_mouse)
df <- df[order(df$Year),]

tabletext <- cbind(c("Bibliography (year)", paste0(gsub("([A-Za-z]+).*", "\\1", df$Bibliography)," (",df$Year,")"), NA, NA),
                   c("Study ID",as.character(df$comparedID), NA, "Summary"),
                   c("Species", as.character(df$Species), NA, NA),
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

forestplot(title = paste("Forest Plot with", unique(df$Drug), "&", unique(df$Endpointname), "(Control on Daily Dose)"),
           tabletext, 
           hrzl_lines = gpar(col="#444444"),
           cochrane_from_rmeta,new_page = TRUE,
           is.summary=c(TRUE,rep(FALSE,nrow(df)+1),TRUE),
           clip=c(round(min(df$LL),2),round(max(df$UL),2)), 
           xlog=F,
           xlab = paste("lower than control", "                        ","higher than control"),
           txt_gp = fpTxtGp(xlab = gpar(cex = 1)),
           graphwidth = unit(3,"inches"),
           col = fpColors(box="royalblue",line="darkblue", summary="royalblue",
                          text = c("black", "darkgreen",
                                   "black")))

#T (Adjust df and text color for requested plot)
df <- as.data.frame(drugT_lwa_nhp)
df <- df[order(df$Year),]

tabletext <- cbind(c("Bibliography (year)", paste0(gsub("([A-Za-z]+).*", "\\1", df$Bibliography)," (",df$Year,")"), NA, NA),
                   c("Study ID",as.character(df$comparedID), NA, "Summary"),
                   c("Species", as.character(df$Species), NA, NA),
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

forestplot(title = paste("Forest Plot with", unique(df$Drug), "&", unique(df$Endpointname), "(Control on Daily Dose)"),
           tabletext, 
           hrzl_lines = gpar(col="#444444"),
           cochrane_from_rmeta,new_page = TRUE,
           is.summary=c(TRUE,rep(FALSE,nrow(df)+1),TRUE),
           clip=c(round(min(df$LL),2),round(max(df$UL),2)), 
           xlog=F,
           xlab = paste("lower than control", "                        ","higher than control"),
           txt_gp = fpTxtGp(xlab = gpar(cex = 1)),
           graphwidth = unit(3,"inches"),
           col = fpColors(box="royalblue",line="darkblue", summary="royalblue",
                          text = c("black", "darkred", "darkred", "darkred",
                                   "black")))

#R without subgrouping species
#R_ALT (rat, mouse)
#R_AST (rat, mouse)
#R_LWA (rat, mouse)
#R_LWR (rat, mouse)
#Adjust df and text color for requested plot
df <- as.data.frame(drugR_lwr)
df <- df[order(df$Species, df$Year),]

tabletext <- cbind(c("Bibliography (year)", paste0(gsub("([A-Za-z]+).*", "\\1", df$Bibliography)," (",df$Year,")"), NA, NA),
                   c("Study ID",as.character(df$comparedID), NA, "Summary"),
                   c("Species", as.character(df$Species), NA, NA),
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

forestplot(title = paste("Forest Plot with", unique(df$Drug), "&", unique(df$Endpointname), "(Control on Daily Dose)"),
           tabletext, 
           hrzl_lines = gpar(col="#444444"),
           cochrane_from_rmeta,new_page = TRUE,
           is.summary=c(TRUE,rep(FALSE,nrow(df)+1),TRUE),
           clip=c(round(min(df$LL),2),round(max(df$UL),2)), 
           xlog=F,
           xlab = paste("lower than control", "                        ","higher than control"),
           txt_gp = fpTxtGp(xlab = gpar(cex = 1)),
           graphwidth = unit(3,"inches"),
           col = fpColors(box="royalblue",line="darkblue", summary="royalblue",
                          text = c("black", "darkgreen", "darkgreen", "darkgreen", "darkgreen", "darkgreen", "darkgreen",
                                   "black")))

#T without subgrouping species
#T_LWA (rat, nhp)
#T_LWR (rat, nhp)
#Adjust df and text color for requested plot
df <- as.data.frame(drugT_lwr)
df <- df[order(df$Year),]

tabletext <- cbind(c("Bibliography (year)", paste0(gsub("([A-Za-z]+).*", "\\1", df$Bibliography)," (",df$Year,")"), NA, NA),
                   c("Study ID",as.character(df$comparedID), NA, "Summary"),
                   c("Species", as.character(df$Species), NA, NA),
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

forestplot(title = paste("Forest Plot with", unique(df$Drug), "&", unique(df$Endpointname), "(Control on Daily Dose)"),
           tabletext, 
           hrzl_lines = gpar(col="#444444"),
           cochrane_from_rmeta,new_page = TRUE,
           is.summary=c(TRUE,rep(FALSE,nrow(df)+1),TRUE),
           clip=c(round(min(df$LL),2),round(max(df$UL),2)), 
           xlog=F,
           xlab = paste("lower than control", "                        ","higher than control"),
           txt_gp = fpTxtGp(xlab = gpar(cex = 1)),
           graphwidth = unit(3,"inches"),
           col = fpColors(box="royalblue",line="darkblue", summary="royalblue",
                          text = c("black", "darkred", "darkred", "darkred", "darkred", "darkred",
                                   "black")))