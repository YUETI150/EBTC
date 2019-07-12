#Import the dataset
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

##################################################################################################
#Subgroup by Species
data_rat = datad[datad[, "Species"] == "Rat", ]
data_mouse = datad[datad[, "Species"] == "Mouse", ]
data_nhp = datad[datad[, "Species"] == "Non-human primate", ]

##################################################################################################
#Subgroup by Species & Endpoint name
#Rat
#ALT
rat_alt = data_rat[data_rat[, "Endpointname"] == "ALT", ]
#AST
rat_ast = data_rat[data_rat[, "Endpointname"] == "AST", ]
#ALP
rat_alp = data_rat[data_rat[, "Endpointname"] == "ALP", ]
#Liver weight, relative
rat_lwr = data_rat[data_rat[, "Endpointname"] == "Liver weight, relative", ]
#Liver weight, absolute
rat_lwa = data_rat[data_rat[, "Endpointname"] == "Liver weight, absolute", ]
#Gamma-glutamyl transferase (γGT)
rat_ggt = data_rat[data_rat[, "Endpointname"] == "Gamma-glutamyl transferase (γGT)", ]
#SGOT
rat_sgot = data_rat[data_rat[, "Endpointname"] == "SGOT", ]
#Albumin
rat_alb = data_rat[data_rat[, "Endpointname"] == "Albumin", ]
#Bilirubin (total)
rat_bt = data_rat[data_rat[, "Endpointname"] == "Bilirubin (total)", ]
#Bilirubin (direct)
rat_bd = data_rat[data_rat[, "Endpointname"] == "Bilirubin (direct)", ]

#Mouse
#ALT
mouse_alt = data_mouse[data_mouse[, "Endpointname"] == "ALT", ]
#AST
mouse_ast = data_mouse[data_mouse[, "Endpointname"] == "AST", ]
#Liver weight, absolute
mouse_lwa = data_mouse[data_mouse[, "Endpointname"] == "Liver weight, absolute", ]
#Trialylglycerol
mouse_tri = data_mouse[data_mouse[, "Endpointname"] == "Trialylglycerol", ]

#Non-human primate
#Liver weight, relative
nhp_lwr = data_nhp[data_nhp[, "Endpointname"] == "Liver weight, relative", ]
#Liver weight, absolute
nhp_lwa = data_nhp[data_nhp[, "Endpointname"] == "Liver weight, absolute", ]

##################################################################################################
#Subgroup by different drugs
#Rat_T
rat_alt_t <- subset(rat_alt, Drug == "Troglitazone/Rezulin")
rat_ast_t <- subset(rat_ast, Drug == "Troglitazone/Rezulin")
rat_alp_t <- subset(rat_alp, Drug == "Troglitazone/Rezulin")
rat_lwa_t <- subset(rat_lwa, Drug == "Troglitazone/Rezulin")
rat_lwr_t <- subset(rat_lwr, Drug == "Troglitazone/Rezulin")
rat_bt_t <- subset(rat_bt, Drug == "Troglitazone/Rezulin")
rat_bd_t <- subset(rat_bd, Drug == "Troglitazone/Rezulin")
#Rat_R
rat_alt_r <- subset(rat_alt, Drug == "Rosiglitazone/Avandia")
rat_ast_r <- subset(rat_ast, Drug == "Rosiglitazone/Avandia")
rat_alp_r <- subset(rat_alp, Drug == "Rosiglitazone/Avandia")
rat_lwa_r <- subset(rat_lwa, Drug == "Rosiglitazone/Avandia")
rat_lwr_r <- subset(rat_lwr, Drug == "Rosiglitazone/Avandia")
rat_ggt_r <- subset(rat_ggt, Drug == "Rosiglitazone/Avandia")
rat_sgot_r <- subset(rat_sgot, Drug == "Rosiglitazone/Avandia")
rat_alb_r <- subset(rat_alb, Drug == "Rosiglitazone/Avandia")
rat_bt_r <- subset(rat_bt, Drug == "Rosiglitazone/Avandia")

#Mouse_R
mouse_alt_r <- subset(mouse_alt, Drug == "Rosiglitazone/Avandia")
mouse_ast_r <- subset(mouse_ast, Drug == "Rosiglitazone/Avandia")
mouse_lwa_r <- subset(mouse_lwa, Drug == "Rosiglitazone/Avandia")
mouse_tri_r <- subset(mouse_tri, Drug == "Rosiglitazone/Avandia")

#NHP_T
nhp_lwa_t <- subset(nhp_lwa, Drug == "Troglitazone/Rezulin")
nhp_lwr_t <- subset(nhp_lwr, Drug == "Troglitazone/Rezulin")

##########################################################################################
library("forestplot")

##########################################################################################
#Rat_T
df <- as.data.frame(rat_bd_t)
df <- df[order(df$Year),]

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
           xlab = paste("lower than control","                   ","higher than control"),
           txt_gp = fpTxtGp(xlab = gpar(cex = 1)),
           graphwidth = unit(3,"inches"),
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue", 
                        text = c("black", "darkred", "darkred", "black")))

#Rat_R
df <- as.data.frame(rat_bt_r)
df <- df[order(df$Year),]

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
           xlab = paste("lower than control"),
           txt_gp = fpTxtGp(xlab = gpar(cex = 1)),
           graphwidth = unit(3,"inches"),
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue", 
                        text = c("black", "darkgreen", "darkgreen",
                                 "black")))

#Mouse_R
df <- as.data.frame(mouse_tri_r)
df <- df[order(df$Year),]

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
                        text = c("black", "darkgreen",
                                 "black")))

#NHP_T
df <- as.data.frame(nhp_lwr_t)
df <- df[order(df$Year),]

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
           xlab = paste("higher than control"),
           txt_gp = fpTxtGp(xlab = gpar(cex = 1)),
           graphwidth = unit(3,"inches"),
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue", 
                        text = c("black", "darkred", "darkred", "darkred", "black")))

#Multiple Drugs
#rat_alt, rat_ast, rat_alp, rat_lwa, rat_lwr, rat_bt
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