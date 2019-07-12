#import dataset
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
datatp$Timepoint.x[datatp$Timepoint.x == "12 month"] <- "12 months"
datatp$Timepoint.x[datatp$Timepoint.x == "Month 3"] <- "3 months"
datatp$Timepoint.x[datatp$Timepoint.x == "Month 8"] <- "8 months"
datatp$Timepoint.x[datatp$Timepoint.x == "52 weeks"] <- "12 months"

#############################################################################
#Subgroup by Drugs
data_T <- datatp[datatp[, "Drug"] == "Troglitazone/Rezulin", ]
data_R <- datatp[datatp[, "Drug"] == "Rosiglitazone/Avandia", ]

#############################################################################
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
#Liver weight, absolute
drugT_lwa <- data_T[data_T[, "Endpointname"] == "Liver weight, absolute", ]
#Liver weight, relative
drugT_lwr <- data_T[data_T[, "Endpointname"] == "Liver weight, relative", ]

#Drug R
#ALT
drugR_alt <- data_R[data_R[, "Endpointname"] == "ALT", ]
#AST
drugR_ast <- data_R[data_R[, "Endpointname"] == "AST", ]
#Bilirubin (free)
drugR_bf <- data_R[data_R[, "Endpointname"] == "Bilirubin (free)", ]

#############################################################################
#Subgroup by different species
#T
#Human
drugT_alt_human <- subset(drugT_alt, Species == "Human")
drugT_ast_human <- subset(drugT_ast, Species == "Human")
#Rat
drugT_alt_rat <- subset(drugT_alt, Species == "Rat")
drugT_ast_rat <- subset(drugT_ast, Species == "Rat")
drugT_bt_rat <- subset(drugT_bt, Species == "Rat")
drugT_lwa_rat <- subset(drugT_lwa, Species == "Rat")
drugT_lwr_rat <- subset(drugT_lwr, Species == "Rat")
#Non-human primate
drugT_alt_nhp <- subset(drugT_alt, Species == "Non-human primate")
drugT_alp_nhp <- subset(drugT_alp, Species == "Non-human primate")
drugT_bt_nhp <- subset(drugT_bt, Species == "Non-human primate")

#R
#Human
drugR_alt_human <- subset(drugR_alt, Species == "Human")
drugR_ast_human <- subset(drugR_ast, Species == "Human")
drugR_bf_human <- subset(drugR_bf, Species == "Human")

###########################################################################################
library("forestplot")

###########################################################################################
#T (Adjust df and text color for requested plot)
df <- as.data.frame(drugT_alt_human)
df <- df[order(df$Year),]

tabletext <- cbind(c("Bibliography (year)", paste0(gsub("([A-Za-z]+).*", "\\1", df$Bibliography)," (",df$Year,")"), NA, NA),
                   c("Study ID",as.character(df$comparedID), NA, "Summary"),
                   c("Species", as.character(df$Species), NA, NA),
                   c("Timepoint", as.character(df$Timepoint.x), NA, NA),
                   c("Lower Limit", round(df$LL,2), NA, round(mean(df$LL),2)),
                   c("Effect Size", paste0(df$Effectsize," (",df$Endpointunit,")"), NA, round(mean(df$Effectsize),2)),
                   c("Upper Limit", round(df$UL,2), NA, round(mean(df$UL),2)),
                   c("Effect Percentage", as.character(df$Effectpercent), NA, NA))

cochrane_from_rmeta <- structure(list(
  mean  = c(NA, df$Effectsize, NA, mean(df$Effectsize)), 
  lower = c(NA, df$LL, NA, mean(df$LL)),
  upper = c(NA, df$UL, NA, mean(df$UL))),
  row.names = c(NA, nrow(df)+3),
  class = "data.frame")

forestplot(title = paste("Forest Plot with", unique(df$Drug), "&", unique(df$Endpointname), "(Control on Timepoint)"),
           tabletext, 
           hrzl_lines = gpar(col="#444444"),
           cochrane_from_rmeta,new_page = TRUE,
           is.summary=c(TRUE,rep(FALSE,nrow(df)+1),TRUE),
           clip=c(round(min(df$LL),2),round(max(df$UL),2)), 
           xlog=F,
           xlab = paste("lower than control","                     ","higher than control"),
           txt_gp = fpTxtGp(xlab = gpar(cex = 1)),
           graphwidth = unit(3,"inches"),
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue",
                        text = c("black","darkred", "darkred", "black")))

#R (Adjust df and text color for requested plot)
df <- as.data.frame(drugR_alt_human)
df <- df[order(df$Year),]

tabletext <- cbind(c("Bibliography (year)", paste0(gsub("([A-Za-z]+).*", "\\1", df$Bibliography)," (",df$Year,")"), NA, NA),
                   c("Study ID",as.character(df$comparedID), NA, "Summary"),
                   c("Species", as.character(df$Species), NA, NA),
                   c("Timepoint", as.character(df$Timepoint.x), NA, NA),
                   c("Lower Limit", round(df$LL,2), NA, round(mean(df$LL),2)),
                   c("Effect Size", paste0(df$Effectsize," (",df$Endpointunit,")"), NA, round(mean(df$Effectsize),2)),
                   c("Upper Limit", round(df$UL,2), NA, round(mean(df$UL),2)),
                   c("Effect Percentage", as.character(df$Effectpercent), NA, NA))

cochrane_from_rmeta <- structure(list(
  mean  = c(NA, df$Effectsize, NA, mean(df$Effectsize)), 
  lower = c(NA, df$LL, NA, mean(df$LL)),
  upper = c(NA, df$UL, NA, mean(df$UL))),
  row.names = c(NA, nrow(df)+3),
  class = "data.frame")

forestplot(title = paste("Forest Plot with", unique(df$Drug), "&", unique(df$Endpointname), "(Control on Timepoint)"),
           tabletext, 
           hrzl_lines = gpar(col="#444444"),
           cochrane_from_rmeta,new_page = TRUE,
           is.summary=c(TRUE,rep(FALSE,nrow(df)+1),TRUE),
           clip=c(round(min(df$LL),2),round(max(df$UL),2)), 
           xlog=F,
           xlab = paste("lower than control","                     ","higher than control"),
           txt_gp = fpTxtGp(xlab = gpar(cex = 1)),
           graphwidth = unit(3,"inches"),
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue",
                        text = c("black", "darkgreen", "darkgreen", "darkgreen", "darkgreen", 
                                 "darkgreen",  "darkgreen", "darkgreen",
                                 "black")))

#T without subgrouping species
#T_ALT (rat, human, non-human primate)
df <- as.data.frame(drugT_alt)
df <- df[order(df$Year),]

tabletext <- cbind(c("Bibliography (year)", paste0(gsub("([A-Za-z]+).*", "\\1", df$Bibliography)," (",df$Year,")"), NA, NA),
                   c("Study ID",as.character(df$comparedID), NA, "Summary"),
                   c("Species", as.character(df$Species), NA, NA),
                   c("Timepoint", as.character(df$Timepoint.x), NA, NA),
                   c("Lower Limit", round(df$LL,2), NA, round(mean(df$LL),2)),
                   c("Effect Size", paste0(df$Effectsize," (",df$Endpointunit,")"), NA, round(mean(df$Effectsize),2)),
                   c("Upper Limit", round(df$UL,2), NA, round(mean(df$UL),2)),
                   c("Effect Percentage", as.character(df$Effectpercent), NA, NA))

cochrane_from_rmeta <- structure(list(
  mean  = c(NA, df$Effectsize, NA, mean(df$Effectsize)), 
  lower = c(NA, df$LL, NA, mean(df$LL)),
  upper = c(NA, df$UL, NA, mean(df$UL))),
  row.names = c(NA, nrow(df)+3),
  class = "data.frame")

forestplot(title = paste("Forest Plot with", unique(df$Drug), "&", unique(df$Endpointname), "(Control on Timepoint)"),
           tabletext, 
           hrzl_lines = gpar(col="#444444"),
           cochrane_from_rmeta,new_page = TRUE,
           is.summary=c(TRUE,rep(FALSE,nrow(df)+1),TRUE),
           clip=c(round(min(df$LL),2),round(max(df$UL),2)), 
           xlog=F,
           xlab = paste("lower than control","                     ","higher than control"),
           txt_gp = fpTxtGp(xlab = gpar(cex = 1)),
           graphwidth = unit(3,"inches"),
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue",
                        text = c("black","darkred", "darkred", "darkred", "darkred",
                                 "darkred", "darkred", "darkred", "darkred",
                                 "darkred", "darkred", "darkred", "darkred",
                                 "darkred", "darkred", "darkred", "darkred",
                                 "darkred", "darkred", "darkred", "darkred",
                                 "darkred", "darkred", "darkred",
                                 "black")))
#T_AST (rat, human)
df <- as.data.frame(drugT_ast)
df <- df[order(df$Year),]

tabletext <- cbind(c("Bibliography (year)", paste0(gsub("([A-Za-z]+).*", "\\1", df$Bibliography)," (",df$Year,")"), NA, NA),
                   c("Study ID",as.character(df$comparedID), NA, "Summary"),
                   c("Species", as.character(df$Species), NA, NA),
                   c("Timepoint", as.character(df$Timepoint.x), NA, NA),
                   c("Lower Limit", round(df$LL,2), NA, round(mean(df$LL),2)),
                   c("Effect Size", paste0(df$Effectsize," (",df$Endpointunit,")"), NA, round(mean(df$Effectsize),2)),
                   c("Upper Limit", round(df$UL,2), NA, round(mean(df$UL),2)),
                   c("Effect Percentage", as.character(df$Effectpercent), NA, NA))

cochrane_from_rmeta <- structure(list(
  mean  = c(NA, df$Effectsize, NA, mean(df$Effectsize)), 
  lower = c(NA, df$LL, NA, mean(df$LL)),
  upper = c(NA, df$UL, NA, mean(df$UL))),
  row.names = c(NA, nrow(df)+3),
  class = "data.frame")

forestplot(title = paste("Forest Plot with", unique(df$Drug), "&", unique(df$Endpointname), "(Control on Timepoint)"),
           tabletext, 
           hrzl_lines = gpar(col="#444444"),
           cochrane_from_rmeta,new_page = TRUE,
           is.summary=c(TRUE,rep(FALSE,nrow(df)+1),TRUE),
           clip=c(round(min(df$LL),2),round(max(df$UL),2)), 
           xlog=F,
           xlab = paste("lower than control","                     ","higher than control"),
           txt_gp = fpTxtGp(xlab = gpar(cex = 1)),
           graphwidth = unit(3,"inches"),
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue",
                        text = c("black","darkred", "darkred", "darkred", "darkred",
                                 "darkred", "darkred",
                                 "black")))