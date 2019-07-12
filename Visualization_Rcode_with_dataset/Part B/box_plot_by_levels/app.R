library(openxlsx)
library(ggplot2)
library(plotly)
library(shiny)
library(dplyr)
library(readxl)
library(gridExtra)
library(rsconnect)
library(scales)
library(DT)
library(plotly)
library(stringr)


df <- read.xlsx("dta_partB.xlsx")


df$ResearchName <- str_match(df$Bibliography, "(.*?), (.*?).")[, 1]
df$ResearchYear <- str_match(df$Bibliography, "\\d{4}")

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
df$Endpointname[df$Endpointname == "SGOT"] <- "AST"

df <- df[order(df$DailyDose.x, decreasing = FALSE),]
rownames(df) <- NULL

df$Levels <- NA
df$Levels[1:46] <- "Low"
df$Levels[47:92] <- "Medium"
df$Levels[93:137] <- "High"
table(df$Levels)

df$Effectpercent <- (df$MeanofEffectvalue_treated)/(df$MeanofEffectvalue_control)*100





ui <- fluidPage(
        headerPanel(title = "EBTC Tox21 Systematic Review Results: DILI biomarkers across levels"),
        sidebarPanel( width = 3,
                      selectInput("endpointname", "Endpoint Name:", 
                                  c("All" = "All",
                                    "AST" = "AST",
                                    "ALT" = "ALT",
                                    "ALP" = "ALP",
                                    "Bilirubin (free)" = "Bilirubin (free)",
                                    "Bilirubin (total)" = "Bilirubin (total)",
                                    "Bilirubin (direct)" = "Bilirubin (direct)",
                                    "Albumin" = "Albumin",
                                    "Liver weight, relative" = "Liver weight, relative",
                                    "Liver weight, absolute" = "Liver weight, absolute",
                                    "Gamma-glutamyl transferase (γGT)" = "Gamma-glutamyl transferase (γGT)",
                                    "Trialylglycerol" = "Trialylglycerol")),
                      selectInput("Levels", "Levels:", 
                                  c("All" = "All",
                                    "Low" = "Low",
                                    "Medium" = "Medium",
                                    "High" = "High")),
                      checkboxInput("overlay", "Without overlay", FALSE)
                      #checkboxInput("logy", "logarithmic Y-axis", FALSE)
        ),
        
        mainPanel(
                plotlyOutput("plot", 
                             width = "105%", 
                             height = 700),
                align = "center"
        )
)



server <- function(input, output) {
        
        
        
        output$plot <- renderPlotly({
                
                validate(
                        need(input$Levels %in% c(df$Levels[if(input$endpointname=="All"){df$Endpointname==df$Endpointname}else{df$Endpointname == input$endpointname}], "All"), "Data doesn't exist, please select others")
                )
                
                
                if(input$endpointname == "All"){
                        df <- df
                } else {
                        df <- df[df$Endpointname == input$endpointname, ]
                }
                
                
                
                
                if(input$Levels == "All" & input$overlay == TRUE){
                        
                       
                        l <- list(x = 0.02, y = 0.98, bgcolor = "rgba(0,0,0,0)")
                        ggplot(data = df, aes(x=Drug, y=Effectpercent, fill = Drug, text = c(paste0("Research Year: ", ResearchYear,"\n","Research Name: ", ResearchName,"\n","Study ID: ", comparedID)))) + 
                                geom_boxplot(outlier.colour="grey", outlier.shape=16, outlier.size=2) +
                                geom_point(pch = 21, position = position_jitterdodge())+
                                scale_y_continuous(breaks = seq(20, 600, by = 20)) + 
                                scale_fill_manual(
                                        breaks = c("Rosiglitazone/Avandia", "Troglitazone/Rezulin"),
                                        values=c("green", "red")) +
                                facet_grid(.~ordered(Levels, levels = c("Low", "Medium", "High"))) +
                                theme( axis.text.x=element_blank(), axis.title.y=element_blank(),
                                       axis.ticks.x=element_blank()) + 
                                xlab("Levels")
                        #scale_y_continuous(breaks = c(-512,-256,-128,-64,-32,-16,-8,-4,-2,0,2,2^2,2^3,2^4,2^5,2^6,2^7,2^8))+
                        #coord_trans(y = "log2")+
                        #theme_bw()+
                        #theme(legend.position = "top")
                        ggplotly() %>% layout(boxmode = "group", 
                                              legend = l,
                                              yaxis = list(
                                                      title = "Effect compared with control (%)"))
                        
                        
                } else if(input$Levels == "All" & input$overlay == FALSE){
                        
                        l <- list(x = 0.02, y = 0.98, bgcolor = "rgba(0,0,0,0)")
                        ggplot(data = df, aes(x=Drug, y=Effectpercent, fill = Drug, text = c(paste0("Research Year: ", ResearchYear,"\n","Research Name: ", ResearchName,"\n","Study ID: ", comparedID)))) + 
                                geom_boxplot(outlier.colour="grey", outlier.shape=16, outlier.size=2) +
                                scale_y_continuous(breaks = seq(20, 600, by = 20))+
                                scale_fill_manual(
                                        breaks = c("Rosiglitazone/Avandia", "Troglitazone/Rezulin"),
                                        values=c("green", "red")) +
                                facet_grid(.~ordered(Levels, levels = c("Low", "Medium", "High")))+
                                theme( axis.text.x=element_blank(),axis.title.y=element_blank(),
                                       axis.ticks.x=element_blank()) + 
                                xlab("Levels")
                        
                        #scale_y_continuous(breaks = c(-512,-256,-128,-64,-32,-16,-8,-4,-2,0,2,2^2,2^3,2^4,2^5,2^6,2^7,2^8))+
                        #coord_trans(y = "asinh")+
                        #theme_bw()+
                        #theme(legend.position = "top")
                        ggplotly() %>% layout(boxmode = "group", 
                                              legend = l,
                                              yaxis = list(
                                                      title = "Effect compared with control (%)"))
                        
                } else if(input$Levels == "Low" & input$overlay == TRUE){
                        df <- df[df$Levels == "Low",]
                        l <- list(x = 0.02, y = 0.98, bgcolor = "rgba(0,0,0,0)")
                        ggplot(data = df, aes(x=Drug, y=Effectpercent, fill = Drug, text = c(paste0("Research Year: ", ResearchYear,"\n","Research Name: ", ResearchName,"\n","Study ID: ", comparedID)))) + 
                                geom_boxplot(outlier.colour="grey", outlier.shape=16, outlier.size=2) +
                                geom_point(pch = 21, position = position_jitterdodge())+
                                scale_y_continuous(breaks = seq(40, 580, by = 20))+
                                scale_fill_manual(
                                        breaks = c("Rosiglitazone/Avandia", "Troglitazone/Rezulin"),
                                        values=c("green", "red"))+
                                facet_grid(.~ordered(Levels, levels = c("Low", "Medium", "High")))+
                                theme( axis.text.x=element_blank(),axis.title.y=element_blank(),
                                       axis.ticks.x=element_blank()) + 
                                xlab("Levels")
                        
                        #scale_y_continuous(breaks = c(-256,-128,-64,-32,-16,-8,-4,-2,0,2,2^2,2^3,2^4,2^5,2^6,2^7,2^8))+
                        #coord_trans(y = "asinh")+
                        #theme_bw()+
                        #theme(axis.title.x=element_blank())+
                        #theme(legend.position = "top")
                        ggplotly() %>% layout(boxmode = "group", 
                                              legend = l,
                                              yaxis = list(
                                                      title = "Effect compared with control (%)"))
                        
                } else if(input$Levels == "Low" & input$overlay == FALSE){
                        
                        df <- df[df$Levels == "Low",]
                        l <- list(x = 0.02, y = 0.98, bgcolor = "rgba(0,0,0,0)")
                        ggplot(data = df, aes(x=Drug, y=Effectpercent, fill = Drug, text = c(paste0("Research Year: ", ResearchYear,"\n","Research Name: ", ResearchName,"\n","Study ID: ", comparedID)))) + 
                                geom_boxplot(outlier.colour="grey", outlier.shape=16, outlier.size=2) +
                                scale_y_continuous(breaks = seq(40, 580, by = 20))+
                                scale_fill_manual(
                                        breaks = c("Rosiglitazone/Avandia", "Troglitazone/Rezulin"),
                                        values=c("green", "red"))+
                                facet_grid(.~ordered(Levels, levels = c("Low", "Medium", "High")))+
                                theme( axis.text.x=element_blank(),axis.title.y=element_blank(),
                                       axis.ticks.x=element_blank()) + 
                                xlab("Levels")
                        
                        #scale_y_continuous(breaks = c(-128,-64,-32,-16,-8,-4,-2,0,2,2^2,2^3,2^4,2^5,2^6,2^7,2^8))+
                        #coord_trans(y = "asinh")+
                        #theme_bw()+
                        #theme(axis.title.x=element_blank())+
                        #theme(legend.position = "top")
                        ggplotly() %>% layout(boxmode = "group", 
                                              legend = l,
                                              yaxis = list(
                                                      title = "Effect compared with control (%)"))
                        
                } else if(input$Levels == "Medium" & input$overlay == TRUE){
                        df <- df[df$Levels == "Medium",]
                        l <- list(x = 0.02, y = 0.98, bgcolor = "rgba(0,0,0,0)")
                        ggplot(data = df, aes(x=Drug, y=Effectpercent, fill = Drug, text = c(paste0("Research Year: ", ResearchYear,"\n","Research Name: ", ResearchName,"\n","Study ID: ", comparedID)))) + 
                                geom_boxplot(outlier.colour="grey", outlier.shape=16, outlier.size=2) +
                                geom_point(pch = 21, position = position_jitterdodge())+
                                scale_y_continuous(breaks = seq(30, 180, by = 10))+
                                scale_fill_manual(
                                        breaks = c("Rosiglitazone/Avandia", "Troglitazone/Rezulin"),
                                        values=c("green", "red"))+
                                facet_grid(.~ordered(Levels, levels = c("Low", "Medium", "High")))+
                                theme( axis.text.x=element_blank(),axis.title.y=element_blank(),
                                       axis.ticks.x=element_blank()) + 
                                xlab("Levels")
                        
                        #scale_y_continuous(breaks = c(-512,-256,-128,-64,-32,-16,-8,-4,-2,0,2,2^2,2^3,2^4,2^5,2^6))+
                        #coord_trans(y = "asinh")+
                        #theme_bw()+
                        #theme(axis.title.x=element_blank())+
                        #theme(legend.position = "top")
                        ggplotly() %>% layout(boxmode = "group", 
                                              legend = l,
                                              yaxis = list(
                                                      title = "Effect compared with control (%)"))
                        
                } else if(input$Levels == "Medium" & input$overlay == FALSE){
                        
                        df <- df[df$Levels == "Medium",]
                        l <- list(x = 0.02, y = 0.98, bgcolor = "rgba(0,0,0,0)")
                        ggplot(data = df, aes(x=Drug, y=Effectpercent, fill = Drug, text = c(paste0("Research Year: ", ResearchYear,"\n","Research Name: ", ResearchName,"\n","Study ID: ", comparedID)))) + 
                                geom_boxplot(outlier.colour="grey", outlier.shape=16, outlier.size=2) +
                                scale_y_continuous(breaks = seq(30, 180, by = 10)) +
                                scale_fill_manual(
                                        breaks = c("Rosiglitazone/Avandia", "Troglitazone/Rezulin"),
                                        values=c("green", "red"))+
                                facet_grid(.~ordered(Levels, levels = c("Low", "Medium", "High")))+
                                theme( axis.text.x=element_blank(),axis.title.y=element_blank(),
                                       axis.ticks.x=element_blank()) + 
                                xlab("Levels")
                        
                        #scale_y_continuous(breaks = c(-512,-256,-128,-64,-32,-16,-8,-4,-2,0,2,2^2,2^3,2^4,2^5,2^6))+
                        #coord_trans(y = "asinh")+
                        #theme_bw()+
                        #theme(axis.title.x=element_blank())+
                        #theme(legend.position = "top")
                        ggplotly() %>% layout(boxmode = "group", 
                                              legend = l,
                                              yaxis = list(
                                                      title = "Effect compared with control (%)"))
                        
                } else if(input$Levels == "High" & input$overlay == TRUE){
                        df <- df[df$Levels == "High",]
                        l <- list(x = 0.02, y = 0.98, bgcolor = "rgba(0,0,0,0)")
                        ggplot(data = df, aes(x=Drug, y=Effectpercent, fill = Drug, text = c(paste0("Research Year: ", ResearchYear,"\n","Research Name: ", ResearchName,"\n","Study ID: ", comparedID)))) + 
                                geom_boxplot(outlier.colour="grey", outlier.shape=16, outlier.size=2) +
                                geom_point(pch = 21, position = position_jitterdodge())+
                                scale_y_continuous(breaks = seq(80, 130, by = 5)) +
                                scale_fill_manual(
                                        breaks = c("Rosiglitazone/Avandia", "Troglitazone/Rezulin"),
                                        values=c("green", "red"))+
                                facet_grid(.~ordered(Levels, levels = c("Low", "Medium", "High")))+
                                theme( axis.text.x=element_blank(),axis.title.y=element_blank(),
                                       axis.ticks.x=element_blank()) + 
                                xlab("Levels")
                        
                        #scale_y_continuous(breaks = c(-8,-4,-2,0,2,2^2,2^3))+
                        #coord_trans(y = "asinh")+
                        #theme_bw()+
                        #theme(axis.title.x=element_blank())+
                        #theme(legend.position = "top")
                        ggplotly() %>% layout(boxmode = "group", 
                                              legend = l,
                                              yaxis = list(
                                                      title = "Effect compared with control (%)"))
                        
                } else if(input$Levels == "High" & input$overlay == FALSE){
                        df <- df[df$Levels == "High",]
                        l <- list(x = 0.02, y = 0.98, bgcolor = "rgba(0,0,0,0)")
                        ggplot(data = df, aes(x=Drug, y=Effectpercent, fill = Drug, text = c(paste0("Research Year: ", ResearchYear,"\n","Research Name: ", ResearchName,"\n","Study ID: ", comparedID)))) + 
                                geom_boxplot(outlier.colour="grey", outlier.shape=16, outlier.size=2) +
                                scale_y_continuous(breaks = seq(80, 130, by = 5)) +
                                scale_fill_manual(
                                        breaks = c("Rosiglitazone/Avandia", "Troglitazone/Rezulin"),
                                        values=c("green", "red"))+
                                facet_grid(.~ordered(Levels, levels = c("Low", "Medium", "High")))+
                                theme( axis.text.x=element_blank(),axis.title.y=element_blank(),
                                       axis.ticks.x=element_blank()) + 
                                xlab("Levels")
                        
                        #scale_y_continuous(breaks = c(-8,-4,-2,0,2,2^2,2^3))+
                        #coord_trans(y = "asinh")+
                        #theme_bw()+
                        #theme(axis.title.x=element_blank())+
                        #theme(legend.position = "top")
                        ggplotly() %>% layout(boxmode = "group", 
                                              legend = l,
                                              yaxis = list(
                                                      title = "Effect compared with control (%)"))
                        
                } 
        })
        
}


shinyApp(ui, server)





########################
#references:
#split boxes into 2 drugs
#http://t-redactyl.io/blog/2016/04/creating-plots-in-r-using-ggplot2-part-10-boxplots.html
#https://ggplot2.tidyverse.org/reference/position_jitterdodge.html
#http://www.sthda.com/english/wiki/ggplot2-box-plot-quick-start-guide-r-software-and-data-visualization