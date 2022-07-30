#Data compilation

#Final clean paper script

# COMPILE DATA ------------------------------------------------------------

#Load libraries
library(dplyr)
library(tidyverse)

#Load assessment data and clean
Data <- read_csv("Data/Final_assessment_scores_April14_Final.csv")
Data$GSP_name <- as.factor(Data$GSP_name)
Data$Basin_number <- as.factor(Data$Basin_number) #Note that Yuba has two basin numbers which are combined here. 
Data$Number_MTsset_ofseven <- as.integer(Data$Number_MTsset_ofseven)

#Add in Darcy's dry well analysis data for domestic wells
Drywells <- read_csv("Data/gsp_tallies2_GSPsrenamed.csv")
Drywells <- Drywells[-1]
Drywells <- Drywells[-1]
Drywells$GSP_name <- as.factor(Drywells$GSP_name)
Drywells <- Drywells[-3]

#Make NAs zeros - CHECK THIS WITH DARCY IF USING ANY OF THESE VARIABLES!
Drywells[is.na(Drywells)] = 0

#Try making percent of wells dry variable
Drywells$perc_1900 <- (Drywells$CD_PLmt1900) /(Drywells$count1900)
Drywells$perc_1975 <- (Drywells$CD_PLmt1975) /(Drywells$count1975)
Drywells$perc_1990 <- (Drywells$CD_PLmt1990) /(Drywells$count1990)

#Try joining by GSP name see if works
Data <- left_join(Data, Drywells, by = "GSP_name")
Data$GSP_name <- as.factor(Data$GSP_name)

#Add Darcy's public water supply well assessment
Pubwells <- read_csv("Data/publicsupplywells_dry_namesmatched.csv") #Note that in this csv version int he R data folder I added the four GSPs that didn't have any public supply wells wiht all zeros into the spreadsheet to try to avoid confusing them with those that didn't get analyzed which will show up as NAs once I join it with the other data
Pubwells <- Pubwells[-1]
Pubwells$GSP_Name <- as.factor(Pubwells$GSP_Name)

Data <- left_join(Data, Pubwells, by = c("GSP_name"="GSP_Name"))
Data$GSP_name <- as.factor(Data$GSP_name)

#Add in GSP level summarized data on representation, instituitons etc.
GSPdata <- read_csv("Data/VariablessummarizedtoGSPlevel3.csv")
GSPdata$GSP_name <- as.factor(GSPdata$GSP_name)
GSPdata <- GSPdata[-c(2, 6, 7,8)]
GSPdata$COB <- as.factor(GSPdata$COB)
GSPdata$IsthereacollabGSAauthor <- as.factor(GSPdata$IsthereacollabGSAauthor)
GSPdata$Committee_any <- as.factor(GSPdata$Committee_any)
GSPdata$Committee_DW_any <- as.factor(GSPdata$Committee_DW_any)
GSPdata$Committee_DAC_any <- as.factor(GSPdata$Committee_DAC_any)
GSPdata$County <- as.factor(GSPdata$County)

#Join data
Data <- left_join(Data, GSPdata, by = "GSP_name")

#Add in final variables on DACs, acres etc. 
AcresandDACs <- read_csv("Data/GSP_acresandDACs.csv")
AcresandDACs$GSP_name <- as.factor(AcresandDACs$GSP_name)
#join data
Data <- left_join(Data, AcresandDACs, by = "GSP_name")

#Create new variables
#percent GSA authors that are collaborative
Data$per_collab <- ((Data$Count_GSAauthors_collab) / (Data$Number_GSA_authors_unique))*100

#Percent board seats that are DAC reps
Data$board_perc_DAC <- ((Data$Number_boardseats_DAC) / (Data$Number_boardseats))*100

#DACs per board seat NOTE I'm USING 2016 here
Data$DAC_per_DACseat <- ((Data$Number_boardseats_DAC) / (Data$Count_DACwithND_2016))*100

#Number board seats that are DW reps
Data$board_perc_DW <- ((Data$Number_boardseats_DW) / (Data$Number_boardseats))*100 

#PWSs per DW board seat
Data$PWS_per_DWseat <- ((Data$Number_boardseats_DW)/(Data$count_pws))*100 

#percent of board seats held by women
Data$board_per_women <- ((Data$Number_directors_women) / (Data$Number_boardseats))*100 

#percent of board seats held by women
Data$board_perPOC <- ((Data$Number_directors_nonwhite) / (Data$Number_boardseats))*100 

#Percent GSAs with a stakeholder or advisory committee
Data$Committee_perc <- ((Data$Count_GSAswithcommittee) / (Data$Number_GSA_authors_unique))*100 

#Percent GSAs with committee with DW rep. Assumed all could should have DW rep
Data$Committee_DW_perc <- ((Data$Count_committeewith_DW) / (Data$Number_GSA_authors_unique))*100

#Make combined outcome quality variable
Data$Quality_outcomecombined <- ((Data$Number_MTsset_ofseven)*(Data$MCLs_used))
summary(Data$Quality_outcomecombined)

#Make variable for whether any Prop 1 Category 1 funding received
Data$Prop1 <- ifelse(Data$Prop1Cat1_TA == 1 | Data$Prop1Cat1_project == 1, 1, 0)
summary(Data$Prop1)
table(Data$Prop1)
table(Data$Prop1Cat1_TA)
table(Data$Prop1Cat1_project)

#Make a variable for the assessment score (minus engagement) in quartiles
summary(Data$SCORE_minusengagement)
Data$SCORE_factor <- NA
Data$SCORE_factor <- ifelse(Data$SCORE_minusengagement <= 22.222, 0, Data$SCORE_factor)
Data$SCORE_factor <- ifelse(Data$SCORE_minusengagement > 22.222 & Data$SCORE_minusengagement <= 32.500, 1, Data$SCORE_factor)
Data$SCORE_factor <- ifelse(Data$SCORE_minusengagement > 32.500 & Data$SCORE_minusengagement <= 47.500, 2, Data$SCORE_factor)
Data$SCORE_factor <- ifelse(Data$SCORE_minusengagement > 47.500, 3, Data$SCORE_factor)
Data$SCORE_factor <- as.factor(Data$SCORE_factor)
summary(Data$SCORE_factor)

#Make more streamlined factor MT quality thresholds variable
Data$QualityMT <- NA
Data$QualityMT <- ifelse(Data$Number_MTsset_ofseven == 0, 0, Data$QualityMT)
Data$QualityMT <- ifelse(Data$Number_MTsset_ofseven > 0 & Data$MCLs_used == 0, 1, Data$QualityMT)
Data$QualityMT <- ifelse(Data$Number_MTsset_ofseven > 0 & Data$MCLs_used == 1, 2, Data$QualityMT)
Data$QualityMT <- ifelse(Data$Number_MTsset_ofseven > 0 & Data$MCLs_used == 2, 3, Data$QualityMT)
summary(Data$QualityMT)

#Deal with Adding NAs for those wihtout DACs
summary(Data$board_perc_DAC)
Data$board_perc_DAC <- ifelse(Data$Count_DACwithND_2016 == 0, NA, Data$board_perc_DAC)
summary(Data$board_perc_DAC)

summary(Data$DAC_per_DACseat)#already done because 0/0 is NA

summary(Data$Committee_DAC_any)
Data$Committee_DAC_any <- ifelse(Data$Count_DACwithND_2016 == 0, NA, Data$Committee_DAC_any)
summary(Data$Committee_DAC_any) #Changed to numeric for some reason but 1 is no and 2 is yes so fine for now

#Reduce data to something more manageable
Datareduced <- Data %>% select(GSP_name, SCORE_minusengagement_summed, SCORE_minusengagement_summed_minusproj,PROJECTS_SCORE, Projects_DW, SCORE_minusengagement_Averagesubscore, SCORE_factor, QualityMT, perc_1975, percfail, MCLs_used, Number_MTsset_ofseven, percwsf, Outreachandinvolvement, Ceplan, Incorporating_comments, Translation, count1975, count_domwells, count_pswells,  count_hhsd_all, avgpl_1975,Avg_TCD_dom, AllWells, Avg_TCD_ps, count_pws, WaterSys, Count_DACwithND_2016, Count_DACwithND_2018, Number_GSA_authors_unique, Number_GSPs_inbasin, Number_agencies_ingovagreements_total, IsthereacollabGSAauthor, per_collab, board_perc_DAC, DAC_per_DACseat, board_perc_DW, PWS_per_DWseat, board_per_women, board_perPOC, Committee_perc, Committee_any, Committee_DAC_any, Committee_DW_perc, County_ag_profits_2019, Perc_irrigated_Darcy, GSP_area_acres_me, Summed_EJ_Engagement, CWC, SHE, LCJA, Facilitation_orcoordinationsupport_recieved, Facilitation_received_dierct, Count_letters, Quality_outcomecombined, Prop1Cat1_project, Prop1Cat1_TA, Prop1, Combined_risk_percentil, COB)

#Fix last types
Datareduced$IsthereacollabGSAauthor <- as.numeric(Datareduced$IsthereacollabGSAauthor)
Datareduced$Committee_any <- as.numeric(Datareduced$Committee_any)
Datareduced$COB <- as.numeric(Datareduced$COB)

#Save final data to outputs
write_csv(Datareduced, "Outputs/Datareduced.csv")
