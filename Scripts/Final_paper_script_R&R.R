#Paper script (descriptive and models/figures combined)
library(tidyverse)

#Load data
Datareduced <- read_csv("Outputs/Datareduced.csv")

# Descriptives and correlation plot  ---------------------------------------

#Descriptives of DVs
summary(Datareduced$QualityMT)
sd(Datareduced$QualityMT)
table(Datareduced$QualityMT)#53% set no MTs, just 4 plans set MTs above MCLs in all cases
summary(Datareduced$SCORE_minusengagement_summed_minusproj)#mean score is 41.80, median is 38.46, high 76.92 and low 5
sd(Datareduced$SCORE_minusengagement_summed_minusproj)
summary(Datareduced$Projects_DW)
table(Datareduced$Projects_DW)

#Descriptives of IVs
summary(Datareduced$Number_GSPs_inbasin)
sd(Datareduced$Number_GSPs_inbasin)
summary(Datareduced$Committee_perc)

#Make county agricultural profits in billions
Datareduced$County_ag_profits_2019_inbillions <- (Datareduced$County_ag_profits_2019/1000000000)
summary(Datareduced$County_ag_profits_2019_inbillions)

#Make a dichotomous QualityMT variable
Datareduced$QualityMT2 <- ifelse(Datareduced$QualityMT >= 2, 1, 0)

#Make summed EJ involvement variable into a factor
Datareduced$Summed_EJ_Engagement_factor <- ifelse(Datareduced$Summed_EJ_Engagement == 0, 0, "placeholder")
Datareduced$Summed_EJ_Engagement_factor <- ifelse(Datareduced$Summed_EJ_Engagement > 0, 1, Datareduced$Summed_EJ_Engagement_factor)
Datareduced$Summed_EJ_Engagement_factor <- ifelse(Datareduced$Summed_EJ_Engagement > 2,2, Datareduced$Summed_EJ_Engagement_factor)
table(Datareduced$Summed_EJ_Engagement_factor)

#Make factor variables numeric just for corplott
Datareduced$Summed_EJ_Engagement_factor <- as.numeric(Datareduced$Summed_EJ_Engagement_factor)
Datareduced$Outreachandinvolvement <- as.numeric(Datareduced$Outreachandinvolvement)
Datareduced$Incorporating_comments <- as.numeric(Datareduced$Incorporating_comments)

#Corr plot final
library(corrplot)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

#Data with all three DVs for correlation plot
DataBoth <- Datareduced %>% select(SCORE_minusengagement_summed_minusproj, QualityMT2, Projects_DW, count_hhsd_all, Outreachandinvolvement, Incorporating_comments, Combined_risk_percentil, Count_DACwithND_2016, Number_GSPs_inbasin, board_perc_DW, board_perPOC, Committee_perc, County_ag_profits_2019_inbillions, Perc_irrigated_Darcy, Summed_EJ_Engagement_factor, Number_agencies_ingovagreements_total)
DataBoth_Complete <- na.omit(DataBoth)

# matrix of the p-value of the correlations
MBoth <- cor(DataBoth_Complete)
p.matBoth <- cor.mtest(DataBoth_Complete)
head(p.matBoth[, 1:5])

#NAMES: SCORE_minusengagement_summed_minusproj, QualityMT, Projects_DW, count_hhsd_all, Outreachandinvolvement, Incorporating_comments, Combined_risk_percentil, Count_DACwithND_2016, Number_GSPs_inbasin, board_perc_DW, board_perPOC, Committee_perc, County_ag_profits_2019, Perc_irrigated_Darcy, Summed_EJ_Engagement, Number_agencies_ingovagreements_total
colnames(MBoth) <- c("EJ rubric", "quality criteria", "projects", "well outages", "outreach", "comments", "quality risk", "# DACs", "# GSPs", "% board DW", "% board POC", "committee", "ag profits", "% irrigated", "EJ involvement", "# agencies")
rownames(MBoth) <- c("EJ rubric", "quality criteria", "projects", "well outages", "outreach", "comments", "quality risk", "# DACs", "# GSPs", "% board DW", "% board POC", "committee", "ag profits", "% irrigated", "EJ involvement", "# agencies")
colnames(p.matBoth) <- c("EJ rubric", "quality criteria", "projects", "well outages", "outreach", "comments", "quality risk", "# DACs", "# GSPs", "% board DW", "% board POC", "committee", "ag profits", "% irrigated", "EJ involvement", "# agencies")
#c("Environmental justice rubric", "alignment of water quality criteria", "inclusion of drinking water projects", "hosuehold well outages", "degree of stakeholder engagement and outreach", "evidence of incorproating comments", "aquifer quality risk percentile ", "Number DACs", "Number GSPs", "Percent of board representing drinking water", "percent of board people of color", "Use of stakeholder committee", "Annual agricultural production", "percent plan area irrigated agriculture", "Environmental justice organization involvement", "Number of agencies")
rownames(p.matBoth) <- c("EJ rubric", "quality criteria", "projects", "well outages", "outreach", "comments", "quality risk", "# DACs", "# GSPs", "% board DW", "% board POC", "committee", "ag profits", "% irrigated", "EJ involvement", "# agencies")

corrplot(MBoth, method="color", col=col(200),  
         type="upper", order="hclust", 
         tl.col="black", tl.srt=45, 
         p.mat = p.matBoth, sig.level = 0.1, insig= "blank",
         addCoef.col = "black",
         diag=FALSE)$corrPos -> p1
text(p1$x, p1$y, round(p1$corr, 2))

# FINAL MODELS ------------------------------------------------------------
#load additional libraries
library(caret)
library(gbm)

#Make some independent variable categorical again
Datareduced$Incorporating_comments <- as.ordered(Datareduced$Incorporating_comments)
Datareduced$Outreachandinvolvement <- as.ordered(Datareduced$Outreachandinvolvement)
Datareduced$Summed_EJ_Engagement_factor <- as.ordered(Datareduced$Summed_EJ_Engagement_factor)

### ASSESSMENT SCORE MODEL
#Make data
DataScore <- Datareduced %>% select(SCORE_minusengagement_summed_minusproj, count_hhsd_all, Outreachandinvolvement, Incorporating_comments, Combined_risk_percentil, Count_DACwithND_2016, Number_GSPs_inbasin, board_perc_DW, board_perPOC, Committee_perc, County_ag_profits_2019_inbillions, Perc_irrigated_Darcy, Summed_EJ_Engagement_factor, Number_agencies_ingovagreements_total)
DataScore_Complete <- na.omit(DataScore)

#run model
model<-SCORE_minusengagement_summed_minusproj ~.
metric<-"RMSE"
fitControl<-trainControl(method="repeatedcv", number =10, repeats = 5)
grid<-expand.grid(interaction.depth=c(2:4), n.trees=seq(800, 5000, 100), shrinkage =c(0.001, 0.0025, 0.01, 0.025, 0.05), n.minobsinnode=c(3:6))  #

set.seed(1990)
tuneScore<-train(model, data=DataScore_Complete, method="gbm", trControl=fitControl, tuneGrid=grid, metric=metric, bag.fraction=0.5)
tuneScore
max(tuneScore$results["RMSE"]) #RMSE 13.7793. The final values used for the model were n.trees = 2700, interaction.depth = 4, shrinkage = 0.0025 and n.minobsinnode = 6. 

model<-SCORE_minusengagement_summed_minusproj ~.
set.seed(1990)
gbmScore <- gbm(formula =model, data = DataScore, n.trees = 2700, interaction.depth = 4, shrinkage = 0.0025, bag.fraction=0.5, n.minobsinnode=6)
gbmScore

importanceScore <- tibble::as_tibble(gbm::summary.gbm(gbmScore, plotit = FALSE))
importanceScore

#add categories and aggregate by group
importanceScore$Category <- c("Problem severity", "Elite capture", "Problem severity", "Representation", "Collaboration", "Stakeholder engagement", "Problem severity", "Representation", "Elite capture", "Stakeholder engagement", "Collaboration", "Stakeholder engagement", "Representation")
importanceScore$Category <- as.factor(importanceScore$Category)

aggregate(importanceScore$rel.inf, by=list(Category=importanceScore$Category), FUN=sum)

#Null model
Null_Score <- nullModel(y = DataScore$SCORE_minusengagement_summed_minusproj)
Null_score_vector <- as.vector(predict(Null_Score))
postResample(pred =  Null_score_vector, obs = DataScore$SCORE_minusengagement_summed_minusproj) #Baseline RMSE 16.96961


###MT MODEL
#Make data
DataFactor <- Datareduced %>% select(QualityMT2, count_hhsd_all, Outreachandinvolvement, Incorporating_comments, Combined_risk_percentil, Count_DACwithND_2016, Number_GSPs_inbasin, board_perc_DW, board_perPOC, Committee_perc, County_ag_profits_2019_inbillions, Perc_irrigated_Darcy, Summed_EJ_Engagement_factor, Number_agencies_ingovagreements_total)
DataFactor_Complete <- na.omit(DataFactor)
DataFactor_Complete$QualityMT2 <- as.factor(DataFactor_Complete$QualityMT2)

#run model
model<-QualityMT2~.
metric<-"Accuracy"
fitControl<-trainControl(method="repeatedcv", number =10, repeats = 5)
grid<-expand.grid(interaction.depth=c(2:4), n.trees=seq(800, 5000, 100), shrinkage =c(0.005, 0.0025, 0.01, 0.025, 0.05), n.minobsinnode=c(3:6)) 

set.seed(1990)
tuneMT<-train(model, data=DataFactor_Complete, method="gbm", trControl=fitControl, tuneGrid=grid, metric=metric, bag.fraction=0.6)
tuneMT
max(tuneMT$results["Accuracy"]) # acuracy 0.82. The final values used for the model were n.trees = 1600, interaction.depth = 3, shrinkage = 0.05 and n.minobsinnode = 4.

model<-QualityMT2~.
set.seed(1990)
gbmMT<- gbm(formula =model, data = DataFactor, n.trees = 1600, interaction.depth = 3, shrinkage = 0.05, bag.fraction=0.6, n.minobsinnode=4)
gbmMT

importanceMT <- tibble::as_tibble(gbm::summary.gbm(gbmMT, plotit = FALSE))
importanceMT

#add categories and aggregate by group
importanceMT$Category <- c("Problem severity", "Problem severity", "Representation", "Representation", "Elite capture", "Collaboration", "Stakeholder engagement", "Collaboration", "Representation", "Elite capture", "Problem severity", "Stakeholder engagement", "Stakeholder engagement")
importanceMT$Category <- as.factor(importanceMT$Category)

aggregate(importanceMT$rel.inf, by=list(Category=importanceMT$Category), FUN=sum)

#Baseline accuracy is 30/45 or 66.6%

###PROJECT MODEL
#Make data
DataProj <- Datareduced %>% select(Projects_DW, count_hhsd_all, Outreachandinvolvement, Incorporating_comments, Combined_risk_percentil, Count_DACwithND_2016, Number_GSPs_inbasin, board_perc_DW, board_perPOC, Committee_perc, County_ag_profits_2019_inbillions, Perc_irrigated_Darcy, Summed_EJ_Engagement_factor, Number_agencies_ingovagreements_total)
DataProj$Projects_DW <- ifelse(DataProj$Projects_DW== 2, 1, 0)
DataProj_Complete <- na.omit(DataProj)
DataProj_Complete$Projects_DW <- as.factor(DataProj_Complete$Projects_DW)

#run model
model<-Projects_DW~.
metric<-"Accuracy"
fitControl<-trainControl(method="repeatedcv", number =10, repeats = 5)
grid<-expand.grid(interaction.depth=c(2:4), n.trees=seq(800, 5000, 100), shrinkage =c(0.005, 0.0025, 0.01, 0.025, 0.05), n.minobsinnode=c(3:6))

set.seed(1990)
tunePROJ<-train(model, data=DataProj_Complete, method="gbm", trControl=fitControl, tuneGrid=grid, metric=metric, bag.fraction=0.60)
tunePROJ
max(tunePROJ$results["Accuracy"]) #accuracy 0.7583333 The final values used for the model were n.trees = 3500, interaction.depth = 4, shrinkage = 0.025 and n.minobsinnode = 3.

model<-Projects_DW~.
set.seed(1990)
gbmPROJ<- gbm(formula =model, data = DataProj, distribution = "bernoulli", n.trees = 3500, interaction.depth = 4, shrinkage = 0.025, bag.fraction=0.6, n.minobsinnode=3)
gbmPROJ

importancePROJ <- tibble::as_tibble(gbm::summary.gbm(gbmPROJ, plotit = FALSE))
importancePROJ

#add categories and aggregate by group
importancePROJ$Category <- c("Representation", "Problem severity", "Elite capture",  "Collaboration", "Problem severity", "Representation", "Problem severity", "Representation", "Stakeholder engagement", "Elite capture", "Stakeholder engagement", "Collaboration", "Stakeholder engagement")
importancePROJ$Category <- as.factor(importancePROJ$Category)

aggregate(importancePROJ$rel.inf, by=list(Category=importancePROJ$Category), FUN=sum)

#Baseline accuracy is 30/45 or 66.6%

#See final model tuning to accompany paper script for model tuning efforts

# PLOTS -------------------------------------------------------------------


# Figure 1 ----------------------------------------------------------------


#Relative influence by variable
impplotScore<- ggplot(data=importanceScore, aes(x = reorder(var, rel.inf), y = rel.inf, fill = Category)) + geom_col()+  coord_flip() + theme(plot.margin = margin(t =.5, r=.5, b=.5, l=.5, unit = "cm")) + labs( x = "", y = "Relative Influence (%)", colour = "Category") + scale_fill_manual(values = c("#00798c", "#d1495b", "#edae49", "#66a182", "#2e4057", "#8d96a3")) +scale_x_discrete(labels=c("Percent of board representing people of color (+)", "Degree of outreach and stakeholder engagement (+)", "Number of agencies (+/-)", "Environmental justice organization involvement (+)", "Annual agricultural production value (in billions) (+)","Percent of GSAs with stakeholder committee (+)","Number of DACs in plan area (+)", "Eveidence of incorporating comments (+)", "Nubmer of GSPs (-)", "Percent of board representing drinking water (+)", "Aquifer quality risk percentile (+)", "Percent of plan area irrigated agriculture (-)", "Reported household well outages (+)")) 
impplotScore 

pdf("Outputs/impplotScore.pdf", width = 3, height = 4)
impplotScore
dev.off()

impplotMT<- ggplot(data=importanceMT, aes(x = reorder(var, rel.inf), y = rel.inf, fill = Category)) + geom_col()+  coord_flip() + theme(plot.margin = margin(t =.5, r=.5, b=.5, l=.5, unit = "cm")) + labs( x = "", y = "Relative Influence (%)", colour = "Category") + scale_fill_manual(values = c("#00798c", "#d1495b", "#edae49", "#66a182", "#2e4057", "#8d96a3")) +scale_x_discrete(labels=c("Evidence of incorproating comments (+)", "Environmental justice organization involvement (-)", "Number of DACs in plan area (-)", "Annual agricultural production value (in billions) (-)", "Percent of board representing drinking water (+)", "Number of agencies (+)", "Degree of outreach and staekholder engagement (+)", "Number of GSPs (+)", "Percent of plan area irrigated agriculture (+)", "Percent of board members people of color (+)", "Percent of GSAs with stakeholder committee (+)", "Reported household well outages (+)", "Aquifer quality risk percentile (+)"))
impplotMT 

pdf("Outputs/impplotMT.pdf", width = 3, height = 4)
impplotMT
dev.off()

impplotPROJ<- ggplot(data=importancePROJ, aes(x = reorder(var, rel.inf), y = rel.inf, fill = Category)) + geom_col()+  coord_flip() + theme(plot.margin = margin(t =.5, r=.5, b=.5, l=.5, unit = "cm")) + labs( x = "", y = "Relative Influence (%)", colour = "Category") + scale_fill_manual(values = c("#00798c", "#d1495b", "#edae49", "#66a182", "#2e4057", "#8d96a3")) +scale_x_discrete(labels=c("Degree of outreach and stakeholder engagement (+)", "Number of GSPs (-)", "Environmental justice organization involvement (+)", "Annual agricultural production value (in billions) (+)", "Evidence of incorporating comments (+)",  "Percent of GSAs with stakeholder committee (+)", "Aquifer quality risk percentile (+)", "Percent of board members people of color (+)", "Reported household well outages (+/-)", "Number of agencies (+)", "Percent of plan area irrigated agriculture (-)", "Number of DACs in plan area (+)",  "Percent of board representing drinking water (+)"))

impplotPROJ ##

pdf("Outputs/impplotPROJ.pdf", width = 3, height = 4)
impplotPROJ
dev.off()

#combine into one figure 
library(ggpubr)

#all
Figure1 <- ggarrange(impplotScore, impplotMT, impplotPROJ, labels = c("A", "B", "C"), font.label = list(size=12), ncol = 1, nrow = 3, vjust = 0, hjust = c(-0.1, -0.05, -0.1), align = "hv", widths = c(1,1), heights = c(1,1), common.legend = TRUE, legend = "right") + theme(plot.margin = margin(1,0.2,0.2,0.2, "cm")); Figure1

pdf("Outputs/Figure1.pdf", width = 7, height = 9)
Figure1
dev.off()

# Partial dependence plots ------------------------------------------------
library(pdp)

#Score
p1<-partial(gbmScore, pred.var="count_hhsd_all", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=2700); p1

p2<-partial(gbmScore, pred.var="Perc_irrigated_Darcy", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=2700); p2

p3<-partial(gbmScore, pred.var="Combined_risk_percentil", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=2700); p3

p4<-partial(gbmScore, pred.var="board_perc_DW", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=2700); p4

p5<-partial(gbmScore, pred.var="Number_GSPs_inbasin", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=2700); p5

p6<-partial(gbmScore, pred.var="Count_DACwithND_2016", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=2700); p6

p7<-partial(gbmScore, pred.var="Incorporating_comments", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=2700); p7

p8<-partial(gbmScore, pred.var="Summed_EJ_Engagement_factor", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=2700); p8

p9<-partial(gbmScore, pred.var="Committee_perc", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=2700); p9

p10<-partial(gbmScore, pred.var="County_ag_profits_2019_inbillions", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=2700); p10

p11<-partial(gbmScore, pred.var="Outreachandinvolvement", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=2700); p11

p12<-partial(gbmScore, pred.var="Number_agencies_ingovagreements_total", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=2700); p12

p13<-partial(gbmScore, pred.var="board_perPOC", plot=FALSE, prob=FALSE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=2700); p13

#MT

q1<-partial(gbmMT, pred.var="count_hhsd_all", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=1600); q1

q2<-partial(gbmMT, pred.var="Perc_irrigated_Darcy", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=1600); q2

q3<-partial(gbmMT, pred.var="Combined_risk_percentil", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=1600); q3

q4<-partial(gbmMT, pred.var="board_perc_DW", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=1600); q4

q5<-partial(gbmMT, pred.var="Number_GSPs_inbasin", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=1600); q5

q6<-partial(gbmMT, pred.var="Count_DACwithND_2016", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=1600); q6

q7<-partial(gbmMT, pred.var="Incorporating_comments", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=1600); q7

q8<-partial(gbmMT, pred.var="Summed_EJ_Engagement_factor", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=1600); q8

q9<-partial(gbmMT, pred.var="Committee_perc", plot=FALSE, prob=TRUE, plot.engine="ggplot2", n.trees=1600); q9

q10<-partial(gbmMT, pred.var="County_ag_profits_2019_inbillions", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=1600); q10

q11<-partial(gbmMT, pred.var="Outreachandinvolvement", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=1600); q11

q12<-partial(gbmMT, pred.var="Number_agencies_ingovagreements_total", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=1600); q12

q13<-partial(gbmMT, pred.var="board_perPOC", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=1600); q13

#PROJ

r1<-partial(gbmPROJ, pred.var="count_hhsd_all", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=3500); r1

r2<-partial(gbmPROJ, pred.var="Perc_irrigated_Darcy", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=3500); r2

r3<-partial(gbmPROJ, pred.var="Combined_risk_percentil", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=3500); r3

r4<-partial(gbmPROJ, pred.var="board_perc_DW", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=3500); r4

r5<-partial(gbmPROJ, pred.var="Number_GSPs_inbasin", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=3500); r5

r6<-partial(gbmPROJ, pred.var="Count_DACwithND_2016", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=3500); r6

r7<-partial(gbmPROJ, pred.var="Incorporating_comments", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=3500); r7

r8<-partial(gbmPROJ, pred.var="Summed_EJ_Engagement_factor", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=3500); r8

r9<-partial(gbmPROJ, pred.var="Committee_perc", plot=FALSE, prob=TRUE, plot.engine="ggplot2", n.trees=3500); r9

r10<-partial(gbmPROJ, pred.var="County_ag_profits_2019_inbillions", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=3500); r10

r11<-partial(gbmPROJ, pred.var="Outreachandinvolvement", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=3500); r11

r12<-partial(gbmPROJ, pred.var="Number_agencies_ingovagreements_total", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=FALSE, n.trees=3500); r12

r13<-partial(gbmPROJ, pred.var="board_perPOC", plot=FALSE, prob=TRUE, plot.engine="ggplot2", trim.outliers=TRUE, n.trees=3500); r13

##Plot top four of each model for paper

#SCORE
P1 <- as.data.frame(p1)
Score1 <- ggplot(data=P1, aes(x= count_hhsd_all, y = yhat)) + geom_line() + xlab("Reported household well outages") + ylab("") + ylim(35,53) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size = 10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm"), plot.title = element_text(size = 14, hjust = 0.5, vjust = 2)); Score1

P2 <- as.data.frame(p2)
Score2 <- ggplot(data=P2, aes(x= Perc_irrigated_Darcy, y = yhat)) + geom_line() + xlab("Percent of plan area irrigated agriculture") + ylab("") + ylim(35,53) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size = 10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm")); Score2

P3 <- as.data.frame(p3)
Score3 <- ggplot(data=P3, aes(x= Combined_risk_percentil, y = yhat)) + geom_line() + xlab("Aquifer quality risk percentile") + ylab("") + ylim(35,53) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size = 10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm")); Score3

P4 <- as.data.frame(p4)
Score4 <- ggplot(data=P4, aes(x= board_perc_DW, y = yhat)) + geom_line() + xlab("Percent of board representing drinking water") + ylab("") + ylim(35,53) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size = 10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm")); Score4

#MT
library(stringr)
Q3 <- as.data.frame(q3)
MT1<- ggplot(data=Q3, aes(x= Combined_risk_percentil, y = yhat)) + geom_line() + xlab("Aquifer quality risk percentile") + ylab("") + ylim(0,1) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size = 10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm"), plot.title = element_text(size = 14, hjust = 0.5, vjust = 2)); MT1

Q1 <- as.data.frame(q1)
MT2 <- ggplot(data=Q1, aes(x= count_hhsd_all, y = yhat)) + geom_line() + xlab("Reported household well outages") + ylab("") + ylim(0,1) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size = 10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm")); MT2

Q9 <- as.data.frame(q9)
MT3<- ggplot(data=Q9, aes(x= Committee_perc, y = yhat)) + geom_line() + xlab("Percent of GSAs with stakeholder committee") + ylab("") + ylim(0,1) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size = 10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm")); MT3

Q13 <- as.data.frame(q13)
MT4 <- ggplot(data=Q13, aes(x= board_perPOC, y = yhat)) + geom_line() + xlab("Percent of board members people of color") + ylab("") + ylim(0,1) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size = 10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm")); MT4

#PROJ
R4 <- as.data.frame(r4)
PROJ1 <- ggplot(data=R4, aes(x= board_perc_DW, y = yhat)) + geom_line() + xlab("Percent of board representing drinking water") + ylab("") + ylim(0,1) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size = 10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm")); PROJ1

R6 <- as.data.frame(r6)
PROJ2 <- ggplot(data=R6, aes(x= Count_DACwithND_2016, y = yhat)) + geom_line() + xlab("Number of DACs in plan area") + ylab("") + ylim(0,1) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size = 10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm")); PROJ2

R2 <- as.data.frame(r2)
PROJ3 <- ggplot(data=R2, aes(x= Perc_irrigated_Darcy, y = yhat)) + geom_line() + xlab("Percent of plan area irrigated agriculture") + ylab("") + ylim(0,1) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size = 10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm")); PROJ3

R12 <- as.data.frame(r12)
PROJ4 <- ggplot(data=R12, aes(x= Number_agencies_ingovagreements_total, y = yhat)) + geom_line() + xlab("Number of agencies") + ylab("") + ylim(0,1) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size = 10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm")); PROJ4

#Combine into one figure

#OLDFigure2 <- ggarrange(Score1, MT1, PROJ1, Score2, MT2, PROJ2, Score3, MT3, PROJ3, Score4, MT4, PROJ4, ncol = 3, nrow = 4, labels = c("Environmental justice rubric score", "Correspondence of water quality criteria \n with drinking water standards", "Inclusion of drinking water \n related projects"), align = "hv", vjust = c(-1.5, -0.75, -1.5), hjust = c(-0.3,-0.2,-.2), font.label = list(size =11, face = "bold", color = "black")) + theme(plot.margin = margin(2,0.1,0.1,0.1, "cm"))
#Can't for the life of me get the labels in the right place so just do them in word with arial/helvetica bold font

Figure2 <- ggarrange(Score1, MT1, PROJ1, Score2, MT2, PROJ2, Score3, MT3, PROJ3, Score4, MT4, PROJ4, ncol = 3, nrow = 4) + theme(plot.margin = margin(2,0.1,0.1,0.1, "cm"))

pdf("Outputs/Figure2.pdf", width = 10, height = 9)
Figure2
dev.off()

##Make figures for appendix of other pd plots

#score model
P5 <- as.data.frame(p5)
Score5 <- ggplot(data=P5, aes(x= Number_GSPs_inbasin, y = yhat)) + geom_line() + xlab("Number of GSPs") + ylab("") + ylim(35,53) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size = 10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm")); Score5

P6 <- as.data.frame(p7)
Score6 <- ggplot(P6, aes(x=Incorporating_comments, y=yhat)) + geom_bar(stat="identity", fill="white", color = "black") + xlab("Evidence of incorporating comments")+ ylab("") + ylim(0,53) + theme(legend.position="none") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.text = element_text(size=10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm"))+theme(axis.title=element_text(size=10))+ scale_x_discrete(labels=c("0"="None", "1"= "Some", "2" ="Yes")); Score6

P7 <- as.data.frame(p6)
Score7 <- ggplot(data=P7, aes(x= Count_DACwithND_2016, y = yhat)) + geom_line() + xlab("Numbers of DACs in plan area") + ylab("") + ylim(35,53) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size = 10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm")); Score7

P8 <- as.data.frame(p9)
Score8 <- ggplot(data=P8, aes(x= Committee_perc, y = yhat)) + geom_line() + xlab("Percent of GSAs with stakeholder committee") + ylab("") + ylim(35,53) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size = 10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm")); Score8

P9 <- as.data.frame(p10)
Score9 <- ggplot(data=P9, aes(x= County_ag_profits_2019_inbillions, y = yhat)) + geom_line() + xlab("Annual value of agricultural production (billions)") + ylab("") + ylim(35,53) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size = 10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm")); Score9

P10 <- as.data.frame(p8)
Score10 <- ggplot(data=P10, aes(x= Summed_EJ_Engagement_factor, y = yhat)) + geom_bar(stat="identity", fill="white", color = "black") + xlab("Environmental justice organization involvement")+ ylab("")+ylim(0, 53) + theme(legend.position="none") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.text = element_text(size=10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm")) +theme(axis.title=element_text(size=10)) + scale_x_discrete(labels=c("0"="Minimal", "1"= "Some", "2" ="More")); Score10

P11 <- as.data.frame(p12)
Score11 <- ggplot(data=P11, aes(x= Number_agencies_ingovagreements_total, y = yhat)) + geom_line() + xlab("Number of agencies") + ylab("") + ylim(35,53) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size = 10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm")); Score11

P12 <- as.data.frame(p11)
Score12 <- ggplot(P12, aes(x=Outreachandinvolvement, y=yhat)) + geom_bar(stat="identity", fill="white", color = "black") + xlab("Degree of outreach and stakeholder involvement")+ ylab("")+ylim(0, 53) + theme(legend.position="none") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),  axis.text = element_text(size=10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm")) +theme(axis.title=element_text(size=10)) + scale_x_discrete(labels=c("0"="Minimal", "1"= "Some", "2" ="More")); Score12

P13 <- as.data.frame(p13)
Score13 <- ggplot(data=P13, aes(x= board_perPOC, y = yhat)) + geom_line() + xlab("Percent of board members who are people of color") + ylab("") + ylim(35,53) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size = 10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm")); Score13

#combine into one 
FigureGa <- ggarrange(Score5, Score6, Score7, Score8, Score9, Score10, Score11, Score12, Score13, ncol = 2, nrow = 5, align = "hv", widths = c(1,1), common.legend = FALSE); FigureGa

pdf("Outputs/FigureGa.pdf", width = 10, height = 9)
FigureGa
dev.off()

#MT model
Q2 <- as.data.frame(q2)
MT5 <- ggplot(data=Q2, aes(x= Perc_irrigated_Darcy, y = yhat)) + geom_line() + xlab("Percent of plan area irrigated agriculture") + ylab("") + ylim(0,1) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size = 10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm")); MT5

Q5 <- as.data.frame(q5)
MT6 <- ggplot(data=Q5, aes(x= Number_GSPs_inbasin, y = yhat)) + geom_line() + xlab("Number GSPs") + ylab("") + ylim(0,1) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size = 10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm")); MT6

Q11 <- as.data.frame(q11)
MT7 <- ggplot(Q11, aes(x=Outreachandinvolvement, y=yhat)) + geom_bar(stat="identity", fill="white", color = "black") + xlab("Degree of outreach and stakeholder engagement")+ ylab("") + ylim(0, 1) + theme(legend.position="none") +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.text = element_text(size=10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm")) +theme(axis.title=element_text(size=10)) +  scale_x_discrete(labels=c("0"="Min", "1"= "Some", "2" ="More")); MT7

Q12 <- as.data.frame(q12)
MT8 <- ggplot(data=Q12, aes(x= Number_agencies_ingovagreements_total, y = yhat)) + geom_line() + xlab("Number of agencies") + ylab("") + ylim(0,1) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size = 10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm")); MT8

Q4 <- as.data.frame(q4)
MT9 <- ggplot(data=Q4, aes(x= board_perc_DW, y = yhat)) + geom_line() + xlab("Percent of board representing drinking water") + ylab("") + ylim(0,1) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size = 10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm")); MT9

Q10 <- as.data.frame(q10)
MT10 <- ggplot(data=Q10, aes(x= County_ag_profits_2019_inbillions, y = yhat)) + geom_line() + xlab("Annual value of agricultural production (billions)") + ylab("") + ylim(0,1) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size = 10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm")); MT10

Q6 <- as.data.frame(q6)
MT11 <- ggplot(data=Q6, aes(x= Count_DACwithND_2016, y = yhat)) + geom_line() + xlab("Number of DACs in plan area") + ylab("") + ylim(0,1) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size = 10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm")); MT11

Q8 <- as.data.frame(q8)
MT12 <- ggplot(data=Q8, aes(x= Summed_EJ_Engagement_factor, y = yhat)) + geom_bar(stat="identity", fill="white", color = "black") + xlab("Environmental justice organization involvement")+ ylab("")+ylim(0,1) + theme(legend.position="none") +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.text = element_text(size=10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm")) +theme(axis.title=element_text(size=10)) + scale_x_discrete(labels=c("0"="None", "1"= "Some", "2" ="Yes")); MT12

Q7 <- as.data.frame(q7)
MT13 <- ggplot(Q7, aes(x=Incorporating_comments, y=yhat)) + geom_bar(stat="identity", fill="white", color = "black") + xlab("Evidence of incorporating comments")+ ylab("")+ylim(0,1) + theme(legend.position="none") +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.text = element_text(size=10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm")) +theme(axis.title=element_text(size=10)) + scale_x_discrete(labels=c("0"="None", "1"= "Some", "2" ="Yes")); MT13

#combine into one 
FigureGb <- ggarrange(MT5, MT6, MT7, MT8, MT9, MT10, MT11, MT12, MT13, ncol = 2, nrow = 5, align = "hv", widths = c(1,1), common.legend = FALSE); FigureGb

pdf("Outputs/FigureGb.pdf", width = 10, height = 9)
FigureGb
dev.off()

#PROJ model
R1 <- as.data.frame(r1)
PROJ5 <- ggplot(data=R1, aes(x= count_hhsd_all, y = yhat)) + geom_line() + xlab("Reported household well outages") + ylab("") + ylim(0,1) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size = 10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm")); PROJ5

R13 <- as.data.frame(r13)
PROJ6 <- ggplot(data=R13, aes(x= board_perPOC, y = yhat)) + geom_line() + xlab("Percent of board members that are people of color") + ylab("") + ylim(0,1) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size = 10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm")); PROJ6

R3 <- as.data.frame(r3)
PROJ7 <- ggplot(data=R3, aes(x= Combined_risk_percentil, y = yhat)) + geom_line() + xlab("Aquifer risk percentile") + ylab("") + ylim(0,1) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size = 10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm")); PROJ7

R9 <- as.data.frame(r9)
PROJ8 <- ggplot(data=R9, aes(x= Committee_perc, y = yhat)) + geom_line() + xlab("Percent of GSAs with stakeholder committee") + ylab("") + ylim(0,1) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size = 10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm")); PROJ8

R7 <- as.data.frame(r7)
PROJ9 <- ggplot(R7, aes(x=Incorporating_comments, y=yhat)) + geom_bar(stat="identity", fill="white", color = "black") + xlab("Evidence of incorporating comments")+ ylab("") + ylim(0, 1) + theme(legend.position="none") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.text = element_text(size=10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm")) +theme(axis.title=element_text(size=10)) + scale_x_discrete(labels=c("0"="None", "1"= "Some", "2" ="Yes")); PROJ9

R10 <- as.data.frame(r10)
PROJ10 <- ggplot(data=R10, aes(x= County_ag_profits_2019_inbillions, y = yhat)) + geom_line() + xlab("Annual value of agricultural production (billions)") + ylab("") + ylim(0,1) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size = 10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm")); PROJ10

R8 <- as.data.frame(r8)
PROJ11 <- ggplot(data=R8, aes(x= Summed_EJ_Engagement_factor, y = yhat)) + geom_bar(stat="identity", fill="white", color = "black") + xlab("Environmental justice organization involvement")+ ylab("") + ylim(0, 1) + theme(legend.position="none") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.text = element_text(size=10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm")) +theme(axis.title=element_text(size=10)) + scale_x_discrete(labels=c("0"="None", "1"= "Some", "2" ="Yes")); PROJ11

R5 <- as.data.frame(r5)
PROJ12 <- ggplot(data=R5, aes(x= Number_GSPs_inbasin, y = yhat)) + geom_line() + xlab("Number of GSPs") + ylab("") + ylim(0,1) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title = element_text(size = 10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm")); PROJ12

R11 <- as.data.frame(r11)
PROJ13 <- ggplot(R11, aes(x=Outreachandinvolvement, y=yhat)) + geom_bar(stat="identity", fill="white", color = "black") + xlab("Degree of outreach and stakeholder engagement")+ ylab("")+ylim(0,1) + theme(legend.position="none") +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.text = element_text(size=10), plot.margin = margin(t =.5, r=.5, b=.5, l=0, unit = "cm")) +theme(axis.title=element_text(size=10)) + scale_x_discrete(labels=c("0"="Min", "1"= "Some", "2" ="More")); PROJ13

#combine into one 
FigureGc <- ggarrange(PROJ5, PROJ6, PROJ7, PROJ8, PROJ9, PROJ10, PROJ11, PROJ12, PROJ13, ncol = 2, nrow = 5, align = "hv", widths = c(1,1), common.legend = FALSE); FigureGc

pdf("Outputs/FigureGc.pdf", width = 10, height = 9)
FigureGc
dev.off()