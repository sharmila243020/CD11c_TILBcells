args = commandArgs(trailingOnly=TRUE)
if (length(args)!=4) {
  stop("Need 4 arguments - Directory , Input File, Output File, High Low File") 
}
print("####")
directory = args[1]
in_file = args[2]
out_file = args[3]
high_low_file = args[4] 
#low_q=as.numeric(args[5])

   
setwd(paste0("C:/Users/sambants/Desktop/TCGA Survival analysis/",directory))
library(survminer)
library(survival)
library(ggplot2)
library(dplyr)
library(Seurat)
pdf(paste0("C:/Users/sambants/Desktop/TCGA Survival analysis/",directory,"/",out_file), width = 20, height = 12, pointsize = 40)
LUAD <-read.csv(paste0("C:/Users/sambants/Desktop/TCGA Survival analysis/",directory,"/",in_file))
LUAD_high_low <-read.csv(paste0("C:/Users/sambants/Desktop/TCGA Survival analysis/",directory,"/",high_low_file))

names(LUAD)
LUAD_high_low
LUAD_CD79A <- LUAD %>% mutate(CD79A = ifelse(CD79A >=  LUAD_high_low$CD79A[2], "HIGH", ifelse(CD79A <=  LUAD_high_low$CD79A[1], "LOW","MEDIUM")))
LUAD_CD79A$CD79A <- factor(LUAD_CD79A$CD79A)
surv_object <- Surv(time = LUAD_CD79A$Time_months, event = LUAD_CD79A$Status)
fit1 <- survfit(surv_object ~ CD79A, data = LUAD_CD79A)
ggsurvplot(fit1, data = LUAD_CD79A, pval = TRUE)


LUAD_CD8A <- LUAD %>% mutate(CD8A = ifelse(CD8A >=  LUAD_high_low$CD8A[2], "HIGH", ifelse(CD8A <=  LUAD_high_low$CD8A[1], "LOW","MEDIUM")))
LUAD_CD8A$CD8A <- factor(LUAD_CD8A$CD8A)
surv_object <- Surv(time = LUAD_CD8A$Time_months, event = LUAD_CD8A$Status)
fit1 <- survfit(surv_object ~ CD8A, data = LUAD_CD8A)
ggsurvplot(fit1, data = LUAD_CD8A, pval = TRUE)

dev.off()


