#packages
library(readr)
library(tidyr)
library(dplyr)
library(tidyverse)
#===============================================================================================================================================
#load data

HPV_DATA<-read.csv("C:\\Users\\user\\Desktop\\HPV_DATA.csv")
dim(HPV_DATA)
#====================================================================================================================================
#Renaming variables
df_HPV_DATA<-HPV_DATA %>%
  rename(Education_level="Education.level",Monthly_income="Monthly.income",Employment_status="Employment.status",
         Marital_status="Marital.status",When_First_Diagnosed_withHIV="When.First.Diagnosed.withHIV",
         Year_Diagonised_with_HIV="Year.Diagonised.with.HIV",Year_of_Reference="Year.of.Reference",
         Years_Lived_with_HIV="Years.Lived.with.HIV",Year_treatment_started="Year.when.treatment.was.started",
         Skipped_Medication="Skipped.medication",Years_lived_current_partner="Years.lived.with.current.partner",
         Smoking="Smoking",Alcohol_intake="Alcohol.intake",Use_drugs="Use.of.any.drugs",Times_given_birth="Times.given.birth",
         Family_planning="Use.of.family.planning" ,Length_contraceptive_use="Length.of.contraceptive.use",
         Age_contraceptive_started="Age..contraceptive.started",Age_sexually_active="Age.became.sexually.active",
         Sex_partners_past_one_year="Sex.partners.in.the.past.one.year",Lifetime_sex_partner="Lifetime.sex.partners",
         Condom_previous_six_months="Condom.use.in.previous.six.months",History_cancer="History.of.cancer",
         History_STIs ="History.of.STIs",Knowledge_HPV="Knowledge.of.HPV",Believe_HPV_causes_cervical_cancer="Believe.that.HPV.causes.cervical.cancer",
         HPV_16_Status="HPV.16.Status",HPV_45_Ct="HPV.45..Ct.",HPV_45_Status="HPV.45.Status",HPV_16_Ct="HPV.16..Ct.",
         HPV_18_Status="HPV.18.Status",HPV_18_Ct="HPV.18..Ct.",Date_sample_collection="Date.of.sample.collection",
         Test_undergone="Test.undergone.VIA.VILI.pap.smear",Period_test_done="Period.the.test.was.done",
         Abnormal_results="Abnormal.results",
         HPV_Status="HPV.Status",HR_HPV_Status="HR.HPV.Status",HR_HPV_Ct="HR.HPV..Ct.") %>%
  view()
#===============================================================================================================================================================
#Removing missing Rows
df_HPV_DATA %>%
  select(Age,Ethnicity,Education_level,Monthly_income,Employment_status,Marital_status,When_First_Diagnosed_withHIV,Years_Lived_with_HIV,
         Year_treatment_started,Years_lived_current_partner,Skipped_Medication,Alcohol_intake,Use_drugs,Family_planning,Length_contraceptive_use,
         Age_contraceptive_started,Age_sexually_active,Lifetime_sex_partner,Condom_previous_six_months,History_cancer,History_STIs,
         Knowledge_HPV,Believe_HPV_causes_cervical_cancer,Test_undergone,Period_test_done,Abnormal_results,HPV_Status,
         HR_HPV_Status,HPV_16_Status,HPV_18_Status,HPV_45_Status) %>%
  filter(complete.cases(.))%>%
  View()
#===================================================================================================================================================
#Converting into factors

df_HPV_DATA$Education_level<-as.factor(df_HPV_DATA$Education_level)
df_HPV_DATA$Monthly_income<-as.factor(df_HPV_DATA$Monthly_income)
df_HPV_DATA$Employment_status<-as.factor(df_HPV_DATA$Employment_status)
df_HPV_DATA$Marital_status<-as.factor(df_HPV_DATA$Marital_status)
df_HPV_DATA$Skipped_Medication<-as.factor(df_HPV_DATA$Skipped_Medication)
df_HPV_DATA$Alcohol_intake<-as.factor(df_HPV_DATA$Alcohol_intake)
df_HPV_DATA$Use_drugs<-as.factor(df_HPV_DATA$Use_drugs)
df_HPV_DATA$Family_planning<-as.factor(df_HPV_DATA$Family_planning)
df_HPV_DATA$History_cancer<-as.factor(df_HPV_DATA$History_cancer)
df_HPV_DATA$History_STIs<-as.factor(df_HPV_DATA$History_STIs)
df_HPV_DATA$HPV_Status<-as.factor(df_HPV_DATA$HPV_Status)
df_HPV_DATA$Years_lived_current_partner<-as.factor(df_HPV_DATA$Years_lived_current_partner)
df_HPV_DATA$Age_sexually_active<-as.factor(df_HPV_DATA$Age_sexually_active)
df_HPV_DATA$Condom_previous_six_months<-as.factor(df_HPV_DATA$Condom_previous_six_months)
glimpse(df_HPV_DATA)
#=======================================================================================================================================
#Selecting the variables to work with
df_HPV_DATA %>%
  select(Age,Education_level,Monthly_income,Employment_status,Marital_status,Skipped_Medication,
         Alcohol_intake,Use_drugs,Family_planning,History_cancer,History_STIs,HPV_Status,Years_lived_current_partner,
         Age_sexually_active,Condom_previous_six_months,) %>%
  filter(complete.cases(.))%>%
  
  View()
#========================================================================================================================================
#checking the values in each variables selected
unique(df_HPV_DATA$Education_level)
unique(df_HPV_DATA$Monthly_income)
unique(df_HPV_DATA$Employment_status)
unique(df_HPV_DATA$Marital_status)
unique(df_HPV_DATA$Skipped_Medication)
unique(df_HPV_DATA$Alcohol_intake)
unique(df_HPV_DATA$Use_drugs)
unique(df_HPV_DATA$History_cancer)
unique(df_HPV_DATA$History_STIs)
unique(df_HPV_DATA$HPV_Status)
unique(df_HPV_DATA$Years_lived_current_partner)
#=======================================================================================================================================
#Re-coding values of variables
df_HPV_DATA$Education_level<-recode(df_HPV_DATA$Education_level,Primary="primary")
df_HPV_DATA$Years_lived_current_partner<-recode(df_HPV_DATA$Years_lived_current_partner)
unique(df_HPV_DATA$Education_level)
unique(df_HPV_DATA$HPV_Status)
#============================================================================================================================================
##Removing duplicates
HPV <- unique(df_HPV_DATA) 
HPV
df_HPV<-df_HPV_DATA %>%
  select(Age , Education_level , Monthly_income , Employment_status ,Marital_status ,
         Skipped_Medication ,Alcohol_intake , Use_drugs , History_cancer , History_STIs ,HPV_Status,
         Years_lived_current_partner) %>%
  filter(complete.cases(.)) %>%

  view()
#=======================================================================================================================================================
#model
mode1<-glm(HPV_Status~Age + Education_level + Monthly_income + Employment_status,
           family = binomial(link = "logit"),data = df_HPV)


summary(mode1)
#===========================================================================================================================
#Cross tabulation
library(gtsummary)
tbl_logistic_model<- mode1 %>%
  tbl_regression(
    exponentiate = TRUE,
    pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  add_glance_table(everything())%>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()
tbl_logistic_model
#=================================================================================================================================
#Data Visualization
##plot bar chat for Educational level
Table_Edulevel<-table(df_HPV$Education_level)
Table_Edulevel_prop<-prop.table(Table_Edulevel) #proportions
Table_Edulevel_prop_percentages<-round(Table_Edulevel_prop*100,2)

#Generate the Bar Plot
Bar_Edulevel <- barplot(Table_Edulevel_prop_percentages,
                          xlab = "Level of Education", ylab = "Percentage (%) of Education level",
                          main = "Bar plot of Education level",
                          col = c("blue"), 
                          beside = TRUE,
                          ylim = c(0, 60))

#Display the percentages on the Bar Plot

text(Bar_Edulevel,0, Table_Edulevel_prop_percentages, pos=3)
#==========================================================================================================================================
#pie chart for Monthly income

Edu_Monthly_income<-table(df_HPV$Monthly_income)
pie(Edu_Monthly_income, labels = paste(names (Edu_Monthly_income),"\n", Edu_Monthly_income,
                             sep =""), main = "A pie chart showing Monthly income of participants",
    col = c("brown", "blue"))
legend("topright",c("0-20000","21000-35000"), cex = 1.0,
       fill =c("brown","blue"))

#Add percentages to pie chart

pie_Monthly_income<-paste0(round(100*prop.table(Edu_Monthly_income),3),"%")
pie(Edu_Monthly_income, labels = pie_Monthly_income, main = "A pie chart showing Monthly income",
    col =c("brown","blue"))
legend("topright", c("0-20000","21000-35000"),cex = 1.0,
       fill =c("brown","blue"))


#================================================================================================================================
#pie chart for Employment status

Edu_Empl_status<-table(df_HPV$Employment_status)
pie(Edu_Empl_status, labels = paste(names (Edu_Empl_status),"\n", Edu_Empl_status,
                                       sep =""), main = "A pie chart showing Employment status of participants",
    col = c("brown", "blue","red"))
legend("bottomright",c("Employed","Not employed","Self-employed"), cex = 0.9,
       fill =c("brown","blue","red"))

#Add percentages to pie chart

pie_Empl_status<-paste0(round(100*prop.table(Edu_Empl_status),2), "%")
pie(Edu_Empl_status, labels = pie_Empl_status, main = "A pie chart showing Employment status of participants",
    col =c("brown","blue","red"))
legend("bottomright", c("Employed","Not employed","Self-employed"), cex = 0.9,
       fill =c("brown","blue","red"))
#=========================================================================================================================================
##Bar chat for Marital status
Table_Marital_status<-table(df_HPV$Marital_status)
Table_Marital_status_prop<-prop.table(Table_Marital_status) #proportions
Table_Marital_status_prop_percentages<-round(Table_Marital_status_prop*100,2)
#Generate the Bar Plot
Bar_Marital_status <- barplot(Table_Marital_status_prop_percentages,
                        xlab = "Level of Education", ylab = "Percentage (%) of Marital status",
                        main = "Bar plot of Marital status",
                        col = c("blue"), 
                        beside = TRUE,
                        ylim = c(0, 60))
#Display the percentages on the Bar Plot

text(Bar_Marital_status,0, Table_Marital_status_prop_percentages, pos=3)
#==================================================================================================================================

#pie chart for skipped medication

Skip_med<-table(df_HPV$Skipped_Medication)
pie(Skip_med, labels = paste(names (Skip_med),"\n", Skip_med,
                                    sep =""), main = "A pie chart showing whether a participant skipped medication",
    col = c("brown", "blue"))
legend("bottomright",c("No","Yes"), cex = 0.9,
       fill =c("brown","blue"))

#Add percentages to pie chart

pie_Skip_med<-paste0(round(100*prop.table(Skip_med),2), "%")
pie(Skip_med, labels = pie_Skip_med, main = "A pie chart showing whether a participant skipped medication",
    col =c("brown","blue"))
legend("bottomright", c("No","Yes"), cex = 0.9,
       fill =c("brown","blue"))

#===========================================================================================================================================
#pie chart showing Alcohol intake

table(df_HPV$Alcohol_intake)
Al_intake<-table(df_HPV$Alcohol_intake)
pie(Al_intake, labels = paste(names (Al_intake),"\n", Al_intake,
                             sep =""), main = "A pie chart showing whether a participant was taking Alcohol",
    col = c("brown", "blue"))
legend("bottomright",c("No","Yes"), cex = 0.9,
       fill =c("brown","blue"))

#Add percentages to pie chart

pie_Al_intake<-paste0(round(100*prop.table(Al_intake),2), "%")
pie(Al_intake, labels = pie_Al_intake, main = "A pie chart showing whether a participant was taking Alcohol",
    col =c("brown","blue"))
legend("bottomright", c("No","Yes"), cex = 0.9,
       fill =c("brown","blue"))
#=============================================================================================================================
#pie chart showing History of cancer

His_cancer<-table(df_HPV$History_cancer)
pie(His_cancer, labels = paste(names (His_cancer),"\n", His_cancer,
                              sep =""), main = "A pie chart showing History of cancer",
    col = c("brown", "blue"))
legend("bottomright",c("No","Yes"), cex = 0.9,
       fill =c("brown","blue"))

#Add percentages to pie chart

pie_His_cancer<-paste0(round(100*prop.table(His_cancer),2), "%")
pie(His_cancer, labels = pie_His_cancer, main = "A pie chart showing History of cancer",
    col =c("brown","blue"))
legend("bottomright", c("No","Yes"), cex = 0.9,
       fill =c("brown","blue"))


#====================================================================================================================================
#pie chart showing History of STIs

His_STI<-table(df_HPV$History_STIs)
pie(His_STI, labels = paste(names (His_STI),"\n", His_STI,
                               sep =""), main = "A pie chart showing History of STIs",
    col = c("brown", "blue"))
legend("bottomright",c("No","Yes"), cex = 0.9,
       fill =c("brown","blue"))

#Add percentages to pie chart

pie_His_STI<-paste0(round(100*prop.table(His_STI),2), "%")
pie(His_STI, labels = pie_His_STI, main = "A pie chart showing History of STIs",
    col =c("brown","blue"))
legend("bottomright", c("No","Yes"), cex = 0.9,
       fill =c("brown","blue"))
#===============================================================================================================================
#pie chart showing HPV status

Hpv_status<-table(df_HPV$HPV_Status)
pie(Hpv_status, labels = paste(names (Hpv_status),"\n", Hpv_status,
                            sep =""), main = "A pie chart showing HPV status",
    col = c("brown", "blue"))
legend("bottomright",c("No","Yes"), cex = 0.9,
       fill =c("brown","blue"))

#Add percentages to pie chart

pie_Hpv_status<-paste0(round(100*prop.table(Hpv_status),2), "%")
pie(Hpv_status, labels = pie_Hpv_status, main = "A pie chart showing HPV status",
    col =c("brown","blue"))
legend("bottomright", c("No","Yes"), cex = 0.9,
       fill =c("brown","blue"))

#=============================================================================================================================
#Descriptive statistics

df_HPV %>%
  tbl_summary(by = HPV_Status) %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 2)) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**HPV status**") %>%
  modify_footnote(
    all_stat_cols() ~ "Median (IQR) or Frequency (%)"
  ) %>%
  modify_caption("**Table 1. HPV Status Characteristics**") %>%
  bold_labels()
#=======================================================================================================================================
#Box plot
df_Hpv_status<-df_HPV %>%
  select(Age,HPV_Status) %>%
  view()

df_Hpv_status %>% 
  filter(HPV_Status == "Positive" | HPV_Status == "Negative") %>%
  ggplot(aes(y=Age, x=HPV_Status, fill=as.factor(HPV_Status))) + 
  geom_boxplot() + 
  theme_light() +
  labs(title = "A box plot of HPV status", 
       subtitle = "HPV Status(Positive or Negative)",
       y = "Age of women", 
       x = "HPV Status", 
       fill = "HPV_Status"
  ) 

#========================================================================================================================================
#Histogram(Age)
Bins<-5
hist(df_HPV$Age,main = "A Histogram of Ages of women ",ylab = "Number of women",
     xlab = "Bin width",col = "red",
     breaks = Bins)

#=============================================================================================================================
df_HPV_genotypes<-df_HPV_DATA %>%
  select(HPV_16_Status,HPV_18_Status,HPV_45_Status,HR_HPV_Status,HPV_Status) %>%
  filter(complete.cases(.)) %>%
  view()

#Table for HPV status 
df_HPV_genotypes %>%
  tbl_summary(by = HPV_Status) %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 2)) %>%
  add_overall() %>%
  add_n() %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**HPV status**") %>%
  modify_footnote(
    all_stat_cols() ~ "Median (IQR) or Frequency (%)"
  ) %>%
  modify_caption("**Table 2. HPV Status Characteristics**") %>%
  bold_labels()
#=========================================================================================================================
##Bar chat HPV 16 status
Table_16_status<-table(df_HPV_genotypes$HPV_16_Status)
Table_16_status_prop<-prop.table(Table_16_status) #proportions
Table_16_status_prop_percentages<-round(Table_16_status_prop*100,2)
#Generate the Bar Plot
Bar_16_status <- barplot(Table_16_status_prop_percentages,
                              xlab = "HPV 16 status", ylab = "Percentage (%) of HPV 16 Status",
                              main = "Bar plot of HPV 16 status",
                              col = c("purple"), 
                              beside = TRUE,
                              ylim = c(0, 90))
#Display the percentages on the Bar Plot

text(Bar_16_status,0, Table_16_status_prop_percentages, pos=3)
#==============================================================================================================================
##Bar chat HPV 18 status
Table_18_status<-table(df_HPV_genotypes$HPV_18_Status)
Table_18_status_prop<-prop.table(Table_18_status) #proportions
Table_18_status_prop_percentages<-round(Table_18_status_prop*100,2)
#Generate the Bar Plot
Bar_18_status <- barplot(Table_18_status_prop_percentages,
                         xlab = "HPV 18 status", ylab = "Percentage (%) of HPV 18 Status",
                         main = "Bar plot of HPV 18 status",
                         col = c("purple"), 
                         beside = TRUE,
                         ylim = c(0, 90))
#Display the percentages on the Bar Plot

text(Bar_18_status,0, Table_18_status_prop_percentages, pos=3)
#===============================================================================================================================

##Bar chat HPV 45 status
Table_45_status<-table(df_HPV_genotypes$HPV_45_Status)
Table_45_status_prop<-prop.table(Table_45_status) #proportions
Table_45_status_prop_percentages<-round(Table_45_status_prop*100,2)
#Generate the Bar Plot
Bar_45_status <- barplot(Table_45_status_prop_percentages,
                         xlab = "HPV 45 status", ylab = "Percentage (%) of HPV 45 Status",
                         main = "Bar plot of HPV 45 status",
                         col = c("purple"), 
                         beside = TRUE,
                         ylim = c(0, 90))
#Display the percentages on the Bar Plot

text(Bar_45_status,0, Table_45_status_prop_percentages, pos=3)
#===================================================================================================================================
##Bar chat HR HPV status
Table_HR_status<-table(df_HPV_genotypes$HR_HPV_Status)
Table_HR_status_prop<-prop.table(Table_HR_status) #proportions
Table_HR_status_prop_percentages<-round(Table_HR_status_prop*100,2)
#Generate the Bar Plot
Bar_HR_status <- barplot(Table_HR_status_prop_percentages,
                         xlab = "HR HPV status", ylab = "Percentage (%) of HR HPV status",
                         main = "Bar plot of HR HPV status",
                         col = c("purple"), 
                         beside = TRUE,
                         ylim = c(0, 90))
#Display the percentages on the Bar Plot

text(Bar_HR_status,0, Table_HR_status_prop_percentages, pos=3)














