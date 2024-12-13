#PATERNAL CARE BEHAVIOR_TIME BUDGETS

#----------------------------------------------

library(lme4)
library(nlme)
library(MuMIn)
library(Hmisc)
library(glmmTMB)
library(AICcmodavg)
library(bbmle)
library(DHARMa)
library(dplyr)
library(ggplot2)
library(car)
library(GLMMadaptive)
library(performance)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(readr)
library(tidyverse)
library(rphylopic)
library(pROC)
library(stringr)
library(gridExtra)

install.packages("R2admb")

install.packages("glmmADMB", 
                 repos=c("http://glmmadmb.r-forge.r-project.org/repos",
                         getOption("repos")),
                 type="source")

library(glmmADMB)
library(readr)
library(vegan)
library(ggplot2)
library(psych)
library(rstatix)
library(car)
library(multcomp)
library(vtable)
library(cowplot)


#-------------------------------------------------
setwd("C:/Users/kaunert/Desktop/Kaunert FULL DISSERTATION/Publication Drafts/PatCare/Animal Behaviour")

tb.data <- read.csv(choose.files())

View(tb.data)

str(tb.data)

tb.data$YEAR<-as.factor(tb.data$YEAR)
tb.data$WATERWAY<-as.factor(tb.data$WATERWAY)
tb.data$SITE<-as.factor(tb.data$SITE)
tb.data$SUBJECT<-as.factor(tb.data$SUBJECT)
tb.data$NB<-as.factor(tb.data$NB)
tb.data$DATE<-as.factor(tb.data$DATE)
tb.data$MONTH<-as.factor(tb.data$MONTH)
tb.data$FATE<-as.factor(tb.data$FATE)
tb.data$EGG_SURV_CAT<-as.factor(tb.data$EGG_SURV_CAT)
tb.data$DEV_STAGE<-as.factor(tb.data$DEV_STAGE)
tb.data$PIT<-as.factor(tb.data$PIT)
tb.data$MALE_CLASS<-as.factor(tb.data$MALE_CLASS)
tb.data$VID_ID<-as.factor(tb.data$VID_ID)


str(tb.data)


#PLAN
  #1)BROAD SUMMARY OF ETHOGRAM BEHAVIORS (Just proportion of Time)
    #ACROSS ALL OBSERVATIONS

mean(tb.data$CARE_PROP)
sum.stats.CARE_PROP<-describeBy(tb.data$CARE_PROP)
sum.stats.CARE_PROP

st(tb.data[24:64])


  #2)#SUMMARY STATS TABLE BY NEST (Just proportion of Time)

sum.stats.NB.CARE_PROP<-describeBy(tb.data$CARE_PROP, group = tb.data$SUBJECT)
sum.stats.NB.CARE_PROP

sumtable(tb.data, 
         group = 'SUBJECT', 
         group.long = TRUE, 
         vars = c('CARE_PROP',
                  'FAN_PROP',
                  'AGIT_PROP',
                  'TFAN_PROP',
                  'ROCKTAIL_PROP',
                  'ROCKAG_PROP',
                  'WALKAG_PROP',
                  'HEADAG_PROP',
                  'CANN_PROP',
                  'GUARD_PROP',
                  'POS_CHAMB_PROP',
                  'POS_TUN_PROP',
                  'POS_OUT_PROP',
                  'PROP_NOCARE',
                  'PROP_WALK',
                  'PROP_ROCKO2',
                  'PROP_SBREATH',
                  'PROP_YAWN',
                  'PROP_STILL',
                  'DEPTH',
                  'TEMP',
                  'PH',
                  'COND',
                  'DO'))


  #3)OVERALL TOTAL CARE ACROSS EGG,HATCH,LARVAE
      #JUSTIFY ONLY USING EGG/HATCH DATA BELOW

        #DOES DEVELOPMENTAL STAGE (EGGS, HATCH, LARVAE) AFFECT OVERALL CARE?========================
        

                      #CARE TOTAL PROP--------------------------------------------------------
                      
                      pdev<-glmer(CARE_PROP~DEV_STAGE+ 
                                   (1|SUBJECT),
                                 weights = TOT_LENG_S,
                                 family = "binomial",
                                 data = tb.data)
                      
                      
                      summary(pdev)
                    
                      summary(glht(pdev, mcp(DEV_STAGE="Tukey")))
                   
                  #BOXPLOT
                      boxplot(CARE_PROP ~ DEV_STAGE, data = tb.data, 
                              main = "", xlab = "Developmental Stage", ylab = "Proportion of Time in Care Behaviors") 
        
                      
                  #MEAN + SE PLOT
                      sum.stats.dev<-describeBy(tb.data$CARE_PROP, group=tb.data$DEV_STAGE, mat = TRUE) 
                      sum.stats.dev 
                      
                      dev.plot<-ggplot(sum.stats.dev, aes(x = group1, y= mean))+
                        theme_classic()+
                        geom_point(size = 3)+
                        geom_errorbar(aes(ymin=mean-se, ymax=mean+se), linewidth = 0.5, width=.1)+
                        labs(y="Proportion of Time in Care Behaviors", x= "Developmental Stage")
                      
                      dev.plot
                      
                      dev.plot +
                        theme(axis.title.x = element_text(size = 18))+
                        theme(axis.title.y = element_text(size = 18))+
                        theme(axis.text.x = element_text(size = 15))+
                        theme(axis.text.y = element_text(size = 15))
                      
                      
                      
                      
                      
                      
                      
  #3)RUN MANY MODELS WITH EGG/HATCH DATA ONLY


#SUBSET ONLY EGG and HATCH DATA------------------------------------------------------------
egg.data <- subset(tb.data, tb.data$DEV_STAGE %in% c("EGGS", "HATCH"))


View(egg.data)
#------------------------------------------------------------------------------------------

#NEW PLAN#######---VDP 6.2.23----########################################################################

#BEH LIST

#####GROUPED BEHAVIORS#####
      #CARE TOTAL--------------------------------------------------------
        
      #FAN TOTAL---------------------------------------------------------
      
      #AGIT TOTAL--------------------------------------------------------

#####INDIVIDUAL BEHAVIORS#####     
      #TFAN -------------------------------------------------------------
      
      #ROCKTAIL ---------------------------------------------------------
      
      #ROCKAG -----------------------------------------------------------
      
      #WALKAG -----------------------------------------------------------
      
      #HEADAG -----------------------------------------------------------
      
      #CANN -------------------------------------------------------------
      
      #GUARD ------------------------------------------------------------

      #ROCKO2 -----------------------------------------------------------


#MODELING-----

#FREQ-------------------------------------------------------------------------------------------------------------

      #CREATE NEW COLUMNs TO STANDARDIZE FREQUENCY OF BEHAVIORS WITH LENGTH OF VIDEO (FREQUENCY PER MINUTE)
          #EGG/HATCH DATA
            egg.data$CARE_MIN <- (egg.data$CARE_FREQ/(egg.data$TOT_LENG_S/60))
            egg.data$FAN_MIN <- (egg.data$FAN_FREQ/(egg.data$TOT_LENG_S/60))
            egg.data$AGIT_MIN <- (egg.data$AGIT_FREQ/(egg.data$TOT_LENG_S/60))
            egg.data$TFAN_MIN <- (egg.data$TFAN_FREQ/(egg.data$TOT_LENG_S/60))
            egg.data$ROCKTAIL_MIN <- (egg.data$ROCKTAIL_FREQ/(egg.data$TOT_LENG_S/60))
            egg.data$ROCKAG_MIN <- (egg.data$ROCKAG_FREQ/(egg.data$TOT_LENG_S/60))
            egg.data$WALKAG_MIN <- (egg.data$WALKAG_FREQ/(egg.data$TOT_LENG_S/60))
            egg.data$HEADAG_MIN <- (egg.data$HEADAG_FREQ/(egg.data$TOT_LENG_S/60))
            egg.data$CANN_MIN <- (egg.data$CANN_FREQ/(egg.data$TOT_LENG_S/60))
            egg.data$GUARD_MIN <- (egg.data$GUARD_FREQ/(egg.data$TOT_LENG_S/60))
            egg.data$ROCKO2_MIN <- (egg.data$ROCKO2_FREQ/(egg.data$TOT_LENG_S/60))
            
 
          #FULL DATA (EGG,HATCH,LARV)
            tb.data$CARE_MIN <- (tb.data$CARE_FREQ/(tb.data$TOT_LENG_S/60))
            tb.data$FAN_MIN <- (tb.data$FAN_FREQ/(tb.data$TOT_LENG_S/60))
            tb.data$AGIT_MIN <- (tb.data$AGIT_FREQ/(tb.data$TOT_LENG_S/60))
            tb.data$TFAN_MIN <- (tb.data$TFAN_FREQ/(tb.data$TOT_LENG_S/60))
            tb.data$ROCKTAIL_MIN <- (tb.data$ROCKTAIL_FREQ/(tb.data$TOT_LENG_S/60))
            tb.data$ROCKAG_MIN <- (tb.data$ROCKAG_FREQ/(tb.data$TOT_LENG_S/60))
            tb.data$WALKAG_MIN <- (tb.data$WALKAG_FREQ/(tb.data$TOT_LENG_S/60))
            tb.data$HEADAG_MIN <- (tb.data$HEADAG_FREQ/(tb.data$TOT_LENG_S/60))
            tb.data$CANN_MIN <- (tb.data$CANN_FREQ/(tb.data$TOT_LENG_S/60))
            tb.data$GUARD_MIN <- (tb.data$GUARD_FREQ/(tb.data$TOT_LENG_S/60))
            tb.data$ROCKO2_MIN <- (tb.data$ROCKO2_FREQ/(tb.data$TOT_LENG_S/60))
            

            print(egg.data)
            print(tb.data)


  #MULTIPLE PREDICTOR MODELS-----------------------------------          

            
      #ENVIRONMENTAL (TEMP,DO,COND)-----
          
            #TEST CORRELATIONS B/T PREDICTORS
            cor(egg.data$DO, egg.data$TEMP)
            cor(egg.data$DO, egg.data$COND)
            cor(egg.data$TEMP, egg.data$COND)
            
            #####GROUPED BEHAVIORS#####
                  #CARE TOTAL--------------------------------------------------------
                  
                        fe1<-lme(CARE_MIN~scale(TEMP)+scale(DO)+scale(COND), 
                                   random = ~1|SUBJECT,
                                   data = egg.data)
                        
                        
                        summary(fe1)
                        
                        #PLOTS
                        plot(egg.data$TEMP,egg.data$CARE_MIN)
                        plot(egg.data$DO,egg.data$CARE_MIN)
                        plot(egg.data$COND,egg.data$CARE_MIN)
                        
            
                  #FAN TOTAL---------------------------------------------------------
                        fe2<-lme(FAN_MIN~scale(TEMP)+scale(DO)+scale(COND), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(fe2)
                        
                        #PLOTS
                        plot(egg.data$TEMP,egg.data$FAN_MIN)
                        plot(egg.data$DO,egg.data$FAN_MIN)
                        plot(egg.data$COND,egg.data$FAN_MIN)
                        
                  #AGIT TOTAL--------------------------------------------------------
                        fe3<-lme(AGIT_MIN~scale(TEMP)+scale(DO)+scale(COND), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(fe3)
                        
                        #PLOTS
                        plot(egg.data$TEMP,egg.data$AGIT_MIN)
                        plot(egg.data$DO,egg.data$AGIT_MIN)
                        plot(egg.data$COND,egg.data$AGIT_MIN)
                        
            #####INDIVIDUAL BEHAVIORS#####     
                  #TFAN -------------------------------------------------------------
                        fe4<-lme(TFAN_MIN~scale(TEMP)+scale(DO)+scale(COND), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(fe4)
                        
                        #PLOTS
                        plot(egg.data$TEMP,egg.data$TFAN_MIN)
                        plot(egg.data$DO,egg.data$TFAN_MIN)
                        plot(egg.data$COND,egg.data$TFAN_MIN)
                        
                  #ROCKTAIL ---------------------------------------------------------
                        fe5<-lme(ROCKTAIL_MIN~scale(TEMP)+scale(DO)+scale(COND), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(fe5)
                        
                        #PLOTS
                        plot(egg.data$TEMP,egg.data$ROCKTAIL_MIN)
                        plot(egg.data$DO,egg.data$ROCKTAIL_MIN)
                        plot(egg.data$COND,egg.data$ROCKTAIL_MIN)
                        
                  #ROCKAG -----------------------------------------------------------
                        fe6<-lme(ROCKAG_MIN~scale(TEMP)+scale(DO)+scale(COND), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(fe6)
                        
                        #PLOTS
                        plot(egg.data$TEMP,egg.data$ROCKAG_MIN)
                        plot(egg.data$DO,egg.data$ROCKAG_MIN)
                        plot(egg.data$COND,egg.data$ROCKAG_MIN)
                  #WALKAG -----------------------------------------------------------
                        fe7<-lme(WALKAG_MIN~scale(TEMP)+scale(DO)+scale(COND), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(fe7)
                        
                        #PLOTS
                        plot(egg.data$TEMP,egg.data$WALKAG_MIN)
                        plot(egg.data$DO,egg.data$WALKAG_MIN)
                        plot(egg.data$COND,egg.data$WALKAG_MIN)
                        
                  #HEADAG -----------------------------------------------------------
                        fe8<-lme(HEADAG_MIN~scale(TEMP)+scale(DO)+scale(COND), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(fe8)
                        
                        #PLOTS
                        plot(egg.data$TEMP,egg.data$HEADAG_MIN)
                        plot(egg.data$DO,egg.data$HEADAG_MIN)
                        plot(egg.data$COND,egg.data$HEADAG_MIN)
                        
                  #CANN -------------------------------------------------------------
                        fe9<-lme(CANN_MIN~scale(TEMP)+scale(DO)+scale(COND), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(fe9)
                        
                        #PLOTS
                        plot(egg.data$TEMP,egg.data$CANN_MIN)
                        plot(egg.data$DO,egg.data$CANN_MIN)
                        plot(egg.data$COND,egg.data$CANN_MIN)
                   
                        
                  #GUARD ------------------------------------------------------------
                  
                        fe10<-lme(GUARD_MIN~scale(TEMP)+scale(DO)+scale(COND), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(fe10)
                        
                        #PLOTS
                        plot(egg.data$TEMP,egg.data$GUARD_MIN)
                        plot(egg.data$DO,egg.data$GUARD_MIN)
                        plot(egg.data$COND,egg.data$GUARD_MIN)
                        
                    
                  #ROCKO2 ------------------------------------------------------------
                        
                        fe11<-lme(ROCKO2_MIN~scale(TEMP)+scale(DO)+scale(COND), 
                                  random = ~1|SUBJECT,
                                  data = egg.data)
                        
                        
                        summary(fe11)
                        
                        #PLOTS
                        plot(egg.data$TEMP,egg.data$ROCKO2_MIN)
                        plot(egg.data$DO,egg.data$ROCKO2_MIN)
                        plot(egg.data$COND,egg.data$ROCKO2_MIN)       
                        
            
      #BIOLOGICAL (TL, EGGS_IN_NEST, MALE_CLASS)-----
            
            #TEST CORRELATIONS B/T PREDICTORS
            cor(egg.data$TL, egg.data$EGGS_IN_NEST)
      
            #####GROUPED BEHAVIORS#####
                  #CARE TOTAL--------------------------------------------------------
            
                        fb1<-lme(CARE_MIN~scale(TL)+scale(EGGS_IN_NEST)+MALE_CLASS, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(fb1)
                        
                        #PLOTS
                        plot(egg.data$TL,egg.data$CARE_MIN)
                        plot(egg.data$EGGS_IN_NEST,egg.data$CARE_MIN)
                        plot(egg.data$MALE_CLASS,egg.data$CARE_MIN,
                             xlab = "Male Age Class",
                             ylab = "Care Frequency")

                  #FAN TOTAL---------------------------------------------------------
                        fb2<-lme(FAN_MIN~scale(TL)+scale(EGGS_IN_NEST)+MALE_CLASS, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(fb2)
                        
                        #PLOTS
                        plot(egg.data$TL,egg.data$FAN_MIN)
                        plot(egg.data$EGGS_IN_NEST,egg.data$FAN_MIN)
                        plot(egg.data$MALE_CLASS,egg.data$FAN_MIN,
                             xlab = "Male Age Class",
                             ylab = "Fan Frequency")
                        
                  #AGIT TOTAL--------------------------------------------------------
                        fb3<-lme(AGIT_MIN~scale(TL)+scale(EGGS_IN_NEST)+MALE_CLASS, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(fb3)
                        
                        #PLOTS
                        plot(egg.data$TL,egg.data$AGIT_MIN)
                        plot(egg.data$EGGS_IN_NEST,egg.data$AGIT_MIN)
                        plot(egg.data$MALE_CLASS,egg.data$AGIT_MIN,
                             xlab = "Male Age Class",
                             ylab = "Fan Frequency")
                        
            #####INDIVIDUAL BEHAVIORS##### 
                        
                  #TFAN -------------------------------------------------------------
                        fb4<-lme(TFAN_MIN~scale(TL)+scale(EGGS_IN_NEST)+MALE_CLASS, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(fb4)
                        
                        #PLOTS
                        plot(egg.data$TL,egg.data$TFAN_MIN)
                        plot(egg.data$EGGS_IN_NEST,egg.data$TFAN_MIN)
                        plot(egg.data$MALE_CLASS,egg.data$TFAN_MIN,
                             xlab = "Male Age Class",
                             ylab = "Fan Frequency")
                        
                  #ROCKTAIL ---------------------------------------------------------
                        fb5<-lme(ROCKTAIL_MIN~scale(TL)+scale(EGGS_IN_NEST)+MALE_CLASS, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(fb5)
                        
                        #PLOTS
                        plot(egg.data$TL,egg.data$ROCKTAIL_MIN)
                        plot(egg.data$EGGS_IN_NEST,egg.data$ROCKTAIL_MIN)
                        plot(egg.data$MALE_CLASS,egg.data$ROCKTAIL_MIN,
                             xlab = "Male Age Class",
                             ylab = "Rock/Tail Frequency")

                  #ROCKAG -----------------------------------------------------------
                        fb6<-lme(ROCKAG_MIN~scale(TL)+scale(EGGS_IN_NEST)+MALE_CLASS, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(fb6)
                        
                        #PLOTS
                        plot(egg.data$TL,egg.data$ROCKAG_MIN)
                        plot(egg.data$EGGS_IN_NEST,egg.data$ROCKAG_MIN)
                        plot(egg.data$MALE_CLASS,egg.data$ROCKAG_MIN,
                             xlab = "Male Age Class",
                             ylab = "Rock Agit Frequency")
                        
                  #WALKAG -----------------------------------------------------------
                        fb7<-lme(WALKAG_MIN~scale(TL)+scale(EGGS_IN_NEST)+MALE_CLASS, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(fb7)
                        
                        #PLOTS
                        plot(egg.data$TL,egg.data$WALKAG_MIN)
                        plot(egg.data$EGGS_IN_NEST,egg.data$WALKAG_MIN)
                        plot(egg.data$MALE_CLASS,egg.data$WALKAG_MIN,
                             xlab = "Male Age Class",
                             ylab = "Walk Agit Frequency")
                  #HEADAG -----------------------------------------------------------
                        fb8<-lme(HEADAG_MIN~scale(TL)+scale(EGGS_IN_NEST)+MALE_CLASS, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(fb8)
                        
                        #PLOTS
                        plot(egg.data$TL,egg.data$HEADAG_MIN)
                        plot(egg.data$EGGS_IN_NEST,egg.data$HEADAG_MIN)
                        plot(egg.data$MALE_CLASS,egg.data$HEADAG_MIN,
                             xlab = "Male Age Class",
                             ylab = "Head Agit Frequency")
                  #CANN -------------------------------------------------------------
                        fb9<-lme(CANN_MIN~scale(TL)+scale(EGGS_IN_NEST)+MALE_CLASS, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(fb9)
                        
                        #PLOTS
                        plot(egg.data$TL,egg.data$CANN_MIN)
                        plot(egg.data$EGGS_IN_NEST,egg.data$CANN_MIN)
                        plot(egg.data$MALE_CLASS,egg.data$CANN_MIN,
                             xlab = "Male Age Class",
                             ylab = "Cannibalism Frequency")
                  #GUARD ------------------------------------------------------------     
                        fb10<-lme(GUARD_MIN~scale(TL)+scale(EGGS_IN_NEST)+MALE_CLASS, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(fb10)
                        
                        #PLOTS
                        plot(egg.data$TL,egg.data$GUARD_MIN)
                        plot(egg.data$EGGS_IN_NEST,egg.data$GUARD_MIN)
                        plot(egg.data$MALE_CLASS,egg.data$GUARD_MIN,
                             xlab = "Male Age Class",
                             ylab = "Guarding Frequency")
                        
                  #ROCKO2 ------------------------------------------------------------     
                        fb11<-lme(ROCKO2_MIN~scale(TL)+scale(EGGS_IN_NEST)+MALE_CLASS, 
                                  random = ~1|SUBJECT,
                                  data = egg.data)
                        
                        
                        summary(fb11)
                        
                        #PLOTS
                        plot(egg.data$TL,egg.data$ROCKO2_MIN)
                        plot(egg.data$EGGS_IN_NEST,egg.data$ROCKO2_MIN)
                        plot(egg.data$MALE_CLASS,egg.data$ROCKO2_MIN,
                             xlab = "Male Age Class",
                             ylab = "ROCKO2 Frequency")     
                        
        
      #TEMPORAL (DAY_SEP6)-----
      
            #####GROUPED BEHAVIORS#####
                  #CARE TOTAL--------------------------------------------------------
                        
                        ft1<-lme(CARE_MIN~scale(DAY_SEP6), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(ft1)
                        
                        #PLOTS
                        plot(egg.data$DAY_SEP6,egg.data$CARE_MIN)
                        
                        
                  #FAN TOTAL---------------------------------------------------------
                        
                        ft2<-lme(FAN_MIN~scale(DAY_SEP6), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(ft2)
                        
                        #PLOTS
                        plot(egg.data$DAY_SEP6,egg.data$FAN_MIN)
                        
                  #AGIT TOTAL--------------------------------------------------------
                        ft3<-lme(AGIT_MIN~scale(DAY_SEP6), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(ft3)
                        
                        #PLOTS
                        plot(egg.data$DAY_SEP6,egg.data$AGIT_MIN)
                        
            #####INDIVIDUAL BEHAVIORS#####     
                  #TFAN -------------------------------------------------------------
                        
                        ft4<-lme(TFAN_MIN~scale(DAY_SEP6), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(ft4)
                        
                        #PLOTS
                        plot(egg.data$DAY_SEP6,egg.data$TFAN_MIN)
                        
                  #ROCKTAIL ---------------------------------------------------------
                        ft5<-lme(ROCKTAIL_MIN~scale(DAY_SEP6), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(ft5)
                        
                        #PLOTS
                        plot(egg.data$DAY_SEP6,egg.data$ROCKTAIL_MIN)
                        
                  #ROCKAG -----------------------------------------------------------
                        ft6<-lme(ROCKAG_MIN~scale(DAY_SEP6), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(ft6)
                        
                        #PLOTS
                        plot(egg.data$DAY_SEP6,egg.data$ROCKAG_MIN)
                        
                  #WALKAG -----------------------------------------------------------
                        ft7<-lme(WALKAG_MIN~scale(DAY_SEP6), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(ft7)
                        
                        #PLOTS
                        plot(egg.data$DAY_SEP6,egg.data$WALKAG_MIN)
                        
                  #HEADAG -----------------------------------------------------------
                        ft8<-lme(HEADAG_MIN~scale(DAY_SEP6), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(ft8)
                        
                        #PLOTS
                        plot(egg.data$DAY_SEP6,egg.data$HEADAG_MIN)
                  #CANN -------------------------------------------------------------
                        ft9<-lme(CANN_MIN~scale(DAY_SEP6), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(ft9)
                        
                        #PLOTS
                        plot(egg.data$DAY_SEP6,egg.data$CANN_MIN)
                  #GUARD ------------------------------------------------------------
                        ft10<-lme(GUARD_MIN~scale(DAY_SEP6), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(ft10)
                        
                        #PLOTS
                        plot(egg.data$DAY_SEP6,egg.data$GUARD_MIN)

                  #ROCKO2 ------------------------------------------------------------
                        ft11<-lme(ROCKO2_MIN~scale(DAY_SEP6), 
                                  random = ~1|SUBJECT,
                                  data = egg.data)
                        
                        
                        summary(ft11)
                        
                        #PLOTS
                        plot(egg.data$DAY_SEP6,egg.data$ROCKO2_MIN)

#MEAN DURATION OF BEHAVIORS-------------------------------------------------------------------------------------------------------------
                        

  #MULTIPLE PREDICTOR MODELS-----------------------------------          
                        
      #ENVIRONMENTAL (TEMP,DO,COND)-----
                        
            #TEST CORRELATIONS B/T PREDICTORS
            cor(egg.data$DO, egg.data$TEMP)
            cor(egg.data$DO, egg.data$COND)
            cor(egg.data$TEMP, egg.data$COND)
                        
          #####GROUPED BEHAVIORS#####
            
                  #CARE TOTAL--------------------------------------------------------
                
                        me1<-lme(CARE_MEAN~scale(TEMP)+scale(DO)+scale(COND), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(me1)
                        
                        #PLOTS
                        plot(egg.data$TEMP,egg.data$CARE_MEAN)
                        plot(egg.data$DO,egg.data$CARE_MEAN)
                        plot(egg.data$COND,egg.data$CARE_MEAN)                        
                        
                  #FAN TOTAL---------------------------------------------------------
                        me2<-lme(FAN_MEAN~scale(TEMP)+scale(DO)+scale(COND), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(me2)
                        
                        #PLOTS
                        plot(egg.data$TEMP,egg.data$FAN_MEAN)
                        plot(egg.data$DO,egg.data$FAN_MEAN)
                        plot(egg.data$COND,egg.data$FAN_MEAN)
                        
                  #AGIT TOTAL--------------------------------------------------------
                        me3<-lme(AGIT_MEAN~scale(TEMP)+scale(DO)+scale(COND), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(me3)
                        
                        #PLOTS
                        plot(egg.data$TEMP,egg.data$AGIT_MEAN)
                        plot(egg.data$DO,egg.data$AGIT_MEAN)
                        plot(egg.data$COND,egg.data$AGIT_MEAN)
                        
          #####INDIVIDUAL BEHAVIORS#####     
                  #TFAN -------------------------------------------------------------
                        me4<-lme(TFAN_MEAN~scale(TEMP)+scale(DO)+scale(COND), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(me4)
                        
                        #PLOTS
                        plot(egg.data$TEMP,egg.data$TFAN_MEAN)
                        plot(egg.data$DO,egg.data$TFAN_MEAN)
                        plot(egg.data$COND,egg.data$TFAN_MEAN)
                        
                  #ROCKTAIL ---------------------------------------------------------
                        me5<-lme(ROCKTAIL_MEAN~scale(TEMP)+scale(DO)+scale(COND), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(me5)
                        
                        #PLOTS
                        plot(egg.data$TEMP,egg.data$ROCKTAIL_MEAN)
                        plot(egg.data$DO,egg.data$ROCKTAIL_MEAN)
                        plot(egg.data$COND,egg.data$ROCKTAIL_MEAN)
                        
                  #ROCKAG -----------------------------------------------------------
                        me6<-lme(ROCKAG_MEAN~scale(TEMP)+scale(DO)+scale(COND), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(me6)
                        
                        #PLOTS
                        plot(egg.data$TEMP,egg.data$ROCKAG_MEAN)
                        plot(egg.data$DO,egg.data$ROCKAG_MEAN)
                        plot(egg.data$COND,egg.data$ROCKAG_MEAN)

                        
                  #WALKAG -----------------------------------------------------------
                        me7<-lme(WALKAG_MEAN~scale(TEMP)+scale(DO)+scale(COND), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(me7)
                        
                        #PLOTS
                        plot(egg.data$TEMP,egg.data$WALKAG_MEAN)
                        plot(egg.data$DO,egg.data$WALKAG_MEAN)
                        plot(egg.data$COND,egg.data$WALKAG_MEAN)
                  #HEADAG -----------------------------------------------------------
                        me8<-lme(HEADAG_MEAN~scale(TEMP)+scale(DO)+scale(COND), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(me8)
                        
                        #PLOTS
                        plot(egg.data$TEMP,egg.data$HEADAG_MEAN)
                        plot(egg.data$DO,egg.data$HEADAG_MEAN)
                        plot(egg.data$COND,egg.data$HEADAG_MEAN)
                  #CANN -------------------------------------------------------------
                        me9<-lme(CANN_MEAN~scale(TEMP)+scale(DO)+scale(COND), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(me9)
                        
                        #PLOTS
                        plot(egg.data$TEMP,egg.data$CANN_MEAN)
                        plot(egg.data$DO,egg.data$CANN_MEAN)
                        plot(egg.data$COND,egg.data$CANN_MEAN)
                        
                  #GUARD ------------------------------------------------------------
                        me10<-lme(GUARD_MEAN~scale(TEMP)+scale(DO)+scale(COND), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(me10)
                        
                        #PLOTS
                        plot(egg.data$TEMP,egg.data$GUARD_MEAN)
                        plot(egg.data$DO,egg.data$GUARD_MEAN)
                        plot(egg.data$COND,egg.data$GUARD_MEAN)

                  #ROCKO2 ------------------------------------------------------------
                        me11<-lme(ROCKO2_MEAN~scale(TEMP)+scale(DO)+scale(COND), 
                                  random = ~1|SUBJECT,
                                  data = egg.data)
                        
                        
                        summary(me11)
                        
                        #PLOTS
                        plot(egg.data$TEMP,egg.data$ROCKO2_MEAN)
                        plot(egg.data$DO,egg.data$ROCKO2_MEAN)
                        plot(egg.data$COND,egg.data$ROCKO2_MEAN)
                        
                        
                        
      #BIOLOGICAL (TL, CLUTCH SIZE, MALE_CLASS)-----
                        
            #TEST CORRELATIONS B/T PREDICTORS
            cor(egg.data$TL, egg.data$EGGS_IN_NEST)
                        
              #####GROUPED BEHAVIORS#####
                        
                  #CARE TOTAL--------------------------------------------------------
                        
                        mb1<-lme(CARE_MEAN~scale(TL)+scale(EGGS_IN_NEST)+MALE_CLASS, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(mb1)
                        
                        #PLOTS
                        plot(egg.data$TL,egg.data$CARE_MEAN)
                        plot(egg.data$EGGS_IN_NEST,egg.data$CARE_MEAN)
                        plot(egg.data$MALE_CLASS,egg.data$CARE_MEAN)                         
                        
                  #FAN TOTAL---------------------------------------------------------
                        
                        mb2<-lme(FAN_MEAN~scale(TL)+scale(EGGS_IN_NEST)+MALE_CLASS, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(mb2)
                        
                        #PLOTS
                        plot(egg.data$TL,egg.data$FAN_MEAN)
                        plot(egg.data$EGGS_IN_NEST,egg.data$FAN_MEAN)
                        plot(egg.data$MALE_CLASS,egg.data$FAN_MEAN)  
                        
                  #AGIT TOTAL--------------------------------------------------------
                        
                        mb3<-lme(AGIT_MEAN~scale(TL)+scale(EGGS_IN_NEST)+MALE_CLASS, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(mb3)
                        
                        #PLOTS
                        plot(egg.data$TL,egg.data$AGIT_MEAN)
                        plot(egg.data$EGGS_IN_NEST,egg.data$AGIT_MEAN)
                        plot(egg.data$MALE_CLASS,egg.data$AGIT_MEAN)
                        
              #####INDIVIDUAL BEHAVIORS#####     
                  #TFAN -------------------------------------------------------------
                        
                        mb4<-lme(TFAN_MEAN~scale(TL)+scale(EGGS_IN_NEST)+MALE_CLASS, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(mb4)
                        
                        #PLOTS
                        plot(egg.data$TL,egg.data$TFAN_MEAN)
                        plot(egg.data$EGGS_IN_NEST,egg.data$TFAN_MEAN)
                        plot(egg.data$MALE_CLASS,egg.data$TFAN_MEAN)
                        
                  #ROCKTAIL ---------------------------------------------------------
                        
                        mb5<-lme(ROCKTAIL_MEAN~scale(TL)+scale(EGGS_IN_NEST)+MALE_CLASS, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(mb5)
                        
                        #PLOTS
                        plot(egg.data$TL,egg.data$ROCKTAIL_MEAN)
                        plot(egg.data$EGGS_IN_NEST,egg.data$ROCKTAIL_MEAN)
                        plot(egg.data$MALE_CLASS,egg.data$ROCKTAIL_MEAN)
                        
                  #ROCKAG -----------------------------------------------------------
                        
                        mb6<-lme(ROCKAG_MEAN~scale(TL)+scale(EGGS_IN_NEST)+MALE_CLASS, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(mb6)
                        
                        #PLOTS
                        plot(egg.data$TL,egg.data$ROCKAG_MEAN)
                        plot(egg.data$EGGS_IN_NEST,egg.data$ROCKAG_MEAN)
                        plot(egg.data$MALE_CLASS,egg.data$ROCKAG_MEAN)
                        
                  #WALKAG -----------------------------------------------------------
                        
                        mb7<-lme(WALKAG_MEAN~scale(TL)+scale(EGGS_IN_NEST)+MALE_CLASS, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(mb7)
                        
                        #PLOTS
                        plot(egg.data$TL,egg.data$WALKAG_MEAN)
                        plot(egg.data$EGGS_IN_NEST,egg.data$WALKAG_MEAN)
                        plot(egg.data$MALE_CLASS,egg.data$WALKAG_MEAN)
                        
                  #HEADAG -----------------------------------------------------------
                        
                        mb8<-lme(HEADAG_MEAN~scale(TL)+scale(EGGS_IN_NEST)+MALE_CLASS, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(mb8)
                        
                        #PLOTS
                        plot(egg.data$TL,egg.data$HEADAG_MEAN)
                        plot(egg.data$EGGS_IN_NEST,egg.data$HEADAG_MEAN)
                        plot(egg.data$MALE_CLASS,egg.data$HEADAG_MEAN)
                        
                  #CANN -------------------------------------------------------------
                        
                        mb9<-lme(CANN_MEAN~scale(TL)+scale(EGGS_IN_NEST)+MALE_CLASS, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(mb9)
                        
                        #PLOTS
                        plot(egg.data$TL,egg.data$CANN_MEAN)
                        plot(egg.data$EGGS_IN_NEST,egg.data$CANN_MEAN)
                        plot(egg.data$MALE_CLASS,egg.data$CANN_MEAN)
                        
                  #GUARD ------------------------------------------------------------
                        
                        mb10<-lme(GUARD_MEAN~scale(TL)+scale(EGGS_IN_NEST)+MALE_CLASS, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(mb10)
                        
                        #PLOTS
                        plot(egg.data$TL,egg.data$GUARD_MEAN)
                        plot(egg.data$EGGS_IN_NEST,egg.data$GUARD_MEAN)
                        plot(egg.data$MALE_CLASS,egg.data$GUARD_MEAN)
                        
                        
                  #ROCKO2 ------------------------------------------------------------
                        
                        mb11<-lme(ROCKO2_MEAN~scale(TL)+scale(EGGS_IN_NEST)+MALE_CLASS, 
                                  random = ~1|SUBJECT,
                                  data = egg.data)
                        
                        
                        summary(mb11)
                        
                        #PLOTS
                        plot(egg.data$TL,egg.data$ROCKO2_MEAN)
                        plot(egg.data$EGGS_IN_NEST,egg.data$ROCKO2_MEAN)
                        plot(egg.data$MALE_CLASS,egg.data$ROCKO2_MEAN)      
                        
      #TEMPORAL (DAY_SEP6)-----
                        
              #####GROUPED BEHAVIORS#####
                        
                  #CARE TOTAL--------------------------------------------------------
                        
                        mt1<-lme(CARE_MEAN~scale(DAY_SEP6), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(mt1)
                        
                        #PLOTS
                        plot(egg.data$DAY_SEP6,egg.data$CARE_MEAN)             
                        
                        
                  #FAN TOTAL---------------------------------------------------------
                        
                        mt2<-lme(FAN_MEAN~scale(DAY_SEP6), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(mt2)
                        
                        #PLOTS
                        plot(egg.data$DAY_SEP6,egg.data$FAN_MEAN) 
                        
                  #AGIT TOTAL--------------------------------------------------------
                        
                        mt3<-lme(AGIT_MEAN~scale(DAY_SEP6), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(mt3)
                        
                        #PLOTS
                        plot(egg.data$DAY_SEP6,egg.data$AGIT_MEAN) 
                        
              #####INDIVIDUAL BEHAVIORS#####     
                  #TFAN -------------------------------------------------------------
                        
                        mt4<-lme(TFAN_MEAN~scale(DAY_SEP6), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(mt4)
                        
                        #PLOTS
                        plot(egg.data$DAY_SEP6,egg.data$TFAN_MEAN)
                        
                  #ROCKTAIL ---------------------------------------------------------
                        
                        mt5<-lme(ROCKTAIL_MEAN~scale(DAY_SEP6), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(mt5)
                        
                        #PLOTS
                        plot(egg.data$DAY_SEP6,egg.data$ROCKTAIL_MEAN)
                        
                  #ROCKAG -----------------------------------------------------------
                        
                        mt6<-lme(ROCKAG_MEAN~scale(DAY_SEP6), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(mt6)
                        
                        #PLOTS
                        plot(egg.data$DAY_SEP6,egg.data$ROCKAG_MEAN)
                        
                  #WALKAG -----------------------------------------------------------
                        
                        mt7<-lme(WALKAG_MEAN~scale(DAY_SEP6), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(mt7)
                        
                        #PLOTS
                        plot(egg.data$DAY_SEP6,egg.data$WALKAG_MEAN)
                        
                  #HEADAG -----------------------------------------------------------
                        
                        mt8<-lme(HEADAG_MEAN~scale(DAY_SEP6), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(mt8)
                        
                        #PLOTS
                        plot(egg.data$DAY_SEP6,egg.data$HEADAG_MEAN)
                        
                  #CANN -------------------------------------------------------------
                        
                        mt9<-lme(CANN_MEAN~scale(DAY_SEP6), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(mt9)
                        
                        #PLOTS
                        plot(egg.data$DAY_SEP6,egg.data$CANN_MEAN)
                        
                  #GUARD ------------------------------------------------------------
                        mt10<-lme(GUARD_MEAN~scale(DAY_SEP6), 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(mt10)
                        
                        #PLOTS
                        plot(egg.data$DAY_SEP6,egg.data$GUARD_MEAN)
                        
                  #GUARD ------------------------------------------------------------
                        mt11<-lme(ROCKO2_MEAN~scale(DAY_SEP6), 
                                  random = ~1|SUBJECT,
                                  data = egg.data)
                        
                        
                        summary(mt11)
                        
                        #PLOTS
                        plot(egg.data$DAY_SEP6,egg.data$ROCKO2_MEAN)    
                        
#PROPORTION OF TIME SPENT IN BEHAVIORS-------------------------------------------------------------------------------------------------------------
                        
      #ENVIRONMENTAL (TEMP,DO,COND)-----
                        
                #TEST CORRELATIONS B/T PREDICTORS
                cor(egg.data$DO, egg.data$TEMP)
                cor(egg.data$DO, egg.data$COND)
                cor(egg.data$TEMP, egg.data$COND)
                
                egg.data.num <- egg.data[ c(68:71)]
                
                rcorr(as.matrix(egg.data.num), type="pearson") 
      
 
            #####GROUPED BEHAVIORS#####
                        
                  #CARE TOTAL--------------------------------------------------------
                        
                        pe1<-glmer(CARE_PROP~scale(TEMP)+scale(DO)+scale(COND)+
                                 (1|SUBJECT),
                                 weights = TOT_LENG_S,
                                 family = "binomial",
                                 data = egg.data)
                        
                        
                        summary(pe1)
                        
                        
                        #PLOTS
                        plot(egg.data$TEMP,egg.data$CARE_PROP)
                        plot(egg.data$DO,egg.data$CARE_PROP)
                        plot(egg.data$COND,egg.data$CARE_PROP) 
                    
                       
                  #FAN TOTAL---------------------------------------------------------
                  
                        pe2<-glmer(FAN_PROP~scale(TEMP)+scale(DO)+scale(COND)+ 
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pe2)
                        
                        #PLOTS
                        plot(egg.data$TEMP,egg.data$FAN_PROP)
                        plot(egg.data$DO,egg.data$FAN_PROP)
                        plot(egg.data$COND,egg.data$FAN_PROP)   
                        
                
                  #AGIT TOTAL--------------------------------------------------------
                  
                        pe3<-glmer(AGIT_PROP~scale(TEMP)+scale(DO)+scale(COND)+ 
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pe3)

                        
                        #PLOTS
                        plot(egg.data$TEMP,egg.data$AGIT_PROP)
                        plot(egg.data$DO,egg.data$AGIT_PROP)
                        plot(egg.data$COND,egg.data$AGIT_PROP) 
                        
              
            #####INDIVIDUAL BEHAVIORS#####     
                  #TFAN -------------------------------------------------------------
                        
                        pe4<-glmer(TFAN_PROP~scale(TEMP)+scale(DO)+scale(COND)+ 
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pe4)
                        
                        #PLOTS
                        plot(egg.data$TEMP,egg.data$TFAN_PROP)
                        plot(egg.data$DO,egg.data$TFAN_PROP)
                        plot(egg.data$COND,egg.data$TFAN_PROP) 
                       
                        
                  #ROCKTAIL ---------------------------------------------------------
                        
                        pe5<-glmer(ROCKTAIL_PROP~scale(TEMP)+scale(DO)+scale(COND)+ 
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pe5)
                        
                        #PLOTS
                        plot(egg.data$TEMP,egg.data$ROCKTAIL_PROP)
                        plot(egg.data$DO,egg.data$ROCKTAIL_PROP)
                        plot(egg.data$COND,egg.data$ROCKTAIL_PROP) 
                        
                        
                       
                  #ROCKAG -----------------------------------------------------------
                        
                        pe6<-glmer(ROCKAG_PROP~scale(TEMP)+scale(DO)+scale(COND)+ 
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pe6)
                        
                        #PLOTS
                        plot(egg.data$TEMP,egg.data$ROCKAG_PROP)
                        plot(egg.data$DO,egg.data$ROCKAG_PROP)
                        plot(egg.data$COND,egg.data$ROCKAG_PROP) 
                        
                       
                  #WALKAG -----------------------------------------------------------
                        
                        pe7<-glmer(WALKAG_PROP~scale(TEMP)+scale(DO)+scale(COND)+ 
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pe7)
                       
                        #PLOTS
                        plot(egg.data$TEMP,egg.data$WALKAG_PROP)
                        plot(egg.data$DO,egg.data$WALKAG_PROP)
                        plot(egg.data$COND,egg.data$WALKAG_PROP) 
                        
                        
                  #HEADAG -----------------------------------------------------------
                        
                        pe8<-glmer(HEADAG_PROP~scale(TEMP)+scale(DO)+scale(COND)+ 
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pe8)
                        
                        #PLOTS
                        plot(egg.data$TEMP,egg.data$HEADAG_PROP)
                        plot(egg.data$DO,egg.data$HEADAG_PROP)
                        plot(egg.data$COND,egg.data$HEADAG_PROP)
                        
                       
                  #CANN -------------------------------------------------------------
                        
                        pe9<-glmer(CANN_PROP~scale(TEMP)+scale(DO)+scale(COND)+ 
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pe9)
                        
                        #PLOTS
                        plot(egg.data$TEMP,egg.data$CANN_PROP)
                        plot(egg.data$DO,egg.data$CANN_PROP)
                        plot(egg.data$COND,egg.data$CANN_PROP)
                        
                       
                  #GUARD ------------------------------------------------------------
                  
                        pe10<-glmer(GUARD_PROP~scale(TEMP)+scale(DO)+scale(COND)+ 
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pe10)
                        
                        #PLOTS
                        plot(egg.data$TEMP,egg.data$GUARD_PROP)
                        plot(egg.data$DO,egg.data$GUARD_PROP)
                        plot(egg.data$COND,egg.data$GUARD_PROP)
                        
                        
                  #ROCKO2 ------------------------------------------------------------
                        
                        pe11<-glmer(ROCKO2_PROP~scale(TEMP)+scale(DO)+scale(COND)+ 
                                      (1|SUBJECT),
                                    weights = TOT_LENG_S,
                                    family = "binomial",
                                    data = egg.data)
                        
                        
                        summary(pe11)
                        
                        #PLOTS
                        plot(egg.data$TEMP,egg.data$ROCKO2_PROP)
                        plot(egg.data$DO,egg.data$ROCKO2_PROP)
                        plot(egg.data$COND,egg.data$ROCKO2_PROP)      
                     
    #BIOLOGICAL-----
                        
          #TEST CORRELATIONS B/T PREDICTORS
          cor(egg.data$TL, egg.data$EGGS_IN_NEST)

                        
          
            #####GROUPED BEHAVIORS#####
                        
                  #CARE TOTAL--------------------------------------------------------
                        
                        pb1<-glmer(CARE_PROP~scale(TL)+scale(EGGS_IN_NEST)+ MALE_CLASS+
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pb1)
                        
                        #PLOTS
                        plot(egg.data$TL,egg.data$CARE_PROP)
                        plot(egg.data$EGGS_IN_NEST,egg.data$CARE_PROP)
                        plot(egg.data$MALE_CLASS,egg.data$CARE_PROP)                 
                        
                        
                        qplot(TL, CARE_PROP, data=egg.data, colour=FATE)
                        qplot(EGGS_IN_NEST, CARE_PROP, data=egg.data, colour=FATE)

                    #MEAN + SE PLOT
                        pb1.ss<-describeBy(egg.data$CARE_PROP, group=egg.data$MALE_CLASS, mat = TRUE) 
                        pb1.ss 
                        
                        pb1.plot<-ggplot(pb1.ss, aes(x = group1, y= mean))+
                          theme_classic()+
                          geom_point(size = 3)+
                          geom_errorbar(aes(ymin=mean-se, ymax=mean+se), linewidth = 0.5, width=.1)+
                          labs(y="CARE_PROP", x= "Male Size Class")
                        
                        pb1.plot
                        
                  #FAN TOTAL---------------------------------------------------------
                        
                        pb2<-glmer(FAN_PROP~scale(TL)+scale(EGGS_IN_NEST)+ MALE_CLASS+
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pb2)
                        
                        #PLOTS
                        plot(egg.data$TL,egg.data$FAN_PROP)
                        plot(egg.data$EGGS_IN_NEST,egg.data$FAN_PROP)
                        plot(egg.data$MALE_CLASS,egg.data$FAN_PROP) 
                        
                        
                        qplot(TL, FAN_PROP, data=egg.data, colour=FATE)
                        qplot(EGGS_IN_NEST, FAN_PROP, data=egg.data, colour=FATE)
                        
                        
                    #MEAN + SE PLOT
                        pb2.ss<-describeBy(egg.data$FAN_PROP, group=egg.data$MALE_CLASS, mat = TRUE) 
                        pb2.ss 
                        
                        pb2.plot<-ggplot(pb2.ss, aes(x = group1, y= mean))+
                          theme_classic()+
                          geom_point(size = 3)+
                          geom_errorbar(aes(ymin=mean-se, ymax=mean+se), linewidth = 0.5, width=.1)+
                          labs(y="FAN_PROP", x= "Male Size Class")
                        
                        pb2.plot
                        
                  #AGIT TOTAL--------------------------------------------------------
                        
                        pb3<-glmer(AGIT_PROP~scale(TL)+scale(EGGS_IN_NEST)+ MALE_CLASS+
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pb3)
                        
                        #PLOTS
                        plot(egg.data$TL,egg.data$AGIT_PROP)
                        plot(egg.data$EGGS_IN_NEST,egg.data$AGIT_PROP)
                        plot(egg.data$MALE_CLASS,egg.data$AGIT_PROP) 
                        
                        
                        qplot(TL, AGIT_PROP, data=egg.data, colour=FATE)
                        qplot(EGGS_IN_NEST, AGIT_PROP, data=egg.data, colour=FATE)
                        
                        
            #####INDIVIDUAL BEHAVIORS#####     
                  #TFAN -------------------------------------------------------------
                        
                        pb4<-glmer(TFAN_PROP~scale(TL)+scale(EGGS_IN_NEST)+ MALE_CLASS+
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pb4)
                        
                        #PLOTS
                        plot(egg.data$TL,egg.data$TFAN_PROP)
                        plot(egg.data$EGGS_IN_NEST,egg.data$TFAN_PROP)
                        plot(egg.data$MALE_CLASS,egg.data$TFAN_PROP) 
                        
                        qplot(TL, TFAN_PROP, data=egg.data, colour=FATE)
                        qplot(EGGS_IN_NEST, TFAN_PROP, data=egg.data, colour=FATE)
                        
                  #ROCKTAIL ---------------------------------------------------------
                        
                        pb5<-glmer(ROCKTAIL_PROP~scale(TL)+scale(EGGS_IN_NEST)+ MALE_CLASS+
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pb5)
                        
                        #PLOTS
                        plot(egg.data$TL,egg.data$ROCKTAIL_PROP)
                        plot(egg.data$EGGS_IN_NEST,egg.data$ROCKTAIL_PROP)
                        plot(egg.data$MALE_CLASS,egg.data$ROCKTAIL_PROP) 
                        
                        qplot(TL, AGIT_PROP, data=egg.data, colour=FATE)
                        qplot(EGGS_IN_NEST, AGIT_PROP, data=egg.data, colour=FATE)
                        
                    #MEAN + SE PLOT
                        pb5.ss<-describeBy(egg.data$ROCKTAIL_PROP, group=egg.data$MALE_CLASS, mat = TRUE) 
                        pb5.ss 
                        
                        pb5.plot<-ggplot(pb5.ss, aes(x = group1, y= mean))+
                          theme_classic()+
                          geom_point(size = 3)+
                          geom_errorbar(aes(ymin=mean-se, ymax=mean+se), linewidth = 0.5, width=.1)+
                          labs(y="ROCKTAIL_PROP", x= "Male Size Class")
                        
                        pb5.plot  
                        
                      
                  #ROCKAG -----------------------------------------------------------
                        
                        pb6<-glmer(ROCKAG_PROP~scale(TL)+scale(EGGS_IN_NEST)+ MALE_CLASS+
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pb6)
                        
                        #PLOTS
                        plot(egg.data$TL,egg.data$ROCKAG_PROP)
                        plot(egg.data$EGGS_IN_NEST,egg.data$ROCKAG_PROP)
                        plot(egg.data$MALE_CLASS,egg.data$ROCKAG_PROP)
                        
                        qplot(TL, ROCKAG_PROP, data=egg.data, colour=FATE)
                        qplot(EGGS_IN_NEST, ROCKAG_PROP, data=egg.data, colour=FATE)
                        
                  #WALKAG -----------------------------------------------------------
                        
                        pb7<-glmer(WALKAG_PROP~scale(TL)+scale(EGGS_IN_NEST)+ MALE_CLASS+
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pb7)
                        
                        #PLOTS
                        plot(egg.data$TL,egg.data$WALKAG_PROP)
                        plot(egg.data$EGGS_IN_NEST,egg.data$WALKAG_PROP)
                        plot(egg.data$MALE_CLASS,egg.data$WALKAG_PROP)
                        
                        qplot(TL, WALKAG_PROP, data=egg.data, colour=FATE)
                        qplot(EGGS_IN_NEST, WALKAG_PROP, data=egg.data, colour=FATE)
                        
                  #HEADAG -----------------------------------------------------------
                        
                        pb8<-glmer(HEADAG_PROP~scale(TL)+scale(EGGS_IN_NEST)+ MALE_CLASS+
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pb8)
                        
                        #PLOTS
                        plot(egg.data$TL,egg.data$HEADAG_PROP)
                        plot(egg.data$EGGS_IN_NEST,egg.data$HEADAG_PROP)
                        plot(egg.data$MALE_CLASS,egg.data$HEADAG_PROP)
                        
                        qplot(TL, HEADAG_PROP, data=egg.data, colour=FATE)
                        qplot(EGGS_IN_NEST, HEADAG_PROP, data=egg.data, colour=FATE)
                        
                  #CANN -------------------------------------------------------------
                        
                        pb9<-glmer(CANN_PROP~scale(TL)+scale(EGGS_IN_NEST)+ MALE_CLASS+
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pb9)
                        
                        #PLOTS
                        plot(egg.data$TL,egg.data$CANN_PROP)
                        plot(egg.data$EGGS_IN_NEST,egg.data$CANN_PROP)
                        plot(egg.data$MALE_CLASS,egg.data$CANN_PROP)
                        
                        qplot(TL, CANN_PROP, data=egg.data, colour=FATE)
                        qplot(EGGS_IN_NEST, CANN_PROP, data=egg.data, colour=FATE)
                        
                  #GUARD ------------------------------------------------------------
                        
                        pb10<-glmer(GUARD_PROP~scale(TL)+scale(EGGS_IN_NEST)+ MALE_CLASS+
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pb10)
                        
                        #PLOTS
                        plot(egg.data$TL,egg.data$GUARD_PROP)
                        plot(egg.data$EGGS_IN_NEST,egg.data$GUARD_PROP)
                        plot(egg.data$MALE_CLASS,egg.data$GUARD_PROP)
                        
                        qplot(TL, GUARD_PROP, data=egg.data, colour=FATE)
                        qplot(EGGS_IN_NEST, GUARD_PROP, data=egg.data, colour=FATE)
  
                  #ROCKO2 ------------------------------------------------------------
                        
                        pb11<-glmer(ROCKO2_PROP~scale(TL)+scale(EGGS_IN_NEST)+ MALE_CLASS+
                                      (1|SUBJECT),
                                    weights = TOT_LENG_S,
                                    family = "binomial",
                                    data = egg.data)
                        
                        
                        summary(pb11)
                        
                        #PLOTS
                        plot(egg.data$TL,egg.data$ROCKO2_PROP)
                        plot(egg.data$EGGS_IN_NEST,egg.data$ROCKO2_PROP)
                        plot(egg.data$MALE_CLASS,egg.data$ROCKO2_PROP) 
                        
                        qplot(TL, AGIT_PROP, data=egg.data, colour=FATE)
                        qplot(EGGS_IN_NEST, AGIT_PROP, data=egg.data, colour=FATE)
                        
                        #MEAN + SE PLOT
                        pb11.ss<-describeBy(egg.data$ROCKO2_PROP, group=egg.data$MALE_CLASS, mat = TRUE) 
                        pb11.ss 
                        
                        pb11.plot<-ggplot(pb11.ss, aes(x = group1, y= mean))+
                          theme_classic()+
                          geom_point(size = 3)+
                          geom_errorbar(aes(ymin=mean-se, ymax=mean+se), linewidth = 0.5, width=.1)+
                          labs(y="ROCKO2_PROP", x= "Male Size Class")
                        
                        pb11.plot  
                        
    #TEMPORAL-----
                        
          #####GROUPED BEHAVIORS#####
                        
                  #CARE TOTAL--------------------------------------------------------
                        
                        pt1<-glmer(CARE_PROP~scale(DAY_SEP6)+
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pt1)
                        
                        #PLOTS
                        plot(egg.data$DAY_SEP6,egg.data$CARE_PROP)
                        
                        qplot(DAY_SEP6, CARE_PROP, data=egg.data, colour=FATE)
                        
                  #FAN TOTAL---------------------------------------------------------
                        
                        pt2<-glmer(FAN_PROP~scale(DAY_SEP6)+
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pt2)
                        
                        #PLOTS
                        plot(egg.data$DAY_SEP6,egg.data$FAN_PROP)
                        
                        qplot(DAY_SEP6, FAN_PROP, data=egg.data, colour=FATE)
                        
                        
                  #AGIT TOTAL--------------------------------------------------------
                        
                        pt3<-glmer(AGIT_PROP~scale(DAY_SEP6)+
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pt3)
                        
                        #PLOTS
                        plot(egg.data$DAY_SEP6,egg.data$AGIT_PROP)
                        
                        qplot(DAY_SEP6, AGIT_PROP, data=egg.data, colour=FATE)
                        
                        
          #####INDIVIDUAL BEHAVIORS#####     
                  #TFAN -------------------------------------------------------------
                        
                        pt4<-glmer(TFAN_PROP~scale(DAY_SEP6)+
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pt4)
                        
                        #PLOTS
                        plot(egg.data$DAY_SEP6,egg.data$TFAN_PROP)
                        
                        qplot(DAY_SEP6, TFAN_PROP, data=egg.data, colour=FATE)
                        
                        
                  #ROCKTAIL ---------------------------------------------------------
                        
                        pt5<-glmer(ROCKTAIL_PROP~scale(DAY_SEP6)+
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pt5)
                        
                        #PLOTS
                        plot(egg.data$DAY_SEP6,egg.data$ROCKTAIL_PROP)
                        
                        qplot(DAY_SEP6, ROCKTAIL_PROP, data=egg.data, colour=FATE)
                        
                        
                  #ROCKAG -----------------------------------------------------------
                        
                        pt6<-glmer(ROCKAG_PROP~scale(DAY_SEP6)+
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pt6)
                        
                        #PLOTS
                        plot(egg.data$DAY_SEP6,egg.data$ROCKAG_PROP)
                        
                        qplot(DAY_SEP6, ROCKAG_PROP, data=egg.data, colour=FATE)
                        
                        
                  #WALKAG -----------------------------------------------------------
                        
                        pt7<-glmer(WALKAG_PROP~scale(DAY_SEP6)+
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pt7)
                        
                        #PLOTS
                        plot(egg.data$DAY_SEP6,egg.data$WALKAG_PROP)
                        
                        qplot(DAY_SEP6, WALKAG_PROP, data=egg.data, colour=FATE)
                        
                        
                  #HEADAG -----------------------------------------------------------
                        
                        pt8<-glmer(HEADAG_PROP~scale(DAY_SEP6)+
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pt8)
                        
                        #PLOTS
                        plot(egg.data$DAY_SEP6,egg.data$HEADAG_PROP)
                        
                        qplot(DAY_SEP6, HEADAG_PROP, data=egg.data, colour=FATE)
                        
                        
                  #CANN -------------------------------------------------------------
                        
                        pt9<-glmer(CANN_PROP~scale(DAY_SEP6)+
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pt9)
                        
                        #PLOTS
                        plot(egg.data$DAY_SEP6,egg.data$CANN_PROP)
                        
                        qplot(DAY_SEP6, CANN_PROP, data=egg.data, colour=FATE)
                        
                        
                  #GUARD ------------------------------------------------------------
                        pt10<-glmer(GUARD_PROP~scale(DAY_SEP6)+
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pt10)
                        
                        #PLOTS
                        plot(egg.data$DAY_SEP6,egg.data$GUARD_PROP)
                        
                        qplot(DAY_SEP6, GUARD_PROP, data=egg.data, colour=FATE)
                        
                        
                  #ROCKO2 ------------------------------------------------------------
                        pt11<-glmer(ROCKO2_PROP~scale(DAY_SEP6)+
                                      (1|SUBJECT),
                                    weights = TOT_LENG_S,
                                    family = "binomial",
                                    data = egg.data)
                        
                        
                        summary(pt11)
                        
                        #PLOTS
                        plot(egg.data$DAY_SEP6,egg.data$ROCKO2_PROP) 
                        
                        qplot(DAY_SEP6, ROCKO2_PROP, data=egg.data, colour=FATE)
                        
                  
#AIC MODEL SELECTION------------------
     
                        
                        
        #PROP               
              #####GROUPED BEHAVIORS#####
              #CARE TOTAL--------------------------------------------------------
              model.sel(pe1,pb1,pt1)    
              #FAN TOTAL---------------------------------------------------------
              model.sel(pe2,pb2,pt2) 
              #AGIT TOTAL--------------------------------------------------------
              model.sel(pe3,pb3,pt3) 
              #####INDIVIDUAL BEHAVIORS#####     
              #TFAN -------------------------------------------------------------
              model.sel(pe4,pb4,pt4) 
              #ROCKTAIL ---------------------------------------------------------
              model.sel(pe5,pb5,pt5) 
              #ROCKAG -----------------------------------------------------------
              model.sel(pe6,pb6,pt6) 
              #WALKAG -----------------------------------------------------------
              model.sel(pe7,pb7,pt7) 
              #HEADAG -----------------------------------------------------------
              model.sel(pe8,pb8,pt8) 
              #CANN -------------------------------------------------------------
              model.sel(pe9,pb9,pt9) 
              #GUARD ----------------------------         
              model.sel(pe10,pb10,pt10) 
              #ROCKO2 ----------------------------         
              model.sel(pe11,pb11,pt11)                 
                        
              
                        
      #FREQ
          #####GROUPED BEHAVIORS#####
              #CARE TOTAL--------------------------------------------------------
                        model.sel(fe1,fb1,ft1)    
              #FAN TOTAL---------------------------------------------------------
                        model.sel(fe2,fb2,ft2) 
              #AGIT TOTAL--------------------------------------------------------
                        model.sel(fe3,fb3,ft3) 
          #####INDIVIDUAL BEHAVIORS#####     
              #TFAN -------------------------------------------------------------
                        model.sel(fe4,fb4,ft4) 
              #ROCKTAIL ---------------------------------------------------------
                        model.sel(fe5,fb5,ft5) 
              #ROCKAG -----------------------------------------------------------
                        model.sel(fe6,fb6,ft6) 
              #WALKAG -----------------------------------------------------------
                        model.sel(fe7,fb7,ft7) 
              #HEADAG -----------------------------------------------------------
                        model.sel(fe8,fb8,ft8) 
              #CANN -------------------------------------------------------------
                        model.sel(fe9,fb9,ft9) 
              #GUARD ----------------------------         
                        model.sel(fe10,fb10,ft10) 
              #ROCKO2 ----------------------------         
                        model.sel(fe11,fb11,ft11)          
                        
                        
                        
        #MEAN     
              #####GROUPED BEHAVIORS#####
              #CARE TOTAL--------------------------------------------------------
              model.sel(me1,mb1,mt1)    
              #FAN TOTAL---------------------------------------------------------
              model.sel(me2,mb2,mt2) 
              #AGIT TOTAL--------------------------------------------------------
              model.sel(me3,mb3,mt3) 
              #####INDIVIDUAL BEHAVIORS#####     
              #TFAN -------------------------------------------------------------
              model.sel(me4,mb4,mt4) 
              #ROCKTAIL ---------------------------------------------------------
              model.sel(me5,mb5,mt5) 
              #ROCKAG -----------------------------------------------------------
              model.sel(me6,mb6,mt6) 
              #WALKAG -----------------------------------------------------------
              model.sel(me7,mb7,mt7) 
              #HEADAG -----------------------------------------------------------
              model.sel(me8,mb8,mt8) 
              #CANN -------------------------------------------------------------
              model.sel(me9,mb9,mt9) 
              #GUARD ----------------------------         
              model.sel(me10,mb10,mt10) 
              #ROCKO2 ----------------------------         
              model.sel(me11,mb11,mt11)
                        
#R^2 VALUES of Top Models (PROP)---------------------------------------------------------
    #marginal = variation of fixed effects
    #conditional = variation of fixed + random
        
        performance(mb11)
        performance(mt11)
        performance(me11)
        

#ODDS-RATIO PLOTS-----------
     library(ggeffects)
     library(questionr)
     library(epiR)
     
     #INTERPRETING ODDS RATIOS
     #UNSCALED VALUES IN PLOTS CORRECT???    
     #TRANSFORMED OR NON-TRANSFORMED VALUES IN PLOT? (transformed = ODDS RATIO; non = LOG ODDS?)
            #transform = NULL --> regression coefficients from table displayed on plot
          
     
     set_theme(base = theme_classic(), #To remove the background color and the grids
               theme.font = 'serif',   #To change the font type
               axis.title.size = 2.5,  #To change axis title size
               axis.textsize.x = 1.2,  #To change x axis text size
               axis.textsize.y = 2)  #To change y axis text size

    
    #TOTAL CARE, TOTAL FAN, TOTAL AGIT--------
         x1<-plot_models(pe1, pe2, pe3,
                      axis.labels = c("Conductivity (µS/cm)",
                                      "DO (mg/L)",
                                      "Temperature (°C)"),
                      m.labels = c("Total Care", "Total Fanning", "Total Agitation"),
                      vline.color = "gray70",
                      show.values = TRUE,
                      value.size = 7,
                      axis.title = "Odds Ratio",
                      axis.lim = c(0.5,5),
                      dot.size = 3,
                      line.size = 1.5,
                      spacing = 0.75,
                      show.p = TRUE,
                      ci.lvl = 0.95,
                      colors = c("gray60","gray40","black"))

     #CHANGE LEGEND TITLE
     plot(x1)+theme(legend.key.size = unit(1, 'cm'),
                     legend.title = element_blank(),
                     legend.text = element_text(size = 20))
     
     
     #Interpret - 1 unit standard deviation in X results in ODDS-RATIO value-fold increase in the probability of Y
     
           sd(egg.data$TEMP)
           sd(egg.data$DO)
           sd(egg.data$COND)
           sd(egg.data$DAY_SEP6)
           
           #e.g.; - 1 unit standard deviation (~83) increase in conductity, results in 4.43 fold increase in the probability of fanning

     #ERROR BARS??????
     #width, alpha, and scale
     #Passed down to geom_errorbar() or geom_density_ridges(), for forest or diagnostic plots
     
     
         #TOTAL FAN, TFAN, ROCKTAIL--------
        
         x2<-plot_models(pe2, pe4, pe5,
                     axis.labels = c("Conductivity (µS/cm)",
                                     "DO (mg/L)",
                                     "Temperature (°C)"),
                     m.labels = c("Total Fanning", "Tail Fanning", "Rock/Tail"),
                     vline.color = "gray70",
                     show.values = TRUE,
                     value.size = 7,
                     axis.title = "Odds Ratio",
                     axis.lim = c(0.5,5),
                     dot.size = 3,
                     line.size = 2,
                     spacing = 0.75,
                     show.p = TRUE,
                     ci.lvl = 0.95,
                     colors = c("gray60","gray40","black"))
         
           x2
           
           plot(x2)+theme(legend.key.size = unit(1, 'cm'),
                          legend.title = element_blank(),
                          legend.text = element_text(size = 20))
           
        #TOTAL AGIT, ROCKAG, WALKAG, HEADAG--------
       
        x3<-plot_models(pe3, pe6, pe7, pe8,
                   axis.labels = c("Conductivity (µS/cm)",
                                   "DO (mg/L)",
                                   "Temperature (°C)"),
                   m.labels = c("Total Agitation", "Rock Agitation", "Walking Agitation","Head Agitation"),
                   vline.color = "gray70",
                   show.values = TRUE,
                   value.size = 5.5,
                   axis.title = "Odds Ratio",
                   axis.lim = c(0.5,5),
                   dot.size = 3,
                   line.size = 1.5,
                   spacing = 0.7,
                   show.p = TRUE,
                   ci.lvl = 0.95,
                   colors = c("gray70","gray60","gray40","black"))
           
           x3
           
           plot(x3)+theme(legend.key.size = unit(1, 'cm'),
                          legend.title = element_blank(),
                          legend.text = element_text(size = 20))
           

       #TAILFAN, ROCKTAIL, ROCKO2--------
       
       plot_models(pe4, pe5, pe11,
                   axis.labels = c("Conductivity (µS/cm)",
                                   "DO (mg/L)",
                                   "Temperature (°C)"),
                   m.labels = c("Tail Fanning", "Rock/Tail", "Rock O2"),
                   vline.color = "gray70",
                   show.values = TRUE,
                   axis.title = "Odds Ratio",
                   axis.lim = c(0.1,5),
                   dot.size = 3,
                   line.size = 1.5,
                   spacing = 0.6,
                   show.p = TRUE,
                   ci.lvl = 0.95,
                   colors = c("gray60","gray40","black"))
         
         
         
         
     
     #INDIVIDUAL ODDS RATIO PLOTS==================================
      
          #CARE TOTAL--------
        
          plot_model(pe1, sort.est = FALSE, vline.color = "red",show.values = TRUE, value.offset = .3,
                     title = "Proportion of Time in Care Behaviors", axis.title = "Odds Ratios")

     
          #ODDS RATIO (TRANSFORMED DATA)
           plot_model(pe1, 
                      sort.est = FALSE, 
                      vline.color = "red",
                      show.values = TRUE, 
                      value.offset = .3,
                      title = "Total Fanning", 
                      axis.title = "Odds Ratio",
                      axis.labels = c("Conductivity (µS/cm)",
                                      "DO (mg/L)",
                                      "Temperature (°C)"),
                      colors = "bw",
                      dot.size = 3,
                      line.size = 0.5,
                      width = 0.2,
                      scale = 1.5,
                      bpe.style = "line",
                      bpe.color = "white")
           
           
          
        #EXTRACT CIs for ODDS RATIO
           se <- sqrt(diag(vcov(pe1)))
           se
          
           # table of estimates with 95% CI
           (tab <- cbind(Est = fixef(pe1), LL = fixef(pe1) - 1.96 * se, UL = fixef(pe1) + 1.96 *
                           se))
           
           exp(tab)
           
           
          #FAN TOTAL----
           
          plot_model(pe2, sort.est = FALSE, vline.color = "red",show.values = TRUE, value.offset = .3,
                     title = "Proportion of Time Fanning", axis.title = "Odds Ratios")
          
          plot_model(pe2, 
                     sort.est = FALSE, 
                     vline.color = "red",
                     show.values = TRUE, 
                     value.offset = .3,
                     title = "Total Fanning", 
                     axis.title = "Odds Ratio",
                     axis.labels = c("Conductivity (µS/cm)",
                                     "DO (mg/L)",
                                    "Temperature (°C)"),
                     colors = "bw",
                     dot.size = 3,
                     line.size = 0.5,
                     width = 0.2,
                     scale = 1.5,
                     bpe.style = "line",
                     bpe.color = "white")
          
          #EXTRACT CIs for ODDS RATIO
          se <- sqrt(diag(vcov(pe2)))
          se
          
          # table of estimates with 95% CI
          (tab <- cbind(Est = fixef(pe2), LL = fixef(pe2) - 1.96 * se, UL = fixef(pe2) + 1.96 *
                          se))
          
          exp(tab)

          #AGIT TOTAL--------------------------------------------------------
          plot_model(pe3, sort.est = FALSE, vline.color = "red",show.values = TRUE, value.offset = .3,
                     title = "Proportion of Time Agitating", axis.title = "Odds Ratios")
          
          plot_model(pe3, 
                     sort.est = FALSE, 
                     vline.color = "red",
                     show.values = TRUE, 
                     value.offset = .3,
                     title = "Total Agitation", 
                     axis.title = "Odds Ratio",
                     axis.labels = c("Conductivity (µS/cm)",
                                     "DO (mg/L)",
                                     "Temperature (°C)"),
                     colors = "bw",
                     dot.size = 3,
                     line.size = 0.5,
                     width = 0.2,
                     scale = 1.5,
                     bpe.style = "line",
                     bpe.color = "white")
          
          se <- sqrt(diag(vcov(pe3)))
          se
          
          # table of estimates with 95% CI
          (tab <- cbind(Est = fixef(pe3), LL = fixef(pe3) - 1.96 * se, UL = fixef(pe3) + 1.96 *
                          se))
          
          exp(tab)
          
          #####INDIVIDUAL BEHAVIORS#####     
          #TFAN -------------------------------------------------------------
          plot_model(pe4, sort.est = FALSE, vline.color = "red",show.values = TRUE, value.offset = .3,
                     title = "Proportion of Time Tail Fanning", axis.title = "Odds Ratios")
          
          plot_model(pe4, 
                     sort.est = FALSE, 
                     vline.color = "red",
                     show.values = TRUE, 
                     value.offset = .3,
                     title = "Tail Fanning", 
                     axis.title = "Odds Ratio",
                     axis.labels = c("Conductivity (µS/cm)",
                                     "DO (mg/L)",
                                     "Temperature (°C)"),
                     colors = "bw",
                     dot.size = 3,
                     line.size = 0.5,
                     width = 0.2,
                     scale = 1.5,
                     bpe.style = "line",
                     bpe.color = "white")
          
          #EXTRACT CIs for ODDS RATIO
          se <- sqrt(diag(vcov(pe4)))
          se
          
          # table of estimates with 95% CI
          (tab <- cbind(Est = fixef(pe4), LL = fixef(pe4) - 1.96 * se, UL = fixef(pe4) + 1.96 *
                          se))
          
          exp(tab)
          
          #ROCKTAIL ---------------------------------------------------------
          plot_model(pe5, sort.est = FALSE, vline.color = "red",show.values = TRUE, value.offset = .3,
                     title = "Proportion of Time Rock/Tail", axis.title = "Odds Ratios")
          
          plot_model(pe5, 
                     sort.est = FALSE, 
                     vline.color = "red",
                     show.values = TRUE, 
                     value.offset = .3,
                     title = "Rock/Tail", 
                     axis.title = "Odds Ratio",
                     axis.labels = c("Conductivity (µS/cm)",
                                     "DO (mg/L)",
                                     "Temperature (°C)"),
                     colors = "bw",
                     dot.size = 3,
                     line.size = 0.5,
                     width = 0.2,
                     scale = 1.5,
                     bpe.style = "line",
                     bpe.color = "white")
          
          
          #EXTRACT CIs for ODDS RATIO
          se <- sqrt(diag(vcov(pe5)))
          se
          
          # table of estimates with 95% CI
          (tab <- cbind(Est = fixef(pe5), LL = fixef(pe5) - 1.96 * se, UL = fixef(pe5) + 1.96 *
                          se))
          
          exp(tab)
          
          #ROCKAG -----------------------------------------------------------
          plot_model(pe6, sort.est = FALSE, vline.color = "red",show.values = TRUE, value.offset = .3,
                     title = "Proportion of Time Rock Agitation", axis.title = "Odds Ratios")
          
          plot_model(pe6, 
                     sort.est = FALSE, 
                     vline.color = "red",
                     show.values = TRUE, 
                     value.offset = .3,
                     title = "Rock Agitation", 
                     axis.title = "Odds Ratio",
                     axis.labels = c("Conductivity (µS/cm)",
                                     "DO (mg/L)",
                                     "Temperature (°C)"),
                     colors = "bw",
                     dot.size = 3,
                     line.size = 0.5,
                     width = 0.2,
                     scale = 1.5,
                     bpe.style = "line",
                     bpe.color = "white")
          
          se <- sqrt(diag(vcov(pe6)))
          se
          
          # table of estimates with 95% CI
          (tab <- cbind(Est = fixef(pe6), LL = fixef(pe6) - 1.96 * se, UL = fixef(pe6) + 1.96 *
                          se))
          
          exp(tab)
          
          #WALKAG -----------------------------------------------------------
          plot_model(pe7, sort.est = FALSE, vline.color = "red",show.values = TRUE, value.offset = .3,
                     title = "Proportion of Time Walk Agitation", axis.title = "Odds Ratios")
          
          plot_model(pe7, 
                     sort.est = FALSE, 
                     vline.color = "red",
                     show.values = TRUE, 
                     value.offset = .3,
                     title = "Walk Agitation", 
                     axis.title = "Odds Ratio",
                     axis.labels = c("Conductivity (µS/cm)",
                                     "DO (mg/L)",
                                     "Temperature (°C)"),
                     colors = "bw",
                     dot.size = 3,
                     line.size = 0.5,
                     width = 0.2,
                     scale = 1.5,
                     bpe.style = "line",
                     bpe.color = "white")
          
          se <- sqrt(diag(vcov(pe7)))
          se
          
          # table of estimates with 95% CI
          (tab <- cbind(Est = fixef(pe7), LL = fixef(pe7) - 1.96 * se, UL = fixef(pe7) + 1.96 *
                          se))
          
          exp(tab)
          
          #HEADAG -----------------------------------------------------------
          plot_model(pe8, sort.est = FALSE, vline.color = "red",show.values = TRUE, value.offset = .3,
                     title = "Proportion of Time Head Agitation", axis.title = "Odds Ratios")
          
          plot_model(pe8, 
                     sort.est = FALSE, 
                     vline.color = "red",
                     show.values = TRUE, 
                     value.offset = .3,
                     title = "Head Agitation", 
                     axis.title = "Odds Ratio",
                     axis.labels = c("Conductivity (µS/cm)",
                                     "DO (mg/L)",
                                     "Temperature (°C)"),
                     colors = "bw",
                     dot.size = 3,
                     line.size = 0.5,
                     width = 0.2,
                     scale = 1.5,
                     bpe.style = "line",
                     bpe.color = "white")
          
          
          se <- sqrt(diag(vcov(pe8)))
          se
          
          # table of estimates with 95% CI
          (tab <- cbind(Est = fixef(pe8), LL = fixef(pe8) - 1.96 * se, UL = fixef(pe8) + 1.96 *
                          se))
          
          exp(tab)
          
          
          #CANN -------------------------------------------------------------
          plot_model(pe9, sort.est = FALSE, vline.color = "red",show.values = TRUE, value.offset = .3,
                     title = "Proportion of Time Cannibalism", axis.title = "Odds Ratios")
          
          plot_model(pe9, 
                     sort.est = FALSE, 
                     vline.color = "red",
                     show.values = TRUE, 
                     value.offset = .3,
                     title = "Cannibalism", 
                     axis.title = "Odds Ratio",
                     axis.labels = c("Conductivity (µS/cm)",
                                     "DO (mg/L)",
                                     "Temperature (°C)"),
                     colors = "bw",
                     dot.size = 3,
                     line.size = 0.5,
                     width = 0.2,
                     scale = 1.5,
                     bpe.style = "line",
                     bpe.color = "white")
          
          
          se <- sqrt(diag(vcov(pe9)))
          se
          
          # table of estimates with 95% CI
          (tab <- cbind(Est = fixef(pe9), LL = fixef(pe9) - 1.96 * se, UL = fixef(pe9) + 1.96 *
                          se))
          
          exp(tab)
          
          
          #GUARD ----------------------------  
          
          plot_model(pt10, 
                     sort.est = FALSE, 
                     vline.color = "red",
                     show.values = TRUE, 
                     value.offset = .3,
                     title = "Guarding", 
                     axis.title = "Odds Ratio",
                     axis.labels = ("Days Since September 6"),
                     colors = "bw",
                     dot.size = 3,
                     line.size = 0.5,
                     width = 0.2,
                     scale = 1.5,
                     bpe.style = "line",
                     bpe.color = "white")
          
          
          se <- sqrt(diag(vcov(pt10)))
          se
          
          # table of estimates with 95% CI
          (tab <- cbind(Est = fixef(pt10), LL = fixef(pt10) - 1.96 * se, UL = fixef(pt10) + 1.96 *
                          se))
          
          exp(tab)
          
          #ROCKO2 ----------------------------         
          plot_model(pt11, 
                     sort.est = FALSE, 
                     vline.color = "red",
                     show.values = TRUE, 
                     value.offset = .3,
                     title = "RockO2", 
                     axis.title = "Odds Ratio",
                     axis.labels = ("Days Since September 6"),
                     colors = "bw",
                     dot.size = 3,
                     line.size = 0.5,
                     width = 0.2,
                     scale = 1.5,
                     bpe.style = "line",
                     bpe.color = "white")
          
          #temporal model
          se <- sqrt(diag(vcov(pt11)))
          se
          
          # table of estimates with 95% CI
          (tab <- cbind(Est = fixef(pt11), LL = fixef(pt11) - 1.96 * se, UL = fixef(pt11) + 1.96 *
                          se))
          
          exp(tab)
          
          
          #environmental model
          se <- sqrt(diag(vcov(pe11)))
          se
          
          # table of estimates with 95% CI
          (tab <- cbind(Est = fixef(pe11), LL = fixef(pe11) - 1.96 * se, UL = fixef(pe11) + 1.96 *
                          se))
          
          exp(tab)
#PREDICTIVE PLOT------------       
  #TOP MODEL FOR EACH BEHAVIOR, PLOT EACH VARIABLE IN TOP MODEL------------------------------------------------------
          ??ggpredict
          ??plot
    
          #####GROUPED BEHAVIORS#####
          #CARE TOTAL--------------------------------------------------------
          
          p1<-plot(ggpredict(pe1, terms = "TEMP[all]")) +
            scale_y_continuous(limits = c(0, 1.0)) + 
            labs(
              x="Temperature (°C)",
              y="Total Care",
              title = element_blank())+
            font_size(axis_title.x = 18,axis_title.y = 16,labels.x = 12,labels.y = 12)+
            theme(panel.grid = element_blank())

          p2<-plot(ggpredict(pe1, terms = "DO[all]")) +
            scale_y_continuous(limits = c(0, 1.0)) + 
            labs(
              x="D.O. (mg/L)",
              y="Predicted Probability of Care Behaviors",
              title = element_blank())+
            font_size(axis_title.x = 18,axis_title.y = 16,labels.x = 12,labels.y = 12)+
            theme(panel.grid = element_blank())
          
          p3<-plot(ggpredict(pe1, terms = "COND[all]")) +
            scale_y_continuous(limits = c(0, 1.0)) + 
            labs(
              x="Conductivity (µS/cm)",
              y="Predicted Probability of Care Behaviors",
              title = element_blank())+
            font_size(axis_title.x = 18,axis_title.y = 16,labels.x = 12,labels.y = 12)+
            theme(panel.grid = element_blank())
          
          #FAN TOTAL---------------------------------------------------------
          
          p4<-plot(ggpredict(pe2, terms = "TEMP[all]")) +
            scale_y_continuous(limits = c(0, 1.0)) + 
            labs(
              x="Temperature (°C)",
              y="Total Fanning",
              title = element_blank())+
            font_size(axis_title.x = 18,axis_title.y = 16,labels.x = 12,labels.y = 12)+
            theme(panel.grid = element_blank())
          
          p5<-plot(ggpredict(pe2, terms = "DO[all]")) +
            scale_y_continuous(limits = c(0, 1.0)) + 
            labs(
              x="D.O. (mg/L)",
              y="Predicted Probability of Total Fanning",
              title = element_blank())+
            font_size(axis_title.x = 18,axis_title.y = 16,labels.x = 12,labels.y = 12)+
            theme(panel.grid = element_blank())
          
          p6<-plot(ggpredict(pe2, terms = "COND[all]")) +
            scale_y_continuous(limits = c(0, 1.0)) + 
            labs(
              x="Conductivity (µS/cm)",
              y="Predicted Probability of Total Fanning",
              title = element_blank())+
            font_size(axis_title.x = 18,axis_title.y = 16,labels.x = 12,labels.y = 12)+
            theme(panel.grid = element_blank())
          
          #AGIT TOTAL--------------------------------------------------------
          
          p7<-plot(ggpredict(pe3, terms = "TEMP[all]")) +
            scale_y_continuous(limits = c(0, 1.0)) + 
            labs(
              x="Temperature (°C)",
              y="Total Agitation",
              title = element_blank())+
            font_size(axis_title.x = 18,axis_title.y = 16,labels.x = 12,labels.y = 12)+
            theme(panel.grid = element_blank())
          
          p8<-plot(ggpredict(pe3, terms = "DO[all]")) +
            scale_y_continuous(limits = c(0, 1.0)) + 
            labs(
              x="D.O. (mg/L)",
              y="Predicted Probability of Total Agitation",
              title = element_blank())+
            font_size(axis_title.x = 18,axis_title.y = 16,labels.x = 12,labels.y = 12)+
            theme(panel.grid = element_blank())
          
          p9<-plot(ggpredict(pe3, terms = "COND[all]")) +
            scale_y_continuous(limits = c(0, 1.0)) + 
            labs(
              x="Conductivity (µS/cm)",
              y="Predicted Probability of Total Agitation",
              title = element_blank())+
            font_size(axis_title.x = 18,axis_title.y = 16,labels.x = 12,labels.y = 12)+
            theme(panel.grid = element_blank())
          
          
          #PANEL PLOT
          
          #PANEL PLOT EXAMPLE
          library(cowplot)

          #Can extract individual legends here, and exclude axes labels in single step
          predpan<-plot_grid(
            p1 + theme(legend.position = "none")+xlab(label = element_blank()),
            p2 + theme(legend.position = "none")+xlab(label = element_blank())+ylab(label = element_blank()),
            p3 + theme(legend.position = "none")+xlab(label = element_blank())+ylab(label = element_blank()),
            p4 + theme(legend.position = "none")+xlab(label = element_blank()),
            p5 + theme(legend.position = "none")+xlab(label = element_blank())+ylab(label = element_blank()),
            p6 + theme(legend.position = "none")+xlab(label = element_blank())+ylab(label = element_blank()),
            p7 + theme(legend.position = "none"),
            p8 + theme(legend.position = "none")+ylab(label = element_blank()),
            p9 + theme(legend.position = "none")+ylab(label = element_blank()),
            ncol = 3, 
            nrow = 3,
            align = "hv")
          
          predpan
          
          
          #Get legend from one of the plots
          legend <- get_legend(
            # create some space to the left of the legend
            a + theme(legend.box.margin = margin(0, 0, 0, 12))
          )
          
          plot_grid(grid,legend,rel_widths = c(3,0.75))#adjusts relative widths of 1 plot vs. legend
          
          
          #####INDIVIDUAL BEHAVIORS#####     
          #TFAN -------------------------------------------------------------
          plot(ggpredict(pe4, terms = "TEMP[all]")) +
            #scale_y_continuous(limits = c(0, 1.0)) + 
            labs(
              x="Temperature (°C)",
              y="Predicted Probability of Tail Fanning",
              title = element_blank())+
            font_size(axis_title.x = 18,axis_title.y = 16,labels.x = 12,labels.y = 12)+
            theme(panel.grid = element_blank())
          
          plot(ggpredict(pe4, terms = "DO[all]")) +
            #scale_y_continuous(limits = c(0, 1.0)) + 
            labs(
              x="D.O. (mg/L)",
              y="Predicted Probability of Tail Fanning",
              title = element_blank())+
            font_size(axis_title.x = 18,axis_title.y = 16,labels.x = 12,labels.y = 12)+
            theme(panel.grid = element_blank())
          
          plot(ggpredict(pe4, terms = "COND[all]")) +
            #scale_y_continuous(limits = c(0, 1.0)) + 
            labs(
              x="Conductivity (µS/cm)",
              y="Predicted Probability of Tail Fanning",
              title = element_blank())+
            font_size(axis_title.x = 18,axis_title.y = 16,labels.x = 12,labels.y = 12)+
            theme(panel.grid = element_blank())
          
          #ROCKTAIL ---------------------------------------------------------
          plot(ggpredict(pe5, terms = "TEMP[all]")) +
            #scale_y_continuous(limits = c(0, 1.0)) + 
            labs(
              x="Temperature (°C)",
              y="Predicted Probability of Rock/Tail",
              title = element_blank())+
            font_size(axis_title.x = 18,axis_title.y = 16,labels.x = 12,labels.y = 12)+
            theme(panel.grid = element_blank())
          
          plot(ggpredict(pe5, terms = "DO[all]")) +
            #scale_y_continuous(limits = c(0, 1.0)) + 
            labs(
              x="D.O. (mg/L)",
              y="Predicted Probability of Rock/Tail",
              title = element_blank())+
            font_size(axis_title.x = 18,axis_title.y = 16,labels.x = 12,labels.y = 12)+
            theme(panel.grid = element_blank())
          
          plot(ggpredict(pe5, terms = "COND[all]")) +
            #scale_y_continuous(limits = c(0, 1.0)) + 
            labs(
              x="Conductivity (µS/cm)",
              y="Predicted Probability of Rock/Tail",
              title = element_blank())+
            font_size(axis_title.x = 18,axis_title.y = 16,labels.x = 12,labels.y = 12)+
            theme(panel.grid = element_blank())
          #ROCKAG -----------------------------------------------------------
          plot(ggpredict(pe6, terms = "TEMP[all]")) +
            #scale_y_continuous(limits = c(0, 1.0)) + 
            labs(
              x="Temperature (°C)",
              y="Predicted Probability of Rock Agitation",
              title = element_blank())+
            font_size(axis_title.x = 18,axis_title.y = 16,labels.x = 12,labels.y = 12)+
            theme(panel.grid = element_blank())
          
          plot(ggpredict(pe6, terms = "DO[all]")) +
            #scale_y_continuous(limits = c(0, 1.0)) + 
            labs(
              x="D.O. (mg/L)",
              y="Predicted Probability of Rock Agitation",
              title = element_blank())+
            font_size(axis_title.x = 18,axis_title.y = 16,labels.x = 12,labels.y = 12)+
            theme(panel.grid = element_blank())
          
          plot(ggpredict(pe6, terms = "COND[all]")) +
            #scale_y_continuous(limits = c(0, 1.0)) + 
            labs(
              x="Conductivity (µS/cm)",
              y="Predicted Probability of Rock Agitation",
              title = element_blank())+
            font_size(axis_title.x = 18,axis_title.y = 16,labels.x = 12,labels.y = 12)+
            theme(panel.grid = element_blank())
          #WALKAG -----------------------------------------------------------
          plot(ggpredict(pe7, terms = "TEMP[all]")) +
            #scale_y_continuous(limits = c(0, 1.0)) + 
            labs(
              x="Temperature (°C)",
              y="Predicted Probability of Walking Agitation",
              title = element_blank())+
            font_size(axis_title.x = 18,axis_title.y = 16,labels.x = 12,labels.y = 12)+
            theme(panel.grid = element_blank())
          
          plot(ggpredict(pe7, terms = "DO[all]")) +
            #scale_y_continuous(limits = c(0, 1.0)) + 
            labs(
              x="D.O. (mg/L)",
              y="Predicted Probability of Walking Agitation",
              title = element_blank())+
            font_size(axis_title.x = 18,axis_title.y = 16,labels.x = 12,labels.y = 12)+
            theme(panel.grid = element_blank())
          
          plot(ggpredict(pe7, terms = "COND[all]")) +
            #scale_y_continuous(limits = c(0, 1.0)) + 
            labs(
              x="Conductivity (µS/cm)",
              y="Predicted Probability of Walking Agitation",
              title = element_blank())+
            font_size(axis_title.x = 18,axis_title.y = 16,labels.x = 12,labels.y = 12)+
            theme(panel.grid = element_blank())
          #HEADAG -----------------------------------------------------------
          plot(ggpredict(pe8, terms = "TEMP[all]")) +
            #scale_y_continuous(limits = c(0, 1.0)) + 
            labs(
              x="Temperature (°C)",
              y="Predicted Probability of Head Agitation",
              title = element_blank())+
            font_size(axis_title.x = 18,axis_title.y = 16,labels.x = 12,labels.y = 12)+
            theme(panel.grid = element_blank())
          
          plot(ggpredict(pe8, terms = "DO[all]")) +
            #scale_y_continuous(limits = c(0, 1.0)) + 
            labs(
              x="D.O. (mg/L)",
              y="Predicted Probability of Head Agitation",
              title = element_blank())+
            font_size(axis_title.x = 18,axis_title.y = 16,labels.x = 12,labels.y = 12)+
            theme(panel.grid = element_blank())
          
          plot(ggpredict(pe8, terms = "COND[all]")) +
            #scale_y_continuous(limits = c(0, 1.0)) + 
            labs(
              x="Conductivity (µS/cm)",
              y="Predicted Probability of Head Agitation",
              title = element_blank())+
            font_size(axis_title.x = 18,axis_title.y = 16,labels.x = 12,labels.y = 12)+
            theme(panel.grid = element_blank())
          #CANN -------------------------------------------------------------
          plot(ggpredict(pe9, terms = "TEMP[all]")) +
            #scale_y_continuous(limits = c(0, 1.0)) + 
            labs(
              x="Temperature (°C)",
              y="Predicted Probability of Cannibalism",
              title = element_blank())+
            font_size(axis_title.x = 18,axis_title.y = 16,labels.x = 12,labels.y = 12)+
            theme(panel.grid = element_blank())
          
          plot(ggpredict(pe9, terms = "DO[all]")) +
            #scale_y_continuous(limits = c(0, 1.0)) + 
            labs(
              x="D.O. (mg/L)",
              y="Predicted Probability of Cannibalism",
              title = element_blank())+
            font_size(axis_title.x = 18,axis_title.y = 16,labels.x = 12,labels.y = 12)+
            theme(panel.grid = element_blank())
          
          plot(ggpredict(pe9, terms = "COND[all]")) +
            #scale_y_continuous(limits = c(0, 1.0)) + 
            labs(
              x="Conductivity (µS/cm)",
              y="Predicted Probability of Cannibalism",
              title = element_blank())+
            font_size(axis_title.x = 18,axis_title.y = 16,labels.x = 12,labels.y = 12)+
            theme(panel.grid = element_blank())
          #GUARD ------------------------------------------------------------
          
          #ROCKO2 -----------------------------------------------------------
          
 
    
         
##########################################################################################################################################
                        
#SECONDARY RELATIONSHIPS B/T WATER QUALITY, BEHAVIOR PROP, FATE--------------------------------------------
      
      #ATTENTION***********************
              
      #GROUPS (ATTEND AND ABANDON) COME OUT SWITCHED HERE RELATIVE TO OBSERVED DATA
      #TRICKY STEP 
              
              
              #RE-ARRANGE LEGEND
              #Check Default Legend Order
              levels(egg.data$FATE)
              #Reverse Order
              egg.data$FATE <-factor(egg.data$FATE, levels = rev(levels(egg.data$FATE)))      
              
              
  #PROPORTION OF TIME SPENT IN BEHAVIORS-------------------------------------------------------------------------------------------------------------
              
    
              
              
              
      #####GROUPED BEHAVIORS#####
              
          #CARE TOTAL--------------------------------------------------------
              
              #PLOTS BY FATE GROUP
              a<-ggplot(egg.data) + 
                aes(x=egg.data$TEMP, y=egg.data$CARE_PROP, col=as.factor(egg.data$FATE)) +
                geom_point(size = 3) +
                stat_smooth(method = "gam", se = TRUE,size = 1)+
                xlab(label = "Temperature (°C)") +
                ylab(label = "Total Care") +
                scale_colour_manual(labels = c("Attended","Abandoned"),values = c("red","blue"))+
                theme(legend.title = element_blank(),legend.text = element_text(size = 15))

              a
              a1<-a+theme(legend.position = "none")+
                xlab(label = element_blank())

              b<-ggplot(egg.data) + 
                aes(x=egg.data$DO, y=egg.data$CARE_PROP, col=as.factor(egg.data$FATE)) +
                geom_point(size = 3) +
                stat_smooth(method = "gam", se = TRUE)+
                xlab(label = "D.O. (mg/L)") +
                ylab(label = "Proportion of Time in Care Behaviors") +
                scale_colour_manual(labels = c("Attended","Abandoned"),values = c("red","blue"))+
                theme(legend.title = element_blank(),legend.text = element_text(size = 15))
  
              b
              b1<-b+theme(legend.position = "none")+
                xlab(label = element_blank())+
                ylab(label = element_blank())
               
              b1
              
              c<-ggplot(egg.data) + 
                aes(x=egg.data$COND, y=egg.data$CARE_PROP, col=as.factor(egg.data$FATE)) +
                geom_point(size = 3) +
                stat_smooth(method = "gam", se = TRUE) +
                xlab(label = "Conductivity (µS/cm)") +
                ylab(label = "Proportion of Time in Care Behaviors") +
                scale_colour_manual(labels = c("Attended","Abandoned"),values = c("red","blue"))+
                theme(legend.title = element_blank(),legend.text = element_text(size = 15))
              
              c
              c1<-c+theme(legend.position = "none")+
                xlab(label = element_blank())+
                ylab(label = element_blank())
              
                
              
          #FAN TOTAL---------------------------------------------------------
            
              #PLOTS BY FATE GROUP
              d<-ggplot(egg.data) + 
                aes(x=egg.data$TEMP, y=egg.data$FAN_PROP, col=as.factor(egg.data$FATE)) +
                geom_point(size = 3) +
                stat_smooth(method = "gam", se = TRUE,size = 1)+
                xlab(label = "Temperature (°C)") +
                ylab(label = "Total Fanning") +
                scale_colour_manual(labels = c("Attended","Abandoned"),values = c("red","blue"))+
                theme(legend.title = element_blank(),legend.text = element_text(size = 15))+
                ylim(0,1)
              
              d
              d1<-d+theme(legend.position = "none")+
                xlab(label = element_blank())
             
              e<-ggplot(egg.data) + 
                aes(x=egg.data$DO, y=egg.data$FAN_PROP, col=as.factor(egg.data$FATE)) +
                geom_point(size = 3) +
                stat_smooth(method = "gam", se = TRUE)+
                xlab(label = "D.O. (mg/L)") +
                ylab(label = "Proportion of Time Fanning") +
                scale_colour_manual(labels = c("Attended","Abandoned"),values = c("red","blue"))+
                theme(legend.title = element_blank(),legend.text = element_text(size = 15))+
                ylim(0,1)
              
              e
              e1<-e+theme(legend.position = "none")+
                xlab(label = element_blank())+
                ylab(label = element_blank())
              

              f<-ggplot(egg.data) + 
                aes(x=egg.data$COND, y=egg.data$FAN_PROP, col=as.factor(egg.data$FATE)) +
                geom_point(size = 3) +
                stat_smooth(method = "gam", se = TRUE) +
                xlab(label = "Conductivity (µS/cm)") +
                ylab(label = "Proportion of Time Fanning") +
                scale_colour_manual(labels = c("Attended","Abandoned"),values = c("red","blue"))+
                theme(legend.title = element_blank(),legend.text = element_text(size = 15))+
                ylim(0,1)
              
              f
              f1<-f+theme(legend.position = "none")+
                xlab(label = element_blank())+
                ylab(label = element_blank())
          
              
              plot(egg.data$COND,egg.data$FAN_PROP, col = as.factor(egg.data$FATE))
        #AGIT TOTAL--------------------------------------------------------
           
              #PLOTS BY FATE GROUP
              g<-ggplot(egg.data) + 
                aes(x=egg.data$TEMP, y=egg.data$AGIT_PROP, col=as.factor(egg.data$FATE)) +
                geom_point(size = 3) +
                stat_smooth(method = "gam", se = TRUE,size = 1)+
                xlab(label = "Temperature (°C)") +
                ylab(label = "Total Agitation") +
                scale_colour_manual(labels = c("Attended","Abandoned"),values = c("red","blue"))+
                theme(legend.title = element_blank(),legend.text = element_text(size = 15))+
                ylim(0,1)
              
              g
              g1<-g+theme(legend.position = "none")

              h<-ggplot(egg.data) + 
                aes(x=egg.data$DO, y=egg.data$AGIT_PROP, col=as.factor(egg.data$FATE)) +
                geom_point(size = 3) +
                stat_smooth(method = "gam", se = TRUE)+
                xlab(label = "D.O. (mg/L)") +
                ylab(label = "Proportion of Time Agitating") +
                scale_colour_manual(labels = c("Attended","Abandoned"),values = c("red","blue"))+
                theme(legend.title = element_blank(),legend.text = element_text(size = 15))+
                ylim(0,1)
              
              h
              h1<-h+theme(legend.position = "none")+
                ylab(label = element_blank())
              
              i<-ggplot(egg.data) + 
                aes(x=egg.data$COND, y=egg.data$AGIT_PROP, col=as.factor(egg.data$FATE)) +
                geom_point(size = 3) +
                stat_smooth(method = "gam", se = TRUE) +
                xlab(label = "Conductivity (µS/cm)") +
                ylab(label = "Proportion of Time Agitating") +
                scale_colour_manual(labels = c("Attended","Abandoned"),values = c("red","blue"))+
                theme(legend.title = element_blank(),legend.text = element_text(size = 15))+
                ylim(0,1)
              
              i
              i1<-i+theme(legend.position = "none")+
                ylab(label = element_blank())
              i1
              
              
      #PANEL PLOT EXAMPLE
              library(cowplot)
              
              #Originally extracted legends first
              plot_grid(a1,b1,c1,d1,e1,f1,g1,h1,i1, 
                        ncol = 3, 
                        nrow = 3,
                        align = "hv")

              #Can extract individual legends here, and exclude axes labels in single step
              grid<-plot_grid(
                a + theme(legend.position = "none")+xlab(label = element_blank()),
                b + theme(legend.position = "none")+xlab(label = element_blank())+ylab(label = element_blank()),
                c + theme(legend.position = "none")+xlab(label = element_blank())+ylab(label = element_blank()),
                d + theme(legend.position = "none")+xlab(label = element_blank()),
                e + theme(legend.position = "none")+xlab(label = element_blank())+ylab(label = element_blank()),
                f + theme(legend.position = "none")+xlab(label = element_blank())+ylab(label = element_blank()),
                g + theme(legend.position = "none"),
                h + theme(legend.position = "none")+ylab(label = element_blank()),
                i + theme(legend.position = "none")+ylab(label = element_blank()),
                        ncol = 3, 
                        nrow = 3,
                        align = "hv")
              
              grid
              
              
              #Get legend from one of the plots
              legend <- get_legend(
                # create some space to the left of the legend
                a + theme(legend.box.margin = margin(0, 0, 0, 12))
              )
              
              plot_grid(grid,legend,rel_widths = c(3,0.75))#adjusts relative widths of 1 plot vs. legend
              
      
              
              
#FATE*WATER QUALITY MODELS ========================================================================================================================              

              #RE-ARRANGE LEGEND
              #Check Default Legend Order
              levels(egg.data$FATE)
              #Reverse Order
              egg.data$FATE <-factor(egg.data$FATE, levels = rev(levels(egg.data$FATE)))   
              
              
              #PROPORTION OF TIME SPENT IN BEHAVIORS----------------------------------------------------------- 
              
              #####GROUPED BEHAVIORS#####
              
              #CARE TOTAL--------------------------------------------------------
              
              #TEMP
              fw1<-glmer(CARE_PROP~scale(TEMP)*FATE+
                           (1|SUBJECT)-1,
                         weights = TOT_LENG_S,
                         family = "binomial",
                         data = egg.data)
              
              
              summary(fw1)
              
              
             #DO
              fw2<-glmer(CARE_PROP~scale(DO)*FATE+
                           (1|SUBJECT)-1,
                         weights = TOT_LENG_S,
                         family = "binomial",
                         data = egg.data)
              
              
              summary(fw2)
              
              #COND
              fw3<-glmer(CARE_PROP~scale(COND)*FATE+
                           (1|SUBJECT),
                         weights = TOT_LENG_S,
                         family = "binomial",
                         data = egg.data)
              
              
              summary(fw3)
              
       
              #FAN TOTAL---------------------------------------------------------
              
              #TEMP
              fw4<-glmer(FAN_PROP~scale(TEMP)*FATE+
                           (1|SUBJECT),
                         weights = TOT_LENG_S,
                         family = "binomial",
                         data = egg.data)
              
              
              summary(fw4)
              
              #DO
              fw5<-glmer(FAN_PROP~scale(DO)*FATE+
                           (1|SUBJECT),
                         weights = TOT_LENG_S,
                         family = "binomial",
                         data = egg.data)
              
              
              summary(fw5)
              
              #COND
              fw6<-glmer(FAN_PROP~scale(COND)*FATE+
                           (1|SUBJECT)-1,
                         weights = TOT_LENG_S,
                         family = "binomial",
                         data = egg.data)
              
              
              summary(fw6)
              
              #AGIT TOTAL--------------------------------------------------------
              
              #TEMP
              fw7<-glmer(AGIT_PROP~scale(TEMP)*FATE+
                           (1|SUBJECT),
                         weights = TOT_LENG_S,
                         family = "binomial",
                         data = egg.data)
              
              
              summary(fw7)
              
              #DO
              fw8<-glmer(AGIT_PROP~scale(DO)*FATE+
                           (1|SUBJECT),
                         weights = TOT_LENG_S,
                         family = "binomial",
                         data = egg.data)
              
              
              summary(fw8)
              
              #COND
              fw9<-glmer(AGIT_PROP~scale(COND)*FATE+
                           (1|SUBJECT),
                         weights = TOT_LENG_S,
                         family = "binomial",
                         data = egg.data)
              
              
              summary(fw9)
              
     
        #####INDIVIDUAL BEHAVIORS#####     
              #TFAN -------------------------------------------------------------
              
              j<-ggplot(egg.data) + 
                aes(x=egg.data$TEMP, y=egg.data$TFAN_PROP, 
                    col=as.factor(egg.data$FATE)) +
                geom_point(size = 3) +
                stat_smooth(method = "gam", se = TRUE)+
                xlab(label = "Temperature (°C)") +
                ylab(label = "Tail Fanning") +
                scale_colour_manual(labels = c("Attended","Abandoned"),values = c("red","blue"))+
                theme(legend.title = element_blank(),legend.text = element_text(size = 15))
              
              j
              
              k<-ggplot(egg.data) + 
                aes(x=egg.data$DO, y=egg.data$TFAN_PROP, 
                    col=as.factor(egg.data$FATE)) +
                geom_point(size = 3) +
                stat_smooth(method = "gam", se = TRUE)+
                xlab(label = "D.O. (mg/L)") +
                ylab(label = "Proportion of Time Tail Fanning") +
                scale_colour_manual(labels = c("Attended","Abandoned"),values = c("red","blue"))+
                theme(legend.title = element_blank(),legend.text = element_text(size = 15))
              
              k
              
              l<-ggplot(egg.data) + 
                aes(x=egg.data$COND, y=egg.data$TFAN_PROP, 
                    col=as.factor(egg.data$FATE)) +
                geom_point(size = 3) +
                stat_smooth(method = "gam", se = TRUE)+
                xlab(label = "Conductivity (µS/cm)") +
                ylab(label = "Proportion of Time Tail Fanning") +
                scale_colour_manual(labels = c("Attended","Abandoned"),values = c("red","blue"))+
                theme(legend.title = element_blank(),legend.text = element_text(size = 15))
              
              l
              
              
              #Can extract individual legends here, and exclude axes labels in single step
              grid2<-plot_grid(
                j + theme(legend.position = "none"),
                k + theme(legend.position = "none")+ylab(label = element_blank()),
                l + theme(legend.position = "none")+ylab(label = element_blank()),
                ncol = 3, 
                nrow = 1,
                align = "hv")
              
              grid2
              
              
              #Get legend from one of the plots
              legend <- get_legend(
                # create some space to the left of the legend
                l + theme(legend.box.margin = margin(0, 0, 0, 12))
              )
              
              plot_grid(grid2,legend,rel_widths = c(3,0.75))#adjusts relative widths of 1 plot vs. legend
              
              
              
              
              #ROCKTAIL ---------------------------------------------------------
 
              m<-ggplot(egg.data) + 
                aes(x=egg.data$TEMP, y=egg.data$ROCKTAIL_PROP, 
                    col=as.factor(egg.data$FATE)) +
                geom_point(size = 3) +
                stat_smooth(method = "gam", se = TRUE)+
                xlab(label = "Temperature (°C)") +
                ylab(label = "Rock/Tail") +
                scale_colour_manual(labels = c("Attended","Abandoned"),values = c("red","blue"))+
                theme(legend.title = element_blank(),legend.text = element_text(size = 15))
              
              m
              
              n<-ggplot(egg.data) + 
                aes(x=egg.data$DO, y=egg.data$ROCKTAIL_PROP, 
                    col=as.factor(egg.data$FATE)) +
                geom_point(size = 3) +
                stat_smooth(method = "gam", se = TRUE)+
                xlab(label = "D.O. (mg/L)") +
                ylab(label = "Proportion of Time Rock/Tail") +
                scale_colour_manual(labels = c("Attended","Abandoned"),values = c("red","blue"))+
                theme(legend.title = element_blank(),legend.text = element_text(size = 15))
              
              n
              
              o<-ggplot(egg.data) + 
                aes(x=egg.data$COND, y=egg.data$ROCKTAIL_PROP, 
                    col=as.factor(egg.data$FATE)) +
                geom_point(size = 3) +
                stat_smooth(method = "gam", se = TRUE)+
                xlab(label = "Conductivity (µS/cm)") +
                ylab(label = "Proportion of Time Rock/Tail") +
                scale_colour_manual(labels = c("Attended","Abandoned"),values = c("red","blue"))+
                theme(legend.title = element_blank(),legend.text = element_text(size = 15))
              
              o
              
              #Can extract individual legends here, and exclude axes labels in single step
              grid3<-plot_grid(
                m + theme(legend.position = "none"),
                n + theme(legend.position = "none")+ylab(label = element_blank()),
                o + theme(legend.position = "none")+ylab(label = element_blank()),
                ncol = 3, 
                nrow = 1,
                align = "hv")
              
              grid3
              
              
              #Get legend from one of the plots
              legend <- get_legend(
                # create some space to the left of the legend
                o + theme(legend.box.margin = margin(0, 0, 0, 12))
              )
              
              plot_grid(grid3,legend,rel_widths = c(3,0.75))#adjusts relative widths of 1 plot vs. legend
              
              
              
              
              #ROCKAG -----------------------------------------------------------
              
              p<-ggplot(egg.data) + 
                aes(x=egg.data$TEMP, y=egg.data$ROCKAG_PROP, 
                    col=as.factor(egg.data$FATE)) +
                geom_point(size = 3) +
                stat_smooth(method = "gam", se = TRUE)+
                xlab(label = "Temperature (°C)") +
                ylab(label = "Rock Agitation") +
                scale_colour_manual(labels = c("Attended","Abandoned"),values = c("red","blue"))+
                theme(legend.title = element_blank(),legend.text = element_text(size = 15))
              
              p
              
              q<-ggplot(egg.data) + 
                aes(x=egg.data$DO, y=egg.data$ROCKAG_PROP, 
                    col=as.factor(egg.data$FATE)) +
                geom_point(size = 3) +
                stat_smooth(method = "gam", se = TRUE)+
                xlab(label = "D.O. (mg/L)") +
                ylab(label = "Proportion of Time Rock Agitation") +
                scale_colour_manual(labels = c("Attended","Abandoned"),values = c("red","blue"))+
                theme(legend.title = element_blank(),legend.text = element_text(size = 15))
              
              q
              
              r<-ggplot(egg.data) + 
                aes(x=egg.data$COND, y=egg.data$ROCKAG_PROP, 
                    col=as.factor(egg.data$FATE)) +
                geom_point(size = 3) +
                stat_smooth(method = "gam", se = TRUE)+
                xlab(label = "Conductivity (µS/cm)") +
                ylab(label = "Proportion of Time Rock Agitation") +
                scale_colour_manual(labels = c("Attended","Abandoned"),values = c("red","blue"))+
                theme(legend.title = element_blank(),legend.text = element_text(size = 15))
              
              r
              
              #Can extract individual legends here, and exclude axes labels in single step
              grid4<-plot_grid(
                p + theme(legend.position = "none"),
                q + theme(legend.position = "none")+ylab(label = element_blank()),
                r + theme(legend.position = "none")+ylab(label = element_blank()),
                ncol = 3, 
                nrow = 1,
                align = "hv")
              
              grid4
              
              
              #Get legend from one of the plots
              legend <- get_legend(
                # create some space to the left of the legend
                o + theme(legend.box.margin = margin(0, 0, 0, 12))
              )
              
              plot_grid(grid4,legend,rel_widths = c(3,0.75))#adjusts relative widths of 1 plot vs. legend
              
              
              
              
              #WALKAG -----------------------------------------------------------
              
              s<-ggplot(egg.data) + 
                aes(x=egg.data$TEMP, y=egg.data$WALKAG_PROP, 
                    col=as.factor(egg.data$FATE)) +
                geom_point(size = 3) +
                stat_smooth(method = "gam", se = TRUE)+
                xlab(label = "Temperature (°C)") +
                ylab(label = "Walk Agitation") +
                scale_colour_manual(labels = c("Attended","Abandoned"),values = c("red","blue"))+
                theme(legend.title = element_blank(),legend.text = element_text(size = 15))
              
              s
              
              t<-ggplot(egg.data) + 
                aes(x=egg.data$DO, y=egg.data$WALKAG_PROP, 
                    col=as.factor(egg.data$FATE)) +
                geom_point(size = 3) +
                stat_smooth(method = "gam", se = TRUE)+
                xlab(label = "D.O. (mg/L)") +
                ylab(label = "Proportion of Time Walk Agitation") +
                scale_colour_manual(labels = c("Attended","Abandoned"),values = c("red","blue"))+
                theme(legend.title = element_blank(),legend.text = element_text(size = 15))
              
              t
              
              u<-ggplot(egg.data) + 
                aes(x=egg.data$COND, y=egg.data$WALKAG_PROP, 
                    col=as.factor(egg.data$FATE)) +
                geom_point(size = 3) +
                stat_smooth(method = "gam", se = TRUE)+
                xlab(label = "Conductivity (µS/cm)") +
                ylab(label = "Proportion of Time Walk Agitation") +
                scale_colour_manual(labels = c("Attended","Abandoned"),values = c("red","blue"))+
                theme(legend.title = element_blank(),legend.text = element_text(size = 15))
              
              u
              
              #Can extract individual legends here, and exclude axes labels in single step
              grid5<-plot_grid(
                s + theme(legend.position = "none"),
                t + theme(legend.position = "none")+ylab(label = element_blank()),
                u + theme(legend.position = "none")+ylab(label = element_blank()),
                ncol = 3, 
                nrow = 1,
                align = "hv")
              
              grid5
              
              
              #Get legend from one of the plots
              legend <- get_legend(
                # create some space to the left of the legend
                o + theme(legend.box.margin = margin(0, 0, 0, 12))
              )
              
              plot_grid(grid5,legend,rel_widths = c(3,0.75))#adjusts relative widths of 1 plot vs. legend
              
              #HEADAG -----------------------------------------------------------
              v<-ggplot(egg.data) + 
                aes(x=egg.data$TEMP, y=egg.data$HEADAG_PROP, 
                    col=as.factor(egg.data$FATE)) +
                geom_point(size = 3) +
                stat_smooth(method = "gam", se = TRUE)+
                xlab(label = "Temperature (°C)") +
                ylab(label = "Head Agitation") +
                scale_colour_manual(labels = c("Attended","Abandoned"),values = c("red","blue"))+
                theme(legend.title = element_blank(),legend.text = element_text(size = 15))
              
              v
              
              w<-ggplot(egg.data) + 
                aes(x=egg.data$DO, y=egg.data$HEADAG_PROP, 
                    col=as.factor(egg.data$FATE)) +
                geom_point(size = 3) +
                stat_smooth(method = "gam", se = TRUE)+
                xlab(label = "D.O. (mg/L)") +
                ylab(label = "Proportion of Time Head Agitation") +
                scale_colour_manual(labels = c("Attended","Abandoned"),values = c("red","blue"))+
                theme(legend.title = element_blank(),legend.text = element_text(size = 15))
              
              w
              
              x<-ggplot(egg.data) + 
                aes(x=egg.data$COND, y=egg.data$HEADAG_PROP, 
                    col=as.factor(egg.data$FATE)) +
                geom_point(size = 3) +
                stat_smooth(method = "gam", se = TRUE)+
                xlab(label = "Conductivity (µS/cm)") +
                ylab(label = "Proportion of Time Head Agitation") +
                scale_colour_manual(labels = c("Attended","Abandoned"),values = c("red","blue"))+
                theme(legend.title = element_blank(),legend.text = element_text(size = 15))
              
              x
              
              #Can extract individual legends here, and exclude axes labels in single step
              grid6<-plot_grid(
                v + theme(legend.position = "none"),
                w + theme(legend.position = "none")+ylab(label = element_blank()),
                x + theme(legend.position = "none")+ylab(label = element_blank()),
                ncol = 3, 
                nrow = 1,
                align = "hv")
              
              grid6
              
              
              #Get legend from one of the plots
              legend <- get_legend(
                # create some space to the left of the legend
                o + theme(legend.box.margin = margin(0, 0, 0, 12))
              )
              
              plot_grid(grid6,legend,rel_widths = c(3,0.75))#adjusts relative widths of 1 plot vs. legend
              
              
              
              
              #CANN -------------------------------------------------------------
              y<-ggplot(egg.data) + 
                aes(x=egg.data$TEMP, y=egg.data$CANN_PROP, 
                    col=as.factor(egg.data$FATE)) +
                geom_point(size = 3) +
                stat_smooth(method = "gam", se = TRUE)+
                xlab(label = "Temperature (°C)") +
                ylab(label = "Cannibalism") +
                scale_colour_manual(labels = c("Attended","Abandoned"),values = c("red","blue"))+
                theme(legend.title = element_blank(),legend.text = element_text(size = 15))
              
              y
              
              z<-ggplot(egg.data) + 
                aes(x=egg.data$DO, y=egg.data$CANN_PROP, 
                    col=as.factor(egg.data$FATE)) +
                geom_point(size = 3) +
                stat_smooth(method = "gam", se = TRUE)+
                xlab(label = "D.O. (mg/L)") +
                ylab(label = "Proportion of Time Cannibalism") +
                scale_colour_manual(labels = c("Attended","Abandoned"),values = c("red","blue"))+
                theme(legend.title = element_blank(),legend.text = element_text(size = 15))
              
              z
              
              za<-ggplot(egg.data) + 
                aes(x=egg.data$COND, y=egg.data$CANN_PROP, 
                    col=as.factor(egg.data$FATE)) +
                geom_point(size = 3) +
                stat_smooth(method = "gam", se = TRUE)+
                xlab(label = "Conductivity (µS/cm)") +
                ylab(label = "Proportion of Time Cannibalism") +
                scale_colour_manual(labels = c("Attended","Abandoned"),values = c("red","blue"))+
                theme(legend.title = element_blank(),legend.text = element_text(size = 15))
              
              za
              
              #Can extract individual legends here, and exclude axes labels in single step
              grid7<-plot_grid(
                y + theme(legend.position = "none"),
                z + theme(legend.position = "none")+ylab(label = element_blank()),
                za + theme(legend.position = "none")+ylab(label = element_blank()),
                ncol = 3, 
                nrow = 1,
                align = "hv")
              
              grid7
              
              
              #Get legend from one of the plots
              legend <- get_legend(
                # create some space to the left of the legend
                o + theme(legend.box.margin = margin(0, 0, 0, 12))
              )
              
              plot_grid(grid7,legend,rel_widths = c(3,0.75))#adjusts relative widths of 1 plot vs. legend
              
              
              #CANN_FREQ
              
              y1<-ggplot(egg.data) + 
                aes(x=egg.data$TEMP, y=egg.data$CANN_FREQ, 
                    col=as.factor(egg.data$FATE)) +
                geom_point(size = 3) +
                stat_smooth(method = "gam", se = TRUE)+
                xlab(label = "Temperature (°C)") +
                ylab(label = "Cannibalism RAW FREQ") +
                scale_colour_manual(labels = c("Attended","Abandoned"),values = c("red","blue"))+
                theme(legend.title = element_blank(),legend.text = element_text(size = 15))
              
              y1
              
              z1<-ggplot(egg.data) + 
                aes(x=egg.data$DO, y=egg.data$CANN_FREQ, 
                    col=as.factor(egg.data$FATE)) +
                geom_point(size = 3) +
                stat_smooth(method = "gam", se = TRUE)+
                xlab(label = "D.O. (mg/L)") +
                ylab(label = "Freq Cannibalism") +
                scale_colour_manual(labels = c("Attended","Abandoned"),values = c("red","blue"))+
                theme(legend.title = element_blank(),legend.text = element_text(size = 15))
              
              z1
              
              za1<-ggplot(egg.data) + 
                aes(x=egg.data$COND, y=egg.data$CANN_FREQ, 
                    col=as.factor(egg.data$FATE)) +
                geom_point(size = 3) +
                stat_smooth(method = "gam", se = TRUE)+
                xlab(label = "Conductivity (µS/cm)") +
                ylab(label = "Freq Cannibalism") +
                scale_colour_manual(labels = c("Attended","Abandoned"),values = c("red","blue"))+
                theme(legend.title = element_blank(),legend.text = element_text(size = 15))
              
              za1
              
              #Can extract individual legends here, and exclude axes labels in single step
              grid7a<-plot_grid(
                y1 + theme(legend.position = "none"),
                z1 + theme(legend.position = "none")+ylab(label = element_blank()),
                za1 + theme(legend.position = "none")+ylab(label = element_blank()),
                ncol = 3, 
                nrow = 1,
                align = "hv")
              
              grid7a
              
              
              #Get legend from one of the plots
              legend <- get_legend(
                # create some space to the left of the legend
                o + theme(legend.box.margin = margin(0, 0, 0, 12))
              )
              
              plot_grid(grid7a,legend,rel_widths = c(3,0.75))#adjusts relative widths of 1 plot vs. legend
              
  
              #GUARD ------------------------------------------------------------
              zb<-ggplot(egg.data) + 
                aes(x=egg.data$TEMP, y=egg.data$GUARD_PROP, 
                    col=as.factor(egg.data$FATE)) +
                geom_point(size = 3) +
                stat_smooth(method = "gam", se = TRUE)+
                xlab(label = "Temperature (°C)") +
                ylab(label = "Guarding") +
                scale_colour_manual(labels = c("Attended","Abandoned"),values = c("red","blue"))+
                theme(legend.title = element_blank(),legend.text = element_text(size = 15))
              
              zb
              
              zc<-ggplot(egg.data) + 
                aes(x=egg.data$DO, y=egg.data$GUARD_PROP, 
                    col=as.factor(egg.data$FATE)) +
                geom_point(size = 3) +
                stat_smooth(method = "gam", se = TRUE)+
                xlab(label = "D.O. (mg/L)") +
                ylab(label = "Proportion of Time Guarding") +
                scale_colour_manual(labels = c("Attended","Abandoned"),values = c("red","blue"))+
                theme(legend.title = element_blank(),legend.text = element_text(size = 15))
              
              zc
              
              zd<-ggplot(egg.data) + 
                aes(x=egg.data$COND, y=egg.data$GUARD_PROP, 
                    col=as.factor(egg.data$FATE)) +
                geom_point(size = 3) +
                stat_smooth(method = "gam", se = TRUE)+
                xlab(label = "Conductivity (µS/cm)") +
                ylab(label = "Proportion of Time Guarding") +
                scale_colour_manual(labels = c("Attended","Abandoned"),values = c("red","blue"))+
                theme(legend.title = element_blank(),legend.text = element_text(size = 15))
              
              zd
              
              #Can extract individual legends here, and exclude axes labels in single step
              grid8<-plot_grid(
                zb + theme(legend.position = "none"),
                zc + theme(legend.position = "none")+ylab(label = element_blank()),
                zd + theme(legend.position = "none")+ylab(label = element_blank()),
                ncol = 3, 
                nrow = 1,
                align = "hv")
              
              grid8
              
              
              #Get legend from one of the plots
              legend <- get_legend(
                # create some space to the left of the legend
                o + theme(legend.box.margin = margin(0, 0, 0, 12))
              )
              
              plot_grid(grid8,legend,rel_widths = c(3,0.75))#adjusts relative widths of 1 plot vs. legend
              
              
              
              
              
              #ROCKO2 ------------------------------------------------------------
              ze<-ggplot(egg.data) + 
                aes(x=egg.data$TEMP, y=egg.data$ROCKO2_PROP, 
                    col=as.factor(egg.data$FATE)) +
                geom_point(size = 3) +
                stat_smooth(method = "gam", se = TRUE)+
                xlab(label = "Temperature (°C)") +
                ylab(label = "Rock O2") +
                scale_colour_manual(labels = c("Attended","Abandoned"),values = c("red","blue"))+
                theme(legend.title = element_blank(),legend.text = element_text(size = 15))
              
              ze
              
              zf<-ggplot(egg.data) + 
                aes(x=egg.data$DO, y=egg.data$ROCKO2_PROP, 
                    col=as.factor(egg.data$FATE)) +
                geom_point(size = 3) +
                stat_smooth(method = "gam", se = TRUE)+
                xlab(label = "D.O. (mg/L)") +
                ylab(label = "Proportion of Time Rock O2") +
                scale_colour_manual(labels = c("Attended","Abandoned"),values = c("red","blue"))+
                theme(legend.title = element_blank(),legend.text = element_text(size = 15))
              
              zf
              
              zg<-ggplot(egg.data) + 
                aes(x=egg.data$COND, y=egg.data$ROCKO2_PROP, 
                    col=as.factor(egg.data$FATE)) +
                geom_point(size = 3) +
                stat_smooth(method = "gam", se = TRUE)+
                xlab(label = "Conductivity (µS/cm)") +
                ylab(label = "Proportion of Time Rock O2") +
                scale_colour_manual(labels = c("Attended","Abandoned"),values = c("red","blue"))+
                theme(legend.title = element_blank(),legend.text = element_text(size = 15))
              
              zg
              
              #Can extract individual legends here, and exclude axes labels in single step
              grid9<-plot_grid(
                ze + theme(legend.position = "none"),
                zf + theme(legend.position = "none")+ylab(label = element_blank()),
                zg + theme(legend.position = "none")+ylab(label = element_blank()),
                ncol = 3, 
                nrow = 1,
                align = "hv")
              
              grid9
              
              
              #Get legend from one of the plots
              legend <- get_legend(
                # create some space to the left of the legend
                o + theme(legend.box.margin = margin(0, 0, 0, 12))
              )
              
              plot_grid(grid9,legend,rel_widths = c(3,0.75))#adjusts relative widths of 1 plot vs. legend        
              
              

             
              

              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
              
#OLDER STUFF===========================================================================================================================
              #########################################################################################################################
              #SECONDARY RELATIONSHIPS B/T WATER QUALITY, BEHAVIOR PROP, MALE_CLASS--------------------------------------------
              
              
              #PROPORTION OF TIME SPENT IN BEHAVIORS-------------------------------------------------------------------------------------------------------------
              
              #####GROUPED BEHAVIORS#####
              
              #CARE TOTAL--------------------------------------------------------
              
              #PLOTS BY FATE GROUP
              ggplot(egg.data) + 
                aes(x=egg.data$TEMP, y=egg.data$CARE_PROP, col=as.factor(egg.data$MALE_CLASS)) +
                geom_point() +
                stat_smooth(method = "gam", se = TRUE)
              
              ggplot(egg.data) + 
                aes(x=egg.data$DO, y=egg.data$CARE_PROP, col=as.factor(egg.data$MALE_CLASS)) +
                geom_point() +
                stat_smooth(method = "gam", se = TRUE)
              
              ggplot(egg.data) + 
                aes(x=egg.data$COND, y=egg.data$CARE_PROP, col=as.factor(egg.data$MALE_CLASS)) +
                geom_point() +
                stat_smooth(method = "gam", se = TRUE)
              
              #FAN TOTAL---------------------------------------------------------
              
              #PLOTS BY FATE GROUP
              ggplot(egg.data) + 
                aes(x=egg.data$TEMP, y=egg.data$FAN_PROP, 
                    col=as.factor(egg.data$MALE_CLASS)) +
                geom_point() +
                stat_smooth(method = "gam", se = TRUE)
              
              ggplot(egg.data) + 
                aes(x=egg.data$DO, y=egg.data$FAN_PROP, 
                    col=as.factor(egg.data$MALE_CLASS)) +
                geom_point() +
                stat_smooth(method = "gam", se = TRUE)
              
              ggplot(egg.data) + 
                aes(x=egg.data$COND, y=egg.data$FAN_PROP, 
                    col=as.factor(egg.data$MALE_CLASS)) +
                geom_point() +
                stat_smooth(method = "gam", se = TRUE)
              
              #AGIT TOTAL--------------------------------------------------------
              
              #PLOTS BY FATE GROUP
              ggplot(egg.data) + 
                aes(x=egg.data$TEMP, y=egg.data$AGIT_PROP, 
                    col=as.factor(egg.data$MALE_CLASS)) +
                geom_point() +
                stat_smooth(method = "gam", se = TRUE)
              
              ggplot(egg.data) + 
                aes(x=egg.data$DO, y=egg.data$AGIT_PROP, 
                    col=as.factor(egg.data$MALE_CLASS)) +
                geom_point() +
                stat_smooth(method = "gam", se = TRUE)
              
              ggplot(egg.data) + 
                aes(x=egg.data$COND, y=egg.data$AGIT_PROP, 
                    col=as.factor(egg.data$MALE_CLASS)) +
                geom_point() +
                stat_smooth(method = "gam", se = TRUE)
              
              #####INDIVIDUAL BEHAVIORS#####     
              #TFAN -------------------------------------------------------------
              
              ggplot(egg.data) + 
                aes(x=egg.data$TEMP, y=egg.data$TFAN_PROP, 
                    col=as.factor(egg.data$MALE_CLASS)) +
                geom_point() +
                stat_smooth(method = "gam", se = TRUE)
              
              ggplot(egg.data) + 
                aes(x=egg.data$DO, y=egg.data$TFAN_PROP, 
                    col=as.factor(egg.data$MALE_CLASS)) +
                geom_point() +
                stat_smooth(method = "gam", se = TRUE)
              
              ggplot(egg.data) + 
                aes(x=egg.data$COND, y=egg.data$TFAN_PROP, 
                    col=as.factor(egg.data$MALE_CLASS)) +
                geom_point() +
                stat_smooth(method = "gam", se = TRUE)
              
              #ROCKTAIL ---------------------------------------------------------
              
              ggplot(egg.data) + 
                aes(x=egg.data$TEMP, y=egg.data$ROCKTAIL_PROP, 
                    col=as.factor(egg.data$MALE_CLASS)) +
                geom_point() +
                stat_smooth(method = "gam", se = TRUE)
              
              ggplot(egg.data) + 
                aes(x=egg.data$DO, y=egg.data$ROCKTAIL_PROP, 
                    col=as.factor(egg.data$MALE_CLASS)) +
                geom_point() +
                stat_smooth(method = "gam", se = TRUE)
              
              ggplot(egg.data) + 
                aes(x=egg.data$COND, y=egg.data$ROCKTAIL_PROP, 
                    col=as.factor(egg.data$MALE_CLASS)) +
                geom_point() +
                stat_smooth(method = "gam", se = TRUE)
              #ROCKAG -----------------------------------------------------------
              
              ggplot(egg.data) + 
                aes(x=egg.data$TEMP, y=egg.data$ROCKAG_PROP, 
                    col=as.factor(egg.data$MALE_CLASS)) +
                geom_point() +
                stat_smooth(method = "gam", se = TRUE)
              
              ggplot(egg.data) + 
                aes(x=egg.data$DO, y=egg.data$ROCKAG_PROP, 
                    col=as.factor(egg.data$MALE_CLASS)) +
                geom_point() +
                stat_smooth(method = "gam", se = TRUE)
              
              ggplot(egg.data) + 
                aes(x=egg.data$COND, y=egg.data$ROCKAG_PROP, 
                    col=as.factor(egg.data$MALE_CLASS)) +
                geom_point() +
                stat_smooth(method = "gam", se = TRUE)
              #WALKAG -----------------------------------------------------------
              
              ggplot(egg.data) + 
                aes(x=egg.data$TEMP, y=egg.data$WALKAG_PROP, 
                    col=as.factor(egg.data$MALE_CLASS)) +
                geom_point() +
                stat_smooth(method = "gam", se = TRUE)
              
              ggplot(egg.data) + 
                aes(x=egg.data$DO, y=egg.data$WALKAG_PROP, 
                    col=as.factor(egg.data$MALE_CLASS)) +
                geom_point() +
                stat_smooth(method = "gam", se = TRUE)
              
              ggplot(egg.data) + 
                aes(x=egg.data$COND, y=egg.data$WALKAG_PROP, 
                    col=as.factor(egg.data$MALE_CLASS)) +
                geom_point() +
                stat_smooth(method = "gam", se = TRUE)
              
              #HEADAG -----------------------------------------------------------
              ggplot(egg.data) + 
                aes(x=egg.data$TEMP, y=egg.data$HEADAG_PROP, 
                    col=as.factor(egg.data$MALE_CLASS)) +
                geom_point() +
                stat_smooth(method = "gam", se = TRUE)
              
              ggplot(egg.data) + 
                aes(x=egg.data$DO, y=egg.data$HEADAG_PROP, 
                    col=as.factor(egg.data$MALE_CLASS)) +
                geom_point() +
                stat_smooth(method = "gam", se = TRUE)
              
              ggplot(egg.data) + 
                aes(x=egg.data$COND, y=egg.data$HEADAG_PROP, 
                    col=as.factor(egg.data$MALE_CLASS)) +
                geom_point() +
                stat_smooth(method = "gam", se = TRUE)
              #CANN -------------------------------------------------------------
              ggplot(egg.data) + 
                aes(x=egg.data$TEMP, y=egg.data$CANN_PROP, 
                    col=as.factor(egg.data$MALE_CLASS)) +
                geom_point() +
                stat_smooth(method = "gam", se = TRUE)
              
              ggplot(egg.data) + 
                aes(x=egg.data$DO, y=egg.data$CANN_PROP, 
                    col=as.factor(egg.data$MALE_CLASS)) +
                geom_point() +
                stat_smooth(method = "gam", se = TRUE)
              
              ggplot(egg.data) + 
                aes(x=egg.data$COND, y=egg.data$CANN_PROP, 
                    col=as.factor(egg.data$MALE_CLASS)) +
                geom_point() +
                stat_smooth(method = "gam", se = TRUE)
              #GUARD ------------------------------------------------------------
              ggplot(egg.data) + 
                aes(x=egg.data$TEMP, y=egg.data$GUARD_PROP, 
                    col=as.factor(egg.data$MALE_CLASS)) +
                geom_point() +
                stat_smooth(method = "gam", se = TRUE)
              
              ggplot(egg.data) + 
                aes(x=egg.data$DO, y=egg.data$GUARD_PROP, 
                    col=as.factor(egg.data$MALE_CLASS)) +
                geom_point() +
                stat_smooth(method = "gam", se = TRUE)
              
              ggplot(egg.data) + 
                aes(x=egg.data$COND, y=egg.data$GUARD_PROP, 
                    col=as.factor(egg.data$MALE_CLASS)) +
                geom_point() +
                stat_smooth(method = "gam", se = TRUE)
              #ROCKO2 ------------------------------------------------------------
              ggplot(egg.data) + 
                aes(x=egg.data$TEMP, y=egg.data$ROCKO2_PROP, 
                    col=as.factor(egg.data$MALE_CLASS)) +
                geom_point() +
                stat_smooth(method = "gam", se = TRUE)
              
              ggplot(egg.data) + 
                aes(x=egg.data$DO, y=egg.data$ROCKO2_PROP, 
                    col=as.factor(egg.data$MALE_CLASS)) +
                geom_point() +
                stat_smooth(method = "gam", se = TRUE)
              
              ggplot(egg.data) + 
                aes(x=egg.data$COND, y=egg.data$ROCKO2_PROP, 
                    col=as.factor(egg.data$MALE_CLASS)) +
                geom_point() +
                stat_smooth(method = "gam", se = TRUE)                    
     citation()         
              
#INDIVIDUAL FATE MODELS ========================================================================================================================              
#FATE (ABANDON VS ATTENDED?)
    
#PROPORTION OF TIME SPENT IN BEHAVIORS----------------------------------------------------------- 
                        
        #####GROUPED BEHAVIORS#####
                    
              #CARE TOTAL--------------------------------------------------------
                        
                        pf1<-glmer(CARE_PROP~FATE+
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pf1)
                  
                        #PLOTS
                        plot(egg.data$FATE,egg.data$CARE_PROP) 
                      
              #FAN TOTAL---------------------------------------------------------
                        
                        pf2<-glmer(FAN_PROP~FATE+
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pf2)
                        
                        #PLOTS
                        plot(egg.data$FATE,egg.data$FAN_PROP) 
                        
              #AGIT TOTAL--------------------------------------------------------
                        
                        pf3<-glmer(AGIT_PROP~FATE+
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pf3)
                        
                        #PLOTS
                        plot(egg.data$FATE,egg.data$AGIT_PROP)
                        
                        
      #####INDIVIDUAL BEHAVIORS#####     
              #TFAN -------------------------------------------------------------
                        
                        pf4<-glmer(TFAN_PROP~FATE+
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pf4)
                        
                        #PLOTS
                        plot(egg.data$FATE,egg.data$TFAN_PROP)
                        
              #ROCKTAIL ---------------------------------------------------------
                        
                        pf5<-glmer(ROCKTAIL_PROP~FATE+
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pf5)
                        
                        #PLOTS
                        plot(egg.data$FATE,egg.data$ROCKTAIL_PROP)
                        
              #ROCKAG -----------------------------------------------------------
                        
                        pf6<-glmer(ROCKAG_PROP~FATE+
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pf6)
                        
                        #PLOTS
                        plot(egg.data$FATE,egg.data$ROCKAG_PROP)
                        
              #WALKAG -----------------------------------------------------------
                        
                        pf7<-glmer(WALKAG_PROP~FATE+
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pf7)
                        
                        #PLOTS
                        plot(egg.data$FATE,egg.data$WALKAG_PROP)
                        
              #HEADAG -----------------------------------------------------------
                        
                        pf8<-glmer(HEADAG_PROP~FATE+
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pf8)
                        
                        #PLOTS
                        plot(egg.data$FATE,egg.data$HEADAG_PROP)
                        
              #CANN -------------------------------------------------------------
                        
                        pf9<-glmer(CANN_PROP~FATE+
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pf9)
                        
                        #PLOTS
                        plot(egg.data$FATE,egg.data$CANN_PROP)
                        
              #GUARD ------------------------------------------------------------                  
                        
                        pf10<-glmer(GUARD_PROP~FATE+
                                     (1|SUBJECT),
                                   weights = TOT_LENG_S,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(pf10)
                        
                        #PLOTS
                        plot(egg.data$FATE,egg.data$GUARD_PROP)
                        
              #ROCK O2 ------------------------------------------------------------                  
                        
                        pf11<-glmer(ROCKO2_PROP~FATE+
                                      (1|SUBJECT),
                                    weights = TOT_LENG_S,
                                    family = "binomial",
                                    data = egg.data)
                        
                        
                        summary(pf11)
                        
                        #PLOTS
                        plot(egg.data$FATE,egg.data$ROCKO2_PROP)         
#FREQ----------------------------------------------------------- 
                     
                        
        #####GROUPED BEHAVIORS#####
              #CARE TOTAL--------------------------------------------------------
                        
                        ff1<-lme(CARE_MIN~FATE, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(ff1)
                        
                        #PLOTS
                        plot(egg.data$FATE,egg.data$CARE_MIN)
                                        
                        
                #FAN TOTAL---------------------------------------------------------
                        
                        ff2<-lme(FAN_MIN~FATE, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(ff2)
                        
                        #PLOTS
                        plot(egg.data$FATE,egg.data$FAN_MIN)
                        
                #AGIT TOTAL--------------------------------------------------------
                        
                        ff3<-lme(AGIT_MIN~FATE, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(ff3)
                        
                        #PLOTS
                        plot(egg.data$FATE,egg.data$AGIT_MIN)
                        
        #####INDIVIDUAL BEHAVIORS#####     
                #TFAN -------------------------------------------------------------
                        
                        ff4<-lme(TFAN_MIN~FATE, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(ff4)
                        
                        #PLOTS
                        plot(egg.data$FATE,egg.data$TFAN_MIN)
                        
                #ROCKTAIL ---------------------------------------------------------
                        
                        ff5<-lme(ROCKTAIL_MIN~FATE, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(ff5)
                        
                        #PLOTS
                        plot(egg.data$FATE,egg.data$ROCKTAIL_MIN)
                #ROCKAG -----------------------------------------------------------
                        
                        ff6<-lme(ROCKAG_MIN~FATE, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(ff6)
                        
                        #PLOTS
                        plot(egg.data$FATE,egg.data$ROCKAG_MIN)
                        
                #WALKAG -----------------------------------------------------------
                        
                        ff7<-lme(WALKAG_MIN~FATE, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(ff7)
                        
                        #PLOTS
                        plot(egg.data$FATE,egg.data$WALKAG_MIN)
                        
                #HEADAG -----------------------------------------------------------
                        
                        ff8<-lme(HEADAG_MIN~FATE, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(ff8)
                        
                        #PLOTS
                        plot(egg.data$FATE,egg.data$HEADAG_MIN)
                        
                #CANN -------------------------------------------------------------
                        
                        ff9<-lme(CANN_MIN~FATE, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(ff9)
                        
                        #PLOTS
                        plot(egg.data$FATE,egg.data$CANN_MIN)
                        
                #GUARD ------------------------------------------------------------           
                        ff10<-lme(GUARD_MIN~FATE, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(ff10)
                        
                        #PLOTS
                        plot(egg.data$FATE,egg.data$GUARD_MIN)
                        
#MEAN DURATION----------------------------------------------------------- 
                        
                        
      #####GROUPED BEHAVIORS#####
                #CARE TOTAL--------------------------------------------------------                        
                        
                        mf1<-lme(CARE_MEAN~FATE, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(mf1)
                        
                        #PLOTS
                        plot(egg.data$FATE,egg.data$CARE_MEAN)     
                        
                #FAN TOTAL---------------------------------------------------------
                        
                        mf2<-lme(FAN_MEAN~FATE, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(mf2)
                        
                        #PLOTS
                        plot(egg.data$FATE,egg.data$FAN_MEAN)
                        
                #AGIT TOTAL--------------------------------------------------------
                        
                        mf3<-lme(AGIT_MEAN~FATE, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(mf3)
                        
                        #PLOTS
                        plot(egg.data$FATE,egg.data$AGIT_MEAN)
                        
      #####INDIVIDUAL BEHAVIORS#####     
                #TFAN -------------------------------------------------------------
                        mf4<-lme(TFAN_MEAN~FATE, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(mf4)
                        
                        #PLOTS
                        plot(egg.data$FATE,egg.data$TFAN_MEAN)
                #ROCKTAIL ---------------------------------------------------------
                        
                        mf5<-lme(ROCKTAIL_MEAN~FATE, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(mf5)
                        
                        #PLOTS
                        plot(egg.data$FATE,egg.data$ROCKTAIL_MEAN)
                        
                #ROCKAG -----------------------------------------------------------
                        mf6<-lme(ROCKAG_MEAN~FATE, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(mf6)
                        
                        #PLOTS
                        plot(egg.data$FATE,egg.data$ROCKAG_MEAN)
                #WALKAG -----------------------------------------------------------
                        mf7<-lme(WALKAG_MEAN~FATE, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(mf7)
                        
                        #PLOTS
                        plot(egg.data$FATE,egg.data$WALKAG_MEAN)
                #HEADAG -----------------------------------------------------------
                        mf8<-lme(HEADAG_MEAN~FATE, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(mf8)
                        
                        #PLOTS
                        plot(egg.data$FATE,egg.data$HEADAG_MEAN)
                #CANN -------------------------------------------------------------
                        mf9<-lme(CANN_MEAN~FATE, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(mf9)
                        
                        #PLOTS
                        plot(egg.data$FATE,egg.data$CANN_MEAN)
                      
                #GUARD ------------------------------------------------------------
                        mf10<-lme(GUARD_MEAN~FATE, 
                                 random = ~1|SUBJECT,
                                 data = egg.data)
                        
                        
                        summary(mf10)
                        
                        #PLOTS
                        plot(egg.data$FATE,egg.data$GUARD_MEAN)
                        
                        
    
#WATER QUALITY----------------------------------------------------------- 
                        
                #TEMP       
                        wqf1<-lme(TEMP~FATE, 
                                  random = ~1|SUBJECT,
                                  data = egg.data)
                        
                        
                        summary(wqf1)
                        
                        #PLOTS
                        plot(egg.data$FATE,egg.data$TEMP)       
                        
                        
                #DO     
                        wqf2<-lme(DO~FATE, 
                                  random = ~1|SUBJECT,
                                  data = egg.data)
                        
                        
                        summary(wqf2)
                        
                        #PLOTS
                        plot(egg.data$FATE,egg.data$DO)          
                        
                        
                #COND  
                        wqf3<-lme(COND~FATE, 
                                  random = ~1|SUBJECT,
                                  data = egg.data)
                        
                        
                        summary(wqf3)
                        
                        #PLOTS
                        plot(egg.data$FATE,egg.data$COND)          
                        
                        
                #PH
                        wqf4<-lme(PH~FATE, 
                                  random = ~1|SUBJECT,
                                  data = egg.data)
                        
                        
                        summary(wqf4)
                        
                        #PLOTS
                        plot(egg.data$FATE,egg.data$PH)         
                        
                        
                  #DEPTH
                        wqf5<-lme(DEPTH~FATE, 
                                  random = ~1|SUBJECT,
                                  data = egg.data)
                        
                        
                        summary(wqf5)
                        
                        #PLOTS
                        plot(egg.data$FATE,egg.data$DEPTH)       
                        
                        #MEAN + SE PLOT
                        wqf5.ss<-describeBy(egg.data$DEPTH, group=egg.data$FATE, mat = TRUE) 
                        wqf5.ss 
                        
                        wqf5.plot<-ggplot(wqf5.ss, aes(x = group1, y= mean))+
                          theme_classic()+
                          geom_point(size = 3)+
                          geom_errorbar(aes(ymin=mean-se, ymax=mean+se), linewidth = 0.5, width=.1)+
                          labs(y="Water Depth (cm)", x= "Nest Fate")
                        
                       wqf5.plot  
                      
                       
                       wqf5.plot +
                         theme(axis.title.x = element_text(size = 18))+
                         theme(axis.title.y = element_text(size = 18))+
                         theme(axis.text.x = element_text(size = 15))+
                         theme(axis.text.y = element_text(size = 15))
                      
                  
#EGG SURVIVAL MODELS---------------------------------------------------------------------------------                        
#EGG_SURV = Y
#2 predictors? (e.g.; CARE_MIN + CARE_PROP)?
# ONLY PROP AND FREQ AS X
#DONT WORRY ABOUT MEAN (MISSING THE VARIATION SO NOT VERY INFORMATIVE)
#ONLY CARE, FAN, AGIT, GUARD AS BEHAVIORS (OR ANY OTHERS DEEMED IMPORTANT)                        
#SURV~PROP 
#SURV~ FREQ
#CARE TOTAL, AGIT, FAN                      
       
      #EGG SURVIVAL-----------------------------------
                        
            #CORRELATIONS AMONG NUMERIC PREDICTORS-------------------------------------------------
            egg.data.num <- egg.data[ c(25:55)]
            
            rcorr(as.matrix(egg.data.num), type="pearson") 
            
            cor(egg.data$CARE_PROP,egg.data$CARE_FREQ)
            cor(egg.data$FAN_PROP,egg.data$FAN_FREQ)
            cor(egg.data$AGIT_PROP,egg.data$AGIT_FREQ)
            
            
            #PROP AND FREQ HIGHLY CORRELATED WITH ONE ANOTHER IN MOST CASES***
            #RAN FREQ AND PROP SEPARATELY***
            
      #EGG SURVIVAL = X
            #####GROUPED BEHAVIORS#####
                        
                  #CARE TOTAL--------------------------------------------------------
                        
                        ps1<-glmer(EGG_SURV~scale(CARE_PROP)+
                                     (1|SUBJECT),
                                   weights = EGGS_IN_NEST,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(ps1)
                        
                        #PLOTS
                        plot(egg.data$CARE_PROP,egg.data$EGG_SURV)         
                        
                            
                  #FAN TOTAL---------------------------------------------------------
                        
                        ps2<-glmer(EGG_SURV~scale(FAN_PROP)+
                                     (1|SUBJECT),
                                   weights = EGGS_IN_NEST,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(ps2)
                        
                        #PLOTS
                        plot(egg.data$FAN_PROP,egg.data$EGG_SURV)
                        
                  #AGIT TOTAL--------------------------------------------------------
                        
                        ps3<-glmer(EGG_SURV~scale(AGIT_PROP)+
                                     (1|SUBJECT),
                                   weights = EGGS_IN_NEST,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(ps3)
                        
                        #PLOTS
                        plot(egg.data$AGIT_PROP,egg.data$EGG_SURV)
                        
            #####INDIVIDUAL BEHAVIORS#####     
                  #TFAN -------------------------------------------------------------
                        ps4<-glmer(EGG_SURV~scale(TFAN_PROP)+
                                     (1|SUBJECT),
                                   weights = EGGS_IN_NEST,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(ps4)
                        
                        #PLOTS
                        plot(egg.data$TFAN_PROP,egg.data$EGG_SURV)
                        
                  #ROCKTAIL ---------------------------------------------------------
                        ps5<-glmer(EGG_SURV~scale(ROCKTAIL_PROP)+
                                     (1|SUBJECT),
                                   weights = EGGS_IN_NEST,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(ps5)
                        
                        #PLOTS
                        plot(egg.data$ROCKTAIL_PROP,egg.data$EGG_SURV)
                  #ROCKAG -----------------------------------------------------------
                        ps6<-glmer(EGG_SURV~scale(ROCKAG_PROP)+
                                     (1|SUBJECT),
                                   weights = EGGS_IN_NEST,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(ps6)
                        
                        #PLOTS
                        plot(egg.data$ROCKAG_PROP,egg.data$EGG_SURV)
                  #WALKAG -----------------------------------------------------------
                        ps7<-glmer(EGG_SURV~scale(WALKAG_PROP)+
                                     (1|SUBJECT),
                                   weights = EGGS_IN_NEST,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(ps7)
                        
                        #PLOTS
                        plot(egg.data$WALKAG_PROP,egg.data$EGG_SURV)
                  #HEADAG -----------------------------------------------------------
                        
                        ps8<-glmer(EGG_SURV~scale(HEADAG_PROP)+
                                     (1|SUBJECT),
                                   weights = EGGS_IN_NEST,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(ps8)
                        
                        #PLOTS
                        plot(egg.data$HEADAG_PROP,egg.data$EGG_SURV)
                        
                  #CANN -------------------------------------------------------------
                        ps9<-glmer(EGG_SURV~scale(CANN_PROP)+
                                     (1|SUBJECT),
                                   weights = EGGS_IN_NEST,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(ps9)
                        
                        #PLOTS
                        plot(egg.data$CANN_PROP,egg.data$EGG_SURV)
                  #GUARD ------------------------------------------------------------
                        ps10<-glmer(EGG_SURV~scale(GUARD_PROP)+
                                     (1|SUBJECT),
                                    weights = EGGS_IN_NEST,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(ps10)
                        
                        #PLOTS
                        plot(egg.data$GUARD_PROP,egg.data$EGG_SURV)
                        
                        
#FREQ OF BEHAVIORS----------------------------------------------
                  
            #####GROUPED BEHAVIORS#####
                        
                  #CARE TOTAL--------------------------------------------------------
                        
                        fs1<-glmer(EGG_SURV~scale(CARE_MIN)+
                                     (1|SUBJECT),
                                   weights = EGGS_IN_NEST,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(fs1)
                        
                        #PLOTS
                        plot(egg.data$CARE_MIN,egg.data$EGG_SURV)         
                        
                        
                  #FAN TOTAL---------------------------------------------------------
                        
                        fs2<-glmer(EGG_SURV~scale(FAN_MIN)+
                                     (1|SUBJECT),
                                   weights = EGGS_IN_NEST,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(fs2)
                        
                        #PLOTS
                        plot(egg.data$FAN_MIN,egg.data$EGG_SURV)
                        
                  #AGIT TOTAL--------------------------------------------------------
                        
                        fs3<-glmer(EGG_SURV~scale(AGIT_MIN)+
                                     (1|SUBJECT),
                                   weights = EGGS_IN_NEST,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(fs3)
                        
                        #PLOTS
                        plot(egg.data$AGIT_MIN,egg.data$EGG_SURV)
                        
            #####INDIVIDUAL BEHAVIORS#####     
                  #TFAN -------------------------------------------------------------
                        fs4<-glmer(EGG_SURV~scale(TFAN_MIN)+
                                     (1|SUBJECT),
                                   weights = EGGS_IN_NEST,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(fs4)
                        
                        #PLOTS
                        plot(egg.data$TFAN_MIN,egg.data$EGG_SURV)
                        
                  #ROCKTAIL ---------------------------------------------------------
                        fs5<-glmer(EGG_SURV~scale(ROCKTAIL_MIN)+
                                     (1|SUBJECT),
                                   weights = EGGS_IN_NEST,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(fs5)
                        
                        #PLOTS
                        plot(egg.data$ROCKTAIL_MIN,egg.data$EGG_SURV)
                  #ROCKAG -----------------------------------------------------------
                        fs6<-glmer(EGG_SURV~scale(ROCKAG_MIN)+
                                     (1|SUBJECT),
                                   weights = EGGS_IN_NEST,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(fs6)
                        
                        #PLOTS
                        plot(egg.data$ROCKAG_MIN,egg.data$EGG_SURV)
                  #WALKAG -----------------------------------------------------------
                        fs7<-glmer(EGG_SURV~scale(WALKAG_MIN)+
                                     (1|SUBJECT),
                                   weights = EGGS_IN_NEST,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(fs7)
                        
                        #PLOTS
                        plot(egg.data$WALKAG_MIN,egg.data$EGG_SURV)
                  #HEADAG -----------------------------------------------------------
                        
                        fs8<-glmer(EGG_SURV~scale(HEADAG_MIN)+
                                     (1|SUBJECT),
                                   weights = EGGS_IN_NEST,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(fs8)
                        
                        #PLOTS
                        plot(egg.data$HEADAG_MIN,egg.data$EGG_SURV)
                        
                  #CANN -------------------------------------------------------------
                        fs9<-glmer(EGG_SURV~scale(CANN_MIN)+
                                     (1|SUBJECT),
                                   weights = EGGS_IN_NEST,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(fs9)
                        
                        #PLOTS
                        plot(egg.data$CANN_MIN,egg.data$EGG_SURV)
                  #GUARD ------------------------------------------------------------
                        fs10<-glmer(EGG_SURV~scale(GUARD_MIN)+
                                      (1|SUBJECT),
                                    weights = EGGS_IN_NEST,
                                    family = "binomial",
                                    data = egg.data)
                        
                        
                        summary(fs10)
                        
                        #PLOTS
                        plot(egg.data$GUARD_MIN,egg.data$EGG_SURV)
            
                        
#WATER QUALITY----------------------------------------------
                        
            
                  #TEMP--------------------------------------------------------
                        
                        wqs1<-glmer(EGG_SURV~scale(TEMP)+
                                     (1|SUBJECT),
                                    weights = EGGS_IN_NEST,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(wqs1)
                        
                        #PLOTS
                        plot(egg.data$TEMP,egg.data$EGG_SURV)         
                       
                       
                    #DO--------------------------------------------------------
                        
                        wqs2<-glmer(EGG_SURV~scale(DO)+
                                      (1|SUBJECT),
                                    weights = EGGS_IN_NEST,
                                    family = "binomial",
                                    data = egg.data)
                        
                        
                        summary(wqs2)
                        
                        #PLOTS
                        plot(egg.data$DO,egg.data$EGG_SURV)     
                        
                    #COND--------------------------------------------------------
                        
                        wqs3<-glmer(EGG_SURV~scale(COND)+
                                      (1|SUBJECT),
                                    weights = EGGS_IN_NEST,
                                    family = "binomial",
                                    data = egg.data)
                        
                        
                        summary(wqs3)
                        
                        #PLOTS
                        plot(egg.data$COND,egg.data$EGG_SURV)     
                        
                    #DEPTH--------------------------------------------------------
                        
                        wqs4<-glmer(EGG_SURV~scale(DEPTH)+
                                      (1|SUBJECT),
                                    weights = EGGS_IN_NEST,
                                    family = "binomial",
                                    data = egg.data)
                        
                        
                        summary(wqs4)
                        
                        #PLOTS
                        plot(egg.data$DEPTH,egg.data$EGG_SURV)    
                        
                        
#MALE SIZE----------------------------------------------
                        
                        
                    #TL-------------------------------------------------------
                        
                        ms1<-glmer(EGG_SURV~scale(TL)+
                                      (1|SUBJECT),
                                    weights = EGGS_IN_NEST,
                                    family = "binomial",
                                    data = egg.data)
                        
                        
                        summary(ms1)
                        
                        #PLOTS
                        plot(egg.data$TL,egg.data$EGG_SURV)                            
                        
                    #MALE CLASS-------------------------------------------------------
                        
                        ms2<-glmer(EGG_SURV~MALE_CLASS+
                                     (1|SUBJECT),
                                   #weights = EGGS_IN_NEST,
                                   family = "binomial",
                                   data = egg.data)
                        
                        
                        summary(ms2)
                        
                        #PLOTS
                        plot(egg.data$MALE_CLASS,egg.data$EGG_SURV)       
                        
              
                        #MEAN + SE PLOT
                        ms2.ss<-describeBy(tb.data$EGG_SURV, group=tb.data$MALE_CLASS, mat = TRUE) 
                        ms2.ss
                        
                        ms2.plot<-ggplot(ms2.ss, aes(x = group1, y= mean))+
                          theme_classic()+
                          geom_point(size = 3)+
                          geom_errorbar(aes(ymin=mean-se, ymax=mean+se), linewidth = 0.5, width=.1)+
                          labs(y="Egg Survival", x= "Male Class")
                        
                        ms2.plot 
                        
                        
                        
                        
                        
                        
                        
                        
                        
  