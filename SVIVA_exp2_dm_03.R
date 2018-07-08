### SVIVA experiment 2 data management ###

# Hi there!

#loading packages

library(tidyverse)
#library(psych)
library(car)
library(stringr)
library(lubridate)


#loading raw data CSV file
SVIVA2_raw_00 <- read.csv("SVIVA_exp2.csv")
CITY_00 <- read.csv("CITY.csv")

SVIVA2_raw <- SVIVA2_raw_00 %>% filter(V10==1)
CITY <- CITY_00 %>% filter(V10==1)


SVIVA2_raw[is.na(SVIVA2_raw)] <- 0

SVIVA2_00 = SVIVA2_raw %>%
  dplyr::select(V4,V5) %>%
  mutate(IP = SVIVA2_raw$V6,
         USER_ID = SVIVA2_raw$i.user3,
         START_DATE = dmy_hm(SVIVA2_raw$V8),
         END_DATE = dmy_hm(SVIVA2_raw$V9)) %>%  
         mutate(TIMER_total = as.integer(END_DATE-START_DATE),
         FINISHED = SVIVA2_raw$V10) %>%
  dplyr::select(-c(1:2)) %>%
  
  ##Define variables####


##interface
mutate(MOBILE = if_else(SVIVA2_raw$Q23.1+SVIVA2_raw$Q22.1==1,1,0)) %>%
  
  #controls####
mutate(ENVIRONMENT_INTEREST = Recode(SVIVA2_raw$Q3.2,"0=NA"),
       ENVIRONMENT_FOLLOW = Recode(SVIVA2_raw$Q3.3,"0=NA"),
       ENVIRONMENT_PREFERENCE = Recode(SVIVA2_raw$Q3.4,"0=NA"),
       GOV_EVALUATION = SVIVA2_raw$Q2.3,
       GOV_TRUST = SVIVA2_raw$Q2.4,
       IDEOLOGY = Recode(SVIVA2_raw$Q2.5,"0=NA"),
       CHILDREN = if_else(SVIVA2_raw$Q313==5,0,1),
       CHILDREN_young = if_else(SVIVA2_raw$Q313 %in% c(1,2),1,0),
       FACTORIES_relatives = Recode(SVIVA2_raw$Q314,"0=NA;1=0;2=1"))%>%
mutate(IDEOLOGY=IDEOLOGY-1) %>%   
  
  ##demographics
  mutate(AGE = Recode(SVIVA2_raw$Q24.2_1,"0=NA"),
         EDUCATION = Recode(SVIVA2_raw$Q24.3,"0=NA"),
         INCOME = Recode(SVIVA2_raw$Q24.4,"0=NA"),
         HOME = Recode(SVIVA2_raw$Q24.5,"0=NA"),
         GENDER = Recode(SVIVA2_raw$Q24.10,"0=NA;1=0;2=1;3=NA"),
         HEBREW = SVIVA2_raw$Q24.11)#%>%


SVIVA2_00 = SVIVA2_00 %>%   
  
  #conditions####
### relevance
mutate(RELEVANCE_exp = if_else(SVIVA2_raw$DO.BR.FL_346=="Relevance_treatment",1,0))%>%
  ###symbol
  mutate(SYMBOL = Recode(SVIVA2_raw$DO.BR.FL_214,
                         "'POLICY PROGRAMS intro_control'=0;
                         'POLICY PROGRAMS intro_placebo'=1;
                         'POLICY PROGRAMS intro_symbol'=2"))%>%
  mutate(SYMBOL_bi = as.integer(Recode(SYMBOL,"1=0;2=1"))) %>% 
  ###policy order
  mutate(t1 = paste(SVIVA2_raw$DO.BR.FL_269,
                    SVIVA2_raw$DO.BR.FL_273,
                    SVIVA2_raw$DO.BR.FL_291,
                    SVIVA2_raw$DO.BR.FL_365,
                    SVIVA2_raw$DO.BR.FL_375,
                    SVIVA2_raw$DO.BR.FL_385)) #%>% 

SVIVA2_00 = SVIVA2_00 %>% 
  mutate(AIR_order = if_else(str_detect(SVIVA2_00$t1, "air")==TRUE,1,2))%>%
  mutate(WASTE_order = if_else(str_detect(SVIVA2_00$t1, "waste")==TRUE,1,2))%>%
  ###information
  mutate(INFORMATION_air = if_else(str_detect(SVIVA2_00$t1, "air a")==TRUE|
                                     str_detect(SVIVA2_00$t1, "air b")==TRUE|
                                     str_detect(SVIVA2_00$t1, "air c")==TRUE|
                                     str_detect(SVIVA2_00$t1, "waste d")==TRUE|
                                     str_detect(SVIVA2_00$t1, "waste e")==TRUE|
                                     str_detect(SVIVA2_00$t1, "waste f")==TRUE,1,0))%>%
  mutate(INFORMATION_waste = Recode(INFORMATION_air,"0=1;1=0"))%>%
  mutate(CONDITION_air = if_else(SYMBOL==2&INFORMATION_air==1,1,
                                 if_else(SYMBOL==1&INFORMATION_air==1,2,
                                         if_else(SYMBOL==0&INFORMATION_air==1,3,
                                                 if_else(SYMBOL==2&INFORMATION_air==0,4,
                                                         if_else(SYMBOL==1&INFORMATION_air==0,5,
                                                                 if_else(SYMBOL==0&INFORMATION_air==0,6,0)))))))%>%
  mutate(CONDITION_waste = if_else(SYMBOL==2&INFORMATION_waste==1,1,
                                   if_else(SYMBOL==1&INFORMATION_waste==1,2,
                                           if_else(SYMBOL==0&INFORMATION_waste==1,3,
                                                   if_else(SYMBOL==2&INFORMATION_waste==0,4,
                                                           if_else(SYMBOL==1&INFORMATION_waste==0,5,
                                                                   if_else(SYMBOL==0&INFORMATION_waste==0,6,0)))))))%>%
  mutate(CONDITION_air =  Recode(CONDITION_air,"1='A';2='B';3='C';4='D';5='E';6='F'")) %>%
  mutate(CONDITION_waste =  Recode(CONDITION_waste,"1='A';2='B';3='C';4='D';5='E';6='F'")) #%>% 


SVIVA2_00 = SVIVA2_00 %>% 
  #Area
  mutate(AREA_reside = SVIVA2_raw$Q24.6+SVIVA2_raw$Q3.5,
         CITY_reside_raw = paste(SVIVA2_raw$Q3.6,SVIVA2_raw$Q24.7),
         CITY_reside_he = CITY$CITY_reside_he,
         CITY_reside_en = CITY$CITY_reside_en,
         AREA_work = SVIVA2_raw$Q24.9,
         CITY_reside_youth =SVIVA2_raw$Q24.8)

t2=if_else(SVIVA2_00$CITY_reside_en %in% c("HAIFA",
                                           "NESHER",
                                           "OTHER (HAIFA BAY)",
                                           "QIRYAT ATA",
                                           "QIRYAT BIALIK",
                                           "QIRYAT HAIM",
                                           "QIRYAT MOZKIN",
                                           "QIRYAT YAM"),"HAIFA","SHFELA")
SVIVA2_00 = SVIVA2_00 %>% 
  mutate(AREA_reside_haifa = Recode(t2,"'HAIFA'=1;'SHFELA'=0"),
         AREA_work_haifa = if_else(AREA_work==2,1,0))%>%
  mutate(AREA = if_else(AREA_reside_haifa+AREA_work_haifa>0,1,0))%>%
  mutate(AREA_names = Recode(AREA,"0='Center';1='Haifa-Bay'"))%>%
  #ouctome vars####
#trust in Haifa Bay policy
mutate(TRUST_air_q1 =  SVIVA2_raw$Q8.5+
         SVIVA2_raw$Q9.5+
         SVIVA2_raw$Q12.5+
         SVIVA2_raw$Q13.5+
         SVIVA2_raw$Q16.5+
         SVIVA2_raw$Q17.5+
         SVIVA2_raw$Q30.4_1+
         SVIVA2_raw$Q31.4_1+
         SVIVA2_raw$Q34.4_1+
         SVIVA2_raw$Q35.4_1+
         SVIVA2_raw$Q38.4_1+
         SVIVA2_raw$Q39.4_1,
       TRUST_air_q2 =  SVIVA2_raw$Q8.6+
         SVIVA2_raw$Q9.6+
         SVIVA2_raw$Q12.6+
         SVIVA2_raw$Q13.6+
         SVIVA2_raw$Q16.6+
         SVIVA2_raw$Q17.6+
         SVIVA2_raw$Q30.4_2+
         SVIVA2_raw$Q31.4_2+
         SVIVA2_raw$Q34.4_2+
         SVIVA2_raw$Q35.4_2+
         SVIVA2_raw$Q38.4_2+
         SVIVA2_raw$Q39.4_2,
       TRUST_air_q3 =  SVIVA2_raw$Q8.7+
         SVIVA2_raw$Q9.7+
         SVIVA2_raw$Q12.7+
         SVIVA2_raw$Q13.7+
         SVIVA2_raw$Q16.7+
         SVIVA2_raw$Q17.7+
         SVIVA2_raw$Q30.4_3+
         SVIVA2_raw$Q31.4_3+
         SVIVA2_raw$Q34.4_3+
         SVIVA2_raw$Q35.4_3+
         SVIVA2_raw$Q38.4_3+
         SVIVA2_raw$Q39.4_3,
       TRUST_air_q4 =  SVIVA2_raw$Q8.8+
         SVIVA2_raw$Q9.8+
         SVIVA2_raw$Q12.8+
         SVIVA2_raw$Q13.8+
         SVIVA2_raw$Q16.8+
         SVIVA2_raw$Q17.8+
         SVIVA2_raw$Q30.4_4+
         SVIVA2_raw$Q31.4_4+
         SVIVA2_raw$Q34.4_4+
         SVIVA2_raw$Q35.4_4+
         SVIVA2_raw$Q38.4_4+
         SVIVA2_raw$Q39.4_4,
       TRUST_air_q5 =  SVIVA2_raw$Q8.9+
         SVIVA2_raw$Q9.9+
         SVIVA2_raw$Q12.9+
         SVIVA2_raw$Q13.9+
         SVIVA2_raw$Q16.9+
         SVIVA2_raw$Q17.9+
         SVIVA2_raw$Q30.4_5+
         SVIVA2_raw$Q31.4_5+
         SVIVA2_raw$Q34.4_5+
         SVIVA2_raw$Q35.4_5+
         SVIVA2_raw$Q38.4_5+
         SVIVA2_raw$Q39.4_5,
       TRUST_air_q6 =  SVIVA2_raw$Q8.10+
         SVIVA2_raw$Q9.10+
         SVIVA2_raw$Q12.10+
         SVIVA2_raw$Q13.10+
         SVIVA2_raw$Q16.10+
         SVIVA2_raw$Q17.10+
         SVIVA2_raw$Q30.4_6+
         SVIVA2_raw$Q31.4_6+
         SVIVA2_raw$Q34.4_6+
         SVIVA2_raw$Q35.4_6+
         SVIVA2_raw$Q38.4_6+
         SVIVA2_raw$Q39.4_6)%>%
  mutate(TRUST_air_competence = (TRUST_air_q1+TRUST_air_q2)/2,
         TRUST_air_benevolence = (TRUST_air_q3+TRUST_air_q4)/2,
         TRUST_air_integrity = (TRUST_air_q5+TRUST_air_q6)/2,
         TRUST_air_INDEX = (TRUST_air_q1+
                              TRUST_air_q2+
                              TRUST_air_q3+
                              TRUST_air_q4+
                              TRUST_air_q5+
                              TRUST_air_q6)/6)%>%
  mutate(TRUST_air_timer = SVIVA2_raw$Q8.11_3+
                           SVIVA2_raw$Q9.11_3+
                           SVIVA2_raw$Q12.11_3+
                           SVIVA2_raw$Q13.11_3+
                           SVIVA2_raw$Q16.11_3+
                           SVIVA2_raw$Q17.11_3+
                           SVIVA2_raw$Q30.5_3+
                           SVIVA2_raw$Q31.5_3+
                           SVIVA2_raw$Q34.5_3+
                           SVIVA2_raw$Q35.5_3+
                           SVIVA2_raw$Q38.5_3+
                           SVIVA2_raw$Q39.5_3) %>% 
  #trust in waste policy
  mutate(TRUST_waste_q1 =  SVIVA2_raw$Q10.5+
           SVIVA2_raw$Q11.5+
           SVIVA2_raw$Q14.5+
           SVIVA2_raw$Q15.5+
           SVIVA2_raw$Q18.5+
           SVIVA2_raw$Q19.5+
           SVIVA2_raw$Q32.4_1+
           SVIVA2_raw$Q33.4_1+
           SVIVA2_raw$Q36.4_1+
           SVIVA2_raw$Q37.4_1+
           SVIVA2_raw$Q40.4_1+
           SVIVA2_raw$Q41.4_1,
         TRUST_waste_q2 =  SVIVA2_raw$Q10.6+
           SVIVA2_raw$Q11.6+
           SVIVA2_raw$Q14.6+
           SVIVA2_raw$Q15.6+
           SVIVA2_raw$Q18.6+
           SVIVA2_raw$Q19.6+
           SVIVA2_raw$Q32.4_2+
           SVIVA2_raw$Q33.4_2+
           SVIVA2_raw$Q36.4_2+
           SVIVA2_raw$Q37.4_2+
           SVIVA2_raw$Q40.4_2+
           SVIVA2_raw$Q41.4_2,
         TRUST_waste_q3 =  SVIVA2_raw$Q10.7+
           SVIVA2_raw$Q11.7+
           SVIVA2_raw$Q14.7+
           SVIVA2_raw$Q15.7+
           SVIVA2_raw$Q18.7+
           SVIVA2_raw$Q19.7+
           SVIVA2_raw$Q32.4_3+
           SVIVA2_raw$Q33.4_3+
           SVIVA2_raw$Q36.4_3+
           SVIVA2_raw$Q37.4_3+
           SVIVA2_raw$Q40.4_3+
           SVIVA2_raw$Q41.4_3,
         TRUST_waste_q4 =  SVIVA2_raw$Q10.8+
           SVIVA2_raw$Q11.8+
           SVIVA2_raw$Q14.8+
           SVIVA2_raw$Q15.8+
           SVIVA2_raw$Q18.8+
           SVIVA2_raw$Q19.8+
           SVIVA2_raw$Q32.4_4+
           SVIVA2_raw$Q33.4_4+
           SVIVA2_raw$Q36.4_4+
           SVIVA2_raw$Q37.4_4+
           SVIVA2_raw$Q40.4_4+
           SVIVA2_raw$Q41.4_4,
         TRUST_waste_q5 =  SVIVA2_raw$Q10.9+
           SVIVA2_raw$Q11.9+
           SVIVA2_raw$Q14.9+
           SVIVA2_raw$Q15.9+
           SVIVA2_raw$Q18.9+
           SVIVA2_raw$Q19.9+
           SVIVA2_raw$Q32.4_5+
           SVIVA2_raw$Q33.4_5+
           SVIVA2_raw$Q36.4_5+
           SVIVA2_raw$Q37.4_5+
           SVIVA2_raw$Q40.4_5+
           SVIVA2_raw$Q41.4_5,
         TRUST_waste_q6 =  SVIVA2_raw$Q10.10+
           SVIVA2_raw$Q11.10+
           SVIVA2_raw$Q14.10+
           SVIVA2_raw$Q15.10+
           SVIVA2_raw$Q18.10+
           SVIVA2_raw$Q19.10+
           SVIVA2_raw$Q32.4_6+
           SVIVA2_raw$Q33.4_6+
           SVIVA2_raw$Q36.4_6+
           SVIVA2_raw$Q37.4_6+
           SVIVA2_raw$Q40.4_6+
           SVIVA2_raw$Q41.4_6)%>%
  mutate(TRUST_waste_competence = (TRUST_waste_q1+TRUST_waste_q2)/2,
         TRUST_waste_benevolence = (TRUST_waste_q3+TRUST_waste_q4)/2,
         TRUST_waste_integrity = (TRUST_waste_q5+TRUST_waste_q6)/2,
         TRUST_waste_INDEX = (TRUST_waste_q1+
                                TRUST_waste_q2+
                                TRUST_waste_q3+
                                TRUST_waste_q4+
                                TRUST_waste_q5+
                                TRUST_waste_q6)/6)%>%

      mutate(TRUST_waste_timer = SVIVA2_raw$Q10.11_3+
             SVIVA2_raw$Q11.11_3+
             SVIVA2_raw$Q14.11_3+
             SVIVA2_raw$Q15.11_3+
             SVIVA2_raw$Q18.11_3+
             SVIVA2_raw$Q19.11_3+
             SVIVA2_raw$Q32.5_3+
             SVIVA2_raw$Q33.5_3+
             SVIVA2_raw$Q36.5_3+
             SVIVA2_raw$Q37.5_3+
             SVIVA2_raw$Q40.5_3+
             SVIVA2_raw$Q41.5_3) %>% 
  
    
  #IMC
  mutate(IMC_item = paste0(SVIVA2_raw$Q42.5_TEXT,
                           SVIVA2_raw$Q20.5_TEXT,
                           SVIVA2_raw$Q21.5_TEXT,
                           SVIVA2_raw$Q22.10_TEXT,
                           SVIVA2_raw$Q23.10_TEXT)) %>%
  mutate(IMC = if_else(str_detect(IMC_item, "9")==TRUE,1,0)) %>%


#elaboration of policy plans
###perceived elaboration
  mutate(ELABORATION_air_q1 = SVIVA2_raw$Q23.1+SVIVA2_raw$Q20.1_1,
         ELABORATION_air_q2 = SVIVA2_raw$Q23.2+SVIVA2_raw$Q20.1_2,
         ELABORATION_air_q3 = SVIVA2_raw$Q23.3+SVIVA2_raw$Q20.1_3,
         ELABORATION_air_q4 = SVIVA2_raw$Q23.4+SVIVA2_raw$Q20.1_4,
         ELABORATION_air_q5 = SVIVA2_raw$Q23.5+SVIVA2_raw$Q20.1_5)%>%
  mutate(ELABORATION_air_q1 = Recode(ELABORATION_air_q1,"0=NA"),
         ELABORATION_air_q2 = Recode(ELABORATION_air_q2,"0=NA"),
         ELABORATION_air_q3 = Recode(ELABORATION_air_q3,"0=NA"),
         ELABORATION_air_q4 = Recode(ELABORATION_air_q4,"0=NA"),
         ELABORATION_air_q5 = Recode(ELABORATION_air_q5,"0=NA"))%>%
  mutate(ELABORATION_air_q2.r = Recode(ELABORATION_air_q2,
                                       "1=7;2=6;3=5;5=3;6=2;7=1"),
         ELABORATION_air_q5.r = Recode(ELABORATION_air_q5,
                                       "1=7;2=6;3=5;5=3;6=2;7=1"))%>%
  mutate(ELABORATION_air_INDEX = (ELABORATION_air_q1+
                                    ELABORATION_air_q2.r+
                                    ELABORATION_air_q3+
                                    ELABORATION_air_q4+ 
                                    ELABORATION_air_q5.r)/5) %>%
  mutate(ELABORATION_air_INDEX_01 = (ELABORATION_air_q1+
                                    ELABORATION_air_q2.r+
                                    ELABORATION_air_q3+
                                     ELABORATION_air_q5.r)/4) %>%
  
  mutate(ELABORATION_waste_q1 = SVIVA2_raw$Q22.1+SVIVA2_raw$Q42.1_1,
         ELABORATION_waste_q2 = SVIVA2_raw$Q22.2+SVIVA2_raw$Q42.1_2,
         ELABORATION_waste_q3 = SVIVA2_raw$Q22.3+SVIVA2_raw$Q42.1_3,
         ELABORATION_waste_q4 = SVIVA2_raw$Q22.4+SVIVA2_raw$Q42.1_4,
         ELABORATION_waste_q5 = SVIVA2_raw$Q22.5+SVIVA2_raw$Q42.1_5)%>%
  mutate(ELABORATION_waste_q1 = Recode(ELABORATION_waste_q1,"0=NA"),
         ELABORATION_waste_q2 = Recode(ELABORATION_waste_q2,"0=NA"),
         ELABORATION_waste_q3 = Recode(ELABORATION_waste_q3,"0=NA"),
         ELABORATION_waste_q4 = Recode(ELABORATION_waste_q4,"0=NA"),
         ELABORATION_waste_q5 = Recode(ELABORATION_waste_q5,"0=NA"))%>%
  
  mutate(ELABORATION_waste_q2.r = Recode(ELABORATION_waste_q2,
                                         "1=7;2=6;3=5;5=3;6=2;7=1"),
         ELABORATION_waste_q5.r = Recode(ELABORATION_waste_q5,
                                         "1=7;2=6;3=5;5=3;6=2;7=1"))%>%      
  mutate(ELABORATION_waste_INDEX = (ELABORATION_waste_q1+
                                    ELABORATION_waste_q2.r+
                                    ELABORATION_waste_q3+
                                    ELABORATION_waste_q4+ 
                                    ELABORATION_waste_q5.r)/5) %>%
  mutate(ELABORATION_waste_INDEX_01 = (ELABORATION_waste_q1+
                                      ELABORATION_waste_q2.r+
                                      ELABORATION_waste_q3+
                                      ELABORATION_waste_q5.r)/4) %>%
  
  ###elaboration time
  mutate(ELABORATION_air_time = SVIVA2_raw$Q8.2_3+
           SVIVA2_raw$Q9.2_3+
           SVIVA2_raw$Q12.2_3+
           SVIVA2_raw$Q13.2_3+
           SVIVA2_raw$Q16.2_3+
           SVIVA2_raw$Q17.2_3+
           SVIVA2_raw$Q30.2_3+
           SVIVA2_raw$Q31.2_3+
           SVIVA2_raw$Q34.2_3+
           SVIVA2_raw$Q35.2_3+
           SVIVA2_raw$Q38.2_3+
           SVIVA2_raw$Q39.2_3,
         ELABORATION_waste_time = SVIVA2_raw$Q10.2_3+
           SVIVA2_raw$Q11.2_3+
           SVIVA2_raw$Q14.2_3+
           SVIVA2_raw$Q15.2_3+
           SVIVA2_raw$Q18.2_3+
           SVIVA2_raw$Q19.2_3+
           SVIVA2_raw$Q32.2_3+
           SVIVA2_raw$Q33.2_3+
           SVIVA2_raw$Q36.2_3+
           SVIVA2_raw$Q37.2_3+
           SVIVA2_raw$Q40.2_3+
           SVIVA2_raw$Q41.2_3)%>%
  mutate(ELABORATION_air_time_log = log(ELABORATION_air_time+1),
         ELABORATION_waste_time_log = log(ELABORATION_waste_time+1)) %>%
  mutate(ELABORATION_total_time_log = log(ELABORATION_air_time+ELABORATION_waste_time+1)) %>% 

###memory score  
  mutate(MEMORY_air_correct = ifelse(AIR_order==2,
                                      if_else(INFORMATION_air==1,
                                              SVIVA2_raw$Q23.7_1+SVIVA2_raw$Q23.7_2+
                                                SVIVA2_raw$Q20.2_1+SVIVA2_raw$Q20.2_2,
                                              SVIVA2_raw$Q23.7_1+SVIVA2_raw$Q23.7_4+
                                                SVIVA2_raw$Q20.2_1+SVIVA2_raw$Q20.2_4),NA)) %>% 
  mutate(MEMORY_air_incorrect = ifelse(AIR_order==2,
                                        if_else(INFORMATION_air==1,
                                                SVIVA2_raw$Q23.7_4+SVIVA2_raw$Q23.7_5+
                                                  SVIVA2_raw$Q20.2_4+SVIVA2_raw$Q20.2_5,
                                                SVIVA2_raw$Q23.7_2+SVIVA2_raw$Q23.7_5+
                                                  SVIVA2_raw$Q20.2_2+SVIVA2_raw$Q20.2_5),NA)) %>%  
  mutate(MEMORY_air_score = if_else(MEMORY_air_correct-MEMORY_air_incorrect>=0,
                                    MEMORY_air_correct-MEMORY_air_incorrect,0)) %>%  
  
  mutate(MEMORY_waste_correct = ifelse(WASTE_order==2,
                                        if_else(INFORMATION_waste==1,
                                                SVIVA2_raw$Q22.7_1+SVIVA2_raw$Q22.7_2+
                                                  SVIVA2_raw$Q20.2_1+SVIVA2_raw$Q20.2_2,
                                                SVIVA2_raw$Q22.7_1+SVIVA2_raw$Q22.7_4+
                                                  SVIVA2_raw$Q20.2_1+SVIVA2_raw$Q20.2_4),NA)) %>%  
  mutate(MEMORY_waste_incorrect = ifelse(WASTE_order==2,
                                          if_else(INFORMATION_waste==1,
                                                  SVIVA2_raw$Q22.7_4+SVIVA2_raw$Q22.7_5+
                                                  SVIVA2_raw$Q42.2_4+SVIVA2_raw$Q42.2_5,
                                                  SVIVA2_raw$Q22.7_2+SVIVA2_raw$Q22.7_5+
                                                  SVIVA2_raw$Q42.2_2+SVIVA2_raw$Q42.2_5),NA)) %>% 
  mutate(MEMORY_waste_score = if_else(MEMORY_waste_correct-MEMORY_waste_incorrect>=0,
                                      MEMORY_waste_correct-MEMORY_waste_incorrect,0)) %>% 
  mutate(MEMORY_total = ifelse(!(MEMORY_air_score %in% c(NA)),MEMORY_air_score,MEMORY_waste_score)) %>% 
  

##perceived personal relevance (observational)
mutate(RELEVANCE_air_obs =  SVIVA2_raw$Q20.3+
         SVIVA2_raw$Q22.9+
         SVIVA2_raw$Q23.8+
         SVIVA2_raw$Q42.4,
       RELEVANCE_waste_obs =  SVIVA2_raw$Q20.4+
         SVIVA2_raw$Q22.8+
         SVIVA2_raw$Q23.9+
         SVIVA2_raw$Q42.3) %>% 



#man. checks####
mutate(RECOGNIZE_waste_real = SVIVA2_raw$Q25.6+
                              SVIVA2_raw$Q27.16+
                              SVIVA2_raw$Q29.6,
       RECOGNIZE_air_real = SVIVA2_raw$Q25.11+
                               SVIVA2_raw$Q27.20+
                               SVIVA2_raw$Q29.11,
       RECOGNIZE_SVIVA_logo = SVIVA2_raw$Q316+
                               SVIVA2_raw$Q318+
                               SVIVA2_raw$Q320,
       AFFECT_waste_real = SVIVA2_raw$Q25.4+
                           SVIVA2_raw$Q27.14+
                           SVIVA2_raw$Q29.4,
       AFFECT_air_real = SVIVA2_raw$Q25.9+
                           SVIVA2_raw$Q27.18+
                           SVIVA2_raw$Q29.9,
       AFFECT_waste_fake = ifelse(SYMBOL==1,SVIVA2_raw$Q27.4,NA),
       AFFECT_air_fake = ifelse(SYMBOL==1,SVIVA2_raw$Q27.9,NA)) %>% 
  mutate(SYMBOL_n = Recode(SYMBOL,"0='0 no symbols';
                                      1='1 fake symbols';
                                      2='2 Real symbols'"),
         INFORMATION_air_n = Recode(INFORMATION_air,"0='0 weak policy';
                                                    1='1 strong policy'"),
         RELEVANCE_exp_n = Recode(RELEVANCE_exp,"0='0 control';
                                                    1='1 treatment'"),
         INFORMATION_waste_n = Recode(INFORMATION_waste,"0='0 weak policy';
                                               1='1 strong policy'")) %>% 
  mutate(REPETITION_elaboration = if_else(AIR_order==2,
                                          if_else(ELABORATION_air_INDEX!=4 &
                                                    ELABORATION_air_q1==ELABORATION_air_q2 &
                                                    ELABORATION_air_q1==ELABORATION_air_q3 &
                                                    ELABORATION_air_q1==ELABORATION_air_q4 &
                                                    ELABORATION_air_q1==ELABORATION_air_q5,1,0),
                                          if_else(ELABORATION_waste_INDEX!=4 &
                                                    ELABORATION_waste_q1==ELABORATION_waste_q2 &
                                                    ELABORATION_waste_q1==ELABORATION_waste_q3 &
                                                    ELABORATION_waste_q1==ELABORATION_waste_q4 &
                                                    ELABORATION_waste_q1==ELABORATION_waste_q5,1,0))) %>% 
  mutate(RELEVANCE_exp_treat_text = SVIVA2_raw$Q3.7,
         RELEVANCE_exp_control_text = SVIVA2_raw$Q4.7)%>%
  
  mutate(WITHIN_DELTA = if_else(INFORMATION_air==1,
                         TRUST_air_INDEX-TRUST_waste_INDEX,
                         TRUST_waste_INDEX-TRUST_air_INDEX))

####Datasets#####

SVIVA2_01 = SVIVA2_00 %>%
  distinct(IP,.keep_all=TRUE) %>% #exclude double IP
  filter(!(AGE %in% 1:17),
         IMC==1,
         TIMER_total>=3,TIMER_total<=30,  
         TRUST_air_timer>=10|
           TRUST_waste_timer>=10, 
         REPETITION_elaboration==0)
       

SVIVA2_01_comb = SVIVA2_01 %>% 
  dplyr::select(IP,USER_ID,AREA,AREA_names,
      RELEVANCE_exp,RELEVANCE_exp_n,
      SYMBOL,SYMBOL_n,
      INFORMATION_air,INFORMATION_air_n,
      INFORMATION_waste,INFORMATION_waste_n,
      TRUST_air_INDEX,
      TRUST_waste_INDEX,
      ELABORATION_air_time_log,
      ELABORATION_waste_time_log,
      ELABORATION_air_time,
      ELABORATION_waste_time,
      MEMORY_air_score,
      MEMORY_waste_score,
      GOV_TRUST,
      GENDER,
      AIR_order,
      WASTE_order,
      IDEOLOGY,
      INCOME,
      EDUCATION,
      RECOGNIZE_SVIVA_logo,
      RECOGNIZE_air_real,
      RECOGNIZE_waste_real,
      RELEVANCE_air_obs,
      RELEVANCE_waste_obs,
      ENVIRONMENT_INTEREST,
      ENVIRONMENT_FOLLOW,
      CHILDREN,
      CHILDREN_young) %>% 
  gather(key=policy,value=trust,TRUST_air_INDEX,TRUST_waste_INDEX) %>% 
  mutate(INFORMATION = ifelse(policy=="TRUST_air_INDEX",INFORMATION_air,
                              INFORMATION_waste),
         INFORMATION_n = ifelse(policy=="TRUST_air_INDEX",INFORMATION_air_n,
                                INFORMATION_waste_n),
         ELABORATION_time_log = ifelse(policy=="TRUST_air_INDEX",ELABORATION_air_time_log,
                                       ELABORATION_waste_time_log),
         MEMORY_score = ifelse(policy=="TRUST_air_INDEX",MEMORY_air_score,
                               MEMORY_waste_score),
         ELABORATION_time = ifelse(policy=="TRUST_air_INDEX",RELEVANCE_air_obs,
                                   RELEVANCE_waste_obs),
         RELEVANCE_obs = ifelse(policy=="TRUST_air_INDEX",ELABORATION_air_time,
                                ELABORATION_waste_time)) %>% 
  mutate(TRUST_air_INDEX = ifelse(policy=="TRUST_air_INDEX",trust,NA),
         TRUST_waste_INDEX = ifelse(policy=="TRUST_waste_INDEX",trust,NA),
         INFORMATION_weak = Recode(INFORMATION,"0=1;1=0"),
         AREA_center = Recode(AREA,"0=1;1=0"),
         SYMBOL_t = factor(Recode(SYMBOL,"1=2;2=1")),
         SYMBOL_t.r = factor(Recode(SYMBOL,"0=2;2=1;1=0")),
         SYMBOL_t.r_2 = factor(Recode(SYMBOL,"0=2;2=0")),
         ORDER = ifelse(policy=="TRUST_air_INDEX",AIR_order,WASTE_order),
         RECOGNIZE_campaign = ifelse(policy=="TRUST_air_INDEX",RECOGNIZE_air_real,RECOGNIZE_waste_real))


save.image("SVIVA_R_ENV.RData")

       
       
       
       
