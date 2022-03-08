##################################################################################################################################################################################################################
##################################################################################################################################################################################################################
#set print options
options(max.print=1000000)

##################################################################################################################################################################################################################
##################################################################################################################################################################################################################
#load packages
require(MBNMAtime)
require(multinma)
require(meta)
require(metafor)
require(tidyverse)
require(readxl)
require(data.table)
require(igraph)
require(purrr)
require(netmeta)
require(magrittr)
require(dmetar)

##################################################################################################################################################################################################################
##################################################################################################################################################################################################################
#resources

#https://cran.r-project.org/web/packages/MBNMAtime/vignettes/MBNMAtime.html

##################################################################################################################################################################################################################
##################################################################################################################################################################################################################
#load data sets

#data pooled groups where primary class is the same
dat_prim_class<- PrimaryClass_CollatedData <- read.delim("C:/Users/tsaue/OneDrive/Tobias/Bücher Tobias/NMA_chronicLBP/NMA_Chronic/PrimaryClass_CollatedData.txt")

#data pooled groups where treatment within class is same
dat_prim_treat<-PrimaryTreatment_CollatedData <- read.delim("C:/Users/tsaue/OneDrive/Tobias/Bücher Tobias/NMA_chronicLBP/NMA_Chronic/PrimaryTreatment_CollatedData.txt")
##################################################################################################################################################################################################################
##################################################################################################################################################################################################################
#Eliminate studies with the same primary treatment
dat_prim_class %>% filter(INTDiff.Same=="INT_Diff")->dat_prim_class
dat_prim_treat %>% filter(INTDiff.Same=="INT_Diff")->dat_prim_treat

##################################################################################################################################################################################################################
##################################################################################################################################################################################################################
#Adjust for clustering 
#https://training.cochrane.org/handbook/current/chapter-23#section-23-1

#find cluster RCT
if(FALSE) { 
dat_prim_class %>% filter(Study_Design=="Cluster randomized trial")->dat_prim_class_cluster
dat_prim_treat %>% filter(Study_Design=="Cluster randomized trial")->dat_prim_treat_cluster
}
#Kainz 2006 
M<-(1274/7) # mean cluster size
#ICC
ICC<-0.03 # Becker et al. 2008 (included cluster RCT) used that for power calculations
DE_kainz<-1+(M-1)*ICC #calculation is per Cochrane
DE_kainz #the sample size adjust value.

dat_prim_class %>% mutate(POPULATION_N.enrolled =ifelse(INFO_Author.Year=="Kainz 2006",round(POPULATION_N.enrolled/DE_kainz),POPULATION_N.enrolled))->dat_prim_class
dat_prim_class %>% mutate(Back.Pain_N =ifelse(INFO_Author.Year=="Kainz 2006",round(Back.Pain_N/DE_kainz),Back.Pain_N))->dat_prim_class
dat_prim_class %>% mutate(Leg.or.Sciatic.Pain_N =ifelse(INFO_Author.Year=="Kainz 2006",round(Leg.or.Sciatic.Pain_N/DE_kainz),Leg.or.Sciatic.Pain_N))->dat_prim_class
dat_prim_class %>% mutate(Disability_N =ifelse(INFO_Author.Year=="Kainz 2006",round(Disability_N/DE_kainz),Disability_N))->dat_prim_class
dat_prim_class %>% mutate(Mental.Health_N =ifelse(INFO_Author.Year=="Kainz 2006",round(Mental.Health_N /DE_kainz),Mental.Health_N ))->dat_prim_class

dat_prim_treat %>% mutate(POPULATION_N.enrolled =ifelse(INFO_Author.Year=="Kainz 2006",round(POPULATION_N.enrolled/DE_kainz),POPULATION_N.enrolled))->dat_prim_treat
dat_prim_treat %>% mutate(Back.Pain_N =ifelse(INFO_Author.Year=="Kainz 2006",round(Back.Pain_N/DE_kainz),Back.Pain_N))->dat_prim_treat
dat_prim_treat %>% mutate(Leg.or.Sciatic.Pain_N =ifelse(INFO_Author.Year=="Kainz 2006",round(Leg.or.Sciatic.Pain_N/DE_kainz),Leg.or.Sciatic.Pain_N))->dat_prim_treat
dat_prim_treat %>% mutate(Disability_N =ifelse(INFO_Author.Year=="Kainz 2006",round(Disability_N/DE_kainz),Disability_N))->dat_prim_treat
dat_prim_treat %>% mutate(Mental.Health_N =ifelse(INFO_Author.Year=="Kainz 2006",round(Mental.Health_N /DE_kainz),Mental.Health_N ))->dat_prim_treat


#Becker 2008
M<-(1378/118) # mean cluster size
#ICC
ICC<-0.03 # Becker et al. 2008 used that for power calculations
DE_becker<-1+(M-1)*ICC #calculation is per Cochrane
DE_becker #the sample size adjust value.

dat_prim_class %>% mutate(POPULATION_N.enrolled =ifelse(INFO_Author.Year=="Becker 2008",round(POPULATION_N.enrolled/DE_becker),POPULATION_N.enrolled))->dat_prim_class
dat_prim_class %>% mutate(Back.Pain_N =ifelse(INFO_Author.Year=="Becker 2008",round(Back.Pain_N/DE_becker),Back.Pain_N))->dat_prim_class
dat_prim_class %>% mutate(Leg.or.Sciatic.Pain_N =ifelse(INFO_Author.Year=="Becker 2008",round(Leg.or.Sciatic.Pain_N/DE_becker),Leg.or.Sciatic.Pain_N))->dat_prim_class
dat_prim_class %>% mutate(Disability_N =ifelse(INFO_Author.Year=="Becker 2008",round(Disability_N/DE_becker),Disability_N))->dat_prim_class
dat_prim_class %>% mutate(Mental.Health_N =ifelse(INFO_Author.Year=="Becker 2008",round(Mental.Health_N /DE_becker),Mental.Health_N ))->dat_prim_class

dat_prim_treat %>% mutate(POPULATION_N.enrolled =ifelse(INFO_Author.Year=="Becker 2008",round(POPULATION_N.enrolled/DE_becker),POPULATION_N.enrolled))->dat_prim_treat
dat_prim_treat %>% mutate(Back.Pain_N =ifelse(INFO_Author.Year=="Becker 2008",round(Back.Pain_N/DE_becker),Back.Pain_N))->dat_prim_treat
dat_prim_treat %>% mutate(Leg.or.Sciatic.Pain_N =ifelse(INFO_Author.Year=="Becker 2008",round(Leg.or.Sciatic.Pain_N/DE_becker),Leg.or.Sciatic.Pain_N))->dat_prim_treat
dat_prim_treat %>% mutate(Disability_N =ifelse(INFO_Author.Year=="Becker 2008",round(Disability_N/DE_becker),Disability_N))->dat_prim_treat
dat_prim_treat %>% mutate(Mental.Health_N =ifelse(INFO_Author.Year=="Becker 2008",round(Mental.Health_N /DE_becker),Mental.Health_N ))->dat_prim_treat

##################################################################################################################################################################################################################
##################################################################################################################################################################################################################
#cross-over RCTs

#identify trials
 
dat_prim_class %>% filter(Study_Design=="Randomised cross-over trial")->dat_prim_class_crossover
dat_prim_treat %>% filter(Study_Design=="Randomised cross-over trial")->dat_prim_treat_crossover

dat_prim_class_crossover %>% filter(INFO_Author.Year%in%c("Itoh 2006","Mendelson 1978","Bakhtiarya 2005"))->dat_prim_class_crossover_firstperiod
dat_prim_treat_crossover %>% filter(INFO_Author.Year%in%c("Itoh 2006","Mendelson 1978","Bakhtiarya 2005"))->dat_prim_treat_crossover_firstperiod

dat_prim_class_crossover %>% filter(!INFO_Author.Year%in%c("Itoh 2006","Mendelson 1978","Bakhtiarya 2005"))->dat_prim_class_crossover_NO_firstperiod
dat_prim_treat_crossover %>% filter(!INFO_Author.Year%in%c("Itoh 2006","Mendelson 1978","Bakhtiarya 2005"))->dat_prim_treat_crossover_NO_firstperiod


if(FALSE) {#gets me all the names of he cross-over RCTs
  dat_prim_class_crossover %>% distinct(INFO_Author.Year,.keep_all = TRUE)->x
}

#filter all cross-over RCTs out except (they have first period data)
#Itoh 2006
#Mendelson 1978 
#Bakhtiarya 2005 

dat_prim_class %>% filter(!Study_Design=="Randomised cross-over trial")->dat_prim_class
dat_prim_treat %>% filter(!Study_Design=="Randomised cross-over trial")->dat_prim_treat


#add first period data back in
dat_prim_class<-rbind(dat_prim_class,dat_prim_class_crossover_firstperiod)
dat_treat_class<-rbind(dat_prim_treat,dat_prim_treat_crossover_firstperiod)

##################################################################################################################################################################################################################
##################################################################################################################################################################################################################
#comparing baseline pain, disability and pain duration for each treatment


##baseline pain

#count number of different pain scales
table(dat_prim_class$Back.pain.or.Pain_Tool, useNA = 'always')

#count number of different pain scales
sink("painscales_other.txt")
table(dat_prim_class$Back.pain.or.Pain_If.Other..enter.tool, useNA = 'always')
sink()

#create column with conversion factor for pain scale
dat_prim_class$conversion_factor_pain<-NA

dat_prim_class$conversion_factor_pain<-replmiss(dat_prim_class$conversion_factor_pain,if_else(dat_prim_class$Back.pain.or.Pain_Tool=="Borg back pain intensity scale (10-point scale)",10,NA_real_))
dat_prim_class$conversion_factor_pain<-replmiss(dat_prim_class$conversion_factor_pain,if_else(dat_prim_class$Back.pain.or.Pain_Tool=="Brief pain inventory (BPI) long form (0-10)",10,NA_real_))
dat_prim_class$conversion_factor_pain<-replmiss(dat_prim_class$conversion_factor_pain,if_else(dat_prim_class$Back.pain.or.Pain_Tool=="Brief pain inventory (BPI) short form (0-10)",10,NA_real_))
dat_prim_class$conversion_factor_pain<-replmiss(dat_prim_class$conversion_factor_pain,if_else(dat_prim_class$Back.pain.or.Pain_Tool=="MPQ McGill pain questionnaire (0-5)",5,NA_real_))
dat_prim_class$conversion_factor_pain<-replmiss(dat_prim_class$conversion_factor_pain,if_else(dat_prim_class$Back.pain.or.Pain_Tool=="MPQ McGill pain questionnaire (0-78)",78,NA_real_))
dat_prim_class$conversion_factor_pain<-replmiss(dat_prim_class$conversion_factor_pain,if_else(dat_prim_class$Back.pain.or.Pain_Tool=="NRS or NPRS (0-10)",10,NA_real_))
dat_prim_class$conversion_factor_pain<-replmiss(dat_prim_class$conversion_factor_pain,if_else(dat_prim_class$Back.pain.or.Pain_Tool=="SF-36 (Bodily Pain; 100 to 0)",100,NA_real_))
dat_prim_class$conversion_factor_pain<-replmiss(dat_prim_class$conversion_factor_pain,if_else(dat_prim_class$Back.pain.or.Pain_Tool=="VAS (0-100mm)",100,NA_real_))
dat_prim_class$conversion_factor_pain<-replmiss(dat_prim_class$conversion_factor_pain,if_else(dat_prim_class$Back.pain.or.Pain_Tool=="VAS (0-10cm)",10,NA_real_))
dat_prim_class$conversion_factor_pain<-replmiss(dat_prim_class$conversion_factor_pain,if_else(dat_prim_class$Back.pain.or.Pain_Tool=="Other",NA_real_,NA_real_))


#get the conversions factors from "other"
dat_prim_class %>%
  extract(Back.pain.or.Pain_If.Other..enter.tool, c("Min_pain", "Max_pain"), "([\\d.]+)[^\\d.]+([\\d.]+)", convert = TRUE)->dat_prim_class

dat_prim_class$Max_pain<-as.numeric(dat_prim_class$Max_pain)
dat_prim_class$Min_pain<-as.numeric(dat_prim_class$Min_pain)

dat_prim_class$conversion_factor_pain<-replmiss(dat_prim_class$conversion_factor_pain,if_else(dat_prim_class$Back.pain.or.Pain_Tool=="Other",as.integer(dat_prim_class$Max_pain-dat_prim_class$Min_pain),NA_integer_))


#standardize baseline means on 0-100 scale pain scale

dat_prim_class$Baseline.Back.Pain_Mean_NORM <-dat_prim_class$Baseline.Back.Pain_Mean*(100/dat_prim_class$conversion_factor_pain)

#scale direction no scale with more is better


#filter pain scales "other" out
filter(dat_prim_class,Back.pain.or.Pain_Tool=="Other")->y


a<-c(y$INFO_Author.Year)
b<-c(y$INFO_Covidence.ID)
e<-c(y$Back.pain.or.Pain_Tool)
d<-c(y$Back.pain.or.Pain_If.Other..enter.tool)
sink("other_pain_scales.txt")
data.frame(a,b,e,d)
sink()


#filter disability scales "other" out
dat_prim_class%>% filter(Disability_Tool=="Other")->y


a<-c(y$INFO_Author.Year)
b<-c(y$INFO_Covidence.ID)
e<-c(y$Disability_Tool)
d<-c(y$Disability_If.Other..enter.tool)
sink("other_disability_scales.txt")
data.frame(a,b,e,d)
sink()

#filter legpain scales "other" out
dat_prim_class%>% filter(Leg.or.Sciatic.Pain_Tool=="Other")->y


a<-c(y$INFO_Author.Year)
b<-c(y$INFO_Covidence.ID)
e<-c(y$Leg.or.Sciatic.Pain_Tool)
d<-c(y$Leg.or.Sciatic.Pain_If.Other..enter.tool)
sink("other_legpain_scales.txt")
data.frame(a,b,e,d)
sink()

#filter Mental health scales "other" out
dat_prim_class%>% filter(Mental.Health_Tool=="Other")->y


a<-c(y$INFO_Author.Year)
b<-c(y$INFO_Covidence.ID)
e<-c(y$Mental.Health_Tool)
d<-c(y$Mental.Health_If.Other..enter.tool)
sink("other_mentalhealth_scales.txt")
data.frame(a,b,e,d)
sink()


#filter NAs out
dat_prim_class_without_other[!is.na(dat_prim_class_without_other$Baseline.Back.Pain_Mean_NORM),]->dat_prim_class_without_other

#check if there are values out of scale range...
#filter(dat_prim_class_without_other,Baseline.Back.Pain_Mean_NORM>100 | Baseline.Back.Pain_Mean_NORM<0)->x
#sink(file="pain.txt",split=TRUE)
#x
#sink()


#create box plots
ggplot(dat_prim_class_without_other, aes(x = Baseline.Back.Pain_Mean_NORM, y =INTERVENTION.COMPARATOR_Primary.treatment.component )) + 
  geom_boxplot() +
  xlim(0,100)+
  labs(title="Pain NMA", x="Baseline Pain (normalized to 0-100)", y="Primary Treatment Components")

##################################################################################################################################################################################################################
#baseline disability

#count number of different pain scales
table(dat_prim_class$Disability_Tool, useNA = 'always')

#create column with conversion factor for pain scale

dat_prim_class$conversion_factor_disability<-NA

dat_prim_class$conversion_factor_disability<-replmiss(dat_prim_class$conversion_factor_disability,if_else(dat_prim_class$Disability_Tool=="Funktionsfragebogen Hannover (FFbH-R) 0-100",100,NA_real_))
dat_prim_class$conversion_factor_disability<-replmiss(dat_prim_class$conversion_factor_disability,if_else(dat_prim_class$Disability_Tool=="Modified Oswestry (0-100)",100,NA_real_))
dat_prim_class$conversion_factor_disability<-replmiss(dat_prim_class$conversion_factor_disability,if_else(dat_prim_class$Disability_Tool=="Oswestry (0-100)",100,NA_real_))
dat_prim_class$conversion_factor_disability<-replmiss(dat_prim_class$conversion_factor_disability,if_else(dat_prim_class$Disability_Tool=="Quebec Pain Disability (0-100)",100,NA_real_))
dat_prim_class$conversion_factor_disability<-replmiss(dat_prim_class$conversion_factor_disability,if_else(dat_prim_class$Disability_Tool=="Roland Morris (0-24)",24,NA_real_))
dat_prim_class$conversion_factor_disability<-replmiss(dat_prim_class$conversion_factor_disability,if_else(dat_prim_class$Disability_Tool=="NRS or NPRS (0-10)",10,NA_real_))
dat_prim_class$conversion_factor_disability<-replmiss(dat_prim_class$conversion_factor_disability,if_else(dat_prim_class$Disability_Tool=="SF-12 (physical function subscale; 100 to 0)",100,NA_real_))
dat_prim_class$conversion_factor_disability<-replmiss(dat_prim_class$conversion_factor_disability,if_else(dat_prim_class$Disability_Tool=="SF-36 (physical function subscale; 100 to 0)",100,NA_real_))
dat_prim_class$conversion_factor_disability<-replmiss(dat_prim_class$conversion_factor_disability,if_else(dat_prim_class$Disability_Tool=="Modified Roland Morris (23 questions)",23,NA_real_))


#get the conversions factors from "other"
dat_prim_class %>%
  extract(Disability_If.Other..enter.tool, c("Min_disability", "Max_disability"), "([\\d.]+)[^\\d.]+([\\d.]+)", convert = TRUE)->dat_prim_class

dat_prim_class$Max_disability<-as.numeric(dat_prim_class$Max_disability)
dat_prim_class$Min_disability<-as.numeric(dat_prim_class$Min-disability)

dat_prim_class$conversion_factor_disability<-replmiss(dat_prim_class$conversion_factor_disability,if_else(dat_prim_class$Disability_Tool=="Other",as.integer(dat_prim_class$Max_disability-dat_prim_class$Min_disability),NA_integer_))




#standardize baseline means on 0-100 scale pain scale

dat_prim_class$Baseline.Disability_Mean_NORM <-round(dat_prim_class$Baseline.Disability_Mean*(100/dat_prim_class$conversion_factor_disability),2)

#scale direction
#is that correct?
dat_prim_class<-transform(dat_prim_class,Baseline.Disability_Mean_NORM=ifelse(Disability_Is.more.or.less.better.=="more_is_better",(Baseline.Disability_Mean_NORM-100)*-1,Baseline.Disability_Mean_NORM*1))


#filter NAs out
dat_prim_class[!is.na(dat_prim_class$Baseline.Disability_Mean_NORM),]->dat_prim_class_without_MRM

#check if there are values out of scale range...
#filter(dat_prim_class_without_MRM,Baseline.Disability_Mean_NORM>100|Baseline.Disability_Mean_NORM<0)->x
#sink(file="disab.txt",split=TRUE)
#x
#sink()



#create box plots
ggplot(dat_prim_class_without_MRM, aes(x = Baseline.Back.Pain_Mean_NORM, y =INTERVENTION.COMPARATOR_Primary.treatment.component )) + 
  geom_boxplot() +
  xlim(0,100)+
  labs(title="Disability NMA", x="Baseline Disability (normalized to 0-100)", y="Primary Treatment Components")

##################################################################################################################################################################################################################
##################################################################################################################################################################################################################
##################################################################################################################################################################################################################
##################################################################################################################################################################################################################
##################################################################################################################################################################################################################
##################################################################################################################################################################################################################
#MAIN ANALYSIS 


##################################################################################################################################################################################################################
##################################################################################################################################################################################################################
#pain immediate-term Main analysis
dat_prim_class %>% filter(current.time.label=="00dto01d")-> dat_prim_class_immediate_backpain

#count number of arms in class
dat_prim_class_immediate_backpain %>% group_by(PrimaryClass) %>%summarise(N_in_ARMS=sum(Back.Pain_N,na.rm=TRUE))->N_ARMS_immediate

#remove class with less than 50 patients
#edu,mul,psy,usu

dat_prim_class_immediate_backpain %>% filter(!PrimaryClass %in% c("edu","mul","psy","usu"))->dat_prim_class_immediate_backpain


#remove single arms
dat_prim_class_immediate_backpain %>% add_count(INFO_Author.Year) %>% filter(n > 1) -> dat_prim_class_immediate_backpain




#remove studies
if(FALSE) {
  dat_prim_class_immediate_backpain %>%
    filter(!INFO_Author.Year %in% c('Markman 2015',"Shen 2009",
                                    "Aronsohn 2010",
                                    "Bialosky 2009", 
                                    "Bush 1985",
                                    "Klinger 2017",
                                    "Mehyar 2020", 
                                    "Murakibhavi 2011", 
                                    "Ushinohama 2016",
                                    "Wang 2000"))->dat_prim_class_immediate_backpain
  
#rename Wang to 2005a  
dat_prim_class_immediate_backpain[c(15,16),1]<-"Wang 2005a"
#Wang 2005 #2312 -> be careful this study has also a counterpart also named Wang 2005 with diff INTs
}

#calculate effect sizes
pain_immediate<-pairwise(treat=PrimaryClass, 
             mean=Back.Pain_Mean,sd=Back.Pain_SD,n=Back.Pain_N,
             data =dat_prim_class_immediate_backpain, studlab=INFO_Author.Year,sm="SMD")


#rescale -/+1
pain_immediate <- transform(pain_immediate, pain_immediate$TE==ifelse(Back.pain.or.Pain_Is.more.or.less.better.1=="more.is.better", pain_immediate$TE*-1, pain_immediate$TE*1)) 



#calculate pairwise RE MA

pain_immediate_pwise <- netmeta(TE, seTE, treat1, treat2, studlab,
                                data = pain_immediate, sm = "SMD",details.chkmultiarm = TRUE)

####Question method.tau="REML" does not work why is that?


###missing data (details.chkmultiarm = TRUE)
# "Shen 2009",
# "Aronsohn 2010",
# "Bialosky 2009", 
# "Bush 1985",
# "Klinger 2017",
# "Mehyar 2020", 
# "Murakibhavi 2011", 
# "Ushinohama 2016",
# "Wang 2000"

# Calculate and print consise results for all pairwise
# meta-analyses

pain_immediate_pwise <- netpairwise(pain_immediate_pwise,method.tau = "REML")
pain_immediate_pwise

## Forest 
forest(pain_immediate_pwise)


#show tau and subgroups
a<-pain_immediate_pwise$tau.w
b<-pain_immediate_pwise$bylevs
c<-as.data.frame(a,b)

#create plot of tau
#create box plots
d<-ggplot(c, aes(x = b,y=a)) +
  geom_point() +
  labs(title="Meta-analyses Pain immediate", x="Pairwise Comparisons", y="Tau")

#coordinates flipped
d +
  coord_flip()


#find trials/analyses that lead to (very high heterogeneity)
#pha:pla tau=1.0953
pain_immediate %>% filter(str_detect(treat1,"pha|pla"))%>%
  filter(str_detect(treat2,"pha|pla"))-> pain_immediate_pha_pla

#suspicious studies
#	Murata 2009 #582

#outliers, metaregression funnel plot
#too few studies 



#######################################################################################################################################################################
#######################################################################################################################################################################
#NMA immediate

pain_immediate %>%
  dplyr::select(c(1:5),c(6,9)) %>%
  pivot_longer(-studlab,
               names_to = c(".value"),
               names_pattern = "(.)") ->pain_immediate


#look for multiple arms in one study 
pain_immediate %>% pull(studlab) %>% 
  table() %>% 
  {.[. > 2]}


#remove unneccessary arms of multiple group trials and correct the se
pain_immediate %>% group_by(studlab) %>% filter(n()>2) %>% 
  group_by(studlab) %>% slice(-c(2:4)) %>%
  group_by(studlab) %>% mutate(s=case_when(!is.na(s)~s,is.na(s)~ sqrt(prod(s,na.rm =TRUE)*0.5))) ->d

#combine tibbles 

pain_immediate %>% group_by(studlab) %>% filter(!n()>2) ->c


pain_immediate <-rbind(c,d)





#set-up network
pain_immediate_net <- set_agd_contrast(pain_immediate, 
                              study = studlab,
                              trt = t,
                              y = T, 
                              se = s,
                              sample_size=n )
pain_immediate_net


#plot network structure
plot(pain_immediate_net, weight_edges = TRUE, weight_nodes = TRUE)

#fit RE model
res_pain_immediate <- nma(pain_immediate_net, 
                    seed = 1150676438,
                    trt_effects = "random",
                    prior_trt = normal(scale = 1), #okay?
                    prior_het = half_normal(scale = 0.5), #okay?
                    adapt_delta = 0.99)
print(res_pain_immediate)

#find relative effects
contr_releff_FE <- relative_effects(res_pain_immediate, trt_ref = "pla")


##################################################################################################################################################################################################################
##################################################################################################################################################################################################################
#pain short-term Main analysis

dat_prim_class %>% filter(current.time.label=="01dto03mo")-> dat_prim_class_shortterm_backpain

#count number of arms in class
dat_prim_class_shortterm_backpain %>% group_by(PrimaryClass) %>%summarise(N_in_ARMS=sum(Back.Pain_N,na.rm=TRUE))->N_ARMS_shorterm

#enough participants in all classes

#calculate effect sizes

#Error: Identical treatments for the following studies:
#'Barendse 2001' - 'Carvalho 2016' - 'Durmus 2014' - 'Licciardone 2013' - 'Licciardone 2016' - 'Markman 2015' - 'Monticone 2014' - 'Paolucci 2012' - 'Skljarevski 2010' - 'Smeets 2006' - 'Tavafian 2017' - 'Yun 2012'

if(FALSE) {
  dat_prim_class_shortterm_backpain %>%
    filter(!INFO_Author.Year %in% c('Barendse 2001','Carvalho 2016','Durmus 2014' ,'Licciardone 2013',
                                      'Licciardone 2016','Markman 2015' ,'Monticone 2014' , 'Paolucci 2012', 
                                      'Skljarevski 2010','Smeets 2006' ,'Tavafian 2017' ,'Yun 2012'))->dat_prim_class_shortterm_backpain
  
  
  #rename  Kim 2020  
  dat_prim_class_shortterm_backpain[c(671,672),1]<-"Kim 2020a"
  #Kim 2020  be careful this study has also a counterpart also named Kim 2020 with diff INTs
  
  }


pain_shortterm<-pairwise(treat=PrimaryClass, 
                         mean=Back.Pain_Mean,sd=Back.Pain_SD,n=Back.Pain_N,
                         data =dat_prim_class_shortterm_backpain, studlab=INFO_Author.Year,sm="SMD")


#rescale -/+1
pain_shortterm <- transform(pain_shortterm, pain_shortterm$TE==ifelse(Back.pain.or.Pain_Is.more.or.less.better.1=="more.is.better", pain_shortterm$TE*-1, pain_shortterm$TE*1)) 
pain_shortterm

#drop NAs
pain_shortterm %>% drop_na(TE,seTE)->pain_shortterm


#calculate pairwise RE MA

pain_shortterm_pwise <- netmeta(TE, seTE, treat1, treat2, studlab,
                                data = pain_shortterm, sm = "SMD",details.chkmultiarm = TRUE)
forest(pain_shortterm_pwise,ref="pla")

# Calculate  for all pairwise meta-analyses
pain_shortterm_pwise <- netpairwise(pain_shortterm_pwise,method.tau="REML")


## Forest 
forest(pain_shortterm_pwise)

#show tau and subgroup
a<-pain_shortterm_pwise$tau.w
b<-pain_shortterm_pwise$bylevs
c<-as.data.frame(a,b)

#create plot of tau
#create box plots
d<-ggplot(c, aes(x = b,y=a)) + 
    geom_point() +
    labs(title="Meta-analyses Pain short-term", x="Pairwise Comparisons", y="Tau")

#coordinates flipped
d + 
  coord_flip()

#find trials/analyses that lead to (very high heterogeneity)
#man:psy tau=5.5174
pain_shortterm %>% filter(str_detect(treat1,"man|psy"))%>%
 filter(str_detect(treat2,"man|psy"))-> pain_shortterm_man_psy

#suspicious studies
#VibeFersum 2019 #25211

############################################################################################################################################################################################
############################################################################################################################################################################################

#find/fit outliers, funnel plot and meta-regression for k>10

pain_shortterm_pwise$k.all.w #number of studies in MA



############################################################################################################################################################################################
#pha:pla 
pain_shortterm %>% filter(str_detect(treat1,"pha|pla"))%>%
  filter(str_detect(treat2,"pha|pla"))%>%
  arrange(treat1)-> pain_shortterm_pha_pla

#rescale -/+1 as some trials they have the wrong sign
pain_shortterm_pha_pla %>% mutate(TE=case_when(!treat1=="pla"~TE,treat1=="pla"~ TE*(-1)))->pain_shortterm_pha_pla

#run RE MA for pha:pla
pha_pla_shortterm <- metagen(TE = TE,
                   seTE = seTE,
                   studlab = studlab,
                   data = pain_shortterm_pha_pla,
                   sm = "SMD",
                   fixed = FALSE,
                   random = TRUE,
                   method.tau = "REML",prediction=TRUE)
forest(pha_pla_shortterm)


find.outliers(pha_pla_shortterm)
#"Atkinson 1998", "Coats 2004", "Hale 2007", "Hale 2010", "Markman 2018", "Markman 2019", 
#"Muehlbacher 2006","Birbara 2003", "Hale 2005", "Ma 2011", "SchiphorstPreuper 2014", "Hale 2015" 

pha_pla.inf <- InfluenceAnalysis(pha_pla_shortterm, random = TRUE)
plot(pha_pla.inf, "baujat")
plot(pha_pla.inf, "influence")

#"Hale 2005","Hale 2007" 
# update.meta(pha_pla_shortterm, exclude = c(8,43)) %>% 
#   forest()

#fit meta-regression with baseline pain and disability values
pha_pla_reg_basepain_shortterm <- metareg(pha_pla_shortterm, ~XXX)
bubble(pha_pla_reg_basepain_shortterm, studlab = TRUE)

pha_pla_reg_basedisab_shortterm <- metareg(pha_pla_shortterm, ~XXX)
bubble(pha_pla_reg_basedisab_shortterm, studlab = TRUE)

#funnel plot
funnel(pha_pla_shortterm,studlab = FALSE)
############################################################################################################################################################################################
#mas:pla
pain_shortterm %>% filter(str_detect(treat1,"mas|pla"))%>%
  filter(str_detect(treat2,"mas|pla"))%>%
  arrange(treat1)-> pain_shortterm_mas_pla

#rescale -/+1 as some trials they have the wrong sign
pain_shortterm_mas_pla %>% mutate(TE=case_when(!treat1=="pla"~TE,treat1=="pla"~ TE*(-1)))->pain_shortterm_mas_pla

#run RE MA
mas_pla_shortterm <- metagen(TE = TE,
                             seTE = seTE,
                             studlab = studlab,
                             data = pain_shortterm_mas_pla,
                             sm = "SMD",
                             fixed = FALSE,
                             random = TRUE,
                             method.tau = "REML",prediction=TRUE)
forest(mas_pla_shortterm)

find.outliers(mas_pla_shortterm)
#"Vas 2014"

mas_pla.inf <- InfluenceAnalysis(mas_pla_shortterm, random = TRUE)
plot(mas_pla.inf, "baujat")
plot(mas_pla.inf, "influence")

#"Vas 2014"  
# update.meta(mas_pla_shortterm, exclude = c(8)) %>% 
#   forest()

#fit meta-regression with baseline pain and disability values
mas_pla_reg_basepain_shortterm <- metareg(mas_pla_shortterm, ~XXX)
bubble(mas_pla_reg_basepain_shortterm, studlab = TRUE)

mas_pla_reg_basedisab_shortterm <- metareg(mas_pla_shortterm, ~XXX)
bubble(mas_pla_reg_basedisab_shortterm, studlab = TRUE)

#funnel plot
funnel(mas_pla_shortterm,studlab = FALSE)

############################################################################################################################################################################################
#elc:acu

pain_shortterm %>% filter(str_detect(treat1,"elc|acu"))%>%
  filter(str_detect(treat2,"elc|acu"))%>%
  arrange(treat1)-> pain_shortterm_elc_acu

#rescale -/+1 as some trials they have the wrong sign
pain_shortterm_elc_acu %>% mutate(TE=case_when(!treat1=="acu"~TE,treat1=="acu"~ TE*(-1)))->pain_shortterm_elc_acu

#run RE MA
elc_acu_shortterm <- metagen(TE = TE,
                             seTE = seTE,
                             studlab = studlab,
                             data = pain_shortterm_elc_acu,
                             sm = "SMD",
                             fixed = FALSE,
                             random = TRUE,
                             method.tau = "REML",prediction=TRUE)
forest(elc_acu_shortterm)

find.outliers(elc_acu_shortterm)
#"Sator-Katzenschlager 2004"

elc_acu.inf <- InfluenceAnalysis(elc_acu_shortterm, random = TRUE)
plot(elc_acu.inf, "baujat")
plot(elc_acu.inf, "influence")

#"Sator-Katzenschlager 2004"  
# update.meta(elc_acu_shortterm, exclude = c(11)) %>% 
#   forest()

#fit meta-regression with baseline pain and disability values
elc_acu_reg_basepain_shortterm <- metareg(elc_acu_shortterm, ~XXX)
bubble(elc_acu_reg_basepain_shortterm, studlab = TRUE)

elc_acu_reg_basedisab_shortterm <- metareg(elc_acu_shortterm, ~XXX)
bubble(elc_acu_reg_basedisab_shortterm, studlab = TRUE)

#funnel plot
funnel(elc_acu_shortterm,studlab = FALSE)

############################################################################################################################################################################################
#exe:pio

pain_shortterm %>% filter(str_detect(treat1,"exe|pio"))%>%
  filter(str_detect(treat2,"exe|pio"))%>%
  arrange(treat1)-> pain_shortterm_exe_pio

#rescale -/+1 as some trials they have the wrong sign
pain_shortterm_exe_pio %>% mutate(TE=case_when(!treat1=="pio"~TE,treat1=="pio"~ TE*(-1)))->pain_shortterm_exe_pio

#run RE MA
exe_pio_shortterm <- metagen(TE = TE,
                             seTE = seTE,
                             studlab = studlab,
                             data = pain_shortterm_exe_pio,
                             sm = "SMD",
                             fixed = FALSE,
                             random = TRUE,
                             method.tau = "REML",prediction=TRUE)
forest(exe_pio_shortterm)

find.outliers(exe_pio_shortterm)
#"Cruz-Díaz 2015", "O'Sullivan 1997", "Hwang 2013" 

exe_pio.inf <- InfluenceAnalysis(exe_pio_shortterm, random = TRUE)
plot(exe_pio.inf, "baujat")
plot(exe_pio.inf, "influence")

#"Cruz-Díaz 2015"  
# update.meta(exe_pio_shortterm, exclude = c(3)) %>% 
#forest()

#fit meta-regression with baseline pain and disability values
exe_pio_reg_basepain_shortterm <- metareg(exe_pio_shortterm, ~XXX)
bubble(exe_pio_reg_basepain_shortterm, studlab = TRUE)

exe_pio_reg_basedisab_shortterm <- metareg(exe_pio_shortterm, ~XXX)
bubble(exe_pio_reg_basedisab_shortterm, studlab = TRUE)

#funnel plot
funnel(exe_pio_shortterm,studlab = FALSE)

############################################################################################################################################################################################
#elc:pla

pain_shortterm %>% filter(str_detect(treat1,"elc|pla"))%>%
  filter(str_detect(treat2,"elc|pla"))%>%
  arrange(treat1)-> pain_shortterm_elc_pla

#rescale -/+1 as some trials they have the wrong sign
pain_shortterm_elc_pla %>% mutate(TE=case_when(!treat1=="pla"~TE,treat1=="pla"~ TE*(-1)))->pain_shortterm_elc_pla

#run RE MA
elc_pla_shortterm <- metagen(TE = TE,
                             seTE = seTE,
                             studlab = studlab,
                             data = pain_shortterm_elc_pla,
                             sm = "SMD",
                             fixed = FALSE,
                             random = TRUE,
                             method.tau = "REML",prediction=TRUE)
forest(elc_pla_shortterm)

find.outliers(elc_pla_shortterm)
#"Ahmed 2009", "Çelik 2020", "Gale 2006", "Gibson 1985", "KIBAR 2020", "Leichtfried 2014", 
#"Tantawy 2020", "YoungEunMoon 2017", "Elshiwi 2019", "Glazov 2014"  

elc_pla.inf <- InfluenceAnalysis(elc_pla_shortterm, random = TRUE)
plot(elc_pla.inf, "baujat")
plot(elc_pla.inf, "influence")

#"YoungEunMoon 2017" "Ahmed 2009"  
# update.meta(elc_pla_shortterm, exclude = c(5,40)) %>% 
#forest()

#fit meta-regression with baseline pain and disability values
elc_pla_reg_basepain_shortterm <- metareg(elc_pla_shortterm, ~XXX)
bubble(elc_pla_reg_basepain_shortterm, studlab = TRUE)

elc_pla_reg_basedisab_shortterm <- metareg(elc_pla_shortterm, ~XXX)
bubble(elc_pla_reg_basedisab_shortterm, studlab = TRUE)

#funnel plot
funnel(elc_pla_shortterm,studlab = FALSE)


############################################################################################################################################################################################
#exe:man

pain_shortterm %>% filter(str_detect(treat1,"exe|man"))%>%
  filter(str_detect(treat2,"exe|man"))%>%
  arrange(treat1)-> pain_shortterm_exe_man

#rescale -/+1 as some trials they have the wrong sign
pain_shortterm_exe_man %>% mutate(TE=case_when(!treat1=="man"~TE,treat1=="man"~ TE*(-1)))->pain_shortterm_exe_man

#run RE MA
exe_man_shortterm <- metagen(TE = TE,
                             seTE = seTE,
                             studlab = studlab,
                             data = pain_shortterm_exe_man,
                             sm = "SMD",
                             fixed = FALSE,
                             random = TRUE,
                             method.tau = "REML",prediction=TRUE)
forest(exe_man_shortterm)

find.outliers(exe_man_shortterm)
#"Nagrale 2012", "Sanei 2020", "Ali 2015"  

exe_man.inf <- InfluenceAnalysis(exe_man_shortterm, random = TRUE)
plot(exe_man.inf, "baujat")
plot(exe_man.inf, "influence")

#"Ali 2015"  
# update.meta(exe_man_shortterm, exclude = c(12)) %>% 
#forest()

#fit meta-regression with baseline pain and disability values
exe_man_reg_basepain_shortterm <- metareg(exe_man_shortterm, ~XXX)
bubble(exe_man_reg_basepain_shortterm, studlab = TRUE)

exe_man_reg_basedisab_shortterm <- metareg(exe_man_shortterm, ~XXX)
bubble(exe_man_reg_basedisab_shortterm, studlab = TRUE)

#funnel plot
funnel(exe_man_shortterm,studlab = FALSE)

############################################################################################################################################################################################
#edu:exe

pain_shortterm %>% filter(str_detect(treat1,"edu|exe"))%>%
  filter(str_detect(treat2,"edu|exe"))%>%
  arrange(treat1)-> pain_shortterm_edu_exe

#rescale -/+1 as some trials they have the wrong sign
pain_shortterm_edu_exe %>% mutate(TE=case_when(!treat1=="exe"~TE,treat1=="exe"~ TE*(-1)))->pain_shortterm_edu_exe 

#run RE MA
edu_exe_shortterm <- metagen(TE = TE,
                             seTE = seTE,
                             studlab = studlab,
                             data = pain_shortterm_edu_exe,
                             sm = "SMD",
                             fixed = FALSE,
                             random = TRUE,
                             method.tau = "REML",prediction=TRUE)
forest(edu_exe_shortterm)

find.outliers(edu_exe_shortterm)
#"Kim 2020", "Valenza 2017", "Zhang 2014", "A 2017", "BodesPardo 2018", "Kuvacic 2018", "Phattharasupharerk 2019"

edu_exe.inf <- InfluenceAnalysis(edu_exe_shortterm, random = TRUE)
plot(edu_exe.inf, "baujat")
plot(edu_exe.inf, "influence")

#"A 2017"  
# update.meta(exe_man_shortterm, exclude = c(9)) %>% 
#forest()

#fit meta-regression with baseline pain and disability values
edu_exe_reg_basepain_shortterm <- metareg(edu_exe_shortterm, ~XXX)
bubble(edu_exe_reg_basepain_shortterm, studlab = TRUE)

edu_exe_reg_basedisab_shortterm <- metareg(edu_exe_shortterm, ~XXX)
bubble(edu_exe_reg_basedisab_shortterm, studlab = TRUE)

#funnel plot
funnel(edu_exe_shortterm,studlab = FALSE)

############################################################################################################################################################################################
#exe:psy 

pain_shortterm %>% filter(str_detect(treat1,"exe|psy"))%>%
  filter(str_detect(treat2,"exe|psy"))%>%
  arrange(treat1)-> pain_shortterm_exe_psy

#rescale -/+1 as some trials they have the wrong sign
pain_shortterm_exe_psy %>% mutate(TE=case_when(!treat1=="psy"~TE,treat1=="psy"~ TE*(-1)))->pain_shortterm_exe_psy

#run RE MA
exe_psy_shortterm <- metagen(TE = TE,
                             seTE = seTE,
                             studlab = studlab,
                             data = pain_shortterm_exe_psy,
                             sm = "SMD",
                             fixed = FALSE,
                             random = TRUE,
                             method.tau = "REML",prediction=TRUE)
forest(exe_psy_shortterm)

find.outliers(exe_psy_shortterm)
#"Khan 2014"

exe_psy.inf <- InfluenceAnalysis(exe_psy_shortterm, random = TRUE)
plot(exe_psy.inf, "baujat")
plot(exe_psy.inf, "influence")

#"Khan 2014"  
# update.meta(exe_psy_shortterm, exclude = c(8)) %>% 
#forest()

#fit meta-regression with baseline pain and disability values
exe_psy_reg_basepain_shortterm <- metareg(exe_psy_shortterm, ~XXX)
bubble(exe_psy_reg_basepain_shortterm, studlab = TRUE)

exe_psy_reg_basedisab_shortterm <- metareg(exe_psy_shortterm, ~XXX)
bubble(exe_psy_reg_basedisab_shortterm, studlab = TRUE)

#funnel plot
funnel(exe_psy_shortterm,studlab = FALSE)

############################################################################################################################################################################################
#elc:exe 

pain_shortterm %>% filter(str_detect(treat1,"elc|exe"))%>%
  filter(str_detect(treat2,"elc|exe"))%>%
  arrange(treat1)-> pain_shortterm_elc_exe

#rescale -/+1 as some trials they have the wrong sign
pain_shortterm_elc_exe %>% mutate(TE=case_when(!treat1=="exe"~TE,treat1=="exe"~ TE*(-1)))->pain_shortterm_elc_exe

#run RE MA
elc_exe_shortterm <- metagen(TE = TE,
                             seTE = seTE,
                             studlab = studlab,
                             data = pain_shortterm_elc_exe,
                             sm = "SMD",
                             fixed = FALSE,
                             random = TRUE,
                             method.tau = "REML",prediction=TRUE)
forest(elc_exe_shortterm )

find.outliers(elc_exe_shortterm )
#"Kumar 2010", "Sahin 2018", "Tantawy 2019", "Tsui 2004", "Chatzitheodorou 2007", "Durmus 2013", "Gaowgzeh 2020", "Kim 2020", 
#"Kofotolis 2008", "Murtezani 2011", "RenovatoFrança 2019", "Tae Hoon Kim 2015" 

elc_exe.inf <- InfluenceAnalysis(elc_exe_shortterm , random = TRUE)
plot(elc_exe.inf, "baujat")
plot(elc_exe.inf, "influence")

#"Kim 2020"  
# update.meta(elc_exe_shortterm, exclude = c(18)) %>% 
#forest()

#fit meta-regression with baseline pain and disability values
elc_exe_reg_basepain_shortterm <- metareg(elc_exe_shortterm , ~XXX)
bubble(elc_exe_reg_basepain_shortterm, studlab = TRUE)

elc_exe_reg_basedisab_shortterm <- metareg(elc_exe_shortterm , ~XXX)
bubble(elc_exe_reg_basedisab_shortterm, studlab = TRUE)

#funnel plot
funnel(elc_exe_shortterm ,studlab = FALSE)

############################################################################################################################################################################################
#exe:tru 

pain_shortterm %>% filter(str_detect(treat1,"exe|tru"))%>%
  filter(str_detect(treat2,"exe|tru"))%>%
  arrange(treat1)-> pain_shortterm_exe_tru

#rescale -/+1 as some trials they have the wrong sign
pain_shortterm_exe_tru %>% mutate(TE=case_when(!treat1=="tru"~TE,treat1=="tru"~ TE*(-1)))->pain_shortterm_exe_tru

#run RE MA
exe_tru_shortterm <- metagen(TE = TE,
                             seTE = seTE,
                             studlab = studlab,
                             data = pain_shortterm_exe_tru,
                             sm = "SMD",
                             fixed = FALSE,
                             random = TRUE,
                             method.tau = "REML",prediction=TRUE)
forest(exe_tru_shortterm)

find.outliers(exe_tru_shortterm)
#"Cruz-Díaz 2018", "Hasanpour-Dehkordi 2017", "José Clemente da Silva 2014", "Kamioka 2011", 
#"Khalil 1992", "Kuukkanen 1996", "Kuukkanen 2000", "Liu 2019", "Mazloum 2018", "Rhee 2012", 
#"Weifen 2013", "Williams 2009", "Zou 2019", "Kofotolis 2016", "Moussouli 2014"  

exe_tru.inf <- InfluenceAnalysis(exe_tru_shortterm, random = TRUE)
plot(exe_tru.inf, "baujat")
plot(exe_tru.inf, "influence")

#none 
# update.meta(exe_tru_shortterm, exclude = c()) %>% 
#forest()

#fit meta-regression with baseline pain and disability values
exe_tru_reg_basepain_shortterm <- metareg(exe_tru_shortterm , ~XXX)
bubble(exe_tru_reg_basepain_shortterm, studlab = TRUE)

exe_tru_reg_basedisab_shortterm <- metareg(exe_tru_shortterm , ~XXX)
bubble(exe_tru_reg_basedisab_shortterm, studlab = TRUE)

#funnel plot
funnel(exe_tru_shortterm,studlab = FALSE)

############################################################################################################################################################################################
#man:pla

pain_shortterm %>% filter(str_detect(treat1,"man|pla"))%>%
  filter(str_detect(treat2,"man|pla"))%>%
  arrange(treat1)-> pain_shortterm_man_pla

#rescale -/+1 as some trials they have the wrong sign
pain_shortterm_man_pla %>% mutate(TE=case_when(!treat1=="pla"~TE,treat1=="pla"~ TE*(-1)))->pain_shortterm_man_pla

#run RE MA
man_pla_shortterm <- metagen(TE = TE,
                             seTE = seTE,
                             studlab = studlab,
                             data = pain_shortterm_man_pla,
                             sm = "SMD",
                             fixed = FALSE,
                             random = TRUE,
                             method.tau = "REML",prediction=TRUE)
forest(man_pla_shortterm)

find.outliers(man_pla_shortterm)
#"Bond 2020", "Krekoukias 2017"  

man_pla.inf <- InfluenceAnalysis(man_pla_shortterm, random = TRUE)
plot(man_pla.inf, "baujat")
plot(man_pla.inf, "influence")

#"Krekoukias 2017" 
# update.meta(man_pla_shortterm, exclude = c(7)) %>% 
#forest()

#fit meta-regression with baseline pain and disability values
man_pla_reg_basepain_shortterm <- metareg(man_pla_shortterm , ~XXX)
bubble(man_plau_reg_basepain_shortterm, studlab = TRUE)

man_pla_reg_basedisab_shortterm <- metareg(man_pla_shortterm , ~XXX)
bubble(man_pla_reg_basedisab_shortterm, studlab = TRUE)

#funnel plot
funnel(man_pla_shortterm,studlab = FALSE)

############################################################################################################################################################################################
#pla:acu

pain_shortterm %>% filter(str_detect(treat1,"pla|acu"))%>%
  filter(str_detect(treat2,"pla|acu"))%>%
  arrange(treat1)-> pain_shortterm_pla_acu

#rescale -/+1 as some trials they have the wrong sign
pain_shortterm_pla_acu %>% mutate(TE=case_when(!treat1=="acu"~TE,treat1=="acu"~ TE*(-1)))->pain_shortterm_pla_acu

#run RE MA
pla_acu_shortterm <- metagen(TE = TE,
                             seTE = seTE,
                             studlab = studlab,
                             data = pain_shortterm_pla_acu,
                             sm = "SMD",
                             fixed = FALSE,
                             random = TRUE,
                             method.tau = "REML",prediction=TRUE)
forest(pla_acu_shortterm)

find.outliers(pla_acu_shortterm)
#"Qin 2020" 

pla_acu.inf <- InfluenceAnalysis(pla_acu_shortterm, random = TRUE)
plot(pla_acu.inf, "baujat")
plot(pla_acu.inf, "influence")

#None
# update.meta(pla_acu_shortterm, exclude = c()) %>% 
#forest()

#fit meta-regression with baseline pain and disability values
pla_acu_reg_basepain_shortterm <- metareg(pla_acu_shortterm , ~XXX)
bubble(pla_acu_reg_basepain_shortterm, studlab = TRUE)

pla_acu_reg_basedisab_shortterm <- metareg(pla_acu_shortterm , ~XXX)
bubble(pla_acu_reg_basedisab_shortterm, studlab = TRUE)

#funnel plot
funnel(pla_acu_shortterm,studlab = FALSE)

############################################################################################################################################################################################
#per:pla 

pain_shortterm %>% filter(str_detect(treat1,"per|pla"))%>%
  filter(str_detect(treat2,"per|pla"))%>%
  arrange(treat1)-> pain_shortterm_per_pla

#rescale -/+1 as some trials they have the wrong sign
pain_shortterm_per_pla %>% mutate(TE=case_when(!treat1=="pla"~TE,treat1=="pla"~ TE*(-1)))->pain_shortterm_per_pla

#run RE MA
per_pla_shortterm <- metagen(TE = TE,
                             seTE = seTE,
                             studlab = studlab,
                             data = pain_shortterm_per_pla,
                             sm = "SMD",
                             fixed = FALSE,
                             random = TRUE,
                             method.tau = "REML",prediction=TRUE)
forest(per_pla_shortterm)

find.outliers(per_pla_shortterm)
#"Mehta 2018", "Moussa 2016" 

per_pla.inf <- InfluenceAnalysis(per_pla_shortterm, random = TRUE)
plot(per_pla.inf, "baujat")
plot(per_pla.inf, "influence")

#"Mehta 2018"
# update.meta(per_pla_shortterm, exclude = c(5)) %>% 
#forest()

#fit meta-regression with baseline pain and disability values
per_pla_reg_basepain_shortterm <- metareg(per_pla_shortterm , ~XXX)
bubble(per_pla_reg_basepain_shortterm, studlab = TRUE)

per_pla_reg_basedisab_shortterm <- metareg(per_pla_shortterm , ~XXX)
bubble(per_pla_reg_basedisab_shortterm, studlab = TRUE)

#funnel plot
funnel(per_pla_shortterm,studlab = FALSE)



#######################################################################################################################################################
#######################################################################################################################################################
#Bayesian NMA short-term

pain_shortterm %>%
  dplyr::select(c(1:5),c(6,9)) %>%
  pivot_longer(-studlab,
               names_to = c(".value"),
               names_pattern = "(.)") -> pain_shortterm

#look for multiple arms in one study 
pain_shortterm %>% pull(studlab) %>% 
  table() %>% 
  {.[. > 2]}

#remove unneccessary arms of multiple group trials and correct the se
pain_shortterm %>% group_by(studlab) %>% filter(n()>2) %>% 
group_by(studlab) %>% slice(-c(2:4)) %>%
group_by(studlab) %>% mutate(s=case_when(!is.na(s)~s,is.na(s)~ sqrt(prod(s,na.rm =TRUE)*0.5))) ->d

#combine tibbles 
pain_shortterm %>% group_by(studlab) %>% filter(!n()>2) ->c
pain_shortterm <-rbind(c,d)


#set-up network
pain_shortterm_net <- set_agd_contrast(pain_shortterm, 
                                       study = studlab,
                                       trt =t,
                                       y =T, 
                                       se =s,
                                       sample_size=n,trt_ref="pla")
pain_shortterm_net


#plot network structure
plot(pain_shortterm_net, weight_edges = TRUE, weight_nodes = TRUE)

#fit RE model
res_pain_shortterm <- nma(pain_shortterm_net, 
                          seed = 1150676438,
                          trt_effects = "random",
                          prior_trt = normal(scale = 1), #okay?
                          prior_het = half_normal(scale = 0.5), #okay?
                          adapt_delta = 0.99)
print(res_pain_shortterm)

res_pain_shortterm$stanfit$d[]




#plot posterior distribution
plot_prior_posterior(res_pain_shortterm)

print(res_pain_shortterm, pars = c("d", "delta"))

pairs(res_pain_shortterm, pars = c("d[pla]", "delta[exe: exe vs. pla]", "tau"))#do not understand this

#model fit
dic_res_pain_shortterm <- dic(res_pain_shortterm)
plot(dic_res_pain_shortterm)


#find relative effects
res_pain_shortterm_releffects <- relative_effects(res_pain_shortterm, trt_ref = "pla")

plot(res_pain_shortterm_releffects)+
geom_vline(xintercept=-0.5,linetype='dashed', color='red')+
  geom_vline(xintercept=0.5,linetype='dashed', color='red')


#ranking
ranks_res_pain_shortterm <- posterior_ranks(res_pain_shortterm)
plot(ranks_res_pain_shortterm)

#cumulative ranking probabilities
rankprobs_res_pain_shortterm  <- posterior_rank_probs(res_pain_shortterm)
plot(rankprobs_res_pain_shortterm)
cumrankprob_res_pain_shortterm <- posterior_rank_probs(res_pain_shortterm, cumulative = TRUE)
plot(cumrankprob_res_pain_shortterm)


#checking for inconsistency

#UME
pain_shortterm_net_ume <- nma(pain_shortterm_net, 
                  consistency = "ume",
                  trt_effects = "random",
                  prior_trt = normal(scale = 1),#okay?
                  prior_het = normal(scale = 0.5))#okay?
pain_shortterm_net_ume

#model comparison
dic_pain_shortterm_net_ume <- dic(pain_shortterm_net_ume)

plot(dic_res_pain_shortterm, dic_pain_shortterm_net_ume, point_alpha = 0.5, interval_alpha = 0.2)


#node splitting
pain_shortterm_net_nodesplit <- nma(pain_shortterm_net, 
                     consistency = "nodesplit",
                     trt_effects = "random",
                     prior_trt = normal(scale = 1), #okay?
                     prior_het = normal(scale = 0.5))#okay?
summary(pain_shortterm_net_nodesplit)

plot(pain_shortterm_net_nodesplit) +
  ggplot2::theme(legend.position = "bottom", legend.direct = "horizontal")




##################################################################################################################################################################################################################
##################################################################################################################################################################################################################
#pain intermediate-term Main analysis 

dat_prim_class %>% filter(current.time.label=="03to12mo")-> dat_prim_class_intermediateterm_backpain

#count number of arms in class
dat_prim_class_intermediateterm_backpain %>% group_by(PrimaryClass) %>%summarise(N_in_ARMS=sum(Back.Pain_N,na.rm=TRUE))->N_ARMS_intermediateterm

#enough participants!


#calculate effect sizes

#Error: Identical treatments for the following studies:
#'Tavafian 2017','Yun 2012'


if(FALSE) {
  dat_prim_class_intermediateterm_backpain %>%
    filter(!INFO_Author.Year %in% c('Tavafian 2017','Yun 2012','Monticone 2014' ))->dat_prim_class_intermediateterm_backpain
}


pain_intermediateterm<-pairwise(treat=PrimaryClass, 
                                mean=Back.Pain_Mean,sd=Back.Pain_SD,n=Back.Pain_N,
                                data =dat_prim_class_intermediateterm_backpain, studlab=INFO_Author.Year,sm="SMD")
#rescale -/+1
pain_intermediateterm <- transform(pain_intermediateterm, pain_intermediateterm$TE==ifelse(Back.pain.or.Pain_Is.more.or.less.better.1=="more.is.better", pain_intermediateterm$TE*-1, pain_intermediateterm$TE*1)) 


#drop NAs
pain_intermediateterm %>% drop_na(TE)->pain_intermediateterm

####check those after SDs imputed!!!!


#calculate pairwise RE MA

pain_intermediateterm_pwise <- netmeta(TE, seTE, treat1, treat2, studlab,
                                       data = pain_intermediateterm, sm = "SMD",details.chkmultiarm = TRUE)


# Calculate  for all pairwise meta-analyses
pain_intermediateterm_pwise <- netpairwise(pain_intermediateterm_pwise,method.tau="REML")
pain_intermediateterm_pwise 

## Forest 
forest(pain_intermediateterm_pwise)


#show tau and subgroup
a<-pain_intermediateterm_pwise $tau.w
b<-pain_intermediateterm_pwise $bylevs
c<-as.data.frame(a,b)

#create plot of tau
#create box plots
d<-ggplot(c, aes(x = b,y=a)) + 
  geom_point() +
  labs(title="Meta-analyses Pain intermediate-term", x="Pairwise Comparisons", y="Tau")
d
#coordinates flipped
d + 
  coord_flip()



#find trials/analyses that lead to (very high heterogeneity) 
#epi:per tau= 4.9476
pain_intermediateterm %>% filter(str_detect(treat1,"epi|per"))%>%
  filter(str_detect(treat2,"epi|per"))-> pain_intermediateterm_epi_per

#exe:tru tau=3.5929
pain_intermediateterm %>% filter(str_detect(treat1,"exe|tru"))%>%
  filter(str_detect(treat2,"exe|tru"))-> pain_intermediateterm_exe_tru


#suspicious studies
#Staats 2016 	#25525
#Storheim 2000 #7177
#Williams 2009 #8489



############################################################################################################################################################################################
############################################################################################################################################################################################

#find/fit outliers, funnel plot and meta-regression for k>10

pain_intermediateterm_pwise$k.all.w #number of studies in MA

############################################################################################################################################################################################
#exe:tru 

pain_intermediateterm %>% filter(str_detect(treat1,"exe|tru"))%>%
  filter(str_detect(treat2,"exe|tru"))%>%
  arrange(treat1)-> pain_intermediateterm_exe_tru

#rescale -/+1 as some trials they have the wrong sign
pain_intermediateterm_exe_tru %>% mutate(TE=case_when(!treat1=="tru"~TE,treat1=="tru"~ TE*(-1)))->pain_intermediateterm_exe_tru

#run RE MA
exe_tru_intermediateterm <- metagen(TE = TE,
                             seTE = seTE,
                             studlab = studlab,
                             data = pain_intermediateterm_exe_tru,
                             sm = "SMD",
                             fixed = FALSE,
                             random = TRUE,
                             method.tau = "REML",prediction=TRUE)
forest(exe_tru_intermediateterm)

find.outliers(exe_tru_intermediateterm )
#"Storheim 2000"   

exe_tru.inf <- InfluenceAnalysis(exe_tru_intermediateterm , random = TRUE)
plot(exe_tru.inf, "baujat")
plot(exe_tru.inf, "influence")

#"Storheim 2000"   
# update.meta(exe_tru_intermediateterm , exclude = c(7)) %>% 
#forest()

#fit meta-regression with baseline pain and disability values
exe_tru_reg_basepain_shortterm <- metareg(exe_tru_intermediateterm, ~XXX)
bubble(exe_tru_reg_basepain_shortterm, studlab = TRUE)

exe_tru_reg_basedisab_shortterm <- metareg(exe_tru_intermediateterm, ~XXX)
bubble(exe_tru_reg_basedisab_shortterm, studlab = TRUE)

#funnel plot
funnel(exe_tru_intermediateterm,studlab = FALSE)



############################################################################################################################################################################################
#edu:exe

pain_intermediateterm %>% filter(str_detect(treat1,"edu|exe"))%>%
  filter(str_detect(treat2,"edu|exe"))%>%
  arrange(treat1)-> pain_intermediateterm_edu_exe

#rescale -/+1 as some trials they have the wrong sign
pain_intermediateterm_edu_exe %>% mutate(TE=case_when(!treat1=="exe"~TE,treat1=="exe"~ TE*(-1)))->pain_intermediateterm_edu_exe

#run RE MA
edu_exe_intermediateterm <- metagen(TE = TE,
                             seTE = seTE,
                             studlab = studlab,
                             data = pain_intermediateterm_edu_exe,
                             sm = "SMD",
                             fixed = FALSE,
                             random = TRUE,
                             method.tau = "REML",prediction=TRUE)
forest(edu_exe_intermediateterm)

find.outliers(edu_exe_intermediateterm)
#"Cuesta-Vargas 2012", "Durmus 2014" 

edu_exe.inf <- InfluenceAnalysis(edu_exe_intermediateterm, random = TRUE)
plot(edu_exe.inf, "baujat")
plot(edu_exe.inf, "influence")

#none
# update.meta(edu_exe_intermediateterm, exclude = c()) %>% 
#forest()

#fit meta-regression with baseline pain and disability values
edu_exe_reg_basepain_shortterm <- metareg(edu_exe_intermediateterm, ~XXX)
bubble(edu_exe_reg_basepain_shortterm, studlab = TRUE)

edu_exe_reg_basedisab_shortterm <- metareg(edu_exe_intermediateterm, ~XXX)
bubble(edu_exe_reg_basedisab_shortterm, studlab = TRUE)

#funnel plot
funnel(edu_exe_intermediateterm,studlab = FALSE)

############################################################################################################################################################################################
#exe:psy 

pain_intermediateterm %>% filter(str_detect(treat1,"exe|psy"))%>%
  filter(str_detect(treat2,"exe|psy"))%>%
  arrange(treat1)-> pain_intermediateterm_exe_psy

#rescale -/+1 as some trials they have the wrong sign
pain_intermediateterm_exe_psy %>% mutate(TE=case_when(!treat1=="psy"~TE,treat1=="psy"~ TE*(-1)))->pain_intermediateterm_exe_psy

#run RE MA
exe_psy_intermediateterm <- metagen(TE = TE,
                             seTE = seTE,
                             studlab = studlab,
                             data = pain_intermediateterm_exe_psy,
                             sm = "SMD",
                             fixed = FALSE,
                             random = TRUE,
                             method.tau = "REML",prediction=TRUE)
forest(exe_psy_intermediateterm)

find.outliers(exe_psy_intermediateterm)
#"Friedrich 2005" 

exe_psy.inf <- InfluenceAnalysis(exe_psy_intermediateterm, random = TRUE)
plot(exe_psy.inf, "baujat")
plot(exe_psy.inf, "influence")

#none 
# update.meta(exe_psy_intermediateterm, exclude = c()) %>% 
#forest()

#fit meta-regression with baseline pain and disability values
exe_psy_reg_basepain_shortterm <- metareg(exe_psy_intermediateterm, ~XXX)
bubble(exe_psy_reg_basepain_shortterm, studlab = TRUE)

exe_psy_reg_basedisab_shortterm <- metareg(exe_psy_intermediateterm, ~XXX)
bubble(exe_psy_reg_basedisab_shortterm, studlab = TRUE)

#funnel plot
funnel(exe_psy_intermediateterm,studlab = FALSE)



############################################################################################################################################################################################
#exe:pio

pain_intermediateterm %>% filter(str_detect(treat1,"exe|pio"))%>%
  filter(str_detect(treat2,"exe|pio"))%>%
  arrange(treat1)-> pain_intermediateterm_exe_pio

#rescale -/+1 as some trials they have the wrong sign
pain_intermediateterm_exe_pio %>% mutate(TE=case_when(!treat1=="pio"~TE,treat1=="pio"~ TE*(-1)))->pain_intermediateterm_exe_pio

#run RE MA
exe_pio_intermediateterm<- metagen(TE = TE,
                             seTE = seTE,
                             studlab = studlab,
                             data = pain_intermediateterm_exe_pio,
                             sm = "SMD",
                             fixed = FALSE,
                             random = TRUE,
                             method.tau = "REML",prediction=TRUE)
forest(exe_pio_intermediateterm)

find.outliers(exe_pio_intermediateterm)
#"Jaromi 2012", "O'Sullivan 1997" 

exe_pio.inf <- InfluenceAnalysis(exe_pio_intermediateterm, random = TRUE)
plot(exe_pio.inf, "baujat")
plot(exe_pio.inf, "influence")

#"O'Sullivan 1997" 
# update.meta(exe_pio_intermediateterm, exclude = c(6)) %>% 
#forest()

#fit meta-regression with baseline pain and disability values
exe_pio_reg_basepain_shortterm <- metareg(exe_pio_intermediateterm, ~XXX)
bubble(exe_pio_reg_basepain_shortterm, studlab = TRUE)

exe_pio_reg_basedisab_shortterm <- metareg(exe_pio_intermediateterm, ~XXX)
bubble(exe_pio_reg_basedisab_shortterm, studlab = TRUE)

#funnel plot
funnel(exe_pio_intermediateterm,studlab = FALSE)


##########################################################################################################################################################################
##########################################################################################################################################################################
#Bayesian NMA intermediate term

pain_intermediateterm %>%
  dplyr::select(c(1:5),c(6,9)) %>%
  pivot_longer(-studlab,
               names_to = c(".value"),
               names_pattern = "(.)") -> pain_intermediateterm

#look for multiple arms in one study 
pain_intermediateterm %>% pull(studlab) %>% 
  table() %>% 
  {.[. > 2]}


#remove unneccessary arms of multiple group trials and correct se
pain_intermediateterm %>% group_by(studlab) %>% filter(n()>2) %>% 
  group_by(studlab) %>% slice(-c(2:4)) %>%
  group_by(studlab) %>% mutate(s=case_when(!is.na(s)~s,is.na(s)~ sqrt(prod(s,na.rm =TRUE)*0.5))) ->d

#combine tibbles 
pain_intermediateterm %>% group_by(studlab) %>% filter(!n()>2) ->c
pain_intermediateterm <-rbind(c,d)



#set-up network
pain_intermediateterm_net <- set_agd_contrast(pain_intermediateterm, 
                                              study = studlab,
                                              trt =t,
                                              y =T, 
                                              se =s,
                                              sample_size=n,trt_ref = "exe")
pain_intermediateterm_net


#plot network structure
plot(pain_intermediateterm_net, weight_edges = TRUE, weight_nodes = TRUE)

#fit RE model
res_pain_intermediateterm <- nma(pain_intermediateterm_net, 
                                 seed = 1150676438,
                                 trt_effects = "random",
                                 prior_trt = normal(scale = 1), #okay?
                                 prior_het = half_normal(scale = 0.5), #okay?
                                 adapt_delta = 0.99)
print(res_pain_intermediateterm)

#plot posterior distribution
plot_prior_posterior(res_pain_intermediateterm)

print(res_pain_intermediateterm, pars = c("d", "delta"))

pairs(res_pain_intermediateterm, pars = c("d[pla]", "delta[exe: exe vs. pla]", "tau"))#do not understand this

#model fit
dic_res_pain_intermediateterm <- dic(res_pain_intermediateterm)
plot(dic_res_pain_intermediateterm)


#find relative effects
res_pain_intermediate_releffects <- relative_effects(res_pain_intermediateterm, trt_ref = "pla")

plot(res_pain_intermediate_releffects)+
  geom_vline(xintercept=-0.5,linetype='dashed', color='red')+
  geom_vline(xintercept=0.5,linetype='dashed', color='red')


#ranking
ranks_res_pain_intermediate <- posterior_ranks(res_pain_intermediate)
plot(ranks_res_pain_intermediate)

#cumulative ranking probabilities
rankprobs_res_pain_intermediate  <- posterior_rank_probs(res_pain_intermediate)
plot(rankprobs_res_pain_intermediate)
cumrankprob_res_pain_intermediate <- posterior_rank_probs(res_pain_intermediate, cumulative = TRUE)
plot(cumrankprob_res_pain_intermediate)


#checking for inconsistency

#UME
pain_intermediate_net_ume <- nma(pain_intermediateterm_net, 
                              consistency = "ume",
                              trt_effects = "random",
                              prior_trt = normal(scale = 1),#okay?
                              prior_het = normal(scale = 0.5))#okay?
pain_intermediate_net_ume

#model comparison
dic_pain_intermediate_net_ume <- dic(pain_intermediate_net_ume)

plot(dic_res_pain_intermediate, dic_pain_intermediate_net_ume, point_alpha = 0.5, interval_alpha = 0.2)


#node splitting
pain_intermediate_net_nodesplit <- nma(pain_intermediateterm_net, 
                                    consistency = "nodesplit",
                                    trt_effects = "random",
                                    prior_trt = normal(scale = 1), #okay?
                                    prior_het = normal(scale = 0.5))#okay?
summary(pain_intermediate_net_nodesplit)

plot(pain_intermediate_net_nodesplit) +
  ggplot2::theme(legend.position = "bottom", legend.direct = "horizontal")



##########################################################################################################################################################################
##########################################################################################################################################################################
#pain long-term Main analysis

dat_prim_class %>% filter(current.time.label=="12moplus")-> dat_prim_class_longterm_backpain


#count number of arms in class
dat_prim_class_longterm_backpain %>% group_by(PrimaryClass) %>%summarise(N_in_ARMS=sum(Back.Pain_N,na.rm=TRUE))->N_ARMS_longterm

#tra= 0 participants

dat_prim_class_longterm_backpain %>% filter(!PrimaryClass %in% c("tra"))->dat_prim_class_longterm_backpain


#remove single arms
dat_prim_class_longterm_backpain %>% add_count(INFO_Author.Year) %>% filter(n > 1) -> dat_prim_class_longterm_backpain


#calculate effect sizes

#Error: Identical treatments for the following studies:
#'Bendix 1998' - 'Tavafian 2017'


if(FALSE) {
  dat_prim_class_longterm_backpain %>%
    filter(!INFO_Author.Year %in% c('Bendix 1998', 'Tavafian 2017',"Monticone 2014"))->dat_prim_class_longterm_backpain
}


pain_longterm<-pairwise(treat=PrimaryClass, 
                         mean=Back.Pain_Mean,sd=Back.Pain_SD,n=Back.Pain_N,
                         data =dat_prim_class_longterm_backpain, studlab=INFO_Author.Year,sm="SMD")

#rescale -/+1
pain_longterm<- transform(pain_longterm, pain_longterm$TE==ifelse(Back.pain.or.Pain_Is.more.or.less.better.1=="more.is.better", pain_longterm$TE*-1, pain_longterm$TE*1)) 


#drop NAs
pain_longterm %>% drop_na(TE)->pain_longterm
#check for missing Sds and NAs after imputation!!!!

#calculate pairwise RE MA

pain_longterm_pwise <- netmeta(TE, seTE, treat1, treat2, studlab,
                                data = pain_longterm, sm = "SMD")


# Calculate  for all pairwise meta-analyses
pain_longterm_pwise<- netpairwise(pain_longterm_pwise,method.tau="REML")
pain_longterm_pwise

## Forest 
forest(pain_longterm_pwise)


#show tau and subgroup
a<-pain_longterm_pwise$tau.w
b<-pain_longterm_pwise$bylevs
c<-as.data.frame(a,b)

#create plot of tau
#create box plots
d<-ggplot(c, aes(x = b,y=a)) + 
  geom_point() +
  labs(title="Meta-analyses Pain long-term", x="Pairwise Comparisons", y="Tau")
d
#coordinates flipped
d + 
  coord_flip()

#find trials/analyses that lead to (very high heterogeneity)
#per:pla   tau=2.8369
pain_longterm %>% filter(str_detect(treat1,"per|pla"))%>%
  filter(str_detect(treat2,"per|pla"))-> pain_longterm_per_pla

#suspicious studies
#Moussa 2016 #10207




############################################################################################################################################################################################
############################################################################################################################################################################################

#find/fit outliers, funnel plot and meta-regression for k>10

pain_longterm_pwise$k.all.w #number of studies in MA


############################################################################################################################################################################################
#exe:man

pain_longterm %>% filter(str_detect(treat1,"exe|man"))%>%
  filter(str_detect(treat2,"exe|man"))%>%
  arrange(treat1)-> pain_longterm_exe_man

#rescale -/+1 as some trials they have the wrong sign
pain_longterm_exe_man %>% mutate(TE=case_when(!treat1=="man"~TE,treat1=="man"~ TE*(-1)))->pain_longterm_exe_man

#run RE MA
exe_man_longterm <- metagen(TE = TE,
                             seTE = seTE,
                             studlab = studlab,
                             data = pain_longterm_exe_man,
                             sm = "SMD",
                             fixed = FALSE,
                             random = TRUE,
                             method.tau = "REML",prediction=TRUE)
forest(exe_man_longterm)

find.outliers(exe_man_longterm)
#"Nambi 2018"  

exe_man.inf <- InfluenceAnalysis(exe_man_longterm, random = TRUE)
plot(exe_man.inf, "baujat")
plot(exe_man.inf, "influence")

#"Nambi 2018"  
# update.meta(exe_man_longterm, exclude = c(9)) %>% 
#forest()

#fit meta-regression with baseline pain and disability values
exe_man_reg_basepain_shortterm <- metareg(exe_man_longterm, ~XXX)
bubble(exe_man_reg_basepain_shortterm, studlab = TRUE)

exe_man_reg_basedisab_shortterm <- metareg(exe_man_longterm, ~XXX)
bubble(exe_man_reg_basedisab_shortterm, studlab = TRUE)

#funnel plot
funnel(exe_man_longterm,studlab = FALSE)


####################################################################################################################################################################################################################
####################################################################################################################################################################################################################
#Bayesian NMA long-term

pain_longterm%>%
  dplyr::select(c(1:5),c(6,9)) %>%
  pivot_longer(-studlab,
               names_to = c(".value"),
               names_pattern = "(.)") -> pain_longterm

#look for multiple arms in one study 
pain_longterm %>% pull(studlab) %>% 
  table() %>% 
  {.[. > 2]}


#remove unneccessary arms of multiple group trials and correct se
pain_longterm %>% group_by(studlab) %>% filter(n()>2) %>% 
  group_by(studlab) %>% slice(-c(2:4)) %>%
  group_by(studlab) %>% mutate(s=case_when(!is.na(s)~s,is.na(s)~ sqrt(prod(s,na.rm =TRUE)*0.5))) ->d

#combine tibbles 
pain_longterm %>% group_by(studlab) %>% filter(!n()>2) ->c
pain_longterm <-rbind(c,d)



#set-up network
pain_longterm_net <- set_agd_contrast(pain_longterm, 
                                       study = studlab,
                                       trt =t,
                                       y =T, 
                                       se =s,
                                       sample_size=n,trt_ref="exe")
pain_longterm_net


#plot network structure
plot(pain_longterm_net, weight_edges = TRUE, weight_nodes = TRUE)

#fit RE model
res_pain_longterm <- nma(pain_longterm_net, 
                          seed = 1150676438,
                          trt_effects = "random",
                          prior_trt = normal(scale = 1), #okay?
                          prior_het = half_normal(scale = 0.5), #okay?
                          adapt_delta = 0.99)
print(res_pain_longterm)

#plot posterior distribution
plot_prior_posterior(res_pain_longterm)

print(res_pain_longterm, pars = c("d", "delta"))

pairs(res_pain_longterm)#do not understand this

#model fit
dic_res_pain_longterm <- dic(res_pain_longterm)
plot(dic_res_pain_longterm)

#find relative effects
res_pain_longterm_releffects <- relative_effects(res_pain_longterm, trt_ref = "pla")

plot(res_pain_longterm_releffects)+
  geom_vline(xintercept=-0.5,linetype='dashed', color='red')+
  geom_vline(xintercept=0.5,linetype='dashed', color='red')


#ranking
ranks_res_pain_longterm <- posterior_ranks(res_pain_longterm)
plot(ranks_res_pain_longterm)

#cumulative ranking probabilities
rankprobs_res_pain_longterm  <- posterior_rank_probs(res_pain_longterm)
plot(rankprobs_res_pain_longterm)
cumrankprob_res_pain_longterm <- posterior_rank_probs(res_pain_longterm, cumulative = TRUE)
plot(cumrankprob_res_pain_longterm)


#node splitting
get_nodesplits(pain_longterm_net, include_consistency = TRUE)


#checking for inconsistency

#UME
pain_longterm_net_ume <- nma(pain_longterm_net, 
                              consistency = "ume",
                              trt_effects = "random",
                              prior_trt = normal(scale = 1),#okay?
                              prior_het = normal(scale = 0.5))#okay?
pain_longterm_net_ume

#model comparison
dic_pain_longterm_net_ume <- dic(pain_longterm_net_ume)

plot(dic_res_pain_longterm, dic_pain_longterm_net_ume, point_alpha = 0.5, interval_alpha = 0.2)


#node splitting
pain_longterm_net_nodesplit <- nma(pain_longterm_net, 
                                    consistency = "nodesplit",
                                    trt_effects = "random",
                                    prior_trt = normal(scale = 1), #okay?
                                    prior_het = normal(scale = 0.5))#okay?
summary(pain_longterm_net_nodesplit)

plot(pain_longterm_net_nodesplit) +
  ggplot2::theme(legend.position = "bottom", legend.direct = "horizontal")




##########################################################################################################################################################################
##########################################################################################################################################################################
##########################################################################################################################################################################
##########################################################################################################################################################################
#Main analysis time course NMA 
#Variables needed:
#studyID, time, treatment, y = mean, se = standard error of the mean, n = sample size


#count number of arms in class
dat_prim_class %>% group_by(PrimaryClass) %>%summarise(N_in_ARMS=sum(Back.Pain_N,na.rm=TRUE))->N_ARMS_timecourse

#enough participants

#rename data set
dat_prim_class ->dat_prim_class_timecourse_backpain


#select necessary columns and rename them
dat_prim_class_timecourse_backpain %>% select(INFO_Author.Year,Baseline.Back.Pain_Mean,Baseline.Back.Pain_SD,POPULATION_N.enrolled,Back.pain.or.Pain_Is.more.or.less.better., Back.Pain_Week, current.time.label,PrimaryClass,Back.Pain_Mean,Back.Pain_N, Back.Pain_SD)   %>%  
rename(studyID=INFO_Author.Year, y=Back.Pain_Mean,n=Back.Pain_N, s=Back.Pain_SD,time= current.time.label,treatment=PrimaryClass,y0=Baseline.Back.Pain_Mean,s0=Baseline.Back.Pain_SD,n0=POPULATION_N.enrolled)->dat_prim_class_timecourse_backpain

#add baseline data
dat_prim_class_timecourse_backpain %>% select(studyID:treatment,-time) %>% 
  pivot_longer(!c(studyID,Back.pain.or.Pain_Is.more.or.less.better.,Back.Pain_Week,treatment),names_to=c(".value","time") ,names_pattern  ="(.)(.)")->xy

dat_prim_class_timecourse_backpain %>% select(c(studyID,Back.pain.or.Pain_Is.more.or.less.better.,Back.Pain_Week,time,treatment,y,n,s))-> dat

rbind(dat,xy)-> dat_prim_class_timecourse_backpain

dat_prim_class_timecourse_backpain %>%arrange(studyID)->dat_prim_class_timecourse_backpain

#calculate standard errors
dat_prim_class_timecourse_backpain$se<-dat_prim_class_timecourse_backpain$s/sqrt(dat_prim_class_timecourse_backpain$n)

#rescale -/+1 means
dat_prim_class_timecourse_backpain<- transform(dat_prim_class_timecourse_backpain, dat_prim_class_timecourse_backpain$y==ifelse(Back.pain.or.Pain_Is.more.or.less.better.=="more.is.better", dat_prim_class_timecourse_backpain$y*-1, dat_prim_class_timecourse_backpain$y*1))

#select necessary data for analysis
dat_prim_class_timecourse_backpain %>% select(studyID,time,treatment,y,se,n)->dat_prim_class_timecourse_backpain



#time needs to be numeric
dat_prim_class_timecourse_backpain$time[dat_prim_class_timecourse_backpain$time=="00dto01d"] <- 1
dat_prim_class_timecourse_backpain$time[dat_prim_class_timecourse_backpain$time=="01dto03mo"] <- 2
dat_prim_class_timecourse_backpain$time[dat_prim_class_timecourse_backpain$time=="03to12mo"] <- 3
dat_prim_class_timecourse_backpain$time[dat_prim_class_timecourse_backpain$time=="12moplus"] <- 4

#drop NAs
dat_prim_class_timecourse_backpain %>% drop_na(y,se,n,time)->dat_prim_class_timecourse_backpain

#remove data with "double baseline" arms
dat_prim_class_timecourse_backpain%>% distinct()->dat_prim_class_timecourse_backpain

#as numeric
dat_prim_class_timecourse_backpain$y<-as.numeric(dat_prim_class_timecourse_backpain$y)
dat_prim_class_timecourse_backpain$se<-as.numeric(dat_prim_class_timecourse_backpain$se)
dat_prim_class_timecourse_backpain$time<-as.numeric(dat_prim_class_timecourse_backpain$time)





#################################################################################################################################################################################
#################################################################################################################################################################################


#sds of zero in full data set
# consider setting to NA and then impute !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
if(FALSE){
  dat_prim_class_timecourse_backpain %>%
    filter(!studyID%in% c("Arneja 2016","Cheing 1999","Hurley 2015",
                          "Cohen 2008",
                          "Ferreira 2017",
                          "Hwang 2013",
                          "Yoo 2014")) -> dat_prim_class_timecourse_backpain 


#missing study arms in full data set
  dat_prim_class_timecourse_backpain %>%
    filter(!studyID%in% c("Ekman 2005",
                          "Schinhan 2016 ","Schinhan 2016",
                          "Marchand 1993", "Patel 2016","Saper 2009","Saper 2009 ")) -> dat_prim_class_timecourse_backpain 
  
#Saper has empty space [1] "Saper 2009 "
#"Schinhan 2016" has empty space
#dat_prim_class_timecourse_backpain[2110,1]
  
  


  dat_prim_class_timecourse_backpain %>%
    filter(!studyID%in% c("Brinkhaus 2006","Facci 2011","Fischgrund 2019",
                          "Durmus 2014","Harkapaa 1989","Kopecky 2017","Licciardone 2013",
                          "Markman 2015",
                          "Monticone 2014",
                          "Murakibhavi 2011",
                          "Paolucci 2012",
                          "Patel 2012","Spinhoven 2004",
                          "Turner 1982",
                          "Turner 1988",
                          "Turner 1990",
                          "Turner 1993")) -> dat_prim_class_timecourse_backpain


  dat_prim_class_timecourse_backpain %>%
    filter(!studyID%in% c("Bendix 1998","Barendse 2001",
                          "Carvalho 2016",
                          "Gordon 2010",
                          "Joseph 2018",
                          "Licciardone 2016",
                          "Skljarevski 2010",
                          "Smeets 2006",
                          "Tavafian 2017",
                          "Yun 2012")) -> dat_prim_class_timecourse_backpain 
}

#################################################################################################################################################################################
#################################################################################################################################################################################


#as numeric
dat_prim_class_timecourse_backpain$y<-as.numeric(dat_prim_class_timecourse_backpain$y)
dat_prim_class_timecourse_backpain$se<-as.numeric(dat_prim_class_timecourse_backpain$se)
dat_prim_class_timecourse_backpain$time<-as.numeric(dat_prim_class_timecourse_backpain$time)

# look for studies with se < or =0
#dat_prim_class_timecourse_backpain%>%filter(se<=0)->xay

#dat_prim_class %>% filter(Back.Pain_SD==0)->cv


#################################################################################################################################################################################
#################################################################################################################################################################################
#set-up

#count number of arms and studies
dat_prim_class_timecourse_backpain  %>%count(studyID)->count_studies


#create network
network.pain <- mb.network(dat_prim_class_timecourse_backpain, reference = "pla")
print(network.pain)

#plot network
plot(network.pain, layout=igraph::as_star(), label.distance = 5)

#Time course
# Draw plot of raw study responses over time
timeplot(network.pain, plotby = "rel") 


#################################################################################################################################################################################
#################################################################################################################################################################################
#model formulation

#common effects model with linear time course
pain_res_common_lin <- mb.run(network.pain, fun=tpoly(degree=1, pool.1 = "rel", method.1 = "common"), link="smd",
                             n.iter=30000, n.burnin=10000)
summary(pain_res_common_lin)

mcmcplots::mcmcplot(pain_res_common_lin)

devplot(pain_res_commo_lin)

pain_res_common_lin$BUGSoutput$median$totresdev
nrow(network.pain$data.ab)


#random effects model with linear time course
pain_res_random_lin <- mb.run(network.pain, fun=tpoly(degree=1, pool.1 = "rel", method.1 = "random"), link="smd",
                n.iter=30000, n.burnin=10000)

mcmcplots::mcmcplot(pain_res_random_lin)

devplot(pain_res_random_lin)

pain_res_random_lin$BUGSoutput$median$totresdev
nrow(network.pain$data.ab)


#random effects model with linear splines
pain_res_random_lin_splines <- mb.run(network.pain, fun= tspline(type="bs", knots=2, pool.1 = "rel", method.1 = "random"), link="smd",
                       n.iter=30000, n.burnin=10000)

summary(pain_res_random_lin_splines)

mcmcplots::mcmcplot(pain_res_random_lin_splines)

devplot(pain_res_random_lin_splines)
fitplot(pain_res_random_lin_splines, n.iter=1000)

pain_res_random_lin_splines$BUGSoutput$median$totresdev
nrow(network.pain$data.ab)


#################################################################################################################################################################################
#################################################################################################################################################################################
#final model

#forest plot
plot(pain_res_random_lin_splines)+
geom_vline(xintercept=-0.5,linetype='dashed', color='red')+
  geom_vline(xintercept=0.5,linetype='dashed', color='red')





#Ranking of treatments
ranks<-rank(mbnma, lower_better=TRUE)

plot(ranks)


#nodesplitting
#if necessary install.packages("overlapping")
nodesplit <- mb.nodesplit(network.pain, fun=tpoly(degree=1, pool.1 = "rel", method.1 = "random"), link="smd",
                          n.iter=30000, n.burnin=10000, nodesplit.parameters="all")
print(nodesplit)

# Plot forest plots of direct and indirect results for each node-split comparison
plot(nodesplit, plot.type="forest")

# Plot posterior densities of direct and indirect results for each node-split comparisons
plot(nodesplit, plot.type="density")


#################################################################################################################################################################################
#################################################################################################################################################################################
#class-effects model

#Variables needed:
#studyID, time, treatment, y = mean, se = standard error of the mean, n = sample size, class
#count number of arms per treatment
dat_prim_treat %>% group_by(PrimaryTreatment) %>%summarise(N_in_ARMS=sum(Back.Pain_N,na.rm=TRUE))->N_ARMS_timecourse_treat

#tra_man = 31 keep in mind will leave in for now


#rename data set
dat_prim_treat-> dat_prim_treat_timecourse_backpain


#select necessary columns and rename them
dat_prim_treat_timecourse_backpain %>% select(INFO_Author.Year,Baseline.Back.Pain_Mean,Baseline.Back.Pain_SD,POPULATION_N.enrolled,Back.pain.or.Pain_Is.more.or.less.better., Back.Pain_Week, current.time.label,PrimaryClass,PrimaryTreatment,Back.Pain_Mean,Back.Pain_N, Back.Pain_SD)   %>%  
  rename(studyID=INFO_Author.Year, y=Back.Pain_Mean,n=Back.Pain_N, s=Back.Pain_SD,time= current.time.label,treatment=PrimaryTreatment,y0=Baseline.Back.Pain_Mean,s0=Baseline.Back.Pain_SD,n0=POPULATION_N.enrolled,class=PrimaryClass)->dat_prim_treat_timecourse_backpain

#add baseline data
dat_prim_treat_timecourse_backpain %>% select(studyID:treatment,-time) %>% 
  pivot_longer(!c(studyID,Back.pain.or.Pain_Is.more.or.less.better.,Back.Pain_Week,treatment,class),names_to=c(".value","time") ,names_pattern  ="(.)(.)")->xy



dat_prim_treat_timecourse_backpain%>% select(c(studyID,Back.pain.or.Pain_Is.more.or.less.better.,Back.Pain_Week,time,treatment,class,y,n,s))-> dat


rbind(dat,xy)-> dat_prim_treat_timecourse_backpain

dat_prim_treat_timecourse_backpain %>%arrange(studyID)->dat_prim_treat_timecourse_backpain



#calculate standard errors
dat_prim_treat_timecourse_backpain$se<-dat_prim_treat_timecourse_backpain$s/sqrt(dat_prim_treat_timecourse_backpain$n)

#rescale -/+1 means
dat_prim_treat_timecourse_backpain <- transform(dat_prim_treat_timecourse_backpain, dat_prim_treat_timecourse_backpain$y==ifelse(Back.pain.or.Pain_Is.more.or.less.better.=="more.is.better", dat_prim_treat_timecourse_backpain$y*-1, dat_prim_treat_timecourse_backpain$y*1))

#select necessary data for analysis
dat_prim_treat_timecourse_backpain %>% select(studyID,time,treatment,class,y,se,n)->dat_prim_treat_timecourse_backpain


#time needs to be numeric
dat_prim_treat_timecourse_backpain$time[dat_prim_treat_timecourse_backpain$time=="00dto01d"] <- 1
dat_prim_treat_timecourse_backpain$time[dat_prim_treat_timecourse_backpain$time=="01dto03mo"] <- 2
dat_prim_treat_timecourse_backpain$time[dat_prim_treat_timecourse_backpain$time=="03to12mo"] <- 3
dat_prim_treat_timecourse_backpain$time[dat_prim_treat_timecourse_backpain$time=="12moplus"] <- 4

#drop NAs
dat_prim_treat_timecourse_backpain %>% drop_na(y,se,n,time)->dat_prim_treat_timecourse_backpain

#remove data with "double baseline" arms
dat_prim_treat_timecourse_backpain%>% distinct()->dat_prim_treat_timecourse_backpain

#as numeric
dat_prim_treat_timecourse_backpain$y<-as.numeric(dat_prim_treat_timecourse_backpain$y)
dat_prim_treat_timecourse_backpain$se<-as.numeric(dat_prim_treat_timecourse_backpain$se)
dat_prim_treat_timecourse_backpain$time<-as.numeric(dat_prim_treat_timecourse_backpain$time)




#################################################################################################################################################################################
#################################################################################################################################################################################
#main analysis data set 



######################
if(FALSE){
  dat_prim_treat_timecourse_backpain %>%
    filter(!studyID%in% c("Arneja 2016","Cheing 1999","Hurley 2015",
                          "Cohen 2008",
                          "Ferreira 2017",
                          "Hwang 2013",
                          "Yoo 2014")) -> dat_prim_treat_timecourse_backpain




#missing study arms in full data set
  dat_prim_treat_timecourse_backpain %>%
  filter(!studyID%in% c("Ekman 2005",
                        "Schinhan 2016 ","Schinhan 2016",
                        "Marchand 1993", "Patel 2016","Saper 2009","Saper 2009 ")) -> dat_prim_treat_timecourse_backpain

#Saper has empty space [1] "Saper 2009 "
#"Schinhan 2016" has empty space
#dat_prim_class_timecourse_backpain[2110,1] 
  
  
  dat_prim_treat_timecourse_backpain %>%
    filter(!studyID%in% c("Brinkhaus 2006","Facci 2011","Fischgrund 2019",
                          "Durmus 2014","Harkapaa 1989","Kopecky 2017","Licciardone 2013",
                          "Markman 2015",
                          "Monticone 2014",
                          "Murakibhavi 2011",
                          "Paolucci 2012",
                          "Patel 2012","Spinhoven 2004",
                          "Turner 1982",
                          "Turner 1988",
                          "Turner 1990",
                          "Turner 1993")) -> dat_prim_treat_timecourse_backpain

#wrong coding of class and treatment 

  dat_prim_treat_timecourse_backpain %>%
    filter(!studyID%in% c("Staats 2016",
                          "NarcisoGarcia 2013",
                          "Nambi 2018",
                          "Luo 2019","Yilmaz Yelvar 2017",
                          "Bendix 2000",
                          "Brouwer 2017",
                          "Veihelmann 2006",
                          "Cambron 2014",
                          "Yurdakul 2019")) -> dat_prim_treat_timecourse_backpain 

  dat_prim_treat_timecourse_backpain  %>%
    filter(!studyID%in% c("Bendix 1998","Barendse 2001",
                          "Carvalho 2016",
                          "Joseph 2018",
                          "Licciardone 2016",
                          "Skljarevski 2010",
                          "Smeets 2006",
                          "Tavafian 2017",
                          "Yun 2012")) -> dat_prim_treat_timecourse_backpain  
}
  
#################################################################################################################################################################################
#################################################################################################################################################################################
#set-up

# look for studies with se < or =0
#dat_prim_treat_timecourse_backpain%>%filter(se<=0)->xayx

#dat_prim_treat %>% filter(Back.Pain_SD==0)->cv

#create network
network.pain_class <- mb.network(dat_prim_treat_timecourse_backpain, reference = "pla_pla")
print(network.pain_class)



#plot network
plot(network.pain_class, layout=igraph::as_star(), label.distance = 6.5)

#Time course
# Draw plot of raw study responses over time
timeplot(network.pain_class, plotby = "rel") 


#################################################################################################################################################################################
#################################################################################################################################################################################
#model formulation

#common effects model with linear time course
pain_res_common_lin_class <- mb.run(network.pain_class, fun=tpoly(degree=1, pool.1 = "rel", method.1 = "common"), link="smd",
                              n.iter=30000, n.burnin=10000)
summary(pain_res_common_lin_class)

mcmcplots::mcmcplot(pain_res_common_lin_class)

devplot(pain_res_common_lin_class)

pain_res_common_lin_class$BUGSoutput$median$totresdev
nrow(network.pain_class$data.ab)


#random effects model with linear time course
pain_res_random_lin_class <- mb.run(network.pain_class, fun=tpoly(degree=1, pool.1 = "rel", method.1 = "random"), link="smd",
                              n.iter=30000, n.burnin=10000)

mcmcplots::mcmcplot(pain_res_random_lin_class)

devplot(pain_res_random_lin_class)

pain_res_random_lin_class$BUGSoutput$median$totresdev
nrow(pain_res_random_lin_class$data.ab)


#random effects model with linear splines
pain_res_random_lin_splines_class <- mb.run(network.pain_class, fun= tspline(type="ls", knots=1, pool.1 = "rel", method.1 = "random"), link="smd",
                                      n.iter=30000, n.burnin=10000)

summary(pain_res_random_lin_splines_class)

mcmcplots::mcmcplot(pain_res_random_lin_splines_class)

devplot(pain_res_random_lin_splines_class)
fitplot(pain_res_random_lin_splines_class, n.iter=1000)

pain_res_random_lin_splines_class $BUGSoutput$median$totresdev
nrow(pain_res_random_lin_splines_class$data.ab)


#################################################################################################################################################################################
#################################################################################################################################################################################
#final model


#forest plot
plot(pain_res_random_lin_splines)+
  geom_vline(xintercept=-0.5,linetype='dashed', color='red')+
  geom_vline(xintercept=0.5,linetype='dashed', color='red')



#Ranking of treatments
ranks<-rank(mbnma, lower_better=TRUE)

plot(ranks)


#nodesplitting
#if necessary install.packages("overlapping")
nodesplit <- mb.nodesplit(network.pain, fun=tpoly(degree=1, pool.1 = "rel", method.1 = "random"), link="smd",
                          n.iter=30000, n.burnin=10000, nodesplit.parameters="all")
print(nodesplit)

# Plot forest plots of direct and indirect results for each node-split comparison
plot(nodesplit, plot.type="forest")

# Plot posterior densities of direct and indirect results for each node-split comparisons
plot(nodesplit, plot.type="density")




#################################################################################################################################################################################
#################################################################################################################################################################################
#Excluding studies with imputed missing SD and imputed medians and data extracted from figures
#filter out post-treatment and BASELINE SDs with NAs
dat_prim_class  %>% filter(OUTCOME_outcomes.are.present_Back.pain=="Yes")%>%
  filter(!is.na(Back.Pain_SD)) %>% filter(!is.na(Baseline.Back.Pain_SD))->dat_prim_class_medandSDs

#also excluding thing
dat_prim_class_medandSDs %>% filter(!Back.Pain_Notes.on.Followup.data_Note.on.SD=="SD calculated from non-parametric data (e.g. median, max/min)")%>%
  filter(!Back.Pain_Notes.on.Followup.data_Note.on.SD=="SD extracted from a Figure") %>%
  filter(!Back.Pain_Notes.on.Followup.data_Note.on.SD=="extracted from a Figure and transformed in some way ")->dat_prim_class_medandSDs

#table(dat_prim_class_medandSDs$Back.Pain_Notes.on.Followup.data_Note.on.SD, useNA = 'always')

#Variables needed:
#studyID, time, treatment, y = mean, se = standard error of the mean, n = sample size

#rename data set
dat_prim_class_medandSDs-> dat_prim_class_timecourse_backpain_medandSDs

#count number of arms in class
dat_prim_class_timecourse_backpain_medandSDs %>% group_by(PrimaryClass) %>%summarise(N_in_ARMS=sum(Back.Pain_N,na.rm=TRUE))->N_ARMS_timecourse_medandSDs
#enough participants

#select necessary columns and rename them
dat_prim_class_timecourse_backpain_medandSDs %>% select(INFO_Author.Year,Baseline.Back.Pain_Mean,Baseline.Back.Pain_SD,POPULATION_N.enrolled,Back.pain.or.Pain_Is.more.or.less.better., Back.Pain_Week, current.time.label,PrimaryClass,Back.Pain_Mean,Back.Pain_N, Back.Pain_SD)   %>%  
  rename(studyID=INFO_Author.Year, y=Back.Pain_Mean,n=Back.Pain_N, s=Back.Pain_SD,time= current.time.label,treatment=PrimaryClass,y0=Baseline.Back.Pain_Mean,s0=Baseline.Back.Pain_SD,n0=POPULATION_N.enrolled)->dat_prim_class_timecourse_backpain_medandSDs

#add baseline data
dat_prim_class_timecourse_backpain_medandSDs %>% select(studyID:treatment,-time) %>% 
  pivot_longer(!c(studyID,Back.pain.or.Pain_Is.more.or.less.better.,Back.Pain_Week,treatment),names_to=c(".value","time") ,names_pattern  ="(.)(.)")->xy



dat_prim_class_timecourse_backpain_medandSDs %>% select(c(studyID,Back.pain.or.Pain_Is.more.or.less.better.,Back.Pain_Week,time,treatment,y,n,s))-> dat


rbind(dat,xy)-> dat_prim_class_timecourse_backpain_medandSDs

dat_prim_class_timecourse_backpain_medandSDs %>%arrange(studyID)->dat_prim_class_timecourse_backpain_medandSDs



#calculate standard errors
dat_prim_class_timecourse_backpain_medandSDs$se<-dat_prim_class_timecourse_backpain_medandSDs$s/sqrt(dat_prim_class_timecourse_backpain_medandSDs$n)

#rescale -/+1 means
dat_prim_class_timecourse_backpain_medandSDs <- transform(dat_prim_class_timecourse_backpain_medandSDs, dat_prim_class_timecourse_backpain_medandSDs$y==ifelse(Back.pain.or.Pain_Is.more.or.less.better.=="more.is.better", dat_prim_class_timecourse_backpain_medandSDs$y*-1, dat_prim_class_timecourse_backpain_medandSDs$y*1))

#select necessary data for analysis
dat_prim_class_timecourse_backpain_medandSDs%>% select(studyID,time,treatment,y,se,n)->dat_prim_class_timecourse_backpain_medandSDs



#time needs to be numeric
dat_prim_class_timecourse_backpain_medandSDs$time[dat_prim_class_timecourse_backpain_medandSDs$time=="00dto01d"] <- 1
dat_prim_class_timecourse_backpain_medandSDs$time[dat_prim_class_timecourse_backpain_medandSDs$time=="01dto03mo"] <- 2
dat_prim_class_timecourse_backpain_medandSDs$time[dat_prim_class_timecourse_backpain_medandSDs$time=="03to12mo"] <- 3
dat_prim_class_timecourse_backpain_medandSDs$time[dat_prim_class_timecourse_backpain_medandSDs$time=="12moplus"] <- 4

#drop NAs
dat_prim_class_timecourse_backpain_medandSDs %>% drop_na(y,se,n,time)->dat_prim_class_timecourse_backpain_medandSDs

#remove data with "double baseline" arms
dat_prim_class_timecourse_backpain_medandSDs%>% distinct()->dat_prim_class_timecourse_backpain_medandSDs

#as numeric
dat_prim_class_timecourse_backpain_medandSDs$y<-as.numeric(dat_prim_class_timecourse_backpain_medandSDs$y)
dat_prim_class_timecourse_backpain_medandSDs$se<-as.numeric(dat_prim_class_timecourse_backpain_medandSDs$se)
dat_prim_class_timecourse_backpain_medandSDs$time<-as.numeric(dat_prim_class_timecourse_backpain_medandSDs$time)



#################################################################################################################################################################################
#################################################################################################################################################################################
#take data out
#set-up

# look for studies with se < or =0
#dat_prim_class_timecourse_backpain_medandSDs%>%filter(se<=0)->xayx

#dat_prim_class %>% filter(Back.Pain_SD==0)->cv

#sds of zero in full data set
# consider setting to NA and then impute !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
if(FALSE){
  dat_prim_class_timecourse_backpain_medandSDs %>%
    filter(!studyID%in% c("Cheing 1999",
                          "Cohen 2008",
                          "Ferreira 2017",
                          "Hurley 2015","Hwang 2013",
                          "Yoo 2014")) -> dat_prim_class_timecourse_backpain_medandSDs 
  
  
  #missing study arms in full data set
  dat_prim_class_timecourse_backpain_medandSDs %>%
    filter(!studyID%in% c("Ekman 2005",
                          "Schinhan 2016 ","Schinhan 2016",
                          "Fischgrund 2019", "Patel 2016",
                          "Harkapaa 1989","Kopecky 2017","Spinhoven 2004")) -> dat_prim_class_timecourse_backpain_medandSDs
  
  
  #"Schinhan 2016" has empty space

  
  
  
  
  dat_prim_class_timecourse_backpain_medandSDs %>%
    filter(!studyID%in% c("Brinkhaus 2006",
                          "Durmus 2014",
                          "Markman 2015",
                          "Monticone 2014",
                          "Murakibhavi 2011",
                          "Paolucci 2012",
                          "Patel 2012",
                          "Turner 1982",
                          "Turner 1988",
                          "Turner 1990",
                          "Turner 1993")) -> dat_prim_class_timecourse_backpain_medandSDs
  
  
  dat_prim_class_timecourse_backpain_medandSDs %>%
    filter(!studyID%in% c("Carvalho 2016",
                          "Skljarevski 2010",
                          "Smeets 2006",
                          "Tavafian 2017",
                          "Yun 2012")) -> dat_prim_class_timecourse_backpain_medandSDs 
}
#################################################################################################################################################################################
#################################################################################################################################################################################
#set-up

#count number of arms and studies
dat_prim_class_timecourse_backpain_medandSDs %>%count(studyID)->count_studies


#create network
network.pain_medandSDs <- mb.network(dat_prim_class_timecourse_backpain_medandSDs, reference = "pla")
print(network.pain_medandSDs )

#plot network
plot(network.pain_medandSDs, layout=igraph::as_star(), label.distance = 5)

#Time course
# Draw plot of raw study responses over time
timeplot(network.pain_medandSDs, plotby = "rel") 


#################################################################################################################################################################################
#################################################################################################################################################################################
#model formulation

#common effects model with linear time course
pain_res_common_lin_medandSDs <- mb.run(network.pain_medandSDs, fun=tpoly(degree=1, pool.1 = "rel", method.1 = "common"), link="smd",
                              n.iter=30000, n.burnin=10000)
pain_res_common_lin_medandSDs

mcmcplots::mcmcplot(pain_res_common_lin_medandSDs)

devplot(pain_res_common_lin_medandSDs)

pain_res_common_lin_medandSDs$BUGSoutput$median$totresdev
nrow(network.pain_medandSDs$data.ab)


#random effects model with linear time course
pain_res_random_lin_medandSDs <- mb.run(network.pain_medandSDs, fun=tpoly(degree=1, pool.1 = "rel", method.1 = "random"), link="smd",
                              n.iter=30000, n.burnin=10000)

mcmcplots::mcmcplot(pain_res_random_lin_medandSDs)

devplot(pain_res_random_lin_medandSDs)

pain_res_random_lin_medandSDs$BUGSoutput$median$totresdev
nrow(network.pain_medandSDs$data.ab)


#random effects model with linear splines
pain_res_random_lin_splines_medandSDs <- mb.run(network.pain_medandSDs, fun= tspline(type="ls", knots=1, pool.1 = "rel", method.1 = "random"), link="smd",
                                      n.iter=30000, n.burnin=10000)

pain_res_random_lin_splines_medandSDs

mcmcplots::mcmcplot(pain_res_random_lin_splines_medandSDs)

devplot(pain_res_random_lin_splines_medandSDs)
fitplot(pain_res_random_lin_splines_medandSDs, n.iter=1000)

pain_res_random_lin_splines_medandSDs$BUGSoutput$median$totresdev
nrow(network.pain_medandSDs$data.ab)


#################################################################################################################################################################################
#################################################################################################################################################################################
#final model

#forest plot
plot(pain_res_random_lin_medandSDs)+
  geom_vline(xintercept=-0.5,linetype='dashed', color='red')+
  geom_vline(xintercept=0.5,linetype='dashed', color='red')



#Ranking of treatments
ranks<-rank(mbnma, lower_better=TRUE)

plot(ranks)


#nodesplitting
#if necessary install.packages("overlapping")
nodesplit <- mb.nodesplit(network.pain, fun=tpoly(degree=1, pool.1 = "rel", method.1 = "random"), link="smd",
                          n.iter=30000, n.burnin=10000, nodesplit.parameters="all")
print(nodesplit)

# Plot forest plots of direct and indirect results for each node-split comparison
plot(nodesplit, plot.type="forest")

# Plot posterior densities of direct and indirect results for each node-split comparisons
plot(nodesplit, plot.type="density")


#################################################################################################################################################################################
#################################################################################################################################################################################
#Excluding studies with radicular pain

dat_prim_class  %>% filter(!POPULATION_Back.pain.type=="radicular syndromes (radicular pain/sciatica/leg pain, radiculopathy, spinal stenosis)")->dat_prim_class_nonspecific

table(dat_prim_class$POPULATION_Back.pain.type, useNA = 'always')



#Variables needed:
#studyID, time, treatment, y = mean, se = standard error of the mean, n = sample size

#filter out study arms with n<50
dat_prim_class_nonspecific%>% filter(Back.Pain_N>=50)-> dat_prim_class_timecourse_50_backpain_nonspecific


#remove single arms 
dat_prim_class_timecourse_50_backpain_nonspecific %>% add_count(INFO_Author.Year) %>% filter(n > 1) -> dat_prim_class_timecourse_50_backpain_nonspecific
dat_prim_class_timecourse_50_backpain_nonspecific%>% group_by(INFO_Author.Year) %>% add_count(current.time.label,name="nn") %>% filter(nn > 1) -> dat_prim_class_timecourse_50_backpain_nonspecific

#select necessary columns and rename them
dat_prim_class_timecourse_50_backpain_nonspecific %>% select(INFO_Author.Year,Baseline.Back.Pain_Mean,Baseline.Back.Pain_SD,POPULATION_N.enrolled,Back.pain.or.Pain_Is.more.or.less.better., Back.Pain_Week, current.time.label,PrimaryClass,Back.Pain_Mean,Back.Pain_N, Back.Pain_SD)   %>%  
  rename(studyID=INFO_Author.Year, y=Back.Pain_Mean,n=Back.Pain_N, s=Back.Pain_SD,time= current.time.label,treatment=PrimaryClass,y0=Baseline.Back.Pain_Mean,s0=Baseline.Back.Pain_SD,n0=POPULATION_N.enrolled)->dat_prim_class_timecourse_50_backpain_nonspecific

#add baseline data
dat_prim_class_timecourse_50_backpain_nonspecific %>% select(studyID:treatment,-time) %>% 
  pivot_longer(!c(studyID,Back.pain.or.Pain_Is.more.or.less.better.,Back.Pain_Week,treatment),names_to=c(".value","time") ,names_pattern  ="(.)(.)")->xy



dat_prim_class_timecourse_50_backpain_nonspecific %>% select(c(studyID,Back.pain.or.Pain_Is.more.or.less.better.,Back.Pain_Week,time,treatment,y,n,s))-> dat


rbind(dat,xy)-> dat_prim_class_timecourse_50_backpain_nonspecific

dat_prim_class_timecourse_50_backpain_nonspecific %>%arrange(studyID)->dat_prim_class_timecourse_50_backpain_nonspecific



#calculate standard errors
dat_prim_class_timecourse_50_backpain_nonspecific$se<-dat_prim_class_timecourse_50_backpain_nonspecific$s/sqrt(dat_prim_class_timecourse_50_backpain_nonspecific$n)

#rescale -/+1 means
dat_prim_class_timecourse_50_backpain_nonspecific<- transform(dat_prim_class_timecourse_50_backpain_nonspecific, dat_prim_class_timecourse_50_backpain_nonspecific$y==ifelse(Back.pain.or.Pain_Is.more.or.less.better.=="more.is.better", dat_prim_class_timecourse_50_backpain_nonspecific$y*-1, dat_prim_class_timecourse_50_backpain_nonspecific$y*1))

#select necessary data for analysis
dat_prim_class_timecourse_50_backpain_nonspecific %>% select(studyID,time,treatment,y,se,n)->dat_prim_class_timecourse_50_backpain_nonspecific



#time needs to be numeric
dat_prim_class_timecourse_50_backpain_nonspecific$time[dat_prim_class_timecourse_50_backpain_nonspecific$time=="00dto01d"] <- 1
dat_prim_class_timecourse_50_backpain_nonspecific$time[dat_prim_class_timecourse_50_backpain_nonspecific$time=="01dto03mo"] <- 2
dat_prim_class_timecourse_50_backpain_nonspecific$time[dat_prim_class_timecourse_50_backpain_nonspecific$time=="03to12mo"] <- 3
dat_prim_class_timecourse_50_backpain_nonspecific$time[dat_prim_class_timecourse_50_backpain_nonspecific$time=="12moplus"] <- 4

#drop NAs
dat_prim_class_timecourse_50_backpain_nonspecific %>% drop_na(y,se,n,time)->dat_prim_class_timecourse_50_backpain_nonspecific

#remove data with "double baseline" arms
dat_prim_class_timecourse_50_backpain_nonspecific%>% distinct()->dat_prim_class_timecourse_50_backpain_nonspecific

#as numeric
dat_prim_class_timecourse_50_backpain_nonspecific$y<-as.numeric(dat_prim_class_timecourse_50_backpain_nonspecific$y)
dat_prim_class_timecourse_50_backpain_nonspecific$se<-as.numeric(dat_prim_class_timecourse_50_backpain_nonspecific$se)
dat_prim_class_timecourse_50_backpain_nonspecific$time<-as.numeric(dat_prim_class_timecourse_50_backpain_nonspecific$time)



#################################################################################################################################################################################
#################################################################################################################################################################################
#take data out


######################
#Murakibhavi 2011
#missing mean of immediate term
if(FALSE){
  dat_prim_class_timecourse_50_backpain_nonspecific %>%
    filter(!studyID%in% c("Kopecky 2017","Facci 2011","Harkapaa 1989",
                          "Licciardone 2013")) -> dat_prim_class_timecourse_50_backpain_nonspecific
}



#they exists twice in the data sheet 
if(FALSE){
  dat_prim_class_timecourse_50_backpain_nonspecific %>%
    filter(!studyID%in% c("Tavafian 2017","Bendix 1998","Licciardone 2016","Skljarevski 2010",
                          "Smeets 2006","Yun 2012")) -> dat_prim_class_timecourse_50_backpain_nonspecific
}


#################################################################################################################################################################################
#################################################################################################################################################################################
#set-up

#count number of arms and studies
dat_prim_class_timecourse_50_backpain_nonspecific%>%count(studyID)->count_studies


#create network
network.pain_nonspecific <- mb.network(dat_prim_class_timecourse_50_backpain_nonspecific, reference = "pla")
print(network.pain_nonspecific)

#plot network
plot(network.pain_nonspecific, layout=igraph::as_star(), label.distance = 5)

#Time course
# Draw plot of raw study responses over time
timeplot(network.pain_nonspecific, plotby = "rel") 


#################################################################################################################################################################################
#################################################################################################################################################################################
#model formulation

#common effects model with linear time course
pain_res_common_lin_nonspecific <- mb.run(network.pain_nonspecific, fun=tpoly(degree=1, pool.1 = "rel", method.1 = "common"), link="smd",
                                        n.iter=30000, n.burnin=10000)
pain_res_common_lin_nonspecific

mcmcplots::mcmcplot(pain_res_common_lin_nonspecific)

devplot(pain_res_common_lin_nonspecific)

pain_res_common_lin_nonspecific$BUGSoutput$median$totresdev
nrow(network.pain_nonspecific$data.ab)


#random effects model with linear time course
pain_res_random_lin_nonspecific <- mb.run(network.pain_nonspecific, fun=tpoly(degree=1, pool.1 = "rel", method.1 = "random"), link="smd",
                                        n.iter=30000, n.burnin=10000)

mcmcplots::mcmcplot(pain_res_random_lin_nonspecific)

devplot(pain_res_random_lin_nonspecific)

pain_res_random_lin_nonspecific$BUGSoutput$median$totresdev
nrow(network.pain_nonspecific$data.ab)


#random effects model with linear splines
pain_res_random_lin_splines_nonspecific <- mb.run(network.pain_nonspecific, fun= tspline(type="ls", knots=1, pool.1 = "rel", method.1 = "random"), link="smd",
                                                n.iter=30000, n.burnin=10000)

pain_res_random_lin_splines_nonspecific

mcmcplots::mcmcplot(pain_res_random_lin_splines_nonspecific)

devplot(pain_res_random_lin_splines_nonspecific)
fitplot(pain_res_random_lin_splines_nonspecific, n.iter=1000)

pain_res_random_lin_splines_nonspecific$BUGSoutput$median$totresdev
nrow(network.pain_nonspecific$data.ab)


#################################################################################################################################################################################
#################################################################################################################################################################################
#final model

#forest plot
plot(pain_res_common_lin_nonspecific)+
  geom_vline(xintercept=-0.5,linetype='dashed', color='red')+
  geom_vline(xintercept=0.5,linetype='dashed', color='red')

#Ranking of treatments
ranks<-rank(mbnma, lower_better=TRUE)

plot(ranks)


#nodesplitting
#if necessary install.packages("overlapping")
nodesplit <- mb.nodesplit(network.pain, fun=tpoly(degree=1, pool.1 = "rel", method.1 = "random"), link="smd",
                          n.iter=30000, n.burnin=10000, nodesplit.parameters="all")
print(nodesplit)

# Plot forest plots of direct and indirect results for each node-split comparison
plot(nodesplit, plot.type="forest")

# Plot posterior densities of direct and indirect results for each node-split comparisons
plot(nodesplit, plot.type="density")









#####################################################################################################################################################################################
#####################################################################################################################################################################################
#look at the data set

#look for duplicate study entries
PrimaryClass_CollatedData %>% distinct(INFO_Author.Year,INFO_Covidence.ID)->x

x %>% distinct(INFO_Author.Year,.keep_all = TRUE)->y

require(daff)

diff_data(x,y)->z

render_diff(z)

#SDs equal NULL
dat_prim_class %>% filter(Back.Pain_SD<=0)->cv
dat_prim_class %>% filter(Disability_SD<=0)->cv1
dat_prim_class %>% filter(Leg.or.Sciatic.Pain_SD<=0)->cv2
dat_prim_class %>% filter(Mental.Health_SD<=0)->cv3


#NAs

#filter out SDs with NAs
PrimaryClass_CollatedData  %>% filter(OUTCOME_outcomes.are.present_Back.pain=="Yes")%>%
filter(is.na(Back.Pain_SD) & Back.Pain_N>=0 & Back.Pain_Mean>0) ->xy

#filter out Means
PrimaryClass_CollatedData  %>% filter(OUTCOME_outcomes.are.present_Back.pain=="Yes")%>%
 filter(is.na(Back.Pain_Mean)& Back.Pain_N>=0)->xyx

#filter out scale 
PrimaryClass_CollatedData %>% filter(Back.pain.or.Pain_Is.more.or.less.better.!= "less.is.better") %>%
  filter(Back.pain.or.Pain_Is.more.or.less.better.!= "more.is.better")%>%
  filter(OUTCOME_outcomes.are.present_Back.pain=="Yes")->xyxyx
  

#filter PrimaryTreatment

PrimaryTreatment_CollatedData %>% filter(is.na(PrimaryTreatment))->xyxyx





