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
dat_prim_class %>% filter(current.time.label=="00dto01d")-> dat_prim_class_immediate_disab

#count number of arms in class
dat_prim_class_immediate_disab %>% group_by(PrimaryClass) %>%summarise(N_in_ARMS=sum(Disability_N,na.rm=TRUE))->N_ARMS_immediate_disab

#remove class with less than 50 patients
#edu,mck,mul,per,psy,sur,tra,usu

dat_prim_class_immediate_disab  %>% filter(!PrimaryClass %in% c("edu","mck","mul","per","psy","sur","tra","usu"))->dat_prim_class_immediate_disab 


#remove single arms
dat_prim_class_immediate_disab  %>% add_count(INFO_Author.Year) %>% filter(n > 1) ->dat_prim_class_immediate_disab 




#remove studies
if(FALSE) {
  dat_prim_class_immediate_disab%>%
    filter(!INFO_Author.Year %in% c('Markman 2015'))->dat_prim_class_immediate_disab
  
  #rename Wang to 2005a  
  dat_prim_class_immediate_disab[c(15,16),1]<-"Wang 2005a"
  #Wang 2005 #2312 -> be careful this study has also a counterpart also named Wang 2005 with diff INTs
}

#calculate effect sizes
disab_immediate<-pairwise(treat=PrimaryClass, 
                         mean=Disability_Mean,sd=Disability_SD,n=Disability_N,
                         data =dat_prim_class_immediate_disab , studlab=INFO_Author.Year,sm="SMD")


#rescale -/+1
disab_immediate <- transform(disab_immediate, disab_immediate$TE==ifelse(Disability_Is.more.or.less.better.1=="more.is.better", disab_immediate$TE*-1, disab_immediate$TE*1)) 


#drop NAs
disab_immediate  %>% drop_na(TE,seTE)->disab_immediate 


#calculate pairwise RE MA

disab_immediate_pwise <- netmeta(TE, seTE, treat1, treat2, studlab,
                                data = disab_immediate, sm = "SMD",details.chkmultiarm = TRUE)



###missing data (details.chkmultiarm = TRUE)


# Calculate and print consise results for all pairwise
# meta-analyses

disab_immediate_pwise <- netpairwise(disab_immediate_pwise,method.tau = "REML")
disab_immediate_pwise

## Forest 
forest(disab_immediate_pwise)


#show tau and subgroups
a<-disab_immediate_pwise$tau.w
b<-disab_immediate_pwise$bylevs
c<-as.data.frame(a,b)

#create plot of tau
#create box plots
d<-ggplot(c, aes(x = b,y=a)) +
  geom_point() +
  labs(title="Meta-analyses Disability immediate", x="Pairwise Comparisons", y="Tau")

#coordinates flipped
d +
  coord_flip()


#find trials/analyses that lead to (very high heterogeneity)
#exe:pha tau=1.0953
disab_immediate %>% filter(str_detect(treat1,"exe|pha"))%>%
  filter(str_detect(treat2,"exe|pha"))-> disab_immediate_exe_pha

#suspicious studies
#	Qiu 2007 #1895

#outliers, metaregression funnel plot
#too few studies 



#######################################################################################################################################################################
#######################################################################################################################################################################
#NMA immediate

disab_immediate  %>%
  dplyr::select(c(1:5),c(6,9)) %>%
  pivot_longer(-studlab,
               names_to = c(".value"),
               names_pattern = "(.)") ->disab_immediate 


#look for multiple arms in one study 
disab_immediate  %>% pull(studlab) %>% 
  table() %>% 
  {.[. > 2]}


# #remove unneccessary arms of multiple group trials and correct the se
# pain_immediate %>% group_by(studlab) %>% filter(n()>2) %>% 
#   group_by(studlab) %>% slice(-c(2:4)) %>%
#   group_by(studlab) %>% mutate(s=case_when(!is.na(s)~s,is.na(s)~ sqrt(prod(s,na.rm =TRUE)*0.5))) ->d
# 
# #combine tibbles 
# 
# pain_immediate %>% group_by(studlab) %>% filter(!n()>2) ->c
# 
# 
# pain_immediate <-rbind(c,d)





#set-up network
disab_immediate_net <- set_agd_contrast(disab_immediate, 
                                       study = studlab,
                                       trt = t,
                                       y = T, 
                                       se = s,
                                       sample_size=n )
disab_immediate_net


#plot network structure
plot(disab_immediate_net, weight_edges = TRUE, weight_nodes = TRUE)

#fit RE model
res_disab_immediate <- nma(disab_immediate_net, 
                          seed = 1150676438,
                          trt_effects = "random",
                          prior_trt = normal(scale = 1), #okay?
                          prior_het = half_normal(scale = 0.5), #okay?
                          adapt_delta = 0.99)
print(res_disab_immediate)

#find relative effects
contr_releff_FE <- relative_effects(res_disab_immediate, trt_ref = "pla")

