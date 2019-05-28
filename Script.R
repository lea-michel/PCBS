library(readr)
library(ggplot2)
library(plyr)
library(knitr)
donnees <- read_delim("~/Léa/Cogmaster/Projet AE/Données/donnees.CSV", 
                      ";", escape_double = FALSE, trim_ws = TRUE)
#Structurer les données
donnees[donnees==999]<-NA
#creer dataframe sans na      
donnees<-na.omit(donnees)
donnees$CTQ_total<-donnees$PA_total+donnees$EA_total+donnees$SA_total+donnees$PN_total+donnees$EN_total
donnees$CTQ_severity<-NA
i=1
for(x in donnees[["CTQ_total"]]) {
  if(x<=36){
    donnees$CTQ_severity[i]<-1
  }
  if(x>36 & x<=51){
    donnees$CTQ_severity[i]<-2
  }
  if(x>51 & x<=68){
    donnees$CTQ_severity[i]<-3
  }
  if(x>68){
    donnees$CTQ_severity[i]<-4
  }
  i=i+1
}
donnees$Gender<-factor(donnees$Gender, levels = c(1,2), labels = c("male", "femelle"))
donnees$Population<-factor(donnees$Population, levels = c(1,2,3), labels = c("patient", "sain", "sain"))
donnees$Language<-factor(donnees$Language, levels=c(1,2,3,4,5,7,8), labels = c("anglais","allemand","suedois","coreen","neerlandais","turque","norvegien"))
donnees$PA_severity<-factor(donnees$PA_severity, levels = c(1,2,3,4), labels = c("Absence", "Faible", "Modere", "Severe"))
donnees$EA_severity<-factor(donnees$EA_severity, levels = c(1,2,3,4), labels = c("Absence", "Faible", "Modere", "Severe"))
donnees$SA_severity<-factor(donnees$SA_severity, levels = c(1,2,3,4), labels = c("Absence", "Faible", "Modere", "Severe"))
donnees$PN_severity<-factor(donnees$PN_severity, levels = c(1,2,3,4), labels = c("Absence", "Faible", "Modere", "Severe"))
donnees$EN_severity<-factor(donnees$EN_severity, levels = c(1,2,3,4), labels = c("Absence", "Faible", "Modere", "Severe"))
donnees$CTQ_severity<-factor(donnees$CTQ_severity, levels = c(1,2,3,4), labels = c("Absence", "Faible", "Modere", "Severe"))

##Données sur les participants
#Moyenne de l'age
m_age<-mean(donnees$Age)
hist(donnees$Age)

#Compter le nombre de femmes
genre<-donnees[['Gender']]
genre<-as.numeric(genre)
femelle<-0
male<-0
for(x in genre) {
  if(x==1){
    male<-male+1
  }
  if(x==2){
    femelle<-femelle+1
  }
}
N=male+femelle
genre_des_participants<-c("male","femelle")
effectif<-c(male,femelle)
pourcentage<-c(male/N*100,femelle/N*100)
donnees_genre<-data.frame(genre_des_participants,effectif,pourcentage)
kable(donnees_genre)

#patients/sains
population<-donnees[['Population']]
population<-as.numeric(population)
patient<-0
sain<-0
for(x in population) {
  if(x==1){
    patient<-patient+1
  }
  else{
    sain<-sain+1
  }
}
sante_des_participants<-c("sain","patient")
effectif<-c(sain,patient)
pourcentage<-c(sain/N*100,patient/N*100)
donnees_sante<-data.frame(sante_des_participants,effectif,pourcentage)
kable(donnees_sante)

#origine
langue<-donnees[['Language']]
langue<-as.numeric(langue)
anglais<-0
allemand<-0
suedois<-0
coreen<-0
neerlandais<-0
turque<-0
norvegien<-0
for(x in langue) {
  if(x==1){
    anglais<-anglais+1
  }
  if(x==2){
    allemand<-allemand+1
  }
  if(x==3){
    suedois<-suedois+1
  }
  if(x==4){
    coreen<-coreen+1
  }
  if(x==5){
    neerlandais<-neerlandais+1
  }
  if(x==6){
    turque<-turque+1
  }
  if(x==7){
    norvegien<-norvegien+1
  }
}

langues<-c("anglais","allemand","suedois","coreen","neerlandais","turque","norvegien")
effectif<-c(anglais,allemand,suedois,coreen,neerlandais,turque,norvegien)
pourcentage<-c(anglais/N*100,allemand/N*100,suedois/N*100,coreen/N*100,neerlandais/N*100,turque/N*100,norvegien/N*100)
donnees_langues<-data.frame(langues,effective,pourcentage)
kable(donnees_langues)
#langues_bar<-data.frame(language, number)
barplot(table(donnees$Language), main="Distibution des langues", xlab="Langues") 
#barplot(number,names.arg=language, main="Language Distribution", xlab="Language") 

donnees_participants<-donnees[,c("Age","Gender","Language","Population" )]


#Vérifier les calculs 
donnees_questionnaires<-donnees[,c("ctq1","ctq2","ctq3","ctq4","ctq5","ctq6","ctq7","ctq8","ctq9","ctq10","ctq11","ctq12","ctq13","ctq14","ctq15","ctq16","ctq17","ctq18","ctq19","ctq20","ctq21","ctq22","ctq23","ctq24","ctq25","ctq26","ctq27","ctq28")]

#besoin d'inverser les résultats
#mutate(donnees_questionnaires,ctq2R=ifelse(donnees_questionnaires$ctq2==1,5))
donnees_questionnaires$ctq2R<-NA
i=1
for (x in donnees_questionnaires[["ctq2"]]){
  if (x==1){
    donnees_questionnaires$ctq2R[i]<-5
  }
  if (x==2){
    donnees_questionnaires$ctq2R[i]<-4
  }
  if (x==3){
    donnees_questionnaires$ctq2R[i]<-3
  }
  if (x==4){
    donnees_questionnaires$ctq2R[i]<-2
  }
  if (x==5){
    donnees_questionnaires$ctq2R[i]<-1
  }   
  else {}
  i=i+1
}
donnees_questionnaires$ctq5R<-NA
i=1
for (x in donnees_questionnaires[["ctq5"]]){
  if (x==1){
    donnees_questionnaires$ctq5R[i]<-5
  }
  if (x==2){
    donnees_questionnaires$ctq5R[i]<-4
  }
  if (x==3){
    donnees_questionnaires$ctq5R[i]<-3
  }
  if (x==4){
    donnees_questionnaires$ctq5R[i]<-2
  }
  if (x==5){
    donnees_questionnaires$ctq5R[i]<-1
  }   
  i=i+1
}
donnees_questionnaires$ctq7R<-NA
i=1
for (x in donnees_questionnaires[["ctq7"]]){
  if (x==1){
    donnees_questionnaires$ctq7R[i]<-5
  }
  if (x==2){
    donnees_questionnaires$ctq7R[i]<-4
  }
  if (x==3){
    donnees_questionnaires$ctq7R[i]<-3
  }
  if (x==4){
    donnees_questionnaires$ctq7R[i]<-2
  }
  if (x==5){
    donnees_questionnaires$ctq7R[i]<-1
  }   
  i=i+1
}
donnees_questionnaires$ctq13R<-NA
i=1
for (x in donnees_questionnaires[["ctq13"]]){
  if (x==1){
    donnees_questionnaires$ctq13R[i]<-5
  }
  if (x==2){
    donnees_questionnaires$ctq13R[i]<-4
  }
  if (x==3){
    donnees_questionnaires$ctq13R[i]<-3
  }
  if (x==4){
    donnees_questionnaires$ctq13R[i]<-2
  }
  if (x==5){
    donnees_questionnaires$ctq13R[i]<-1
  }   
  i=i+1
}
donnees_questionnaires$ctq19R<-NA
i=1
for (x in donnees_questionnaires[["ctq19"]]){
  if (x==1){
    donnees_questionnaires$ctq19R[i]<-5
  }
  if (x==2){
    donnees_questionnaires$ctq19R[i]<-4
  }  
  if (x==3){
    donnees_questionnaires$ctq19R[i]<-3
  }
  if (x==4){
    donnees_questionnaires$ctq19R[i]<-2
  }
  if (x==5){
    donnees_questionnaires$ctq19R[i]<-1
  }   
  i=i+1
}
donnees_questionnaires$ctq26R<-0
i=1
for (x in donnees_questionnaires[["ctq26"]]){
  if (x==1){
    donnees_questionnaires$ctq26R[i]<-5
  }
  if (x==2){
    donnees_questionnaires$ctq26R[i]<-4
  }
  if (x==3){
    donnees_questionnaires$ctq26R[i]<-3
  }
  if (x==4){
    donnees_questionnaires$ctq26R[i]<-2
  }
  if (x==5){
    donnees_questionnaires$ctq26R[i]<-1
  }   
  i=i+1
}
donnees_questionnaires$ctq28R<-NA
i=1
for (x in donnees_questionnaires[["ctq28"]]){
  if (x==1){
    donnees_questionnaires$ctq28R[i]<-5
  }
  if (x==2){
    donnees_questionnaires$ctq28R[i]<-4
  }
  if (x==3){
    donnees_questionnaires$ctq28R[i]<-3
  }
  if (x==4){
    donnees_questionnaires$ctq28R[i]<-2
  }
  if (x==5){
    donnees_questionnaires$ctq28R[i]<-1
  }   
  i=i+1
}
donnees_questionnaires$ctq10D<-NA
i=1
for (x in donnees_questionnaires[["ctq10"]]){
  if (x==1){
    donnees_questionnaires$ctq10D[i]<-0
  }
  if (x==2){
    donnees_questionnaires$ctq10D[i]<-0
  }
  if (x==3){
    donnees_questionnaires$ctq10D[i]<-0
  }
  if (x==4){
    donnees_questionnaires$ctq10D[i]<-0
  }
  if (x==5){
    donnees_questionnaires$ctq10D[i]<-1
  }   
  i=i+1
}
donnees_questionnaires$ctq16D<-NA
i=1
for (x in donnees_questionnaires[["ctq16"]]){
  if (x==1){
    donnees_questionnaires$ctq16D[i]<-0
  }
  if (x==2){
    donnees_questionnaires$ctq16D[i]<-0
  }
  if (x==3){
    donnees_questionnaires$ctq16D[i]<-0
  }
  if (x==4){
    donnees_questionnaires$ctq16D[i]<-0
  }
  if (x==5){
    donnees_questionnaires$ctq16D[i]<-1
  }   
  i=i+1
}
donnees_questionnaires$ctq22D<-NA
i=1
for (x in donnees_questionnaires[["ctq22"]]){
  if (x==1){
    donnees_questionnaires$ctq22D[i]<-0
  }
  if (x==2){
    donnees_questionnaires$ctq22D[i]<-0
  }
  if (x==3){
    donnees_questionnaires$ctq22D[i]<-0
  }
  if (x==4){
    donnees_questionnaires$ctq22D[i]<-0
  }
  if (x==5){
    donnees_questionnaires$ctq22D[i]<-1
  }   
  i=i+1
}
donnees_questionnaires$Score_EA<-donnees_questionnaires$ctq3+donnees_questionnaires$ctq8+donnees_questionnaires$ctq14+donnees_questionnaires$ctq18+donnees_questionnaires$ctq25
donnees_questionnaires$Score_PA<-donnees_questionnaires$ctq9+donnees_questionnaires$ctq11+donnees_questionnaires$ctq12+donnees_questionnaires$ctq15+donnees_questionnaires$ctq17
donnees_questionnaires$Score_SA<-donnees_questionnaires$ctq20+donnees_questionnaires$ctq21+donnees_questionnaires$ctq23+donnees_questionnaires$ctq24+donnees_questionnaires$ctq27
donnees_questionnaires$Score_PN<-donnees_questionnaires$ctq1+donnees_questionnaires$ctq2R+donnees_questionnaires$ctq4+donnees_questionnaires$ctq6+donnees_questionnaires$ctq26R
donnees_questionnaires$Score_EN<-donnees_questionnaires$ctq5R+donnees_questionnaires$ctq7R+donnees_questionnaires$ctq13R+donnees_questionnaires$ctq19R+donnees_questionnaires$ctq28R
donnees_questionnaires$Score_MD<-donnees_questionnaires$ctq10D+donnees_questionnaires$ctq16D+donnees_questionnaires$ctq22D
kable(head(c(donnees_questionnaires$Score_PA,donnees_questionnaires$Score_EA,donnees_questionnaires$Score_SA,donnees_questionnaires$Score_PN,donnees_questionnaires$Score_EN,donnees_questionnaires$Score_MD)))

#verification
all(donnees$PA_total==donnees_questionnaires$Score_PA)
all(donnees$EA_total==donnees_questionnaires$Score_EA)
all(donnees$SA_total==donnees_questionnaires$Score_SA)
all(donnees$PN_total==donnees_questionnaires$Score_PN)
all(donnees$EN_total==donnees_questionnaires$Score_EN)
all(donnees$MD_dichot==donnees_questionnaires$Score_MD)

#données générales sur chaque échelle
moy_PA<-mean(donnees$PA_total)
sd_PA<-sd(donnees$PA_total)
hist(donnees$PA_total)

moy_EA<-mean(donnees$EA_total)
sd_EA<-sd(donnees$EA_total)
hist(donnees$EA_total)

moy_SA<-mean(donnees$SA_total)
sd_SA<-sd(donnees$SA_total)
hist(donnees$SA_total)

moy_EN<-mean(donnees$EN_total)
sd_EN<-sd(donnees$EN_total)
hist(donnees$EN_total)

moy_PN<-mean(donnees$PN_total)
sd_PN<-sd(donnees$PN_total)
hist(donnees$PN_total)

moy_CTQ<-mean(donnees$CTQ_total)
sd_CTQ<-sd(donnees$CTQ_total)
hist(donnees$CTQ_total)

moy_MD<-mean(donnees$MD_dichot)
sd_MD<-sd(donnees$MD_dichot)
hist(donnees$MD_dichot)

moyenne_des_echelles<-c("PA_total","EA_total","SA_total","PN_total","EN_total","CTQ_total","MD_total")
effectif<-c(moy_PA,moy_EA,moy_SA,moy_PN,moy_EN,moy_CTQ,moy_MD)
sd<-c(sd_PA,sd_EA,sd_SA,sd_PN,sd_EN,sd_CTQ,sd_MD)
donnees_echelles<-data.frame(moyenne_des_echelles,effectif,sd)
kable(donnees_echelles)

#analyse des résultats en fonction des caractéristiques des participants
#résultats échelles en fonction participants
donnees_patient<-subset(donnees,Population=="patient",select=c(PA_total,EA_total,SA_total,PN_total,EN_total,CTQ_total,MD_dichot))
moy_PA_patient<-mean(donnees_patient$PA_total)
sd_PA_patient<-sd(donnees_patient$PA_total)
moy_EA_patient<-mean(donnees_patient$EA_total)
sd_EA_patient<-sd(donnees_patient$EA_total)
moy_SA_patient<-mean(donnees_patient$SA_total)
sd_SA_patient<-sd(donnees_patient$SA_total)
moy_EN_patient<-mean(donnees_patient$EN_total)
sd_EN_patient<-sd(donnees_patient$EN_total)
moy_PN_patient<-mean(donnees_patient$PN_total)
sd_PN_patient<-sd(donnees_patient$PN_total)
moy_CTQ_patient<-mean(donnees_patient$CTQ_total)
sd_CTQ_patient<-sd(donnees_patient$CTQ_total)
moy_MD_patient<-mean(donnees_patient$MD_dichot)
sd_MD_patient<-sd(donnees_patient$MD_dichot)

donnees_sain<-subset(donnees,Population=="sain",select=c(PA_total,EA_total,SA_total,PN_total,EN_total,CTQ_total,MD_dichot))
moy_PA_sain<-mean(donnees_sain$PA_total)
sd_PA_sain<-sd(donnees_sain$PA_total)
moy_EA_sain<-mean(donnees_sain$EA_total)
sd_EA_sain<-sd(donnees_sain$EA_total)
moy_SA_sain<-mean(donnees_sain$SA_total)
sd_SA_sain<-sd(donnees_sain$SA_total)
moy_EN_sain<-mean(donnees_sain$EN_total)
sd_EN_sain<-sd(donnees_sain$EN_total)
moy_PN_sain<-mean(donnees_sain$PN_total)
sd_PN_sain<-sd(donnees_sain$PN_total)
moy_CTQ_sain<-mean(donnees_sain$CTQ_total)
sd_CTQ_sain<-sd(donnees_sain$CTQ_total)
moy_MD_sain<-mean(donnees_sain$MD_dichot)
sd_MD_sain<-sd(donnees_sain$MD_dichot)

echelles<-c("PA_total","EA_total","SA_total","PN_total","EN_total","CTQ_total","MD_total")
effectif_patient<-c(moy_PA_patient,moy_EA_patient,moy_SA_patient,moy_PN_patient,moy_EN_patient,moy_CTQ_patient,moy_MD_patient)
sd_patient<-c(sd_PA_patient,sd_EA_patient,sd_SA_patient,sd_PN_patient,sd_EN_patient,sd_CTQ_patient,sd_MD_patient)
effectif_sain<-c(moy_PA_sain,moy_EA_sain,moy_SA_sain,moy_PN_sain,moy_EN_sain,moy_CTQ_sain,moy_MD_sain)
sd_sain<-c(sd_PA_sain,sd_EA_sain,sd_SA_sain,sd_PN_sain,sd_EN_sain,sd_CTQ_sain,sd_MD_sain)
donnees_echelles_sain_patient<-data.frame(echelles,effectif_patient,sd_patient,effectif_sain,sd_sain)
kable(donnees_echelles_sain_patient)

#résultats sévérité en fonction participants
donnees_patient_severite<-subset(donnees,Population=="patient",select=c(PA_severity,EA_severity,SA_severity,PN_severity,EN_severity,CTQ_severity))
donnees_sain_severite<-subset(donnees,Population=="sain",select=c(PA_severity,EA_severity,SA_severity,PN_severity,EN_severity,CTQ_severity))
PA_patient_severite<-table(donnees_patient_severite$PA_severity)
PA_patient_severite<-as.data.frame.table(PA_patient_severite)
EA_patient_severite<-table(donnees_patient_severite$EA_severity)
EA_patient_severite<-as.data.frame.table(EA_patient_severite)
SA_patient_severite<-table(donnees_patient_severite$SA_severity)
SA_patient_severite<-as.data.frame.table(SA_patient_severite)
PN_patient_severite<-table(donnees_patient_severite$PN_severity)
PN_patient_severite<-as.data.frame.table(PN_patient_severite)
EN_patient_severite<-table(donnees_patient_severite$EN_severity)
EN_patient_severite<-as.data.frame.table(EN_patient_severite)
CTQ_patient_severite<-table(donnees_patient_severite$CTQ_severity)
CTQ_patient_severite<-as.data.frame.table(CTQ_patient_severite)
PA_sain_severite<-table(donnees_sain_severite$PA_severity)
PA_sain_severite<-as.data.frame.table(PA_sain_severite)
EA_sain_severite<-table(donnees_sain_severite$EA_severity)
EA_sain_severite<-as.data.frame.table(EA_sain_severite)
SA_sain_severite<-table(donnees_sain_severite$SA_severity)
SA_sain_severite<-as.data.frame.table(SA_sain_severite)
PN_sain_severite<-table(donnees_sain_severite$PN_severity)
PN_sain_severite<-as.data.frame.table(PN_sain_severite)
EN_sain_severite<-table(donnees_sain_severite$EN_severity)
EN_sain_severite<-as.data.frame.table(EN_sain_severite)
CTQ_sain_severite<-table(donnees_sain_severite$CTQ_severity)
CTQ_sain_severite<-as.data.frame.table(CTQ_sain_severite)

echantillon<-c("PA-sain","PA-patient","EA-sain","EA-patient","SA-sain","SA-patient","PN-sain","PN-patient","EN-sain","EN-patient","CTQ-sain","CTQ-patient")
absence<-c(PA_sain_severite[1,2],PA_patient_severite[1,2],EA_sain_severite[1,2],EA_patient_severite[1,2],SA_sain_severite[1,2],SA_patient_severite[1,2],PN_sain_severite[1,2],PN_patient_severite[1,2],EN_sain_severite[1,2],EN_patient_severite[1,2],CTQ_sain_severite[1,2],CTQ_patient_severite[1,2])
absence_percentage<-c(round(PA_sain_severite[1,2]/11375*100,1),round(PA_patient_severite[1,2]/4622*100,1),round(EA_sain_severite[1,2]/11375*100,1),round(EA_patient_severite[1,2]/4622*100,1),round(SA_sain_severite[1,2]/11375*100,1),round(SA_patient_severite[1,2]/4622*100,1),round(PN_sain_severite[1,2]/11375*100,1),round(PN_patient_severite[1,2]/4622*100,1),round(EN_sain_severite[1,2]/11375*100,1),round(EN_patient_severite[1,2]/4622*100,1),round(CTQ_sain_severite[1,2]/11375*100,1),round(CTQ_patient_severite[1,2]/4622*100,1))
faible<-c(PA_sain_severite[2,2],PA_patient_severite[2,2],EA_sain_severite[2,2],EA_patient_severite[2,2],SA_sain_severite[2,2],SA_patient_severite[2,2],PN_sain_severite[2,2],PN_patient_severite[2,2],EN_sain_severite[2,2],EN_patient_severite[2,2],CTQ_sain_severite[2,2],CTQ_patient_severite[2,2])
faible_percentage<-c(round(PA_sain_severite[2,2]/11375*100,1),round(PA_patient_severite[2,2]/4622*100,1),round(EA_sain_severite[2,2]/11375*100,1),round(EA_patient_severite[2,2]/4622*100,1),round(SA_sain_severite[2,2]/11375*100,1),round(SA_patient_severite[2,2]/4622*100,1),round(PN_sain_severite[2,2]/11375*100,1),round(PN_patient_severite[2,2]/4622*100,1),round(EN_sain_severite[2,2]/11375*100,1),round(EN_patient_severite[2,2]/4622*100,1),round(CTQ_sain_severite[2,2]/11375*100,1),round(CTQ_patient_severite[2,2]/4622*100,1))
modere<-c(PA_sain_severite[3,2],PA_patient_severite[3,2],EA_sain_severite[3,2],EA_patient_severite[3,2],SA_sain_severite[3,2],SA_patient_severite[3,2],PN_sain_severite[3,2],PN_patient_severite[3,2],EN_sain_severite[3,2],EN_patient_severite[3,2],CTQ_sain_severite[3,2],CTQ_patient_severite[3,2])
modere_percentage<-c(round(PA_sain_severite[3,2]/11375*100,1),round(PA_patient_severite[3,2]/4622*100,1),round(EA_sain_severite[3,2]/11375*100,1),round(EA_patient_severite[3,2]/4622*100,1),round(SA_sain_severite[3,2]/11375*100,1),round(SA_patient_severite[3,2]/4622*100,1),round(PN_sain_severite[3,2]/11375*100,1),round(PN_patient_severite[3,2]/4622*100,1),round(EN_sain_severite[3,2]/11375*100,1),round(EN_patient_severite[3,2]/4622*100,1),round(CTQ_sain_severite[3,2]/11375*100,1),round(CTQ_patient_severite[3,2]/4622*100,1))
severe<-c(PA_sain_severite[4,2],PA_patient_severite[4,2],EA_sain_severite[4,2],EA_patient_severite[4,2],SA_sain_severite[4,2],SA_patient_severite[4,2],PN_sain_severite[4,2],PN_patient_severite[4,2],EN_sain_severite[4,2],EN_patient_severite[4,2],CTQ_sain_severite[4,2],CTQ_patient_severite[4,2])
severe_percentage<-c(round(PA_sain_severite[4,2]/11375*100,1),round(PA_patient_severite[4,2]/4622*100,1),round(EA_sain_severite[4,2]/11375*100,1),round(EA_patient_severite[4,2]/4622*100,1),round(SA_sain_severite[4,2]/11375*100,1),round(SA_patient_severite[4,2]/4622*100,1),round(PN_sain_severite[4,2]/11375*100,1),round(PN_patient_severite[4,2]/4622*100,1),round(EN_sain_severite[4,2]/11375*100,1),round(EN_patient_severite[4,2]/4622*100,1),round(CTQ_sain_severite[4,2]/11375*100,1),round(CTQ_patient_severite[4,2]/4622*100,1))
donnees_severite_sain_patient<-data.frame(echantillon,absence,absence_percentage,faible,faible_percentage,modere,modere_percentage,severe,severe_percentage)
kable(donnees_severite_sain_patient)

#corrélation de spearman: cor.test(x,y, method="spearman")
r_CTQ/sante<-cor.test(donnees$CTQ_total,donnees$Population, method="spearman")

ggplot(data=donnees) +
  geom_boxplot(aes(x=Population,y=EA_total), varwidth = TRUE)+
  facet_wrap(~Language)


#Gender/SA_total
SA_femelle<-subset(donnees,Gender==2,select=SA_total)
moy_SA_femelle<-mean(SA_femelle$SA_total,na.rm = TRUE)
SA_male<-subset(donnees,Gender==1,select=SA_total)
moy_SA_male<-mean(SA_male$SA_total,na.rm = TRUE)
SA_genre<-donnees[,c("Gender","SA_total")]
t.test(data=SA_genre, SA_total ~ Gender)

#Health/EA_total
EA_sante<-donnees[,c("Population","EA_total")]
t.test(data=EA_sante, EA_total ~ Population)

#Health/PA_total
PA_sante<-donnees[,c("Population","PA_total")]
t.test(data=PA_sante, PA_total ~ Population)

#Health/SA_total
SA_sante<-donnees[,c("Population","SA_total")]
t.test(data=SA_sante, SA_total ~ Population)

#Essai anova
par(mfrow=c(1,2)) 
plot(donnees$Population,donnees$PA_total)
plot(donnees$Gender,donnees$PA_total)
par(mfrow=c(1,2))
interaction.plot(donnees$Population,donnees$Gender,donnees$PA_total)
interaction.plot(donnees$Gender,donnees$Population,donnees$PA_total)
PA_aov<-aov(donnees$PA_total ~ donnees$Population*donnees$Gender)
summary(PA_aov)
anova(PA_aov)
