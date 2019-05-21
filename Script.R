library(readr)
library(ggplot2)
library(plyr)
library(knitr)
donnees <- read_delim("~/L�a/Cogmaster/Projet AE/Donn�es/donnees.CSV", 
                      ";", escape_double = FALSE, trim_ws = TRUE)
#Structurer les donn�es
donnees[donnees==999]<-NA
#creer dataframe sans na      
donnees<-na.omit(donnees)
donnees$Gender<-factor(donnees$Gender, levels = c(1,2), labels = c("male", "female"))
donnees$Population<-factor(donnees$Population, levels = c(1,2,3), labels = c("patient", "healthy", "healthy"))
donnees$Language<-factor(donnees$Language, levels=c(1,2,3,4,5,7,8), labels = c("english","german","swedish","korean","dutch","turkish","norwegian"))

##Donn�es sur les participants
#Moyenne de l'age
m_age<-mean(donnees$Age)
hist(donnees$Age)
#Compter le nombre de femmes
gender<-donnees[['Gender']]
gender<-as.numeric(gender)
female<-0
male<-0
for(x in gender) {
  if(x==1){
    male<-male+1
  }
  if(x==2){
    female<-female+1
  }
}
N=male+female
gender_of_participants<-c("male","female")
number<-c(male,female)
percentage<-c(male/N*100,female/N*100)
donnees_gender<-data.frame(gender_of_participants, number, percentage)
kable(donnees_gender)

#patients/sains
population<-donnees[['Population']]
population<-as.numeric(population)
patient<-0
healthy<-0
for(x in population) {
  if(x==1){
    patient<-patient+1
  }
  else{
    healthy<-healthy+1
  }
}
health_of_participants<-c("healthy","patient")
number<-c(healthy,patient)
percentage<-c(healthy/N*100,patient/N*100)
donnees_health<-data.frame(health_of_participants, number, percentage)
kable(donnees_health)

#origine
origin<-donnees[['Language']]
origin<-as.numeric(origin)
english<-0
german<-0
swedish<-0
korean<-0
dutch<-0
turkish<-0
norwegian<-0
for(x in origin) {
  if(x==1){
    english<-english+1
  }
  if(x==2){
    german<-german+1
  }
  if(x==3){
    swedish<-swedish+1
  }
  if(x==4){
    korean<-korean+1
  }
  if(x==5){
    dutch<-dutch+1
  }
  if(x==6){
    turkish<-turkish+1
  }
  if(x==7){
    norwegian<-norwegian+1
  }
}

language<-c("english","german","swedish","korean","dutch","turkish","norwegian")
number<-c(english,german,swedish,korean,dutch,turkish,norwegian)
percentage<-c(english/N*100,german/N*100,swedish/N*100,korean/N*100,dutch/N*100,turkish/N*100,norwegian/N*100)
donnees_language<-data.frame(language, number, percentage)
kable(donnees_language)
Language_bar<-data.frame(language, number)
barplot(table(donnees$Language), main="Language Distribution", xlab="Language") 
#barplot(number,names.arg=language, main="Language Distribution", xlab="Language") 

donnees_participants<-donnees[,c("Age","Gender","Language","Population" )]


#V�rifier les calculs 
donnees_questionnaires<-donnees[,c("ctq1","ctq2","ctq3","ctq4","ctq5","ctq6","ctq7","ctq8","ctq9","ctq10","ctq11","ctq12","ctq13","ctq14","ctq15","ctq16","ctq17","ctq18","ctq19","ctq20","ctq21","ctq22","ctq23","ctq24","ctq25","ctq26","ctq27","ctq28")]

#besoin d'inverser les r�sultats
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
donnees_questionnaires$Score_emotional_abuse<-donnees_questionnaires$ctq3+donnees_questionnaires$ctq8+donnees_questionnaires$ctq14+donnees_questionnaires$ctq18+donnees_questionnaires$ctq25
donnees_questionnaires$Score_physical_abuse<-donnees_questionnaires$ctq9+donnees_questionnaires$ctq11+donnees_questionnaires$ctq12+donnees_questionnaires$ctq15+donnees_questionnaires$ctq17
donnees_questionnaires$Score_sexual_abuse<-donnees_questionnaires$ctq20+donnees_questionnaires$ctq21+donnees_questionnaires$ctq23+donnees_questionnaires$ctq24+donnees_questionnaires$ctq27
donnees_questionnaires$Score_physical_neglect<-donnees_questionnaires$ctq1+donnees_questionnaires$ctq2R+donnees_questionnaires$ctq4+donnees_questionnaires$ctq6+donnees_questionnaires$ctq26R
donnees_questionnaires$Score_emotional_neglect<-donnees_questionnaires$ctq5R+donnees_questionnaires$ctq7R+donnees_questionnaires$ctq13R+donnees_questionnaires$ctq19R+donnees_questionnaires$ctq28R
donnees_questionnaires$Score_minimization<-donnees_questionnaires$ctq10D+donnees_questionnaires$ctq16D+donnees_questionnaires$ctq22D

#analyse des r�sultats en fonction des caract�ristiques des participants
ggplot(data=donnees) +
  geom_boxplot(aes(x=Population,y=EA_total), varwidth = TRUE)+
  facet_wrap(~Language)


#Gender/SA_total
SA_female<-subset(donnees,Gender==2,select=SA_total)
mean_SA_female<-mean(SA_female$SA_total,na.rm = TRUE)
SA_male<-subset(donnees,Gender==1,select=SA_total)
mean_SAm<-mean(SA_male$SA_total,na.rm = TRUE)
SA_gender<-donnees[,c("Gender","SA_total")]
t.test(data=SA_gender, SA_total ~ Gender)

#Health/EA_total
EA_health<-donnees[,c("Population","EA_total")]
t.test(data=EA_health, EA_total ~ Population)

#Health/PA_total
PA_health<-donnees[,c("Population","PA_total")]
t.test(data=PA_health, PA_total ~ Population)

#Health/SA_total
SA_health<-donnees[,c("Population","SA_total")]
t.test(data=SA_health, SA_total ~ Population)