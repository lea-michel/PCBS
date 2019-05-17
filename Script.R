library(readr)
donnees <- read_delim("~/Léa/Cogmaster/Projet AE/Données/donnees.CSV", 
                      ";", escape_double = FALSE, trim_ws = TRUE)
#Structurer les données
donnees[donnees==999]<-NA
#creer dataframe sans na      donnees_no_na<-na.omit(donnees)
donnees$Gender<-factor(donnees$Gender, levels = c(1,2), labels = c("male", "female"))
donnees$Population<-factor(donnees$Population, levels = c(1,2,3), labels = c("patient", "healthy", "healthy"))
donnees$Language<-factor(donnees$Language, levels=c(1,2,3,4,5,7,8), labels = c("english","german","swedish","korean","dutch","turkish","norwegian"))

##Données sur les participants
#Moyenne de l'age
m_age<-mean(donnees$Age,na.rm = TRUE)
hist(Age)
#Compter le nombre de femmes
gender<-donnees[['Gender']]
gender<-as.numeric(na.omit(gender))
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
print(female)
print(male)
#patients/sains
population<-donnees[['Population']]
population<-as.numeric(na.omit(population))
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
print(patient)
print(healthy)
#origine
origin<-donnees[['Language']]
origin<-as.numeric(na.omit(origin))
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
  if(x==7){
    turkish<-turkish+1
  }
  if(x==8){
    norwegian<-norwegian+1
  }
}
print(english)
print(german)
print(swedish)
print(korean)
print(dutch)
print(turkish)
print(norwegian)

#Vérifier les calculs ?
donnees_questionnaires<-donnees[,c("ctq1","ctq2","ctq3","ctq4","ctq5","ctq6","ctq7","ctq8","ctq9","ctq10","ctq11","ctq12","ctq13","ctq14","ctq15","ctq16","ctq17","ctq18","ctq19","ctq20","ctq21","ctq22","ctq23","ctq24","ctq25","ctq26","ctq27","ctq28")]
#donnees_questionnaires<-na.omit(donnees_questionnaires)

#besoin d'inverser les résultats
#mutate?
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
donnees_questionnaires$ctq26R<-NA
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

#analyse des résultats en fonction des caractéristiques des participants
ggplot(data=donnees, aes(x=Population,y=SA_total)) +
  geom_boxplot()

#essais
boxplot(EA_total ~ Gender)
boxplot(SA_total ~ Gender)
SA_female<-subset(donnees,Gender==2,select=SA_total)
m_SAf<-mean(SA_female$SA_total,na.rm = TRUE)
SA_male<-subset(donnees,Gender==1,select=SA_total)
m_SAm<-mean(SA_male$SA_total,na.rm = TRUE)
SA_fm<-donnees[,c("Gender","SA_total")]
t.test(data=SA_fm, SA_total ~ Gender)


