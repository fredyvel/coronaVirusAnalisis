library(stringr)
library(ggplot2)

download.file('https://opendata.ecdc.europa.eu/covid19/casedistribution/csv','descarga.csv')
DataCovid <- read.csv("descarga.csv")
DataCovid$dateRep<-as.Date(DataCovid$dateRep,format='%d/%m/%Y')
DataCovid$countriesAndTerritories<-as.character(DataCovid$countriesAndTerritories)
data<-DataCovid
  # countrie<-'Guatemala'
  # data<-DataCovid
  totalCases<-data[0,0]
  countries<-unique(data$countriesAndTerritories)
  
  for(countrie in countries)  {
    
    dataAux<-data[which(data$countriesAndTerritories==countrie),]
    dataAux<-dataAux[order(dataAux$dateRep),]
    dataAux<-cbind(dataAux,0,1:nrow(dataAux),0)
    
    names<-c(colnames(data),'TotalCase','dayPerInit','totalDeath')
    
    colnames(dataAux)<-names
    rownames(dataAux)<-c(1:nrow(dataAux))
    
    for( i in 1:nrow(dataAux)){
      # i=85
      if(i==1) dataAux$totalDeath[i]=dataAux$deaths[i] else dataAux$totalDeath[i]=dataAux$deaths[i]+dataAux$totalDeath[i-1]
    }
    for( i in 1:nrow(dataAux)){
      # i=85
      if(i==1) dataAux$TotalCase[i]=dataAux$cases[i] else dataAux$TotalCase[i]=dataAux$cases[i]+dataAux$TotalCase[i-1]
    }
    totalCases<-rbind(totalCases,dataAux)
    }
  totalCases<-totalCases[-which(totalCases$TotalCase==0),]
  
  
  


graficar<-function(name){
  data<-  resultTotales
  # dataset<-resultTotales
  # cty<-'Italy'
  # name<-unique(as.character(resultTotales$countriesAndTerritories))[1:3]
  
  countriData<-dataset
  for(cty in name){
    dataAux<-dataset[which(dataset$countriesAndTerritories==cty),]
    countriData<-rbind(countriData,dataAux)
    # View(countriData)
  }
  gg1<-ggplot(countriData,aes(x=dayPerInit,y=TotalCase))
  gg1<-gg1+geom_line(aes(color=countriesAndTerritories,linetype=countriesAndTerritories))
  gg1
}

resultTotales<-totalPerCountry(DataCovid)
View(unique(resultTotales$countriesAndTerritories))
graficar(c('El_Salvador','Guatemala','Nicaragua','Costa_Rica','Honduras'))
