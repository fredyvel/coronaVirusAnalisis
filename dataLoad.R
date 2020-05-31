library(stringr)
library(ggplot2)
DataCovid <- read.csv("descarga.csv")
DataCovid$dateRep<-as.Date(DataCovid$dateRep,format='%d/%m/%Y')



totalPerCountry<- function(data){
  
  # countrie<-'Guatemala'
  # data<-DataCovid
  totalCases<-data[0,0]
  countries<-unique(data$countriesAndTerritories)
  for(countrie in countries)  {
    print(countrie)
    dataAux<-data[which(data$countriesAndTerritories==countrie),]
    dataAux<-dataAux[order(dataAux$dateRep),]
    
    names<-c(colnames(dataAux),'TotalCase','dayPerInit')
    dataAux<-cbind(dataAux,0,1:nrow(dataAux))
    colnames(dataAux)<-names
    rownames(dataAux)<-c(1:nrow(dataAux))
    for( i in 1:nrow(dataAux)){
      # i=85
      if(i==1) dataAux$TotalCase[i]=dataAux$cases[i] else dataAux$TotalCase[i]=dataAux$cases[i]+dataAux$TotalCase[i-1]
    }
    totalCases<-rbind(totalCases,dataAux)
  }
  totalCases
}

resultTotales<-totalPerCountry(DataCovid)
View(resultTotales)
guateCase<-resultTotales[which(resultTotales$countriesAndTerritories=='Guatemala'),]
qplot(guateCase$TotalCase)

