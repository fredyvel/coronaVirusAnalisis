#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
    #resultTotales<-data.frame(1,1,row.names = 'countriesAndTerritories'),
    # Application title
    titlePanel("Old Faithful Geyser Data"),
    actionButton("downloadData",label="Actualizar Data"),
    #downloadButton("downloadData", "Cargar Datos"),
    # Sidebar with a slider input for number of bins 
    selectInput("Ciudades", "Variable:",
              unique(totalCases$countriesAndTerritories), multiple = TRUE)
    , 
    mainPanel(
        plotOutput('plotConfirmados')
        
    )
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
    output$plotConfirmados <- renderPlot({
        
        # generate bins based on input$bins from ui.R
        dataset<-  totalCases
        # dataset<-resultTotales
        # cty<-'Italy'
        # name<-unique(as.character(resultTotales$countriesAndTerritories))[1:3]
        
        countriData<-dataset[0,0]
        for(cty in input$Ciudades){
            dataAux<-dataset[which(dataset$countriesAndTerritories==cty),]
            countriData<-rbind(countriData,dataAux)
            # View(countriData)
        }
        gg1<-ggplot(countriData,aes(x=dayPerInit,y=TotalCase))
        gg1<-gg1+geom_line(aes(color=countriesAndTerritories,linetype=countriesAndTerritories))
        gg1
        
    })
    
    observeEvent(input$downloadData, {
       #download.file('https://opendata.ecdc.europa.eu/covid19/casedistribution/csv','descarga.csv')
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
       as.character(input$action)
    })
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)
