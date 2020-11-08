library(readxl)
library(dplyr)
library(openxlsx)
library(ggplot2)
library(broom)
library(plotly)

raw <- read_excel("UKCovidData.xlsx", sheet = 5)
pop <- read_excel("UKPopData.xlsx", sheet = 19)

# Population Data Processing

popMale <- as.numeric(pop[8,4:94])
popFemale <- as.numeric(pop[8,96:186])
popTotal <- popMale + popFemale
popProp <- popTotal / 66796807
popPropGrouped <- c(
  sum(popProp[1]),
  sum(popProp[2:5]),
  sum(popProp[6:10]),
  sum(popProp[11:15]),
  sum(popProp[16:20]),
  sum(popProp[21:25]),
  sum(popProp[26:30]),
  sum(popProp[31:35]),
  sum(popProp[36:40]),
  sum(popProp[41:45]),
  sum(popProp[46:50]),
  sum(popProp[51:55]),
  sum(popProp[56:60]),
  sum(popProp[61:65]),
  sum(popProp[66:70]),
  sum(popProp[71:75]),
  sum(popProp[76:80]),
  sum(popProp[81:85]),
  sum(popProp[86:90]),
  sum(popProp[91]))

# Death Data Processing

deathsDF <- data.frame(
  total=t(raw[8,3:44]),
  fiveYearAverage=t(raw[10,3:44]),
  covidExcess=t(raw[8,3:44])-t(raw[10,3:44]),
  covidOfficial=t(raw[18,3:44])
)




dFrame=list()
sumDeaths=list()
dFrame[[1]] <- data.frame(raw[43:62,3:44]) #Male
dFrame[[2]] <- data.frame(raw[65:84,3:44]) #Female
dFrame[[3]] <- data.frame(raw[21:40,3:44]) #Both

for(i in 1:3) {
  rownames(dFrame[[i]])<-pull(raw[21:40,2]) #Naming age groups
  sumDeaths[[i]] <- rowSums(dFrame[[i]]) #sumDeaths[[1(Male),2(Female),3(Both)]]
}

dates<-as.Date(as.numeric(raw[5,3:44]),origin = "1899-12-30")
dF<-dFrame[[1]]

ages_all<-c(0:90)
AgesMed <- c(0.5,3,7.5,12.5,17.5,22.5,27.5,32.5,37.5,42.5,47.5,52.5,57.5,62.5,67.5,72.5,77.5,82.5,87.5,90)
dF$AgesMed <- AgesMed
sD <- data.frame(SumDeaths = sumDeaths[[1]], AgesMed = AgesMed)

names(dF)[1:42] <- paste("Week", as.character(c(1:42)),sep='')

dF <- dF[c(-1,-20),]
sD <- sD[c(-1,-20),]

ageModel<-lm(SumDeaths~poly(AgesMed,3),data = sD)

aA <- data.frame(AgesAll=ages_all)

agePredictions<-predict(ageModel,data.frame(AgesMed=ages_all))

plot(sD$AgesMed,sD$SumDeaths)

lines(dF$AgesMed, fitted(ageModel), col='red', type='b')

## Death Distribution Pre-Covid19

preCovidDeaths <- data.frame(raw[21:40,3:12]) #Data covers 03/01/2020 to 06/03/2020

preCovidMeans <- rowMeans(preCovidDeaths)

preCovidDist <- preCovidMeans/sum(preCovidMeans)

## Death Distribution During Covid19

midCovidDeaths <- data.frame(raw[21:40,17:20])

midCovidMeans <- rowMeans(midCovidDeaths)

midCovidDist <- midCovidMeans/sum(midCovidMeans)

## Quick Comparative Plot

qComp <- data.frame(x=AgesMed,preC=preCovidDist,midC=midCovidDist)

ggplot(qComp) + geom_line(aes(x=x,y=preC,color="blue")) + geom_line(aes(x=x,y=midC))

## Excess Covid Deaths (per Week)

# absExcessDeaths <- midCovidDeaths - preCovidDeaths # - Not sure if needed anymore

excessDeathsMeans <- midCovidMeans - preCovidMeans

excessDeathsPercentage <- (midCovidMeans/preCovidMeans)*100-100 #Percentage Difference in death rate per age group

excessDeathsProp <- excessDeathsMeans / sum(excessDeathsMeans) #Sum of excess death means is greater than average covid confirmed deaths

## Modeling the Death Distribution

covidDeathAgeData <- data.frame(x=AgesMed,excDP=excessDeathsProp,preCD=preCovidDist)

covidDeathAgeModel <- lm(excDP[-c(1:8)]~poly(x[-c(1:8)],5),data=covidDeathAgeData)

sSplineObj<-smooth.spline(covidDeathAgeData$x,covidDeathAgeData$excDP,df=6)

## Model Plot - Spline through data

sSplinePlot<-sSplineObj %>%
  augment() %>%
  ggplot(aes(x=x,y=y)) +
  geom_point(aes(y=y)) +
  geom_line(aes(y=.fitted))
sSplinePlot

## Model Plot - Altered model (for p<1/10000, p=0)

modelPDF <- predict(sSplineObj,data.frame(x=ages_all))

modelPDF <- data.frame(x=modelPDF[[1]]$x,y=modelPDF[[2]]$x)

for(i in 1:dim(modelPDF)[1]) if(modelPDF$y[i]<0) modelPDF$y[i]=0

modelPDF$y<-modelPDF$y/sum(modelPDF$y)

# Weekly Deaths Based on User Input Function

weeklyDeathProj <- function(a1, a2, absDeaths, modelPDF.=modelPDF){
  
  rangePDF <- rep(0,91)
  rangePDF[(a1+1):(a2+1)] <- modelPDF$y[(a1+1):(a2+1)]
  weeklyDeathData <- rep(0,length(absDeaths))
  
  for (i in 1:length(absDeaths)) weeklyDeathData[i] <- sum(absDeaths[i]*rangePDF)
  
  weeklyDeathData
  
}
# Define server logic required to draw a histogram
shinyServer(function(input, output) {


output$text1 <- renderText({
  a1 <- input$ageRange[1]
  a2 <- input$ageRange[2]
  absDeathsType <- input$absType
  c(a1,a2,absDeathsType)
  
}) 

output$plot1 <- renderPlot({
  
# dF <- switch(input$Gender,
#   M = dFrame[[1]],
#   F = dFrame[[2]],
#   B = dFrame[[3]])

plotData <- reactive({
  
  a1 <- input$ageRange[1]
  a2 <- input$ageRange[2]
  
  aTin <- input$absType
  
  if(aTin=="Excess") aT <- deathsDF$covidExcess
  if(aTin=="Official") aT <- deathsDF$covidOfficial
  
  plotDeathData <- data.frame(dates=dates,deaths=round(weeklyDeathProj(a1,a2,aT)))
  
})

# ggplot(data = plotDeathData, aes(x=dates,y=deaths)) + geom_bar()

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

output$plot1<-renderPlotly({
  ggplot(data = plotData())+
    geom_bar(aes(x=dates,y=deaths),stat = "identity", fill="#428bca", colour="#428bca") +
    ylim(-2000, 12000) +
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette)+
    xlab("") +
    ylab("Deaths") +
    theme(
      panel.background = element_rect(fill = "#ffffff",
                                      colour = "#ffffff",
                                      size = 0.25, linetype = "solid"),
      panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                      colour = "#aaaaaa"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "#aaaaaa")
    )
  })


})

})
