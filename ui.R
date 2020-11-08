

library(shiny)


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    
    titlePanel(h1("Covid 19 Deaths By Age Group in England and Wales", align = "center",style="margin:25px;text-decoration:underline;")),


    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            
            sliderInput("ageRange",
                        "Select an age range:",
                        min = 0,
                        max = 90,
                        value = c(0,90)),
            radioButtons('absType','Select Data',choices = c('Excess','Official'),inline=TRUE),
            HTML('<hr style="border-color: #000;">'),
            h3("Documentation"),
            p("Age-range",style="font-weight:bold"),
            p("Control the age range over which the statistical models are applied. Please note that the resulting graph is not based on firm medical data but is a projection based on the data that is available through the ONS."),
            p("Data",style="font-weight:bold"),
            p("Control the data being graphed. The excess option shows the surplus (and deficit) in total deaths compared with the previous five year average. Therefore, a negative value suggests that the death rate for that week was lower than the five year average. The official option shows the amount of people with Covid-19 as the official cause of death."),
            HTML('<hr style="border-color: #000;">')
        ),

        # Show a plot of the generated distribution
        mainPanel(
            HTML('
                 <p>
                 An exact record of how many people per age group died from Covid-19 in England and Wales over the past year has not been made publicly available. 
                 <br><br>
                 What has been made available are two sets of information:
                 <li> The total amount of official Covid deaths per week.</li>
                 <li> The total deaths in the country per week broken down by age group. </li>
                 <br>
                 The following model projects the amount of people who died of covid per age group. This is accomplished first by comparing how the proportions of deaths changed per age group before and during Covid-19. Performing regression analysis on the resulting proportion provides a model of how many people died for a specific age.
                 <br><br>
                 The difference between the excess deaths and official deaths illustrates a rough sense of how many died from Covid-19 unofficialy, i.e. People that were never tested for the virus.
                 </p>
                 
                 '),
            plotlyOutput("plot1")
            
        )
    )
))
