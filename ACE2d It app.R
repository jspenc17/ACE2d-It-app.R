

#### Load libraries

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyverse)
library(hrbrthemes)
library(plotly)
library(ggthemes)

#### READING IN THE HEAT MAP

heat <- read_excel("~/Desktop/Capstone/R/Playing-with-shiny/WyMortForecast-master/HeatmapData.xlsx")

# Original Heatmap
names(heat)[1]<-"County"
#Turn your 'treatment' column into a character vector
heat$Month <- as.character(heat$Month)
#Then turn it back into a factor with the levels in the correct order
heat$Month <- factor(heat$Month, levels=unique(heat$Month))

heatmap <- ggplot(heat, aes(Month, County, fill= Excess)) +
    geom_tile() +
    scale_fill_gradient(low="white", high="red")

#### READING IN THE SEM DATA

sem.data <- read_excel("~/Desktop/Capstone/R/Playing-with-shiny/WyMortForecast-master/SEM Data .xlsx")

#### NOW START SHINY APP AFTER EVERYTHING (EXCEPT PLOTS) IS READ IN OR DEFINED

ui <- dashboardPage(
    skin = "red",
    dashboardHeader(title = "ACE(2)'d It!"),
    
    dashboardSidebar(
        #sidebarSearchForm(textId="Textsearch", buttonId = "Buttonsearch", label = "Search..."),
        sidebarMenu(
            menuItem("Intro Page", tabName = "a", icon = icon("dashboard")),
            
            menuItem("Project Abstract", tabName = "b", icon = icon("th")),
            
            menuItem("Excess Deaths", tabName = "c", icon = icon("map"),
                     menuSubItem("Heatmap", tabName = "z"),
                     menuSubItem("Line Graph", tabName = "y")),
            
            menuItem("SEM Data", tabName = "d", icon = icon("cog"))
        )
    ),
    #box and and plot need width and height definition
    dashboardBody(
        tabItems(
            
            tabItem(tabName = "a",
                    h2("Welcome!"),
                    fluidPage(
                        box(width = "600px", height = "100%",
                            imageOutput("Concept", width = "550px", height = "100%")
                        )
                    )
            ),#Close tabItem a
            
            tabItem(tabName = "b",
                    h3("Project Abstract"),
                    fluidPage(
                        box(
                            textOutput("abstract"), width = "300px",))
            ), #Close tabItem b
            
            tabItem(tabName = "c"
            ), #Close tabItem c
            #fluidRow(column(width = 12),
            #box(checkboxGroupInput(
            #"County", "Narrow the criteria",
            # c("Albany" = "Albany", 
            #"Big Horn" = "Big Horn", 
            #"Campbell" = "Campbell", 
            #"Cabon" = "Carbon", 
            #"Converse" = "Converse",
            #"Crook" = "Crook",
            #"Fremont" = "Fremont",
            #"Goshen" = "Goshen",
            #"Hot Springs" = "Hot Springs",
            #"Johnson" = "Johnson",
            #"Laramie" = "Laramie",
            #"Lincoln" = "Lincoln",
            #"Natrona" = "Natrona",
            #"Niobrara" = "Niobrara",
            #"Park" = "Park",
            #"Platte" = "Platte",
            #"Sheridan" = "Sheridan",
            #"Sublette" = "Sublette",
            #"Sweetwater" = "Sweetwater",
            #"Teton" = "Teton",
            #"Uinta" = "Uinta",
            #"Washakie" = "Washakie",
            #"Weston" = "Weston"))
            
            
            tabItem(tabName = "z",
                    h2("Heatmap of Excess Deaths"),
                    fluidRow(
                        box(plotOutput("heatmap", width = "600px"), width = "5px", height = "500px")), 
                    fluidRow(
                        box(textOutput("A1")),
                        box(textOutput("H1"))
                    )
            ), #Close tabItem z
            tabItem(tabName = "y",
                    h2("Line Graph of Excess Deaths"),
                    fluidPage(
                        box(imageOutput("Line"))
                    )),
            
            tabItem(tabName = "d",
                    h2("SEM Data"),
                    fluidRow(
                        box(plotOutput("semplot", width = "600px"), width = "5px")),
                    fluidRow(
                        box(textOutput("A2")),
                        box(textOutput("H2"))
                    )
            ) #Close tab item d
        )
    )
)
#)




server <- function(input, output){
    set.seed(122)
    
    #Textsearch <- reactive(input$searchText)
    #Buttonsearch <- reactive(input$searchButton)
    
    output$heatmap <- renderPlot({
        heatmap <- ggplot(heat, aes(Month, County, fill= Excess)) +
            geom_tile() +
            scale_fill_gradient(low="white", high="red")
        
        heatmap  
        
    })
    
    output$Concept <- renderImage({tags$img(src = "https://drive.google.com/file/d/1gQ9bIlEib3ljFWNaEmkmQJdOQsSFCitv/view?usp=sharing")
    }) 
    
    output$A1 <- renderText({"AIM 1
We will statistically estimate the under-representation of COVID-19 data in the State of Wyoming"
        
    })
    output$H1 <- renderText({
        "Hypothesis 1: Utilizing poisson regressions trained with historical data, we will find evidence of certain factors that are correlated with a higher prevalence of under-representation of COVID-19 data."
        
    })
    
    output$A2 <- renderText({"AIM 2
Utilizing Exploratory Factor Analysis and Multivariate Analysis, we aim to statistically quantify the susceptibility of communities that experience mortalities due to COVID-19 infection.
"
        
    })
    
    output$H2 <- renderText({"Hypothesis 2: Utilizing Exploratory Factor Analysis and Multivariate Analysis we will find evidence that the following factors will correlate with varying degrees of prevalence and susceptibility to COVID-19 mortality: socioeconomic status, income distribution, population density, distance from healthcare facilities, and air quality."
        
    })
    
    output$abstract <- renderText({
        "Rural regions of the United States present a unique challenge for the modeling of infectious disease owing to geographic separation of resources resulting in numerous healthcare disparities. These disparities are extremely relevant in light of the COVID-19 pandemic, where accurate collection of real-time epidemiological data is crucial for public health decision making. It is also known that the impacts of the COVID-19 pandemic are not distributed equally and are linked to community factors such as socioeconomic status, wage inequality, insurance coverage, and racial background. All of these concerns are extremely relevant in the state of Wyoming, where nearly 70% of the population lives in a Rural area. Consideration of both potential underreporting of COVID-19 mortalities, as well as the susceptibility of Wyoming communities to the impacts of COVID-19 is paramount for effective decision making, prompting the creation of new statistical tools. 
	This study utilises a hybrid ARIMA-ETS-NNAR model time series forecast and structural equation modelling to provide for a county by county understanding of excess deaths and susceptibility to COVID-19 based on various factors. Utilizing this data we found there to be evidence of underreporting of deaths at the county level, with particularly worrying results in Campbell, Natrona, and Sweetwater counties. Factors found to be significantly significant for the prediction of COVID deaths included higher income inequality and a greater proportion of men in the population. Because of the low population in a number of Wyoming counties, it may be necessary to continue this analysis over a longer period of time to develop better statistical power for other factors.
	The findings of this study suggest a need to more robustly study the impacts of the COVID-19 pandemic on communities in Wyoming, with a specific emphasis on understanding how race and ethnicity may be affecting community susceptibility to COVID-19. Other studies may be indicated to focus on deaths of residents of a given Wyoming county rather than occurrent deaths in the county due to the potential for patients to be transported across county lines for medical care. Additional topics of interest may be how COVID-19 is impacting communities at a community level and if proposed excess death models are aligned with provider perceptions in these counties."
        
    })
    
    
    output$liamplot <- renderPlot({
        liamplot <- liamoveralldeathsshort %>% 
            ggplot( aes(x = Month, y = value, color = County, group = County)) +
            geom_line() +
            ggtitle("Overall Covid Deaths By County") +
            ylab("Deaths") +
            xlab("Month")
        
        liamplot
        
        theme_ipsum()
        
        
    })
    
    output$semplot <- renderPlot({
        sem.plot <- sem.data %>%
            ggplot(aes(x = R2, y = PV, color = Variable,)) +
            geom_point(size = 2) +
            ggtitle("Corrilational Data through SEM") +
            ylab("R2") +
            xlab("P Value") +
            theme(axis.line.x = element_line(colour = "black", size = 1), axis.line.y = element_line( color = "black", size = 1)) +
            ylim(-0.5,1) +
            xlim(-0.5,1) 
        
        semplot
        
    })
}

shinyApp(ui, server)