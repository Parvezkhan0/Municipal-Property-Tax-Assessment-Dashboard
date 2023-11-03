library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(plotly)


options(scipen=8)

#datacleaning
getwd()
setwd("C:/Users/HP/Documents/")
getwd()
#data_clean <- read.csv("property.csv")
data_clean <- read.csv(file.choose(), header = TRUE)
View(data_clean)
subset1<- subset(data_clean, select = c(PID,	CM_ID,	GIS_ID,	ST_NUM,	ST_NAME,	ST_NAME_SUF,	UNIT_NUM,	ZIPCODE,	PTYPE,	LU,	OWN_OCC,	OWNER,	MAIL.CS,MAIL_ZIPCODE,	AV_LAND,	AV_BLDG,	AV_TOTAL,	GROSS_TAX,	LAND_SF,	YR_BUILT,	YR_REMOD, GROSS_AREA,	LIVING_AREA,	NUM_FLOORS))
subset3 <-na_if(subset1,"")

subset4 <- na.omit(subset3)
subset4<-as.data.frame(subset4)
View(subset4)
subset2 <- subset(subset4, YR_REMOD %in% c(2008, 2009,2010,2011,2012,2013,2014,2015,2016,2017,2018))
View(subset2)
prop_clean <- subset2
#write.csv(subset4,"prop.csv")
summary(prop_clean)
subset5<- subset(data_clean, select = c(		AV_LAND,	AV_BLDG,	AV_TOTAL,	GROSS_TAX,	LAND_SF,	YR_BUILT,	YR_REMOD, GROSS_AREA,	LIVING_AREA,	NUM_FLOORS))
subset6 <-na_if(subset5,"")

subset7 <- na.omit(subset6)
subset7<-as.data.frame(subset7)
View(subset4)
subset8 <- subset(subset7, YR_REMOD %in% c(2008, 2009,2010,2011,2012,2013,2014,2015,2016,2017,2018))
View(subset2)
prop_clean1 <- subset8

# compute a correlation matrix
correlation <- round(cor(prop_clean1), 3)
nms <- names(prop_clean1)

#data_clean <- read.csv("C:\Users\HP\Documents\property.csv")

# Define UI for application inspecting Boston property values
ui <- fluidPage(
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      # Select variable for y-axis
      conditionalPanel(condition = "input.ts == 'ls'",
                       selectInput(inputId = "y", 
                  label = "Y-axis:",
                  choices = c("bldg value" = "AV_BLDG", 
                              "GROSS TAX" = "GROSS_TAX"), 
                  selected = "AV_BLDG")),
      
      # Select variable for x-axis
      conditionalPanel(condition = "input.ts == 'ls' && input.y == 'AV_BLDG'" ,
                       selectInput(inputId = "x", 
                  label = "X-axis:",
                  choices = c("bldg value" = "AV_BLDG", 
                              "GROSS TAX" = "GROSS_TAX"), 
                  selected = "GROSS_TAX"),
      
      # Select variable for color
      selectInput(inputId = "z",
                  label = "Color by:",
                  choices = c("Land Use" = "LU",
                              "YEAR REMOD" = "YR_REMOD"),
                  selected = "YR_REMOD") 
    ),
    
    
    # Output
  #  mainPanel(
   #   plotOutput(outputId = "scatterplot", click = "plot_click")
    #)
#  ),
#),
#sidebarLayout(
  # Inputs
 # sidebarPanel(
    # Select variable for y-axis
    conditionalPanel(condition = "input.ts == 'ls'",
                     selectInput(
      inputId = "y1",
      label = "Y-axis:",
      choices = c("GROSS TAX" = "GROSS_TAX"),
      selected = "GROSS_TAX"
    )),
    
    # Select variable for x-axis
    conditionalPanel(condition = "input.ts == 'ls'&& input.y1 == 'GROSS_TAX'",
                     selectInput(
      inputId = "x1",
      label = "X-axis:",
      choices = c("AVG TOTAL" = "AV_TOTAL"),
      
      selected = "AV_TOTAL"
    ),
    
    # Select variable for color
    selectInput(
      inputId = "z1",
      label = "Land Use",
      choices = c("Land Use" = "LU",
                  "Year Remod" = "YR_REMOD"),
      
      selected = "YR_REMOD"
    )
  )),
  #),
  
  
  # Output
#  mainPanel(plotOutput(outputId = "lineChart"))
#),
#mainPanel(
  #plotlyOutput("heat"),
 # plotlyOutput("scatterplot")
#),
#verbatimTextOutput("selection")
#)
mainPanel(
  tabsetPanel(id = 'ts', 
              tabPanel(title = "Correlation between continuos variables", value = 'hs',
                       plotlyOutput("heat"),
                       plotlyOutput("scatterplot"),
                       verbatimTextOutput("selection"),
                       br()),
              
              tabPanel(title = "Tree map for zipcode",  value = 'ls',
                       plotOutput(outputId = "lineChart"),
                       plotOutput(outputId = "scatterplot", click = "plot_click"),
                       
                       br())
              
  )
)
)
)







# Define server function required to create the scatterplot
server <- function(input, output) {
  
  # Create the scatterplot object the plotOutput function is expecting
  output$scatterplot <- renderPlot({
    median_AV_BLDG <- prop_clean %>% group_by(AV_BLDG) %>% summarise(median_AV_BLDG = median(AV_BLDG))
    median_GROSS_TAX <- prop_clean %>% group_by(GROSS_TAX) %>% summarise(median_GROSS_TAX= median(GROSS_TAX))
    ggplot(data = prop_clean, aes_string(x = input$x, y = input$y, color = input$z)) +
      geom_point(alpha = 0.5)
  })
  output$lineChart <- renderPlot({
    ggplot(data = prop_clean, aes_string(
      x = input$x1,
      y = input$y1
    )) +
      geom_line(size = 2) +
      ggtitle("Trends of GrossTax in Boston")
  })
  output$heat <- renderPlotly({
    plot_ly(x = nms, y = nms, z = correlation, 
            key = correlation, type = "heatmap", source = "heatplot") %>%
      layout(xaxis = list(title = ""), 
             yaxis = list(title = ""))
  })
  
  output$selection <- renderPrint({
    s <- event_data("plotly_click")
    if (length(s) == 0) {
      "Click on a cell in the heatmap to display a scatterplot"
    } else {
      cat("You selected: \n\n")
      as.list(s)
    }
  })
  
  output$scatterplot <- renderPlotly({
    s <- event_data("plotly_click", source = "heatplot")
    if (length(s)) {
      vars <- c(s[["x"]], s[["y"]])
      d <- setNames(prop_clean1[vars], c("x", "y"))
      yhat <- fitted(lm(y ~ x, data = d))
      plot_ly(d, x = ~x) %>%
        add_markers(y = ~y) %>%
        add_lines(y = ~yhat) %>%
        layout(xaxis = list(title = s[["x"]]), 
               yaxis = list(title = s[["y"]]), 
               showlegend = FALSE)
    } else {
      plotly_empty()
    }
  })
  
  
}
# Create a Shiny app object
shinyApp(ui = ui, server = server)

#DataSet1 <- read.csv(file.choose())
#head(DataSet1)
#DataSet1 <-subset(DataSet1, YR_BUILT >1890)
#DataSet1 <-subset(DataSet1, GROSS_TAX >0)

#selected<-c("C","I")
#DataSet1 <- subset(DataSet1, DataSet1$LU%in%selected)

#ui <- fluidPage(# Sidebar layout with a input and output definitions
 # sidebarLayout(
    # Inputs
  #  sidebarPanel(
      # Select variable for y-axis
   #   selectInput(
     #   inputId = "y",
    #    label = "Y-axis:",
#        choices = c("GROSS TAX" = "GROSS_TAX"),
 #       selected = "GROSS_TAX"
  #    ),
      
      # Select variable for x-axis
   #   selectInput(
    #    inputId = "x",
     #   label = "X-axis:",
      #  choices = c("Year Built" = "YR_BUILT"),
        
       # selected = "YR_BUILT"
      #),
      
      # Select variable for color
      #selectInput(
       # inputId = "z",
        #label = "Land Use",
        #choices = c("Land Use","C","I"),
        
      #  selected = "C"
      #)
    #),
    
    
    # Output
    #mainPanel(plotOutput(outputId = "lineChart"))
  #))

# Define server function required to create the lineChart
#server <- function(input, output) {
  # Create the scatterplot object the plotOutput function is expecting
 # output$lineChart <- renderPlot({
  #  ggplot(data = DataSet1, aes_string(
   #   x = input$x,
    #  y = input$y
    #)) +
     # geom_line(size = 2) +
      #ggtitle("Trends of GrossTax in Boston")
  #})
  
#}
#abline(lm(AV_LAND~GROSS_TAX))
# Create a Shiny app object
#shinyApp(ui = ui, server = server)



