library(shiny)
library(shinydashboard)
library(ggplot2)
library(wesanderson)
library(highcharter)
library(plyr)
library(dplyr)
library(formattable)
library(data.table)
library(DT)
library(tidyr)

Basefile = fread("guidedsample.csv")
gap = read.csv("mockgap.csv")

Basefile = Basefile %>% separate(Product1, into = c("Cat1", "Cmp1", "Prod1", "Basepack1"),sep = "_" ) %>%  select (-"Cat1", -"Cmp1")
Basefile = Basefile %>% separate(Product2, into = c("Cat2", "Cmp2", "Prod2", "Basepack2"),sep = "_" ) %>% select (-"Cat2", -"Cmp2")

Basefile = Basefile %>% unite("TopSeller",c("Prod1", "Basepack1"))
Basefile = Basefile %>% unite("TopSeller2",c("Prod2", "Basepack2"))
ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = "Guiding you to the right distribution choice!",
                                    titleWidth = 1400),
                    dashboardSidebar(disable = TRUE),
                    dashboardBody(
                      valueBoxOutput("Metric1", width = 2),
                      valueBoxOutput("Metric2", width = 2),
                      title = "Gap Stores by State",
                      solidHeader = TRUE,
                      highchartOutput(outputId = "Scatterplot", width = "60%"),
                      formattableOutput("Value3"),
                      verbatimTextOutput("x_value"),
                      textOutput("text"),
                      DTOutput("tablecontainer"))
                    
                    #title = "State wise gap stores",
                    #width = '600px',height = 400,
                    #solidHeader = TRUE,
                    #background = "teal",
                    #plotOutput(outputId = "Scatterplot", width = '500px'))
)




server <- function(input, output) {
  output$Scatterplot =renderHighchart({colors = c("#ef6548", "#d7301f", "#990000", "#fdd49e", "#fee8c8", "#fc8d59", "#fdbb84", "#fff7ec")
  
  canvasClickFunction <- JS("function(event) {Shiny.onInputChange('canvasClicked', [this.name, event.point.category]);}")
  
  gap %>% group_by(Area) %>%
    summarise(Stores = sum(Stockers)) %>%
    arrange(Stores) %>%
    hchart('column',hcaes('State','Stores',color = c("#ef6548", "#d7301f", "#990000", "#fdd49e", "#fee8c8", "#fc8d59", "#fdbb84", "#fff7ec"))) %>%
    hc_title(text = "Gap stores") %>%
    hc_subtitle(text = "Gap stores by state") %>%
    #hc_plotOptions(series = list(stacking = FALSE, events = list(click = canvasClickFunction))) %>%
    hc_add_theme(hc_theme_538())
  })
  
  
  
  totalStores = sum(gap$GapStockers)
  totalStores2 = sum(gap$GapStockers2[1:5])
  output$Value1 = renderValueBox({
    valueBox(formatC(Stockers, format="d", big.mark=',')
             ,paste('Top Stores:',totalStores)
             ,icon = icon("stats",lib='glyphicon')
             ,color = "red")
  })
  output$Value2 = renderValueBox({
    valueBox(formatC(Stockers, format = "d", big.mark = ',')
             ,paste('Top 5 states:', totalStores2)
             ,icon = icon("stats", lib = 'glyphicon')
             ,color = "orange")
  })
  
  Custom = Basefile%>% select(TopSeller1, TopSeller2,FbyT, State, Pure_FbyT) %>% head()
  #output$Value3 = renderFormattable(formattable(Custom))
  makeReactiveBinding("outputText")
  
  observeEvent(input$canvasClicked, {
    
    outputText <<- paste0("You have selected the State of ", ifelse(input$canvasClicked[2]==0,"West Bengal",
                                                                    ifelse(input$canvasClicked[2]==1,"Orrisa",
                                                                           ifelse(input$canvasClicked[2]==2,"Maharashta",
                                                                                  ifelse(input$canvasClicked[2]==3,"Uttar Pradesh",
                                                                                         ifelse(input$canvasClicked[2]==4,"Rajashthan",
                                                                                                ifelse(input$canvasClicked[2]==5,"Bihar",
                                                                                                       ifelse(input$canvasClicked[2]==6,"Karnataka",
                                                                                                              ifelse(input$canvasClicked[2]==7,"Madhya Pradesh")))))))))
  })
  
  
  output$text <- renderText({
    outputText
  })
  
  #base = Basefile %>% filter(StateCode == input$canvasClicked[2])
  
  base = reactive ({if(!is.null(input$canvasClicked[2])){
    Basefile %>% filter(StateCode == input$canvasClicked[2]) %>% slice(c(10,15,4,31,20)) %>% rename("F by T"= Total_W_by_T , "ND of Target Stores"= ND_cent_of_Target_stores, "ND of Wastage Stores" = ND_Pure_Wasted) %>%
      select(State, Benchmark1, Benchmark2, 'F by T', 'FD of Wastage Stockers', 'ND of Useful Stockers')
    
    
  }
  })
  
  output$tablecontainer <- renderDT(DT::datatable(base(),options = list(lengthChange = FALSE)))
}



shinyApp(ui = ui, server = server)