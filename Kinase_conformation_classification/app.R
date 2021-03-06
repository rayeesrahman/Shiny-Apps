#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#option(browser = "false")
colors = c("#443333", "#cc5522", "#00ccff", "#AA96DA", "grey50" )
data = read.table("./full.data.px.vec.csv", sep = "," , header = T )
data$key = data$PDBid
coln = gsub(".y$" , "", gsub(".x$", ".normalized", names(data)))
coln[1] = "PDBid"
colnames(data) = coln 
featlist  = coln[c(2,3,4,5,6,7,8,9,10,11,21,22,23,24,25,26,27,28,29,30)]
library(shinythemes)


library(shiny)
library(ggplot2)
library(plotly)
# Define UI for application that draws a histogram
ui <- fluidPage(
     
     #featlist  = coln[c(2,3,4,5,6,7,8,9,10,11,21,22,23,24,25,26,27,28,29,30)] , 
    # Application title
    titlePanel("3D scatterplot"),
    sidebarLayout(
      sidebarPanel( 
             selectInput(inputId = "featureInput4", 
                         label = "Please Choose data type:", 
                         choices = c("all","training","test"), selected = "all"),
             selectInput(inputId = "featureInput1", 
                         label = "Select first feature", 
                         choices = featlist, selected = featlist[11]),
             selectInput(inputId = "featureInput2", 
                         label = "Select second feature", 
                         choices = featlist , selected = featlist[12]),

             selectInput(inputId = "featureInput3", 
                         label = "Select third feature", 
                         choices = featlist , selected = featlist[13]),
             
             uiOutput("click")
             ),
    mainPanel(                                                        
      plotlyOutput("scaplot", height = "600px", width = "750px",inline = T)))

)

server <- function(input, output) {
  data2 = data.frame()
  observeEvent(input$featureInput4,{
    if ( input$featureInput4 == "all")
    {
      data2 = data 
      row.names(data2) = data2$PDBid
    }
    else 
    {
      data2 = data[which(data$source == input$featureInput4) , ]
    }
    output$scaplot <- renderPlotly({
      key = data2$PDBid 
      plot_ly(data2, 
             x= data2[,input$featureInput1] , 
             y = data2[,input$featureInput2] ,
             z = data2[,input$featureInput3], 
             color = ~V19, key = ~key ,
             colors = colors,  mode = 'markers', text = ~paste('Id:', PDBid, '<br>Source', source)
             ) %>% 
        layout(
          title = paste(input$featureInput1,"vs",input$featureInput2,"vs",input$featureInput3, sep = " "),
          scene = list(
            xaxis = list(title = input$featureInput1),
            yaxis = list(title = input$featureInput2),
            zaxis = list(title = input$featureInput3)
          ))
    })
    output$click <- renderPrint({
      d <- event_data("plotly_click")
      if (is.null(d))
      {"Click points on the plot to get a link to the PDB" }
      else 
      { 
        pd = gsub("_[A-Z]$", "",  d$key)
        lin = paste("https://www.rcsb.org/3d-view/", pd, "/1" , sep = "")
	mod = paste0("<a href='",  lin, "' target='_blank'>View PDB</a>")
	as.character(paste(d$key, mod, sep = ": ") )
        #browseURL(lin)
      }
    })
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

