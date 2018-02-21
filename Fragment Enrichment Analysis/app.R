## app.R ##
library(shiny)
library(plotly)
library(shinythemes)
library(dplyr)
#library(ChemmineR)
library(rcdk)

server <- function(input, output){

  # Read data
  enrich = read.table("./tc.9.all.fragments.enriched.csv",
                      sep =",",
                      header = T)
  enrich$lp = as.numeric(as.character(enrich$lp))
  enrich.f = enrich[enrich$pval < .05,]
  enrich.f$lp2 = enrich.f$lp + runif(n = nrow(enrich.f), min = -.01 , max = .01)
  enrich.f$lp2 = round(enrich.f$lp2, digits = 4)
  head(enrich.f$lp2)
  thres1 = .05
  thres2 = .05/10535

   # Set some colors
  plotcolor <- "#F5F1DA"
  papercolor <- "#E3DFC8"

  colors = c("#7C2600", "#CE5A28" ,"#00D2F1" ,"#AA96DA" ,"#7F7F7F")
  # Plot time series chart
  output$timeseries <- renderPlotly({
    p = ggplot(enrich.f,  aes(x = factor(conf), y = lp2, color = factor(conf), text = frag))
    p = p +
      geom_jitter(width = 0.2,  height = 0, size = 3) +
      theme_bw() +
      scale_color_manual(values = colors) +
      scale_y_continuous("-log10(p)")+
      scale_x_discrete("kinase conformation") +
      geom_hline(yintercept = -log10(.05), color = "red", size = 1.5)  +
      geom_hline(yintercept = -log10(thres2), color = "purple", size = 1.5)  +
      theme( panel.grid.major.x = element_blank() , panel.grid.major.y = element_line(color = "grey", size = 1.25),
             panel.grid.minor = element_blank(),
             axis.text = element_text(size = 10), axis.title.y = element_text(size = 30),
             legend.position = "none") + scale_fill_discrete(guide=FALSE)
    ggplotly(p, source = "source" )

  })

  # Coupled hover event
  output$correlation <- renderPlot({

    # Read in hover data
    eventdata <- event_data("plotly_hover", source = "source")
    validate(need(!is.null(eventdata), "Hover over the points to vizualize the enriched fragments"))
    lp = eventdata$y
    rw = enrich.f[enrich.f$lp2 == lp,]
    smi = as.character(rw$frag)
    mol = parse.smiles(smi)
    par(mar=c(0,0,0,0))
    d = get.depictor(height = 1000, width = 1000, zoom = 6, style = "cow")
    i = view.image.2d(mol[[1]], depictor = d)
    plot(NA,NA,xlim=c(0,1000),ylim=c(0,1000),xaxt='n',yaxt='n',xlab='',ylab='')
    rasterImage(i,1,1,1000,1000)
  })

}

ui <- fluidPage(
  # Set theme
  theme = shinytheme("spacelab"),
  h2("Enrichment of Fragments per Kinase Conformation"),
  h3("Here we vizualize statisically enriched fragments per kinase conformation"),
  # Some help text
  #h2("Coupled hover-events in plotly charts using Shiny"),
  #h4("This Shiny app showcases coupled hover-events using Plotly's ", tags$code("event_data()"), " function."),

  # Vertical space
  tags$hr(),


  # Plotly Chart Area
  fluidRow(
    column(6, plotlyOutput(outputId = "timeseries", height = "600px", width = "700px")),
    column(6, plotOutput(outputId = "correlation", height = "400px", width = "400px"))),
  tags$hr(),
  tags$blockquote("Hover over the points to vizualize the enriched fragments")
)

shinyApp(ui = ui, server = server)
