library(shiny);library(DT);library(shinycustomloader);library(ggplot2);library(data.table);library(jsmodule);library(scales)
source("global.R")
options(shiny.sanitize.errors = F)

ui <- navbarPage("ALTJ",
                 theme = bslib::bs_theme(version = 4),
                 tabPanel("Data", icon = icon("table"),
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("data_show", "Data", c("All", "RT(+)", "RT(-)"), "All", inline = T)
                            ),
                            mainPanel(
                              withLoader(DTOutput("rawdata"), type="html", loader="loader6")
                            )
                          )
                 )
                 ,
                 tabPanel("Correlation",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("wwo_val", "Correlation coefficient", c("With value", "Only colour"), "With value", inline = T)
                            ),
                            mainPanel(
                              withLoader(plotOutput("correlation"), type="html", loader="loader6")
                            )
                          )
                 ),
                 tabPanel("Cut-off",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("wwo_val2", "Cut-off point", c("With value", "Only colour"), "With value", inline = T)
                            ),
                            mainPanel(
                              withLoader(plotOutput("cutoffgraph"), type="html", loader="loader6")
                            )
                          )

                 ),
                 tabPanel("regional RT(±) distribution",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("selectALTJvalue", "ALTJ value", 
                                          choices = colnames(rd)[grep("ALTJ",colnames(rd))],
                                          selected = "ALTJDmax"
                              )
                            ),
                            mainPanel(
                              withLoader(plotOutput("distribution"), type="html", loader="loader6")
                            )
                          )
                 )
)

server <- function(input, output, session) {
  
  rd1 <- reactive({
    switch(input$data_show, 
           "All"= rd,
           "RT(+)" = rd[RT==1],
           "RT(-)" = rd[RT==0])
  })
  
  output$rawdata <- renderDT({
    datatable(rd1(), rownames = F, caption = "Data",
              options = c(opt.data("rawdata"), list(scrollX = T)))
  })
  
  obj.distribution <- reactive({
    ch<-input$selectALTJvalue
    dd <- rd[,.SD,.SDcols=c("regionalRT_yesno",ch)]

    dd<-rd[!is.na(regionalRT_yesno),,]
    
    ggplot(dd, aes(x=dd[[ch]], fill=regionalRT_yesno)) +
      theme_bw() +
      theme(axis.line = element_line(colour = "black"),
            panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank()) +
      geom_density(alpha=0.3) +
      scale_y_continuous(labels=scales::percent_format()) +
      labs(x=paste0(sub("ALTJ","ALTJ ",ch)," [Gy]"), y="Density", fill="Regional RT") +
      scale_fill_discrete(labels=c("No", "Yes"))
  })
  
  output$distribution <- renderPlot({
    obj.distribution()
  })

  obj.correlation <- reactive({
    
    p<-ggplot(cd, aes(x=x, y=y, fill= z, text=z))+ 
      theme_bw()+
      theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),panel.background = element_blank(),
            axis.title.x = element_blank(),axis.title.y = element_blank())+
      geom_tile()+
      scale_fill_gradient(low = "deepskyblue1", high = "red2", na.value="white")
    
    if(input$wwo_val=="With value"){
      p<-p+geom_text(aes(label=z))
    }
    
    p
    
  })
  
  output$correlation <- renderPlot({
    obj.correlation()
  })
  
  obj.cutoffgraph <- reactive({
    
    p<- ggplot(co, aes(x=Variable, y=Cutoffpoint, fill=as.factor(Variable) )) + 
      geom_bar(stat = "identity") +
      scale_fill_hue(c = 40) +
      theme_bw() +
      theme(axis.line = element_line(colour = "black"),
            panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank(),
            legend.position="none") +
      ylim(0, 32)+
      labs(x="ALTJ variables", y="Cut-off point [Gy]")
    
    if(input$wwo_val2=="With value"){
      p<-p+geom_text(aes(label=Cutoffpoint), vjust=-0.25)
    }
    
    p
  })
  
  output$cutoffgraph <- renderPlot({
    obj.cutoffgraph()
  })
}

shinyApp(ui, server)