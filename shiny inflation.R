library(shiny)
library(shinydashboard)
library(ggplot2)
library(RColorBrewer)
library(shinythemes)
library(dashboardthemes)
library(ggtext)
library(tidyverse)
library(fresh)

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#434C5E"
  ),
  adminlte_sidebar(
    width = "400px",
    dark_bg = "#D8DEE9",
    dark_hover_bg = "#81A1C1",
    dark_color = "#2E3440"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#D8DEE9", 
    info_box_bg = "#D8DEE9"
  )
)

ui <- dashboardPage(
  
  dashboardHeader(title = "Inflation Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("US Cities", tabName = "dashboard", icon = icon("flag-usa")),
      menuItem("Tab 2", tabName = "widgets", icon = icon("earth-americas"))
    ),
    p("Here you'll find some plots comparing inflation around the world")
  ),
  dashboardBody(#shinyDashboardThemes(theme = "grey_light"),
                use_theme(mytheme),
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              # fluidRow(
                #box(plotOutput("plot1", height = 250)),
                
                # box(
                #   title = "Controls",
                #   sliderInput("slider", "Number of observations:", 1, 100, 50)
                # )
              # ),
              fluidRow(
                box(plotOutput("plot2")),
                box(h5("Inflation on several of the main US cities. It's clear that it widely varies."))
                
                
              )
      ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Under construction")
      )
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  my_data <- reactive({
    df = readRDS("data.rds")
  })
  
  # output$plot1 <- renderPlot({
  #   data <- histdata[seq_len(input$slider)]
  #   hist(data)
  # })
  
  output$plot2 <- renderPlot({
    palette <- brewer.pal(n = 8, name = "Dark2")
    
    # Extend the palette using colorRampPalette
    extended_palette <- colorRampPalette(palette)(9)
    
    ggplot(df,aes(x=date,y=value))+
      geom_line(data=select(df,-city), colour="grey",size=1,alpha=0.5)+
      geom_line(size=1.3,aes(color=city))+
      #scale_color_brewer(palette=extended_palette) +
      scale_color_manual(values = extended_palette)+
      facet_wrap(~city,scales = "free_y")+
      labs(color="City",x=NULL,y=NULL,title="Inflation on several major US cities",
           caption="source: US Bureau of Labor Statistics")+ #theme_bw()+
      theme(
        text=element_text(family="Bangers"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.background = element_blank(),
        legend.position="none",
        strip.text = element_textbox_highlight(
          size = 16, #face = "bold",
          family="Bangers",
          fill = "white", box.color = "white", color = "gray40",
          halign = .5, linetype = 1, r = unit(0, "pt"), width = unit(1, "npc"),
          padding = margin(2, 0, 1, 0), margin = margin(0, 1, 3, 1),
          hi.labels = "US Average", hi.family = "Bangers",
          hi.fill = "firebrick", hi.box.col = "firebrick", hi.col = "white"
        )
      )
  })
}



shinyApp(ui, server)

