library(shiny)
library(shinydashboard)
library(ggplot2)
library(RColorBrewer)
library(shinythemes)
library(dashboardthemes)
library(ggtext)
library(tidyverse)
library(fresh)
library(ggrepel)
library(bs4Dash)

element_textbox_highlight <- function(..., hi.labels = NULL, hi.fill = NULL,
                                      hi.col = NULL, hi.box.col = NULL, hi.family = NULL) {
  structure(
    c(element_textbox(...),
      list(hi.labels = hi.labels, hi.fill = hi.fill, hi.col = hi.col, hi.box.col = hi.box.col, hi.family = hi.family)
    ),
    class = c("element_textbox_highlight", "element_textbox", "element_text", "element")
  )
}

element_grob.element_textbox_highlight <- function(element, label = "", ...) {
  if (label %in% element$hi.labels) {
    element$fill <- element$hi.fill %||% element$fill
    element$colour <- element$hi.col %||% element$colour
    element$box.colour <- element$hi.box.col %||% element$box.colour
    element$family <- element$hi.family %||% element$family
  }
  NextMethod()
}

x_mid <- mean(c(max(df2$median_income, na.rm = TRUE), 
                min(df2$median_income, na.rm = TRUE)))

y_mid <- mean(c(max(df2$value, na.rm = TRUE), 
                min(df2$value, na.rm = TRUE)))

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
  dashboardBody(#shinyDashboardThemes(theme = "grey_light"), # usando dashboardthemes
    #use_theme(mytheme), #usando fresh
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
                #box(p("Inflation on several of the main US cities. It's clear that it widely varies."))
                box(plotOutput("plot3"))
                
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
    df2 = readRDS("data2.rds")
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
  
  output$plot3<- renderPlot({
    ggplot(df2,aes(x = median_income, y = value,label=city) ) +
      geom_point(aes(size=df2$difpct*100),color="steelblue") +
      labs(x = "Median Income", y = "CPI (2015=100)", title = "CPI x Median Income and change in CPI from a year ago",size="% change") +
      scale_size(guide = "legend") + 
      geom_text_repel(nudge_x = 0.7,nudge_y = 0.3)+
      geom_vline(xintercept = x_mid) + # plot vertical line
      geom_hline(yintercept = y_mid) +
      #annotate(geom="text",x=110000,y=315,label="Higher income, higher CPI" ,color="grey")+
      #annotate(geom="text",x=55000,y=315,label="Lower income, higher CPI")+
      #annotate(geom="text",x=55000,y=290,label="Lower income, lower CPI")+
      geom_richtext(aes( x=110000,y=315,
                         label = "**Higher income, higher CPI**",
                         size=4), color = "grey", 
                    fill = NA,
                    label.color = NA)+
      geom_richtext(aes( x=55000,y=315,
                         label = "**Lower income, higher CPI**",
                         size=4), color = "grey", 
                    fill = NA,
                    label.color = NA)+
      geom_richtext(aes( x=55000,y=290,
                         label = "**Lower income, lower CPI**",size=4),  
                    fill = NA,color = "grey",
                    label.color = NA)
  })
}



shinyApp(ui, server)

