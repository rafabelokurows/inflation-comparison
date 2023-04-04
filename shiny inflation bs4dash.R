library(shiny)
library(shinydashboard)
library(ggplot2)
library(RColorBrewer)
library(shinythemes)
library(dashboardthemes)
library(ggtext)
library(tidyverse)
#library(fresh)
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




ui <- dashboardPage(
  
  dashboardHeader(title = "Inflation Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("US Cities", tabName = "dashboard", icon = icon("flag-usa")),
      menuItem("Countries", tabName = "widgets", icon = icon("earth-americas"))
    ),
    p("Some plots comparing inflation around the world")
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
              fluidRow(
              box(plotOutput("plot4"))),
              fluidRow(
                plotOutput("plot5"))
      )
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  my_data <- reactive({
    
    dfs=list(df = readRDS("data.rds"),
             df2 = readRDS("data2.rds"),
             dfCountries = readRDS("dfCountries.rds"),
             dfWorld = readRDS("world_data2.rds"))
  })
  
  # output$plot1 <- renderPlot({
  #   data <- histdata[seq_len(input$slider)]
  #   hist(data)
  # })
  
  output$plot2 <- renderPlot({
    palette <- brewer.pal(n = 8, name = "Dark2")
    
    # Extend the palette using colorRampPalette
    extended_palette <- colorRampPalette(palette)(9)
    df=my_data()$df
    df = df %>% mutate(city2=city)
    df %>% 
      ggplot( aes(x=date, y=value)) +
      geom_line( data=df %>% dplyr::select(-city), aes(group=city2), color="grey", size=0.5, alpha=0.5) +
      geom_line( aes(color=city), size=1.2 )+
      scale_color_manual(values=extended_palette) +
      #theme_ipsum() +
      theme(
        #legend.position="none",
        plot.title = element_text(size=14),
        panel.grid = element_blank()
      )+labs(color="City",x=NULL,y=NULL,title="Inflation on a few US cities",
             caption="source: International Monetary Fund")+ theme_bw() +
      facet_wrap(~city)+
      theme(
        text=element_text(family="Bangers"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.background = element_blank(),
        legend.position="none",
        strip.text = element_textbox_highlight(
          size = 16, #face = "bold",
          family="Roboto",
          fill = "white", box.color = "white", color = "gray40",
          halign = .5, linetype = 1, r = unit(0, "pt"), width = unit(1, "npc"),
          padding = margin(2, 0, 1, 0), margin = margin(0, 1, 3, 1),
          hi.labels = "US Average", hi.family = "Bangers",
          hi.fill = "firebrick", hi.box.col = "firebrick", hi.col = "white"
        )
      )
  })
  
  output$plot3<- renderPlot({
    df2 = my_data()$df2
    x_mid <- mean(c(max(df2$median_income, na.rm = TRUE), 
                    min(df2$median_income, na.rm = TRUE)))
    
    y_mid <- mean(c(max(df2$value, na.rm = TRUE), 
                    min(df2$value, na.rm = TRUE)))
    
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
  
  output$plot4<- renderPlot({
    palette <- brewer.pal(n = 8, name = "Dark2")
    extended_palette <- colorRampPalette(palette)(9)
    dfCountries=my_data()$dfCountries
    dfCountries %>% 
      ggplot( aes(x=date, y=value)) +
      geom_line( data=dfCountries %>% dplyr::select(-country), aes(group=country2), color="grey", size=0.5, alpha=0.5) +
      geom_line( aes(color=country), size=1.2 )+
      scale_color_manual(values=extended_palette) +
      #theme_ipsum() +
      theme(
        #legend.position="none",
        plot.title = element_text(size=14),
        panel.grid = element_blank()
      )+labs(color="City",x=NULL,y=NULL,title="Inflation on several countries",
             caption="source: International Monetary Fund")+ theme_bw() +
      facet_wrap(~country)+
      theme(
        text=element_text(family="Bangers"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.background = element_blank(),
        legend.position="none",
        strip.text = element_textbox_highlight(
          size = 16, #face = "bold",
          family="Roboto",
          fill = "white", box.color = "white", color = "gray40",
          halign = .5, linetype = 1, r = unit(0, "pt"), width = unit(1, "npc"),
          padding = margin(2, 0, 1, 0), margin = margin(0, 1, 3, 1),
          hi.labels = "US Average", hi.family = "Bangers",
          hi.fill = "firebrick", hi.box.col = "firebrick", hi.col = "white"
        )
      )
  })
  output$plot5<- renderPlot({
    colors <-rev(c("#7F444B", "#A45151", "#c06b5d","#CC9474","#D6BA85","#79B9A1"))
    world_data2=my_data()$dfWorld
    ggplot() +
      geom_sf(data = world_data2, aes(fill = color,group=color), size=0, alpha=0.9)+
      scale_fill_manual(values = rev(colors),
                        labels= rev(c("Negative","0 to 5%","5 to 10%","10 to 15%","15 to 20%","More than 20%")),
                        na.value = "#9D9595")+
      labs(
        title = "Most recent inflation percentage per country (compared with year before)",
        caption = "Data: IMF | Created by Rafael Belokurows",
        fill= "Inflation"
      ) +
      theme(
        text = element_text(color = "#22211d"), 
        plot.background = element_rect(fill = "#f5f5f2", color = NA), 
        panel.background = element_rect(fill = "#f5f5f2", color = NA), 
        legend.background = element_rect(fill = "#f5f5f2", color = NA),
        
        plot.title = element_text(size= 12, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
        plot.subtitle = element_text(size= 9, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
        plot.caption = element_text( size=7, color = "#4e4d47", margin = margin(b = 0.1, r=-99, unit = "cm") ),
        legend.title = element_text(size = 8, color = "#4e4d47",face = "bold"),
        legend.text  = element_text(size = 7, color = "#4e4d47"),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()#,legend.position = c(1, 0.5)
      )
  })
}



shinyApp(ui, server)
# 
# 
# df=dfs[[1]]
# ggplot(df,aes(x=date,y=value))+
#   geom_line(data=select(df,-city), colour="grey",size=1,alpha=0.5)+
#   geom_line(size=1.3,aes(color=city))+
#   #scale_color_brewer(palette=extended_palette) +
#   scale_color_manual(values = extended_palette)+
#   facet_wrap(~city,scales = "free_y")+
#   labs(color="City",x=NULL,y=NULL,title="Inflation on several major US cities",
#        caption="source: US Bureau of Labor Statistics")+ #theme_bw()+
#   theme(
#     text=element_text(family="Bangers"),
#     axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
#     strip.background = element_blank(),
#     legend.position="none",
#     strip.text = element_textbox_highlight(
#       size = 16, #face = "bold",
#       family="Bangers",
#       fill = "white", box.color = "white", color = "gray40",
#       halign = .5, linetype = 1, r = unit(0, "pt"), width = unit(1, "npc"),
#       padding = margin(2, 0, 1, 0), margin = margin(0, 1, 3, 1),
#       hi.labels = "US Average", hi.family = "Bangers",
#       hi.fill = "firebrick", hi.box.col = "firebrick", hi.col = "white"
#     )
#   )
# 
# df = df %>% mutate(city2=city)
# df %>% 
#   ggplot( aes(x=date, y=value)) +
#   geom_line( data=df %>% dplyr::select(-city), aes(group=city2), color="grey", size=0.5, alpha=0.5) +
#   geom_line( aes(color=city), size=1.2 )+
#   scale_color_manual(values=extended_palette) +
#   #theme_ipsum() +
#   theme(
#     #legend.position="none",
#     plot.title = element_text(size=14),
#     panel.grid = element_blank()
#   )+labs(color="City",x=NULL,y=NULL,title="Inflation on several countries",
#          caption="source: International Monetary Fund")+ theme_bw() +
#   facet_wrap(~city)+
#   theme(
#     text=element_text(family="Bangers"),
#     axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
#     strip.background = element_blank(),
#     legend.position="none",
#     strip.text = element_textbox_highlight(
#       size = 16, #face = "bold",
#       family="Roboto",
#       fill = "white", box.color = "white", color = "gray40",
#       halign = .5, linetype = 1, r = unit(0, "pt"), width = unit(1, "npc"),
#       padding = margin(2, 0, 1, 0), margin = margin(0, 1, 3, 1),
#       hi.labels = "US Average", hi.family = "Bangers",
#       hi.fill = "firebrick", hi.box.col = "firebrick", hi.col = "white"
#     )
#   )
