library(jsonlite)
library(devtools)
install_github("mikeasilva/blsAPI")
library(tidyverse)
library(ggplot2)
library(blsAPI)
library(RColorBrewer)

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


payload <- list('seriesid'=c('CUURS49BSA0L1E','CUURS49DSA0L1E','CUURS37ASA0L1E','CUURS35BSA0L1E',
                             'CUURS35ASA0L1E','CUURS11ASA0L1E','CUURS12ASA0L1E','CUURS23BSA0L1E',
                             'CUUR0000SA0L1E'),
                'startyear'='2013', 'endyear'='2023') 
response <- blsAPI(payload,return_data_frame=T) 

library(zoo)

df = response %>% mutate(yearmon = paste0(year,gsub("M","-",period))) %>% 
  mutate(date=as.Date(paste(yearmon, "-01", sep=""))) %>% 
  mutate(value=as.numeric(value)) %>% 
  #filter(seriesID=="CUURS11ASA0L1E") %>% 
  arrange(seriesID,date) %>% left_join(cities) 

cities = structure(list(seriesID = c("CUURS49BSA0L1E", "CUURS49DSA0L1E", 
                                     "CUURS37ASA0L1E", "CUURS35BSA0L1E", "CUURS35ASA0L1E", "CUURS11ASA0L1E", 
                                     "CUURS12ASA0L1E", "CUURS23BSA0L1E", "CUUR0000SA0L1E"), city = c("San Francisco", 
                                                                                                     "Seattle", "Dallas", "Miami", "Washington", "Boston", "NYC", 
                                                                                                     "Detroit", "US Average"), median_income = c(126187, 105391, 58231, 
                                                                                                                                                 47860, 93547, 81744, 70663, 34762, 69021)), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                                                                                 -9L))

palette_name <- "Dark2"
num_colors <- 12 # the number of colors you want to use

# Get the original palette
palette <- brewer.pal(n = 8, name = palette_name)

df=readRDS("data.rds")

# Extend the palette using colorRampPalette
extended_palette <- colorRampPalette(palette)(9)
library(ggtext)
library(tidyverse)
ggplot(df,aes(x=date,y=value))+
  geom_line(data=select(df,-city), colour="grey",size=1,alpha=0.5)+
  geom_line(size=1.3,aes(color=city))+
  #scale_color_brewer(palette=extended_palette) +
  scale_color_manual(values = extended_palette)+
  facet_wrap(~city,scales = "free_y")+
  labs(color="City",x=NULL,y=NULL,title="Inflation on several major US cities",
       caption="source: US Bureau of Labor Statistics")+ #theme_bw()+
  theme(
    text=element_text(family="Roboto"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    strip.background = element_blank(),
    legend.position="none",
    strip.text = element_textbox_highlight(
      size = 16, #face = "bold",
      family="Roboto",
      fill = "white", box.color = "white", color = "gray40",
      halign = .5, linetype = 1, r = unit(0, "pt"), width = unit(1, "npc"),
      padding = margin(2, 0, 1, 0), margin = margin(0, 1, 3, 1),
      hi.labels = "US Average", hi.family = "Roboto",
      hi.fill = "firebrick", hi.box.col = "firebrick", hi.col = "white"
    )
  )

df2 = df %>% group_by(city) %>% filter(date >= '2021-12-01') %>% slice(c(1, n())) %>% 
  mutate(dif=value-lag(value),difpct=(value-lag(value))/lag(value)) %>% slice_max(date) %>% left_join(cities)
  
library(ggrepel)
x_mid <- mean(c(max(df2$median_income, na.rm = TRUE), 
                min(df2$median_income, na.rm = TRUE)))

y_mid <- mean(c(max(df2$value, na.rm = TRUE), 
                min(df2$value, na.rm = TRUE)))
df2 = df2 %>% 
  mutate(quadrant = case_when(median_income > x_mid & value > y_mid   ~ "Q1",
                              median_income <= x_mid & value > y_mid  ~ "Q2",
                              median_income <= x_mid & value <= y_mid ~ "Q3",
                              TRUE                                         ~ "Q4"))

saveRDS(df2,"data2.rds")
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




  geom_rect(xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0, fill = "blue", alpha = 0.5) +
  geom_rect(xmin = 0, xmax = Inf, ymin = -Inf, ymax = 0, fill = "#BEBADA", alpha = 0.5) +
  geom_rect(xmin = 0, xmax = Inf, ymin = 0, ymax = Inf, fill = "#FDB462", alpha = 0.5) +
  geom_rect(xmin = -Inf, xmax = 0, ymin = 0, ymax = Inf, fill = "#FB8072", alpha = 0.5) +theme_classic()

#
#Sobre as fontes
#https://stackoverflow.com/questions/61204259/how-can-i-resolve-the-no-font-name-issue-when-importing-fonts-into-r-using-ext
#Sobre median income
#https://www.reddit.com/r/datasets/comments/hixfeo/how_to_obtain_median_income_data_for_zip_codes/
#https://bs4dash.rinterface.com/  