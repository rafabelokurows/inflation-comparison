countries = openxlsx::read.xlsx("C:\\Users\\belokurowsr\\Downloads\\Consumer_Price_Index_CPI.xlsx",sheet="CPI")
library(tidyverse)
library(lubridate)
library(ggtext)
Sys.setlocale("LC_TIME", "C")
dfCountries = countries %>% rename(country=X1) %>%
  filter(str_detect(country,"Brazil|Argentina|Italy|Germany|United States|Spain|Japan|Switzerland|Portugal")) %>%
  pivot_longer(cols = c(`Jan.2018`:`Feb.2023`)) %>%
  mutate(date=as.Date(strptime(paste0("01",gsub("[.]","",name)), "%d%b%Y")))

palette_name <- "Dark2"
palette <- RColorBrewer::brewer.pal(n = 8, name = palette_name)

# Extend the palette using colorRampPalette
extended_palette <- colorRampPalette(palette)(9)

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

ggplot(dfCountries,aes(x=date,y=value))+
  geom_line(data=select(dfCountries,-country), colour="grey",size=1,alpha=0.5)+
  geom_line(size=1.3,aes(color=country))+
  #scale_color_brewer(palette=extended_palette) +
  scale_color_manual(values = extended_palette)+
  facet_wrap(~country,scales = "free_y")+
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

d <- purrr::map_dfr(
  letters,
  ~ data.frame(
    idx = 1:400,
    value = cumsum(runif(400, -1, 1)),
    type = .,
    flag = sample(c(TRUE, FALSE), size = 400, replace = TRUE),
    stringsAsFactors = FALSE
  )
)
install.packages("gghighlight")
library(gghighlight)
ggplot(d) +
  geom_line(aes(idx, value, colour = type))
ggplot(d) +
  geom_line(aes(idx, value, colour = type)) +
  gghighlight(max(value) > 19) +
  theme_minimal() +
  facet_wrap(~ type)



ggplot(dfCountries)+
  #geom_line(data=select(dfCountries,-country), colour="grey",size=1,alpha=0.5)+
  geom_line(size=1.3,aes(x=date,y=value,color=country))+
  #scale_color_brewer(palette=extended_palette) +
  scale_color_manual(values = extended_palette)+
  facet_wrap(~country,scales = "free_y")+
  labs(color="City",x=NULL,y=NULL,title="Inflation on several major US cities",
       caption="source: US Bureau of Labor Statistics")+
  gghighlight(max(value) > 19)

dfCountries = dfCountries %>% 
  mutate(country2=country)

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



