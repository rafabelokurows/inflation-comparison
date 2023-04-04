library(rnaturalearth)
library(ggplot2)
library(tidyverse)
library(viridis)

# world <- ne_countries(returnclass = "sf")
# ggplot() + 
#   geom_sf(data = world)
# 
# data <- data.frame(name = c("United States", "Canada", "Mexico"),
#                    value = c(10, 20, 30))
# 
# # Merge data with world dataframe
# world_data <- left_join(world, data, by = "name")
# 
# # Plot data on map
# ggplot() + 
#   geom_sf(data = world_data, aes(fill = value)) + 
#   scale_fill_gradient(low = "blue", high = "red")

dfs=list(df = readRDS("data.rds"), df2 = readRDS("data2.rds"), dfCountries = readRDS("dfCountries.rds"))


countries = dfs[[3]] %>% group_by(country) %>% 
  filter(!is.na(value)) %>% 
  slice_max(date)
  
world_data <- left_join(world, countries, by =c("name"="country"))

inflation = openxlsx::read.xlsx("C:\\Users\\rafae\\Downloads\\Consumer_Price_Index_CPI.xlsx",sheet="CPI")
Sys.setlocale("LC_TIME", "C")
dfCountries = inflation %>% rename(country=X1) %>%
  #filter(str_detect(country,"Brazil|Argentina|Italy|Germany|United States|Spain|Japan|Switzerland|Portugal")) %>%
  pivot_longer(cols = c(`Jan.2018`:`Feb.2023`)) %>%
  mutate(date=as.Date(strptime(paste0("01",gsub("[.]","",name)), "%d%b%Y")))

countries = dfCountries %>% group_by(country) %>% 
  filter(!is.na(value)) %>% 
  slice_max(date)%>% #anti_join(world_data,by=c("country"="name")) %>% 
  mutate(country= case_when(str_detect(country,"Aruba")~"Aruba",
                            str_detect(country,"Curaçao")~"Curaçao",
                            str_detect(country,"Türkiye")~"Turkey",
                            str_detect(country,"Kosovo")~"Kosovo",
                            TRUE~country))%>% 
  mutate(code=countrycode::countrycode(country,origin="country.name",destination = "iso3c"))

breaks <- c(-Inf, 2, 6, 10, 15, 20, 50, 100, 264)


world_data <- left_join(world,countries, by =c("iso_a3"="code"))

world_data$bin <- cut(world_data$value, breaks = breaks, include.lowest = TRUE)
colors <-rev(c("#653f3e","#7F444B", "#994857", "#c06b5d","#AD5A5A","#B7635C", "#d8bd8a", "#d8d78f"))
world_data <- world_data %>%
  mutate(color = case_when(
    bin == "[-Inf,2]" ~ colors[1],
    bin == "(2,6]" ~ colors[2],
    bin == "(6,10]" ~ colors[3],
    bin == "(10,15]" ~ colors[4],
    bin == "(15,20]" ~ colors[5],
    bin == "(20,50]" ~ colors[6],
    bin == "(50,100]" ~ colors[7],
    bin == "(100,264]" ~ colors[8]
  ))
c("[-Inf,2]","(2,6]","(6,10]","(10,15]","(15,20]","(20,50]","(50,100]","(100,264]")

breaks <- c(-Inf, 0,5 , 10, 15, 20, 264)
world_data$bin <- cut(world_data$value, breaks = breaks, include.lowest = TRUE)
#ADE25D
#ADC2BE
"#75B7BD"
colors <-rev(c("#7F444B", "#A45151", "#c06b5d","#CC9474","#D6BA85","#79B9A1"))

world_data <- world_data %>%
  mutate(color = case_when(
    bin == "[-Inf,0]" ~ colors[1],
    bin == "(0,5]" ~ colors[2],
    bin == "(5,10]" ~ colors[3],
    bin == "(10,15]" ~ colors[4],
    bin == "(15,20]" ~ colors[5],
    bin == "(20,264]" ~ colors[6]
  ))
c("[-Inf,2]","(2,6]","(6,10]","(10,15]","(15,20]","(20,50]","(50,100]","(100,264]")

world_data %>% as.data.frame() %>% count(bin)


world_data2 = world_data %>% filter(!name.x %in% "Antarctica"&!is.na(geometry)) %>% sf::st_as_sf()
saveRDS(world_data2,"world_data2.rds")
# Plot data on map
ggplot() +
  #geom_polygon(data = world_data, aes(fill = value), size=0, alpha=0.9)+ 
  geom_sf(data = world_data2, aes(fill = color,group=color), size=0, alpha=0.9)+
  scale_fill_manual(values = rev(colors),
                    labels= rev(c("Negative","0 to 5%","5 to 10%","10 to 15%","15 to 20%","More than 20%")),
                    na.value = "#9D9595") + 
  #scale_fill_gradient(low = "blue", high = "red")+
  # scale_fill_gradientn(colors = rev(c("#653f3e","#7F444B", "#994857", "#c06b5d","#AD5A5A","#B7635C", "#d8bd8a", "#d8d78f")),
  #                      breaks = c(-Inf,2,6,10,15, 20,50,100,round(max(world_data$value,na.rm = T))))+
  # scale_fill_viridis(direction=-1, breaks=c(1,5,10,20,50,100),
  #                   name="Inflation %", guide = guide_legend( keyheight = unit(2, units = "mm"), keywidth=unit(7, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) )+
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

#Austrália
#Venezuela


library(tidyverse)

# assign color names based on the range of values in the factor variable


world_data %>% filter(is.na(value))
countries %>% #anti_join(world_data,by=c("country"="name")) %>% 
  mutate(country= case_when(str_detect(country,"Aruba")~"Aruba",
                   str_detect(country,"Curaçao")~"Curaçao",
                   str_detect(country,"Türkiye")~"Turkey",
                   str_detect(country,"Kosovo")~"Kosovo",
                  TRUE~country                            
                              )) %>% 
  mutate(code=countrycode::countrycode(country,origin="country.name",destination = "iso3c")) %>% 
  inner_join(world_data,by=c("code"="brk_a3")) %>% View()
  filter(is.na(code))

countrycode::codelist %>% filter(str_detect(country.name.en,"Kosovo"))
