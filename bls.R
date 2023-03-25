library(jsonlite)
library(devtools)
install_github("mikeasilva/blsAPI")
library(tidyverse)
library(ggplot2)


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

cities = clipr::read_clip_tbl()

ggplot(df,aes(x=date,y=value))+
  geom_line(data=select(df,-city), colour="grey",size=1,alpha=0.5)+
  geom_line(size=1.3,  aes(color=factor(city)))+
  #scale_color_brewer("Set2") +
  facet_wrap(~city,scales = "free_y")+
  labs(color="City",x=NULL,y=NULL,title="Inflation on several major US cities",
       caption="Data from the US Bureau of Labor Statistics")