countries = openxlsx::read.xlsx("C:\\Users\\rafae\\Downloads\\Consumer_Price_Index_CPI.xlsx",sheet="CPI")

countries_to_select = c()

countries %>% rename(country=X1) %>% 
  filter(str_detect(country,"Brazil|Argentina|Italy|Germany|United States|Spain|Japan|Switzerland|Portugal")) %>% 
  pivot_longer(cols = c(`Jan.2018`:`Feb.2023`)) 
