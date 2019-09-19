library(tidyverse)
city <- read_csv("data-raw/villes_france.csv",col_names = FALSE)
head(city)
# View(city)
city <- city %>% 
  select(name=X6,long=X20,lat=X21,pop=X15) 
usethis::use_data(city,compress = "xz",overwrite = TRUE)

city
