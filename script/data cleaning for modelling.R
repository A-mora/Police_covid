
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(stringr)
library(purrr)
library(lubridate)
library(tidyverse)

# Scientific notation off.
options(scipen=99999)


url <- "https://www.scotland.police.uk/spa-media/gtnn42ej/coronavirus-enforcement-information-to-10-february-2021.xlsx"

download.file(url,
              destfile = "data/raw/feb_2021.xlsx",
              mode = "wb")

cvi_police <- read_excel("data/raw/feb_2021.xlsx",
                          sheet = 2,  
                          range = cell_cols("A:I"),
                          col_types = c("date", 
                                        "text", 
                                        "text", 
                                        "text", 
                                        "numeric",
                                        "numeric",
                                        "numeric",
                                        "numeric",
                                        "numeric")) %>%
  rename(date =Date,
         div_code = 'Division Letter',
         subdiv_code = 'SD Letter',
         area = 'Area Commands',
         dis_infor = 'Asked / Informed',
         dis_instru = `Warned / Instructed`,
         dis_force = `Reasonable Force`,
         fpn = FPN,
         arrested = Arrested) %>%
  mutate(week = week(date),
         year = year(date),
         month = month(date)) %>%
  unite(week_year, week, year, sep = "_") %>%
  mutate(div_name = recode(div_code,
                           A= "North East",
                           D = "Tayside", 
                           N = "Highlands & Islands",
                           C = "Forth Valley", 
                           E = "Edinburgh", 
                           J = "The Lothians & Scottish Borders", 
                           P = "Fife",
                           G = "Glasgow",
                           U = "Ayrshire", 
                           Q = "Lanarkshire",
                           L = "Argyll & West Dunbartonshire",
                           K = "Renfrewshire & Inverclyde",
                           V = "Dumfries & Galloway"))



cvi_police_month <- cvi_police %>%
  mutate(year = year(date),
         month = month(date),
         occasion = month - first(month) + 1) %>% #it create 0 and -1 for Jan and Feb
  unite(month_year, month, year, sep = "_", remove = F) %>%
  arrange(year, month) 
  
  

cvi_police_month <- cvi_police_month %>%
  group_by(div_name, month_year, occasion) %>%
  summarise_at(c("dis_infor", "dis_instru", "dis_force",
                 "arrested", "fpn"),
               ~ sum(.x)) 

#change the 0 and -1 values per occasion
cvi_police_month$occasion[cvi_police_month$occasion==0] <-12
cvi_police_month$occasion[cvi_police_month$occasion==-1] <-11

cvi_police_month <- cvi_police_month %>%
  group_by(div_name) %>%
  mutate(id=group_indices())

# saving data 

write_csv(cvi_police_month, "data/clean/cvi_police_month.csv")

