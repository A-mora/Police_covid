# Interventions by rate


library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)
library(stringr)
library(purrr)
library(lubridate)

#  avoid scientific notation 
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
         year = year(date)) %>%
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
                           L = "Argyll and West Dunbartonshire",
                           K = "Renfrewshire & Inverclyde",
                           V = "Dumfries & Galloway"))



cvi_police <- cvi_police %>%
  group_by(div_name, week_year) %>%
  summarise_at(c("dis_infor", "dis_instru", "dis_force",
                 "arrested", "fpn"),
               ~ sum(.x))


# Data for divisions

library(readxl)

divisions_data <- read_excel("data/raw/Police divisions.xlsx", 
                               sheet = "Data Division") %>%
  select(1:3, 5:6)



rate_enfor <- left_join(cvi_police, divisions_data)

rate_enfor <- rate_enfor %>%
  mutate(fpn_rate=fpn*10000/pop16more,
         dis_force_rate = dis_force*10000/pop16more,
         dis_instru_rate = dis_instru*10000/pop16more,
         dis_infor_rate = dis_infor*10000/pop16more,
         arrest_rate = arrested*10000/pop16more)

#-----------------------------------------------
# Plots by rate
#-----------------------------------------------


# long format

rate_enfor_long <- rate_enfor %>%
  select(1:2, 12:16) %>%
  pivot_longer(c(dis_infor_rate, dis_instru_rate, dis_force_rate, fpn_rate, arrest_rate), 
               names_to = "enforcements", values_to = "cases")

# create a count by division, enforcement and week

div_enf_list <- rate_enfor_long %>%
  group_by(div_name, week_year, enforcements) %>%
  separate(week_year, into = c("week", "year"), sep = "_", remove = F) %>%
  mutate(date = make_date(year = year) + weeks(week)) %>% #to get back the date format
  arrange(div_name, enforcements, year, week) %>%
  ungroup() %>%
  group_split(enforcements) 

a<- div_enf_list[[1]]


# Plots


plot_fun_ <- function(x){
  ggplot() +
    geom_line(data = x,
              mapping = aes(x = date, y = cases),
              colour = "steelblue",
              group = 1) +
     scale_x_date(breaks = seq(as.Date("2020-01-03"), as.Date("2021-02-28"), 
                              by= "1 month"),
                 date_labels = "%b %y") +
    scale_y_continuous(limits = c(0.6*min(x$cases), 1.4*max(x$cases)))+
    #theme_classic() +
    theme_bw() +
    facet_wrap(~div_name) +
    theme(axis.title = element_blank(),
          axis.text.x = element_text(angle = 45, size = 6, vjust = 0.7),
          axis.text.y = element_text(size = 8),
          axis.ticks = element_blank(),
          plot.title = element_text(size = 12, face = "bold"))  
}

# Name for titles
names(div_enf_list) <- sort(unique(rate_enfor_long$enforcements))

# Plots and titles
police_plots_ <- list()
for (i in seq(div_enf_list)) {
  gg <- plot_fun_(div_enf_list[[i]]) +
    labs(title = str_to_title(str_replace_all(string = names(div_enf_list[i]), "_", " ")))
  police_plots_[[i]] <- gg
}

p1 <- police_plots_[[1]]
p2 <- police_plots_[[2]]
p3 <- police_plots_[[3]]
p4 <- police_plots_[[4]]
p5 <- police_plots_[[5]]

ggsave(p1, filename ="figures/arrest_rate.png" , units="in", width=10, height=7)
ggsave(p2, filename ="figures/dis_force_rate1.png" , units="in", width=10, height=7)
ggsave(p3, filename ="figures/dis_informed_rate.png" , units="in", width=10, height=7)
ggsave(p4, filename ="figures/dis_instructed_rate.png" , units="in", width=10, height=7)
ggsave(p5, filename ="figures/fpn_rate.png" , units="in", width=10, height=7)


























