# Load packages.

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
                           L = "Argyll & West Dunbartonshire",
                           K = "Renfrewshire & Inverclyde",
                           V = "Dumfries & Galloway"))
  

cvi_police <- cvi_police %>%
  group_by(div_name, week_year) %>%
  summarise_at(c("dis_infor", "dis_instru", "dis_force",
                     "arrested", "fpn"),
                   ~ sum(.x))

# long format

cvi_police_long <- cvi_police %>%
  select(1:7) %>%
  pivot_longer(c(dis_infor, dis_instru, dis_force, fpn, arrested), 
               names_to = "enforcements", values_to = "cases")

# create a count by division, enforcement and week

div_enf_list <- cvi_police_long %>%
  group_by(div_name, week_year, enforcements) %>%
  separate(week_year, into = c("week", "year"), sep = "_", remove = F) %>%
  mutate(date = make_date(year = year) + weeks(week)) %>% #to get back the date format
  arrange(div_name, enforcements, year, week) %>%
  ungroup() %>%
  group_split(div_name) 


  
#------------------
#### Dispersed when informed

# Plot function: Dispersed when informed
plot_fun_inf <- function(x){
  ggplot() +
    geom_line(data = filter(x, enforcements== 'dis_infor'),
              mapping = aes(x = date, y = cases),
              colour = "steelblue",
              group = 1) +
    geom_point(stat = "identity") +
    scale_x_date(breaks = seq(as.Date("2020-01-03"), as.Date("2021-02-28"), 
                              by= "1 month"),
                 date_labels = "%b %y") +
    scale_y_continuous(limits = c(0.6*min(x$cases), 1.4*max(x$cases)))+
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.text.x = element_text(angle = 45, size = 6, vjust = 0.7),
          axis.text.y = element_text(size = 8),
          axis.ticks = element_blank(),
          plot.title = element_text(size = 10))  
}
  
# Name for titles
names(div_enf_list) <- sort(unique(cvi_police_long$div_name))

# Plots and titles
police_plots_inf <- list()
for (i in seq(div_enf_list)) {
  gg <- plot_fun_inf(div_enf_list[[i]]) +
    labs(title = names(div_enf_list[i]))
  police_plots_inf[[i]] <- gg
}


# Plot
p1 <- plot_grid(plotlist = police_plots_inf, ncol = 4)
ggsave(p1, filename = "disp_informed.png", height = 8, width = 10)

#-------------
### Arrested

# Plot function: arrested

plot_fun_arrest <- function(x){
  ggplot() +
    geom_line(data = filter(x, enforcements== 'arrested'),
              mapping = aes(x = date, y = cases),
              colour = "steelblue",
              group = 1) +
    geom_point(stat = "identity") +
    scale_x_date(breaks = seq(as.Date("2020-01-03"), as.Date("2021-02-28"), 
                              by= "1 month"),
                 date_labels = "%b %y") +
    #scale_y_continuous(limits = c(0.6*min(x$counts), 1.4*max(x$counts)))+
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.text.x = element_text(angle = 45, size = 6, vjust = 0.7),
          axis.text.y = element_text(size = 8),
          axis.ticks = element_blank(),
          plot.title = element_text(size = 10))  
}

# Name for titles.
names(div_enf_list) <- sort(unique(cvi_police_long$div_name))

# Plots and titles
police_plots_arre <- list()
for (i in seq(div_enf_list)) {
  gg <- plot_fun_arrest(div_enf_list[[i]]) +
    labs(title = names(div_enf_list[i]))
  police_plots_arre[[i]] <- gg
}


# Plot
p2 <- plot_grid(plotlist = police_plots_arre, ncol = 3)
ggsave(p2, filename = "arrested.png", height = 10, width = 8)


#-------------
# Plot function: fpn

plot_fun_fpn <- function(x){
  ggplot() +
    geom_line(data = filter(x, enforcements== 'fpn'),
              mapping = aes(x = date, y = cases),
              colour = "steelblue",
              group = 1) +
    geom_point(stat = "identity") +
    scale_x_date(breaks = seq(as.Date("2020-01-03"), as.Date("2021-02-28"), 
                              by= "1 month"),
                 date_labels = "%b %y") +
    #scale_y_continuous(limits = c(0.6*min(x$counts), 1.4*max(x$counts)))+
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.text.x = element_text(angle = 45, size = 6, vjust = 0.7),
          axis.text.y = element_text(size = 8),
          axis.ticks = element_blank(),
          plot.title = element_text(size = 10))  
}

# Name for titles.
names(div_enf_list) <- sort(unique(cvi_police_long$div_name))

# Plots and titles
police_plots_fpn <- list()
for (i in seq(div_enf_list)) {
  gg <- plot_fun_fpn(div_enf_list[[i]]) +
    labs(title = names(div_enf_list[i]))
  police_plots_fpn[[i]] <- gg
}


# Plot
p3 <- plot_grid(plotlist = police_plots_fpn, ncol = 3)
ggsave(p3, filename = "fpn.png", height = 10, width = 8)


#-------------
# Plot function: dispersed by force

plot_fun_force <- function(x){
  ggplot() +
    geom_line(data = filter(x, enforcements== 'dis_force'),
              mapping = aes(x = date, y = cases),
              colour = "steelblue",
              group = 1) +
    geom_point(stat = "identity") +
    scale_x_date(breaks = seq(as.Date("2020-01-03"), as.Date("2021-02-28"), 
                              by= "1 month"),
                 date_labels = "%b %y") +
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.text.x = element_text(angle = 45, size = 6, vjust = 0.7),
          axis.text.y = element_text(size = 8),
          axis.ticks = element_blank(),
          plot.title = element_text(size = 10))  
}

# Name for plot titles.
names(div_enf_list) <- sort(unique(cvi_police_long$div_name))


# plots and add titles.
police_plots_force <- list()
for (i in seq(div_enf_list)) {
  gg <- plot_fun_force(div_enf_list[[i]]) +
    labs(title = names(div_enf_list[i]))
  police_plots_force[[i]] <- gg
}


# Plot
p4 <- plot_grid(plotlist = police_plots_force, ncol = 3)
ggsave(p4, filename = "force.png", height = 10, width = 8)


#-------------
# Plot function: dispersed when instructed

plot_fun_instru <- function(x){
  ggplot() +
    geom_line(data = filter(x, enforcements== 'dis_instru'),
              mapping = aes(x = date, y = cases),
              colour = "steelblue",
              group = 1) +
    geom_point(stat = "identity") +
    scale_x_date(breaks = seq(as.Date("2020-01-03"), as.Date("2021-02-28"), 
                              by= "1 month"),
                 date_labels = "%b %y") +
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.text.x = element_text(angle = 45, size = 6, vjust = 0.7),
          axis.text.y = element_text(size = 8),
          axis.ticks = element_blank(),
          plot.title = element_text(size = 10))  
}

# Name for plot titles
names(div_enf_list) <- sort(unique(cvi_police_long$div_name))


# Plots and add titles
police_plots_instru <- list()
for (i in seq(div_enf_list)) {
  gg <- plot_fun_instru(div_enf_list[[i]]) +
    labs(title = names(div_enf_list[i]))
  police_plots_instru[[i]] <- gg
}


# Plot
p5 <- plot_grid(plotlist = police_plots_instru, ncol = 3)
ggsave(p5, filename = "instructed.png", height = 10, width = 8)

