
rm(list = ls())
gc()

library(knitr)
library(data.table)
library(gdata) # read.xls
library(plyr)
library(dplyr)
options(dplyr.print_min = 100) # default is 10
options(dplyr.print_max = 100) # default is 20
library(ggplot2)
library(magrittr)
library(tidyr) # gather, spread
library(readxl)
library(xlsx)
library(stringr)
library(extrafont)
library(pdata)
library(apitools)

library("grid")
library("gridExtra")

# devtools::install_github("donboyd5/apitools")
# devtools::install_github("donboyd5/pdata")
source("Functions.R")

#****************************************************************************************************
#                System-specific definitions ####
#****************************************************************************************************

runsd       <- "IO_M1_new/"
outputs.dir <- "IO_M1_new/M1_outputs/"

#****************************************************************************************************
#                RIG color palette ####
#****************************************************************************************************

RIG.blue  <- "#003598"
RIG.red   <- "#A50021"
RIG.green <- "#009900"
RIG.yellow <- "#FFFF66"
RIG.purple <- "#9966FF"
RIG.yellow.dark <- "#ffc829"

# font_import(pattern="[A/a]rial")
# loadfonts(device="win")
# loadfonts(device="pdf")

RIG.theme <- function(){
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, color = "gray80"),
        panel.background = element_rect(fill = "white", color = "grey"),
        legend.key = element_rect(fill = "white", color = "grey"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
}



#****************************************************************************************************
#                Functions ####
#****************************************************************************************************
p25 <- function(x) {as.numeric(quantile(x, .25, na.rm=TRUE))} # use braces so function appears in RStudio outline
p50 <- function(x) {as.numeric(quantile(x, .50, na.rm=TRUE))}
p75 <- function(x) {as.numeric(quantile(x, .75, na.rm=TRUE))}
pany <- function(x, p) {as.numeric(quantile(x, p, na.rm=TRUE))}

# ma4 <- function(x) {rollapply(x, 4, function(x) mean(x, na.rm=TRUE), fill=NA, align="right")}
rollsd <- function(x, nobs) {zoo::rollapply(x, nobs, function(x) sd(x, na.rm=TRUE), fill=NA, align="right")}
# note that this is sample standard deviation

rollmean <- function(x, nobs) {zoo::rollapply(x, nobs, function(x) mean(x, na.rm=TRUE), fill=NA, align="right")}

rollmin <- function(x, nobs) {zoo::rollapply(x, nobs, function(x) min(x, na.rm=TRUE), fill=NA, align="right")}

getrun <- function(runfile, dir) {
  fn <- paste0(dir, runfile)
  load(fn)
  df <- outputs_list$results
  return(df)
}


getpop <- function(runfile, dir) {
  fn <- paste0(dir, runfile)
  load(fn)
  assign(runfile, outputs_list$demo)
  #ls <- outputs_list$demo
  
}


#****************************************************************************************************
#                2016 Analysis of simulations ####
#****************************************************************************************************

allrunfiles <- list.files(runsd, pattern="RData")

# Desired runs, in the order I would like them for the table
# A1F075_0           75% initial Funding; No amortization; No Smoothing NOT IN TABLE
# A1F075_C15d        15-year level dollar - closed
# A1F075_C15p        15-year level percent - closed
# A1F075_C30d        30-year level dollar - closed
# A1F075_C30p        30-year level percent - closed
# A1F075_C30pA5      30-year level percent - closed;  5-year asset smoothing
# A1F075_O15d        15-year level dollar - open
# A1F075_O15p        15-year level percent - open
# A1F075_O30d        30-year level dollar - open
# A1F075_O30p        30-year level percent - open
# A1F075_O30pA5      30-year level percent - open; 5-year asset smoothing
# A1F075_O30pA5_cap  30-year level percent - closed; 5-year asset smoothing; 20% ERC cap
# A1F075_soa3        SOA Blue Ribbon Panel Benchmark

load(paste0(runsd, "Outputs_PPD_50_60_10.RData"))
outputs_list$entrant_dist %>% plot


runs <- c("PPD_50_60_10", 
          "PPD_50_60_5",
          "PPD_50_60_0"
          #"PPD_20_60_25",
          )
files2get <- paste0("Outputs_", runs, ".RData")
names(files2get) <- runs


system.time(list_pop <- llply(files2get, getpop, runsd, .progress="text"))
glimpse(list_pop)
names(list_pop)

system.time(df <- ldply(files2get, getrun, runsd, .progress="text"))
glimpse(df)
names(df)
count(df, runname)


df_ret <- 
bind_rows(
list_pop$PPD_50_60_10$retired %>% mutate(planname = "PPD_50_60_10"),
list_pop$PPD_50_60_5$retired %>%  mutate(planname = "PPD_50_60_5"),
list_pop$PPD_50_60_0$retired %>%  mutate(planname = "PPD_50_60_0")
#list_pop$PPD_20_60_25$retired %>% mutate(planname = "PPD_20_60_25")
)


# Impact of yos rules on average retirement age
retAge_Avg <- 
df_ret %>% 
  mutate(planname = factor(planname, levels = c("PPD_50_60_0", 
                                                "PPD_50_60_5", 
                                                "PPD_50_60_10",
                                                "PPD_20_60_25"
                                              ), ordered = TRUE
                           )
         ) %>% 
  group_by(planname, year) %>% 
  filter(year == year.retire) %>% 
  summarise(avg.retAge = sum(age * number.r)/sum(number.r))
retAge_Avg %>%  filter(year == 15)



# Distributions of retirement age 
regAge_dist <- 
df_ret %>% 
  mutate(planname = factor(planname, levels = c("PPD_50_60_0", 
                                                "PPD_50_60_5", 
                                                "PPD_50_60_10",
                                                "PPD_20_60_25"
  ), ordered = TRUE
  )
  ) %>% 
  #group_by(planname, year) %>% 
  filter(year == year.retire, year == 15, age %in% 45:75) %>% 
  group_by(planname, year,age) %>% 
  summarise(number.r = sum(number.r))


regAge_dist %>% ggplot(aes(x = age, y = number.r, color = planname)) +
  geom_line() + 
  geom_point()




# Impact of retirement rules on plan funding
df %>% filter(sim == 0, year %in%c(15)) %>% 
  select(runname, year,  AL, B, ERC_PR, NC_PR)

df %>% filter(sim == 0, year %in%c(30)) %>% 
  select(runname, year,  AL, B, ERC_PR, NC_PR)




# Impact of retirement rules on risk measures

df_all.stch <- df  %>% 
  filter(sim > 0, year %in% 1:30)


df_all.stch %<>%   
  select(runname, sim, year, FR_MA, AL, MA, ERC, EEC, PR, ERC_PR) %>%
  mutate(runname = factor(runname, levels = c("PPD_50_60_0", 
                                              "PPD_50_60_5", 
                                              "PPD_50_60_10",
                                              "PPD_20_60_25"), 
                          ordered = TRUE
  )) %>% 
  group_by(runname, sim) %>% 
  mutate(
    #FR_MA     = 100 * MA / AL,
    FR40less  = cumany(FR_MA <= 40),
    # FR100more = FR_MA >= 100,
    #ERC_high  = cumany(ERC_PR >= 50), 
    ERC_hike     = cumany(na2zero(ERC_PR - lag(ifelse(year == 2016, NA, ERC_PR), 5) >= 10))  # NA for 2016 value: excludes impact of new amort payment in 2017 
  ) %>% 
  group_by(runname, year) %>% 
  summarize(FR40less = 100 * sum(FR40less, na.rm = T)/n(),
            #FR100more = 100 * sum(FR100more, na.rm = T)/n(),
            #ERC_high = 100 * sum(ERC_high, na.rm = T)/n(),
            ERC_hike = 100 * sum(ERC_hike, na.rm = T)/n(),
            #ERC_GF_hike = 100 * sum(ERC_hike, na.rm = T)/n(),
            
            FR.q10   = quantile(FR_MA, 0.1,na.rm = T),
            FR.q25   = quantile(FR_MA, 0.25, na.rm = T),
            FR.q50   = quantile(FR_MA, 0.5, na.rm = T),
            FR.q75   = quantile(FR_MA, 0.75, na.rm = T),
            FR.q90   = quantile(FR_MA, 0.9, na.rm = T),
            
            ERC_PR.q10 = quantile(ERC_PR, 0.1, na.rm = T),
            ERC_PR.q25 = quantile(ERC_PR, 0.25, na.rm = T),
            ERC_PR.q50 = quantile(ERC_PR, 0.5, na.rm = T),
            ERC_PR.q75 = quantile(ERC_PR, 0.75, na.rm = T),
            ERC_PR.q90 = quantile(ERC_PR, 0.9, na.rm = T)
  ) 
  


df_all.stch %>% filter(runname == "PPD_50_60_0") %>% filter(year %in% c(15, 30))
df_all.stch %>% filter(runname == "PPD_50_60_5") %>% filter(year %in% c(15, 30))
df_all.stch %>% filter(runname == "PPD_50_60_10") %>% filter(year %in% c(15, 30))






# average retirement age
retAge_Avg %>%  filter(year == 15)

# distribution of retirement age
regAge_dist %>% ggplot(aes(x = age, y = number.r, color = planname)) +
  geom_line() + 
  geom_point()

# plan funding
df %>% filter(sim == 0, year %in%c(15)) %>% 
  select(runname, year,  AL, B, ERC_PR, NC_PR)

df %>% filter(sim == 0, year %in%c(30)) %>% 
  select(runname, year,  AL, B, ERC_PR, NC_PR)

# risk measures
df_all.stch %>% filter(year %in% c(15))
df_all.stch %>% filter(year %in% c(30))

  
  # Impact of yos rule: 
  #   - minimal on FR40less
  #   - moderate on ERC_hike, especially after 30 years. 
  #        














