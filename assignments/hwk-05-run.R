
# Meta --------------------------------------------------------------------

## Title:         Econ/HLTH 470 Homework 5 Answers
## Author:        Ian McCarthy
## Date Created:  4/13/2020
## Date Edited:   4/16/2020
## Description:   This file renders/runs all relevant R code for the assignment


# Preliminaries -----------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, scales,
               stargazer, ivpack, kableExtra, broom, cobalt, lfe, dotwhisker)


setwd('assignments')
source('paths.R')


# Read data and set workspace for knitr -------------------------------
final.data <- readRDS(paste0(path.insurance.data,'/acs_medicaid.rds'))

final.data <- final.data %>%
  mutate(perc_private = (ins_employer + ins_direct)/adult_pop,
         perc_public = (ins_medicare + ins_medicaid)/adult_pop,
         perc_ins = (adult_pop - uninsured)/adult_pop,
         perc_unins = uninsured/adult_pop,
         perc_employer = ins_employer/adult_pop,
         perc_medicaid = ins_medicaid/adult_pop,
         perc_medicare = ins_medicare/adult_pop,
         perc_direct = ins_direct/adult_pop)



# Create objects for markdown ---------------------------------------------


## Summary, Q1 - Share of direct purchase
direct.plot <- final.data %>% group_by(year) %>% summarize(mean=mean(perc_direct)) %>%
  ggplot(aes(x=year,y=mean)) + geom_line() + geom_point() + theme_bw() +
  labs(
    x="Year",
    y="Fraction with Direct Purchase",
    title="Share of Direct Purchase Insurance over Time"
  ) +
  geom_vline(xintercept=2013.5, color="red")


## Summary, Q2 - Share of medicaid
medicaid.plot <- final.data %>% group_by(year) %>% summarize(mean=mean(perc_medicaid)) %>%
  ggplot(aes(x=year,y=mean)) + geom_line() + geom_point() + theme_bw() +
  labs(
    x="Year",
    y="Fraction with Medicaid",
    title="Share of Medicaid Insurance over Time"
  ) +
  geom_vline(xintercept=2013.5, color="red")

## Sumary, Q3 - Share uninsured
ins.plot.dat <- final.data %>% filter(!is.na(expand_ever)) %>%
  group_by(expand_ever, year) %>% summarize(mean=mean(perc_unins))

uninsurance.plot <- ggplot(data=ins.plot.dat, aes(x=year,y=mean,group=expand_ever,linetype=expand_ever)) + 
  geom_line() + geom_point() + theme_bw() +
  geom_vline(xintercept=2013.5, color="red") +
  geom_text(data = ins.plot.dat %>% filter(year == 2016), 
            aes(label = c("Non-expansion","Expansion"),
                x = year + 1,
                y = mean)) +
  guides(linetype=FALSE) +
  labs(
    x="Year",
    y="Fraction Uninsured",
    title="Share of Uninsured over Time"
  )


## ATE, Q1 - DD Table
dd.table <- final.data %>% 
  filter(!is.na(expand_ever), year %in% c(2012, 2015)) %>%  
  group_by(expand_ever, year) %>%
  summarize(uninsured=mean(perc_unins))

dd.table <- pivot_wider(dd.table, names_from="year", names_prefix="year", values_from="uninsured") %>% 
  ungroup() %>%
  mutate(expand_ever=case_when(
    expand_ever==FALSE ~ 'Non-expansion',
    expand_ever==TRUE ~ 'Expansion')
  ) %>%
  rename(Group=expand_ever,
         Pre=year2012,
         Post=year2015)

## ATE, Q2/3 - DD regression estimates, 2014 expansion only (with and without state/year fixed effects)
reg.data <- final.data %>% mutate(post=(year>=2014),
                                  treat=post*expand_ever) %>%
  filter(is.na(expand_year) | expand_year==2014)

dd.est <- lm(perc_unins~post + expand_ever + treat, data=reg.data)
lfe.est <- felm(perc_unins~treat | factor(State) + factor(year), data=reg.data)

## ATE, Q4 - DD with time varying treatment
reg.data2 <- final.data %>% 
  mutate(treat=case_when(
    year>=expand_year & !is.na(expand_year) ~ 1,
    is.na(expand_year) ~ 0,
    year<expand_year & !is.na(expand_year) ~ 0)
  )
lfe.est2 <- felm(perc_unins~treat | factor(State) + factor(year), data=reg.data2)

## ATE, Q5 - Event study with time varying treatment
event.dat <- reg.data %>%
  mutate(expand_2012 = expand_ever*(year==2012),
         expand_2013 = expand_ever*(year==2013),
         expand_2014 = expand_ever*(year==2014),
         expand_2015 = expand_ever*(year==2015),
         expand_2016 = expand_ever*(year==2016),
         expand_2017 = expand_ever*(year==2017),
         expand_2018 = expand_ever*(year==2018))

event.ins.reg <- lm(perc_unins ~ expand_2012 + expand_2014 + 
                      expand_2015 + expand_2016 + expand_2017 + 
                      expand_2018 + factor(year) + factor(State), data=event.dat)
point.est <- as_tibble(c(event.ins.reg$coefficients[c("expand_2012","expand_2014","expand_2015",
                                                      "expand_2016","expand_2017","expand_2018")]),
                       rownames = "term")
ci.est <- as_tibble(confint(event.ins.reg)[c("expand_2012","expand_2014","expand_2015",
                                             "expand_2016","expand_2017","expand_2018"),],
                    rownames = "term")
point.est <- point.est %>% rename(estimate = value)
ci.est <- ci.est %>% rename(conf.low = `2.5 %`, conf.high = `97.5 %`)
new.row <- tibble(
  term = "expand_2013",
  estimate = 0,
  conf.low = 0,
  conf.high = 0,
  year = 2013
)

event.plot.dat <- point.est %>%
  left_join(ci.est, by=c("term")) %>%
  mutate(year = c(2012, 2014, 2015, 2016, 2017, 2018)) %>%
  bind_rows(new.row) %>%
  arrange(year)

event.plot <- dwplot(event.plot.dat, 
                     vline=geom_vline(xintercept=0, linetype=2), 
                     order_vars = c("expand_2018","expand_2017","expand_2016",
                                    "expand_2015","expand_2014","expand_2013",
                                    "expand_2012"),
                     whisker_args = list(color="black", size=1.1),
                     dot_args = list(color="black")) + 
  coord_flip() + theme_bw() + theme(legend.position = "none") +
  labs(y = "Year",
       x = "Estimate and 95% CI",
       title = "Event Study Estimates for Medicaid and Uninsurance Rate") +
  scale_y_discrete(labels = c("expand_2012" = "2012", 
                              "expand_2013" = "2013",
                              "expand_2014" = "2014",
                              "expand_2015" = "2015",
                              "expand_2016" = "2016",
                              "expand_2017" = "2017",
                              "expand_2018" = "2018"))



## ATE, Q5 - Event study with time varying treatment
event.dat2 <- reg.data2 %>%
  mutate(event_time=case_when(
    !is.na(expand_year) ~ year-expand_year ,
    is.na(expand_year) ~ -1)) %>%
  mutate(time_m4=1*(event_time==-6 | event_time==-5 | event_time==-4),
         time_m3=1*(event_time==-3),
         time_m2=1*(event_time==-2),
         time_0=1*(event_time==0),
         time_p1=1*(event_time==1),
         time_p2=1*(event_time==2),
         time_p3=1*(event_time==3),
         time_p4=1*(event_time==4))

event.ins.reg2 <- lm(perc_unins ~ time_m4 + time_m3 + time_m2 + time_0 +
                      time_p1 + time_p2 + time_p3 + time_p4 +
                      factor(year) + factor(State), data=event.dat2)

point.est2 <- as_tibble(event.ins.reg2$coefficients, rownames="term")
point.est2 <- point.est2 %>% filter(term %in% c("time_m4","time_m3","time_m2","time_0",
                                                        "time_p1","time_p2","time_p3","time_p4"))
ci.est2 <- as_tibble(confint(event.ins.reg2), rownames="term")
ci.est2 <- ci.est2 %>% filter(term %in% c("time_m4","time_m3","time_m2","time_0","time_p1","time_p2","time_p3","time_p4"))
              
point.est2 <- point.est2 %>% rename(estimate = value)
ci.est2 <- ci.est2 %>% rename(conf.low = `2.5 %`, conf.high = `97.5 %`)
new.row <- tibble(
  term = "time_m1",
  estimate = 0,
  conf.low = 0,
  conf.high = 0,
  event_time = -1
)

event.plot.dat2 <- point.est2 %>%
  left_join(ci.est2, by=c("term")) %>%
  mutate(event_time = c(-4,-3,-2,0,1,2,3,4)) %>%
  bind_rows(new.row) %>%
  arrange(event_time)

event.plot2 <- dwplot(event.plot.dat2, 
                     vline=geom_vline(xintercept=0, linetype=2), 
                     order_vars = c("time_p4","time_p3","time_p2",
                                    "time_p1","time_0","time_m1",
                                    "time_m2","time_m3","time_m4"),
                     whisker_args = list(color="black", size=1.1),
                     dot_args = list(color="black")) + 
  coord_flip() + theme_bw() + theme(legend.position = "none") +
  labs(y = "Year",
       x = "Estimate and 95% CI",
       title = "Event Study Estimates for Medicaid and Uninsurance Rate") +
  scale_y_discrete(labels = c("time_p4" = "t+4", 
                              "time_p3" = "t+3",
                              "time_p2" = "t+2",
                              "time_p1" = "t+1",
                              "time_0" = "0",
                              "time_m1" = "t-1",
                              "time_m2" = "t-2",
                              "time_m3" = "t-3",
                              "time_m4" = "t-4+"))


rm(list=c("final.data","ins.plot.dat","reg.data","event.plot.dat","event.plot.dat2"))
save.image("Hwk5_workspace.Rdata")




# Run abstract markdown ---------------------------------------------------
rmarkdown::render(input = 'hwk-05-answers.Rmd',
                  output_file ='hwk-05-answers')



