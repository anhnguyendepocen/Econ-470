
# Meta --------------------------------------------------------------------

## Title:         Econ/HLTH 470 Homework 1 Answers
## Author:        Ian McCarthy
## Date Created:  2/1/2020
## Date Edited:   2/27/2020
## Description:   This file renders/runs all relevant R code for the assignment


# Preliminaries -----------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, scales)

setwd('assignments')
source('paths.R')

# Read data and set workspace for knitr -------------------------------
full.ma.data <- readRDS(paste0(path.ma.data,'/full_ma_data.rds'))
contract.service.area <- readRDS(paste0(path.ma.data,'/contract_service_area.rds'))
ma.penetration.data <- readRDS(paste0(path.ma.data,'/ma_penetration.rds'))
plan.premiums <- readRDS(paste0(path.ma.data,'/plan_premiums.rds'))


# Create objects for markdown ---------------------------------------------

plan.type.table <- full.ma.data %>% group_by(plan_type) %>% count() %>% arrange(-n)
tot.obs <- as.numeric(count(full.ma.data %>% ungroup()))
plan.type.year1 <- full.ma.data %>% group_by(plan_type, year) %>% count() %>% arrange(year, -n)
plan.type.year1 <- pivot_wider(plan.type.year1, names_from="year",values_from="n", names_prefix="Count_")

final.plans <- full.ma.data %>%
  filter(snp == "No" & eghp == "No" &
           (planid < 800 | planid >= 900))
plan.type.year2 <- final.plans %>% group_by(plan_type, year) %>% count() %>% arrange(year, -n)
plan.type.year2 <- pivot_wider(plan.type.year2, names_from="year",values_from="n", names_prefix="Count_")


final.data <- final.plans %>%
  inner_join(contract.service.area %>% 
               select(contractid, fips, year), 
             by=c("contractid", "fips", "year")) %>%
  filter(!is.na(avg_enrollment))

final.data.pen <- final.data %>%
  left_join( ma.penetration.data %>% ungroup() %>%
               rename(state_long=state, county_long=county), 
             by=c("fips", "year"))

final.state <- final.data.pen %>% 
  group_by(state) %>% 
  summarize(state_name=last(state_long, na.rm=TRUE))

final.data.pen <- final.data.pen %>%
  left_join(final.state,
            by=c("state"))

prem.data <- final.data.pen %>%
  left_join( plan.premiums,
             by=c("contractid","planid","state_name"="state","county","year"))


fig.avg.enrollment <- final.data %>%
  group_by(fips, year) %>% 
  select(fips, year, avg_enrollment) %>% 
  summarize(all_enroll=sum(avg_enrollment)) %>%
  ggplot(aes(x=as.factor(year),y=all_enroll)) + 
  stat_summary(fun.y="mean", geom="bar") +
  labs(
    x="Year",
    y="People",
    title="Average Number of MA Enrollees per County"
  ) + scale_y_continuous(labels=comma) +
  theme_bw()

fig.avg.premium <- prem.data %>% ungroup() %>% group_by(year) %>%
  ggplot(aes(x=as.factor(year),y=premium, group=1)) + 
  stat_summary(fun.y="mean", geom="line", na.rm=TRUE) +
  labs(
    x="Year",
    y="Premium",
    title="Average Plan Premium"
  ) + scale_y_continuous(labels=comma) +
  theme_bw()

fig.percent.zero <- prem.data %>% ungroup() %>%
  mutate(prem_0=(premium==0),
         prem_na=(is.na(premium))) %>%
  group_by(year) %>%
  summarize(all_count=n(),prem_0=sum(prem_0, na.rm=TRUE), prem_na=sum(prem_na)) %>%
  mutate(perc_0=prem_0/all_count) %>%
  ggplot(aes(x=as.factor(year), y=perc_0, group=1)) + geom_line() +
  labs(
    x="Year",
    y="Percent",
    title="Percent of plans with $0 premiums"
  ) + scale_y_continuous(labels=percent) +
  theme_bw()


rm(list=c("full.ma.data", "contract.service.area",
           "ma.penetration.data", "plan.premiums", "final.plans",
           "final.data.pen", "final.state","prem.data","final.data"))
save.image("Hwk1_workspace.Rdata")


# Run abstract markdown ---------------------------------------------------
rmarkdown::render(input = 'hwk-01-answers.Rmd',
                  output_file ='hwk-01-answers')
