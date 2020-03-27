
# Meta --------------------------------------------------------------------

## Title:         Econ/HLTH 470 Homework 3 Answers
## Author:        Ian McCarthy
## Date Created:  3/16/2020
## Date Edited:   3/20/2020
## Description:   This file renders/runs all relevant R code for the assignment


# Preliminaries -----------------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, scales,
               stargazer, ivpack)
loadfonts(device = "win")

setwd('assignments')
source('paths.R')


# Read data and set workspace for knitr -------------------------------
cig.data <- readRDS(paste0(path.tobacco.data,'/TaxBurden_Data.rds'))


cig.data <- cig.data %>%
  mutate(tax_change = tax_state - lag(tax_state),
         tax_change_d = ifelse(tax_change==0,0,1),
         price_cpi_2012 = cost_per_pack*(229.5939/index),
         total_tax_cpi_2012=tax_dollar*(229.5939/index),
         ln_sales=log(sales_per_capita),
         ln_price_2012=log(price_cpi_2012))


# Create objects for markdown ---------------------------------------------


## sales per capita
sales.plot <- cig.data %>% 
  ggplot(aes(x=Year,y=sales_per_capita)) + 
  stat_summary(fun.y="mean",geom="line") +
  labs(
    x="Year",
    y="Packs per Capita"
  ) + theme_bw() +
  scale_x_continuous(breaks=seq(1970, 2019, 5))

## tax changes
tax.change.plot <- cig.data %>% group_by(Year) %>% filter(Year<1986, Year>1970) %>%
  summarize(mean_change=mean(tax_change_d)) %>%
  ggplot(aes(x=as.factor(Year), y=mean_change)) +
  geom_bar(stat="identity") +
  labs(
    x="Year",
    y="Share of States"
  ) + 
  theme_bw()
  

## Tax amounts
tax.plot <- cig.data %>% 
  ggplot(aes(x=Year,y=total_tax_cpi_2012)) + 
  stat_summary(fun.y="mean",geom="line") +
  labs(
    x="Year",
    y="Tax per Pack ($)"
  ) + theme_bw() +
  scale_x_continuous(breaks=seq(1970, 2020, 5))

## Cigarette prices
price.plot <- cig.data %>% 
  ggplot(aes(x=Year,y=price_cpi_2012)) + 
  stat_summary(fun.y="mean",geom="line") +
  labs(
    x="Year",
    y="Price per Pack ($)"
  ) + theme_bw() +
  scale_x_continuous(breaks=seq(1970, 2020, 5))


## Price changes
cig.data.change <- cig.data %>% filter(Year==1970) %>% select(state, price_1970=price_cpi_2012) %>%
  left_join(cig.data %>% filter(Year==2018) %>% select(state, price_2018=price_cpi_2012),
            by=c("state")) %>%
  mutate(price_change = price_2018-price_1970)

high.change <- cig.data.change %>% top_n(5, wt=price_change) %>% mutate(change_group="high")
low.change <- cig.data.change %>% top_n(-5, wt=price_change) %>% mutate(change_group="low")
change.group <- rbind(high.change, low.change)

top.bottom.price <- cig.data %>%
  inner_join(change.group %>% select(state, change_group),
            by=c("state"))

## Figure for high price changes
high.price.plot <- top.bottom.price %>% filter(change_group=="high") %>%
  ggplot(aes(x=Year,y=sales_per_capita, color=state)) + 
  stat_summary(fun.y="mean",geom="line") +
  labs(
    x="Year",
    y="Packs per Capita",
    color="State"
  ) + theme_bw() +
  scale_x_continuous(breaks=seq(1970, 2019, 5))


## Figure for low price changes
low.price.plot <- top.bottom.price %>% filter(change_group=="low") %>%
  ggplot(aes(x=Year,y=sales_per_capita, color=state)) + 
  stat_summary(fun.y="mean",geom="line") +
  labs(
    x="Year",
    y="Packs per Capita",
    color="State"
  ) + theme_bw() +
  scale_x_continuous(breaks=seq(1970, 2019, 5))


## Figure for high and low price changes
combined.price.plot <- top.bottom.price %>% 
  ggplot(aes(x=Year,y=sales_per_capita, color=change_group)) + 
  stat_summary(fun.y="mean",geom="line") +
  labs(
    x="Year",
    y="Packs per Capita",
    color="Level of Price Increase"
  ) + theme_bw() +
  scale_x_continuous(breaks=seq(1970, 2019, 5))


## Regression results
ols1 <- lm(ln_sales~ln_price_2012, data=cig.data %>% filter(Year<1991))
iv1 <- ivreg(ln_sales ~ ln_price_2012 | total_tax_cpi_2012, data=cig.data %>% filter(Year<1991))
first.stage <- lm(ln_price_2012~total_tax_cpi_2012, data=cig.data %>% filter(Year<1991))
reduced.form <- lm(ln_sales~total_tax_cpi_2012, data=cig.data %>% filter(Year<1991))

ols2 <- lm(ln_sales~ln_price_2012, data=cig.data %>% filter(Year>=1991))
iv2 <- ivreg(ln_sales ~ ln_price_2012 | total_tax_cpi_2012, data=cig.data %>% filter(Year>=1991 & Year<=2015))
first.stage2 <- lm(ln_price_2012~total_tax_cpi_2012, data=cig.data %>% filter(Year>=1991 & Year<=2015))
reduced.form2 <- lm(ln_sales~total_tax_cpi_2012, data=cig.data %>% filter(Year>=1991 & Year<=2015))

rm(list=c("cig.data", "cig.data.change", "top.bottom.price"))
save.image("Hwk3_workspace.Rdata")


# Run abstract markdown ---------------------------------------------------
rmarkdown::render(input = 'hwk-03-answers.Rmd',
                  output_file ='hwk-03-answers')
