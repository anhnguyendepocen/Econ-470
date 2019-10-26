
# Meta --------------------------------------------------------------------
# Title:    R Script to render bookdown scripts 
# Author:   Ian McCarthy
# Date Created: 10/24/2019
# Date Edited: 10/25/2019




# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, readr, readxl, hrbrthemes,
               scales, plotly, bookdown)
setwd("D:/CloudStation/Professional/Teaching Material/Emory/Econ 470 - Economics and Health Policy/assignments")


# Assignments -------------------------------------------------------------
bookdown::render_book('index.Rmd', 'bookdown::gitbook')
