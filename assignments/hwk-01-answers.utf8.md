---
title: "Homework 1 Answer Key"
author: "Econ 470/HLTH 470: Research in Health Economics"
date: "Due: Wednesday, February 5"
output: 
  bookdown::html_document2:
    toc: TRUE
    toc_float: TRUE
    theme: darkly
---



# Enrollment Data
Answer the following based on the enrollment data:

1. How many observations exist in your current dataset?<br>

First we need to create the enrollment data. Working with the Medicare Advantage Github Repository, you should have created a "full.ma.data" object. The following code reads this dataset into `R` and counts the total number of plans:


```r
full.ma.data <- readRDS("D:/CloudStation/Professional/Research Projects/_Git/Medicare-Advantage/data/full_ma_data.rds")
tot.obs <- count(full.ma.data %>% ungroup())
```
So we have [90m# A tibble: 1 x 1[39m,          n,      [3m[90m<int>[39m[23m, [90m1[39m 21[4m7[24m[4m7[24m[4m2[24m314 total observations in the full dataset, which means there are [90m# A tibble: 1 x 1[39m,          n,      [3m[90m<int>[39m[23m, [90m1[39m 21[4m7[24m[4m7[24m[4m2[24m314 unique combinations of contract/plan/county/year. 



2. How many different *plan_types* exist in the data? <br>

We need to tabulate the different plan types:

```r
plan.type.table <- full.ma.data %>% group_by(plan_type) %>% count() %>% arrange(-n)
knitr::kable(plan.type.table, col.names=c("Plan Type","Count"), format.args=list(big.mark=","))
```



Plan Type                                       Count
-----------------------------------------  ----------
Medicare Prescription Drug Plan             9,863,083
Local PPO                                   4,134,077
HMO/HMOPOS                                  4,113,313
PFFS                                        2,565,867
NA                                            313,526
Employer/Union Only Direct Contract PDP       287,955
Regional PPO                                  273,643
1876 Cost                                      78,180
MSA                                            65,734
Employer/Union Only Direct Contract PFFS       19,775
Medicare-Medicaid Plan HMO/HMOPOS              14,259
HCPP - 1833 Cost                               12,087
National PACE                                  10,597
SHMO                                            3,454
MSA Demo                                        3,274
PSO (State License)                             3,196
RFB PFFS                                        3,006
Pilot                                           2,440
MN Senior Health Options                        2,325
Continuing Care Retirement Community            1,016
ESRD I                                            553
PSO (Federal Waiver of State License)             420
MA Health Senior Care Options                     249
WI Partnership Program                            145
ESRD II                                            76
MN Disability Health Options                       64
The resulting table yields 26 rows, so there are 26 total plan types. I've provided the counts of each plan type just for extra detail.


3. Provide a table of the count of plans under each plan type in each year.<br>


```r
plan.type.year <- full.ma.data %>% group_by(plan_type, year) %>% count() %>% arrange(year, -n)
plan.type.year <- pivot_wider(plan.type.year, names_from="year",values_from="n", names_prefix="Count_")
options(knitr.kable.NA = 0)
knitr::kable(plan.type.year, col.names=c("Plan Type","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"), format.args=list(big.mark=","))
```



Plan Type                                      2006        2007        2008        2009      2010      2011      2012      2013        2014        2015
-----------------------------------------  --------  ----------  ----------  ----------  --------  --------  --------  --------  ----------  ----------
Medicare Prescription Drug Plan             867,792   1,030,521   1,066,772   1,034,001   977,384   845,656   886,729   899,197   1,195,055   1,059,976
PFFS                                        269,553     386,055     647,871     696,693   396,456    51,599    40,268    35,193      26,941      15,238
HMO/HMOPOS                                   68,984      95,692     109,339     516,426   545,066   566,392   545,128   575,463     565,925     524,898
Employer/Union Only Direct Contract PDP      38,866      32,177      28,949      25,724    28,626    28,623    28,581    25,441      25,443      25,525
Regional PPO                                 24,360      29,246      31,376      30,122    30,149    28,904    26,412    26,086      24,572      22,416
Local PPO                                    18,611      22,015      45,345     412,516   426,933   526,768   647,649   645,593     674,289     714,358
1876 Cost                                     6,497       7,374       6,725       6,872     7,251     8,101     8,794     9,129       8,568       8,869
SHMO                                          1,484       1,970           0           0         0         0         0         0           0           0
MN Senior Health Options                      1,079       1,246           0           0         0         0         0         0           0           0
PSO (State License)                             725         798         511         180       325       370       287         0           0           0
National PACE                                   487         642         823         882       980     1,096     1,228     1,364       1,483       1,612
PSO (Federal Waiver of State License)           181         239           0           0         0         0         0         0           0           0
MA Health Senior Care Options                    98         151           0           0         0         0         0         0           0           0
ESRD I                                           84          84         130         131       124         0         0         0           0           0
Continuing Care Retirement Community             69         194         274         262       217         0         0         0           0           0
WI Partnership Program                           61          84           0           0         0         0         0         0           0           0
MN Disability Health Options                     30          34           0           0         0         0         0         0           0           0
Pilot                                            16          15       1,981         326        89         3         3         2           2           3
HCPP - 1833 Cost                                 15          13       4,199       4,107     3,703        11        11        10           9           9
ESRD II                                          10          21          21          12        12         0         0         0           0           0
MSA                                               0       4,511      16,509      12,261       136     6,423     6,430     6,437       6,474       6,553
MSA Demo                                          0       3,274           0           0         0         0         0         0           0           0
Employer/Union Only Direct Contract PFFS          0       3,226       3,318       3,312     3,311     3,309     3,299         0           0           0
0                                                 0           0       8,969      27,420   277,137         0         0         0           0           0
RFB PFFS                                          0           0           0       3,006         0         0         0         0           0           0
Medicare-Medicaid Plan HMO/HMOPOS                 0           0           0           0         0         0         0       283       5,105       8,871

4. Remove all special needs plans (SNP), employer group plans (eghp), and all "800-series" plans.<br>


```r
final.data <- full.ma.data %>%
  filter(snp == "No" & eghp == "No" &
           (planid < 800 | planid >= 900))
plan.type.year <- final.data %>% group_by(plan_type, year) %>% count() %>% arrange(year, -n)
plan.type.year <- pivot_wider(plan.type.year, names_from="year",values_from="n", names_prefix="Count_")
options(knitr.kable.NA = 0)
knitr::kable(plan.type.year, col.names=c("Plan Type","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015"), format.args=list(big.mark=","))
```



Plan Type                                   2006      2007      2008      2009      2010      2011      2012      2013      2014      2015
--------------------------------------  --------  --------  --------  --------  --------  --------  --------  --------  --------  --------
Medicare Prescription Drug Plan          373,858   509,033   532,804   504,132   475,846   370,419   361,582   351,522   375,801   339,352
HMO/HMOPOS                                44,254    55,847    60,190    62,014    62,371    61,508    64,635    69,317    68,791    69,185
PFFS                                      22,691    68,491   124,598   104,574    65,836    27,827    21,306    16,231     8,101     5,818
Regional PPO                              12,615     9,307    10,399    11,900    14,631    15,521    15,212    14,238    13,991    12,251
Local PPO                                  7,556     9,489    12,714    16,766    20,625    24,810    27,251    28,585    27,005    26,856
1876 Cost                                  5,646     6,375     5,631     5,643     5,978     6,914     7,668     7,977     7,521     7,900
SHMO                                       1,269       824         0         0         0         0         0         0         0         0
PSO (State License)                          678       738       370       137       265       304       226         0         0         0
National PACE                                470       632       819       881       980     1,096     1,228     1,364     1,483     1,612
PSO (Federal Waiver of State License)        129       185         0         0         0         0         0         0         0         0
ESRD I                                        84        84       130       131       124         0         0         0         0         0
Continuing Care Retirement Community          69       110       106       108       108         0         0         0         0         0
MSA                                            0     2,267     3,364     2,516        69       135       150       155       192       271
MSA Demo                                       0       129         0         0         0         0         0         0         0         0
0                                              0         0     2,065    13,601    30,071         0         0         0         0         0
RFB PFFS                                       0         0         0     3,006         0         0         0         0         0         0
Medicare-Medicaid Plan HMO/HMOPOS              0         0         0         0         0         0         0       283     5,105     8,871

5. Merge the the contract service area data to the enrollment data and restrict the data only to contracts that are approved in their respective counties.<br>

Let's first read in the contract service area data:

```r
contract.service.area <- readRDS("D:/CloudStation/Professional/Research Projects/_Git/Medicare-Advantage/data/contract_service_area.rds")
```

Now we can join that dataset to our MA data. I'm going to use an inner join, which means I'm only taking rows that match in both datasets.

```r
final.data <- final.data %>%
  inner_join(contract.service.area %>% 
               select(contractid, fips, year), 
             by=c("contractid", "fips", "year"))
```
  

6. Finally, limit your dataset only to plans with non-missing enrollment data. Provide a graph showing the average number of Medicare Advantage enrollees per county from 2008 to 2015.<br>

```r
final.data <- final.data %>%
  filter(!is.na(avg_enrollment))

final.data %>%
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
```

<img src="hwk-01-answers_files/figure-html/unnamed-chunk-7-1.png" width="672" />

# Premium Data

1. Merge the plan characteristics data to the dataset you created in Step 6 above.<br>

As mentioned in the instructions, we first need to merge in the market penetration data to provide a crosswalk between the plan/contract info and the plan characteristics. 

```r
ma.penetration.data <- readRDS("D:/CloudStation/Professional/Research Projects/_Git/Medicare-Advantage/data/ma_penetration.rds")
final.data <- final.data %>%
  left_join( ma.penetration.data %>% ungroup() %>%
               rename(state_long=state, county_long=county), 
             by=c("fips", "year"))
```

Next we need to fill in the state information. I do this by creating a table of unique state names and then merging this back to the original data:

```r
final.state <- final.data %>% 
  group_by(state) %>% 
  summarize(state_name=last(state_long, na.rm=TRUE))

final.data <- final.data %>%
  left_join(final.state,
            by=c("state"))
```

Finally, we can read in the premium data and merge that information to the final dataset

```r
plan.premiums <- readRDS("D:/CloudStation/Professional/Research Projects/_Git/Medicare-Advantage/data/plan_premiums.rds")
final.data <- final.data %>%
  left_join( plan.premiums,
             by=c("contractid","planid","state_name"="state","county","year"))
```

2. Provide a graph showing the average premium over time. Don't forget about formatting!



3. Provide a graph showing the percentage of $0 premium plans over time. Also...remember to format things.


```r
final.data %>% ungroup() %>%
  mutate(prem_0=(premium==0),
         prem_na=(is.na(premium))) %>%
  group_by(year) %>%
  summarize(all_count=n(),prem_0=sum(prem_0, na.rm=TRUE), prem_na=sum(prem_na))
```

```
## # A tibble: 10 x 4
##     year all_count prem_0 prem_na
##    <int>     <int>  <int>   <int>
##  1  2006     12537      0   12537
##  2  2007     12738   5006    1078
##  3  2008     20645   7958     943
##  4  2009     26105  10274     748
##  5  2010     24748   7494     632
##  6  2011     21619   7041    2597
##  7  2012     23724   9012     724
##  8  2013     23778   8753     752
##  9  2014     23866      0    9243
## 10  2015     24066   6643    1392
```





# Summary Questions
With all of this data work and these great summaries, let's take a step back and think about what all this means.

1. Why did we drop the "800-series" plans?

2. Why do so many plans charge a $0 premium? What does that really mean to a beneficiary?

3. Briefly describe your experience working with these data (just a few sentences). Tell me one thing you learned and one thing that really aggravated you.
