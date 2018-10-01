Informality and Financial Inclusion in Developing Economies
================
Marine
9/27/2018

Loading Packages:
=================

``` r
# install.packages("pacman")
library(pacman)
pacman::p_load(foreign, readstata13, tidyverse,
               ggthemes, kableExtra, scales, stats,
               GGally, mctest)
```

Loading the World Bank Enterprise Survey Data for 13 countries:
===============================================================

``` r
madaga_informal <- foreign::read.dta("Madagascar-2009-Informal-full data-.dta")
ivory_informal <- readstata13::read.dta13("Côte d'Ivoire-2009-Informal-full data-.dta", generate.factors = T)
burkina_informal <- foreign::read.dta("Burkina Faso-2009-Informal-full data-.dta")
mauritius_informal <- foreign::read.dta("Mauritius-2009-Informal-full data-.dta")
capeverde_informal <- foreign::read.dta("Cape Verde-2009-Informal-full data-.dta")
mali_informal <- foreign::read.dta("Mali-2010-Informal-full data-.dta")
botswana_informal <- foreign::read.dta("Botswana-2010-Informal-full data-.dta")
angola_informal <- foreign::read.dta("Angola-2010-Informal-full data-.dta")
rwanda_informal <- foreign::read.dta("Rwandainformal-2011 data-.dta")
nepal_informal <- foreign::read.dta("Nepal-2009-Informal-fulldata-.dta")
argentina_informal <- foreign::read.dta("Argentina-2010-Informal-fulldata-.dta")
peru_informal <- foreign::read.dta("Peru-2010-Informal-fulldata-.dta")
myanmar_informal <- foreign::read.dta("Myanmar_Informal-2014-fulldata.dta")
```

Subsetting the Data:
====================

I use a for loop to add a "formality" column with constant value 0 to all 10 datasets:

``` r
filenames <- names(which(sapply(.GlobalEnv, is.data.frame)))
for(i in seq_along(filenames)) {
  x <- get(filenames[i])
  for(j in 1:length(x))
  {
    x$formality[[j]] <- 0
  }
  assign(filenames[i], x)
}
```

Perceived Benefits of Registering
=================================

Some Data Cleaning
------------------

``` r
mad_benefits <- madaga_informal %>% select(r11a, r11b, r11c, r11d, r11e, 
         r11f, r11g, r11h, r11i, r12a)
ivo_benefits <- ivory_informal %>% select(r11a, r11b, r11c, r11d, r11e, 
         r11f, r11g, r11h, r11i, r12a)
mau_benefits <- mauritius_informal %>% select(r11a, r11b, r11c, r11d,
         r11e, r11f, r11g, r11h, r11i, r12a)
bur_benefits <- burkina_informal %>% select(r6a, r6b, r6c, r6d, r6e, 
         r6f, r6g, r6h, r6i, r7a)
cap_benefits <- capeverde_informal %>% select(r6a, r6b, r6c, r6d,
         r6e, r6f, r6g, r6h, r6i, r7a)
mal_benefits <- mali_informal %>% select(r6a, r6b, r6c, r6d, r6e, r6f, 
         r6g, r6h, r6i, r7a)
bot_benefits <- botswana_informal %>% select(r6a, r6b, r6c, r6d, r6e, r6f, r6g, 
         r6h, r6i, r7a)
ang_benefits <- angola_informal %>% select(r6a, r6b, r6c, r6d, r6e, r6f, r6g,
         r6h, r6i, r7a)

colnames(mad_benefits) <-  c("finance", "raw_materials", "less_bribes", "legal_foundations", 
           "gvt_programs", "workers", "formal_firms", "markets", "services", 
          "most_important")

colnames(ivo_benefits) <-  c("finance", "raw_materials", "less_bribes", "legal_foundations", 
           "gvt_programs", "workers", "formal_firms", "markets", "services", 
          "most_important")

colnames(mau_benefits) <-  c("finance", "raw_materials", "less_bribes", "legal_foundations", 
           "gvt_programs", "workers", "formal_firms", "markets", "services", 
          "most_important")

colnames(bur_benefits) <-  c("finance", "raw_materials", "less_bribes", "legal_foundations", 
           "gvt_programs", "workers", "formal_firms", "markets", "services", 
          "most_important")

colnames(cap_benefits) <-  c("finance", "raw_materials", "less_bribes", "legal_foundations", 
           "gvt_programs", "workers", "formal_firms", "markets", "services", 
          "most_important")

colnames(mal_benefits) <-  c("finance", "raw_materials", "less_bribes", "legal_foundations", 
           "gvt_programs", "workers", "formal_firms", "markets", "services", 
          "most_important")

colnames(bot_benefits) <-  c("finance", "raw_materials", "less_bribes", "legal_foundations", 
           "gvt_programs", "workers", "formal_firms", "markets", "services", 
          "most_important")

colnames(ang_benefits) <-  c("finance", "raw_materials", "less_bribes", "legal_foundations", 
           "gvt_programs", "workers", "formal_firms", "markets", "services", 
          "most_important")

perceived_benefits <- rbind(mad_benefits, ivo_benefits, mau_benefits, bur_benefits,
                            cap_benefits, mal_benefits, bot_benefits, ang_benefits)

perceived_benefits[perceived_benefits == "don't know"] <- NA
perceived_benefits[perceived_benefits == "not applicable    "] <- NA
perceived_benefits[perceived_benefits == "not applicable"] <- NA
perceived_benefits[perceived_benefits == "do not know      "] <- NA
perceived_benefits[perceived_benefits == "Does Not Apply"] <- NA
perceived_benefits[perceived_benefits == "do not know"] <- NA
perceived_benefits[perceived_benefits == "Don't Know (Spontaneous)"] <- NA
perceived_benefits[perceived_benefits == "not applicable "] <- NA
perceived_benefits[perceived_benefits == "do not know    "] <- NA
perceived_benefits[perceived_benefits == "not applicable "] <- NA
perceived_benefits[perceived_benefits == "do not know   "] <- NA
perceived_benefits[perceived_benefits == "not applicable  "] <- NA
perceived_benefits[perceived_benefits == "Don\x92t know  (spontaneous)"] <- NA


perceived_benefits$finance <- droplevels(perceived_benefits$finance)
perceived_benefits$raw_materials <- droplevels(perceived_benefits$raw_materials)
perceived_benefits$less_bribes <- droplevels(perceived_benefits$less_bribes)
perceived_benefits$legal_foundations <- droplevels(perceived_benefits$legal_foundations)
perceived_benefits$gvt_programs <- droplevels(perceived_benefits$gvt_programs)
perceived_benefits$workers <-droplevels(perceived_benefits$workers)
perceived_benefits$formal_firms <- droplevels(perceived_benefits$formal_firms)
perceived_benefits$markets <- droplevels(perceived_benefits$markets)
perceived_benefits$services <-droplevels(perceived_benefits$services)
perceived_benefits$most_important <- droplevels(perceived_benefits$most_important)

levels(perceived_benefits$finance)[c(2, 4, 6)] <- "No"
levels(perceived_benefits$finance)[c(1, 3, 5)] <- "Yes" 

levels(perceived_benefits$raw_materials)[c(2, 4, 6)] <- "No"
levels(perceived_benefits$raw_materials)[c(1, 3, 5)] <- "Yes" 

levels(perceived_benefits$less_bribes)[c(2, 4, 6)] <- "No"
levels(perceived_benefits$less_bribes)[c(1, 3, 5)] <- "Yes" 

levels(perceived_benefits$legal_foundations)[c(2, 3, 5)] <- "No"
levels(perceived_benefits$legal_foundations)[c(1, 4)] <- "Yes" 

levels(perceived_benefits$gvt_programs)[c(2, 3, 5)] <- "No"
levels(perceived_benefits$gvt_programs)[c(1, 4)] <- "Yes" 

levels(perceived_benefits$workers)[c(2, 3, 5)] <- "No"
levels(perceived_benefits$workers)[c(1, 4)] <- "Yes" 

levels(perceived_benefits$formal_firms)[c(2, 3, 5)] <- "No"
levels(perceived_benefits$formal_firms)[c(1, 4)] <- "Yes" 

levels(perceived_benefits$markets)[c(2, 3, 5)] <- "No"
levels(perceived_benefits$markets)[c(1, 4)] <- "Yes" 

levels(perceived_benefits$services)[c(2, 3, 5)] <- "No"
levels(perceived_benefits$services)[c(1, 4)] <- "Yes" 

levels(perceived_benefits$most_important)[c(1, 10, 19)] <- "Better access to financing" 
levels(perceived_benefits$most_important)[c(2, 10, 18)] <- "Better access to raw materials"
levels(perceived_benefits$most_important)[c(3, 10, 17)] <- "Less bribes to pay"
levels(perceived_benefits$most_important)[c(4, 10, 16)] <- "Better legal foundations on the property rights of land and buildings" 
levels(perceived_benefits$most_important)[c(5, 10, 15)] <- "More access to government programs or services"
levels(perceived_benefits$most_important)[c(6, 10, 14)] <- "Better access to workers"
levels(perceived_benefits$most_important)[c(7, 10, 13)] <- "Better opportunities with formal firms"
levels(perceived_benefits$most_important)[c(8, 10, 12)] <- "Better access to markets"
levels(perceived_benefits$most_important)[c(9, 10, 11)] <- "Better access to infrastructure services"

perceived_benefits$most_important <- droplevels(perceived_benefits$most_important)

lapply(perceived_benefits, FUN = levels)
```

    ## $finance
    ## [1] "Yes" "No" 
    ## 
    ## $raw_materials
    ## [1] "Yes" "No" 
    ## 
    ## $less_bribes
    ## [1] "Yes" "No" 
    ## 
    ## $legal_foundations
    ## [1] "Yes" "No" 
    ## 
    ## $gvt_programs
    ## [1] "Yes" "No" 
    ## 
    ## $workers
    ## [1] "Yes" "No" 
    ## 
    ## $formal_firms
    ## [1] "Yes" "No" 
    ## 
    ## $markets
    ## [1] "Yes" "No" 
    ## 
    ## $services
    ## [1] "Yes" "No" 
    ## 
    ## $most_important
    ## [1] "Better access to financing"                                           
    ## [2] "Better access to raw materials"                                       
    ## [3] "Less bribes to pay"                                                   
    ## [4] "Better legal foundations on the property rights of land and buildings"
    ## [5] "More access to government programs or services"                       
    ## [6] "Better access to workers"                                             
    ## [7] "Better opportunities with formal firms"                               
    ## [8] "Better access to markets"                                             
    ## [9] "Better access to infrastructure services"

Visualization Benefits
----------------------

### Tidy Format

``` r
# putting the data into tidy format to perform better data visualization 

benefits_viz <- perceived_benefits %>% select(-most_important) %>% 
  gather(key = "benefit", value = "value")

benefits_viz$benefit <- as.factor(benefits_viz$benefit)
levels(benefits_viz$benefit) <- c("Better Access to Financing", "Better Opportunities with Formal Firms", 
                                   "More Access to Government Programs", "Better Legal Foundations",
                                  "Less Bribes to Pay", "Better Access to Markets", 
                                  "Better Access to Materials", "Better Access to Infrastructure Services",
                                  "Better Access to Workers")

benefits_viz$value <- factor(benefits_viz$value, levels = c("Yes", "No"))
```

### Barplot 1 Registration Benefits

``` r
benefits_viz %>% filter(!is.na(value)) %>%
ggplot(aes(x = value)) + geom_bar(stat = "count", fill = "dodgerblue3", width = 0.75) + 
  facet_wrap(benefit ~., nrow = 4) + theme_economist() + 
  xlab("Perceived Benefit") + 
  ggtitle("Barplots Perceived Benefit of Formalization") + ylab("Count") + 
  theme(plot.title = element_text(color="dark blue", size=20, face="bold", hjust = 0.5),
        axis.title.x = element_text(color="dark blue", size=15, face="bold.italic", hjust = 0.5), 
        axis.title.y = element_text(color="dark blue", size=15, face="bold.italic", vjust = 0.5)) + 
  theme(strip.text.x = element_text(size = 8, colour = "dark blue", face = "bold.italic"), 
        axis.text.x = element_text(color="midnightblue", size=10.5, face="bold"),
        axis.text.y = element_text(color="midnightblue", size=10.5, face="bold"))
```

![](pods_coding_git_files/figure-markdown_github/unnamed-chunk-7-1.png)

**the gap between the frequency of yes/no answers is the greatest for "Better Access to Financing"**

### Barplot 2: Most Important Benefit

**When asked to identify the most important benefit of registering their businesses, firms overwhelmingly chose access to finance.**

``` r
levels(perceived_benefits$most_important)[4] <- "Better legal foundations"
levels(perceived_benefits$most_important)[5] <- "More access to government programs or services"
perceived_benefits %>% filter(!is.na(most_important)) %>%
ggplot(aes(x = most_important, fill = most_important)) + geom_bar(aes(y=..count../sum(..count..))) + 
  scale_fill_discrete(name = "Most Important Benefit") +  
  ggtitle("Most Important Perceived Benefit of Formalization") + ylab("Frequency") + theme_economist() +
  theme(plot.title = element_text(color="dark blue", size= 19, face="bold", hjust = 0.5),
        axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_text(color="dark blue", size=15, face="bold.italic", vjust = 0.5), 
        axis.text.y = element_text(color="midnightblue", size=11, face="bold"), axis.line.x =  element_blank()) + 
  theme(legend.title = element_text (color = "dark blue", size = 15, face = "bold"),
        legend.text  = element_text(color = "dark blue", size = 13, face = "bold.italic"), 
        legend.position=c(0.6,0.6), legend.box = "vertical", legend.background = element_rect(fill="transparent"))
```

![](pods_coding_git_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
perceived_benefits %>% filter(!is.na(most_important)) %>%
  ggplot(aes(x = most_important, fill = most_important)) + geom_bar(aes(y=..count../sum(..count..))) + 
  scale_fill_discrete(name = "Most Important Benefit") +  
  ggtitle("Most Important Perceived Benefit of Formalization") + ylab("Frequency") + theme_economist() +
  theme(plot.title = element_text(color = "dark blue", size = 19, face = "bold", hjust = 0),
        axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_text(color = "dark blue", size = 15, face = "bold.italic", vjust = 0.5), 
        axis.text.y = element_text(color = "midnightblue", size = 11, face = "bold"), axis.line.x =  element_blank()) + 
  theme(legend.title = element_text (color = "dark blue", size = 15, face = "bold"),
        legend.text  = element_text(color = "dark blue", size = 13, face = "bold.italic"), 
        legend.position= "right", legend.box = "vertical", legend.background = element_rect(fill="transparent"))
```

![](pods_coding_git_files/figure-markdown_github/unnamed-chunk-9-2.png)

Obstacles to Business Formalization
===================================

I will examine what are the major obstacles preventing informal firms from formally registering and whether some obstacles are more burdensome for specific types of informal firms, namely firms managed by women and “reluctant entrepreneurs”, which can offer important policy insights when designing Formalization Assistance Programs.

I will only use various subsets of countries because some datasets do not contain the relevant variables.

1st set of countries: Madagascar, Mauritius, Ivory Coast
--------------------------------------------------------

### Data Cleaning

``` r
# selecting relevant variables

mad_obstacles <- madaga_informal %>% 
  select(b3, b6, r6a, r6b, r6c, r6d, r6e, r6f)

mau_obstacles <- mauritius_informal %>% 
  select(b3, b6, r6a, r6b, r6c, r6d, r6e, r6f)

ivo_obstacles <- ivory_informal %>% 
  select(b3, b6, r6a, r6b, r6c, r6d, r6e, r6f)

obstacles1 <- rbind(mad_obstacles, mau_obstacles, ivo_obstacles)
obstacles1 <- obstacles1 %>% select(-c(b3,b6)) %>% gather(key = "obstacle", value = "degree")

# levels of factor column "obstacles"

obstacles1$obstacle <- as.factor(obstacles1$obstacle)
levels(obstacles1$obstacle) <- c("Information on Registration Procedures",
                                 "Time to Complete Registration Procedures", 
                                 "Registration Fees", "Taxes on Registered Businesses", 
                                 "Inspections by Government Officials",
                                 "Bribes on Registered Businesses")


# levels of "degree" factor variable 

obstacles1$degree <- as.factor(obstacles1$degree)
obstacles1$degree[obstacles1$degree == "does not apply"] <- NA
obstacles1$degree[obstacles1$degree == "don't know"] <- NA
obstacles1$degree <- droplevels(obstacles1$degree)
levels(obstacles1$degree) <- c("Major", "Minor", "Moderate", "No Obstacle", "Severe")
obstacles1$degree <- factor(obstacles1$degree, levels = c("No Obstacle", "Minor", "Moderate", "Major", "Severe"))
```

### Faceted Barplot

``` r
obstacles1 %>% filter(!is.na(degree)) %>% 
ggplot(aes(x = degree)) + geom_bar(stat = "count", width = 0.7, fill = "lightsalmon") +
  facet_wrap(obstacle ~., nrow = 3) + theme_economist() + xlab("How Much of an Obstacle") +
  ggtitle("Barplot Obstacles to Formalization") + ylab("Count") + 
    theme(plot.title = element_text(color="midnightblue", size=20, face="bold", hjust = 0.5),
          axis.title.x = element_text(color="dark blue", size=12, face="bold.italic", hjust = 0.5), 
          axis.title.y = element_text(color="dark blue", size=12, face="bold.italic", vjust = 0.5)) + 
    theme(strip.text.x = element_text(size = 10.5, colour = "navyblue", face = "bold"), 
          axis.text.x = element_text(color="midnightblue", size=10, face="bold"),
          axis.text.y = element_text(color="midnightblue", size=10, face="bold"))
```

![](pods_coding_git_files/figure-markdown_github/unnamed-chunk-11-1.png)

2nd set of countries: Myanmar, Peru, Argentina
----------------------------------------------

#### Data Cleaning

``` r
mya_obstacles <- myanmar_informal %>% 
  select(b3, r2a, r2b, r2c, r2d, r2e)

per_obstacles <- peru_informal %>% 
  select(b3, r2a, r2b, r2c, r2d, r2e)

arg_obstacles <- argentina_informal %>% 
  select(b3, r2a, r2b, r2c, r2d, r2e)

obstacles2 <- rbind(mya_obstacles, per_obstacles, arg_obstacles)

obstacles2 <- obstacles2 %>% gather(key = obstacle, value = "degree", -c(b3))

# levels "obstacles" factor column 

obstacles2$obstacle <- as.factor(obstacles2$obstacle)
levels(obstacles2$obstacle) <- c("Time, Fees and Paper Work Required", 
                                 "Taxes on Registered Businesses", 
                                 "Inspections by Government Officials",
                                 "Bribes on Registered Businesses", 
                                 "No Benefit from Being Registered")

# levels "degree" variable 
obstacles2$degree <- as.factor(obstacles2$degree)
obstacles2$degree[obstacles2$degree == "Don't Know"] <- NA
obstacles2$degree[obstacles2$degree == "Don\x92t know"] <- NA
obstacles2$degree <- droplevels(obstacles2$degree)
```

### Barplot

``` r
obstacles2 %>% filter(!is.na(degree)) %>% 
ggplot(aes(x = degree)) + geom_bar(stat = "count", width = 0.5, fill = "dodgerblue4") +
  facet_wrap(obstacle ~., nrow = 3) + theme_gdocs() + xlab("Obstacle") +
  ggtitle("Barplot Obstacles to Formalization") + ylab("Count") + 
    theme(plot.title = element_text(color="midnightblue", size=20, face="bold.italic", hjust = 0.5),
          axis.title.x = element_text(color="dark blue", size=15, face="bold.italic", hjust = 0.5), 
          axis.title.y = element_text(color="dark blue", size=15, face="bold.italic", vjust = 0.5)) + 
    theme(strip.text.x = element_text(size = 10, colour = "navyblue", face = "bold"), 
          axis.text.x = element_text(color="midnightblue", size=10, face="bold"),
          axis.text.y = element_text(color="midnightblue", size=10, face="bold"))
```

![](pods_coding_git_files/figure-markdown_github/unnamed-chunk-13-1.png)

3rd set of 9 countries: Madacasgar, Mauritius, Ivory Coast, Burkina Faso, Cape Verde, Mali, Botswana, Angola and Nepal
----------------------------------------------------------------------------------------------------------------------

### Cleaning

``` r
main_obstacle <- bind_rows(madaga_informal[,c(40, 52)], mauritius_informal[,c(39, 51)],
                           ivory_informal[,c(38, 50)], burkina_informal[,c(35, 46)],
                           capeverde_informal[,c(31, 43)], mali_informal[,c(33, 45)], 
                           botswana_informal[,c(29, 41)], angola_informal[,c(31, 43)],
                           nepal_informal[,c(32,39)])
```

    ## Warning in bind_rows_(x, .id): Unequal factor levels: coercing to character

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

    ## Warning in bind_rows_(x, .id): binding character and factor vector,
    ## coercing into character vector

``` r
# un-factoring columns 
main_obstacle$r7a <- as.character(main_obstacle$r7a)
main_obstacle$r2a <- as.character(main_obstacle$r2a)
main_obstacle$r2 <- as.character(main_obstacle$r2)

# cleaning
main_obstacle[389:975, 1] <- main_obstacle[389:975, 3]
main_obstacle[976:1095, 1] <- main_obstacle[976:1095, 4]
main_obstacle <- main_obstacle[,c(2,1)]
colnames(main_obstacle) <- c("country", "obstacle")

# transforming into factors 

main_obstacle$country <- as.factor(main_obstacle$country)
main_obstacle$obstacle <- as.factor(main_obstacle$obstacle)

main_obstacle$obstacle[main_obstacle$obstacle == "dk"] <- NA
main_obstacle$obstacle[main_obstacle$obstacle == "don't know"] <- NA
main_obstacle$obstacle[main_obstacle$obstacle == "Don\x92t know"] <- NA
main_obstacle$obstacle[main_obstacle$obstacle == "Other"] <- NA
main_obstacle$obstacle[main_obstacle$obstacle == "other (specify)                                                      "] <- NA
main_obstacle$obstacle <- droplevels(main_obstacle$obstacle)

levels(main_obstacle$obstacle)[c(2,3)] <- levels(main_obstacle$obstacle)[1]
levels(main_obstacle$obstacle)[c(2,3,5)] <- levels(main_obstacle$obstacle)[4]
levels(main_obstacle$obstacle)[c(3,5,6)] <- levels(main_obstacle$obstacle)[4]
levels(main_obstacle$obstacle)[c(5,10)] <- levels(main_obstacle$obstacle)[4]
levels(main_obstacle$obstacle)[c(5,6, 8)] <- levels(main_obstacle$obstacle)[7]
levels(main_obstacle$obstacle)[c(6, 8)] <- levels(main_obstacle$obstacle)[7]
levels(main_obstacle$obstacle)[c(7,8, 10)] <- levels(main_obstacle$obstacle)[9]

levels(main_obstacle$obstacle) <- c("Bribes on Registered Businesses", "Registration Fees", 
                                    "Getting Information on Registration Procedures", 
                                    "Inspections by Government Officials", "Taxes", 
                                    "No Registration Benefits", "Time Registration Procedures")
```

### Barplot Main Obstacle Overall:

``` r
main_obstacle %>% filter(!is.na(obstacle)) %>% 
ggplot(aes(x = obstacle, fill = obstacle)) + geom_bar(stat = "count", width = 0.7) + 
  scale_fill_discrete(name = "Main Obstacle") +  
  ggtitle("Main Obstacle to Business Formalization") + ylab("Count") + theme_economist() +
  theme(plot.title = element_text(color="dark blue", size=19, face="bold", hjust = 0),
        axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_text(color="dark blue", size=15, face="bold.italic", vjust = 0.5), 
        axis.text.y = element_text(color="midnightblue", size=11, face="bold"), axis.line.x =  element_blank()) + 
  theme(legend.title = element_text (color = "dark blue", size = 14, face = "bold"),
        legend.text  = element_text(color = "dark blue", size = 11, face = "bold.italic"), 
        legend.position= "right", legend.box = "vertical", legend.background = element_rect(fill="transparent"))
```

![](pods_coding_git_files/figure-markdown_github/unnamed-chunk-15-1.png)

### Barplot Main Obstacle by Country:

``` r
main_obstacle %>% filter(!is.na(obstacle)) %>% 
ggplot(aes(x = obstacle, fill = obstacle)) + geom_bar(stat = "count", width = 0.7) + 
  facet_wrap(country ~.) +  scale_fill_discrete(name = "Main Obstacle") +  
  ggtitle("Main Obstacle to Business Formalization by Country") + ylab("Count") + theme_economist() +
  theme(plot.title = element_text(color="dark blue", size=17.5, face="bold", hjust = 0.),
        axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_text(color="dark blue", size=15, face="bold.italic", vjust = 0.5), 
        axis.text.y = element_text(color="midnightblue", size=11, face="bold"), axis.line.x =  element_blank(),
        strip.text.x = element_text(size = 8.5, colour = "navyblue", face = "bold")) + 
  theme(legend.title = element_text (color = "dark blue", size = 14, face = "bold"),
        legend.text  = element_text(color = "dark blue", size = 10.5, face = "bold.italic"), 
        legend.position= "right", legend.box = "vertical", legend.background = element_rect(fill="transparent"))
```

![](pods_coding_git_files/figure-markdown_github/unnamed-chunk-16-1.png)

### Proportion Table Main Obstacle:

``` r
table <- as.matrix(round(prop.table(table(main_obstacle$obstacle)), 4)) 
table <- as.data.frame(table)
table$Obstacle <- rownames(table)
table <- table[c(2,1)]
colnames(table)[2] <- "Proportion"
table <- arrange(table, desc(Proportion))
table$Proportion <- percent(table$Proportion)

table %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
                full_width = F, font_size = 18)
```

<table class="table table-striped table-hover table-condensed table-responsive" style="font-size: 18px; width: auto !important; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
Obstacle
</th>
<th style="text-align:left;">
Proportion
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Getting Information on Registration Procedures
</td>
<td style="text-align:left;">
27.9%
</td>
</tr>
<tr>
<td style="text-align:left;">
Taxes
</td>
<td style="text-align:left;">
22.9%
</td>
</tr>
<tr>
<td style="text-align:left;">
Registration Fees
</td>
<td style="text-align:left;">
19.2%
</td>
</tr>
<tr>
<td style="text-align:left;">
No Registration Benefits
</td>
<td style="text-align:left;">
16.4%
</td>
</tr>
<tr>
<td style="text-align:left;">
Time Registration Procedures
</td>
<td style="text-align:left;">
9.5%
</td>
</tr>
<tr>
<td style="text-align:left;">
Inspections by Government Officials
</td>
<td style="text-align:left;">
2.3%
</td>
</tr>
<tr>
<td style="text-align:left;">
Bribes on Registered Businesses
</td>
<td style="text-align:left;">
1.7%
</td>
</tr>
</tbody>
</table>
Regression Analysis:
====================

Questions: What are the main determinants of access to finance? Do registered firms have better access to capital because of their formal status or because formality is often associated with higher managerial ability or other confounders of formality? We will determine wether formality can explain variation in access to finance while controlling for a variety of factors traditionally correlated with financial inclusion. what is the predictive power of formality on measures of financial inclusion?

Further Cleaning Informal Survey Datasets: Madacasgar, Ivory Coast, Mauritius
-----------------------------------------------------------------------------

**Explanatory Variables:** Country (fixed effects) Formality Owner Gender (Female = 1) Owner's Education Level Owner's Years of Experience Owner has Local Nationality Years in Operation Size (number of employees) Sales (converted to $US) Reluctant Entrepreneur

**Reponse Variables**

Bank Account Separate Business and Household Accounts Loan Loan Application Reason for not Applying

### Madagascar:

``` r
madaga_informal$size <- madaga_informal$l1a + madaga_informal$l1b
madaga_informal <- madaga_informal %>% select(-c(l1a, l1b))

madaga_informal <- madaga_informal %>% 
  select(country, formality, b3, b11, b12,afb4a, b8, size, d4, b6,
        k10, k12, k13, k16, k18, k19, k30)



colnames(madaga_informal)[-c(1,2)] <- c("female_owner", "edu_level", "experience", "nationality", 
                                        "yrs_operation", "size", "sales", "reluctant", 
                                        "bank_account", "loan", "source_loan", "collateral", "loan_application",
                                        "reason_no_app", "obstacle_finance") 

madaga_informal[madaga_informal == "don't know"] <- NA

# gender variable  as dichotomous (female = 1)
madaga_informal$female_owner <- droplevels(madaga_informal$female_owner)
levels(madaga_informal$female_owner) <- c(1, 0)

# education level 
madaga_informal$edu_level <- droplevels(madaga_informal$edu_level)
levels(madaga_informal$edu_level) 
```

    ## [1] "no education"                      
    ## [2] "primary school (complete or not)"  
    ## [3] "secondary school (complete or not)"
    ## [4] "vocational training"               
    ## [5] "some university training"

``` r
levels(madaga_informal$edu_level) <- c(1, 2, 3, 4, 5)

# nationality 
madaga_informal$nationality <- droplevels(madaga_informal$nationality)
levels(madaga_informal$nationality) <- 1

# years in operation

madaga_informal$yrs_operation <- 2009 - madaga_informal$yrs_operation

#sales
madaga_informal$sales[madaga_informal$sales == -9] <- NA
madaga_informal$sales <- madaga_informal$sales * 0.00052 # converting to $US

# being 'reluctant entrepreneur'
madaga_informal$reluctant[madaga_informal$reluctant == "other"] <- NA
madaga_informal$reluctant <- droplevels(madaga_informal$reluctant)
levels(madaga_informal$reluctant) 
```

    ## [1] "to take advantage of a business opportunity"                 
    ## [2] "alternative jobs or opportunities were absent or not satisfa"

``` r
levels(madaga_informal$reluctant) <- c(0, 1)

# bank account, separate account, loan, collateral, loan application

madaga_informal$bank_account <-  droplevels(madaga_informal$bank_account)
madaga_informal$loan <- droplevels(madaga_informal$loan)
madaga_informal$collateral <- droplevels(madaga_informal$collateral)
madaga_informal$loan_application <- droplevels(madaga_informal$loan_application)

levels(madaga_informal$bank_account) <- c(1,0)
levels(madaga_informal$loan) <- c(1,0)
levels(madaga_informal$collateral) <- c(1,0)
levels(madaga_informal$loan_application) <- c(1,0)
```

### Ivory Coast:

    ## [1] "no education"                      
    ## [2] "primary school (complete or not)"  
    ## [3] "secondary school (complete or not)"
    ## [4] "vocational training"               
    ## [5] "some university training"

    ## [1] "to take advantage of a business opportunity"                 
    ## [2] "alternative jobs or opportunities were absent or not satisfa"

### Mauritius:

    ## [1] "no education"                      
    ## [2] "primary school (complete or not)"  
    ## [3] "secondary school (complete or not)"
    ## [4] "vocational training"               
    ## [5] "some university training"

    ## [1] "to take advantage of a business opportunity"                 
    ## [2] "alternative jobs or opportunities were absent or not satisfa"

Loading Enterprise Survey Data (for Formal Firms) for 3 countries
-----------------------------------------------------------------

### Importing the Data

``` r
madaga_formal <- foreign::read.dta("Madagascar-2009--full data-1.dta")
ivory_formal <-  foreign::read.dta("Côte d'Ivoire-2009--full data-1.dta")
mauritius_formal <- foreign::read.dta("Mauritius_2009_idstd.dta")
```

### Selecting Variables

``` r
# Madagascar

madaga_formal$country <- "Madagascar"
madaga_formal$formality <- 1

madaga_formal <- madaga_formal %>%
  select(country, formality, b3b, afb7a, b7, afb4a, b5, l1, d2, b3e, 
         k6, k8, k9, k13, k16, k17, k30)

colnames(madaga_formal)[-c(1,2)] <- c("female_owner", "edu_level", "experience", "nationality", 
                                        "yrs_operation", "size", "sales", "reluctant", 
                                        "bank_account", "loan", "source_loan", "collateral", "loan_application",
                                        "reason_no_app", "obstacle_finance") 

madaga_formal[madaga_formal == "don\x92t know "] <- NA
madaga_formal[madaga_formal == "don't know"] <- NA
madaga_formal$nationality <- droplevels(madaga_formal$nationality)
levels(madaga_formal$nationality) <- c(0, 0, 0, 0, 0, 1)

# Ivory Coast

ivory_formal$country <- "Ivory Coast"
ivory_formal$formality <- 1


ivory_formal <- ivory_formal %>%
  select(country, formality, b3b, afb7a, b7, afb4a, b5, l1, d2, b3e, 
         k6, k8, k9, k13, k16, k17, k30)

colnames(ivory_formal)[-c(1,2)] <- c("female_owner", "edu_level", "experience", "nationality", 
                                        "yrs_operation", "size", "sales", "reluctant", 
                                        "bank_account", "loan", "source_loan", "collateral", "loan_application",
                                        "reason_no_app", "obstacle_finance") 

ivory_formal[ivory_formal == "don\x92t know "] <- NA
ivory_formal[ivory_formal == "don't know"] <- NA
ivory_formal$nationality <- droplevels(ivory_formal$nationality)
levels(ivory_formal$nationality) <- c(1, 0, 0, 0, 0, 0, 0)

# Mauritius 


mauritius_formal$country <- "Mauritius"
mauritius_formal$formality <- 1


mauritius_formal <- mauritius_formal %>%
  select(country, formality, b3b, afb7a, b7, afb4a, b5, l1, d2, b3e, 
         k6, k8, k9, k13, k16, k17, k30)

colnames(mauritius_formal)[-c(1,2)] <- c("female_owner", "edu_level", "experience", "nationality", 
                                        "yrs_operation", "size", "sales", "reluctant", 
                                        "bank_account", "loan", "source_loan", "collateral", "loan_application",
                                        "reason_no_app", "obstacle_finance") 

mauritius_formal[mauritius_formal == "don\x92t know "] <- NA
mauritius_formal[mauritius_formal == "don't know"] <- NA

mauritius_formal$nationality <- droplevels(mauritius_formal$nationality)
levels(mauritius_formal$nationality) <- c(0, 0, 0, 0, 0, 1)
```

### Sales Variable: Converting Local Currency to $ US

``` r
# dividing sales variable by 12 to get monthly sales
madaga_formal$sales[madaga_formal$sales == -9] <- NA
ivory_formal$sales[ivory_formal$sales == -9] <- NA
mauritius_formal$sales[mauritius_formal$sales == -9] <- NA

madaga_formal$sales <- madaga_formal$sales/12
ivory_formal$sales <- ivory_formal$sales/12
mauritius_formal$sales <- mauritius_formal$sales/12

# converting Mauritian Rupees to US dollars with 2009 exchange rate (surveys were conducted in 2009)
madaga_formal$sales <- madaga_formal$sales * 0.00052
mauritius_formal$sales <- mauritius_formal$sales * 0.031153 
ivory_formal$sales <- ivory_formal$sales *  0.0022 
```

### Row-Binding datasets and Cleaning:

``` r
formal <- rbind(madaga_formal, mauritius_formal, ivory_formal)

# gender variable  as dichotomous (female = 1)
formal$female_owner <- droplevels(formal$female_owner)
levels(formal$female_owner) <- c(1, 0)

# education level 
formal$edu_level <- droplevels(formal$edu_level)
levels(formal$edu_level) 
```

    ## [1] "no education"                                                
    ## [2] "primary school "                                             
    ## [3] "incomplete secondary school"                                 
    ## [4] "secondary school"                                            
    ## [5] "vocational training"                                         
    ## [6] "some university training"                                    
    ## [7] "graduate degree (ba, bsc etc.)"                              
    ## [8] "masters of business administration (mba) from university in "
    ## [9] "other post graduate degree (ph.d, masters) from university i"

``` r
levels(formal$edu_level) <- c(1, 2, 3, 4, 4, 5, 6, 7, 7)

# years in operation

formal$yrs_operation <- 2009 - formal$yrs_operation

# being 'reluctant entrepreneur'
formal$reluctant[formal$reluctant == "don\x92t know"] <- NA
formal$reluctant <- droplevels(formal$reluctant)
levels(formal$reluctant) 
```

    ## [1] "to take advantage of a business opportunity"                 
    ## [2] "alternative jobs or opportunities were absent or not satisfa"

``` r
levels(formal$reluctant) <- c(0, 1)

# bank account, separate account, loan, collateral, loan application

formal$bank_account <-  droplevels(formal$bank_account)
formal$loan <- droplevels(formal$loan)
formal$collateral <- droplevels(formal$collateral)
formal$loan_application <- droplevels(formal$loan_application)

levels(formal$bank_account) <- c(1,0)
levels(formal$loan) <- c(1,0)
levels(formal$collateral) <- c(1,0)
levels(formal$loan_application) <- c(1,0)
```

### Binding datasets formal and informal

``` r
regression <- rbind(madaga_informal, mauritius_informal, ivory_informal, formal)


# transforming finance as an obstacle into a dichotomous variable:
 regression$obstacle_finance[regression$obstacle_finance == "does not apply"] <- NA
regression$obstacle_finance <- droplevels(regression$obstacle_finance)
levels(regression$obstacle_finance)
```

    ## [1] "no obstacle"          "minor obstacle"       "moderate obstacle"   
    ## [4] "major obstacle"       "very severe obstacle"

``` r
regression$obstacle_finance5 <- regression$obstacle_finance
levels(regression$obstacle_finance) <- c(0, 0, 1, 1, 1)
levels(regression$obstacle_finance5) <- c(1, 2, 3, 4, 5)
```

Burkina Faso, Cape Verde
------------------------

### Informal Survey

``` r
burkina_informal$size <- burkina_informal$l1a + burkina_informal$l1b
burkina_informal$nationality <- NA
burkina_informal$obstacle_finance <- NA
burkina_informal <- burkina_informal %>%
  select(country, formality, b3, b21, b11, nationality, b12, size, d4, afb6,
         k10, k12, k13, k16, k17, k18, obstacle_finance)

colnames(burkina_informal)[-c(1,2)] <- c("female_owner", "edu_level", "experience", "nationality", 
                                        "yrs_operation", "size", "sales", "reluctant", 
                                        "bank_account", "loan", "source_loan", "collateral", "loan_application",
                                        "reason_no_app", "obstacle_finance") 

capeverde_informal$size <- capeverde_informal$l1a + capeverde_informal$l1b
capeverde_informal$nationality <- NA
capeverde_informal$obstacle_finance <- NA 
capeverde_informal <- capeverde_informal %>%
  select(country, formality, b3, b21, b11, nationality, b12, size, d4, afb6,
         k10, k12, k13, k16, k17, k18, obstacle_finance)

colnames(capeverde_informal)[-c(1,2)] <- c("female_owner", "edu_level", "experience", "nationality", 
                                        "yrs_operation", "size", "sales", "reluctant", 
                                        "bank_account", "loan", "source_loan", "collateral", "loan_application",
                                        "reason_no_app", "obstacle_finance") 
```

### Enterprise Survey

``` r
burkina_formal <- foreign::read.dta("Burkina Faso-2009--full data-.dta")
capeverde_formal <- foreign::read.dta("Cape Verde-2009-Indicator-full data-.dta")

# Burkina Faso: 

burkina_formal$country <- "Burkina Faso"
burkina_formal$formality <- 1


burkina_formal <- burkina_formal %>%
  select(country, formality, b3b, afb7a, b7, afb4a, b5, l1, d2, b3e, 
         k6, k8, k9, k13, k16, k17, k30)

colnames(burkina_formal)[-c(1,2)] <- c("female_owner", "edu_level", "experience", "nationality", 
                                        "yrs_operation", "size", "sales", "reluctant", 
                                        "bank_account", "loan", "source_loan", "collateral", "loan_application",
                                        "reason_no_app", "obstacle_finance") 

burkina_formal[burkina_formal == "don\x92t know "] <- NA
burkina_formal[burkina_formal == "don't know"] <- NA
burkina_formal[burkina_formal == "do not know"] <- NA
burkina_formal$nationality <- droplevels(burkina_formal$nationality)
levels(burkina_formal$nationality) <- c(1, 0, 0, 0, 0)

# dividing sales by 12 to get montly sales 
burkina_formal$sales <- burkina_formal$sales/12
burkina_formal$sales[burkina_formal$sales == -8] <- NA
burkina_formal$sales[burkina_formal$sales == -9] <- NA

# Cape Verde

capeverde_formal$country <- "Cape Verde"
capeverde_formal$formality <- 1


capeverde_formal <- capeverde_formal %>%
  select(country, formality, b3b, AFb7a, b7, AFb4a, b5, l1, d2, b3e, 
         k6, k8, k9, k13, k16, k17, k30)

colnames(capeverde_formal)[-c(1,2)] <- c("female_owner", "edu_level", "experience", "nationality", 
                                        "yrs_operation", "size", "sales", "reluctant", 
                                        "bank_account", "loan", "source_loan", "collateral", "loan_application",
                                        "reason_no_app", "obstacle_finance") 

capeverde_formal[capeverde_formal == "don\x92t know "] <- NA
capeverde_formal[capeverde_formal == "don't know"] <- NA
capeverde_formal[capeverde_formal == "do not know"] <- NA
capeverde_formal$nationality <- droplevels(capeverde_formal$nationality)
levels(capeverde_formal$nationality) <- c(1, 0, 0, 0, 0)

# dividing sales by 12 to get montly sales 
capeverde_formal$sales <- capeverde_formal$sales/12
capeverde_formal$sales[capeverde_formal$sales == -8] <- NA
capeverde_formal$sales[capeverde_formal$sales == -9] <- NA
```

### Binding Datasets:

    ##  [1] "no education"                            
    ##  [2] "primary school (complete or not)"        
    ##  [3] "secondary school (complete or not)"      
    ##  [4] "vocational training"                     
    ##  [5] "university training (complete or not)"   
    ##  [6] "some university training"                
    ##  [7] "graduate degree"                         
    ##  [8] "no education                            "
    ##  [9] "primary school (complete or not)        "
    ## [10] "secondary school (complete or not)      "
    ## [11] "vocational training                     "
    ## [12] "some university training                "
    ## [13] "graduate degree                         "

    ## [1] "wanted to have his own business       "               
    ## [2] "could not find another job            "               
    ## [3] "job at another business was terminated"               
    ## [4] "to take advantage of a business opportunity"          
    ## [5] "jobs or opportunities were absent or not satisfactory"

``` r
regression <- rbind(regression, regression2)
regression$sales <- round(regression$sales)
```

### Formal Datasets Cleaning

``` r
# turning factors into numeric variables 
regression$bank_account <- as.numeric(as.character(regression$bank_account))
regression$edu_level <- as.numeric(as.character(regression$edu_level))
regression$experience <- as.numeric(as.character(regression$experience))
regression$edu_level <- as.numeric(as.character(regression$edu_level))
regression$female_owner <- as.numeric(as.character(regression$female_owner))
regression$reluctant <- as.numeric(as.character(regression$reluctant))
regression$obstacle_finance <- as.numeric(as.character(regression$obstacle_finance))
regression$obstacle_finance5 <- as.numeric(as.character(regression$obstacle_finance5))
```

Linear Regression Models:
-------------------------

#### Simple Linear Model without Controls:

``` r
lm(bank_account ~ formality, data = regression) %>% summary()
```

    ## 
    ## Call:
    ## lm(formula = bank_account ~ formality, data = regression)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.8897  0.1103  0.1103  0.1103  0.6202 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.37981    0.01455   26.10   <2e-16 ***
    ## formality    0.50990    0.01677   30.41   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3635 on 2526 degrees of freedom
    ##   (28 observations deleted due to missingness)
    ## Multiple R-squared:  0.268,  Adjusted R-squared:  0.2677 
    ## F-statistic: 924.6 on 1 and 2526 DF,  p-value: < 2.2e-16

#### Simple Linear Models with Fixed Effects:

``` r
lm(bank_account ~ formality + factor(country) - 1, data = regression) %>% summary()
```

    ## 
    ## Call:
    ## lm(formula = bank_account ~ formality + factor(country) - 1, 
    ##     data = regression)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.03459 -0.03459  0.04409  0.13519  0.78141 
    ## 
    ## Coefficients:
    ##                             Estimate Std. Error t value Pr(>|t|)    
    ## formality                    0.52497    0.01635   32.10   <2e-16 ***
    ## factor(country)Burkina Faso  0.43094    0.01988   21.68   <2e-16 ***
    ## factor(country)Cape Verde    0.38539    0.02258   17.07   <2e-16 ***
    ## factor(country)Ivory Coast   0.21859    0.01923   11.37   <2e-16 ***
    ## factor(country)Madagascar    0.33984    0.01935   17.56   <2e-16 ***
    ## factor(country)Mauritius     0.50961    0.01951   26.13   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3487 on 2522 degrees of freedom
    ##   (28 observations deleted due to missingness)
    ## Multiple R-squared:  0.8412, Adjusted R-squared:  0.8408 
    ## F-statistic:  2227 on 6 and 2522 DF,  p-value: < 2.2e-16

#### Linear Model with Controls:

``` r
lm(bank_account ~ formality + female_owner + size + sales + edu_level +
     experience + reluctant + yrs_operation, data = regression) %>% summary()
```

    ## 
    ## Call:
    ## lm(formula = bank_account ~ formality + female_owner + size + 
    ##     sales + edu_level + experience + reluctant + yrs_operation, 
    ##     data = regression)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.08388 -0.30827  0.03171  0.15229  0.74850 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    3.315e-01  3.227e-02  10.273  < 2e-16 ***
    ## formality      4.449e-01  2.689e-02  16.543  < 2e-16 ***
    ## female_owner  -7.842e-02  2.254e-02  -3.480  0.00052 ***
    ## size          -4.432e-06  8.669e-05  -0.051  0.95923    
    ## sales          1.666e-10  1.274e-09   0.131  0.89601    
    ## edu_level      2.395e-02  7.390e-03   3.241  0.00122 ** 
    ## experience     4.813e-03  1.008e-03   4.776 2.02e-06 ***
    ## reluctant     -7.824e-02  2.790e-02  -2.804  0.00512 ** 
    ## yrs_operation  5.794e-06  6.028e-05   0.096  0.92344    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3598 on 1166 degrees of freedom
    ##   (1381 observations deleted due to missingness)
    ## Multiple R-squared:  0.372,  Adjusted R-squared:  0.3677 
    ## F-statistic: 86.34 on 8 and 1166 DF,  p-value: < 2.2e-16

#### Linear Model with Controls and (Country) Fixed Effects:

``` r
lm(bank_account ~ formality + female_owner + size + sales + edu_level +
     experience + reluctant + yrs_operation + factor(country) - 1, data = regression) %>% summary()
```

    ## 
    ## Call:
    ## lm(formula = bank_account ~ formality + female_owner + size + 
    ##     sales + edu_level + experience + reluctant + yrs_operation + 
    ##     factor(country) - 1, data = regression)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.11507 -0.21262  0.01834  0.19911  0.84746 
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## formality                    4.354e-01  2.633e-02  16.535  < 2e-16 ***
    ## female_owner                -4.104e-02  2.208e-02  -1.858   0.0634 .  
    ## size                         3.091e-06  8.271e-05   0.037   0.9702    
    ## sales                       -4.065e-10  1.220e-09  -0.333   0.7391    
    ## edu_level                    4.352e-02  7.490e-03   5.811 8.03e-09 ***
    ## experience                   2.506e-03  1.004e-03   2.496   0.0127 *  
    ## reluctant                   -6.005e-02  2.670e-02  -2.249   0.0247 *  
    ## yrs_operation               -4.559e-05  5.825e-05  -0.783   0.4340    
    ## factor(country)Burkina Faso  3.072e-01  3.475e-02   8.841  < 2e-16 ***
    ## factor(country)Cape Verde    3.823e-01  4.018e-02   9.514  < 2e-16 ***
    ## factor(country)Ivory Coast   2.045e-01  3.815e-02   5.360 1.00e-07 ***
    ## factor(country)Madagascar    1.207e-01  4.030e-02   2.995   0.0028 ** 
    ## factor(country)Mauritius     4.310e-01  3.755e-02  11.478  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.343 on 1162 degrees of freedom
    ##   (1381 observations deleted due to missingness)
    ## Multiple R-squared:  0.8369, Adjusted R-squared:  0.835 
    ## F-statistic: 458.5 on 13 and 1162 DF,  p-value: < 2.2e-16

``` r
# logit model
glm(bank_account ~ formality, family = binomial(link = "logit"), data = regression) %>% summary()
```

    ## 
    ## Call:
    ## glm(formula = bank_account ~ formality, family = binomial(link = "logit"), 
    ##     data = regression)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.0998   0.4834   0.4834   0.4834   1.3915  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -0.49036    0.08248  -5.945 2.76e-09 ***
    ## formality    2.57810    0.11025  23.384  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2763.6  on 2527  degrees of freedom
    ## Residual deviance: 2150.5  on 2526  degrees of freedom
    ##   (28 observations deleted due to missingness)
    ## AIC: 2154.5
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
# need to expand logit model (+ interpreation log of odds ratio)
```

### Test for Multicollinearity Explanatory Variables:

``` r
# visualizing correlation between explanatory variables
X <- regression[, c(2,3,4,5,7,8,9,10)]

ggpairs(X)
```

![](pods_coding_git_files/figure-markdown_github/unnamed-chunk-37-1.png)

``` r
# Farrar – Glauber Test 
mctest::imcdiag(X, regression$bank_account)
```

    ## 
    ## Call:
    ## mctest::imcdiag(x = X, y = regression$bank_account)
    ## 
    ## 
    ## All Individual Multicollinearity Diagnostics Result
    ## 
    ##                  VIF    TOL      Wi       Fi Leamer   CVIF Klein
    ## formality     1.5819 0.6322 97.0087 113.2738 0.7951 2.8786     0
    ## female_owner  1.0508 0.9517  8.4689   9.8888 0.9755 1.9122     0
    ## edu_level     1.5371 0.6506 89.5503 104.5649 0.8066 2.7972     0
    ## experience    1.0802 0.9257 13.3718  15.6138 0.9622 1.9657     0
    ## yrs_operation 1.0127 0.9874  2.1202   2.4757 0.9937 1.8429     0
    ## size          1.1134 0.8981 18.9070  22.0771 0.9477 2.0261     0
    ## sales         1.0069 0.9931  1.1581   1.3523 0.9965 1.8324     0
    ## reluctant     1.0826 0.9237 13.7657  16.0737 0.9611 1.9700     0
    ## 
    ## 1 --> COLLINEARITY is detected by the test 
    ## 0 --> COLLINEARITY is not detected by the test
    ## 
    ## yrs_operation , size , sales , coefficient(s) are non-significant may be due to multicollinearity
    ## 
    ## R-square of y on all x: 0.372 
    ## 
    ## * use method argument to check which regressors may be the reason of collinearity
    ## ===================================

Collinearity was not detected among explanatory variables by the Farrar – Glauber Test.

Adding Country Level Control Variables:
---------------------------------------

### GDP per capita PPP in 2009

source: World Bank Data <https://data.worldbank.org/indicator/NY.GDP.PCAP.PP.CD?end=2015&start=2009>

``` r
# burkina faso
regression$gdp_cap <- 1347.6

# cabo verde 
regression$gdp_cap[regression$country == "Cape Verde"] <- 5620.5

# ivory coast 
regression$gdp_cap[regression$country == "Ivory Coast"] <- 2612.1

# madagascar
regression$gdp_cap[regression$country == "Madagascar"] <- 1375.6

# mauritius
regression$gdp_cap[regression$country == "Mauritius"] <- 14815.8
```

### Domestic Credit to Private Sector(as % of GDP) in 2009

source: World Bank Data <https://data.worldbank.org/indicator/FS.AST.PRVT.GD.ZS?end=2017&start=2009>

``` r
# burkina faso 
regression$credit_gdp <- 16.5

# cabo verde 
regression$credit_gdp[regression$country == "Cape Verde"] <- 58.0

# ivory coast 
regression$credit_gdp[regression$country == "Ivory Coast"] <- 15.7

# madagascar
regression$credit_gdp[regression$country == "Madagascar"] <- 11.4

# mauritius
regression$credit_gdp[regression$country == "Mauritius"] <- 80.1
```

### Depth of credit information index (0 = low to 8 = high) in 2013 (no data for 2009)

Source: World Bank Data <https://data.worldbank.org/indicator/IC.CRD.INFO.XQ?view=chart>

``` r
# burkina faso 
regression$depth_credit <- 0

# cabo verde 
regression$depth_credit[regression$country == "Cape Verde"] <- 6

# ivory coast 
regression$depth_credit[regression$country == "Ivory Coast"] <- 0

# madagascar
regression$depth_credit[regression$country == "Madagascar"] <- 0

# mauritius
regression$depth_credit[regression$country == "Mauritius"] <- 7
```

### Strength of legal rights index (0 = weak to 12 = strong) in 2013 (no data for 2009)

Source: World Bank data <https://data.worldbank.org/indicator/IC.LGL.CRED.XQ?end=2017&start=2013>

``` r
# burkina faso 
regression$legal_rights <- 6

# cabo verde 
regression$legal_rights[regression$country == "Cape Verde"] <- 2

# ivory coast 
regression$legal_rights[regression$country == "Ivory Coast"] <- 6

# madagascar
regression$legal_rights[regression$country == "Madagascar"] <- 1

# mauritius
regression$legal_rights[regression$country == "Mauritius"] <- 6
```

Cleaning loan/loan application variable: (2nd dependent variable)
-----------------------------------------------------------------

``` r
levels(regression$loan_application)
```

    ## [1] "1" "0"

``` r
regression$loan_application <- as.numeric(as.character(regression$loan_application))
regression$loan <- as.numeric(as.character(regression$loan))

regression$y_loan <- regression$loan + regression$loan_application
regression$y_loan <- as.factor(regression$y_loan)
levels(regression$y_loan) <- c(0, 1, 1)

levels(regression$reason_no_app)[c(1,15,20, 23)] <- "don't know"
levels(regression$reason_no_app)[c(7, 18, 27)] <- "other"
levels(regression$reason_no_app)[c(3, 9, 16, 18, 21)] <- "application procedures are complex"
levels(regression$reason_no_app)[c(5, 10, 19)] <- "collateral requirements are too high"
levels(regression$reason_no_app)[c(6,11,14,19)] <- "did not think it would be approved because i am not registered"
levels(regression$reason_no_app)[c(9, 15)] <- "interest rates are not favorable"
levels(regression$reason_no_app)[c(10, 13, 15)] <- "size of loan and maturity are insufficient"
levels(regression$reason_no_app)[c(2, 8, 12, 13)] <- "no need for a loan"

# dataset will be used to implement regression when the dependent variable is: business has/applied for a loan 
# I'm excluding firms that explicitly stated they did not apply because they don't need a loan 
# the dependent variable will therefore be a better proxy of financial inclusion/constraints 

regression_y <- regression %>% filter(!(y_loan == 0 & reason_no_app == "no need for a loan"))
levels(regression_y$reason_no_app) 
```

    ##  [1] "don't know"                                                    
    ##  [2] "no need for a loan"                                            
    ##  [3] "application procedures are complex"                            
    ##  [4] "interest rates are too high"                                   
    ##  [5] "collateral requirements are too high"                          
    ##  [6] "did not think it would be approved because i am not registered"
    ##  [7] "other"                                                         
    ##  [8] "interest rates are not favorable"                              
    ##  [9] "size of loan and maturity are insufficient"                    
    ## [10] "it is necessary to make informal payments to get bank loans"

``` r
regression_y$reason_no_app <- droplevels(regression_y$reason_no_app)

# include test defensive coding to check if new dataset has been properly subsetted
regression_y[regression_y$y_loan == 0 & regression_y$reason_no_app == "no need for a loan"]
```

    ## data frame with 0 columns and 1840 rows

### Regression Tables:

#### Bank Account as Dependent Variable

``` r
lm1 <- lm(bank_account ~ formality, data = regression) 
lm2 <- lm(bank_account ~ formality + female_owner + size + sales + edu_level +
            experience + reluctant + yrs_operation, data = regression) 
lm3 <- lm(bank_account ~ formality + factor(country) - 1, data = regression)
lm4 <- lm(bank_account ~ formality + female_owner + size + sales + edu_level +
            experience + reluctant + yrs_operation + factor(country) - 1, data = regression)
lm5 <- lm(bank_account ~ formality + female_owner + size + sales + edu_level +
            experience + reluctant + yrs_operation + gdp_cap, data = regression)
lm6 <- lm(bank_account ~ formality + female_owner + size + sales + edu_level +
            experience + reluctant + yrs_operation + credit_gdp, data = regression)
lm7 <- lm(bank_account ~ formality + female_owner + size + sales + edu_level +
            experience + reluctant + yrs_operation + depth_credit, data = regression)
lm8 <- lm(bank_account ~ formality + female_owner + size + sales + edu_level +
            experience + reluctant + yrs_operation + legal_rights, data = regression)

stargazer::stargazer(lm1, lm2, lm3, lm4, lm5, lm6, lm7, lm8,
                     type = "html", dep.var.labels = c("Business Has a Bank Account ( = 1)"), 
                     covariate.labels = c("Formality", "Female Owner (=1)", "Size (number of employees)", "Sales (in $US)",
                                          "Owner's Education Level", "Owner's Experience (years)", 
                                          "Owner is a Reluctant Entrepreneur", "Firm Age", 
                                          "GDP/capita PPP", "Credit to Private Sector (% to GDP", 
                                          "Depth of Credit Information Index", "Strength of Legal Rights Index"), 
                     add.lines = list(c("Country Fixed Effects", "No", "No", "Yes", "Yes", "No", "No", "No", "No"),
                                      c("Firm Level Controls", "No", "Yes", "No", "Yes", "Yes", "Yes", "Yes", "Yes"), 
                                      c("Country Level Controls", "No","No", "No", "No", "Yes", "Yes","Yes", "Yes")),
                     omit = "country")
```

<table style="text-align:center">
<tr>
<td colspan="9" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td colspan="8">
<em>Dependent variable:</em>
</td>
</tr>
<tr>
<td>
</td>
<td colspan="8" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td colspan="8">
Business Has a Bank Account ( = 1)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(1)
</td>
<td>
(2)
</td>
<td>
(3)
</td>
<td>
(4)
</td>
<td>
(5)
</td>
<td>
(6)
</td>
<td>
(7)
</td>
<td>
(8)
</td>
</tr>
<tr>
<td colspan="9" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Formality
</td>
<td>
0.510<sup>\*\*\*</sup>
</td>
<td>
0.445<sup>\*\*\*</sup>
</td>
<td>
0.525<sup>\*\*\*</sup>
</td>
<td>
0.435<sup>\*\*\*</sup>
</td>
<td>
0.455<sup>\*\*\*</sup>
</td>
<td>
0.457<sup>\*\*\*</sup>
</td>
<td>
0.460<sup>\*\*\*</sup>
</td>
<td>
0.418<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.017)
</td>
<td>
(0.027)
</td>
<td>
(0.016)
</td>
<td>
(0.026)
</td>
<td>
(0.026)
</td>
<td>
(0.026)
</td>
<td>
(0.026)
</td>
<td>
(0.027)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Female Owner (=1)
</td>
<td>
</td>
<td>
-0.078<sup>\*\*\*</sup>
</td>
<td>
</td>
<td>
-0.041<sup>\*</sup>
</td>
<td>
-0.067<sup>\*\*\*</sup>
</td>
<td>
-0.065<sup>\*\*\*</sup>
</td>
<td>
-0.068<sup>\*\*\*</sup>
</td>
<td>
-0.060<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
(0.023)
</td>
<td>
</td>
<td>
(0.022)
</td>
<td>
(0.022)
</td>
<td>
(0.022)
</td>
<td>
(0.022)
</td>
<td>
(0.022)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Size (number of employees)
</td>
<td>
</td>
<td>
-0.00000
</td>
<td>
</td>
<td>
0.00000
</td>
<td>
-0.00001
</td>
<td>
-0.00001
</td>
<td>
-0.00001
</td>
<td>
0.00000
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
(0.0001)
</td>
<td>
</td>
<td>
(0.0001)
</td>
<td>
(0.0001)
</td>
<td>
(0.0001)
</td>
<td>
(0.0001)
</td>
<td>
(0.0001)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Sales (in US)
</td>
<td>
</td>
<td>
0.000
</td>
<td>
</td>
<td>
-0.000
</td>
<td>
0.000
</td>
<td>
-0.000
</td>
<td>
-0.000
</td>
<td>
0.000
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
(0.000)
</td>
<td>
</td>
<td>
(0.000)
</td>
<td>
(0.000)
</td>
<td>
(0.000)
</td>
<td>
(0.000)
</td>
<td>
(0.000)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Owner's Education Level
</td>
<td>
</td>
<td>
0.024<sup>\*\*\*</sup>
</td>
<td>
</td>
<td>
0.044<sup>\*\*\*</sup>
</td>
<td>
0.028<sup>\*\*\*</sup>
</td>
<td>
0.032<sup>\*\*\*</sup>
</td>
<td>
0.031<sup>\*\*\*</sup>
</td>
<td>
0.034<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
(0.007)
</td>
<td>
</td>
<td>
(0.007)
</td>
<td>
(0.007)
</td>
<td>
(0.007)
</td>
<td>
(0.007)
</td>
<td>
(0.007)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Owner's Experience (years)
</td>
<td>
</td>
<td>
0.005<sup>\*\*\*</sup>
</td>
<td>
</td>
<td>
0.003<sup>\*\*</sup>
</td>
<td>
0.003<sup>\*\*\*</sup>
</td>
<td>
0.003<sup>\*\*</sup>
</td>
<td>
0.003<sup>\*\*\*</sup>
</td>
<td>
0.005<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
(0.001)
</td>
<td>
</td>
<td>
(0.001)
</td>
<td>
(0.001)
</td>
<td>
(0.001)
</td>
<td>
(0.001)
</td>
<td>
(0.001)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Owner is a Reluctant Entrepreneur
</td>
<td>
</td>
<td>
-0.078<sup>\*\*\*</sup>
</td>
<td>
</td>
<td>
-0.060<sup>\*\*</sup>
</td>
<td>
-0.067<sup>\*\*</sup>
</td>
<td>
-0.061<sup>\*\*</sup>
</td>
<td>
-0.060<sup>\*\*</sup>
</td>
<td>
-0.081<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
(0.028)
</td>
<td>
</td>
<td>
(0.027)
</td>
<td>
(0.027)
</td>
<td>
(0.027)
</td>
<td>
(0.027)
</td>
<td>
(0.028)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Firm Age
</td>
<td>
</td>
<td>
0.00001
</td>
<td>
</td>
<td>
-0.00005
</td>
<td>
-0.00002
</td>
<td>
-0.00004
</td>
<td>
-0.00005
</td>
<td>
0.00003
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
(0.0001)
</td>
<td>
</td>
<td>
(0.0001)
</td>
<td>
(0.0001)
</td>
<td>
(0.0001)
</td>
<td>
(0.0001)
</td>
<td>
(0.0001)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
GDP/capita PPP
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
0.00002<sup>\*\*\*</sup>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
(0.00000)
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Credit to Private Sector (% to GDP
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
0.004<sup>\*\*\*</sup>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
(0.0004)
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Depth of Credit Information Index
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
0.030<sup>\*\*\*</sup>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
(0.003)
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Strength of Legal Rights Index
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
0.027<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
(0.005)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Constant
</td>
<td>
0.380<sup>\*\*\*</sup>
</td>
<td>
0.331<sup>\*\*\*</sup>
</td>
<td>
</td>
<td>
</td>
<td>
0.247<sup>\*\*\*</sup>
</td>
<td>
0.198<sup>\*\*\*</sup>
</td>
<td>
0.251<sup>\*\*\*</sup>
</td>
<td>
0.185<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.015)
</td>
<td>
(0.032)
</td>
<td>
</td>
<td>
</td>
<td>
(0.033)
</td>
<td>
(0.034)
</td>
<td>
(0.032)
</td>
<td>
(0.041)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td colspan="9" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Country Fixed Effects
</td>
<td>
No
</td>
<td>
No
</td>
<td>
Yes
</td>
<td>
Yes
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
</tr>
<tr>
<td style="text-align:left">
Firm Level Controls
</td>
<td>
No
</td>
<td>
Yes
</td>
<td>
No
</td>
<td>
Yes
</td>
<td>
Yes
</td>
<td>
Yes
</td>
<td>
Yes
</td>
<td>
Yes
</td>
</tr>
<tr>
<td style="text-align:left">
Country Level Controls
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
<td>
Yes
</td>
<td>
Yes
</td>
<td>
Yes
</td>
<td>
Yes
</td>
</tr>
<tr>
<td style="text-align:left">
Observations
</td>
<td>
2,528
</td>
<td>
1,175
</td>
<td>
2,528
</td>
<td>
1,175
</td>
<td>
1,175
</td>
<td>
1,175
</td>
<td>
1,175
</td>
<td>
1,175
</td>
</tr>
<tr>
<td style="text-align:left">
R<sup>2</sup>
</td>
<td>
0.268
</td>
<td>
0.372
</td>
<td>
0.841
</td>
<td>
0.837
</td>
<td>
0.408
</td>
<td>
0.417
</td>
<td>
0.413
</td>
<td>
0.390
</td>
</tr>
<tr>
<td style="text-align:left">
Adjusted R<sup>2</sup>
</td>
<td>
0.268
</td>
<td>
0.368
</td>
<td>
0.841
</td>
<td>
0.835
</td>
<td>
0.404
</td>
<td>
0.412
</td>
<td>
0.409
</td>
<td>
0.385
</td>
</tr>
<tr>
<td style="text-align:left">
Residual Std. Error
</td>
<td>
0.364 (df = 2526)
</td>
<td>
0.360 (df = 1166)
</td>
<td>
0.349 (df = 2522)
</td>
<td>
0.343 (df = 1162)
</td>
<td>
0.349 (df = 1165)
</td>
<td>
0.347 (df = 1165)
</td>
<td>
0.348 (df = 1165)
</td>
<td>
0.355 (df = 1165)
</td>
</tr>
<tr>
<td style="text-align:left">
F Statistic
</td>
<td>
924.608<sup>\*\*\*</sup> (df = 1; 2526)
</td>
<td>
86.340<sup>\*\*\*</sup> (df = 8; 1166)
</td>
<td>
2,226.912<sup>\*\*\*</sup> (df = 6; 2522)
</td>
<td>
458.530<sup>\*\*\*</sup> (df = 13; 1162)
</td>
<td>
89.325<sup>\*\*\*</sup> (df = 9; 1165)
</td>
<td>
92.418<sup>\*\*\*</sup> (df = 9; 1165)
</td>
<td>
91.254<sup>\*\*\*</sup> (df = 9; 1165)
</td>
<td>
82.672<sup>\*\*\*</sup> (df = 9; 1165)
</td>
</tr>
<tr>
<td colspan="9" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
<em>Note:</em>
</td>
<td colspan="8" style="text-align:right">
<sup>*</sup>p&lt;0.1; <sup>**</sup>p&lt;0.05; <sup>***</sup>p&lt;0.01
</td>
</tr>
</table>
#### Loan/Loan Application as Dependent Variable:

``` r
regression_y$y_loan <- as.numeric(as.character(regression_y$y_loan))

lm9 <- lm(y_loan ~ formality, data = regression_y) 
lm10 <- lm(y_loan ~ formality + female_owner + size + sales + edu_level +
            experience + reluctant + yrs_operation, data = regression_y) 
lm11 <- lm(y_loan ~ formality + factor(country) - 1, data = regression_y)
lm12 <- lm(y_loan ~ formality + female_owner + size + sales + edu_level +
            experience + reluctant + yrs_operation + factor(country) - 1, data = regression_y)
lm13 <- lm(y_loan ~ formality + female_owner + size + sales + edu_level +
            experience + reluctant + yrs_operation + gdp_cap, data = regression_y)
lm14 <- lm(y_loan ~ formality + female_owner + size + sales + edu_level +
            experience + reluctant + yrs_operation + credit_gdp, data = regression_y)
lm15 <- lm(y_loan ~ formality + female_owner + size + sales + edu_level +
            experience + reluctant + yrs_operation + depth_credit, data = regression_y)
lm16 <- lm(y_loan ~ formality + female_owner + size + sales + edu_level +
            experience + reluctant + yrs_operation + legal_rights, data = regression_y)

stargazer::stargazer(lm9, lm10, lm11, lm12, lm13, lm14, lm15, lm16,
                     type = "html", dep.var.labels = c("Business Has /(Applied for) a Loan ( = 1)"), 
                     covariate.labels = c("Formality", "Female Owner (=1)", "Size (number of employees)", "Sales (in $US)",
                                          "Owner's Education Level", "Owner's Experience (years)", 
                                          "Owner is a Reluctant Entrepreneur", "Firm Age", 
                                          "GDP/capita PPP", "Credit to Private Sector (% to GDP", 
                                          "Depth of Credit Information Index", "Strength of Legal Rights Index"), 
                     add.lines = list(c("Country Fixed Effects", "No", "No", "Yes", "Yes", "No", "No", "No", "No"),
                                      c("Firm Level Controls", "No", "Yes", "No", "Yes", "Yes", "Yes", "Yes", "Yes"), 
                                      c("Country Level Controls", "No","No", "No", "No", "Yes", "Yes","Yes", "Yes")),
                     omit = "country")
```

<table style="text-align:center">
<tr>
<td colspan="9" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td colspan="8">
<em>Dependent variable:</em>
</td>
</tr>
<tr>
<td>
</td>
<td colspan="8" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td colspan="8">
Business Has /(Applied for) a Loan ( = 1)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(1)
</td>
<td>
(2)
</td>
<td>
(3)
</td>
<td>
(4)
</td>
<td>
(5)
</td>
<td>
(6)
</td>
<td>
(7)
</td>
<td>
(8)
</td>
</tr>
<tr>
<td colspan="9" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Formality
</td>
<td>
0.279<sup>\*\*\*</sup>
</td>
<td>
0.173<sup>\*\*\*</sup>
</td>
<td>
0.291<sup>\*\*\*</sup>
</td>
<td>
0.182<sup>\*\*\*</sup>
</td>
<td>
0.165<sup>\*\*\*</sup>
</td>
<td>
0.167<sup>\*\*\*</sup>
</td>
<td>
0.174<sup>\*\*\*</sup>
</td>
<td>
0.160<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.027)
</td>
<td>
(0.040)
</td>
<td>
(0.025)
</td>
<td>
(0.039)
</td>
<td>
(0.038)
</td>
<td>
(0.038)
</td>
<td>
(0.038)
</td>
<td>
(0.041)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Female Owner (=1)
</td>
<td>
</td>
<td>
-0.030
</td>
<td>
</td>
<td>
0.017
</td>
<td>
-0.011
</td>
<td>
-0.006
</td>
<td>
-0.013
</td>
<td>
-0.022
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
(0.034)
</td>
<td>
</td>
<td>
(0.033)
</td>
<td>
(0.032)
</td>
<td>
(0.032)
</td>
<td>
(0.032)
</td>
<td>
(0.034)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Size (number of employees)
</td>
<td>
</td>
<td>
0.001<sup>\*\*\*</sup>
</td>
<td>
</td>
<td>
0.0004<sup>\*\*\*</sup>
</td>
<td>
0.0005<sup>\*\*\*</sup>
</td>
<td>
0.0005<sup>\*\*\*</sup>
</td>
<td>
0.0005<sup>\*\*\*</sup>
</td>
<td>
0.001<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
(0.0002)
</td>
<td>
</td>
<td>
(0.0001)
</td>
<td>
(0.0001)
</td>
<td>
(0.0001)
</td>
<td>
(0.0001)
</td>
<td>
(0.0002)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Sales (in US)
</td>
<td>
</td>
<td>
-0.000
</td>
<td>
</td>
<td>
0.000
</td>
<td>
-0.000
</td>
<td>
-0.000
</td>
<td>
-0.000
</td>
<td>
-0.000
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
(0.000)
</td>
<td>
</td>
<td>
(0.000)
</td>
<td>
(0.000)
</td>
<td>
(0.000)
</td>
<td>
(0.000)
</td>
<td>
(0.000)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Owner's Education Level
</td>
<td>
</td>
<td>
0.031<sup>\*\*\*</sup>
</td>
<td>
</td>
<td>
0.040<sup>\*\*\*</sup>
</td>
<td>
0.036<sup>\*\*\*</sup>
</td>
<td>
0.041<sup>\*\*\*</sup>
</td>
<td>
0.039<sup>\*\*\*</sup>
</td>
<td>
0.036<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
(0.011)
</td>
<td>
</td>
<td>
(0.011)
</td>
<td>
(0.010)
</td>
<td>
(0.010)
</td>
<td>
(0.010)
</td>
<td>
(0.011)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Owner's Experience (years)
</td>
<td>
</td>
<td>
0.010<sup>\*\*\*</sup>
</td>
<td>
</td>
<td>
0.006<sup>\*\*\*</sup>
</td>
<td>
0.007<sup>\*\*\*</sup>
</td>
<td>
0.007<sup>\*\*\*</sup>
</td>
<td>
0.007<sup>\*\*\*</sup>
</td>
<td>
0.010<sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
(0.002)
</td>
<td>
</td>
<td>
(0.002)
</td>
<td>
(0.001)
</td>
<td>
(0.001)
</td>
<td>
(0.002)
</td>
<td>
(0.002)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Owner is a Reluctant Entrepreneur
</td>
<td>
</td>
<td>
0.001
</td>
<td>
</td>
<td>
0.029
</td>
<td>
0.020
</td>
<td>
0.028
</td>
<td>
0.030
</td>
<td>
0.0002
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
(0.042)
</td>
<td>
</td>
<td>
(0.039)
</td>
<td>
(0.040)
</td>
<td>
(0.040)
</td>
<td>
(0.040)
</td>
<td>
(0.042)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Firm Age
</td>
<td>
</td>
<td>
0.0001<sup>\*</sup>
</td>
<td>
</td>
<td>
0.0001
</td>
<td>
0.0001
</td>
<td>
0.0001
</td>
<td>
0.00004
</td>
<td>
0.0001<sup>\*</sup>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
(0.0001)
</td>
<td>
</td>
<td>
(0.0001)
</td>
<td>
(0.0001)
</td>
<td>
(0.0001)
</td>
<td>
(0.0001)
</td>
<td>
(0.0001)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
GDP/capita PPP
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
0.00003<sup>\*\*\*</sup>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
(0.00000)
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Credit to Private Sector (% to GDP
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
0.006<sup>\*\*\*</sup>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
(0.001)
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Depth of Credit Information Index
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
0.050<sup>\*\*\*</sup>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
(0.005)
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Strength of Legal Rights Index
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
0.011
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
(0.007)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td style="text-align:left">
Constant
</td>
<td>
0.214<sup>\*\*\*</sup>
</td>
<td>
0.063
</td>
<td>
</td>
<td>
</td>
<td>
-0.051
</td>
<td>
-0.116<sup>\*\*</sup>
</td>
<td>
-0.029
</td>
<td>
0.003
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
(0.024)
</td>
<td>
(0.047)
</td>
<td>
</td>
<td>
</td>
<td>
(0.047)
</td>
<td>
(0.048)
</td>
<td>
(0.046)
</td>
<td>
(0.062)
</td>
</tr>
<tr>
<td style="text-align:left">
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
<td>
</td>
</tr>
<tr>
<td colspan="9" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
Country Fixed Effects
</td>
<td>
No
</td>
<td>
No
</td>
<td>
Yes
</td>
<td>
Yes
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
</tr>
<tr>
<td style="text-align:left">
Firm Level Controls
</td>
<td>
No
</td>
<td>
Yes
</td>
<td>
No
</td>
<td>
Yes
</td>
<td>
Yes
</td>
<td>
Yes
</td>
<td>
Yes
</td>
<td>
Yes
</td>
</tr>
<tr>
<td style="text-align:left">
Country Level Controls
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
<td>
No
</td>
<td>
Yes
</td>
<td>
Yes
</td>
<td>
Yes
</td>
<td>
Yes
</td>
</tr>
<tr>
<td style="text-align:left">
Observations
</td>
<td>
1,822
</td>
<td>
852
</td>
<td>
1,822
</td>
<td>
852
</td>
<td>
852
</td>
<td>
852
</td>
<td>
852
</td>
<td>
852
</td>
</tr>
<tr>
<td style="text-align:left">
R<sup>2</sup>
</td>
<td>
0.055
</td>
<td>
0.171
</td>
<td>
0.566
</td>
<td>
0.593
</td>
<td>
0.252
</td>
<td>
0.261
</td>
<td>
0.257
</td>
<td>
0.174
</td>
</tr>
<tr>
<td style="text-align:left">
Adjusted R<sup>2</sup>
</td>
<td>
0.055
</td>
<td>
0.164
</td>
<td>
0.565
</td>
<td>
0.587
</td>
<td>
0.244
</td>
<td>
0.253
</td>
<td>
0.249
</td>
<td>
0.165
</td>
</tr>
<tr>
<td style="text-align:left">
Residual Std. Error
</td>
<td>
0.482 (df = 1820)
</td>
<td>
0.454 (df = 843)
</td>
<td>
0.433 (df = 1816)
</td>
<td>
0.425 (df = 839)
</td>
<td>
0.432 (df = 842)
</td>
<td>
0.429 (df = 842)
</td>
<td>
0.430 (df = 842)
</td>
<td>
0.454 (df = 842)
</td>
</tr>
<tr>
<td style="text-align:left">
F Statistic
</td>
<td>
106.124<sup>\*\*\*</sup> (df = 1; 1820)
</td>
<td>
21.808<sup>\*\*\*</sup> (df = 8; 843)
</td>
<td>
394.960<sup>\*\*\*</sup> (df = 6; 1816)
</td>
<td>
94.015<sup>\*\*\*</sup> (df = 13; 839)
</td>
<td>
31.508<sup>\*\*\*</sup> (df = 9; 842)
</td>
<td>
33.010<sup>\*\*\*</sup> (df = 9; 842)
</td>
<td>
32.422<sup>\*\*\*</sup> (df = 9; 842)
</td>
<td>
19.671<sup>\*\*\*</sup> (df = 9; 842)
</td>
</tr>
<tr>
<td colspan="9" style="border-bottom: 1px solid black">
</td>
</tr>
<tr>
<td style="text-align:left">
<em>Note:</em>
</td>
<td colspan="8" style="text-align:right">
<sup>*</sup>p&lt;0.1; <sup>**</sup>p&lt;0.05; <sup>***</sup>p&lt;0.01
</td>
</tr>
</table>
GLM: Logistic Regression Models:
--------------------------------
