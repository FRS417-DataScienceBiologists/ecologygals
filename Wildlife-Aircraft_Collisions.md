---
title: "Wildlife-Aircraft Collisions"
author: "Tricia"
date: "2/19/2019"
output: 
  html_document: 
    keep_md: yes
---



# Importing the Data and Removing the NAs/Unknowns

```r
aircraft_strikes <- readr::read_csv("~/Desktop/FAA_National_Wildlife_Aircraft_Strike_Database__Year_2000_and_after_.csv", na = c("", " ", "NA", "#N/A", "-999", "\\", "UNKNOWN", "UNKNOWN - SMALL", "UNKNOWN - MEDIUM", "UNKNOWN - LARGE", "UNK", "UNKNOWN BIRD", "UNKNOWN BIRD - SMALL", "UNKNOWN BIRD - MEDIUM", "UNKNOWN BIRD - LARGE"))
```

```
## Parsed with column specification:
## cols(
##   .default = col_character(),
##   `Height (feet)` = col_double(),
##   Speed = col_double(),
##   `Cost of Repairs` = col_double(),
##   `Other Costs` = col_double()
## )
```

```
## See spec(...) for full column specifications.
```

# Summary functions! Let's take a look at the data.


```r
names(aircraft_strikes)
```

```
##  [1] "Operator ID"         "Operator"            "Aircraft Type"      
##  [4] "Registration Number" "Airport"             "Runway"             
##  [7] "State"               "City"                "Date"               
## [10] "Height (feet)"       "Speed"               "Phase of Flight"    
## [13] "Sky"                 "Precipitation"       "Species"            
## [16] "Birds Seen"          "Birds Struck"        "Size"               
## [19] "Cost of Repairs"     "Other Costs"
```


## Load the libraries  

```r
library(tidyverse)
library(skimr)
library("RColorBrewer")
```

## View the data.

```r
glimpse(aircraft_strikes)
```

```
## Observations: 59,971
## Variables: 20
## $ `Operator ID`         <chr> "UAL", NA, "UAL", NA, "UAL", NA, NA, NA, "…
## $ Operator              <chr> "UNITED AIRLINES", NA, "UNITED AIRLINES", …
## $ `Aircraft Type`       <chr> "B-737-500", NA, "A-320", NA, "B-777-200",…
## $ `Registration Number` <chr> "N933UA", NA, "N453UA", NA, "N777UA", NA, …
## $ Airport               <chr> NA, "PAJN", NA, "KOMA", NA, "KCLE", "KAUS"…
## $ Runway                <chr> NA, "8/26", NA, "18", NA, "24", "17R/35L",…
## $ State                 <chr> NA, "AK", NA, "NE", NA, "OH", "TX", "MD", …
## $ City                  <chr> NA, "JUNEAU INTL ARPT", NA, "EPPLEY AIRFIE…
## $ Date                  <chr> "09/03/2004", "09/09/2004", "10/27/2004", …
## $ `Height (feet)`       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, 0, NA, NA,…
## $ Speed                 <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
## $ `Phase of Flight`     <chr> NA, NA, NA, NA, NA, NA, NA, NA, "Take-off …
## $ Sky                   <chr> NA, NA, NA, NA, NA, NA, NA, NA, "No Cloud"…
## $ Precipitation         <chr> NA, NA, NA, NA, NA, NA, NA, NA, "None", NA…
## $ Species               <chr> NA, "FOX SPARROW", NA, "KILLDEER", NA, "KI…
## $ `Birds Seen`          <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
## $ `Birds Struck`        <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1…
## $ Size                  <chr> NA, "Small", NA, "Small", NA, "Small", "Sm…
## $ `Cost of Repairs`     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
## $ `Other Costs`         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
```


## Which species is recorded as being hit the most?

```r
aircraft_strikes %>% 
  filter(Species!= "UNKNOWN") %>% 
  count(Species) %>% 
  arrange(desc(n)) 
```

```
## # A tibble: 483 x 2
##    Species               n
##    <chr>             <int>
##  1 MOURNING DOVE      2291
##  2 GULLS              2186
##  3 EUROPEAN STARLING  1427
##  4 AMERICAN KESTREL   1422
##  5 SPARROWS           1220
##  6 KILLDEER           1189
##  7 ROCK PIGEON        1045
##  8 HORNED LARK         832
##  9 BARN SWALLOW        753
## 10 RED-TAILED HAWK     732
## # … with 473 more rows
```

## On average, how fast were the aircrafts going when they collided with the wildlife?

```r
aircraft_strikes %>% 
  ggplot(aes(x=Speed)) +
  geom_histogram(fill="yellowgreen", alpha=.8, color="forestgreen")+
  labs(title = "Distribution of Speed")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```
## Warning: Removed 36894 rows containing non-finite values (stat_bin).
```

![](Wildlife-Aircraft_Collisions_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
## American kestrels and red-tailed hawks are two common birds of prey. Let's compare the average speeds of the aircrafts when they collided with these birds. 



```r
aircraft_strikes %>% 
  filter(Species=="AMERICAN KESTREL" | Species =="RED-TAILED HAWK") %>% 
  ggplot(aes(x=Species, y=Speed))+
  geom_boxplot(color="darkgray")+
  labs(title = "Speed of Collisions involving American Kestrels vs. Red-Tailed Hawks",
       x = "Species",
       y = "Speed")+
  theme(plot.title = element_text(size = rel(1.25)))
```

```
## Warning: Removed 1783 rows containing non-finite values (stat_boxplot).
```

![](Wildlife-Aircraft_Collisions_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
## Based on the boxplot, do you think these speeds are above average, average, or below average compared to other wildlife species?

These speeds lie near the peak of the distribution, suggesting that they are average.

## Let's see what the real average speed is and compare these speeds to the average.


```r
aircraft_strikes %>% 
  summarise(avgspeed=mean(Speed, na.rm=T))
```

```
## # A tibble: 1 x 1
##   avgspeed
##      <dbl>
## 1     143.
```

```r
aircraft_strikes %>% 
  filter(Species=="AMERICAN KESTREL") %>% 
  summarise(avgspeed_kestrel=mean(Speed, na.rm=T))
```

```
## # A tibble: 1 x 1
##   avgspeed_kestrel
##              <dbl>
## 1             107.
```


```r
aircraft_strikes %>% 
  filter(Species=="RED-TAILED HAWK") %>% 
  summarise(avgspeed_hawk=mean(Speed, na.rm=T))
```

```
## # A tibble: 1 x 1
##   avgspeed_hawk
##           <dbl>
## 1          121.
```

These average speeds of the aircrafts that collided with American kestrels and red-tailed hawks are somewhat below average.

## Which airports have had the most collisions with which species?

```r
species <- 
aircraft_strikes %>% 
  filter(!Size=="NA", 
         !Species=="UNKNOWN BIRD - SMALL",
         !Species=="NA",
         !Species=="UNKNOWN BIRD - MEDIUM",
         !Species=="UNKNOWN BIRD - LARGE") %>% 
  group_by(Airport, City, Speed, Sky, Species) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count))
species
```

```
## # A tibble: 13,871 x 6
## # Groups:   Airport, City, Speed, Sky [6,668]
##    Airport City                Speed Sky   Species               count
##    <chr>   <chr>               <dbl> <chr> <chr>                 <int>
##  1 KDEN    DENVER INTL AIRPORT    NA <NA>  HORNED LARK             206
##  2 KPHX    PHOENIX SKY HARBOR     NA <NA>  MOURNING DOVE           192
##  3 KMDW    CHICAGO MIDWAY         NA <NA>  AMERICAN KESTREL        144
##  4 KDFW    DALLAS FORT WORTH      NA <NA>  MOURNING DOVE           114
##  5 KJFK    JOHN F KENNEDY INTL    NA <NA>  HERRING GULL            113
##  6 PHNL    HONOLULU INTL ARPT     NA <NA>  PACIFIC GOLDEN-PLOVER   103
##  7 KPDX    PORTLAND INTL (OR)     NA <NA>  AMERICAN KESTREL         87
##  8 KTEB    TETERBORO AIRPORT      NA <NA>  AMERICAN KESTREL         81
##  9 KMCI    KANSAS CITY INTL       NA <NA>  HORNED LARK              74
## 10 KPHX    PHOENIX SKY HARBOR     NA <NA>  ROCK PIGEON              71
## # … with 13,861 more rows
```

## Between the American kestrel and the red-tailed hawk, which species was involved in more collisions?

```r
species %>% 
  filter(Species=="AMERICAN KESTREL" | Species=="RED-TAILED HAWK") %>% 
  ggplot(aes(x=Species, y=count))+
  geom_boxplot(color="darkgray")+
  labs(title = "Collisions of American Kestrels vs. Red-Tailed Hawks",
       x = "Species",
       y = "Count")+
  theme(plot.title = element_text(size = rel(1.25)))+
  theme_light()+
  scale_y_log10()
```

![](Wildlife-Aircraft_Collisions_files/figure-html/unnamed-chunk-12-1.png)<!-- -->


## Let's also look at the speeds of the aircrafts when they struck geese versus the red-tailed hawk, which are examples of waterfowl and birds of prey, respectively. In general, which species encountered faster moving aircrafts? 

```r
species %>% 
  filter(Species=="GEESE" | Species=="RED-TAILED HAWK") %>% 
  ggplot(aes(x=Species, y=Speed))+
  geom_boxplot(fill="lavender")+
  labs(title = "Collisions of Geese vs. Red-Tailed Hawks",
       x = "Species",
       y = "Speed")+
  theme(plot.title = element_text(size = rel(1.25)))+
  scale_fill_brewer(palette = "BrBG")+
  theme_light()
```

```
## Warning: Removed 200 rows containing non-finite values (stat_boxplot).
```

![](Wildlife-Aircraft_Collisions_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

##Did the sky conditions affect the speeds at which they collided with the aircrafts?


```r
species %>% 
  filter(Species=="GEESE" | Species=="RED-TAILED HAWK") %>% 
  ggplot(aes(x=Species, y=Speed, fill=Sky))+
  geom_boxplot()+
  labs(title = "Collisions of Geese vs. Red-Tailed Hawks",
       x = "Species",
       y = "Speed")+
  theme(plot.title = element_text(size = rel(1.25)))+
  scale_fill_brewer(palette = "BrBG")+
  theme_light()
```

```
## Warning: Removed 200 rows containing non-finite values (stat_boxplot).
```

![](Wildlife-Aircraft_Collisions_files/figure-html/unnamed-chunk-14-1.png)<!-- -->



