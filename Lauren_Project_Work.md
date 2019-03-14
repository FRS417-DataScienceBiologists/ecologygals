---
title: "Project"
author: "Lauren Alimento"
date: "2/25/2019"
output: 
  html_document: 
    fig_caption: yes
    keep_md: yes
keep_md: true
---

```r
library(tidyverse)
```

```
## Warning: package 'tidyverse' was built under R version 3.4.2
```

```
## ── Attaching packages ────────────────────────────────── tidyverse 1.2.1 ──
```

```
## ✔ ggplot2 3.1.0     ✔ purrr   0.2.5
## ✔ tibble  1.4.2     ✔ dplyr   0.7.8
## ✔ tidyr   0.8.2     ✔ stringr 1.3.1
## ✔ readr   1.3.1     ✔ forcats 0.3.0
```

```
## Warning: package 'ggplot2' was built under R version 3.4.4
```

```
## Warning: package 'tibble' was built under R version 3.4.3
```

```
## Warning: package 'tidyr' was built under R version 3.4.4
```

```
## Warning: package 'readr' was built under R version 3.4.4
```

```
## Warning: package 'purrr' was built under R version 3.4.4
```

```
## Warning: package 'dplyr' was built under R version 3.4.4
```

```
## Warning: package 'stringr' was built under R version 3.4.4
```

```
## Warning: package 'forcats' was built under R version 3.4.3
```

```
## ── Conflicts ───────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```


```r
aircraft_strikes <-
  readr::read_csv(file = "~/Desktop/FRS 417/ecologygals-master/FAA_National_Wildlife_Aircraft_Strike_Database__Year_2000_and_after_.csv")
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

```r
glimpse(aircraft_strikes)
```

```
## Observations: 59,971
## Variables: 20
## $ `Operator ID`         <chr> "UAL", "UNK", "UAL", "UNK", "UAL", "UNK"...
## $ Operator              <chr> "UNITED AIRLINES", "UNKNOWN", "UNITED AI...
## $ `Aircraft Type`       <chr> "B-737-500", "UNKNOWN", "A-320", "UNKNOW...
## $ `Registration Number` <chr> "N933UA", NA, "N453UA", NA, "N777UA", NA...
## $ Airport               <chr> "UNK", "PAJN", "UNK", "KOMA", "UNK", "KC...
## $ Runway                <chr> NA, "8/26", NA, "18", NA, "24", "17R/35L...
## $ State                 <chr> NA, "AK", NA, "NE", NA, "OH", "TX", "MD"...
## $ City                  <chr> "UNKNOWN", "JUNEAU INTL ARPT", "UNKNOWN"...
## $ Date                  <chr> "09/03/2004", "09/09/2004", "10/27/2004"...
## $ `Height (feet)`       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, 0, NA, N...
## $ Speed                 <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
## $ `Phase of Flight`     <chr> NA, NA, NA, NA, NA, NA, NA, NA, "Take-of...
## $ Sky                   <chr> NA, NA, NA, NA, NA, NA, NA, NA, "No Clou...
## $ Precipitation         <chr> NA, NA, NA, NA, NA, NA, NA, NA, "None", ...
## $ Species               <chr> "UNKNOWN BIRD", "FOX SPARROW", "UNKNOWN ...
## $ `Birds Seen`          <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
## $ `Birds Struck`        <chr> "1", "1", "1", "1", "1", "1", "1", "1", ...
## $ Size                  <chr> NA, "Small", NA, "Small", NA, "Small", "...
## $ `Cost of Repairs`     <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
## $ `Other Costs`         <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, ...
```


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


```r
gather(aircraft_strikes)
```

```
## # A tibble: 1,199,420 x 2
##    key         value
##    <chr>       <chr>
##  1 Operator ID UAL  
##  2 Operator ID UNK  
##  3 Operator ID UAL  
##  4 Operator ID UNK  
##  5 Operator ID UAL  
##  6 Operator ID UNK  
##  7 Operator ID UNK  
##  8 Operator ID UNK  
##  9 Operator ID QXE  
## 10 Operator ID UNK  
## # ... with 1,199,410 more rows
```



```r
aircraft_strikes_tidy <-
  aircraft_strikes %>% 
  na_if("UNKNOWN") %>% 
  na_if("UNKNOWN BIRD") %>% 
  na_if("UNK") %>% 
  na_if("UNKNOWN BIRD - SMALL") %>% 
  na_if("UNKNOWN BIRD - MEDIUM") %>% 
  na_if("UNKNOWN BIRD - SMALL")
aircraft_strikes_tidy
```

```
## # A tibble: 59,971 x 20
##    `Operator ID` Operator `Aircraft Type` `Registration N… Airport Runway
##    <chr>         <chr>    <chr>           <chr>            <chr>   <chr> 
##  1 UAL           UNITED … B-737-500       N933UA           <NA>    <NA>  
##  2 <NA>          <NA>     <NA>            <NA>             PAJN    8/26  
##  3 UAL           UNITED … A-320           N453UA           <NA>    <NA>  
##  4 <NA>          <NA>     <NA>            <NA>             KOMA    18    
##  5 UAL           UNITED … B-777-200       N777UA           <NA>    <NA>  
##  6 <NA>          <NA>     <NA>            <NA>             KCLE    24    
##  7 <NA>          <NA>     <NA>            <NA>             KAUS    17R/3…
##  8 <NA>          <NA>     <NA>            <NA>             KBWI    10/28 
##  9 QXE           HORIZON… CL-RJ700        <NA>             KOMA    14R   
## 10 <NA>          <NA>     <NA>            <NA>             KMDW    31C   
## # ... with 59,961 more rows, and 14 more variables: State <chr>,
## #   City <chr>, Date <chr>, `Height (feet)` <dbl>, Speed <dbl>, `Phase of
## #   Flight` <chr>, Sky <chr>, Precipitation <chr>, Species <chr>, `Birds
## #   Seen` <chr>, `Birds Struck` <chr>, Size <chr>, `Cost of
## #   Repairs` <dbl>, `Other Costs` <dbl>
```



```r
names(aircraft_strikes_tidy)
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


```r
strikes_by_airline <- 
aircraft_strikes_tidy %>% 
  group_by(Operator) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count))
```

```
## Warning: package 'bindrcpp' was built under R version 3.4.4
```

```r
strikes_by_airline
```

```
## # A tibble: 317 x 2
##    Operator           count
##    <chr>              <int>
##  1 <NA>               12476
##  2 UNITED AIRLINES     7833
##  3 BUSINESS            4931
##  4 SOUTHWEST AIRLINES  4192
##  5 FEDEX EXPRESS       3025
##  6 AMERICAN AIRLINES   2585
##  7 UPS AIRLINES        2252
##  8 DELTA AIR LINES     1905
##  9 NORTHWEST AIRLINES  1675
## 10 US AIRWAYS          1671
## # ... with 307 more rows
```



```r
strikes_by_airline %>%
  top_n(20, count) %>% 
  ggplot(aes(x=reorder(Operator, count), y=count,fill=Operator))+
  geom_col()+
  coord_flip()+
  labs(title="Strikesm by Airline",
       x="Airlines",
       y="Number of Strikes")
```

![](Lauren_Project_Work_files/figure-html/unnamed-chunk-9-1.png)<!-- -->


```r
aircraft_strikes_tidy %>% 
  count(Species) %>% 
  arrange(desc(n))
```

```
## # A tibble: 485 x 2
##    Species                  n
##    <chr>                <int>
##  1 <NA>                 31848
##  2 MOURNING DOVE         2291
##  3 GULLS                 2186
##  4 EUROPEAN STARLING     1427
##  5 AMERICAN KESTREL      1422
##  6 SPARROWS              1220
##  7 KILLDEER              1189
##  8 UNKNOWN BIRD - LARGE  1076
##  9 ROCK PIGEON           1045
## 10 HORNED LARK            832
## # ... with 475 more rows
```



```r
aircraft_strikes_tidy %>% 
  filter(!`Birds Struck`>1) %>% 
  ggplot(aes(x=Size, y=as.numeric(`Birds Struck`)))+
  geom_col(fill="lightseagreen",alpha=0.5)+
  labs(title="Birds Struck by Size",
       x="Bird Size",
       y="Number of Birds Struck")
```

![](Lauren_Project_Work_files/figure-html/unnamed-chunk-11-1.png)<!-- -->


```r
aircraft_strikes_tidy %>%
  filter(!Size=="NA") %>% 
  group_by(Size) %>% 
  summarize(count=n()) %>% 
  ggplot(aes(x=Size, y=count))+
  geom_col(fill="lightseagreen",alpha=0.5)+
  labs(title="Number of Birds by Size",
       x="Bird Size",
       y="Number of Birds")
```

![](Lauren_Project_Work_files/figure-html/unnamed-chunk-12-1.png)<!-- -->


```r
airport_species <- 
aircraft_strikes_tidy %>% 
  group_by(Airport, Species) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count))
airport_species
```

```
## # A tibble: 8,509 x 3
## # Groups:   Airport [1,342]
##    Airport Species count
##    <chr>   <chr>   <int>
##  1 <NA>    <NA>     7281
##  2 KDEN    <NA>     1267
##  3 KMEM    <NA>     1110
##  4 KORD    <NA>      841
##  5 KSMF    <NA>      655
##  6 KDFW    <NA>      642
##  7 KSDF    <NA>      614
##  8 KIAD    <NA>      537
##  9 KSFO    <NA>      399
## 10 KDTW    <NA>      333
## # ... with 8,499 more rows
```


```r
strikes_by_airport <- 
aircraft_strikes_tidy %>% 
  group_by(Airport) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count))
strikes_by_airport
```

```
## # A tibble: 1,342 x 2
##    Airport count
##    <chr>   <int>
##  1 <NA>     8170
##  2 KDEN     2090
##  3 KDFW     1529
##  4 KMEM     1316
##  5 KORD     1305
##  6 KJFK     1199
##  7 KSMF      944
##  8 KPDX      829
##  9 KIAD      806
## 10 KMCI      804
## # ... with 1,332 more rows
```


```r
strikes_by_airport %>%
  top_n(20, count) %>% 
  ggplot(aes(x=reorder(Airport, count), y=count, fill=Airport))+
  geom_col()+
  coord_flip()+
  labs(title="Strikes by Airport",
       y="Number of Strikes",
       x="Airport")
```

![](Lauren_Project_Work_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

```r
strikes_by_state <- 
aircraft_strikes_tidy %>% 
  group_by(State) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count))
strikes_by_state
```

```
## # A tibble: 63 x 2
##    State count
##    <chr> <int>
##  1 <NA>   8611
##  2 CA     4857
##  3 TX     4041
##  4 FL     3174
##  5 NY     3025
##  6 IL     2451
##  7 CO     2430
##  8 OH     2109
##  9 TN     1914
## 10 MI     1654
## # ... with 53 more rows
```

```r
strikes_by_state %>%
  top_n(20, count) %>% 
  ggplot(aes(x=reorder(State, count), y=count, fill=State))+
  geom_col()+
  coord_flip()+
  labs(title="Strikes by State",
       x="State",
       y="Number of Strikes")
```

![](Lauren_Project_Work_files/figure-html/unnamed-chunk-17-1.png)<!-- -->


```r
airports_by_state <- 
aircraft_strikes_tidy %>% 
  select(State, Airport) %>% 
  count(State) 
airports_by_state
```

```
## # A tibble: 63 x 2
##    State     n
##    <chr> <int>
##  1 AB       28
##  2 AK      382
##  3 AL      362
##  4 AR      191
##  5 AZ     1216
##  6 BC       60
##  7 CA     4857
##  8 CO     2430
##  9 CT      524
## 10 DC     1218
## # ... with 53 more rows
```

