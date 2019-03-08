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
#glimpse(aircraft_strikes)
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
#gather(aircraft_strikes)
```

~/Desktop/FRS 417/ecologygals-master/FAA_National_Wildlife_Aircraft_Strike_Database__Year_2000_and_after_.csv

~/Desktop/FRS 417/ecologygals-master/SPE_pitlatrine.csv

Each make 4 questions and decide which ones we want to use
Scatterplot
names, spread, gather, etc


```r
aircraft_strikes_tidy <-
  aircraft_strikes %>% 
  na_if("UNKNOWN") %>% 
  na_if("UNKNOWN BIRD") %>% 
  na_if("UNK")
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

Scatterplot
Make a scatterplot of the name of the airlines and compare it to the number of birds they've hit

```r
aircraft_strikes_tidy %>% 
  ggplot(aes(x=`Birds Struck`))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 60,hjust=1,size=5))+
  theme(axis.text.y=element_text(hjust=1))
```

![](Untitled_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


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
  top_n(10, count) %>% 
  ggplot(aes(x=reorder(Operator, count), y=count, fill=Operator))+
  geom_col()+
  coord_flip()
```

![](Untitled_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


```r
aircraft_strikes_tidy %>% 
  count(Species) %>% 
  arrange(desc(n))
```

```
## # A tibble: 487 x 2
##    Species                   n
##    <chr>                 <int>
##  1 <NA>                  16436
##  2 UNKNOWN BIRD - SMALL  11759
##  3 UNKNOWN BIRD - MEDIUM  3653
##  4 MOURNING DOVE          2291
##  5 GULLS                  2186
##  6 EUROPEAN STARLING      1427
##  7 AMERICAN KESTREL       1422
##  8 SPARROWS               1220
##  9 KILLDEER               1189
## 10 UNKNOWN BIRD - LARGE   1076
## # ... with 477 more rows
```


```r
QQQ <- 
  aircraft_strikes_tidy %>% 
  filter(!Species=="UNKNOWN BIRD - SMALL",
         !Species=="NA",
         !Species=="UNKNOWN BIRD - MEDIUM",
         !Species=="UNKNOWN BIRD - LARGE")
QQQ
```

```
## # A tibble: 27,047 x 20
##    `Operator ID` Operator `Aircraft Type` `Registration N… Airport Runway
##    <chr>         <chr>    <chr>           <chr>            <chr>   <chr> 
##  1 <NA>          <NA>     <NA>            <NA>             PAJN    8/26  
##  2 <NA>          <NA>     <NA>            <NA>             KOMA    18    
##  3 <NA>          <NA>     <NA>            <NA>             KCLE    24    
##  4 <NA>          <NA>     <NA>            <NA>             KAUS    17R/3…
##  5 <NA>          <NA>     <NA>            <NA>             KBWI    10/28 
##  6 <NA>          <NA>     <NA>            <NA>             KMDW    31C   
##  7 <NA>          <NA>     <NA>            <NA>             KBOS    <NA>  
##  8 <NA>          <NA>     <NA>            <NA>             KTEB    6     
##  9 <NA>          <NA>     <NA>            <NA>             KPSM    16    
## 10 <NA>          <NA>     <NA>            <NA>             KSLC    17/35 
## # ... with 27,037 more rows, and 14 more variables: State <chr>,
## #   City <chr>, Date <chr>, `Height (feet)` <dbl>, Speed <dbl>, `Phase of
## #   Flight` <chr>, Sky <chr>, Precipitation <chr>, Species <chr>, `Birds
## #   Seen` <chr>, `Birds Struck` <chr>, Size <chr>, `Cost of
## #   Repairs` <dbl>, `Other Costs` <dbl>
```


```r
QQQ %>% 
  ggplot(aes(x=Size, y=`Birds Struck`))+
  geom_boxplot()
```

![](Untitled_files/figure-html/unnamed-chunk-13-1.png)<!-- -->


```r
QQQ %>% 
  filter(!`Birds Struck`>1) %>% 
  ggplot(aes(x=Size, y=as.numeric(`Birds Struck`)))+
  geom_boxplot()
```

![](Untitled_files/figure-html/unnamed-chunk-14-1.png)<!-- -->


```r
QQQ %>%
  filter(!Size=="NA") %>% 
  group_by(Size) %>% 
  summarize(count=n()) %>% 
  ggplot(aes(x=Size, y=count))+
  geom_col()
```

![](Untitled_files/figure-html/unnamed-chunk-15-1.png)<!-- -->


```r
airport_species <- 
QQQ %>% 
  group_by(Airport, Species) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count))
airport_species
```

```
## # A tibble: 7,351 x 3
## # Groups:   Airport [1,081]
##    Airport Species               count
##    <chr>   <chr>                 <int>
##  1 KDEN    HORNED LARK             244
##  2 KPHX    MOURNING DOVE           208
##  3 KDFW    MOURNING DOVE           207
##  4 KMDW    AMERICAN KESTREL        164
##  5 KJFK    HERRING GULL            157
##  6 PHNL    PACIFIC GOLDEN-PLOVER   146
##  7 KPDX    AMERICAN KESTREL        117
##  8 KDFW    ROCK PIGEON             102
##  9 PHLI    PACIFIC GOLDEN-PLOVER   101
## 10 KTEB    EUROPEAN STARLING        93
## # ... with 7,341 more rows
```


```r
airport_species %>% 
  filter(Species=="HORNED LARK" | Species=="MOURNING DOVE") %>% 
  ggplot(aes(x=Species, y=count))+
  geom_boxplot()
```

![](Untitled_files/figure-html/unnamed-chunk-17-1.png)<!-- -->


```r
airport_species %>% 
  filter(Species=="HORNED LARK" | Species=="MOURNING DOVE")
```

```
## # A tibble: 316 x 3
## # Groups:   Airport [252]
##    Airport Species       count
##    <chr>   <chr>         <int>
##  1 KDEN    HORNED LARK     244
##  2 KPHX    MOURNING DOVE   208
##  3 KDFW    MOURNING DOVE   207
##  4 KMCI    HORNED LARK      89
##  5 KCMH    MOURNING DOVE    82
##  6 KCOS    HORNED LARK      72
##  7 KCLE    MOURNING DOVE    71
##  8 KDEN    MOURNING DOVE    67
##  9 KSLC    HORNED LARK      67
## 10 KSHV    MOURNING DOVE    60
## # ... with 306 more rows
```





