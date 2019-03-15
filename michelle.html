---
title: "Aircraft Species Struck"
Author: "Michelle Cui"
output:
  html_document:
    theme: spacelab
    keep_md: true
    toc: no
---

```{r}
library(tidyverse)
```


```{r}
aircraft_strikes <-
  readr::read_csv("FAA_National_Wildlife_Aircraft_Strike_Database__Year_2000_and_after_.csv")
```

```{r}
names(aircraft_strikes)
```

```{r}
gather(aircraft_strikes)
```

# Is the data tidy? If so, how do you know?

# No because values are not sorted into clean columns and have scattered observations that can be grouped together.

```{r}
aircraft_strikes_tidy <-
  aircraft_strikes %>%
  na_if("UNKNOWN") %>%
  na_if("UNKOWN BIRD") %>%
  na_if("UNK") %>%
  na_if("UNKNOWN BIRD - SMALL") %>%
  na_if("UNKNOWN BIRD - MEDIUM")
aircraft_strikes
```

```{r}
aircraft_strikes_tidy <-
  aircraft_strikes %>%
  filter(!Species=="UNKNOWN BIRD - SMALL",
         !Species=="NA",
         !Species=="UNKNOWN BIRD - MEDIUM",
         !Species=="UNKNOWN BIRD - LARGE",
         !Species=="UNKNOWN BIRD",
         !`Height (feet)`=="NA")
aircraft_strikes_tidy
```

```{r}
aircraft_species <-
  aircraft_strikes_tidy %>%
  filter(!`Phase of Flight`=="NA",
         !Size=="NA") %>%
  select(Size,`Height (feet)`, `Phase of Flight`)
aircraft_species
```

# Produce a summary of the birds that were struck. Be sure to include the minimum, maximum, and total n of species struck.

```{r}
aircraft_strikes_tidy %>%
  summarize(min_Species=min(Species),
            max_Species=max(Species),
            total=n())
```

# Between the Acadian Flycatcher and the Zebra Dove, produce a summary that shows the phase of flights they were struck at.

```{r}
aircraft_strikes_tidy %>%
  filter(!`Phase of Flight`=="NA",
         Species=="ACADIAN FLYCATCHER" | Species=="ZEBRA DOVE") %>%
  select(`Phase of Flight`, Species) %>%
  arrange(desc(`Phase of Flight`))
```

# Graph the distribution of the height during flights that birds were struck at with a histogram and density curve.

```{r}
rip_birds <-
  aircraft_strikes_tidy %>%
  ggplot(aes(x=log10(`Height (feet)`)))+
  geom_histogram(aes(y=..density..), binwidth = .4, fill="steelblue", alpha=0.8, color="black")+
    geom_density(color="red")+
  labs(title="Distribution of Height")

rip_birds
```


# Birds were struck at different phases and heights during airplane flights. What was the most common phase of flight that birds were struck? Likewise, what was the most common height that they were struck at?

```{r}
aircraft_species <-
  aircraft_strikes_tidy %>%
  filter(!`Phase of Flight`=="NA",
         !Size=="NA") %>%
  select(Size, `Height (feet)`, `Phase of Flight`) %>%
  ggplot(aes(x=Size, y=`Height (feet)`))+
  geom_bar(stat="identity")+
  facet_wrap(~`Phase of Flight`)+
  labs(title="Phases and Height of Flight Of Birds Struck")
aircraft_species
```

# What is the relationship between phase of flight and weather conditions at the time of flight?

```{r}
weather <-
  aircraft_strikes_tidy %>%
  filter(!`Phase of Flight`=="NA",
         !Sky=="NA",
         !Precipitation=="NA") %>%
  select(`Phase of Flight`, Sky, `Birds Struck`) %>%
  ggplot(aes(x=`Phase of Flight`, fill=Sky))+
  geom_bar(position="dodge")+
  coord_flip()+
  labs(title="x")
weather
```

