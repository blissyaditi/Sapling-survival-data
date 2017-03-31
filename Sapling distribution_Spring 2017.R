library(ggplot2) # graphing package
library(dplyr) # this package is very powerful for summarizing and manipulating data
library(gridExtra) # works with ggplot to create grids of graphs, arranging plots
library(ggthemes) # gives you additional ggplot themes, to make look graph diff ways
library(RColorBrewer) # this gives you custom color ramps, including color blind friendly stuff
library(tidyverse)





sapdata <- read.csv('./cleaned_data.csv', stringsAsFactors = FALSE)
head(sapdata)

## Histogram of dead saplings

# What 'gather' does - gather is the one who takes all the columns that we don't have listed with -In Front of them, and makes them a column that we have defined as "month" and then takes the contents of all those columns, and puts it into a column we've defined as "live_dead"

# What 'mutate' does- mutate creates new columns. Here we create two, binary, which is 1 if the sapling is live and 0 if its dead and group, which is just collapsing area down into two categories, C and F 

# What 'separate' does- separate is to separate the month column into a column which just contains the word "month" and another which has the numeric value. We are sep=5 because month has five characters.
# we are turning the num column into a numeric column called month_num
# filtering out all the dead saplings

inonecol <- sapdata %>% gather("month","live_dead", -AREA, -SITE, -largeplot_id, -smallplot_id, -tag, -SPECIES, -height) %>% 
            mutate(binary = ifelse(live_dead=="L",1,0),
            group=ifelse(AREA=="C1"|AREA=="C2"|AREA=="C3"|AREA=="C4","C","F")) %>% separate(month, into=c("word","num"),sep=5) %>%
            mutate(month_num = as.numeric(num)) %>%
            filter(live_dead=="L")

head(inonecol)

ggplot()+
    geom_histogram(data=inonecol, aes(x=month_num, group=group, fill=group, alpha=0.5), stat="count") + 
    facet_wrap(~SPECIES)

# This graph shows total number of saplings for each species in C and F plots
inonecol <- sapdata %>% 
    
    gather("month","live_dead", -AREA, -SITE, -largeplot_id, -smallplot_id, -tag, -SPECIES, -height) %>%
    
    mutate(binary = ifelse(live_dead=="L",1,0),
           group=ifelse(AREA=="C1"|AREA=="C2"|AREA=="C3"|AREA=="C4","C","F")) %>%
    
    separate(month, into=c("word","num"),sep=5) %>%
    
    mutate(month_num = as.numeric(num)) %>%
    # filters out just the first month
    filter(month_num==1) %>%
    # groups by SPECIES and the group (C and F)
    group_by(SPECIES, group) %>%
    # summarizes, so for each unique combination of C and F it will give us the count
    summarize(count=n())

head(inonecol)

ggplot()+
    geom_bar(data=inonecol, aes(x=SPECIES, y=count, fill=group), stat="identity", position="dodge")


# histogram of sapling abundance irrespective of proximity 

inonecol <- sapdata %>% 
    gather("month","live_or_dead", -AREA, -SITE, -largeplot_id, -smallplot_id, -tag, -SPECIES, -height) %>%
    filter(month=="month1"|month=="month29",live_or_dead=="L") %>%
    group_by(SPECIES, month) %>%
    summarize(count=n())

ggplot(dat=inonecol)+
    geom_bar(aes(x=SPECIES, y=count, group=month, fill=month), position="dodge",stat="identity")



#Histogram of sapling abundance for month 1 and 29 only for CLOSE plots 

inonecol <- sapdata %>% 
    gather("month","live_or_dead", -AREA, -SITE, -largeplot_id, -smallplot_id, -tag, -SPECIES, -height) %>%
    mutate(group=ifelse(AREA=="C1"|AREA=="C2"|AREA=="C3"|AREA=="C4","C","F")) %>%
    filter(month=="month1"|month=="month29", group== "C",live_or_dead=="L") %>%
    group_by(SPECIES, month) %>%
    summarize(count=n())

ggplot(dat=inonecol)+
    geom_bar(aes(x=SPECIES, y=count, group=month, fill=month), position="dodge",stat="identity")

#Histogram of sapling abundance for month 1 and 29 only for FAR plots 

inonecol <- sapdata %>% 
    gather("month","live_or_dead", -AREA, -SITE, -largeplot_id, -smallplot_id, -tag, -SPECIES, -height) %>%
    mutate(group=ifelse(AREA=="C1"|AREA=="C2"|AREA=="C3"|AREA=="C4","C","F")) %>%
    filter(month=="month1"|month=="month29", group== "F",live_or_dead=="L") %>%
    group_by(SPECIES, month) %>%
    summarize(count=n())

ggplot(dat=inonecol)+
    geom_bar(aes(x=SPECIES, y=count, group=month, fill=month), position="dodge",stat="identity")

# Sapling height distribution 

inonecol <- sapdata %>% 
   
    gather("month","live_dead", -AREA, -SITE, -largeplot_id, -smallplot_id, -tag, -SPECIES, -height) %>%
    # mutate creates new columns
    # here we create two, binary, which is 1 if the sapling is live and 0 if its dead
    # and group, which is just collapsing area down into two categories, C and F
    mutate(binary = ifelse(live_dead=="L",1,0),
    group=ifelse(AREA=="C1"|AREA=="C2"|AREA=="C3"|AREA=="C4","C","F")) %>%
    separate(month, into=c("word","num"),sep=5) %>%
    # we are turning the num column into a numeric column called month_num
    mutate(month_num = as.numeric(num)) %>%
    # filters out just the first month
    filter(month_num==1) 


ggplot()+
    geom_boxplot(data=inonecol, aes(x=SPECIES, y=height, fill=group), position="dodge") 



