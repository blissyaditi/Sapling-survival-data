library(tidyverse)

# reads in the data, you will need to change the file path to match your file path
dat <- read.csv("~/../Dropbox/Other_People/aditi/cleaned_data.csv", stringsAsFactors = FALSE) 


gdat <- dat %>% 
            gather("month","live_or_dead", 
                   -AREA, -SITE, -largeplot_id, -smallplot_id, -tag, -SPECIES, -height) %>%
            filter(month=="month1"|month=="month29",
                   live_or_dead=="L") %>%
            group_by(SPECIES, month) %>%
            summarize(count=n())

ggplot(dat=gdat)+
      geom_bar(aes(x=SPECIES, y=count, group=month, fill=month), position="dodge",stat="identity")