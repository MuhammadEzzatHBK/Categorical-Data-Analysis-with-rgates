#Including Packages
library(tidyverse)
library(rgates)
library(ggthemes)
#Preparing Data
netflix <- read_csv('netflix_titles.csv')
netflix <- na.omit(netflix)
summary(netflix)
netflix <- arrange(netflix,title)
netflix <- separate(netflix,duration,into = c('dur','unit'),sep=' ')
netflix$dur <- as.numeric(netflix$dur) 

#Using rgates in filtering
comedy_tv_shows <- filter(netflix, and(netflix$type=="TV Show",grepl('Comedies',netflix$listed_in)))
non_romantic_movies <- filter(netflix,inhibit(netflix$type =='Movie',grepl('Romantic',netflix$listed_in)))
USA_nonDramaMovies <- filter(netflix,and(netflix$country=='United States',
                                         inhibit(netflix$type =='Movie',grepl('Drama',netflix$listed_in))))

#Using rgates in grouping , summarizing & plotting
filter(netflix,netflix$type=='Movie')%>%
        mutate(is_american_millennial =
            and(release_year>=2000,grepl('United States',country)))%>%
                group_by(is_american_millennial) %>% summarize(mean_duration = mean(dur))%>%
  ggplot(aes(x=is_american_millennial,y=mean_duration))+geom_col()+
                theme_hc()+theme(panel.grid.major.x = element_blank())+
  xlab('Is it an american movies released after 2000?')+
  ylab('Mean movie duration')+
  ggtitle('Mean duration for american millennial movies & other movies',
          subtitle = 'Using rgates package in categorical data analysis')

