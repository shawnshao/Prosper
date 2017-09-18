library(dplyr)
library(ggplot2)
setwd('/home/sshao/other/movies')


movies <- read_csv('/home/sshao/other/movie_metadata.csv')
#parse data
movies<-movies %>% separate(genres,c("genres1","genres2","genres3","genres4")) 

str(movie)
temp3 <- temp %>% group_by(country) %>% summarise(gross = sum(as.numeric(gross),na.rm=TRUE)) %>% arrange(desc(gross))
g <- ggplot(temp3[1:10,],aes(country))
g+geom_bar(aes(weight=gross))

asia <-movies %>% filter(country == 'China' | country == 'Taiwan' | country == 'Hong Kong'| country == 'Japan'| country == 'South Korea') %>% mutate(
    profit = ifnull(gross,0)-isnull(budget,0) ) 
Views(asia)
dim(asia)
asia2 <- asia %>% distinct()
dim(asia2)
View(asia2)

ggplot() + geom_point( data = asia2, aes(x=country, y=imdb_score,size=profit)) 
ggplot() + geom_point( data = asia2, aes(x=title_year, y=profit,size=imdb_score,color=country)) 

bygenres <- movies %>% group_by(title_year,genres1) %>% summarise(imdb=mean(imdb_score))%>%arrange(title_year)

for (i in 1:length(unique(movies$genres1))) 
{
  print(i)
  temp <- bygenres[which(bygenres$genres1==unique(bygenres$genres1)[i]),]
  print(ggplot(data= temp) + geom_smooth( aes(x=title_year,y=imdb)) + ggtitle(unique(bygenres$genres1)[i]))
  ggsave(paste("movies_",unique(bygenres$genres1)[i],".png",sep=''))
}

for (i in 1:length(unique(movies$genres1))) 
{
 i=i+1
 print(i)
}

temp <- bygenres[which(bygenres$genres1==unique(bygenres$genres1)[10]),]
ggplot(data= temp) + geom_smooth( aes(x=title_year,y=imdb)) + ggtitle(unique(bygenres$genres1)[10])
