library(rvest)

url <- 'https://www.imdb.com/search/title?count=100&release_date=2017,2017&title_type=feature'

# xml2.read_html ~ read html
webpage <- read_html(url)

# Data description

# Rank: From 1 to 100, list of most popular feature films released in 2017.
# Title: Title of Feature film.
# Description: Description of feature film.
# Runtime: Duration of feature film.
# Genre: Genre of feature film,
# Rating: IMDb rating
# Metascore: The metascore rating
# Votes: Vote count of feature film.
# Gross_Earning_in_Mil: Gross earnings (Millions).
# Director: Main Director of feature film. (in case of multiple directors, first one applies only).
# Actor: Main actor of feature film. (in case of multiple actors, first one applies only).

# CSS selectors to scrap the rankings section
# rvest.html_nodes ~ extracts using css selectors in conjunction with selectorgadget
rank_data_html <- html_nodes(webpage,'.text-primary')

# Converting the ranking data to text
rank_data <- html_text(rank_data_html)
head(rank_data)

# Data-Preprocessing: Converting rankings to numerical
rank_data<-as.numeric(rank_data)
str(rank_data)

# CSS selector to scrape title section
title_data_html <- html_nodes(webpage, '.lister-item-header a')

# Converting the title data to text
title_data <- html_text(title_data_html)
View(title_data)

# Complete the rest of the data:
# Description
description_data_html <- html_nodes(webpage, '.ratings-bar+ .text-muted')
description_data <- html_text(description_data_html)
View(description_data)
head(description_data)

#   data contains \n, need to remove them
#   gsub ~ grep search '\n\ replace with '', location description_data
description_data <- gsub("\n","",description_data)
head(description_data)

# Runtime
runtime_data_html <- html_nodes(webpage,'.text-muted .runtime')
runtime_data <- html_text(runtime_data_html)
head(runtime_data)
View(runtime_data)

runtime_data <- append(runtime_data, NA, after = 95)
View(runtime_data)

#   removing mins and converting it to numerical
runtime_data<-gsub(" min","",runtime_data)
runtime_data<-as.numeric(runtime_data)
View(runtime_data)

# Movie genre
genre_data_html <- html_nodes(webpage,'.genre')
genre_data <- html_text(genre_data_html)
head(genre_data)

genre_data<-gsub("\n","",genre_data)
genre_data<-gsub(" ","",genre_data)
# taking only the first genre of each movie
genre_data<-gsub(",.*","",genre_data)
genre_data<-as.factor(genre_data)
head(genre_data)

# IMDB rating
rating_data_html <- html_nodes(webpage,'.ratings-imdb-rating strong')
rating_data <- html_text(rating_data_html)
head(rating_data)

rating_data<-as.numeric(rating_data)
head(rating_data)

# Votes
votes_data_html <- html_nodes(webpage,'.sort-num_votes-visible span:nth-child(2)')
votes_data <- html_text(votes_data_html)
head(votes_data)
#   removing commas
votes_data<-gsub(",","",votes_data)
votes_data<-as.numeric(votes_data)
head(votes_data)

# Directors
directors_data_html <- html_nodes(webpage,'.text-muted+ p a:nth-child(1)')
directors_data <- html_text(directors_data_html)
head(directors_data)
directors_data<-as.factor(directors_data)

# Actors
actors_data_html <- html_nodes(webpage,'.lister-item-content .ghost+ a')
actors_data <- html_text(actors_data_html)
head(actors_data)
actors_data<-as.factor(actors_data)

# metascore rating
metascore_data_html <- html_nodes(webpage,'.metascore')
metascore_data <- html_text(metascore_data_html)
head(metascore_data)
#   removing extra space in metascore
metascore_data<-gsub(" ","",metascore_data)
length(metascore_data)

# missing 6 ratings
for (i in c(51 ,67, 71, 76, 96, 97)){
  a <- metascore_data[1:(i-1)]
  b <- metascore_data[i:length(metascore_data)]
  metascore_data <- append(a, list('NA'))
  metascore_data <- append(metascore_data, b)
}
View(metascore_data)

#   converting metascore to numerical
metascore_data<-as.numeric(metascore_data)
length(metascore_data)
View(metascore_data)

summary(metascore_data)

# Gross revenue
gross_data_html <- html_nodes(webpage,'.ghost~ .text-muted+ span')
gross_data <- html_text(gross_data_html)
head(gross_data)

#   removing '$' and 'M' signs
gross_data<-gsub("M","",gross_data)
gross_data<-substring(gross_data,2,6)
length(gross_data)

# fill missing entries with NA
# TO DO LATER

#for (i in c(FIND MISSING VALUE IN INDEX OF GROSS REVENUE)){
#  a<-gross_data[1:(i-1)]
#  b<-gross_data[i:length(gross_data)]
#  gross_data<-append(a,list("NA"))
#  gross_data<-append(gross_data,b)
#}
#gross_data<-as.numeric(gross_data)
#length(gross_data)
summary(gross_data)

# Combining all the list to form data frame
movies_df<-data.frame(Rank = rank_data, Title = title_data,
                      Description = description_data, Runtime = runtime_data,
                      Genre = genre_data, Rating = rating_data,
                      Metascore = metascore_data, Votes = votes_data,
                      Director = directors_data, Actor = actors_data)
str(movies_df)

# analyzing data

library(ggplot2)
qplot(data = movies_df,Runtime,fill = Genre,bins = 30)    # Longest run time is Drama

ggplot(movies_df,aes(x=Runtime,y=Rating))+
  geom_point(aes(size=Votes,col=Genre))     # Votes with the higest runtime, is action, drama

# NEED TO FIX GROSS REV
#ggplot(movies_df,aes(x=Runtime,y=Gross_Earning_in_Mil))+
#  geom_point(aes(size=Rating,col=Genre))




