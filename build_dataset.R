library(billboard)
library(geniusr)
library(dplyr)
library(tidyr)
library(magrittr)

data(wiki_hot_100s)
library(geniusr)
token="ggVzQY9ZmFQiNThaBOSCv_9vAlCokBcFDl90jOxDiFVBaJmLAyfddTxgbTMP9thL"
genius_token()



data(spotify_track_data)
spotify_track_data %<>% select(-c("explicit", "type")) %>%
  group_by(year) %>% mutate(rank=row_number(year))

library(lme4)
data=spotify_track_data1 %>% select(rank, year, mode, mode, valence)
fit1=lm(formula = rank~ mode+tempo+valence, data=spotify_track_data1)
summary(fit1)
plot(fitted(fit1),resid(fit1))
fit2=lmer(formula = rank~ mode+tempo+speechiness+valence+(1|year),data=spotify_track_data1)
confint(fit2)



#------------------------------------------------
n=length(wiki_hot_100s$title)
song_id=rep(NA, n)
for (i in 682:n){
  song_id[i]=search_song(search_term = wiki_hot_100s$title[i], access_token = token)[[1,1]]
}

ly=scrape_lyrics_id(song_id[1], access_token = token)


song_url1=song_id=rep(NA, 100)
for (i in 1:100){
  song_url1[i]=search_song(search_term = wiki_hot_100s$title[i], access_token = token)[[1,3]]
}

#-------------------------------------
data(spotify_track_data)
spotify_track_data1=spotify_track_data %>% select(-c("explicit", "type")) %>%
  rename(artist=artist_name, title=track_name)
wiki_hot_100s1=wiki_hot_100s %>% filter(year!="2016")
billboard100=left_join(wiki_hot_100s1, spotify_track_data1, by=c("title", "artist", "year"))


# A=billboard100 %>% group_by(year) %>% summarise(n())
# B=wiki_hot_100s1 %>% group_by(year) %>% summarise(n())
# D=spotify_track_data1  %>% group_by(year) %>% summarise(n())
# C=cbind(A,B, D)

#---------------------------------------
library(spotifyr)

keys <- spotifyOAuth("ma578","d8c25b0077f64f9e9b08f535dbdda0f9","e19c512a12a340afa6b51e317df02362")


Sys.setenv(SPOTIFY_CLIENT_ID = 'd8c25b0077f64f9e9b08f535dbdda0f9')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'e19c512a12a340afa6b51e317df02362')
access_token <- get_spotify_access_token()


wiki_hot_100s1

get_track_audio_features(ids, authorization = get_spotify_access_token())

#-------------------------------------------




