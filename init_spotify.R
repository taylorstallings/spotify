install.packages('devtools')
install.packages('genius')
library(devtools)
devtools::install_github('charlie86/spotifyr', force=TRUE)
library(spotifyr)
library(tidyverse)
library(knitr)
library(lubridate)
library(genius)


Sys.setenv(SPOTIFY_CLIENT_ID = 'c29caf73edfd49a7b36586a9c5e7de41')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '8bd0fa75072b4781876f12069be61b42')
access_token <- get_spotify_access_token()

artists <- c('The Killers', 'Twenty One Pilots', 'The Avett Brothers')

artist_audio_features <-map_df(artists)

beatles %>% 
  count(key_mode, sort = TRUE) %>% 
  head(5) %>% 
  kable()

get_my_recently_played(limit = 50) %>% 
  mutate(artist.name = map_chr(track.artists, function(x) x$name[1]),
         played_at = as_datetime(played_at)) %>% 
  select(track.name, artist.name, track.album.name, played_at) %>% 
  kable()

get_my_top_artists_or_tracks(type = 'artists', time_range = 'long_term', limit = 10) %>% 
  select(name, genres) %>% 
  rowwise %>% 
  mutate(genres = paste(genres, collapse = ', ')) %>% 
  ungroup %>% 
  kable()


get_my_top_artists_or_tracks(type = 'tracks', time_range = 'long_term', limit = 50) %>% 
  mutate(artist.name = map_chr(artists, function(x) x$name[1])) %>% 
  select(name, artist.name, album.name) %>% 
  kable()

all_my_fav_tracks <-
  ceiling(get_my_saved_tracks(include_meta_info = TRUE)[['total']] / 50) %>%
  seq() %>%
  map(function(x) {
    get_my_saved_tracks(limit = 50, offset = (x - 1) * 50)
  }) %>% reduce(rbind) %>%
  
  glimpse(all_my_fav_tracks)

artist_from_fav_tracks <-
  all_my_fav_tracks %>%
  select(track.artists) %>%
  reduce(rbind) %>%
  reduce(rbind) %>%
  # I don't think i'll need Urls in further analyses, id (unique mark of artist) and name are selected here.
  select(id, name)

artist_from_fav_tracks

track_num_artist <-
  artist_from_fav_tracks %>%
  count(id, sort = TRUE) %>%
  left_join(artist_from_fav_tracks, by = 'id',.) %>%
  unique() %>%
  select(-id) %>%
  top_n(20, n)

track_num_artist  %>%
  kable()



# For numerical variables, sometimes for simplifying problems, cut them into fractions is a good idea. Here,  go further and fill the column plot with different color to represent different frequency group.
track_num_artist %>%
  mutate(
    freq = case_when(
      n > 100 ~ 'More than 100 tracks',
      between(n, 50, 99) ~ '50~99 tracks',
      between(n, 20, 49) ~ '20~49 tracks',
      TRUE ~ 'Less than 20 tracks'
    )
  )
