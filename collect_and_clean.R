library(tidyverse)
library(genius)

# collecting data using the genius package --------------------------------

albums <- tribble(~artist, ~album, ~type,
               "Mac Miller", "Circles", "album",
               "Mac Miller", "Swimming", "album",
               "Mac Miller", "The Divine Feminine", "album",
               "Mac Miller", "Faces", "album",
               "Mac Miller", "Macadelic", "album",
               "Mac Miller", "K.I.D.S.", "album")

mac <- albums %>%
  add_genius(artist, album, type)

saveRDS(mac, file = "data/unprocessed/mac.rds")

# cleaning the data -------------------------------------------------------

library(naniar)

# no missing data!
miss_var_summary(mac)

# however, checking that all songs are represented (possible glitch with the
# genius package)
mac %>% 
  arrange(year, track) %>% 
  filter(line == 1) %>% 
  print(n = nrow(mac))

# some albums are missing a few songs so will manually add them
# faces tracks 15 16
# the divine feminine track 5
# swimming track 5

# pulling track titles in order to copy syntax exactly
genius_tracklist(artist = "Mac Miller", album = "Faces")
genius_tracklist(artist = "Mac Miller", album = "The Divine Feminine")
genius_tracklist(artist = "Mac Miller", album = "Swimming")

missingsongs <- tribble(
  ~artist, ~track,
  "Mac Miller", "55",
  "Mac Miller", "San Francisco",
  "Mac Miller", "Cinderella (Ft. Ty Dolla $ign)",
  "Mac Miller", "Self Care"
)

missinglyrics <- missingsongs %>%
  add_genius(artist, track, type = "track")

missinglyrics %>% 
  filter_all(all_vars(is.na(.)))

missinglyrics <- missinglyrics %>% 
  mutate(track_n = ifelse(track_title == "55", 15,
                       ifelse(track_title == "San Francisco", 16, 5)),
         album = ifelse(song == "Self Care", "Swimming",
                        ifelse(song == "Cinderella (Ft. Ty Dolla $ign)",
                               "The Divine Feminine", "faces")))

mac <- rbind(mac, missinglyrics)

mac <- mac %>% 
  janitor::clean_names()

# renaming columns due to personal preference
mac <- mac %>% 
  mutate(artist = NULL, type = NULL) %>% 
  rename(song = track_title,
         track = track_n)

# lowercase everything for consistent ~aesthetic~ (and personal preference)
mac$album = tolower(mac$album)
mac$lyric = tolower(mac$lyric)
mac$song = tolower(mac$song)

# manually researched and added year of release
malcolm <- mac %>% 
  mutate(year = ifelse(album == "circles", 2020,
                       ifelse(album == "swimming", 2018,
                              ifelse(album == "the divine feminine", 2016,
                                     ifelse(album == "faces", 2014,
                                            ifelse(album == "macadelic", 2012, 2010))))))

malcolm$year <- as.factor(malcolm$year)

saveRDS(malcolm, "data/processed/malcolm.rds")

# creating tidy text format (every lyric is split word by word)

library(tidytext)

macaroni <- malcolm %>%
  unnest_tokens(word, lyric) 

saveRDS(macaroni, "data/processed/macaroni.rds")