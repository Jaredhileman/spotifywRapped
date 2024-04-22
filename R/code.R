ID = "b6bed725df9a4966a23b8984a2d40457"
secret = "5609c10a964b4f2082f47842a5d2fc50"

access_token <- spotifyr::get_spotify_access_token(ID, secret)
authorization <- spotifyr::get_spotify_authorization_code(ID, secret,
                        scope = c("user-top-read", "user-read-recently-played",
                                 "user-read-currently-playing"))



# Obtain top medium_term artists
offset = seq(0, 950, 50)
top_artists_longterm <- data.frame()
for (batch in offset) {
  curr_artists <- spotifyr::get_my_top_artists_or_tracks(type = "artists",
                                                         limit = 50,
                                                         offset = batch,
                                                         time_range = "medium_term",
                                                         authorization = authorization)
  top_artists_longterm <- dplyr::bind_rows(top_artists_longterm, curr_artists)
}


# Obtain top medium_term tracks
top_tracks_longterm <- data.frame()
for (batch in offset) {
  curr_tracks <- spotifyr::get_my_top_artists_or_tracks(type = "tracks",
                                                        limit = 50,
                                                        offset = batch,
                                                        time_range = "medium_term",
                                                        authorization = authorization)
  top_tracks_longterm <- dplyr::bind_rows(top_tracks_longterm, curr_tracks)
}

#offset value
# offset_length <- ceiling(spotifyr::get_my_saved_tracks(include_meta_info = TRUE,
#                                                        authorization = authorization)[['total']] / 50)
# offset_total <- (seq(offset_length) - 1) * 50

## Obtain saved tracks
# saved_tracks<- data.frame()
# for (batch in offset_total) {
#   curr_saved_tracks <- spotifyr::get_my_saved_tracks(limit = 50,
#                                                      offset = batch,
#                                                      authorization = authorization)
#   saved_tracks <- dplyr::bind_rows(saved_tracks, curr_saved_tracks)
# }


# Save data
# q: how to append data frames to the end of data frames in R
# a: https://stackoverflow.com/questions/3505701/rbind-data-frames-in-a-list

usethis::use_data(saved_tracks, overwrite = TRUE)
usethis::use_data(top_artists_longterm, overwrite = TRUE)
usethis::use_data(top_tracks_longterm, overwrite = TRUE)

###################################
###################################

# Fetch data using the calculated date range - MEDIUM TERM (approx 6 months)
offset = seq(0, 950, 50)
top_artists_mediumterm <- data.frame()
for (batch in offset) {
  curr_artists <- spotifyr::get_my_top_artists_or_tracks(type = "artists",
                                                         limit = 50,
                                                         offset = batch,
                                                         time_range = "medium_term",
                                                         authorization = authorization)
  top_artists_mediumterm <- dplyr::bind_rows(top_artists_mediumterm, curr_artists)
}


# Obtain top medium_term tracks
top_tracks_mediumterm <- data.frame()
for (batch in offset) {
  curr_tracks <- spotifyr::get_my_top_artists_or_tracks(type = "tracks",
                                                        limit = 50,
                                                        offset = batch,
                                                        time_range = "medium_term",
                                                        authorization = authorization)
  top_tracks_mediumterm <- dplyr::bind_rows(top_tracks_mediumterm, curr_tracks)
}

# usethis::use_data(saved_tracks, overwrite = TRUE)
usethis::use_data(top_artists_mediumterm, overwrite = TRUE)
usethis::use_data(top_tracks_mediumterm, overwrite = TRUE)


###################################

# Fetch data using the calculated date range - SHORT TERM (approx 1 month)
offset = seq(0, 950, 50)
top_artists_shortterm <- data.frame()
for (batch in offset) {
  curr_artists <- spotifyr::get_my_top_artists_or_tracks(type = "artists",
                                                         limit = 50,
                                                         offset = batch,
                                                         time_range = "short_term",
                                                         authorization = authorization)
  top_artists_shortterm <- dplyr::bind_rows(top_artists_shortterm, curr_artists)
}


# Obtain top medium_term tracks
top_tracks_shortterm <- data.frame()
for (batch in offset) {
  curr_tracks <- spotifyr::get_my_top_artists_or_tracks(type = "tracks",
                                                        limit = 50,
                                                        offset = batch,
                                                        time_range = "short_term",
                                                        authorization = authorization)
  top_tracks_shortterm <- dplyr::bind_rows(top_tracks_shortterm, curr_tracks)
}

# usethis::use_data(saved_tracks, overwrite = TRUE)
usethis::use_data(top_artists_shortterm, overwrite = TRUE)
usethis::use_data(top_tracks_shortterm, overwrite = TRUE)

###################################

# I think short data is in medium data and medium data in long data

###################################
###################################
# No use with below code
####################################

# Fetch data using the calculated date range - ALL DATA
offset = seq(0, 950, 50)
top_artists_alltime <- data.frame()
top_tracks_alltime <- data.frame()
for (batch in offset) {
  curr_artists <- spotifyr::get_my_top_artists_or_tracks(type = "artists",
                                                         limit = 50,
                                                         offset = batch,
                                                         time_range = "short_term",
                                                         authorization = authorization)
  top_artists_alltime <- dplyr::bind_rows(top_artists_alltime, curr_artists)

  curr_tracks <- spotifyr::get_my_top_artists_or_tracks(type = "tracks",
                                                        limit = 50,
                                                        offset = batch,
                                                        time_range = "short_term",
                                                        authorization = authorization)
  top_tracks_alltime <- dplyr::bind_rows(top_tracks_alltime, curr_tracks)
}

get_alltime <- function(type_data, auth_code){
  alltime <- data.frame()
  curr_artists <- spotifyr::get_my_top_artists_or_tracks(type = type_data,
                                                         limit = 50,
                                                         offset = 5,
                                                         time_range = "long_term",
                                                         authorization = auth_code)
  top_artists_alltime <- dplyr::bind_rows(top_artists_alltime, curr_artists)
}

# usethis::use_data(saved_tracks, overwrite = TRUE)
usethis::use_data(top_artists_alltime, overwrite = TRUE)
usethis::use_data(top_tracks_alltime, overwrite = TRUE)

#########################################
track_ids <- top_artists_shortterm$id
# Get audio features for each track
audio_features <- lapply(track_ids, function(track_id) {
  spotifyr::get_track_audio_features(track_ids)
})

# Extract valence values
valence <- sapply(audio_features, function(features) {
  features$valence
})

# Combine track names and valence values
valence_data <- data.frame(track_name = recently_played$track.name, valence = valence)

# Sort the data frame by valence in descending order
valence_data_sorted <- valence_data[order(-valence_data$valence), ]

# Print the sorted valence values and track names
print(valence_data_sorted)

###########################################

Feat_scraper <- function(x, token) {
  # omitting progress info
  base::options(warn =-1)
  # assigning length of an ID vector to a proxy object
  entire <- length(x)
  # setting seed for repo purposes
  set.seed(1, sample.kind = "Rounding")
  # assigning 100 sampled IDs to a vector to account for Spotify's limit
  #v1a <- as.character(sample(x, 100, replace = T))
  # assigning a tibble with features of those 100 IDs. This tibble will be
  # extended below.
  tib <- spotifyr::get_track_audio_features(id_s, token)
  # replacing any IDs with new ones if those IDs are already in the tibble
  if (any(x %in% tib$id) == T) {x = x[which(!x %in% tib$id)]}
  # creating a while loop on the condition that the rows of the tibble are
  # less and/or equal to the length of the entire object
  while (nrow(tib) <= entire) {
    # Setting seed for repo purposes
    set.seed(42, sample.kind = "Rounding")
    # assigning 100 sampled IDs from the new IDs from above to a base vector
    # according to Spotify's limit as long as the object IDs are greater
    # than 100. If the remaining IDs are less than 100, these remaining IDs
    # will be sampled.
    v1b <- as.character(sample(x, ifelse(length(x) > 100, 100, length(x)),
                               replace = F))
    # extending the tibble from above to create a complete tibble with all
    # retrieved audio features of all track IDs of the object in question
    tib %<>% full_join(spotifyr::get_track_audio_features(v1b,token),
                       by = c("danceability", "energy", "key", "loudness",
                              "mode", "speechiness", "acousticness",
                              "instrumentalness", "liveness",
                              "valence", "tempo", "type", "id", "uri",
                              "track_href", "analysis_url", "duration_ms",
                              "time_signature"))
    # replacing any IDs with new ones if those IDs are already in the tibble
    if (any(x %in% tib$id) == T) {x = x[which(!x %in% tib$id)]}
    # If the rows of the tibble are equal to the length of the entire object
    # in question…,
    if (nrow(tib) == entire)
      #…break the loop.
      break
  }
  # outputting the entire tibble
  return(tib)
}

id_s <- top_tracks_shortterm$id
Feats <- Feat_scraper(id_s, authorization)
