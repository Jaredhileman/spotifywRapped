ID = "b6bed725df9a4966a23b8984a2d40457"
secret = "5609c10a964b4f2082f47842a5d2fc50"

access_token <- spotifyr::get_spotify_access_token(ID, secret)
authorization <- spotifyr::get_spotify_authorization_code(ID,
                                                            secret,
                                                            "user-top-read user-library-read")

# Obtain top 1000 artists
offset = seq(0, 950, 50)
top_artists_longterm <- data.frame()
for (batch in offset) {
  curr_artists <- spotifyr::get_my_top_artists_or_tracks(type = "artists",
                                                        limit = 50,
                                                        offset = batch,
                                                        time_range = "long_term",
                                                        authorization = authorization)
  top_artists_longterm <- dplyr::bind_rows(top_artists_longterm, curr_artists)
}


# Obtain top 1000 tracks
top_tracks_longterm <- data.frame()
for (batch in offset) {
  curr_tracks <- spotifyr::get_my_top_artists_or_tracks(type = "tracks",
                                                         limit = 50,
                                                         offset = batch,
                                                         time_range = "long_term",
                                                         authorization = authorization)
  top_tracks_longterm <- dplyr::bind_rows(top_tracks_longterm, curr_tracks)
}





offset_length <- ceiling(spotifyr::get_my_saved_tracks(include_meta_info = TRUE,
                                                       authorization = authorization)[['total']] / 50)
offset_total <- (seq(offset_length) - 1) * 50


# Obtain saved tracks
saved_tracks<- data.frame()
for (batch in offset_total) {
  curr_saved_tracks <- spotifyr::get_my_saved_tracks(limit = 50,
                                                        offset = batch,
                                                        authorization = authorization)
  saved_tracks <- dplyr::bind_rows(saved_tracks, curr_saved_tracks)
}

# Get artist name
for (i in 1:nrow(saved_tracks)) {
  saved_tracks$artist[i] <- saved_tracks$track.artists[i][[1]]$name
  audio_features <- spotifyr::get_track_audio_features(saved_tracks$track.id[i],
                                                       access_token = access_token)
  saved_tracks$energy[i] <- audio_features$energy
  saved_tracks$valence[i] <- audio_features$valence
}


# q: how to append data frames to the end of data frames in R
# a: https://stackoverflow.com/questions/3505701/rbind-data-frames-in-a-list

usethis::use_data(saved_tracks, overwrite = TRUE)
usethis::use_data(top_artists_longterm, overwrite = TRUE)
usethis::use_data(top_tracks_longterm, overwrite = TRUE)
