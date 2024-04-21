library(spotifyr)
library(tidyverse)

NumberArtists <- function(category = "saved") {
  stopifnot(category == "saved" || category == "top")
}


#set-up
id <- '1c68aad85abf44c09d1345783463851b'
secret <- '9e7fc07f6d0e4fa0b5d34122905a4766'

access_token <- get_spotify_access_token(id, secret)
authorization <- get_spotify_authorization_code(client_id = id,
                                                client_secret = secret,
                                                scope = c("user-top-read",
                                                          "user-library-read"))

#saved tracks data frame
saved_tracks <- get_my_saved_tracks(authorization = authorization)
saved_tracks <- rbind(saved_tracks,
                      get_my_saved_tracks(authorization = authorization))
reduce(saved_tracks$track.artists)

# top tracks data frame
top_tracks <- get_my_top_artists_or_tracks(type = "tracks",
                                           time_range = "short_term",
                                           authorization = authorization)
top_tracks <- rbind(top_tracks,
                    get_my_top_artists_or_tracks(type = "tracks",
                                                 offset = 20,
                                                 time_range = "short_term",
                                                 authorization = authorization))
top_tracks <- rbind(top_tracks,
                    get_my_top_artists_or_tracks(type = "tracks",
                                                 offset = 40,
                                                 time_range = "short_term",
                                                 authorization = authorization))

# graphing
occurences <- table(unlist(top_tracks$album.name))
df <- as.data.frame(occurences)
data_sorted <- df[order(df$Freq,
                                decreasing = TRUE), ]
new_df = head(data_sorted, 10)
graph <- ggplot(data = new_df, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Var1), vjust=1.6, color="orange", size = 3) +
  scale_x_discrete(labels = NULL, breaks = NULL) +
  labs(x = "", y = "") +
  #scale_color_manual(palette = c()) +
  theme_minimal()

