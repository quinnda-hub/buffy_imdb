library(data.table)
library(rvest)
library(purrr)

buffy_seasons <- function() {
  paste0("https://www.imdb.com/title/tt0118276/episodes?season=", 1:7)
}

scrape_episodes <- function(season.url) {
  gather_links     <- function(season.url) {
    paste0(
      "https://www.imdb.com",
      read_html(season.url) |>
        html_elements("#episodes_content strong a") |>
        html_attr("href")
    )
  }
  scrape_season    <- function(episode) {
    episode |>
      html_elements(".LumGv") |>
      html_text() 
    
  }
  scrape_title     <- function(episode) {
    potential_titles <- map(c(".fbOhB", ".cAMrQp", ".eKrKux"),
                            ~ html_elements(episode, .x) |> html_text())
    
    compact(potential_titles) |> 
      unlist()
  }
  scrape_directors <- function(episode) {
    episode |>
      html_elements(
        ".ipc-metadata-list__item:nth-child(1) .baseAlt .ipc-metadata-list-item__list-content-item--link") |>
      html_text() |>
      unique()
  }
  scrape_writers   <- function(episode) {
    potential_writers <- map(
      c(".ipc-metadata-list__item:nth-child(2) .baseAlt .ipc-metadata-list-item__list-content-item--link",
        ".ipc-metadata-list-item--link:nth-child(2) .baseAlt .ipc-metadata-list-item__list-content-item--link"),
                            ~ html_elements(episode, .x) |> html_text())
    
    compact(potential_writers) |> 
      unlist() |>
      unique()

  }
  scrape_rating    <- function(episode) {
    episode |>
      html_elements(".jGRxWM") |> 
      html_text() |>
      unique() 
  }
  scrape_votes     <- function(season) {
    season |>
      html_elements(".ipl-rating-star__total-votes") |>
      html_text()
    
  }
  scrape_airdate   <- function(episode) {
    episode |>
      html_elements(".kqWovI .ipc-inline-list__item:nth-child(1)") |>
      html_text()
  }
  
  season <- read_html(season.url)
  episodes <- gather_links(season.url) |>
    map(~ {message(.x); read_html(.x)})
  
  data.table(episode = map(episodes, scrape_season) |> unlist(),
             title = map(episodes, scrape_title) |> unlist(),
             directors = map(episodes, scrape_directors),
             writers = map(episodes, scrape_writers),
             rating = map(episodes, scrape_rating) |> unlist(),
             total_votes = scrape_votes(season),
             air_date = map(episodes, scrape_airdate) |> unlist())
}

clean_episodes <- function(DT) {
  clean_episode_number <- function(DT) {
    x <- strsplit(DT$episode, ".", fixed = TRUE)
    
    s  <- map(x, pluck, 1) |> unlist() |> readr::parse_number()
    e  <- map(x, pluck, 2) |> unlist() |> readr::parse_number()

    DT[, `:=` (episode = e,
               season  = s)]
  }
  clean_directors      <- function(DT) {
    DT[, directors := map(directors, ~ paste0(.x, collapse = ", ")) |>
         map(~ paste0("[", .x, "]")) |> unlist()]
  }
  clean_writers        <- function(DT) {
    DT[, writers := map(writers, ~ paste0(.x, collapse = ", ")) |>
         map(~ paste0("[", .x, "]")) |> unlist()]
    
  }
  clean_rating         <- function(DT) {
    DT[, rating := as.numeric(rating)]
  }
  clean_votes          <- function(DT) {
    DT[, total_votes := gsub("\\(|\\)|,", "", total_votes) |> as.numeric()]
  } 
  clean_airdate        <- function(DT) {
    DT[, air_date := gsub("Episode aired| ", "", air_date)
       ][, air_date := as.Date(air_date, "%b %e, %Y")][]
  }
  
  dt <- copy(DT)
  
  dt <- dt |> 
        clean_episode_number() |>
        clean_directors() |>
        clean_writers() |> 
        clean_rating() |>
        clean_votes() |>
        clean_airdate()
  
  setcolorder(dt, c("season", "episode", "title",
                    "air_date", "writers", "directors",
                    "rating", "total_votes"))
  
  dt[]
}

episodes_raw <- map(buffy_seasons(), scrape_episodes) |>
  rbindlist()

episodes <- clean_episodes(episodes_raw[title != "Unaired Pilot"])

p <- episodes |>
  {\(DT) {dt <- copy(DT); dt[, title := forcats::fct_inorder(title)
  ][, episode_number := 1:nrow(dt)]}}() |>
  ggplot(aes(episode_number, rating)) +
  geom_line() + 
  geom_smooth(method = "loess") +
  geom_point(aes(colour = factor(season), size = total_votes)) + 
  geom_text_repel(aes(label = title)) +
  expand_limits(x = -1) +
  scale_y_continuous(breaks = seq(6.5, 10.0, 0.5),
                     minor_breaks = seq(6.5, 10.0, 0.5)) +
  theme(panel.grid.major = element_blank(),
        legend.position = "none") +
  labs(x = "Episode Number",
       y = "IMDb Rating",
       title = "Popularity of Buffy the Vampire Slayer Over Time",
       subtitle = "Colour represents season and size represents number of ratings") +
  scale_color_manual(values = hcl.colors(max(episodes$season), "Zissou"))

png("popularity.png", 3840, 2160, res = 300)
print(p)
dev.off()
  
fwrite(episodes, "buffy_imdb.csv")
