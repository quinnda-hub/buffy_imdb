library(data.table)
library(glmnet)
library(tidyr)
library(tidytext)
library(ggplot2)

buffy <- fread("buffy_imdb.csv")

setnames(buffy, c("directors", "writers"), c("director", "writer"))

# Remove brackets and replace with blank space. 
buffy[, director := gsub("\\[|\\]|", "", director)
      ][, writer := gsub("\\[|\\]|", "", writer)]

## Director and writer features.
# Use tidyr to engineer director and writer features.
director_writer_features <- buffy[, .(title, director, writer)] |>
  gather(type, value, director, writer) |>
  separate_rows(value, sep = ", ") |>
  unite(feature, type, value, sep = ": ") |>
  as.data.table()

# Lump uncommon directors/writers.
director_writer_features[, feature := forcats::fct_lump(
  feature, 25, other_level = "director-writer: Other")
  ][, value := 1]

## Season features.
season_features <- copy(buffy[, .(title, season)])
season_features <- season_features[, `:=` (feature = paste0("season: ", season), 
                                           value = 1)
                                   ][, .(title, feature, value)]

# Combine both sets of features.
features <- rbind(director_writer_features, season_features)

# Turn these features into a sparse matrix.
episode_feature_matrix <- features |>
  cast_sparse(title, feature, value)

ratings <- buffy$rating[match(rownames(episode_feature_matrix), buffy$title)]

# Use glmmnet to do lasso regresson.

mod <- cv.glmnet(episode_feature_matrix, ratings)

fit <- mod$glmnet.fit |>
  broom::tidy() |>
  as.data.table()

fit <- fit[lambda == mod$lambda.min
           ][term != "(Intercept)"
           ][, term := forcats::fct_reorder(term, estimate)]

theme_set(theme_light())

p <- fit |>
  ggplot(aes(term, estimate, fill = estimate > 0)) +
  geom_col() +
  coord_flip() +
  expand_limits(y = -0.75) +
  labs(y = "Estimated Effect On Rating of Episode",
       x = "",
       title = "Results of Penalized Regression (L1)") +
  theme(legend.position = "none")

png("lasso_min.png", 3840, 2160, res = 300)
print(p)  
dev.off()
