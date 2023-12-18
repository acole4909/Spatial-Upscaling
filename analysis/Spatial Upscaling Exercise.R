##### 2.2 Random Cross-Validation
library(tidyverse)
library(dplyr)
library(here)
library(knitr)
library(tidyterra)
library(ggplot2)
library(ranger)
library(OneR)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
### Data Loading & Preparation
# Load data
dfs <- readRDS(here::here("data-raw/dfs.rds"))

# Split dataset into training and testing sets
set.seed(456)  # for reproducibility
split <- rsample::initial_split(dfs, prop = 0.7)
df_train <- rsample::training(split)
df_test <- rsample::testing(split)

# Filter out any NA to avoid error when running a Random Forest
df_train <- df_train |> tidyr::drop_na()
df_test <- df_test   |> tidyr::drop_na()

### Random Cross-Validation
pp <- recipes::recipe(leafN ~ elv+map+mat+ndep+mai+Species, data = df_train) |>
  recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) |>
  recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes())

modcv <- caret::train(
  pp,
  data = df_train %>%
    drop_na(),
  method = "ranger",
  trControl = caret::trainControl(method = "cv", number = 5, savePredictions = "final"),
  tuneGrid = expand.grid( .mtry = 3,
                          .min.node.size = 12,
                          .splitrule = "variance"),
  metric = "RMSE",
  replace = FALSE,
  sample.fraction = 0.5,
  num.trees = 100,
  seed = 456                # for reproducibility
)

metrics_byfold <- modcv$resample

saveRDS(modcv,
        here::here("data/modcv_for_leafN.rds"))

saveRDS(df_train,
        here::here("data/cal_modcv_for_leafN.rds"))

saveRDS(df_test,
        here::here("data/val_modcv_for_leafN.rds"))

### Evaluate Model
modcv <- readRDS(here::here("data/modcv_for_leafN.rds"))
df_train <- readRDS(here::here("data/cal_modcv_for_leafN.rds"))
df_test <- readRDS(here::here("data/val_modcv_for_leafN.rds"))

prediction <- predict(modcv, newdata = df_test)

df_test$pred <- prediction

metrics_test <- df_test |>
  yardstick::metrics(leafN, pred)

rmse_test <- metrics_test |>
  filter(.metric == "rmse") |>
  pull(.estimate)

rsq_test <- metrics_test |>
  filter(.metric == "rsq")

##### 2.3 Spatial Cross-Validation
### 2.3.1 Spatial Distribution of Data Across Globe
coast <- rnaturalearth::ne_coastline(scale = 110, returnclass = "sf")

ggplot() +

  # plot coastline
  geom_sf(data = coast,
          colour = 'black',
          size = 0.2) +

  # set extent in longitude and latitude
  coord_sf(
    ylim = c(-60, 80),
    expand = FALSE) +  # to draw map strictly bounded by the specified extent

  # plot points on map
  geom_point(data = dfs, aes(x = lon, y = lat), color = "red", size = 0.2) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom")

#### 2.3.2 Perform Spatial Cross Validation
### Identify geographical clusters using k-means algorithm
dfs <- as.data.frame(dfs, cell = TRUE)
clusters <- kmeans(
  dfs[, 2:3],
  centers = 5
)
dfs$cluster <- clusters$cluster
### Plot geographic clusters on map
p2 <- ggplot() +

  # plot coastline
  geom_sf(data = coast,
          colour = 'black',
          size = 0.2) +

  # set extent in longitude and latitude
  coord_sf(
    ylim = c(-60, 80),
    expand = FALSE) +  # to draw map strictly bounded by the specified extent

  # plot lat and long points on map by cluster
  geom_point(data = dfs, aes(x = lon, y = lat, group=cluster), size = 0.2) +
  scale_color_manual(values = c("1" = "blue", "2" = "green", "3" = "red", "4"="yellow", "5"="pink")) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom")

p2 +
  aes(color = factor(cluster)) +
  scale_color_discrete(name = "Cluster",
                       labels = c("Cluster 1",
                                  "Cluster 2",
                                  "Cluster 3",
                                  "Cluster 4",
                                  "Cluster 5"))

###2.3.3 Distribution of Leaf N by Cluster
p3 <- ggplot() +

  # plot coastline
  geom_sf(data = coast,
          colour = 'black',
          size = 0.2) +

  # set extent in longitude and latitude
  coord_sf(
    ylim = c(-60, 80),
    expand = FALSE) +  # to draw map strictly bounded by the specified extent

  # plot lat and long points on map by cluster
  geom_point(data = dfs, aes(x = lon, y = lat, group=cluster), color = factor(dfs$leafN), size = 0.2) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom")

p3 +
  aes(color = factor(dfs$leafN)) +
  scale_color_discrete(name = "LeafN",
                       labels = c("LeafN Cluster 1",
                                  "LeafN Cluster 2",
                                  "LeafN Cluster 3",
                                  "LeafN Cluster 4",
                                  "LeafN Cluster 5"))

###2.3.4
# create folds based on clusters assuming 'dfs' contains the data and a column called 'cluster' containing the result of the k-means clustering
library(purrr)
group_folds_train <- purrr::map(
  seq(length(unique(dfs$cluster))),
  ~ {
    dfs |>
      select(cluster) |>
      mutate(idx = 1:n()) |>
      filter(cluster != .) |>
      pull(idx)
  }
)

group_folds_test <- purrr::map(
  seq(length(unique(dfs$cluster))),
  ~ {
    dfs |>
      select(cluster) |>
      mutate(idx = 1:n()) |>
      filter(cluster == .) |>
      pull(idx)
  }
)

# create a function that trains a random forest model on a given set of rows and predicts on a disjunct set of rows

target <- dfs$leafN
is.vector(target)


train_test_by_fold <- function(dfs, idx_train, idx_val){

  mod <- ranger::ranger(
    x =  dfs[idx_train, 2:9],  # data frame with columns corresponding to predictors
    y =  target[idx_train],  # a vector of the target values (not a data frame!)
  )

  pred <- predict(mod,       # the fitted model object
                  data = dfs[idx_val, 2:9] # a data frame with columns corresponding to predictors
  )

  tmp <- dfs[idx_val,]
  tmp$preds <- predictions(pred)

  rsq <- yardstick::rsq(tmp, "leafN", "preds") # the R-squared determined on the validation set
  rmse <- yardstick::rmse(tmp, "leafN", "preds") # the root mean square error on the validation set

  return(tibble(rsq = rsq, rmse = rmse))
}

# apply function on each custom fold and collect validation results in a nice data frame
out <- purrr::map2_dfr(
  group_folds_train,
  group_folds_test,
  ~train_test_by_fold(dfs, .x, .y)
) |>
  mutate(test_fold = 1:5)
out

### 2.4 Environmental Cross-Validation
dfs <- as.data.frame(dfs, cell = TRUE)
clusters <- kmeans(
  dfs[, 5:6],
  centers = 5
)
dfs$cluster <- clusters$cluster

group_folds_train <- purrr::map(
  seq(length(unique(dfs$cluster))),
  ~ {
    dfs |>
      select(cluster) |>
      mutate(idx = 1:n()) |>
      filter(cluster != .) |>
      pull(idx)
  }
)

group_folds_test_env <- purrr::map(
  seq(length(unique(dfs$cluster))),
  ~ {
    dfs |>
      select(cluster) |>
      mutate(idx = 1:n()) |>
      filter(cluster == .) |>
      pull(idx)
  }
)


target <- dfs$leafN
is.vector(target)


train_test_by_fold <- function(dfs, idx_train, idx_val){

  mod <- ranger::ranger(
    x =  dfs[idx_train, 2:9],  # data frame with columns corresponding to predictors
    y =  target[idx_train],  # a vector of the target values (not a data frame!)
  )

  pred <- predict(mod,       # the fitted model object
                  data = dfs[idx_val, 2:9] # a data frame with columns corresponding to predictors
  )

  tmp <- dfs[idx_val,]
  tmp$preds <- predictions(pred)

  rsq <- yardstick::rsq(tmp, "leafN", "preds") # the R-squared determined on the validation set
  rmse <- yardstick::rmse(tmp, "leafN", "preds") # the root mean square error on the validation set

  return(tibble(rsq = rsq, rmse = rmse))
}

# apply function on each custom fold and collect validation results in a nice
# data frame
out <- purrr::map2_dfr(
  group_folds_train,
  group_folds_test,
  ~train_test_by_fold(dfs, .x, .y)
) |>
  mutate(test_fold = 1:5)
out
