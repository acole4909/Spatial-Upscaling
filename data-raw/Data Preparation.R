library(tidyverse)
df <- readr::read_csv("https://raw.githubusercontent.com/stineb/leafnp_data/main/data/leafnp_tian_et_al.csv")
common_species <- df |>
  group_by(Species) |>
  summarise(count = n()) |>
  arrange(desc(count)) |>
  slice(1:50) |>
  pull(Species)

dfs <- df |>
  dplyr::select(leafN, lon, lat, elv, mat, map, ndep, mai, Species) |>
  filter(Species %in% common_species)
# group_by(lon, lat) |>
# summarise(across(where(is.numeric), mean))

# quick overview of data
skimr::skim(dfs)

# show missing data
visdat::vis_miss(dfs)

if (!dir.exists(here::here("data-raw"))) system(paste0("mkdir ", here::here("data")))
saveRDS(dfs,
        here::here("data-raw/dfs.rds"))
