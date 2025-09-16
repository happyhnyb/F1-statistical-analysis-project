suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(broom)
  library(car)
  library(lmtest)
  library(rstatix)
  library(ggrepel)
  library(cluster)
})

na_vals <- c("\\N", "NA", "", " ")
data_dir <- "data"
out_dir  <- "outputs"
fig_dir  <- file.path(out_dir, "figures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

save_fig <- function(plot, name, w = 9, h = 6) {
  ggsave(file.path(fig_dir, name), plot = plot, width = w, height = h, dpi = 300)
}

read_csv_typed <- function(fname, colspec) {
  read_csv(file.path(data_dir, fname), col_types = colspec, na = na_vals) |> clean_names()
}

circuits               <- read_csv_typed("circuits.csv", cols())
constructors           <- read_csv_typed("constructors.csv", cols())
drivers                <- read_csv_typed("drivers.csv", cols())
races                  <- read_csv_typed("races.csv", cols())
results                <- read_csv_typed("results.csv", cols())
qualifying             <- read_csv_typed("qualifying.csv", cols())
lap_times              <- read_csv_typed("lap_times.csv", cols())
driver_standings       <- read_csv_typed("driver_standings.csv", cols())
constructor_standings  <- read_csv_typed("constructor_standings.csv", cols())
constructor_results    <- read_csv_typed("constructor_results.csv", cols())

time_to_seconds <- function(x) {
  ifelse(is.na(x) | x == "", NA_real_,
         vapply(strsplit(x, ":"), function(z) {
           if (length(z) == 2) as.numeric(z[1]) * 60 + as.numeric(z[2]) else NA_real_
         }, numeric(1)))
}

base <- results |>
  left_join(races |> select(race_id, year, round, circuit_id), by = "race_id") |>
  left_join(qualifying |> select(race_id, driver_id, position), by = c("race_id", "driver_id")) |>
  rename(quali_pos = position) |>
  mutate(fastest_lap_sec = time_to_seconds(fastest_lap_time)) |>
  group_by(race_id) |>
  mutate(race_fastest_sec = min(fastest_lap_sec, na.rm = TRUE)) |>
  ungroup() |>
  mutate(
    race_fastest_sec = ifelse(is.infinite(race_fastest_sec), NA_real_, race_fastest_sec),
    rel_fastlap      = fastest_lap_sec - race_fastest_sec
  ) |>
  left_join(
    drivers |> mutate(driver_name = paste(forename, surname)) |>
      select(driver_id, driver_name, nationality),
    by = "driver_id"
  ) |>
  left_join(constructors |> select(constructor_id, constructor_name = name), by = "constructor_id") |>
  left_join(circuits |> select(circuit_id, country, location), by = "circuit_id")

driver_season <- base |>
  group_by(year, driver_id, driver_name) |>
  summarise(
    points    = sum(points, na.rm = TRUE),
    mean_quali = mean(quali_pos, na.rm = TRUE),
    mean_rel   = mean(rel_fastlap, na.rm = TRUE),
    starts     = n(), .groups = "drop"
  )

constructor_season <- base |>
  group_by(year, constructor_id, constructor_name) |>
  summarise(
    points   = sum(points, na.rm = TRUE),
    avg_quali = mean(quali_pos, na.rm = TRUE),
    avg_rel   = mean(rel_fastlap, na.rm = TRUE),
    starts    = n(), .groups = "drop"
  )

theme_min <- function() theme_minimal(base_size = 12) + theme(panel.grid.minor = element_blank())

p1 <- ggplot(base, aes(points)) +
  geom_histogram(bins = 40) +
  labs(title = "Distribution of Race Points", x = "Points", y = "Count") +
  theme_min()
save_fig(p1, "points_distribution.png")

p2 <- ggplot(base |> filter(!is.na(quali_pos), !is.na(points)),
             aes(quali_pos, points)) +
  geom_point(alpha = .3) +
  geom_smooth(method = "lm") +
  scale_x_reverse() +
  labs(title = "Qualifying Position vs Race Points",
       x = "Qualifying Position (1 = pole, higher = worse)",
       y = "Race Points") +
  theme_min()
save_fig(p2, "quali_vs_points.png")

p3 <- base |>
  filter(year == 2021) |>
  arrange(round) |>
  group_by(constructor_name) |>
  mutate(cum_pts = cumsum(replace_na(points, 0))) |>
  ungroup() |>
  ggplot(aes(round, cum_pts, color = constructor_name)) +
  geom_line(linewidth = 1) +
  labs(title = "Cumulative Constructor Points (2021)", x = "Round", y = "Cumulative Points", color = "Constructor") +
  theme_min()
save_fig(p3, "constructor_cumulative_2021.png")

p4 <- constructor_season |>
  filter(year >= 2014, year <= 2021) |>
  ggplot(aes(year, constructor_name, fill = points)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "Constructor Season Points (2014–2021)",
       x = "Season", y = "Constructor", fill = "Points") +
  theme_min()
save_fig(p4, "constructor_heatmap_2014_2021.png")

races_by_country <- races |>
  left_join(circuits |> select(circuit_id, country), by = "circuit_id") |>
  count(country, name = "n_races") |>
  arrange(desc(n_races))

p5 <- races_by_country |>
  slice_max(n_races, n = 20, with_ties = FALSE) |>
  mutate(country = fct_reorder(country, n_races)) |>
  ggplot(aes(country, n_races)) +
  geom_col() +
  coord_flip() +
  labs(title = "Races by Country (Top 20, all-time)",
       x = "Country", y = "Number of Grands Prix") +
  theme_min()
save_fig(p5, "races_by_country_top20.png")

d <- base |> filter(!is.na(quali_pos), !is.na(points))
cor_test <- cor.test(d$quali_pos, d$points, method = "pearson")

lm_simple <- lm(points ~ quali_pos, data = d)
lm_simple_glance <- glance(lm_simple)
lm_simple_rmse   <- sqrt(mean(residuals(lm_simple)^2, na.rm = TRUE))

lm_multi <- lm(points ~ quali_pos + rel_fastlap, data = d)
lm_multi_glance <- glance(lm_multi)
lm_multi_rmse   <- sqrt(mean(residuals(lm_multi)^2, na.rm = TRUE))

cat("\n--- Hypothesis 1: Qualifying predicts points ---\n")
cat(sprintf("Pearson r = %.3f (p = %.3g)\n", cor_test$estimate, cor_test$p.value))
cat(sprintf("Simple OLS: R^2 = %.3f, RMSE = %.2f\n", lm_simple_glance$r.squared, lm_simple_rmse))
cat("Simple OLS coefficients:\n"); print(tidy(lm_simple))

cat("\n--- Multiple regression: quali + relative pace ---\n")
cat(sprintf("R^2 = %.3f, Adj R^2 = %.3f, RMSE = %.2f\n",
            lm_multi_glance$r.squared, lm_multi_glance$adj.r.squared, lm_multi_rmse))
print(tidy(lm_multi))
cat("\nHeteroskedasticity (Breusch-Pagan):\n"); print(bptest(lm_multi))
cat("\nAutocorrelation (Durbin-Watson):\n"); print(dwtest(lm_multi))

cat("\n--- Hypothesis 2: Constructor performance differences (2021) ---\n")
aov21 <- aov(points ~ constructor_name, data = filter(base, year == 2021))
print(summary(aov21))

set.seed(42)
d21 <- driver_season |>
  filter(year == 2021) |>
  transmute(driver_name, points, mean_quali, mean_rel = replace_na(mean_rel, 0))

m <- scale(d21[, c("points", "mean_quali", "mean_rel")])
km <- kmeans(m, centers = 3, nstart = 50)
d21$cluster <- factor(km$cluster)

write_csv(d21, file.path(out_dir, "driver_clusters_2021.csv"))

p6 <- as_tibble(m) |>
  mutate(driver_name = d21$driver_name, cluster = d21$cluster) |>
  ggplot(aes(PC1 <- .data[[1]], PC2 <- .data[[2]], color = cluster, label = driver_name)) +
  geom_point(size = 2, alpha = 0.8) +
  ggrepel::geom_text_repel(size = 3, show.legend = FALSE, max.overlaps = 15) +
  labs(title = "Driver Clustering (2021) — k-means (k=3)",
       x = "Scaled feature 1", y = "Scaled feature 2", color = "Cluster") +
  theme_min()
save_fig(p6, "driver_clusters_2021.png")

cat("\nAnalysis complete.\n",
    "Figures saved to outputs/figures/\n",
    "Clusters saved to outputs/driver_clusters_2021.csv\n", sep = "")
