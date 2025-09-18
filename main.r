# ============================================
# f1_analysis.R — Formula 1 Statistical Analysis (script)
# ============================================

# ---- 0) Packages ----
req_pkgs <- c("tidyverse", "broom", "patchwork")
new_pkgs <- req_pkgs[!(req_pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs)) install.packages(new_pkgs, repos = "https://cloud.r-project.org")
invisible(lapply(req_pkgs, library, character.only = TRUE))

# ---- 1) Paths & I/O ----
results_path <- "results.csv"    # update if needed
out_dir <- "figs"
if (!dir.exists(out_dir)) dir.create(out_dir)

# ---- 2) Load & Prepare Data ----
# Expecting columns: grid, points, fastestLapSpeed (mph)
if (!file.exists(results_path)) {
  stop(paste("File not found:", results_path, "\nPut results.csv beside this script or update `results_path`."))
}
results <- readr::read_csv(results_path, show_col_types = FALSE)

needed_cols <- c("grid", "points", "fastestLapSpeed")
if (!all(needed_cols %in% names(results))) {
  stop("Missing required columns. Need: grid, points, fastestLapSpeed")
}

f1_data <- results %>%
  select(grid, points, fastestLapSpeed) %>%
  mutate(
    grid = suppressWarnings(as.numeric(grid)),
    points = suppressWarnings(as.numeric(points)),
    fastestLapSpeed = suppressWarnings(as.numeric(fastestLapSpeed))
  ) %>%
  filter(!is.na(grid), !is.na(points), !is.na(fastestLapSpeed)) %>%
  mutate(
    grid_group = case_when(
      grid <= 5 ~ "Front",
      grid > 15 ~ "Back",
      TRUE ~ "Mid"
    ),
    grid_group = factor(grid_group, levels = c("Front", "Mid", "Back"))
  )

cat("Rows after cleaning:", nrow(f1_data), "\n")
suppressMessages(print(glimpse(f1_data)))

# ---- 3) Descriptive Stats ----
summary_stats <- f1_data %>%
  summarise(
    n = n(),
    grid_mean = mean(grid), grid_sd = sd(grid),
    grid_min = min(grid), grid_q1 = quantile(grid, 0.25),
    grid_med = median(grid), grid_q3 = quantile(grid, 0.75), grid_max = max(grid),

    speed_mean = mean(fastestLapSpeed), speed_sd = sd(fastestLapSpeed),
    speed_min = min(fastestLapSpeed), speed_q1 = quantile(fastestLapSpeed, 0.25),
    speed_med = median(fastestLapSpeed), speed_q3 = quantile(fastestLapSpeed, 0.75),
    speed_max = max(fastestLapSpeed),

    points_mean = mean(points), points_sd = sd(points),
    points_min = min(points), points_q1 = quantile(points, 0.25),
    points_med = median(points), points_q3 = quantile(points, 0.75),
    points_max = max(points)
  )

cat("\nSummary statistics:\n")
print(round(summary_stats, 3))
readr::write_csv(summary_stats, file.path(out_dir, "summary_stats.csv"))

# ---- 4) Plots (EDA) ----
p_hist_speed <- ggplot(f1_data, aes(x = fastestLapSpeed)) +
  geom_histogram(bins = 30, alpha = 0.8) +
  labs(title = "Distribution of Fastest Lap Speed", x = "Fastest Lap Speed (mph)", y = "Count")

p_box_speed <- ggplot(f1_data, aes(x = grid_group, y = fastestLapSpeed, fill = grid_group)) +
  geom_boxplot(alpha = 0.8) +
  labs(title = "Fastest Lap Speed by Grid Group", x = "Grid Group", y = "Fastest Lap Speed (mph)") +
  theme(legend.position = "none")

p_scatter_grid <- ggplot(f1_data, aes(x = grid, y = points)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Points vs. Starting Grid Position", x = "Grid Position", y = "Points")

p_scatter_speed <- ggplot(f1_data, aes(x = fastestLapSpeed, y = points)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Points vs. Fastest Lap Speed", x = "Fastest Lap Speed (mph)", y = "Points")

# Save plots
ggsave(file.path(out_dir, "hist_fastestLapSpeed.png"), p_hist_speed, width = 7, height = 5, dpi = 300)
ggsave(file.path(out_dir, "box_fastestLapSpeed_by_group.png"), p_box_speed, width = 7, height = 5, dpi = 300)
ggsave(file.path(out_dir, "scatter_points_vs_grid.png"), p_scatter_grid, width = 7, height = 5, dpi = 300)
ggsave(file.path(out_dir, "scatter_points_vs_speed.png"), p_scatter_speed, width = 7, height = 5, dpi = 300)

# Optional: 2x2 layout if running interactively
# (p_hist_speed | p_box_speed) / (p_scatter_grid | p_scatter_speed)

# ---- 5) T-test (Front vs Back lap speeds) ----
front_speeds <- f1_data %>% filter(grid_group == "Front") %>% pull(fastestLapSpeed)
back_speeds  <- f1_data %>% filter(grid_group == "Back")  %>% pull(fastestLapSpeed)

if (length(front_speeds) > 2 && length(back_speeds) > 2) {
  t_res <- t.test(front_speeds, back_speeds, alternative = "two.sided", var.equal = FALSE)
  cat("\nT-test (Front vs Back fastestLapSpeed):\n")
  print(t_res)
} else {
  cat("\nT-test skipped (not enough Front/Back observations).\n")
}

# ---- 6) Regression: points ~ grid + fastestLapSpeed ----
model <- lm(points ~ grid + fastestLapSpeed, data = f1_data)

cat("\nModel summary:\n")
print(summary(model))

coefs_tbl <- broom::tidy(model, conf.int = TRUE)
perf_tbl  <- broom::glance(model)

cat("\nCoefficients (with 95% CI):\n")
print(round(coefs_tbl, 4))
cat("\nPerformance metrics:\n")
print(round(perf_tbl, 4))

readr::write_csv(coefs_tbl, file.path(out_dir, "lm_coefficients.csv"))
readr::write_csv(perf_tbl,  file.path(out_dir, "lm_performance.csv"))

# ---- 7) Diagnostics ----
# Residuals vs Fitted
png(file.path(out_dir, "diag_residuals_vs_fitted.png"), width = 900, height = 600, res = 120)
plot(model$fitted.values, resid(model),
     main = "Residuals vs Fitted",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2, col = "gray40")
dev.off()

# Normal Q-Q
png(file.path(out_dir, "diag_qqplot.png"), width = 900, height = 600, res = 120)
qqnorm(resid(model), main = "Normal Q-Q Plot")
qqline(resid(model), col = "gray40")
dev.off()

cat("\nAll done. Outputs saved to 'figs/'\n")
