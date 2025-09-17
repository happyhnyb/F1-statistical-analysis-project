

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
library(tidyverse)
library(ggplot2)
library(broom)
```



1.

```{r load-data}
# Read the results data; replace 'path_to_results' with your local path to results.csv
results_path <- "results.csv"
results <- readr::read_csv(results_path, show_col_types = FALSE)

# Select relevant columns and filter out rows with missing fastest lap speed
f1_data <- results %>%
  select(grid, points, fastestLapSpeed) %>%
  filter(!is.na(fastestLapSpeed))

# Create grid groups: Front (grid ≤ 5), Back (grid > 15), Mid otherwise
f1_data <- f1_data %>%
  mutate(
    grid_group = case_when(
      grid <= 5 ~ "Front",
      grid > 15 ~ "Back",
      TRUE ~ "Mid"
    )
  )

glimpse(f1_data)
```


```{r summary-stats}
summary_stats <- f1_data %>%
  summarise(
    count = n(),
    grid_mean = mean(grid), grid_sd = sd(grid), grid_min = min(grid), grid_max = max(grid),
    speed_mean = mean(fastestLapSpeed), speed_sd = sd(fastestLapSpeed), speed_min = min(fastestLapSpeed), speed_max = max(fastestLapSpeed),
    points_mean = mean(points), points_sd = sd(points), points_min = min(points), points_max = max(points)
  )

knitr::kable(summary_stats, digits = 2, caption = "Summary statistics for key variables.")
```

.

```{r plots, fig.width=10, fig.height=8}
# Histogram of fastest lap speeds
p1 <- ggplot(f1_data, aes(x = fastestLapSpeed)) +
  geom_histogram(bins = 30, alpha = 0.7) +
  labs(title = "Distribution of Fastest Lap Speed", x = "Fastest Lap Speed (km/h)", y = "Count")

# Boxplot of fastest lap speed by grid group
p2 <- ggplot(f1_data, aes(x = grid_group, y = fastestLapSpeed, fill = grid_group)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Fastest Lap Speed by Grid Group", x = "Grid Group", y = "Fastest Lap Speed (km/h)") +
  theme(legend.position = "none")

# Scatter plots for points vs predictors
p3 <- ggplot(f1_data, aes(x = grid, y = points)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Points vs. Starting Grid Position", x = "Grid Position", y = "Points")

p4 <- ggplot(f1_data, aes(x = fastestLapSpeed, y = points)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Points vs. Fastest Lap Speed", x = "Fastest Lap Speed (km/h)", y = "Points")

patchwork::wrap_plots(p1, p2, p3, p4, ncol = 2)
```



```{r t-test}
front_speeds <- f1_data %>% filter(grid_group == "Front") %>% pull(fastestLapSpeed)
back_speeds  <- f1_data %>% filter(grid_group == "Back")  %>% pull(fastestLapSpeed)

t_test_res <- t.test(front_speeds, back_speeds, alternative = "two.sided", var.equal = FALSE)

t_test_res
```



```{r regression}
model <- lm(points ~ grid + fastestLapSpeed, data = f1_data)

summary(model)
```



```{r tidy-regression}
tidy(model, conf.int = TRUE) %>%
  mutate(across(where(is.numeric), round, 3)) %>%
  knitr::kable(caption = "Regression coefficients with 95% confidence intervals.")

glance(model) %>%
  mutate(across(where(is.numeric), round, 3)) %>%
  knitr::kable(caption = "Model performance metrics.")
```


```{r diagnostics, fig.width=10, fig.height=4}
par(mfrow = c(1, 2))
plot(model$fitted.values, model$residuals,
     main = "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2)

qqnorm(model$residuals, main = "Normal Q-Q Plot")
qqline(model$residuals)
```

