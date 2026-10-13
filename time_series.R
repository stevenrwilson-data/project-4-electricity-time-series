# California Electricity Demand — Time Series Forecasting
# ============================================================================
# Complete replacement file: rolling forecast audit and model comparison
# ============================================================================

# ============================================================================
# A. Packages
# ============================================================================
pkgs <- c(
  "ggplot2",
  "dplyr",
  "readxl",
  "lubridate",
  "patchwork",
  "tsibble",
  "feasts",
  "fable",
  "fabletools",
  "tidyr"
)

to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) {
  install.packages(to_install, dependencies = TRUE)
}

library(ggplot2)
library(dplyr)
library(readxl)
library(lubridate)
library(patchwork)
library(tsibble)
library(feasts)
library(fable)
library(fabletools)
library(tidyr)

# ============================================================================
# B. Plot theme and helpers
# ============================================================================
brand_colors <- c(
  field = "#041F2C",
  blue = "#08415C",
  plum = "#511730",
  brick = "#8E443D",
  salmon = "#F7A399",
  sand = "#E0D68A",
  plate = "#F4F0E6",
  plum_ink = "#3A0F21"
)

portfolio_theme <- theme_minimal(base_size = 15) +
  theme(
    text = element_text(family = "Montserrat", color = brand_colors["field"]),
    plot.title = element_text(
      family = "Archivo Black",
      color = brand_colors["plum"],
      size = 18
    ),
    plot.subtitle = element_text(
      family = "Montserrat",
      color = brand_colors["field"],
      size = 13
    ),
    plot.caption = element_text(
      family = "Source Code Pro",
      color = brand_colors["brick"],
      size = 11
    ),
    axis.title = element_text(
      family = "Montserrat",
      color = brand_colors["field"],
      size = 13
    ),
    axis.text = element_text(
      family = "Montserrat",
      color = brand_colors["field"],
      size = 12
    ),
    panel.grid.major = element_line(
      color = brand_colors["salmon"],
      linewidth = 0.25
    ),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = brand_colors["plate"], color = NA),
    panel.background = element_rect(fill = brand_colors["plate"], color = NA)
  )

save_plot <- function(plot, file, width = 7, height = 7) {
  if (!dir.exists("images")) {
    dir.create("images")
  }

  if (!inherits(plot, "patchwork") && !is.null(plot$labels$title)) {
    plot <- plot + labs(
      title = paste(strwrap(plot$labels$title, width = 50), collapse = "\n")
    )
  }

  ggsave(
    filename = file.path("images", file),
    plot = plot,
    width = width,
    height = height,
    dpi = 200
  )
}

mae <- function(actual, forecast) {
  mean(abs(actual - forecast), na.rm = TRUE)
}

mse_signed <- function(actual, forecast) {
  mean(forecast - actual, na.rm = TRUE)
}

# ============================================================================
# C. Data import and cleaning
# ============================================================================
data_raw <- read_excel(
  "Region_CAL.xlsx",
  sheet = "Published Hourly Data"
)

cat("\nINITIAL DATA\n")
cat("Rows:", nrow(data_raw), "\n")
cat("Columns:", ncol(data_raw), "\n")

missing_counts <- sapply(data_raw, function(x) sum(is.na(x)))
cat("Missing demand rows:", missing_counts[["Demand"]], "\n")

data <- data_raw %>%
  mutate(
    utc = with_tz(`UTC time`, tzone = "UTC"),
    demand = Demand,
    demand_forecast = `Demand forecast`
  ) %>%
  select(utc, demand, demand_forecast, everything()) %>%
  arrange(utc)

n_before <- nrow(data)
data <- data %>% filter(!is.na(demand))
cat("Rows dropped for missing demand:", n_before - nrow(data), "\n")
cat("Working demand rows:", nrow(data), "\n")

demand_ts <- data %>% as_tsibble(index = utc)

if (has_gaps(demand_ts)$.gaps) {
  warning("Gaps remain after cleaning.")
} else {
  cat("Series is regular (hourly, no gaps).\n")
}

# ============================================================================
# D. Exploratory seasonality and data-quality checks
# ============================================================================
# 1. Average hourly profile in Pacific time
p_daily <- demand_ts %>%
  mutate(hour = hour(with_tz(utc, "America/Los_Angeles"))) %>%
  ggplot(aes(hour, demand)) +
  stat_summary(
    fun = mean,
    geom = "line",
    linewidth = 1,
    color = brand_colors["plum"]
  ) +
  scale_x_continuous(breaks = seq(0, 23, 3)) +
  labs(
    title = "Average Hourly Demand Profile",
    subtitle = "California region, 2015–2026",
    x = "Hour (Pacific time)",
    y = "Demand (MW)"
  ) +
  portfolio_theme

save_plot(
  p_daily,
  "forecast-demand-daily-profile.png",
  width = 8,
  height = 5
)

# 2. Average demand by month, averaging within year-month first
p_month <- demand_ts %>%
  as_tibble() %>%
  mutate(
    year = year(utc),
    month = month(utc, label = TRUE)
  ) %>%
  group_by(year, month) %>%
  summarise(demand = mean(demand, na.rm = TRUE), .groups = "drop") %>%
  group_by(month) %>%
  summarise(demand = mean(demand, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(month, demand)) +
  geom_col(fill = brand_colors["blue"], width = 0.7) +
  labs(
    title = "Average Demand by Month",
    subtitle = "Averaged across years",
    x = NULL,
    y = "Demand (MW)"
  ) +
  portfolio_theme

save_plot(
  p_month,
  "forecast-demand-by-month.png",
  width = 8,
  height = 5
)

# 3. Daily totals — retain days with at least 20 hourly observations
daily_complete <- demand_ts %>%
  as_tibble() %>%
  mutate(date = as_date(with_tz(utc, "America/Los_Angeles"))) %>%
  group_by(date) %>%
  summarise(
    n_hours = n(),
    demand_total = sum(demand, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_hours >= 20)

p_long <- daily_complete %>%
  ggplot(aes(date, demand_total / 1e3)) +
  geom_line(color = brand_colors["field"], linewidth = 0.3) +
  labs(
    title = "Daily Total Demand",
    subtitle = "Days with at least 20 hours of data",
    x = NULL,
    y = "Daily demand (GWh)"
  ) +
  portfolio_theme

save_plot(
  p_long,
  "forecast-demand-daily-series.png",
  width = 10,
  height = 5
)

# 4. Inspect unusually low hourly demand values
low_demand_hours <- demand_ts %>%
  as_tibble() %>%
  filter(demand < 10000) %>%
  mutate(
    pacific_time = with_tz(utc, "America/Los_Angeles"),
    pacific_date = as_date(pacific_time)
  ) %>%
  select(utc, pacific_time, pacific_date, demand, demand_forecast)

cat("\nHOURS WITH DEMAND < 10,000 MW\n")
if (nrow(low_demand_hours) == 0) {
  cat("None.\n")
} else {
  print(low_demand_hours, n = Inf)
}

write.csv(
  low_demand_hours,
  "forecast-low-demand-hours.csv",
  row.names = FALSE
)

# ============================================================================
# E. Rolling day-ahead forecasting
# ============================================================================
# Evaluation window: final 90 days. Origins are 00:00 UTC, every 24 hours.
horizon_hours <- 24 * 90
cutoff <- max(demand_ts$utc) - hours(horizon_hours)

origins <- demand_ts %>%
  as_tibble() %>%
  filter(utc > cutoff, hour(utc) == 0) %>%
  pull(utc)

rolling_results <- vector("list", length(origins))

cat("\nROLLING FORECASTS\n")
cat("Candidate origins:", length(origins), "\n")

for (i in seq_along(origins)) {
  origin <- origins[i]

  hist_full <- demand_ts %>%
    filter(utc <= origin)

  hist_recent <- demand_ts %>%
    filter(
      utc > origin - days(56),
      utc <= origin
    )

  target <- demand_ts %>%
    filter(
      utc > origin,
      utc <= origin + hours(24)
    ) %>%
    arrange(utc)

  if (nrow(target) < 24) {
    next
  }

  # 24-hour seasonal naive: yesterday at the same forecast horizon.
  naive_fc <- hist_full %>%
    as_tibble() %>%
    filter(
      utc > origin - hours(24),
      utc <= origin
    ) %>%
    arrange(utc) %>%
    pull(demand)

  # Original Fourier + ARIMA specification, using all history.
  fit_full <- hist_full %>%
    model(
      fourier_full = ARIMA(
        demand ~
          fourier(period = 24, K = 3) +
          fourier(period = 168, K = 2) +
          pdq(1, 0, 1) +
          PDQ(0, 0, 0)
      )
    )

  fourier_full_fc <- fit_full %>%
    forecast(h = 24) %>%
    as_tibble() %>%
    pull(.mean)

  # Same specification, but fit only to the trailing 56 days.
  fit_recent <- hist_recent %>%
    model(
      fourier_recent = ARIMA(
        demand ~
          fourier(period = 24, K = 3) +
          fourier(period = 168, K = 2) +
          pdq(1, 0, 1) +
          PDQ(0, 0, 0)
      )
    )

  fourier_recent_fc <- fit_recent %>%
    forecast(h = 24) %>%
    as_tibble() %>%
    pull(.mean)

  rolling_results[[i]] <- tibble(
    origin = origin,
    utc = target$utc,
    horizon = as.integer(difftime(target$utc, origin, units = "hours")),
    actual = target$demand,
    naive = naive_fc,
    fourier_full = fourier_full_fc,
    fourier_recent = fourier_recent_fc
  )
}

rolling_eval <- bind_rows(rolling_results) %>%
  arrange(origin, horizon)

cat("Completed forecast origins:", n_distinct(rolling_eval$origin), "\n")
cat("Forecast rows:", nrow(rolling_eval), "\n")

# ============================================================================
# F. EIA forecast timestamp alignment audit
# ============================================================================
# First score each published forecast row against demand on the same row,
# the following hour, and the previous hour over the rolling evaluation span.
eval_start <- min(rolling_eval$utc)
eval_end <- max(rolling_eval$utc)

alignment_source <- data %>%
  transmute(
    utc,
    demand,
    demand_forecast,
    demand_next = lead(demand),
    demand_prev = lag(demand)
  ) %>%
  filter(utc >= eval_start, utc <= eval_end)

alignment_results <- tibble(
  alignment = c(
    "Same row: forecast[t] vs demand[t]",
    "Next hour: forecast[t] vs demand[t+1]",
    "Previous hour: forecast[t] vs demand[t-1]"
  ),
  shift_hours = c(0L, 1L, -1L),
  mae_mw = c(
    mae(alignment_source$demand, alignment_source$demand_forecast),
    mae(alignment_source$demand_next, alignment_source$demand_forecast),
    mae(alignment_source$demand_prev, alignment_source$demand_forecast)
  ),
  mean_signed_error_mw = c(
    mse_signed(alignment_source$demand, alignment_source$demand_forecast),
    mse_signed(alignment_source$demand_next, alignment_source$demand_forecast),
    mse_signed(alignment_source$demand_prev, alignment_source$demand_forecast)
  )
)

cat("\nEIA FORECAST ALIGNMENT TEST\n")
print(alignment_results)

write.csv(
  alignment_results,
  "forecast-eia-alignment.csv",
  row.names = FALSE
)

best_shift <- alignment_results %>%
  slice_min(mae_mw, n = 1, with_ties = FALSE) %>%
  pull(shift_hours)

best_alignment_label <- alignment_results %>%
  filter(shift_hours == best_shift) %>%
  pull(alignment)

cat("Best EIA alignment by MAE:", best_alignment_label, "\n")

# Build all three row alignments for the actual target timestamps.
# If forecast[t] best matches demand[t+1], then demand at time t uses forecast[t-1].
official_by_actual <- data %>%
  transmute(
    utc,
    official_same = demand_forecast,
    official_if_next_hour = lag(demand_forecast),
    official_if_previous_hour = lead(demand_forecast)
  )

rolling_eval <- rolling_eval %>%
  left_join(official_by_actual, by = "utc") %>%
  mutate(
    official = case_when(
      best_shift == 0L ~ official_same,
      best_shift == 1L ~ official_if_next_hour,
      best_shift == -1L ~ official_if_previous_hour,
      TRUE ~ NA_real_
    )
  )

# ============================================================================
# G. Evaluation
# ============================================================================
model_mae <- tibble(
  model = c(
    "Seasonal naive 24h",
    "Fourier ARIMA — all history",
    "Fourier ARIMA — trailing 56 days",
    "Official EIA — best tested alignment"
  ),
  mae_mw = c(
    mae(rolling_eval$actual, rolling_eval$naive),
    mae(rolling_eval$actual, rolling_eval$fourier_full),
    mae(rolling_eval$actual, rolling_eval$fourier_recent),
    mae(rolling_eval$actual, rolling_eval$official)
  )
) %>%
  arrange(mae_mw)

cat("\nROLLING DAY-AHEAD MAE (MW)\n")
print(model_mae)

write.csv(
  model_mae,
  "forecast-model-mae.csv",
  row.names = FALSE
)

# 1. Error by forecast horizon, h = 1...24
error_by_horizon <- rolling_eval %>%
  group_by(horizon) %>%
  summarise(
    naive = mae(actual, naive),
    fourier_full = mae(actual, fourier_full),
    fourier_recent = mae(actual, fourier_recent),
    official = mae(actual, official),
    .groups = "drop"
  )

cat("\nMAE BY FORECAST HORIZON\n")
print(error_by_horizon, n = 24)

write.csv(
  error_by_horizon,
  "forecast-error-by-horizon.csv",
  row.names = FALSE
)

error_by_horizon_long <- error_by_horizon %>%
  pivot_longer(
    cols = -horizon,
    names_to = "model",
    values_to = "mae_mw"
  ) %>%
  mutate(
    model = recode(
      model,
      naive = "Naive 24h",
      fourier_full = "Fourier ARIMA — all history",
      fourier_recent = "Fourier ARIMA — trailing 56d",
      official = "Official EIA"
    )
  )

p_horizon <- ggplot(
  error_by_horizon_long,
  aes(horizon, mae_mw, color = model, linetype = model)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.0) +
  scale_x_continuous(breaks = seq(1, 24, 2)) +
  scale_color_manual(values = c(
    "Naive 24h" = unname(brand_colors["brick"]),
    "Fourier ARIMA — all history" = unname(brand_colors["plum"]),
    "Fourier ARIMA — trailing 56d" = unname(brand_colors["blue"]),
    "Official EIA" = unname(brand_colors["field"])
  )) +
  scale_linetype_manual(values = c(
    "Naive 24h" = "solid",
    "Fourier ARIMA — all history" = "solid",
    "Fourier ARIMA — trailing 56d" = "dashed",
    "Official EIA" = "dotdash"
  )) +
  labs(
    title = "Absolute Error by Forecast Horizon",
    subtitle = "Rolling day-ahead evaluation",
    x = "Forecast horizon (hours ahead)",
    y = "MAE (MW)",
    color = NULL,
    linetype = NULL
  ) +
  portfolio_theme +
  theme(legend.position = "bottom")

save_plot(
  p_horizon,
  "forecast-error-by-horizon.png",
  width = 9,
  height = 5.5
)

# 2. Hour-of-day error for methods whose forecast is not recursively degrading
# with horizon in the same way: seasonal naive and aligned official benchmark.
error_by_hour <- rolling_eval %>%
  mutate(hour = hour(with_tz(utc, "America/Los_Angeles"))) %>%
  group_by(hour) %>%
  summarise(
    naive = mae(actual, naive),
    official = mae(actual, official),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(naive, official),
    names_to = "model",
    values_to = "mae_mw"
  ) %>%
  mutate(
    model = recode(
      model,
      naive = "Naive 24h",
      official = "Official EIA"
    )
  )

p_hour <- ggplot(error_by_hour, aes(hour, mae_mw, color = model)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.4) +
  scale_x_continuous(breaks = seq(0, 23, 3)) +
  scale_color_manual(values = c(
    "Naive 24h" = unname(brand_colors["brick"]),
    "Official EIA" = unname(brand_colors["field"])
  )) +
  labs(
    title = "Absolute Error by Hour of Day",
    subtitle = "Seasonal naive and aligned EIA benchmark — Pacific time",
    x = "Hour (Pacific time)",
    y = "MAE (MW)",
    color = NULL
  ) +
  portfolio_theme +
  theme(legend.position = "bottom")

save_plot(
  p_hour,
  "forecast-error-by-hour.png",
  width = 8,
  height = 5
)

# 3. Forecast vs actual — final 14 days
last_14 <- rolling_eval %>%
  filter(utc >= max(utc) - days(14))

last_14_long <- last_14 %>%
  select(
    utc,
    Actual = actual,
    `Naive 24h` = naive,
    `Fourier ARIMA — all history` = fourier_full,
    `Fourier ARIMA — trailing 56d` = fourier_recent,
    `Official EIA` = official
  ) %>%
  pivot_longer(
    cols = -utc,
    names_to = "series",
    values_to = "mw"
  )

p_fc <- ggplot(last_14_long, aes(utc, mw, color = series, linetype = series)) +
  geom_line(linewidth = 0.65) +
  scale_color_manual(values = c(
    "Actual" = unname(brand_colors["field"]),
    "Naive 24h" = unname(brand_colors["brick"]),
    "Fourier ARIMA — all history" = unname(brand_colors["salmon"]),
    "Fourier ARIMA — trailing 56d" = unname(brand_colors["blue"]),
    "Official EIA" = unname(brand_colors["plum"])
  )) +
  scale_linetype_manual(values = c(
    "Actual" = "solid",
    "Naive 24h" = "dashed",
    "Fourier ARIMA — all history" = "solid",
    "Fourier ARIMA — trailing 56d" = "dashed",
    "Official EIA" = "dotdash"
  )) +
  labs(
    title = "Forecast vs Actual — Last 14 Days",
    subtitle = "Rolling day-ahead evaluation",
    x = NULL,
    y = "Demand (MW)",
    color = NULL,
    linetype = NULL
  ) +
  portfolio_theme +
  theme(legend.position = "bottom")

save_plot(
  p_fc,
  "forecast-vs-actual-14d.png",
  width = 10,
  height = 5.5
)

# ============================================================================
# H. Compact console summary
# ============================================================================
cat("\n============================================================\n")
cat("FINAL AUDIT SUMMARY\n")
cat("============================================================\n")
cat("Best EIA alignment:", best_alignment_label, "\n\n")
cat("Alignment test:\n")
print(alignment_results)
cat("\nOverall rolling MAE:\n")
print(model_mae)
cat("\nLow-demand hours (<10,000 MW):", nrow(low_demand_hours), "\n")
cat("\nSaved audit tables:\n")
cat("  forecast-eia-alignment.csv\n")
cat("  forecast-model-mae.csv\n")
cat("  forecast-error-by-horizon.csv\n")
cat("  forecast-low-demand-hours.csv\n")
cat("\nSaved figures:\n")
cat("  images/forecast-demand-daily-profile.png\n")
cat("  images/forecast-demand-by-month.png\n")
cat("  images/forecast-demand-daily-series.png\n")
cat("  images/forecast-error-by-horizon.png\n")
cat("  images/forecast-error-by-hour.png\n")
cat("  images/forecast-vs-actual-14d.png\n")
