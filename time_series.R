# California Electricity Demand — Time Series Forecasting
# =============================================================================
#
# TABLE OF CONTENTS
# =============================================================================
#
# A. Packages
#
# B. Plot theme and helper functions
#    1. Brand colors
#    2. Plot theme
#    3. Plot-saving helper
#    4. Accuracy helpers
#
# C. Data import and cleaning
#    1. Import EIA-930 data
#    2. Construct UTC demand series
#    3. Remove missing demand rows
#    4. Confirm hourly regularity
#
# D. Exploratory analysis and data-quality audit
#    1. Average hourly demand profile
#    2. Average demand by month
#    3. Daily demand across the full series
#    4. Audit unusually low demand values
#
# E. Rolling day-ahead forecasting
#    1. Define 90-day evaluation window
#    2. Load cached forecasts or generate them
#
# F. Rolling forecast evaluation
#    1. EIA timestamp-alignment audit
#    2. Add 168-hour seasonal naive
#    3. Add aligned official EIA benchmark
#    4. Construct fixed 50/50 ensemble
#    5. Overall rolling accuracy
#
# G. Rolling forecast diagnostics
#    1. Add Pacific-time fields and errors
#    2. EIA signed error by hour
#    3. Weekday vs weekend performance
#    4. High-demand performance
#    5. Daily model winners
#    6. Final 14-day two-panel figure
#
# H. Multi-origin forecast evaluation
#    1. Define four forecast origins per day
#    2. Load cached forecasts or generate them
#    3. Add Pacific-time fields and errors
#    4. Overall multi-origin accuracy
#    5. Error by forecast horizon
#    6. Error by hour of day
#    7. Final horizon figure
#    8. Final hour-of-day figure
#
# I. Compact console summary
#
# =============================================================================


# =============================================================================
# A. Packages
# =============================================================================

pkgs <- c(
  "ggplot2",
  "dplyr",
  "readxl",
  "lubridate",
  "tsibble",
  "feasts",
  "fable",
  "fabletools",
  "tidyr"
)

to_install <- pkgs[
  !pkgs %in% installed.packages()[, "Package"]
]

if (length(to_install) > 0) {
  install.packages(
    to_install,
    dependencies = TRUE
  )
}

library(ggplot2)
library(dplyr)
library(readxl)
library(lubridate)
library(tsibble)
library(feasts)
library(fable)
library(fabletools)
library(tidyr)


# =============================================================================
# B. Plot theme and helper functions
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Brand colors
# -----------------------------------------------------------------------------

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


# -----------------------------------------------------------------------------
# 2. Plot theme
# -----------------------------------------------------------------------------

portfolio_theme <- theme_minimal(
  base_size = 15
) +
  theme(
    text = element_text(
      family = "Montserrat",
      color = brand_colors["field"]
    ),

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
      size = 10
    ),

    axis.title = element_text(
      family = "Montserrat",
      color = brand_colors["field"],
      size = 13
    ),

    axis.text = element_text(
      family = "Montserrat",
      color = brand_colors["field"],
      size = 11
    ),

    legend.text = element_text(
      family = "Montserrat",
      color = brand_colors["field"],
      size = 10
    ),

    legend.background = element_rect(
      fill = brand_colors["plate"],
      color = NA
    ),

    panel.grid.major = element_line(
      color = brand_colors["salmon"],
      linewidth = 0.25
    ),

    panel.grid.minor = element_blank(),

    plot.background = element_rect(
      fill = brand_colors["plate"],
      color = NA
    ),

    panel.background = element_rect(
      fill = brand_colors["plate"],
      color = NA
    )
  )


# -----------------------------------------------------------------------------
# 3. Plot-saving helper
# -----------------------------------------------------------------------------

save_plot <- function(
  plot,
  file,
  width = 8,
  height = 5
) {

  if (!dir.exists("images")) {
    dir.create("images")
  }

  if (!is.null(plot$labels$title)) {

    plot <- plot +
      labs(
        title = paste(
          strwrap(
            plot$labels$title,
            width = 50
          ),
          collapse = "\n"
        )
      )
  }

  ggsave(
    filename = file.path(
      "images",
      file
    ),
    plot = plot,
    width = width,
    height = height,
    dpi = 200
  )
}


# -----------------------------------------------------------------------------
# 4. Accuracy helpers
# -----------------------------------------------------------------------------

mae <- function(
  actual,
  forecast
) {

  mean(
    abs(
      actual - forecast
    ),
    na.rm = TRUE
  )
}


rmse <- function(
  actual,
  forecast
) {

  sqrt(
    mean(
      (
        actual - forecast
      )^2,
      na.rm = TRUE
    )
  )
}


mean_signed_error <- function(
  actual,
  forecast
) {

  mean(
    forecast - actual,
    na.rm = TRUE
  )
}


# =============================================================================
# C. Data import and cleaning
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Import EIA-930 data
# -----------------------------------------------------------------------------

data_raw <- read_excel(
  "Region_CAL.xlsx",
  sheet = "Published Hourly Data",
  guess_max = 100000
)

cat("\nINITIAL DATA\n")

cat(
  "Rows:",
  nrow(data_raw),
  "\n"
)

cat(
  "Columns:",
  ncol(data_raw),
  "\n"
)

missing_counts <- sapply(
  data_raw,
  function(x) sum(is.na(x))
)

cat(
  "Missing demand rows:",
  missing_counts[["Demand"]],
  "\n"
)


# -----------------------------------------------------------------------------
# 2. Construct UTC demand series
# -----------------------------------------------------------------------------

data <- data_raw %>%
  mutate(
    utc = with_tz(
      `UTC time`,
      tzone = "UTC"
    ),

    demand = Demand,

    demand_forecast = `Demand forecast`
  ) %>%
  select(
    utc,
    demand,
    demand_forecast,
    everything()
  ) %>%
  arrange(
    utc
  )


# -----------------------------------------------------------------------------
# 3. Remove missing demand rows
# -----------------------------------------------------------------------------

n_before <- nrow(data)

data <- data %>%
  filter(
    !is.na(demand)
  )

cat(
  "Rows dropped for missing demand:",
  n_before - nrow(data),
  "\n"
)

cat(
  "Working demand rows:",
  nrow(data),
  "\n"
)


# -----------------------------------------------------------------------------
# 4. Confirm hourly regularity
# -----------------------------------------------------------------------------

demand_ts <- data %>%
  as_tsibble(
    index = utc
  )

if (has_gaps(demand_ts)$.gaps) {

  warning(
    "Gaps remain after cleaning."
  )

} else {

  cat(
    "Series is regular (hourly, no gaps).\n"
  )
}


# =============================================================================
# D. Exploratory analysis and data-quality audit
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Average hourly demand profile
# -----------------------------------------------------------------------------

p_daily <- demand_ts %>%
  mutate(
    hour_pacific = hour(
      with_tz(
        utc,
        "America/Los_Angeles"
      )
    )
  ) %>%
  ggplot(
    aes(
      x = hour_pacific,
      y = demand
    )
  ) +
  stat_summary(
    fun = mean,
    geom = "line",
    linewidth = 1,
    color = brand_colors["plum"]
  ) +
  scale_x_continuous(
    breaks = seq(
      0,
      23,
      3
    )
  ) +
  scale_y_continuous(
    labels = scales::label_comma()
  ) +
  labs(
    title = "Average Hourly Demand Profile",
    subtitle = "California region, July 2015 – August 2026",
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


# -----------------------------------------------------------------------------
# 2. Average demand by month
# -----------------------------------------------------------------------------

monthly_summary <- demand_ts %>%
  as_tibble() %>%
  mutate(
    year = year(
      utc
    ),

    month = month(
      utc,
      label = TRUE
    )
  ) %>%
  group_by(
    year,
    month
  ) %>%
  summarise(
    demand = mean(
      demand,
      na.rm = TRUE
    ),

    .groups = "drop"
  ) %>%
  group_by(
    month
  ) %>%
  summarise(
    demand = mean(
      demand,
      na.rm = TRUE
    ),

    .groups = "drop"
  )

p_month <- monthly_summary %>%
  ggplot(
    aes(
      x = month,
      y = demand
    )
  ) +
  geom_col(
    fill = brand_colors["blue"],
    width = 0.7
  ) +
  scale_y_continuous(
    labels = scales::label_comma()
  ) +
  labs(
    title = "Average Demand by Month",
    subtitle = "Averaged across available years",
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


# -----------------------------------------------------------------------------
# 3. Daily demand across the full series
# -----------------------------------------------------------------------------

daily_complete <- demand_ts %>%
  as_tibble() %>%
  mutate(
    date_pacific = as_date(
      with_tz(
        utc,
        "America/Los_Angeles"
      )
    )
  ) %>%
  group_by(
    date_pacific
  ) %>%
  summarise(
    n_hours = n(),

    demand_total = sum(
      demand,
      na.rm = TRUE
    ),

    .groups = "drop"
  ) %>%
  filter(
    n_hours >= 20
  )

p_long <- daily_complete %>%
  ggplot(
    aes(
      x = date_pacific,
      y = demand_total / 1000
    )
  ) +
  geom_line(
    color = brand_colors["field"],
    linewidth = 0.3
  ) +
  scale_y_continuous(
    labels = scales::label_comma()
  ) +
  labs(
    title = "Daily Total Demand",
    subtitle = "California region — source anomalies retained",
    x = NULL,
    y = "Daily demand (GWh)",
    caption = "Only the November 2024 anomaly is visible at daily resolution."
  ) +
  portfolio_theme

save_plot(
  p_long,
  "forecast-demand-daily-series.png",
  width = 10,
  height = 5
)


# -----------------------------------------------------------------------------
# 4. Audit unusually low demand values
# -----------------------------------------------------------------------------
# These observations are retained rather than silently removed.
# They therefore remain available to the full-history forecasting model.

low_demand_hours <- demand_ts %>%
  as_tibble() %>%
  filter(
    demand < 10000
  ) %>%
  mutate(
    pacific_time = with_tz(
      utc,
      "America/Los_Angeles"
    ),

    pacific_date = as_date(
      pacific_time
    )
  ) %>%
  select(
    utc,
    pacific_time,
    pacific_date,
    demand,
    demand_forecast
  )

cat(
  "\nHOURS WITH DEMAND < 10,000 MW\n"
)

if (nrow(low_demand_hours) == 0) {

  cat(
    "None.\n"
  )

} else {

  print(
    low_demand_hours,
    n = Inf
  )
}

write.csv(
  low_demand_hours,
  "forecast-low-demand-hours.csv",
  row.names = FALSE
)


# =============================================================================
# E. Rolling day-ahead forecasting
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Define 90-day evaluation window
# -----------------------------------------------------------------------------

horizon_hours <- 24 * 90

cutoff <- max(
  demand_ts$utc
) - hours(
  horizon_hours
)

origins <- demand_ts %>%
  as_tibble() %>%
  filter(
    utc > cutoff,
    hour(utc) == 0
  ) %>%
  pull(
    utc
  )

rolling_cache_file <- "forecast-rolling-eval.rds"


# -----------------------------------------------------------------------------
# 2. Load cached forecasts or generate them
# -----------------------------------------------------------------------------

if (file.exists(rolling_cache_file)) {

  cat(
    "\nLOADING CACHED ROLLING FORECASTS\n"
  )

  rolling_eval <- readRDS(
    rolling_cache_file
  )

  rolling_eval <- rolling_eval %>%
    select(
      origin,
      utc,
      horizon,
      actual,
      naive,
      fourier_full,
      fourier_recent
    )

} else {

  rolling_results <- vector(
    "list",
    length(origins)
  )

  cat(
    "\nROLLING DAY-AHEAD FORECASTS\n"
  )

  cat(
    "Candidate origins:",
    length(origins),
    "\n"
  )

  for (i in seq_along(origins)) {

    origin <- origins[i]

    cat(
      "Origin",
      i,
      "of",
      length(origins),
      ":",
      as.character(origin),
      "\n"
    )

    hist_full <- demand_ts %>%
      filter(
        utc <= origin
      )

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
      arrange(
        utc
      )

    if (nrow(target) != 24) {
      next
    }

    naive_fc <- hist_full %>%
      as_tibble() %>%
      filter(
        utc > origin - hours(24),
        utc <= origin
      ) %>%
      arrange(
        utc
      ) %>%
      pull(
        demand
      )

    if (length(naive_fc) != 24) {
      next
    }

    fit_full <- hist_full %>%
      model(
        fourier_full = ARIMA(
          demand ~
            fourier(
              period = 24,
              K = 3
            ) +
            fourier(
              period = 168,
              K = 2
            ) +
            pdq(
              1,
              0,
              1
            ) +
            PDQ(
              0,
              0,
              0
            )
        )
      )

    fourier_full_fc <- fit_full %>%
      forecast(
        h = 24
      ) %>%
      as_tibble() %>%
      pull(
        .mean
      )

    fit_recent <- hist_recent %>%
      model(
        fourier_recent = ARIMA(
          demand ~
            fourier(
              period = 24,
              K = 3
            ) +
            fourier(
              period = 168,
              K = 2
            ) +
            pdq(
              1,
              0,
              1
            ) +
            PDQ(
              0,
              0,
              0
            )
        )
      )

    fourier_recent_fc <- fit_recent %>%
      forecast(
        h = 24
      ) %>%
      as_tibble() %>%
      pull(
        .mean
      )

    rolling_results[[i]] <- tibble(
      origin = origin,

      utc = target$utc,

      horizon = as.integer(
        difftime(
          target$utc,
          origin,
          units = "hours"
        )
      ),

      actual = target$demand,

      naive = naive_fc,

      fourier_full = fourier_full_fc,

      fourier_recent = fourier_recent_fc
    )
  }

  rolling_eval <- bind_rows(
    rolling_results
  ) %>%
    arrange(
      origin,
      horizon
    )

  saveRDS(
    rolling_eval,
    rolling_cache_file
  )
}

cat(
  "Completed rolling forecast origins:",
  n_distinct(
    rolling_eval$origin
  ),
  "\n"
)

cat(
  "Rolling forecast rows:",
  nrow(
    rolling_eval
  ),
  "\n"
)


# =============================================================================
# F. Rolling forecast evaluation
# =============================================================================


# -----------------------------------------------------------------------------
# 1. EIA timestamp-alignment audit
# -----------------------------------------------------------------------------

eval_start <- min(
  rolling_eval$utc
)

eval_end <- max(
  rolling_eval$utc
)

alignment_source <- data %>%
  transmute(
    utc,

    demand,

    demand_forecast,

    demand_next = lead(
      demand
    ),

    demand_prev = lag(
      demand
    )
  ) %>%
  filter(
    utc >= eval_start,
    utc <= eval_end
  )

alignment_results <- tibble(
  alignment = c(
    "Same row: forecast[t] vs demand[t]",
    "Next hour: forecast[t] vs demand[t+1]",
    "Previous hour: forecast[t] vs demand[t-1]"
  ),

  shift_hours = c(
    0L,
    1L,
    -1L
  ),

  mae_mw = c(
    mae(
      alignment_source$demand,
      alignment_source$demand_forecast
    ),

    mae(
      alignment_source$demand_next,
      alignment_source$demand_forecast
    ),

    mae(
      alignment_source$demand_prev,
      alignment_source$demand_forecast
    )
  ),

  mean_signed_error_mw = c(
    mean_signed_error(
      alignment_source$demand,
      alignment_source$demand_forecast
    ),

    mean_signed_error(
      alignment_source$demand_next,
      alignment_source$demand_forecast
    ),

    mean_signed_error(
      alignment_source$demand_prev,
      alignment_source$demand_forecast
    )
  )
)

cat(
  "\nEIA FORECAST ALIGNMENT TEST\n\n"
)

print(
  alignment_results
)

write.csv(
  alignment_results,
  "forecast-eia-alignment.csv",
  row.names = FALSE
)

best_shift <- alignment_results %>%
  slice_min(
    mae_mw,
    n = 1,
    with_ties = FALSE
  ) %>%
  pull(
    shift_hours
  )

best_alignment_label <- alignment_results %>%
  filter(
    shift_hours == best_shift
  ) %>%
  pull(
    alignment
  )

cat(
  "\nBest EIA alignment by MAE:",
  best_alignment_label,
  "\n"
)


# -----------------------------------------------------------------------------
# 2. Add 168-hour seasonal naive
# -----------------------------------------------------------------------------

evaluation_lookup <- demand_ts %>%
  as_tibble() %>%
  select(
    utc,
    demand,
    demand_forecast
  )

naive_168_lookup <- evaluation_lookup %>%
  transmute(
    utc = utc + hours(168),
    naive_168 = demand
  )

rolling_eval <- rolling_eval %>%
  left_join(
    naive_168_lookup,
    by = "utc"
  )


# -----------------------------------------------------------------------------
# 3. Add aligned official EIA benchmark
# -----------------------------------------------------------------------------

official_by_actual <- data %>%
  transmute(
    utc,

    official_same = demand_forecast,

    official_if_next_hour = lag(
      demand_forecast
    ),

    official_if_previous_hour = lead(
      demand_forecast
    )
  )

rolling_eval <- rolling_eval %>%
  left_join(
    official_by_actual,
    by = "utc"
  )

if (best_shift == 0L) {

  rolling_eval$official <- rolling_eval$official_same

} else if (best_shift == 1L) {

  rolling_eval$official <- rolling_eval$official_if_next_hour

} else if (best_shift == -1L) {

  rolling_eval$official <- rolling_eval$official_if_previous_hour

} else {

  rolling_eval$official <- NA_real_
}

official_model_label <- paste0(
  "Official EIA forecast — ",
  best_alignment_label
)


# -----------------------------------------------------------------------------
# 4. Construct fixed 50/50 ensemble
# -----------------------------------------------------------------------------
# The weights are fixed at 50/50.
# They are not fitted to minimize evaluation-period error.

rolling_eval <- rolling_eval %>%
  mutate(
    ensemble = (
      naive +
        fourier_recent
    ) / 2
  )


# -----------------------------------------------------------------------------
# 5. Overall rolling accuracy
# -----------------------------------------------------------------------------

rolling_scores <- tibble(
  model = c(
    "Naive/Fourier 50/50 ensemble",
    "Seasonal naive 24h",
    "Fourier + ARIMA — trailing 56 days",
    "Fourier + ARIMA — full history",
    "Seasonal naive 168h",
    official_model_label
  ),

  MAE = c(
    mae(
      rolling_eval$actual,
      rolling_eval$ensemble
    ),

    mae(
      rolling_eval$actual,
      rolling_eval$naive
    ),

    mae(
      rolling_eval$actual,
      rolling_eval$fourier_recent
    ),

    mae(
      rolling_eval$actual,
      rolling_eval$fourier_full
    ),

    mae(
      rolling_eval$actual,
      rolling_eval$naive_168
    ),

    mae(
      rolling_eval$actual,
      rolling_eval$official
    )
  ),

  RMSE = c(
    rmse(
      rolling_eval$actual,
      rolling_eval$ensemble
    ),

    rmse(
      rolling_eval$actual,
      rolling_eval$naive
    ),

    rmse(
      rolling_eval$actual,
      rolling_eval$fourier_recent
    ),

    rmse(
      rolling_eval$actual,
      rolling_eval$fourier_full
    ),

    rmse(
      rolling_eval$actual,
      rolling_eval$naive_168
    ),

    rmse(
      rolling_eval$actual,
      rolling_eval$official
    )
  ),

  Bias = c(
    mean_signed_error(
      rolling_eval$actual,
      rolling_eval$ensemble
    ),

    mean_signed_error(
      rolling_eval$actual,
      rolling_eval$naive
    ),

    mean_signed_error(
      rolling_eval$actual,
      rolling_eval$fourier_recent
    ),

    mean_signed_error(
      rolling_eval$actual,
      rolling_eval$fourier_full
    ),

    mean_signed_error(
      rolling_eval$actual,
      rolling_eval$naive_168
    ),

    mean_signed_error(
      rolling_eval$actual,
      rolling_eval$official
    )
  )
) %>%
  arrange(
    MAE
  )

cat(
  "\nROLLING 24-HOUR DAY-AHEAD RESULTS\n\n"
)

print(
  rolling_scores,
  n = Inf
)

write.csv(
  rolling_scores,
  "forecast-rolling-model-scores.csv",
  row.names = FALSE
)


# =============================================================================
# G. Rolling forecast diagnostics
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Add Pacific-time fields and errors
# -----------------------------------------------------------------------------

rolling_eval <- rolling_eval %>%
  mutate(
    pacific_time = with_tz(
      utc,
      "America/Los_Angeles"
    ),

    hour_pacific = hour(
      pacific_time
    ),

    weekday = wday(
      pacific_time,
      label = TRUE,
      abbr = FALSE
    ),

    is_weekend = weekday %in% c(
      "Saturday",
      "Sunday"
    ),

    abs_err_naive = abs(
      actual - naive
    ),

    abs_err_naive_168 = abs(
      actual - naive_168
    ),

    abs_err_fourier_full = abs(
      actual - fourier_full
    ),

    abs_err_fourier_recent = abs(
      actual - fourier_recent
    ),

    abs_err_ensemble = abs(
      actual - ensemble
    ),

    abs_err_official = abs(
      actual - official
    ),

    signed_err_official = official - actual
  )


# -----------------------------------------------------------------------------
# 2. EIA signed error by hour
# -----------------------------------------------------------------------------

eia_signed_by_hour <- rolling_eval %>%
  group_by(
    hour_pacific
  ) %>%
  summarise(
    mean_signed_error = mean(
      signed_err_official,
      na.rm = TRUE
    ),

    n = sum(
      !is.na(signed_err_official)
    ),

    .groups = "drop"
  )

eia_signed_summary <- rolling_eval %>%
  summarise(
    overall_mean_signed_error = mean(
      signed_err_official,
      na.rm = TRUE
    ),

    mean_signed_error_08_16 = mean(
      signed_err_official[
        hour_pacific >= 8 &
          hour_pacific <= 16
      ],
      na.rm = TRUE
    ),

    mean_signed_error_other_hours = mean(
      signed_err_official[
        hour_pacific < 8 |
          hour_pacific > 16
      ],
      na.rm = TRUE
    ),

    sum_signed_error_08_16 = sum(
      signed_err_official[
        hour_pacific >= 8 &
          hour_pacific <= 16
      ],
      na.rm = TRUE
    ),

    sum_signed_error_other_hours = sum(
      signed_err_official[
        hour_pacific < 8 |
          hour_pacific > 16
      ],
      na.rm = TRUE
    ),

    share_of_net_signed_error_08_16 =
      sum(
        signed_err_official[
          hour_pacific >= 8 &
            hour_pacific <= 16
        ],
        na.rm = TRUE
      ) /
      sum(
        signed_err_official,
        na.rm = TRUE
      )
  )

cat(
  "\nEIA MEAN SIGNED ERROR BY PACIFIC HOUR\n\n"
)

print(
  eia_signed_by_hour,
  n = 24
)

cat(
  "\nEIA SIGNED-ERROR SUMMARY\n\n"
)

print(
  eia_signed_summary
)

write.csv(
  eia_signed_by_hour,
  "forecast-eia-signed-error-by-hour.csv",
  row.names = FALSE
)

write.csv(
  eia_signed_summary,
  "forecast-eia-signed-error-summary.csv",
  row.names = FALSE
)


# -----------------------------------------------------------------------------
# 3. Weekday vs weekend performance
# -----------------------------------------------------------------------------

error_by_weekpart <- rolling_eval %>%
  mutate(
    weekpart = if_else(
      is_weekend,
      "Weekend",
      "Weekday"
    )
  ) %>%
  group_by(
    weekpart
  ) %>%
  summarise(
    n_hours = n(),

    n_days = n_distinct(
      as_date(
        pacific_time
      )
    ),

    naive_24 = mean(
      abs_err_naive,
      na.rm = TRUE
    ),

    naive_168 = mean(
      abs_err_naive_168,
      na.rm = TRUE
    ),

    fourier_full = mean(
      abs_err_fourier_full,
      na.rm = TRUE
    ),

    fourier_recent = mean(
      abs_err_fourier_recent,
      na.rm = TRUE
    ),

    ensemble = mean(
      abs_err_ensemble,
      na.rm = TRUE
    ),

    official = mean(
      abs_err_official,
      na.rm = TRUE
    ),

    .groups = "drop"
  )

write.csv(
  error_by_weekpart,
  "forecast-error-weekday-weekend.csv",
  row.names = FALSE
)


# -----------------------------------------------------------------------------
# 4. High-demand performance
# -----------------------------------------------------------------------------

high_demand_cutoff <- quantile(
  rolling_eval$actual,
  probs = 0.95,
  na.rm = TRUE
)

rolling_eval <- rolling_eval %>%
  mutate(
    demand_group = if_else(
      actual >= high_demand_cutoff,
      "Top 5% demand hours",
      "Other 95%"
    )
  )

error_by_demand_group <- rolling_eval %>%
  group_by(
    demand_group
  ) %>%
  summarise(
    n = n(),

    naive_24 = mean(
      abs_err_naive,
      na.rm = TRUE
    ),

    naive_168 = mean(
      abs_err_naive_168,
      na.rm = TRUE
    ),

    fourier_full = mean(
      abs_err_fourier_full,
      na.rm = TRUE
    ),

    fourier_recent = mean(
      abs_err_fourier_recent,
      na.rm = TRUE
    ),

    ensemble = mean(
      abs_err_ensemble,
      na.rm = TRUE
    ),

    official = mean(
      abs_err_official,
      na.rm = TRUE
    ),

    .groups = "drop"
  )

write.csv(
  error_by_demand_group,
  "forecast-error-high-demand.csv",
  row.names = FALSE
)


# -----------------------------------------------------------------------------
# 5. Daily model winners
# -----------------------------------------------------------------------------

daily_scores <- rolling_eval %>%
  group_by(
    origin
  ) %>%
  summarise(
    naive_24 = mean(
      abs_err_naive,
      na.rm = TRUE
    ),

    naive_168 = mean(
      abs_err_naive_168,
      na.rm = TRUE
    ),

    fourier_full = mean(
      abs_err_fourier_full,
      na.rm = TRUE
    ),

    fourier_recent = mean(
      abs_err_fourier_recent,
      na.rm = TRUE
    ),

    ensemble = mean(
      abs_err_ensemble,
      na.rm = TRUE
    ),

    official = mean(
      abs_err_official,
      na.rm = TRUE
    ),

    .groups = "drop"
  )

daily_winners <- daily_scores %>%
  pivot_longer(
    cols = -origin,
    names_to = "model",
    values_to = "MAE"
  ) %>%
  group_by(
    origin
  ) %>%
  slice_min(
    MAE,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup()

winner_counts <- daily_winners %>%
  count(
    model,
    sort = TRUE
  ) %>%
  mutate(
    percent = 100 * n / sum(n)
  )

write.csv(
  winner_counts,
  "forecast-daily-winners.csv",
  row.names = FALSE
)


# -----------------------------------------------------------------------------
# 6. Final 14-day two-panel figure
# -----------------------------------------------------------------------------

last_14 <- rolling_eval %>%
  filter(
    utc >= max(utc) - days(14)
  )

last_14_panels <- bind_rows(
  last_14 %>%
    transmute(
      utc,
      panel = "Fourier + ARIMA — 56 days",
      Actual = actual,
      Forecast = fourier_recent
    ),

  last_14 %>%
    transmute(
      utc,
      panel = "Seasonal naive 24h",
      Actual = actual,
      Forecast = naive
    )
) %>%
  pivot_longer(
    cols = c(
      Actual,
      Forecast
    ),
    names_to = "series",
    values_to = "demand"
  )

p_last_14 <- ggplot(
  last_14_panels,
  aes(
    x = utc,
    y = demand,
    color = series,
    linetype = series,
    linewidth = series
  )
) +
  geom_line() +
  facet_wrap(
    ~panel,
    ncol = 1
  ) +
  scale_color_manual(
    values = c(
      "Actual" = unname(
        brand_colors["field"]
      ),

      "Forecast" = unname(
        brand_colors["brick"]
      )
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Actual" = "solid",
      "Forecast" = "dashed"
    )
  ) +
  scale_linewidth_manual(
    values = c(
      "Actual" = 0.9,
      "Forecast" = 0.65
    ),
    guide = "none"
  ) +
  scale_x_datetime(
    date_breaks = "2 days",
    date_labels = "%b %d",
    timezone = "America/Los_Angeles"
  ) +
  scale_y_continuous(
    labels = scales::label_comma()
  ) +
  labs(
    title = "Forecast vs Actual — Last 14 Days",
    subtitle = "Two models, two different failure modes",
    x = NULL,
    y = "Demand (MW)",
    color = NULL,
    linetype = NULL,
    linewidth = NULL
  ) +
  portfolio_theme +
  theme(
    legend.position = "bottom"
  )

save_plot(
  p_last_14,
  "forecast-vs-actual-14d.png",
  width = 10,
  height = 7
)

saveRDS(
  rolling_eval,
  rolling_cache_file
)


# =============================================================================
# H. Multi-origin forecast evaluation
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Define four forecast origins per day
# -----------------------------------------------------------------------------
# Four origins reduce the confounding between forecast horizon and hour of day.
# They do not completely eliminate it.

multi_origin_hours <- c(
  0,
  6,
  12,
  18
)

multi_origins <- demand_ts %>%
  as_tibble() %>%
  filter(
    utc > cutoff,
    hour(utc) %in% multi_origin_hours
  ) %>%
  pull(
    utc
  )

multi_cache_file <- "forecast-multi-origin-eval.rds"


# -----------------------------------------------------------------------------
# 2. Load cached forecasts or generate them
# -----------------------------------------------------------------------------

if (file.exists(multi_cache_file)) {

  cat(
    "\nLOADING CACHED MULTI-ORIGIN FORECASTS\n"
  )

  multi_eval <- readRDS(
    multi_cache_file
  )

  multi_eval <- multi_eval %>%
    select(
      origin,
      utc,
      horizon,
      actual,
      official,
      naive,
      fourier_recent,
      ensemble
    )

} else {

  multi_results <- vector(
    "list",
    length(multi_origins)
  )

  cat(
    "\nMULTI-ORIGIN ROLLING FORECASTS\n"
  )

  cat(
    "Candidate origins:",
    length(multi_origins),
    "\n"
  )

  for (i in seq_along(multi_origins)) {

    origin <- multi_origins[i]

    cat(
      "Origin",
      i,
      "of",
      length(multi_origins),
      ":",
      as.character(origin),
      "\n"
    )

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
      arrange(
        utc
      )

    if (nrow(target) != 24) {
      next
    }

    naive_fc <- demand_ts %>%
      as_tibble() %>%
      filter(
        utc > origin - hours(24),
        utc <= origin
      ) %>%
      arrange(
        utc
      ) %>%
      pull(
        demand
      )

    if (length(naive_fc) != 24) {
      next
    }

    fit_recent <- hist_recent %>%
      model(
        fourier_recent = ARIMA(
          demand ~
            fourier(
              period = 24,
              K = 3
            ) +
            fourier(
              period = 168,
              K = 2
            ) +
            pdq(
              1,
              0,
              1
            ) +
            PDQ(
              0,
              0,
              0
            )
        )
      )

    fourier_fc <- fit_recent %>%
      forecast(
        h = 24
      ) %>%
      as_tibble() %>%
      pull(
        .mean
      )

    multi_results[[i]] <- tibble(
      origin = origin,

      utc = target$utc,

      horizon = as.integer(
        difftime(
          target$utc,
          origin,
          units = "hours"
        )
      ),

      actual = target$demand,

      official = target$demand_forecast,

      naive = naive_fc,

      fourier_recent = fourier_fc,

      ensemble = (
        naive_fc +
          fourier_fc
      ) / 2
    )
  }

  multi_eval <- bind_rows(
    multi_results
  ) %>%
    arrange(
      origin,
      horizon
    )

  saveRDS(
    multi_eval,
    multi_cache_file
  )
}

cat(
  "\nCompleted multi-origin forecasts:",
  n_distinct(
    multi_eval$origin
  ),
  "\n"
)

cat(
  "Multi-origin forecast rows:",
  nrow(
    multi_eval
  ),
  "\n"
)


# -----------------------------------------------------------------------------
# 3. Add Pacific-time fields and errors
# -----------------------------------------------------------------------------

multi_eval <- multi_eval %>%
  mutate(
    pacific_time = with_tz(
      utc,
      "America/Los_Angeles"
    ),

    hour_pacific = hour(
      pacific_time
    ),

    abs_err_naive = abs(
      actual - naive
    ),

    abs_err_fourier = abs(
      actual - fourier_recent
    ),

    abs_err_ensemble = abs(
      actual - ensemble
    ),

    abs_err_official = abs(
      actual - official
    )
  )


# -----------------------------------------------------------------------------
# 4. Overall multi-origin accuracy
# -----------------------------------------------------------------------------

multi_scores <- tibble(
  model = c(
    "Naive/Fourier 50/50 ensemble",
    "Seasonal naive 24h",
    "Fourier + ARIMA — trailing 56 days",
    "Official EIA published forecast"
  ),

  MAE = c(
    mae(
      multi_eval$actual,
      multi_eval$ensemble
    ),

    mae(
      multi_eval$actual,
      multi_eval$naive
    ),

    mae(
      multi_eval$actual,
      multi_eval$fourier_recent
    ),

    mae(
      multi_eval$actual,
      multi_eval$official
    )
  ),

  RMSE = c(
    rmse(
      multi_eval$actual,
      multi_eval$ensemble
    ),

    rmse(
      multi_eval$actual,
      multi_eval$naive
    ),

    rmse(
      multi_eval$actual,
      multi_eval$fourier_recent
    ),

    rmse(
      multi_eval$actual,
      multi_eval$official
    )
  )
) %>%
  arrange(
    MAE
  )

cat(
  "\nMULTI-ORIGIN OVERALL RESULTS\n\n"
)

print(
  multi_scores,
  n = Inf
)

write.csv(
  multi_scores,
  "forecast-multi-origin-scores.csv",
  row.names = FALSE
)


# -----------------------------------------------------------------------------
# 5. Error by forecast horizon
# -----------------------------------------------------------------------------
# EIA is intentionally excluded from this comparison.
# The published EIA series does not have a forecast origin defined by this
# experiment, so assigning it an h-step forecast horizon would be misleading.

multi_error_by_horizon <- multi_eval %>%
  group_by(
    horizon
  ) %>%
  summarise(
    naive = mean(
      abs_err_naive,
      na.rm = TRUE
    ),

    fourier_recent = mean(
      abs_err_fourier,
      na.rm = TRUE
    ),

    ensemble = mean(
      abs_err_ensemble,
      na.rm = TRUE
    ),

    n = n(),

    .groups = "drop"
  )

cat(
  "\nMULTI-ORIGIN ERROR BY FORECAST HORIZON\n\n"
)

print(
  multi_error_by_horizon,
  n = Inf
)

write.csv(
  multi_error_by_horizon,
  "forecast-multi-origin-error-by-horizon.csv",
  row.names = FALSE
)


# -----------------------------------------------------------------------------
# 6. Error by hour of day
# -----------------------------------------------------------------------------

multi_error_by_hour <- multi_eval %>%
  group_by(
    hour_pacific
  ) %>%
  summarise(
    naive = mean(
      abs_err_naive,
      na.rm = TRUE
    ),

    fourier_recent = mean(
      abs_err_fourier,
      na.rm = TRUE
    ),

    ensemble = mean(
      abs_err_ensemble,
      na.rm = TRUE
    ),

    official = mean(
      abs_err_official,
      na.rm = TRUE
    ),

    n = n(),

    .groups = "drop"
  )

cat(
  "\nMULTI-ORIGIN ERROR BY HOUR OF DAY\n\n"
)

print(
  multi_error_by_hour,
  n = Inf
)

write.csv(
  multi_error_by_hour,
  "forecast-multi-origin-error-by-hour.csv",
  row.names = FALSE
)


# -----------------------------------------------------------------------------
# 7. Final horizon figure
# -----------------------------------------------------------------------------

multi_horizon_long <- multi_error_by_horizon %>%
  select(
    -n
  ) %>%
  pivot_longer(
    cols = -horizon,
    names_to = "model",
    values_to = "MAE"
  ) %>%
  mutate(
    model = recode(
      model,

      naive = "Seasonal naive 24h",

      fourier_recent = "Fourier + ARIMA — 56 days",

      ensemble = "Naive/Fourier 50/50 ensemble"
    )
  )

p_multi_horizon <- ggplot(
  multi_horizon_long,
  aes(
    x = horizon,
    y = MAE,
    color = model,
    linetype = model
  )
) +
  annotate(
    "rect",
    xmin = 12.5,
    xmax = 24.5,
    ymin = -Inf,
    ymax = Inf,
    fill = brand_colors["sand"],
    alpha = 0.22
  ) +
  geom_line(
    linewidth = 1
  ) +
  geom_point(
    size = 1.7
  ) +
  scale_color_manual(
    values = c(
      "Seasonal naive 24h" = unname(
        brand_colors["plum"]
      ),

      "Fourier + ARIMA — 56 days" = unname(
        brand_colors["brick"]
      ),

      "Naive/Fourier 50/50 ensemble" = unname(
        brand_colors["blue"]
      )
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Seasonal naive 24h" = "dashed",
      "Fourier + ARIMA — 56 days" = "dotted",
      "Naive/Fourier 50/50 ensemble" = "solid"
    )
  ) +
  scale_x_continuous(
    breaks = seq(
      1,
      24,
      3
    )
  ) +
  scale_y_continuous(
    labels = scales::label_comma()
  ) +
  labs(
    title = "Forecast Error by Forecast Horizon",
    subtitle = "Four forecast origins per day reduce hour-of-day confounding",
    x = "Hours ahead",
    y = "MAE (MW)",
    color = NULL,
    linetype = NULL
  ) +
  portfolio_theme +
  theme(
    legend.position = "bottom"
  ) +
  guides(
    color = guide_legend(
      nrow = 2,
      byrow = TRUE
    ),

    linetype = guide_legend(
      nrow = 2,
      byrow = TRUE
    )
  )

save_plot(
  p_multi_horizon,
  "forecast-multi-origin-error-by-horizon.png",
  width = 9,
  height = 5.5
)


# -----------------------------------------------------------------------------
# 8. Final hour-of-day figure
# -----------------------------------------------------------------------------

multi_hour_long <- multi_error_by_hour %>%
  select(
    -n
  ) %>%
  pivot_longer(
    cols = -hour_pacific,
    names_to = "model",
    values_to = "MAE"
  ) %>%
  mutate(
    model = recode(
      model,

      naive = "Seasonal naive 24h",

      fourier_recent = "Fourier + ARIMA — 56 days",

      ensemble = "Naive/Fourier 50/50 ensemble",

      official = "Official EIA forecast"
    )
  )

multi_hour_headline <- multi_hour_long %>%
  filter(
    model != "Fourier + ARIMA — 56 days"
  )

p_multi_hour <- ggplot(
  multi_hour_headline,
  aes(
    x = hour_pacific,
    y = MAE,
    color = model,
    linetype = model,
    linewidth = model
  )
) +
  annotate(
    "rect",
    xmin = 7.5,
    xmax = 16.5,
    ymin = -Inf,
    ymax = Inf,
    fill = brand_colors["sand"],
    alpha = 0.30
  ) +
  geom_line() +
  scale_color_manual(
    values = c(
      "Official EIA forecast" = unname(
        brand_colors["brick"]
      ),

      "Naive/Fourier 50/50 ensemble" = unname(
        brand_colors["blue"]
      ),

      "Seasonal naive 24h" = unname(
        brand_colors["plum"]
      )
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Official EIA forecast" = "solid",
      "Naive/Fourier 50/50 ensemble" = "solid",
      "Seasonal naive 24h" = "dashed"
    )
  ) +
  scale_linewidth_manual(
    values = c(
      "Official EIA forecast" = 1.5,
      "Naive/Fourier 50/50 ensemble" = 0.9,
      "Seasonal naive 24h" = 0.9
    ),
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = seq(
      0,
      23,
      3
    )
  ) +
  scale_y_continuous(
    labels = scales::label_comma()
  ) +
  labs(
    title = "Forecast Error by Hour of Day",
    subtitle = "EIA performs strongly outside the daytime gap",
    x = "Hour (Pacific time)",
    y = "MAE (MW)",
    color = NULL,
    linetype = NULL,
    linewidth = NULL
  ) +
  portfolio_theme +
  theme(
    legend.position = "bottom"
  ) +
  guides(
    color = guide_legend(
      nrow = 2,
      byrow = TRUE
    ),

    linetype = guide_legend(
      nrow = 2,
      byrow = TRUE
    )
  )

save_plot(
  p_multi_hour,
  "forecast-multi-origin-error-by-hour.png",
  width = 9,
  height = 5.5
)


# =============================================================================
# I. Compact console summary
# =============================================================================

cat(
  "\n============================================================\n"
)

cat(
  "FINAL FORECAST AUDIT SUMMARY\n"
)

cat(
  "============================================================\n"
)


cat(
  "\nDATA\n"
)

cat(
  "Working demand rows:",
  nrow(data),
  "\n"
)

cat(
  "Missing demand rows removed:",
  n_before - nrow(data),
  "\n"
)

cat(
  "Low-demand audit rows:",
  nrow(low_demand_hours),
  "\n"
)


cat(
  "\n------------------------------------------------------------\n"
)

cat(
  "EIA ALIGNMENT AUDIT\n"
)

cat(
  "------------------------------------------------------------\n"
)

print(
  alignment_results
)

cat(
  "\nBest EIA alignment:",
  best_alignment_label,
  "\n"
)


cat(
  "\n------------------------------------------------------------\n"
)

cat(
  "EIA SIGNED ERROR BY PACIFIC HOUR\n"
)

cat(
  "------------------------------------------------------------\n"
)

print(
  eia_signed_by_hour,
  n = 24
)

cat(
  "\nEIA SIGNED-ERROR SUMMARY\n"
)

print(
  eia_signed_summary
)


cat(
  "\n------------------------------------------------------------\n"
)

cat(
  "ROLLING DAY-AHEAD RESULTS\n"
)

cat(
  "------------------------------------------------------------\n"
)

cat(
  "Completed rolling origins:",
  n_distinct(
    rolling_eval$origin
  ),
  "\n"
)

cat(
  "Rolling forecast rows:",
  nrow(
    rolling_eval
  ),
  "\n\n"
)

print(
  rolling_scores,
  n = Inf
)


cat(
  "\n------------------------------------------------------------\n"
)

cat(
  "WEEKDAY VS WEEKEND\n"
)

cat(
  "------------------------------------------------------------\n"
)

print(
  error_by_weekpart
)


cat(
  "\n------------------------------------------------------------\n"
)

cat(
  "HIGH-DEMAND HOURS\n"
)

cat(
  "------------------------------------------------------------\n"
)

cat(
  "95th percentile demand cutoff:",
  round(
    high_demand_cutoff
  ),
  "MW\n\n"
)

print(
  error_by_demand_group
)


cat(
  "\n------------------------------------------------------------\n"
)

cat(
  "DAILY MODEL WINNERS\n"
)

cat(
  "------------------------------------------------------------\n"
)

print(
  winner_counts
)


cat(
  "\n------------------------------------------------------------\n"
)

cat(
  "MULTI-ORIGIN RESULTS\n"
)

cat(
  "------------------------------------------------------------\n"
)

cat(
  "Completed multi-origin forecasts:",
  n_distinct(
    multi_eval$origin
  ),
  "\n"
)

cat(
  "Multi-origin forecast rows:",
  nrow(
    multi_eval
  ),
  "\n\n"
)

print(
  multi_scores,
  n = Inf
)


cat(
  "\n------------------------------------------------------------\n"
)

cat(
  "MULTI-ORIGIN ERROR BY FORECAST HORIZON\n"
)

cat(
  "------------------------------------------------------------\n"
)

print(
  multi_error_by_horizon,
  n = Inf
)


cat(
  "\n------------------------------------------------------------\n"
)

cat(
  "MULTI-ORIGIN ERROR BY HOUR OF DAY\n"
)

cat(
  "------------------------------------------------------------\n"
)

print(
  multi_error_by_hour,
  n = Inf
)


cat(
  "\n============================================================\n"
)

cat(
  "FINAL FIGURES\n"
)

cat(
  "============================================================\n"
)

cat(
  "forecast-demand-daily-profile.png\n"
)

cat(
  "forecast-demand-by-month.png\n"
)

cat(
  "forecast-demand-daily-series.png\n"
)

cat(
  "forecast-vs-actual-14d.png\n"
)

cat(
  "forecast-multi-origin-error-by-horizon.png\n"
)

cat(
  "forecast-multi-origin-error-by-hour.png\n"
)


cat(
  "\n============================================================\n"
)

cat(
  "END OF AUDIT SUMMARY\n"
)

cat(
  "============================================================\n"
)
