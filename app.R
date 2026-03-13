# CRUSH COVID dashboard (archival 2022 codebase)
#
# Original implementation: Georgios Varotsis
# Project context: CRUSH COVID initiative, Uppsala County
# Original start date: 2020-11-06
# Repository edition prepared for public archival release
#
# Public-release notes
# - Do not commit row-level or personally identifiable data.
# - Review all files under `data/` before publishing the repository.
# - Keep any optional passwords, tokens, or upload keys in environment variables.
#
# Notes
# - This code intentionally preserves the package ecosystem and coding style of the
#   2020-2022 deployment period.
# - The repository version removes direct personal contact details and is intended
#   for historical reference, reproducibility, and code review.
# - Data files are expected under the local `data/` and `images/` directories.
#
# Runtime libraries -------------------------------------------------------------
required_packages <- c(
  "plyr",
  "rsconnect",
  "shiny", "shinydashboard", "shinymanager", "shinylogs",
  "shinyWidgets", "shinyjs", "shinythemes", "shinyscreenshot", "shinyalert",
  "leaflet", "rgdal", "maps", "mapdata",
  "rio", "haven", "openxlsx",
  "tidyverse", "reshape2",
  "lubridate", "zoo", "smooth", "lattice", "TTR", "MMWRweek",
  "DT", "highcharter", "googleVis", "plotly", "flexdashboard", "formattable",
  "wesanderson", "magick", "grid",
  "viridis", "viridisLite", "scales", "dichromat",
  "ggthemes", "RColorBrewer", "hrbrthemes",
  "emojifont"
)

for (pkg in required_packages) {
  suppressPackageStartupMessages(
    library(pkg, character.only = TRUE)
  )
}

load.fontawesome()

suppressPackageStartupMessages(library(googleVis))

# Toggle access protection depending on deployment target
passwordON_Off <- 'OFF'

# ==============================================================================
# Configuration and reusable helpers
# ==============================================================================

read_named_sheet <- function(path, sheet_name) {
  sheet_names <- readxl::excel_sheets(path)
  rio::import(path, which = match(sheet_name, sheet_names))
}

drop_existing_cols <- function(df, cols) {
  df[, setdiff(names(df), cols), drop = FALSE]
}

drop_matching_cols <- function(df, patterns) {
  keep <- !Reduce(
    `|`,
    lapply(patterns, function(p) grepl(p, names(df), perl = TRUE))
  )
  df[, keep, drop = FALSE]
}

select_week_columns <- function(df, weeks) {
  week_pattern <- paste0("(", paste(weeks, collapse = "|"), ")")
  dplyr::select(df, dplyr::all_of(names(df)[grep(week_pattern, names(df))]))
}

safe_max <- function(x) {
  if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
}

last_monday <- function(x) {
  7 * floor(as.numeric(x - 1 + 4) / 7) + as.Date(1 - 4, origin = "1970-01-01")
}

build_week_lookup <- function(iso_week_current) {
  df_2020 <- data.frame(
    week = 26:53,
    ISOweek_year = paste(26:53, rep(2020, 28), sep = "-")
  )
  
  df_2021 <- data.frame(
    week = 54:105,
    ISOweek_year = paste(sprintf("%02d", 1:52), rep(2021, 52), sep = "-")
  )
  
  df_2022 <- data.frame(
    week = 106:(105 + iso_week_current),
    ISOweek_year = paste(sprintf("%02d", seq_len(iso_week_current)), rep(2022, iso_week_current), sep = "-")
  )
  
  rbind(df_2020, df_2021, df_2022)
}

extract_weekly_panel <- function(df, weeks, area_col = "area", shiny_col = "shiny_name",
                                 name_template = c("area", "drop1", "drop2", "positivity", "notification", "testing", "shiny_name", "week")) {
  weekly_list <- lapply(weeks, function(i) {
    out <- df %>%
      dplyr::select(
        dplyr::all_of(area_col),
        dplyr::all_of(names(df)[grep(i, names(df))]),
        dplyr::all_of(shiny_col)
      )
    
    out$week <- i
    names(out) <- name_template
    out$drop1 <- NULL
    out$drop2 <- NULL
    out
  })
  
  dplyr::bind_rows(weekly_list)
}

relabel_kommun <- function(x) {
  kommun_lookup <- c(
    "ENKÖPING"   = "Enköping",
    "HEBY"       = "Heby",
    "HÅBO"       = "Håbo",
    "KNIVSTA"    = "Knivsta",
    "TIERP"      = "Tierp",
    "UPPSALA"    = "Uppsala",
    "ÄLVKARLEBY" = "Älvkarleby",
    "ÖSTHAMMAR"  = "Östhammar",
    "LÄNET"      = "Länet"
  )
  
  dplyr::recode(x, !!!as.list(kommun_lookup))
}

compute_percent_change <- function(current, previous, inf_replacement = NULL, digits = 1) {
  out <- round(((current - previous) * 100) / previous, digits = digits)
  
  if (!is.null(inf_replacement)) {
    out[is.infinite(out)] <- inf_replacement
  }
  
  out
}

# ==============================================================================
# Archive configuration
# Manual update parameters used in the original weekly publication workflow.
# Values are intentionally explicit to preserve the original 2022 execution pattern.
# ==============================================================================

dashboard_date_Monday_most_recent <- as.Date("2022-05-23", format = "%Y-%m-%d")
current_week_ecdc_slider <- " 20 2022"

max_cum_freq_motion <- 0.9
number_vacc_outside_Upps <- "23 591 (Uppdaterat vecka 34 från FHM)"

max_notif_stad <- 1200
max_kommun_cases <- 500
max_tester_stad <- 9000

files_csss_trends <- "data/CSSS_berakningar_2siffror.csv"
files_csss_lan_trends_w <- "data/CSSS_berakningar_lan.csv"

iso_week_current <- lubridate::isoweek(dashboard_date_Monday_most_recent) - 1
current_week_numeric <- iso_week_current
non_iso_week_current <- 105 + iso_week_current

lastDataUpdate <- paste0(
  MMWRweek::MMWRweek2Date(
    MMWRyear = 2022,
    MMWRweek = lubridate::isoweek(dashboard_date_Monday_most_recent),
    MMWRday  = 5
  )
)

nextDataUpdate <- "i Augusti"

current_date <- MMWRweek::MMWRweek2Date(
  MMWRyear = 2022,
  MMWRweek = lubridate::isoweek(dashboard_date_Monday_most_recent),
  MMWRday  = 1
)

current_date_mobility_start <- current_date - 69

most_recent_monday <- MMWRweek::MMWRweek2Date(
  MMWRyear = 2022,
  MMWRweek = lubridate::isoweek(dashboard_date_Monday_most_recent) - 1,
  MMWRday  = 2
)

files_ian          <- sprintf("data/kommun_%s.xlsx", non_iso_week_current + 1)
files_stad         <- sprintf("data/stadsdel_%s.xlsx", non_iso_week_current + 1)
files_top10        <- sprintf("data/top10_%s.xlsx", non_iso_week_current + 1)
files_uppsala      <- sprintf("data/traffic_%s.xlsx", non_iso_week_current + 1)
files_uppsala_Tove <- sprintf("data/data_%s_public.xlsx", non_iso_week_current + 1)
files_notif_recent <- sprintf("data/top10_%s.xlsx", non_iso_week_current + 1)

# ==============================================================================
# SciLifeLab export preparation
# ==============================================================================

data.ecdc.download_scilife_96 <- rio::import("data/traffic__up_to_96.xlsx")
data.ecdc.download_scilife_96$details <- dplyr::case_when(
  data.ecdc.download_scilife_96$week < 73 ~ "Indicators_adjusted_per_adult_population",
  TRUE ~ "Indicators_adjusted_per_total_population"
)

data.ecdc.download_scilife <- read_named_sheet(files_uppsala, "total_allage")
data.ecdc.download_scilife <- subset(data.ecdc.download_scilife, week > 96)

data.ecdc.download_scilife <- plyr::join(
  data.ecdc.download_scilife,
  data.ecdc.download_scilife_96[, c(1, 4)],
  by    = "postnummer",
  type  = "left",
  match = "first"
)

data.ecdc.download_scilife <- data.ecdc.download_scilife[, c(1, 2, 3, 7, 4:6)]
data.ecdc.download_scilife$details <- "Indicators_adjusted_per_total_population"
colnames(data.ecdc.download_scilife) <- colnames(data.ecdc.download_scilife_96)

data.ecdc.download_scilife_final <- rbind(
  data.ecdc.download_scilife_96,
  data.ecdc.download_scilife
)

df_weeks_all <- build_week_lookup(iso_week_current)

data.ecdc.download_scilife_final2 <- plyr::join(
  data.ecdc.download_scilife_final,
  df_weeks_all,
  by    = "week",
  type  = "left",
  match = "all"
)

library(httr)

data.ecdc.download_description <- rio::import("data/data_ECDC_kartor_description.xlsx")

colnames(data.ecdc.download_scilife_final2) <- c(
  "postnummer", "total_population", "positivity", "adult_pop",
  "cases_per_100k_14days", "test_per_100k", "week_continuous",
  "details", "ISOweek_year"
)

data.ecdc.download_scilife_final2 <- data.ecdc.download_scilife_final2[, c(1:7, 9, 8)]

write.csv(
  data.ecdc.download_scilife_final2,
  "data/data_ECDC_kartor_scilife.csv",
  row.names = FALSE
)

file_data_scilife <- upload_file("data/data_ECDC_kartor_scilife.csv")

# ==============================================================================
# Week labels and dashboard metadata
# ==============================================================================

vax_ecdc_one <- iso_week_current
vax_ecdc_all <- (iso_week_current - 9):iso_week_current

ecdc_all_one <- as.character(vax_ecdc_one)
ecdc_all <- as.character(vax_ecdc_all)

current_week_veckarapport <- paste("v.", vax_ecdc_one + 1)
current_week_speed <- paste0("v.", iso_week_current)
past_week_speed <- paste("v.", vax_ecdc_one - 1)
past_past_week_speed <- paste("v.", vax_ecdc_one - 2)

current_week_sew <- non_iso_week_current
current_week <- non_iso_week_current
previous_week <- non_iso_week_current - 1
current_week_ecdc <- as.character(iso_week_current)
current_week_numeric <- iso_week_current

lower_week_heatmap <- as.character(iso_week_current - 9)

breaks_weeks <- c(
  current_week - 9,
  current_week - 8,
  current_week - 6,
  current_week - 4,
  current_week - 2,
  current_week
)

labels_weeks <- as.character(c(
  iso_week_current - 9,
  iso_week_current - 8,
  iso_week_current - 6,
  iso_week_current - 4,
  iso_week_current - 2,
  iso_week_current
))

labels_weeks_calls <- as.character((iso_week_current - 9):iso_week_current)

wrong_weeks_vacc <- 54:current_week
week_numbers_loess <- seq_len(current_week_numeric)

week_number <- c(35:53, 1:52, 1:current_week_numeric)

week_number_2020 <- seq(35, 53, by = 2)
week_number_2021 <- seq(1, 52, by = 2)
week_number_2022 <- seq(1, current_week_numeric, by = 2)

week_number_year <- c(
  paste("", week_number_2020, "2020"),
  paste("", week_number_2021, "2021"),
  paste("", week_number_2022, "2022")
)

week_number_year_break <- c(
  seq(35, 53, by = 2),
  seq(54, current_week, by = 2)
)

number_total_pop_upps_lan <- 323111
week_mondays <- seq(most_recent_monday - 63, most_recent_monday, by = "1 week")

# ==============================================================================
# Credentials
# ==============================================================================

password_accept <- function(x) {
  dashboard_password <- Sys.getenv("CRUSH_COVID_INTERNAL_PASSWORD", unset = "")
  
  if (nzchar(dashboard_password) && identical(x, dashboard_password)) {
    invisible(TRUE)
  } else {
    shinyalert(
      "Försök igen",
      "Skriv lösenordet",
      type      = "input",
      inputType = "password",
      time      = 0,
      callbackR = password_accept
    )
  }
}

# ==============================================================================
# Trend data
# ==============================================================================

swed_stad <- rio::import("data/city_population_SE_dashboard_coded.xlsx")

ian_up66    <- rio::import("data/kom_up_to66.xlsx", which = 1)
kommun_up66 <- rio::import("data/kom_up_to66.xlsx", which = 2)
stad_up66   <- rio::import("data/stad_up_to66.xlsx", which = 2)

stad_up67_72   <- rio::import("data/stad_old_table_67_87.xlsx", which = 2)
kommun_up67_72 <- rio::import("data/kom_old_table_68_87.xlsx", which = 2)
ian_up67_72    <- rio::import("data/kom_old_table_68_87.xlsx", which = 1)

stad_up72   <- cbind(stad_up66, stad_up67_72)
kommun_up72 <- cbind(kommun_up66, kommun_up67_72)
ian_up72    <- cbind(ian_up66, ian_up67_72)

week_numbers_old <- 26:96

ian_up66_2    <- select_week_columns(ian_up72, week_numbers_old)
kommun_up72_2 <- select_week_columns(kommun_up72, week_numbers_old)
stad_up72_2   <- select_week_columns(stad_up72, week_numbers_old)

uppsala_ian_new  <- read_named_sheet(files_ian, "total_allage")
uppsala_kom_new  <- read_named_sheet(files_ian, "weekly_allage")
uppsala_stad_new <- read_named_sheet(files_stad, "weekly_allage")

cols_drop_weekly <- c(
  "tests_2w_allage", "cases_2w_allage", "positivity_2w_allage",
  "positivity_all", "pos_total", "tests_total"
)

uppsala_kom_new  <- drop_existing_cols(uppsala_kom_new, cols_drop_weekly)
uppsala_stad_new <- drop_existing_cols(uppsala_stad_new, cols_drop_weekly)
uppsala_ian_new  <- drop_existing_cols(uppsala_ian_new, c("week", "v1"))

selector_names_withPop <- c(
  "Enköping (46 326 inv.)", "Heby (13 982 inv.)",
  "Håbo (22 058 inv.)", "Knivsta (18 447 inv.)",
  "Tierp (21 182 inv.)", "Uppsala (234 935 inv.)",
  "Älvkarleby (9 627 inv.)", "Östhammar (22 246 inv.)"
)

uppsala_kom_new$kommun <- selector_names_withPop
uppsala_stad_new$stadsdel <- swed_stad$stadsdel

names(uppsala_kom_new)[names(uppsala_kom_new) == "kommun"] <- "area"
names(uppsala_stad_new)[names(uppsala_stad_new) == "stadsdel"] <- "area"

uppsala_stad_shiny <- rio::import("data/cities_shiny_names.xlsx", which = 1)

uppsala_kom_new$shiny_name <- c(
  "Enköping", "Heby", "Håbo", "Knivsta",
  "Tierp", "Uppsala", "Älvkarleby", "Östhammar"
)

uppsala_stad_new$shiny_name <- uppsala_stad_shiny$area
uppsala_ian_new$area <- "Uppsala Län"
uppsala_ian_new$shiny_name <- "Uppsala Län (inv. 388 803)"

uppsala_ian2_new <- uppsala_ian_new[, c(ncol(uppsala_ian_new), 1:(ncol(uppsala_ian_new) - 1))]
uppsala_all_new <- rbind(uppsala_ian2_new, uppsala_kom_new, uppsala_stad_new)

week_numbers_new <- 97:current_week

uppsala_all_weeks_new <- extract_weekly_panel(
  df = uppsala_all_new,
  weeks = week_numbers_new
)

create_kommun_trend_data <- function(kommun) {
  uppsala_all_weeks_new[uppsala_all_weeks_new$area == kommun, ]
}

create_stad_trend_data <- function(stad) {
  uppsala_all_weeks_new[uppsala_all_weeks_new$area == stad, ]
}

uppsala_iann_new <- subset(uppsala_all_weeks_new, shiny_name == "Uppsala Län (inv. 388 803)")

kommun_levels <- c(
  "Enköping", "Heby", "Håbo", "Knivsta",
  "Tierp", "Uppsala", "Älvkarleby", "Östhammar"
)

set_kommun <- subset(uppsala_all_weeks_new, shiny_name %in% kommun_levels)

kommun_names_pop <- c(
  "Enköping"   = "Enköping (inv. 46 326*)",
  "Heby"       = "Heby (inv. 13 982*)",
  "Håbo"       = "Håbo (inv. 22 058*)",
  "Knivsta"    = "Knivsta (inv. 18 447*)",
  "Tierp"      = "Tierp (inv. 21 182*)",
  "Uppsala"    = "Uppsala (inv. 191 295*)",
  "Älvkarleby" = "Älvkarleby (inv. 9 627*)",
  "Östhammar"  = "Östhammar (inv. 22 246*)"
)

kommun_labeller <- function(variable, value) {
  kommun_names_pop[value]
}

set_choose_kommun <- subset(uppsala_all_weeks_new, shiny_name %in% kommun_levels)

uppsala_all_weeks_10weeks_new <- uppsala_all_weeks_new %>%
  dplyr::filter(week > (current_week - 10))

# ==============================================================================
# Historical population-based panel
# ==============================================================================

uppsala_ian <- rio::import(files_ian, which = 1)
colnames(uppsala_ian) <- gsub("_allage", "", colnames(uppsala_ian))
uppsala_ian <- cbind(uppsala_ian, ian_up66_2)
uppsala_ian <- uppsala_ian[, unique(colnames(uppsala_ian))]

uppsala_kom <- rio::import(files_ian, which = 2)
colnames(uppsala_kom) <- gsub("_allage", "", colnames(uppsala_kom))
uppsala_kom <- cbind(uppsala_kom, kommun_up72_2)
uppsala_kom <- uppsala_kom[, unique(colnames(uppsala_kom))]

uppsala_stad <- rio::import(files_stad, which = 2)
colnames(uppsala_stad) <- gsub("_allage", "", colnames(uppsala_stad))
uppsala_stad <- cbind(uppsala_stad, stad_up72_2)
uppsala_stad <- uppsala_stad[, unique(colnames(uppsala_stad))]

uppsala_stad_shiny <- rio::import("data/cities_shiny_names.xlsx", which = 1)

uppsala_kom  <- drop_existing_cols(uppsala_kom, c("tests_2w", "cases_2w", "positivity_2w", "positivity_all", "pos_total", "tests_total"))
uppsala_stad <- drop_existing_cols(uppsala_stad, c("tests_2w", "cases_2w", "positivity_2w", "positivity_all", "pos_total", "tests_total"))
uppsala_ian  <- drop_existing_cols(uppsala_ian, c("week", "v1"))

uppsala_kom$kommun <- selector_names_withPop
uppsala_stad$stadsdel <- swed_stad$stadsdel

names(uppsala_kom)[names(uppsala_kom) == "kommun"] <- "area"
names(uppsala_stad)[names(uppsala_stad) == "stadsdel"] <- "area"

uppsala_kom$shiny_name <- kommun_levels
uppsala_stad$shiny_name <- uppsala_stad_shiny$area

uppsala_ian$area <- "Uppsala Län"
uppsala_ian$shiny_name <- "Uppsala Län (inv. 388 803)"

uppsala_ian2 <- uppsala_ian[, c(ncol(uppsala_ian), 1:(ncol(uppsala_ian) - 1))]
uppsala_ian2 <- dplyr::select(uppsala_ian2, colnames(uppsala_kom))
uppsala_ian2 <- dplyr::select(uppsala_ian2, order(colnames(uppsala_ian2)))

uppsala_kom  <- dplyr::select(uppsala_kom, order(colnames(uppsala_kom)))
uppsala_stad <- dplyr::select(uppsala_stad, order(colnames(uppsala_stad)))

patterns_to_drop <- c(
  "^test_per_capita5_w7[3-7]$",
  "^cases_per_capita5_w7[3-7]$"
)

uppsala_stad <- drop_matching_cols(uppsala_stad, patterns_to_drop)
uppsala_ian2 <- drop_matching_cols(uppsala_ian2, patterns_to_drop)
uppsala_kom  <- drop_matching_cols(uppsala_kom, patterns_to_drop)

uppsala_ian2 <- uppsala_ian2[, order(colnames(uppsala_ian2))]
uppsala_kom  <- uppsala_kom[, order(colnames(uppsala_kom))]
uppsala_stad <- uppsala_stad[, order(colnames(uppsala_stad))]

uppsala_all <- rbind(uppsala_ian2, uppsala_kom, uppsala_stad)

week_numbers <- 26:current_week

uppsala_all_weeks <- extract_weekly_panel(
  df = uppsala_all,
  weeks = week_numbers,
  name_template = c("area", "notification", "drop1", "positivity", "testing", "drop2", "shiny_name", "week")
)

uppsala_all_weeks_multi_Kommun <- subset(uppsala_all_weeks, area == "Uppsala (234 935 inv.)" & week > 34)
uppsala_all_weeks_multi_Lan    <- subset(uppsala_all_weeks, area == "Uppsala Län" & week > 34)

# ==============================================================================
# Speedometer
# ==============================================================================

set_speedometer <- subset(
  uppsala_all_weeks_new,
  shiny_name %in% c(kommun_levels, "Uppsala Län (inv. 388 803)")
)

set_speedometer$shiny_name[set_speedometer$shiny_name == "Uppsala Län (inv. 388 803)"] <- "Länet"

sheets_notif_recent <- readxl::excel_sheets(files_ian)
notif_recent <- rio::import(files_ian, which = match("weekly_allage", sheets_notif_recent))

notif_recent <- notif_recent[, c(
  "kommun",
  paste0("cases_per_capita5_w", non_iso_week_current, "_allage"),
  paste0("cases_per_capita5_w", non_iso_week_current - 1, "_allage")
)]

notif_recent$notification <- round(rowSums(notif_recent[, 2:3], na.rm = TRUE), 0)
notif_recent <- notif_recent[, c("kommun", "notification")]

sheets_notif_recent <- readxl::excel_sheets(files_notif_recent)
notif_recent_lan <- rio::import(files_notif_recent, which = match("average_allage", sheets_notif_recent))

names(notif_recent_lan)[names(notif_recent_lan) == "cases_per_capita_allage"] <- "notification"
notif_recent_lan <- drop_existing_cols(
  notif_recent_lan,
  c("positivity_allage", "tests_per_capita_allage")
)

notif_recent_lan$kommun <- "LÄNET"
notif_recent_lan <- notif_recent_lan[, c(2, 1)]

notif_recent <- rbind(notif_recent_lan, notif_recent)

notif_before_lan_sheets <- readxl::excel_sheets(files_ian)

notif_before <- rio::import(files_ian, which = match("weekly_allage", notif_before_lan_sheets))
notif_before <- notif_before[, c(
  "kommun",
  paste0("cases_per_capita5_w", non_iso_week_current - 2, "_allage"),
  paste0("cases_per_capita5_w", non_iso_week_current - 1, "_allage")
)]

notif_before$notification <- round(rowSums(notif_before[, 2:3], na.rm = TRUE), 0)
notif_before <- notif_before[, c("kommun", "notification")]

notif_before_lan <- rio::import(files_ian, which = match("total_allage", notif_before_lan_sheets))
notif_before_lan <- notif_before_lan[, c(
  "v1",
  paste0("cases_per_capita5_w", non_iso_week_current - 2, "_allage"),
  paste0("cases_per_capita5_w", non_iso_week_current - 1, "_allage")
)]

notif_before_lan$notification <- rowSums(notif_before_lan[, 2:3], na.rm = TRUE)
notif_before_lan$kommun <- "LÄNET"
notif_before_lan <- notif_before_lan[, c("kommun", "notification")]

notif_before <- rbind(notif_before_lan, notif_before)

notif_recent$kommun <- relabel_kommun(notif_recent$kommun)
notif_before$kommun <- relabel_kommun(notif_before$kommun)

names(notif_recent)[names(notif_recent) == "kommun"] <- "shiny_name"
names(notif_before)[names(notif_before) == "kommun"] <- "shiny_name"

speedometer_w <- subset(set_speedometer, week == current_week)
speedometer_w$order_kommun <- seq_len(nrow(speedometer_w))
speedometer_w$notification <- NULL

speedometer_w <- merge(speedometer_w, unique(notif_recent), by = "shiny_name", all.x = TRUE)
speedometer_w <- plyr::arrange(speedometer_w, speedometer_w$order_kommun)

speedometer_w$positivity   <- round(speedometer_w$positivity * 100, 1)
speedometer_w$notification <- round(speedometer_w$notification, 0)
speedometer_w$testing      <- round(speedometer_w$testing, 0)

speedometer_notif <- speedometer_w[, c(1, 7)]
speedometer_pos   <- speedometer_w[, c(1, 4)]
speedometer_test  <- speedometer_w[, c(1, 5)]

speedometer_w_last <- subset(set_speedometer, week == previous_week)
speedometer_w_last$order_kommun <- seq_len(nrow(speedometer_w_last))
speedometer_w_last$notification <- NULL

speedometer_w_last <- merge(speedometer_w_last, unique(notif_before), by = "shiny_name", all.x = TRUE)
speedometer_w_last <- plyr::arrange(speedometer_w_last, speedometer_w_last$order_kommun)

speedometer_w_last$positivity   <- round(speedometer_w_last$positivity * 100, 1)
speedometer_w_last$notification <- round(speedometer_w_last$notification, 0)
speedometer_w_last$testing      <- round(speedometer_w_last$testing, 0)

speedometer_w_last_notif <- speedometer_w_last[, c(1, 7)]
speedometer_w_last_pos   <- speedometer_w_last[, c(1, 4)]
speedometer_w_last_test  <- speedometer_w_last[, c(1, 5)]

speedometer_w$compare_perc_not <- compute_percent_change(
  speedometer_w$notification,
  speedometer_w_last$notification,
  inf_replacement = 43,
  digits = 1
)

speedometer_w$compare_perc_pos <- compute_percent_change(
  speedometer_w$positivity,
  speedometer_w_last$positivity,
  digits = 1
)

speedometer_w$compare_perc_test <- compute_percent_change(
  speedometer_w$testing,
  speedometer_w_last$testing,
  digits = 1
)

speedometer_w_perc_diff_speed_not  <- speedometer_w[, c(1, 8)]
speedometer_w_perc_diff_speed_pos  <- speedometer_w[, c(1, 9)]
speedometer_w_perc_diff_speed_test <- speedometer_w[, c(1, 10)]

# ==============================================================================
# CSSS trends
# ==============================================================================

csss_trends <- rio::import(files_csss_trends)
csss_trends$Ort <- NULL

csss_trends2 <- subset(csss_trends, Postnummer %in% c(74, 75))
csss_trends2$day_name <- weekdays(as.Date(csss_trends2$Datum, format = "%Y-%m-%d"))
csss_trends2$week <- lubridate::isoweek(csss_trends2$Datum)

csss_trends3 <- subset(
  csss_trends2,
  Datum > as.Date(lastDataUpdate) - 17 &
    Datum < as.Date(lastDataUpdate) - 2
)

csss_trends3$week <- sub("^", "v.", as.character(csss_trends3$week))

allowed_days <- c("Tuesday", "Wednesday", "Thursday")

csss_filtered <- csss_trends3[csss_trends3$day_name %in% allowed_days, ]
csss_filtered <- csss_filtered[order(csss_filtered$Datum), ]

week_csss <- rep(c("past", "current"), each = 3)
csss_filtered$week_csss <- rep(week_csss, length.out = nrow(csss_filtered))

csss_indicator <- csss_filtered %>%
  dplyr::mutate(
    postal = dplyr::case_when(
      Postnummer == 74 ~ "Länet",
      Postnummer == 75 ~ "Uppsala",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::group_by(postal, week_csss) %>%
  dplyr::summarise(Uppskattning_aver = mean(Uppskattning, na.rm = TRUE), .groups = "drop")

last_week_csss <- subset(csss_indicator, week_csss == "past")
current_week_csss <- subset(csss_indicator, week_csss == "current")

last_week_csss$week_csss <- NULL
current_week_csss$week_csss <- NULL

last_week_csss$Uppskattning_aver <- round(last_week_csss$Uppskattning_aver, 2)
current_week_csss$Uppskattning_aver <- round(current_week_csss$Uppskattning_aver, 2)

perc_diff_csss <- round(
  ((current_week_csss$Uppskattning_aver - last_week_csss$Uppskattning_aver) * 100) /
    current_week_csss$Uppskattning_aver,
  2
)

current_week_csss_change <- current_week_csss
current_week_csss_change$compare_perc_csss <- perc_diff_csss
current_week_csss_change$Uppskattning_aver <- NULL

# ==============================================================================
# Testing maps / ECDC tab
# ==============================================================================

sheets_uppsala <- readxl::excel_sheets(files_uppsala)
uppsala <- rio::import(files_uppsala, which = match("total_allage", sheets_uppsala))

uppsala$notification_rate <- uppsala$cases_per_capita_2w5_allage
uppsala$positivity_rate   <- uppsala$positivity_1w_allage
uppsala$testing_rate      <- uppsala$test_per_capita_1w5_allage

sheets_uppsala_stad_ecdc_map <- readxl::excel_sheets(files_stad)
uppsala_stad_ecdc_map <- rio::import(files_stad, which = match("weekly_allage", sheets_uppsala_stad_ecdc_map))

shape_file_stadsdel <- readOGR("data/stad_shapefile/stad_aug_shp.shp")

datalist_stad_ecdc <- lapply(97:current_week, function(i) {
  dat <- uppsala_stad_ecdc_map %>%
    dplyr::select(
      positivity_2w_allage,
      cases_2w_allage,
      tests_2w_allage,
      stadsdel,
      population,
      dplyr::all_of(names(uppsala_stad_new)[grep(i, names(uppsala_stad_new))])
    )
  
  dat$week <- i
  names(dat) <- c(
    "positivity_2w_allage", "cases_2w_allage", "tests_2w_allage",
    "stadsdel", "population", "positivity_1w", "cases_per_capita_1w5",
    "drop1", "drop2", "test_per_capita_1w5", "week"
  )
  
  dat$drop1 <- NULL
  dat$drop2 <- NULL
  dat
})

stadsdel_all_weeks <- dplyr::bind_rows(datalist_stad_ecdc)
names(stadsdel_all_weeks)[names(stadsdel_all_weeks) == "stadsdel"] <- "stad"

notif_2w_list_df <- dplyr::bind_rows(
  lapply(split(stadsdel_all_weeks, stadsdel_all_weeks$stad), function(df_city) {
    df_city <- df_city[order(df_city$week), ]
    df_city$notification_rate_previous_week <- c(NA, head(df_city$cases_per_capita_1w5, -1))
    df_city
  })
)

notif_2w_list_df$notification_rate_2w <- with(
  notif_2w_list_df,
  notification_rate_previous_week + cases_per_capita_1w5
)

notif_2w_list_df$notification_rate <- notif_2w_list_df$notification_rate_2w
notif_2w_list_df$positivity_rate   <- notif_2w_list_df$positivity_1w
notif_2w_list_df$testing_rate      <- notif_2w_list_df$test_per_capita_1w5

notif_2w_list_df <- drop_existing_cols(
  notif_2w_list_df,
  c("notification_rate_2w", "positivity_1w", "test_per_capita_1w5")
)

notif_2w_list_df$ECDC_Colour <- "Dark_Grey"
notif_2w_list_df$ECDC_Colour[notif_2w_list_df$testing_rate < 300] <- "Light_Grey"

is_green <- (
  (notif_2w_list_df$positivity_rate < 0.01 &
     notif_2w_list_df$notification_rate < 50 &
     notif_2w_list_df$testing_rate >= 300) |
    (notif_2w_list_df$positivity_rate < 0.04 &
       notif_2w_list_df$notification_rate < 50 &
       notif_2w_list_df$testing_rate >= 300)
)

is_orange <- (
  (notif_2w_list_df$positivity_rate >= 0.04 &
     notif_2w_list_df$notification_rate < 50 &
     notif_2w_list_df$testing_rate >= 300) |
    (notif_2w_list_df$positivity_rate >= 0.01 &
       notif_2w_list_df$notification_rate >= 50 &
       notif_2w_list_df$notification_rate < 75 &
       notif_2w_list_df$testing_rate >= 300) |
    (notif_2w_list_df$positivity_rate < 0.04 &
       notif_2w_list_df$notification_rate >= 75 &
       notif_2w_list_df$notification_rate <= 200 &
       notif_2w_list_df$testing_rate >= 300) |
    (notif_2w_list_df$positivity_rate < 0.01 &
       notif_2w_list_df$notification_rate >= 50 &
       notif_2w_list_df$notification_rate < 74 &
       notif_2w_list_df$testing_rate >= 300)
)

is_red <- (
  (notif_2w_list_df$notification_rate > 200 &
     notif_2w_list_df$notification_rate < 500 &
     notif_2w_list_df$testing_rate >= 300) |
    (notif_2w_list_df$positivity_rate >= 0.04 &
       notif_2w_list_df$notification_rate >= 75 &
       notif_2w_list_df$notification_rate <= 200 &
       notif_2w_list_df$testing_rate >= 300)
)

is_dark_red <- notif_2w_list_df$notification_rate >= 500 & notif_2w_list_df$testing_rate >= 300

notif_2w_list_df$ECDC_Colour[is_green]    <- "Green"
notif_2w_list_df$ECDC_Colour[is_orange]   <- "Orange"
notif_2w_list_df$ECDC_Colour[is_red]      <- "Red"
notif_2w_list_df$ECDC_Colour[is_dark_red] <- "DarkRed"

notif_2w_list_df$ECDC_Colour <- factor(
  notif_2w_list_df$ECDC_Colour,
  ordered = TRUE,
  levels = c("Dark_Grey", "Light_Grey", "Green", "Orange", "Red", "DarkRed")
)

shape_file_stadsdel <- spTransform(
  shape_file_stadsdel,
  CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
)

all_data <- merge(shape_file_stadsdel, notif_2w_list_df, by = "stad", duplicateGeoms = TRUE)
all_data <- all_data[complete.cases(all_data$week), ]

all_data$weekNY <- all_data$week

wrong_weeks <- 54:current_week

week_number_2021_ecdc <- seq(1, 52, by = 1)
week_number_2022_ecdc <- seq(1, current_week_numeric, by = 1)

correct_weeks <- c(
  paste("", week_number_2021_ecdc, "2021"),
  paste("", week_number_2022_ecdc, "2022")
)

all_data$weekNY[all_data$weekNY %in% wrong_weeks] <- correct_weeks[
  match(all_data$weekNY[all_data$weekNY %in% wrong_weeks], wrong_weeks)
]

all_data$weekNY <- as.character(all_data$weekNY)
levels_alldata <- as.character(correct_weeks)

all_data$weekNY2 <- factor(
  all_data$weekNY,
  ordered = TRUE,
  levels = unique(levels_alldata)
)

all_data$weekNY2 <- sub(" 202.*", "", all_data$weekNY2)
all_data$weekNY2 <- sub(" ", "", all_data$weekNY2)

all_data <- all_data[!is.na(all_data$weekNY2), ]

create_week_data <- function(weekNY) {
  all_data[all_data$weekNY2 == weekNY, ]
}

cities <- c(
  "Uppsala Län (388 803 inv.)", "Enköping (46 326 inv.)", "Heby (13 982 inv.)",
  "Håbo (22 058 inv.)", "Knivsta (18 447 inv.)", "Tierp (21 182 inv.)",
  "Uppsala (234 935 inv.)", "Älvkarleby (9 627 inv.)", "Östhammar (22 246 inv.)"
)

coordinates <- data.frame(
  cities = cities,
  long   = c(17.6794, 17.0779, 16.8594, 17.5250, 17.7921, 17.520277, 17.6433, 17.4491, 18.3654),
  lati   = c(59.9445, 59.6355, 59.9393, 59.5619, 59.726105, 60.3470, 59.8569, 60.568604, 60.2593),
  zoom   = c(9, 12, 12, 12, 12, 12, 12, 12, 12)
)

create_city_data <- function(city_name) {
  coordinates[coordinates$cities == city_name, ]
}

# ==============================================================================
# Vaccine traffic
# ==============================================================================

total_pop_plus_for_vaccine <- rio::import("data/SCB_pop_per_age_group_and_postal.xlsx", which = 1)
total_pop_plus_for_vaccine_plus <- rio::import("data/SCB_proper_age_group_plus_15.xlsx", which = 1)

total_pop_plus_for_vaccine <- subset(total_pop_plus_for_vaccine, age_group != "Totalt")
total_pop_plus_for_vaccine <- total_pop_plus_for_vaccine[, c(1, 3, 4)]

age_lookup <- c(
  "0-4 år"    = "0-4",
  "5-9 år"    = "5-9",
  "10-11 år"  = "10-11",
  "12-14 år"  = "12-14",
  "15-19 år"  = "15-19",
  "20-24 år"  = "20-29",
  "25-29 år"  = "20-29",
  "30-34 år"  = "30-39",
  "35-39 år"  = "30-39",
  "40-44 år"  = "40-49",
  "45-49 år"  = "40-49",
  "50-54 år"  = "50-59",
  "55-59 år"  = "50-59",
  "60-64 år"  = "60-69",
  "65-69 år"  = "60-69",
  "70-74 år"  = "70+",
  "75-79 år"  = "70+",
  "80-84 år"  = "70+",
  "85-89 år"  = "70+",
  "90-94 år"  = "70+",
  "95-99 år"  = "70+",
  "100+ år"   = "70+"
)

total_pop_plus_for_vaccine$agegroupdecade <- dplyr::recode(
  total_pop_plus_for_vaccine$age_group,
  !!!as.list(age_lookup)
)

vaccine_ecdc_stad <- rio::import("data/ServicePoints_List_PostalCode_5Aug2021.xlsx", which = 1)
vaccine_ecdc_stad <- vaccine_ecdc_stad[, c(1, 3)]
colnames(vaccine_ecdc_stad) <- c("postalcode", "stadsdel")

vacc_traffic_data4 <- plyr::join(
  total_pop_plus_for_vaccine,
  vaccine_ecdc_stad,
  by    = "postalcode",
  type  = "left",
  match = "all"
)

vacc_traffic_data4 <- vacc_traffic_data4[, c(1, 3, 4, 5)]

vacc_traffic_data4_plus <- plyr::join(
  total_pop_plus_for_vaccine_plus,
  vaccine_ecdc_stad,
  by    = "postalcode",
  type  = "left",
  match = "all"
)

colnames(vacc_traffic_data4_plus) <- c("postalcode", "agegroupdecade", "value", "stadsdel")
vacc_traffic_data4_plus <- vacc_traffic_data4_plus[, c(1, 3, 2, 4)]

vacc_traffic_data4_withplus <- rbind(vacc_traffic_data4, vacc_traffic_data4_plus)

vacc_traffic_data4_agegroupdecade <- aggregate(
  list(population = vacc_traffic_data4_withplus$value),
  by = list(
    Stadsdel = vacc_traffic_data4_withplus$stadsdel,
    agegroup = vacc_traffic_data4_withplus$agegroupdecade
  ),
  FUN = sum,
  na.rm = TRUE,
  na.action = NULL
)

# ==============================================================================
# Vaccine traffic: population joins, uptake frequencies and map-ready structure
# ==============================================================================

# Manual correction retained to preserve the original source workflow.
vacc_traffic_data4_agegroupdecade[
  vacc_traffic_data4_agegroupdecade$Stadsdel == "Vänge (Uppsala)" &
    vacc_traffic_data4_agegroupdecade$agegroup == "15plus",
  "population"
] <- 3073

assign_vaccination_colour <- function(x, population, min_population = 50) {
  out <- rep("Dark_Grey", length(x))
  
  eligible <- !is.na(x) & population >= min_population
  out[eligible & x >= 0.90] <- "Green"
  out[eligible & x < 0.90 & x >= 0.80] <- "Orange"
  out[eligible & x < 0.80 & x >= 0.70] <- "Red"
  out[eligible & x < 0.70] <- "DarkRed"
  
  factor(
    out,
    ordered = TRUE,
    levels = c("Dark_Grey", "Green", "Orange", "Red", "DarkRed")
  )
}

map_informal_weeks <- function(x, from, to) {
  x[x %in% from] <- to[match(x[x %in% from], from)]
  x
}

build_complete_panel <- function(weeks, stad, agegroup) {
  expand.grid(
    week = weeks,
    stad = sort(unique(stad)),
    agegroup = unique(agegroup),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
}

make_pump_table <- function(df, week_col, value_col, pump_name, abbr, order_id) {
  out <- df[, c(week_col, value_col)]
  out$Pump <- pump_name
  out$abbr <- abbr
  out$order <- order_id
  names(out) <- c("week_new", "RR_N1copesPPMoV", "Pump", "abbr", "order")
  out$RR_N1copesPPMoV <- as.numeric(out$RR_N1copesPPMoV)
  out
}

# Vaccination inputs
vaccine_decades <- rio::import("data/vacc_dose1_2_stad_age_group.csv", which = 1)
vaccine_decades_plus_all_cum <- rio::import("data/vacc_dose1_2_stad_age_group_15plus.csv", which = 1)

vaccine_decades <- rbind(vaccine_decades, vaccine_decades_plus_all_cum)

vaccine_decades <- plyr::join(
  vaccine_decades,
  vacc_traffic_data4_agegroupdecade,
  by    = c("Stadsdel", "agegroup"),
  type  = "left",
  match = "first"
)

vaccine_decades3 <- vaccine_decades
vaccine_decades3 <- vaccine_decades3[complete.cases(vaccine_decades3$agegroup), ]

# Weekly and cumulative vaccination frequencies
vaccine_decades3$freq_dose1     <- vaccine_decades3$vacc_dose1_raw    / vaccine_decades3$population
vaccine_decades3$cum_freq_dose1 <- vaccine_decades3$vacc_dose1_cum_3w / vaccine_decades3$population

vaccine_decades3$freq_dose2     <- vaccine_decades3$vacc_dose2_raw    / vaccine_decades3$population
vaccine_decades3$cum_freq_dose2 <- vaccine_decades3$vacc_dose2_cum_2w / vaccine_decades3$population

vaccine_decades3$freq_dose3     <- vaccine_decades3$vacc_dose3_raw    / vaccine_decades3$population
vaccine_decades3$cum_freq_dose3 <- vaccine_decades3$vacc_dose3_cum_1w / vaccine_decades3$population

small_population <- vaccine_decades3$population < 50
vaccine_decades3$cum_freq_dose1[small_population] <- NA_real_
vaccine_decades3$cum_freq_dose2[small_population] <- NA_real_
vaccine_decades3$cum_freq_dose3[small_population] <- NA_real_

vaccine_decades3$vacc_color_dose1 <- assign_vaccination_colour(
  vaccine_decades3$cum_freq_dose1,
  vaccine_decades3$population
)

vaccine_decades3$vacc_color_dose2 <- assign_vaccination_colour(
  vaccine_decades3$cum_freq_dose2,
  vaccine_decades3$population
)

vaccine_decades3$vacc_color_dose3 <- assign_vaccination_colour(
  vaccine_decades3$cum_freq_dose3,
  vaccine_decades3$population
)

vaccine_decades3$cum_freq_dose1 <- round(vaccine_decades3$cum_freq_dose1, 3) * 100
vaccine_decades3$cum_freq_dose2 <- round(vaccine_decades3$cum_freq_dose2, 3) * 100
vaccine_decades3$cum_freq_dose3 <- round(vaccine_decades3$cum_freq_dose3, 3) * 100

names(vaccine_decades3)[names(vaccine_decades3) == "Stadsdel"] <- "stad"

all_together2 <- build_complete_panel(
  weeks = wrong_weeks_vacc,
  stad = vaccine_decades3$stad,
  agegroup = vaccine_decades3$agegroup
)

vaccine_decades4 <- plyr::join(
  all_together2,
  vaccine_decades3,
  by    = c("week", "stad", "agegroup"),
  type  = "left",
  match = "all"
)

vaccine_decades4 <- vaccine_decades4[
  !duplicated(vaccine_decades4[c("stad", "week", "agegroup")]),
]

shape_file_vacc <- merge(
  shape_file_stadsdel,
  vaccine_decades4,
  by = "stad",
  duplicateGeoms = TRUE
)

shape_file_vacc2 <- shape_file_vacc
shape_file_vacc2 <- shape_file_vacc2[!is.na(shape_file_vacc2$week), ]
shape_file_vacc2 <- shape_file_vacc2[shape_file_vacc2$week > (non_iso_week_current - 20), ]

correct_weeks_vacc <- c(32:52, 1:current_week_numeric)
wrong_weeks_vacc <- 85:current_week

shape_file_vacc2$week <- map_informal_weeks(
  shape_file_vacc2$week,
  from = wrong_weeks_vacc,
  to   = correct_weeks_vacc
)

shape_file_vacc2$week <- as.character(shape_file_vacc2$week)
shape_file_vacc2$week <- factor(
  shape_file_vacc2$week,
  ordered = TRUE,
  levels = c(32:52, 1:current_week_numeric)
)

shape_file_vacc2$week2 <- shape_file_vacc2$week
shape_file_vacc2 <- shape_file_vacc2[complete.cases(shape_file_vacc2$week2), ]
shape_file_vacc2 <- shape_file_vacc2[complete.cases(shape_file_vacc2$agegroup), ]

create_week_data_vacc_ecdc <- function(x_week_vacc, y_agegroup) {
  shape_file_vacc2[
    (shape_file_vacc2@data$week2 == x_week_vacc) &
      (shape_file_vacc2@data$agegroup == y_agegroup),
  ]
}

# ==============================================================================
# Wastewater
# ==============================================================================

wastewater <- rio::import("data/SARS-CoV-2 in Uppsala wastewater.xlsx", which = 1)
wastewater2 <- wastewater[complete.cases(wastewater$week), ]

# Multiplot wastewater series
wastewater2_Uppsala <- wastewater2[, c(2, 5)]
wastewater2_Uppsala$Pump <- "Uppsala"
wastewater2_Uppsala$abbr <- "Upp."
wastewater2_Uppsala$order <- 1
names(wastewater2_Uppsala)[names(wastewater2_Uppsala) == "Uppsala"] <- "RR_N1copesPPMoV"
wastewater2_Uppsala$RR_N1copesPPMoV <- as.numeric(wastewater2_Uppsala$RR_N1copesPPMoV)

wastewater_pumps2_multiplot <- wastewater2_Uppsala
wastewater_pumps2_multiplot$Pump <- "SARS-CoV-2 i avloppsvatten"

wastewater_pumps2_multiplot_2020 <- wastewater_pumps2_multiplot[1:17, ]
wastewater_pumps2_multiplot_2021 <- wastewater_pumps2_multiplot[18:69, ]
wastewater_pumps2_multiplot_2022 <- wastewater_pumps2_multiplot[70:nrow(wastewater_pumps2_multiplot), ]

wrong_weeks_kmb_vacc_2021 <- 54:105
correct_weeks_kmb_vacc_2021 <- 1:52

wrong_weeks_kmb_vacc_2022 <- 106:current_week
correct_weeks_kmb_vacc_2022 <- 1:current_week_numeric

wastewater_pumps2_multiplot_2021$week <- map_informal_weeks(
  wastewater_pumps2_multiplot_2021$week,
  from = correct_weeks_kmb_vacc_2021,
  to   = wrong_weeks_kmb_vacc_2021
)

wastewater_pumps2_multiplot_2022$week <- map_informal_weeks(
  wastewater_pumps2_multiplot_2022$week,
  from = correct_weeks_kmb_vacc_2022,
  to   = wrong_weeks_kmb_vacc_2022
)

wastewater_pumps2_multiplot <- rbind(
  wastewater_pumps2_multiplot_2020,
  wastewater_pumps2_multiplot_2021,
  wastewater_pumps2_multiplot_2022
)

# Uppsala line overlay data
uppsala_multi <- rio::import(files_uppsala, which = 2)
uppsala_multi2 <- uppsala_multi[, c(1, 2, 3, 4, 6)]

uppsala_multi2$test_per_whole  <- round((uppsala_multi2$total_tests * 100000) / uppsala_multi2$pop, 0)
uppsala_multi2$cases_per_whole <- round((uppsala_multi2$total_pos * 100000) / uppsala_multi2$pop, 0)
uppsala_multi2$positivity      <- round(uppsala_multi2$total_pos / uppsala_multi2$total_tests, 2)

uppsala_multi2_UppKom <- subset(uppsala_multi2, kommun == "UPPSALA")

uppsala_multi2_UppLan_tests <- aggregate(
  x  = uppsala_multi2$total_tests,
  by = list(uppsala_multi2$week),
  FUN = sum,
  na.rm = TRUE,
  na.action = NULL
)
colnames(uppsala_multi2_UppLan_tests) <- c("week", "total_tests")

uppsala_multi2_UppLan_pos <- aggregate(
  x  = uppsala_multi2$total_pos,
  by = list(uppsala_multi2$week),
  FUN = sum,
  na.rm = TRUE,
  na.action = NULL
)
colnames(uppsala_multi2_UppLan_pos) <- c("week", "total_pos")

uppsala_multi2_UppLan <- merge(
  uppsala_multi2_UppLan_tests,
  uppsala_multi2_UppLan_pos,
  by = "week",
  all = TRUE
)

uppsala_multi2_UppLan$pop <- number_total_pop_upps_lan
uppsala_multi2_UppLan$kommun <- "LÄN"
uppsala_multi2_UppLan <- uppsala_multi2_UppLan[, c(5, 1:4)]

uppsala_multi2_UppLan$test_per_whole  <- round((uppsala_multi2_UppLan$total_tests * 100000) / uppsala_multi2_UppLan$pop, 0)
uppsala_multi2_UppLan$cases_per_whole <- round((uppsala_multi2_UppLan$total_pos * 100000) / uppsala_multi2_UppLan$pop, 0)
uppsala_multi2_UppLan$positivity      <- round(uppsala_multi2_UppLan$total_pos / uppsala_multi2_UppLan$total_tests, 2)

# Simple wastewater series for all monitored pumps
wastewater2_simply_plot <- wastewater2

wastewater_All <- list(
  make_pump_table(wastewater2_simply_plot, 3, 5,  "Uppsala",     "Upp.", 1),
  make_pump_table(wastewater2_simply_plot, 3, 6,  "Knivsta",     "Kni.", 2),
  make_pump_table(wastewater2_simply_plot, 3, 7,  "Enköping",    "Enk.", 3),
  make_pump_table(wastewater2_simply_plot, 3, 8,  "Älvkarleby",  "Älv.", 4),
  make_pump_table(wastewater2_simply_plot, 3, 9,  "Östhammar",   "Öst.", 5),
  make_pump_table(wastewater2_simply_plot, 3, 10, "Tierp",       "Tie.", 6)
)

wastewater_All_bind22 <- dplyr::bind_rows(wastewater_All)

wastewater_All_bind22$missing_label <- wastewater_All_bind22$RR_N1copesPPMoV
wastewater_All_bind22$missing_label[is.na(wastewater_All_bind22$missing_label)] <- "Mätning saknas"
wastewater_All_bind22$missing_label[wastewater_All_bind22$missing_label != "Mätning saknas"] <- NA

wastewater_All_bind22$week_new <- factor(wastewater_All_bind22$week_new, ordered = TRUE)

StartDate <- as.Date("2020-08-24")
EndDate <- most_recent_monday

myDays <- seq(StartDate, EndDate, by = "week")

monday_dates <- data.frame(
  monday_dates = myDays,
  week_new = paste0(lubridate::year(myDays), "-", lubridate::isoweek(myDays))
)
monday_dates$week_new <- factor(monday_dates$week_new, ordered = TRUE)

week_2020_df <- data.frame(
  week_new = factor(paste0("2020-", 35:53), ordered = TRUE),
  week = 35:53
)

week_2021_df <- data.frame(
  week_new = factor(paste0("2021-", c(1:9, 10:52)), ordered = TRUE),
  week = 54:105
)

week_2022_df <- data.frame(
  week_new = factor(paste0("2022-", 1:current_week_numeric), ordered = TRUE),
  week = 106:current_week_sew
)

weeks_all <- rbind(week_2020_df, week_2021_df, week_2022_df)
weeks_all$week_new <- factor(weeks_all$week_new, ordered = TRUE)

all_together_week_kommun_waste <- expand.grid(
  week_new = unique(weeks_all$week_new),
  Pump = unique(wastewater_All_bind22$Pump),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)

all_together_week_kommun_waste <- plyr::join(
  all_together_week_kommun_waste,
  weeks_all,
  by    = "week_new",
  type  = "left",
  match = "first"
)

wastewater_All_bind22_old_new_weeks <- plyr::join(
  all_together_week_kommun_waste,
  wastewater_All_bind22,
  by    = c("week_new", "Pump"),
  type  = "left",
  match = "first"
)

wastewater_All_bind22_old_new_weeks2 <- plyr::join(
  wastewater_All_bind22_old_new_weeks,
  monday_dates,
  by    = "week_new",
  type  = "left",
  match = "first"
)

wastewater_All_bind22_old_new_weeks2$Pump <- factor(
  wastewater_All_bind22_old_new_weeks2$Pump,
  ordered = TRUE,
  levels = c("Uppsala", "Knivsta", "Östhammar", "Älvkarleby", "Enköping", "Tierp")
)

kommun_names_waste <- c(
  "Enköping (46 326 inv.)"   = "Enköping",
  "Knivsta (18 447 inv.)"    = "Knivsta",
  "Tierp (21 182 inv.)"      = "Tierp",
  "Uppsala (234 935 inv.)"   = "Uppsala",
  "Älvkarleby (9 627 inv.)"  = "Älvkarleby",
  "Östhammar (22 246 inv.)"  = "Östhammar"
)

kommun_names_waste_labeller <- function(value) {
  kommun_names_waste[value]
}

# ==============================================================================
# CSSS data for multiplot
# ==============================================================================

csss_trends_sewage <- csss_trends[, c(1, 2, 3, 4, 5)]
csss_trends_line <- subset(csss_trends_sewage, Postnummer %in% c(74, 75))

csss_trends_line$week <- lubridate::isoweek(csss_trends_line$Datum)
csss_trends_line$week <- map_informal_weeks(
  csss_trends_line$week,
  from = correct_weeks,
  to   = wrong_weeks
)

csss_trends_line$Ort <- NULL

csss_trends_line_75 <- subset(csss_trends_line, Postnummer == 75)

csss_75_average_w <- aggregate(
  x  = csss_trends_line_75$Uppskattning,
  by = list(csss_trends_line_75$week),
  FUN = base::mean,
  na.rm = TRUE,
  na.action = NULL
)

colnames(csss_75_average_w) <- c("week", "Uppskattning")
csss_75_average_w <- subset(csss_75_average_w, week > 34)

csss_lan_trends_w <- rio::import(files_csss_lan_trends_w)
csss_lan_trends_w <- subset(csss_lan_trends_w, Lan == "Uppsala")

csss_lan_trends_w$week <- lubridate::isoweek(csss_lan_trends_w$Datum)
csss_lan_trends_w$week <- map_informal_weeks(
  csss_lan_trends_w$week,
  from = correct_weeks,
  to   = wrong_weeks
)

csss_lan_average_w <- aggregate(
  x  = csss_lan_trends_w$Uppskattning,
  by = list(csss_lan_trends_w$week),
  FUN = base::mean,
  na.rm = TRUE,
  na.action = NULL
)

csss_lan_average_w_CIL <- aggregate(
  x  = csss_lan_trends_w$Low_CI,
  by = list(csss_lan_trends_w$week),
  FUN = base::mean,
  na.rm = TRUE,
  na.action = NULL
)

csss_lan_average_w_CIH <- aggregate(
  x  = csss_lan_trends_w$High_CI,
  by = list(csss_lan_trends_w$week),
  FUN = base::mean,
  na.rm = TRUE,
  na.action = NULL
)

colnames(csss_lan_average_w)     <- c("week", "Uppskattning")
colnames(csss_lan_average_w_CIL) <- c("week", "lowCI")
colnames(csss_lan_average_w_CIH) <- c("week", "upperCI")

csss_lan_average_w_CI <- merge(csss_lan_average_w_CIH, csss_lan_average_w_CIL, by = "week")
csss_lan_average_w <- subset(csss_lan_average_w, week > 34)

# ==============================================================================
# Hospitalizations
# ==============================================================================

files_uppsala_multi_hospit_Kommun <- files_ian
uppsala_multi_hospit_Kommun <- rio::import(files_uppsala_multi_hospit_Kommun, which = 3)
uppsala_multi_hospit_Lan    <- rio::import(files_uppsala_multi_hospit_Kommun, which = 4)

uppsala_multi_hospit_Kommun <- subset(uppsala_multi_hospit_Kommun, kommun == "UPPSALA")

uppsala_multi_hospit_Lan$pat_per_1k_adult    <- (uppsala_multi_hospit_Lan$Antalpatienter * 1000) / 315289
uppsala_multi_hospit_Kommun$pat_per_1k_adult <- (uppsala_multi_hospit_Kommun$Antalpatienter * 1000) / 191205

uppsala_multi_hospit_Lan    <- subset(uppsala_multi_hospit_Lan, week > 34)
uppsala_multi_hospit_Kommun <- subset(uppsala_multi_hospit_Kommun, week > 34)

# ==============================================================================
# Download buttons
# ==============================================================================

myDownloadButton <- function(outputId, label = "Ladda ner data") {
  tags$a(
    id = outputId,
    class = "btn btn-default shiny-download-link",
    href = "",
    target = "_blank",
    download = NA,
    icon("save"),
    label
  )
}

myDownloadButton_description <- function(outputId, label = "Ladda ner data beskrivning") {
  tags$a(
    id = outputId,
    class = "btn btn-default shiny-download-link",
    href = "",
    target = "_blank",
    download = NA,
    icon("save"),
    label
  )
}

# ==============================================================================
# 112 / 1177 calls
# ==============================================================================

kommun_names_calls <- c(
  "Uppsala Län (323 111 inv.)" = "*LÄNET",
  "Enköping (38 088 inv.)"     = "ENKÖPING",
  "Heby (11 715 inv.)"         = "HEBY",
  "Håbo (17 940 inv.)"         = "HÅBO",
  "Knivsta (14 334 inv.)"      = "KNIVSTA",
  "Tierp (17 681 inv.)"        = "TIERP",
  "Uppsala (196 491 inv.)"     = "UPPSALA",
  "Älvkarleby (8 006 inv.)"    = "ÄLVKARLEBY",
  "Östhammar (18 856 inv.)"    = "ÖSTHAMMAR"
)

kommun_labeller_calls <- function(value) {
  kommun_names_calls[value]
}

all_kommun_lan2 <- rio::import("data/calls112.csv")
final_for_plotting2 <- rio::import("data/calls1177.csv")

final_for_plotting2[final_for_plotting2$week == 100, "adjusted_freq_100k"] <- 0
final_for_plotting2[final_for_plotting2$week == 106, "adjusted_freq_100k"] <- 0

all_kommun_lan2_calls_plot <- all_kommun_lan2[
  all_kommun_lan2$yearweek_floor > min(week_mondays) - 7,
]

all_kommun_lan2_calls_plot$yearweek_floor <- as.Date(
  all_kommun_lan2_calls_plot$yearweek_floor,
  format = "%Y-%m-%d"
)

create_calls_data_all_kommun <- function(cities) {
  all_kommun_lan2_calls_plot[
    all_kommun_lan2_calls_plot$kommun == as.character(kommun_labeller_calls(cities)[[1]]),
  ]
}

final_for_plotting2$yearweek_floor <- as.Date(
  final_for_plotting2$yearweek_floor,
  format = "%Y-%m-%d"
)

create_calls_data_final_plotting <- function(cities) {
  final_for_plotting2[
    final_for_plotting2$kommun == as.character(kommun_labeller_calls(cities)[[1]]),
  ]
}

all_kommun_lan2_lan <- subset(all_kommun_lan2, kommun == "*LÄNET")
all_kommun_lan2_upp <- subset(all_kommun_lan2, kommun == "UPPSALA")

all_kommun_lan2_lan$yearweek_floor <- as.Date(all_kommun_lan2_lan$yearweek_floor, format = "%Y-%m-%d")
all_kommun_lan2_upp$yearweek_floor <- as.Date(all_kommun_lan2_upp$yearweek_floor, format = "%Y-%m-%d")

all_kommun_lan2_lan$week <- as.numeric(lubridate::isoweek(all_kommun_lan2_lan$yearweek_floor))
all_kommun_lan2_upp$week <- as.numeric(lubridate::isoweek(all_kommun_lan2_upp$yearweek_floor))

all_kommun_lan2_lan$week <- map_informal_weeks(
  all_kommun_lan2_lan$week,
  from = correct_weeks,
  to   = wrong_weeks
)

all_kommun_lan2_upp$week <- map_informal_weeks(
  all_kommun_lan2_upp$week,
  from = correct_weeks,
  to   = wrong_weeks
)

all_kommun_lan2_upp <- subset(all_kommun_lan2_upp, yearweek_floor > as.Date("2020-08-24"))
all_kommun_lan2_lan <- subset(all_kommun_lan2_lan, yearweek_floor > as.Date("2020-08-24"))

final_for_plotting2_lan <- subset(final_for_plotting2, kommun == "*LÄNET")
final_for_plotting2_upp <- subset(final_for_plotting2, kommun == "UPPSALA")

final_for_plotting2_upp$week <- as.numeric(final_for_plotting2_upp$week)
final_for_plotting2_lan$week <- as.numeric(final_for_plotting2_lan$week)

# ==============================================================================
# Heatmaps
# ==============================================================================

ageweekpos <- rio::import(files_uppsala, which = 5)
ageweekpos_kom <- rio::import(files_uppsala, which = 4)

wrong_age_cats <- c("1a", "1m", 2, 3, 4, 5, 6, 7)
correct_age_cat <- c("6-12", "13-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70+")

ageweekpos$age_cat[ageweekpos$age_cat %in% wrong_age_cats] <- correct_age_cat[
  match(ageweekpos$age_cat[ageweekpos$age_cat %in% wrong_age_cats], wrong_age_cats)
]

ageweekpos_kom$age_cat[ageweekpos_kom$age_cat %in% wrong_age_cats] <- correct_age_cat[
  match(ageweekpos_kom$age_cat[ageweekpos_kom$age_cat %in% wrong_age_cats], wrong_age_cats)
]

age_levels <- c("6-12", "13-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70+")

ageweekpos$age_cat <- factor(ageweekpos$age_cat, ordered = TRUE, levels = age_levels)
ageweekpos_kom$age_cat <- factor(ageweekpos_kom$age_cat, ordered = TRUE, levels = age_levels)

ageweekpos <- subset(ageweekpos, week > 38)
ageweekpos_kom <- subset(ageweekpos_kom, week > 38)

ageweekpos$positivity_10 <- ageweekpos$positivity * 100
ageweekpos_kom$positivity_10 <- ageweekpos_kom$positivity * 100

ageweekpos$kommun <- "LÄNET"
ageweek_All <- rbind(ageweekpos, ageweekpos_kom)

kommun_names_heatmap <- c(
  "Uppsala Län" = "LÄNET",
  "Enköping" = "ENKÖPING",
  "Heby" = "HEBY",
  "Håbo" = "HÅBO",
  "Knivsta" = "KNIVSTA",
  "Tierp" = "TIERP",
  "Uppsala" = "UPPSALA",
  "Älvkarleby" = "ÄLVKARLEBY",
  "Östhammar" = "ÖSTHAMMAR"
)

kommun_labeller_heatmap <- function(value) {
  kommun_names_heatmap[value]
}

create_heatmap_data <- function(cities) {
  ageweek_All[
    ageweek_All$kommun == as.character(kommun_labeller_heatmap(cities)[[1]]),
  ]
}

# ==============================================================================
# Google mobility
# ==============================================================================

week_mondays_mob <- seq(most_recent_monday - 56, most_recent_monday + 7, by = "1 week")
labelISOweek <- paste("", lubridate::isoweek(week_mondays_mob), sep = "")
labelISOweek2 <- c(" ", labelISOweek)

mobility_se_old <- rio::import("data/2020_SE_Region_Mobility_Report.csv", na.strings = "")
mobility_se_old <- mobility_se_old[complete.cases(mobility_se_old[, 3]), ]
mobility_se_old <- subset(mobility_se_old, date < as.Date("2021-01-01"))
mobility_se_old <- subset(mobility_se_old, sub_region_1 == "Uppsala County")

mobility_se <- rio::import("data/2021_SE_Region_Mobility_Report.csv", na.strings = "")
mobility_se_new <- rio::import("data/2022_SE_Region_Mobility_Report.csv", na.strings = "")

mobility_se     <- mobility_se[complete.cases(mobility_se[, 3]), ]
mobility_se_new <- mobility_se_new[complete.cases(mobility_se_new[, 3]), ]

mobility_se     <- subset(mobility_se, sub_region_1 == "Uppsala County")
mobility_se_new <- subset(mobility_se_new, sub_region_1 == "Uppsala County")

uppsala_mobility2 <- rbind(mobility_se_new, mobility_se, mobility_se_old)
uppsala_mobility2[is.na(uppsala_mobility2$sub_region_2), 4] <- "LÄNET"

uppsala_mobility2$week <- paste0(
  lubridate::isoweek(uppsala_mobility2$date),
  "-",
  lubridate::isoyear(uppsala_mobility2$date)
)

mobility <- uppsala_mobility2[, c(4, 9, 10, 11, 12, 13, 14, 15, 16)]

colnames(mobility) <- c(
  "kommun", "date", "retail_recreation_GM", "grocery_and_pharmacy_GM",
  "parks_GM", "transit_stations_GM", "workplaces_GM", "residential_GM", "week"
)

mobility2 <- mobility[, c(1, 2, 3, 7, 8, 9)]

kommun_names_mobility <- c(
  "Uppsala Län (388 803 inv.)" = "LÄNET",
  "Enköping (46 326 inv.)" = "Enköping Municipality",
  "Heby (13 982 inv.)" = "Heby Municipality",
  "Håbo (22 058 inv.)" = "Håbo Municipality",
  "Knivsta (18 447 inv.)" = "Knivsta Municipality",
  "Tierp (21 182 inv.)" = "Tierp Municipality",
  "Uppsala (234 935 inv.)" = "Uppsala",
  "Älvkarleby (9 627 inv.)" = "Älvkarleby Municipality",
  "Östhammar (22 246 inv.)" = "Östhammar Municipality"
)

kommun_labeller_mobility <- function(value) {
  kommun_names_mobility[value]
}

mobility_weeks <- 42:current_week

mobility_raw_sma <- mobility2 %>%
  dplyr::arrange(date, kommun) %>%
  dplyr::group_by(kommun) %>%
  dplyr::mutate(
    retail_recreation_GM_sma = zoo::rollapply(retail_recreation_GM, 7, mean, na.rm = TRUE, fill = NA),
    workplaces_GM_sma        = zoo::rollapply(workplaces_GM, 7, mean, na.rm = TRUE, fill = NA),
    residential_GM_sma       = zoo::rollapply(residential_GM, 7, mean, na.rm = TRUE, fill = NA)
  ) %>%
  dplyr::ungroup()

mobility_raw_sma2 <- as.data.frame(mobility_raw_sma)
mobility_sma2 <- subset(mobility_raw_sma2, date > as.Date("2020-06-01"))

wrong_weeks_mob <- 23:current_week

week_number_2020_ecdc <- paste0(23:53, "-2020")
week_number_2021_ecdc_mob <- paste(week_number_2021_ecdc, "2021", sep = "-")
week_number_2022_ecdc_mob <- paste(week_number_2022_ecdc, "2022", sep = "-")

correct_weeks_mob <- c(
  week_number_2020_ecdc,
  week_number_2021_ecdc_mob,
  week_number_2022_ecdc_mob
)

mobility_sma2$week <- map_informal_weeks(
  mobility_sma2$week,
  from = correct_weeks_mob,
  to   = wrong_weeks_mob
)

mobility_sma2$week <- as.numeric(mobility_sma2$week)
mobility_sma2 <- mobility_sma2[, c(1, 2, 6:9)]

order_kommun <- data.frame(
  kommun = c(
    "LÄNET",
    "Enköping Municipality",
    "Heby Municipality",
    "Håbo Municipality",
    "Knivsta Municipality",
    "Tierp Municipality",
    "Uppsala",
    "Älvkarleby Municipality",
    "Östhammar Municipality"
  ),
  order = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
)

cols_mob <- c(
  "Shopping och nöje (förändring av antalet besökare)" = "#dc4437",
  "Arbetsplatser (förändring av antalet besökare)" = "#4286f5",
  "Bostadsområden (förändring i varaktighet)" = "#f5c027"
)

cols_mob2 <- factor(
  cols_mob,
  ordered = TRUE,
  levels = c(
    "Bostadsområden (förändring i varaktighet)",
    "Shopping och nöje (förändring av antalet besökare)",
    "Arbetsplatser (förändring av antalet besökare)"
  )
)

mobility_sma3 <- merge(order_kommun, mobility_sma2, by = "kommun", all = FALSE)
mobility_sma3$date <- as.Date(mobility_sma3$date, format = "%Y-%m-%d")

workspace_suitcase <- mobility_sma3[
  mobility_sma3$date == current_date_mobility_start + 1,
  c(1, 2, 3, 4, 6)
]

shopping_cart <- mobility_sma3[
  mobility_sma3$date == current_date_mobility_start + 1,
  c(1, 2, 3, 4, 5)
]

resident_house <- mobility_sma3[
  mobility_sma3$date == current_date_mobility_start + 1,
  c(1, 2, 3, 4, 7)
]

mobility_weekly_avg_lis3_classic_plot <- subset(
  mobility_sma3,
  week > (current_week - 10)
)

create_mobility_data <- function(cities) {
  mobility_weekly_avg_lis3_classic_plot[
    mobility_weekly_avg_lis3_classic_plot$kommun ==
      as.character(kommun_labeller_mobility(cities)[[1]]),
  ]
}

# Weekly averages for multiplot
mobility_weekly_average_10_weekly <- mobility2
mobility_weekly_average_10_weekly$week <- map_informal_weeks(
  mobility_weekly_average_10_weekly$week,
  from = correct_weeks_mob,
  to   = wrong_weeks_mob
)

mobility_weekly_average_10_weekly$week <- as.numeric(mobility_weekly_average_10_weekly$week)
mobility_weekly_average_10_weekly <- subset(
  mobility_weekly_average_10_weekly,
  date > as.Date("2020-06-01")
)

retail_recreation_GM_avg <- stats::aggregate(
  retail_recreation_GM ~ kommun + week,
  FUN = base::mean,
  data = mobility_weekly_average_10_weekly
)

workplaces_GM_avg <- stats::aggregate(
  workplaces_GM ~ kommun + week,
  FUN = base::mean,
  data = mobility_weekly_average_10_weekly
)

residential_GM_avg <- stats::aggregate(
  residential_GM ~ kommun + week,
  FUN = base::mean,
  data = mobility_weekly_average_10_weekly
)

mobility_weekly_average_10_merged <- Reduce(
  function(x, y) plyr::join(x, y, by = c("kommun", "week"), type = "left", match = "all"),
  list(workplaces_GM_avg, retail_recreation_GM_avg, residential_GM_avg)
)

colnames(mobility_weekly_average_10_merged) <- c(
  "kommun", "week", "workplaces_GM_avg",
  "retail_recreation_GM_avg", "residential_GM_avg"
)

mobility_weekly_avg_lis3_lan <- subset(mobility_weekly_average_10_merged, kommun == "LÄNET" & week > 34)
mobility_weekly_avg_lis3_uppsala <- subset(mobility_weekly_average_10_merged, kommun == "Uppsala" & week > 34)

create_workspace_suitcase <- function(cities) {
  workspace_suitcase[
    workspace_suitcase$kommun == as.character(kommun_labeller_mobility(cities)[[1]]),
  ]
}

create_shopping_cart <- function(cities) {
  shopping_cart[
    shopping_cart$kommun == as.character(kommun_labeller_mobility(cities)[[1]]),
  ]
}

create_resident_house <- function(cities) {
  resident_house[
    resident_house$kommun == as.character(kommun_labeller_mobility(cities)[[1]]),
  ]
}

# ==============================================================================
# Vaccination barplot
# ==============================================================================

build_age_group <- function(age) {
  dplyr::case_when(
    age < 5   ~ "0-4 år",
    age < 10  ~ "5-9 år",
    age < 12  ~ "10-11 år",
    age < 15  ~ "12-14 år",
    age < 20  ~ "15-19 år",
    age < 25  ~ "20-24 år",
    age < 30  ~ "25-29 år",
    age < 35  ~ "30-34 år",
    age < 40  ~ "35-39 år",
    age < 45  ~ "40-44 år",
    age < 50  ~ "45-49 år",
    age < 55  ~ "50-54 år",
    age < 60  ~ "55-59 år",
    age < 65  ~ "60-64 år",
    age < 70  ~ "65-69 år",
    age < 75  ~ "70-74 år",
    age < 80  ~ "75-79 år",
    age < 85  ~ "80-84 år",
    age < 90  ~ "85-89 år",
    age < 95  ~ "90-94 år",
    TRUE      ~ "95+ år"
  )
}

extract_vacc_class <- function(df, class_col, vacc_status) {
  out <- df %>%
    dplyr::select(Kommun, age, sex, dplyr::all_of(class_col))
  out$vacc_status <- vacc_status
  names(out)[names(out) == class_col] <- "population"
  out
}

apply_population_corrections <- function(df, corrections) {
  for (i in seq_len(nrow(corrections))) {
    idx <- df$Kommun == corrections$Kommun[i] &
      df$age_group == corrections$age_group[i] &
      df$sex == corrections$sex[i]
    
    df[idx, "population_total"] <- corrections$population_total[i]
  }
  df
}

build_period_table <- function(df, id_col, current_week_a, current_week_b) {
  selected <- df %>%
    dplyr::select(
      dplyr::all_of(id_col),
      dplyr::all_of(names(df)[c(
        grep(paste0("positivity_w", current_week_a), names(df)),
        grep(paste0("positivity_w", current_week_b), names(df)),
        grep(paste0("cases_per_capita5_w", current_week_a, "_allage"), names(df)),
        grep(paste0("cases_per_capita5_w", current_week_b, "_allage"), names(df)),
        grep(paste0("test_per_capita5_w", current_week_b, "_allage"), names(df)),
        grep(paste0("test_per_capita5_w", current_week_a, "_allage"), names(df))
      )])
    )
  
  selected$cases_2w_allage <- selected[, 4] + selected[, 5]
  selected$tests_2w_allage <- selected[, 6] + selected[, 7]
  selected$positivity_2w_allage <- selected[, 8] / selected[, 9]
  selected[, c(2:7)] <- NULL
  
  selected
}

format_period_table <- function(df) {
  df$positivity_2w_allage <- round(df$positivity_2w_allage, 3) * 100
  df$cases_2w_allage <- round(df$cases_2w_allage, 0)
  df$tests_2w_allage <- round(df$tests_2w_allage, 0)
  df
}

build_change_table <- function(current_df, previous_df, id_col, value_col, sort_desc = TRUE, digits_change = NULL) {
  out <- current_df[order(current_df[[value_col]], decreasing = sort_desc), c(id_col, value_col)]
  rownames(out) <- NULL
  
  old_col <- paste0(value_col, "_old")
  out <- plyr::join(
    out,
    previous_df[, c(id_col, old_col)],
    by = id_col,
    type = "left",
    match = "all"
  )
  
  out$Change <- out[[value_col]] - out[[old_col]]
  
  if (!is.null(digits_change)) {
    out$Change <- round(out$Change, digits_change)
  }
  
  out[, c(id_col, value_col, "Change")]
}

ffc.stata <- haven::read_dta(file = "data/for_mats_kommun.dta")
ffc.stata <- as.data.frame(ffc.stata)

vacc_all_vaccinated2 <- dplyr::bind_rows(
  extract_vacc_class(ffc.stata, "class3", "class3"),
  extract_vacc_class(ffc.stata, "class2", "class2"),
  extract_vacc_class(ffc.stata, "class1", "class1"),
  extract_vacc_class(ffc.stata, "class4", "class4"),
  extract_vacc_class(ffc.stata, "class5", "class5")
)

vacc_all_vaccinated_all_kommun <- aggregate(
  list(population = vacc_all_vaccinated2$population),
  by = list(
    Kommun = vacc_all_vaccinated2$Kommun,
    age = vacc_all_vaccinated2$age,
    sex = vacc_all_vaccinated2$sex
  ),
  sum,
  na.rm = TRUE,
  na.action = NULL
)
vacc_all_vaccinated_all_kommun$vacc_status <- "vacc_all"

vacc_all_vaccinated_all_lan <- aggregate(
  list(population = vacc_all_vaccinated2$population),
  by = list(
    age = vacc_all_vaccinated2$age,
    sex = vacc_all_vaccinated2$sex
  ),
  sum,
  na.rm = TRUE,
  na.action = NULL
)

vacc_all_vaccinated_all_lan$Kommun <- "Länet"
vacc_all_vaccinated_all_lan$vacc_status <- "vacc_all_lan"
vacc_all_vaccinated_all_lan <- vacc_all_vaccinated_all_lan[, c(4, 1:3, 5)]

vacc_all_vaccinated_all_all3 <- rbind(
  vacc_all_vaccinated_all_lan,
  vacc_all_vaccinated_all_kommun,
  vacc_all_vaccinated2
)

vacc_all_vaccinated_all_all3 <- subset(vacc_all_vaccinated_all_all3, age > 11)
vacc_all_vaccinated_all_all3$age_group <- build_age_group(vacc_all_vaccinated_all_all3$age)

vacc_all_vaccinated_all_all3_agge_age <- aggregate(
  list(population = vacc_all_vaccinated_all_all3$population),
  by = list(
    Kommun = vacc_all_vaccinated_all_all3$Kommun,
    age_group = vacc_all_vaccinated_all_all3$age_group,
    sex = vacc_all_vaccinated_all_all3$sex,
    vacc_status = vacc_all_vaccinated_all_all3$vacc_status
  ),
  sum,
  na.rm = TRUE,
  na.action = NULL
)

vacc_all_vaccinated_all_all3_for_lan_count <- subset(
  vacc_all_vaccinated_all_all3,
  vacc_status != "vacc_all"
)

vacc_all_vaccinated_all_all3_agge_age_lanet <- aggregate(
  list(population = vacc_all_vaccinated_all_all3_for_lan_count$population),
  by = list(
    age_group = vacc_all_vaccinated_all_all3_for_lan_count$age_group,
    sex = vacc_all_vaccinated_all_all3_for_lan_count$sex,
    vacc_status = vacc_all_vaccinated_all_all3_for_lan_count$vacc_status
  ),
  sum,
  na.rm = TRUE,
  na.action = NULL
)

vacc_all_vaccinated_all_all3_agge_age_lanet$Kommun <- "Länet"
vacc_all_vaccinated_all_all3_agge_age_lanet <- vacc_all_vaccinated_all_all3_agge_age_lanet[, c(5, 1:4)]

vacc_all_vaccinated_all_all3_agge_age <- rbind(
  vacc_all_vaccinated_all_all3_agge_age,
  vacc_all_vaccinated_all_all3_agge_age_lanet
)

vacc_all_vaccinated_all_all3_agge_age_lan <- subset(
  vacc_all_vaccinated_all_all3_agge_age,
  Kommun == "Länet" & vacc_status != "vacc_all"
)

vacc_all_vaccinated_all_all3_agge_age_kommun <- subset(
  vacc_all_vaccinated_all_all3_agge_age,
  Kommun != "Länet" & vacc_status != "vacc_all_lan"
)

scb_postal_new <- rio::import("data/age_year_kommun_20211026.xlsx")
scb_postal_new$age_group <- build_age_group(scb_postal_new$age)

pop_per_komm_age_sex_agg <- aggregate(
  list(population_total = scb_postal_new$population_total),
  by = list(
    Kommun = scb_postal_new$Kommun,
    age_group = scb_postal_new$age_group,
    sex = scb_postal_new$sex
  ),
  sum,
  na.rm = TRUE,
  na.action = NULL
)

pop_per_komm_age_sex_agg_LAN <- aggregate(
  list(population_total = pop_per_komm_age_sex_agg$population_total),
  by = list(
    age_group = pop_per_komm_age_sex_agg$age_group,
    sex = pop_per_komm_age_sex_agg$sex
  ),
  sum,
  na.rm = TRUE,
  na.action = NULL
)

pop_per_komm_age_sex_agg_LAN$Kommun <- "Länet"
pop_per_komm_age_sex_agg_LAN <- pop_per_komm_age_sex_agg_LAN[, c(4, 1:3)]

scb_age_group_sex_all <- rbind(pop_per_komm_age_sex_agg_LAN, pop_per_komm_age_sex_agg)
scb_age_group_sex_all$sex[scb_age_group_sex_all$sex == "Kvinnor"] <- "Kvinna"
scb_age_group_sex_all$sex[scb_age_group_sex_all$sex == "Män"] <- "Man"

togetehr_confus <- rbind(
  vacc_all_vaccinated_all_all3_agge_age_kommun,
  vacc_all_vaccinated_all_all3_agge_age_lan
)

all_together_vacc_and_scb <- plyr::join(
  togetehr_confus,
  scb_age_group_sex_all,
  by = c("Kommun", "age_group", "sex"),
  type = "left",
  match = "first"
)

all_together_vacc_and_scb_Lan_check <- subset(
  all_together_vacc_and_scb,
  Kommun == "Länet" & vacc_status != "vacc_all"
) %>%
  dplyr::distinct()

all_together_vacc_and_scb_Kommun_check <- subset(
  all_together_vacc_and_scb,
  Kommun != "Länet"
) %>%
  dplyr::distinct()

lan_population_corrections <- data.frame(
  Kommun = c("Länet", "Länet", "Länet", "Länet", "Länet", "Länet", "Länet"),
  age_group = c("80-84 år", "80-84 år", "85-89 år", "85-89 år", "90-94 år", "95+ år", "95+ år"),
  sex = c("Man", "Kvinna", "Man", "Kvinna", "Man", "Man", "Kvinna"),
  population_total = c(4691, 5239, 2335, 3366, 977, 248, 734),
  stringsAsFactors = FALSE
)

kommun_population_corrections <- data.frame(
  Kommun = c(
    "ENKÖPING", "ENKÖPING", "ENKÖPING", "ENKÖPING", "ENKÖPING",
    "HEBY",
    "HÅBO", "HÅBO", "HÅBO", "HÅBO", "HÅBO",
    "ÄLVKARLEBY", "ÄLVKARLEBY", "ÄLVKARLEBY",
    "KNIVSTA", "KNIVSTA", "KNIVSTA", "KNIVSTA", "KNIVSTA", "KNIVSTA", "KNIVSTA",
    "ÖSTHAMMAR", "ÖSTHAMMAR", "ÖSTHAMMAR", "ÖSTHAMMAR", "ÖSTHAMMAR", "ÖSTHAMMAR", "ÖSTHAMMAR",
    "TIERP", "TIERP", "TIERP", "TIERP", "TIERP",
    "UPPSALA", "UPPSALA", "UPPSALA", "UPPSALA", "UPPSALA", "UPPSALA", "UPPSALA", "UPPSALA"
  ),
  age_group = c(
    "75-79 år", "80-84 år", "85-89 år", "90-94 år", "90-94 år",
    "75-79 år",
    "80-84 år", "80-84 år", "85-89 år", "85-89 år", "90-94 år",
    "85-89 år", "70-74 år", "75-79 år",
    "90-94 år", "90-94 år", "95+ år", "85-89 år", "80-84 år", "95+ år", "95+ år",
    "95+ år", "95+ år", "90-94 år", "85-89 år", "85-89 år", "80-84 år", "80-84 år",
    "95+ år", "80-84 år", "85-89 år", "75-79 år", "75-79 år",
    "85-89 år", "80-84 år", "80-84 år", "85-89 år", "90-94 år", "90-94 år", "95+ år", "95+ år"
  ),
  sex = c(
    "Man", "Man", "Man", "Kvinna", "Man",
    "Kvinna",
    "Kvinna", "Man", "Kvinna", "Man", "Man",
    "Man", "Man", "Kvinna",
    "Kvinna", "Man", "Kvinna", "Kvinna", "Kvinna", "Man", "Man",
    "Man", "Kvinna", "Man", "Kvinna", "Man", "Kvinna", "Man",
    "Man", "Man", "Man", "Man", "Kvinna",
    "Man", "Man", "Kvinna", "Kvinna", "Man", "Kvinna", "Man", "Kvinna"
  ),
  population_total = c(
    1120, 664, 335, 253, 117,
    371,
    284, 295, 153, 93, 47,
    79, 314, 284,
    50, 36, 19, 88, 167, 8, 8,
    29, 66, 86, 294, 233, 417, 423,
    15, 332, 192, 605, 581,
    1198, 2434, 2882, 1864, 533, 992, 137, 446
  ),
  stringsAsFactors = FALSE
)

all_together_vacc_and_scb_Lan_check <- apply_population_corrections(
  all_together_vacc_and_scb_Lan_check,
  lan_population_corrections
)

all_together_vacc_and_scb_Kommun_check <- apply_population_corrections(
  all_together_vacc_and_scb_Kommun_check,
  kommun_population_corrections
)

all_together_vacc_and_scb_Lan_check_only_vaccTotal <- subset(
  all_together_vacc_and_scb_Lan_check,
  vacc_status == "vacc_all_lan"
)

all_together_vacc_and_scb_Kommun_check_only_vaccTotal <- subset(
  all_together_vacc_and_scb_Kommun_check,
  vacc_status == "vacc_all"
)

all_together_vacc_and_scb_Kommun_check_only_vaccTotal$unvaccinated <-
  all_together_vacc_and_scb_Kommun_check_only_vaccTotal$population_total -
  all_together_vacc_and_scb_Kommun_check_only_vaccTotal$population

all_together_vacc_and_scb_Lan_check_only_vaccTotal$unvaccinated <-
  all_together_vacc_and_scb_Lan_check_only_vaccTotal$population_total -
  all_together_vacc_and_scb_Lan_check_only_vaccTotal$population

all_together_vacc_and_scb_Lan_check_only_vaccTotal <- all_together_vacc_and_scb_Lan_check_only_vaccTotal %>%
  dplyr::select(Kommun, age_group, sex, population_total, unvaccinated)

all_together_vacc_and_scb_Lan_check_only_vaccTotal$vacc_status <- "unvaccinated"
names(all_together_vacc_and_scb_Lan_check_only_vaccTotal)[
  names(all_together_vacc_and_scb_Lan_check_only_vaccTotal) == "unvaccinated"
] <- "population"

all_together_vacc_and_scb_Lan_check_only_vaccTotal <- all_together_vacc_and_scb_Lan_check_only_vaccTotal[
  , c(1, 2, 3, 6, 4, 5)
]

all_together_vacc_and_scb_Kommun_check_only_vaccTotal <- all_together_vacc_and_scb_Kommun_check_only_vaccTotal %>%
  dplyr::select(Kommun, age_group, sex, population_total, unvaccinated)

all_together_vacc_and_scb_Kommun_check_only_vaccTotal$vacc_status <- "unvaccinated"
names(all_together_vacc_and_scb_Kommun_check_only_vaccTotal)[
  names(all_together_vacc_and_scb_Kommun_check_only_vaccTotal) == "unvaccinated"
] <- "population"

all_together_vacc_and_scb_Kommun_check_only_vaccTotal <- all_together_vacc_and_scb_Kommun_check_only_vaccTotal[
  , c(1, 2, 3, 6, 4, 5)
]

final_kommun_vacc_bar <- rbind(
  all_together_vacc_and_scb_Kommun_check,
  all_together_vacc_and_scb_Kommun_check_only_vaccTotal
)

final_Lan_vacc_bar <- rbind(
  all_together_vacc_and_scb_Lan_check,
  all_together_vacc_and_scb_Lan_check_only_vaccTotal
)

final_Lan_vacc_bar2 <- subset(final_Lan_vacc_bar, vacc_status != "vacc_all_lan")
final_kommun_vacc_bar2 <- subset(final_kommun_vacc_bar, vacc_status != "vacc_all")

final_Lan_vacc_bar2$age_group <- stringr::str_remove(final_Lan_vacc_bar2$age_group, " år")
final_kommun_vacc_bar2$age_group <- stringr::str_remove(final_kommun_vacc_bar2$age_group, " år")

levels_age_group_grouped <- c(
  "12-14", "15-19", "20-24", "25-29", "30-34", "35-39",
  "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
  "70-74", "75-79", "80-84", "85-89", "90-94", "95+"
)

final_kommun_vacc_bar2$age_group <- factor(
  final_kommun_vacc_bar2$age_group,
  ordered = TRUE,
  levels = levels_age_group_grouped
)

final_Lan_vacc_bar2$age_group <- factor(
  final_Lan_vacc_bar2$age_group,
  ordered = TRUE,
  levels = levels_age_group_grouped
)

vacc_levels <- c("unvaccinated", "class1", "class2", "class3", "class4", "class5")

final_Lan_vacc_bar2$vacc_status <- factor(
  final_Lan_vacc_bar2$vacc_status,
  ordered = TRUE,
  levels = vacc_levels
)

final_kommun_vacc_bar2$vacc_status <- factor(
  final_kommun_vacc_bar2$vacc_status,
  ordered = TRUE,
  levels = vacc_levels
)

final_vacc_cover <- rbind(final_Lan_vacc_bar2, final_kommun_vacc_bar2)

wrong_kommun_cov <- c(
  "Länet", "ENKÖPING", "HEBY", "HÅBO", "KNIVSTA",
  "TIERP", "UPPSALA", "ÄLVKARLEBY", "ÖSTHAMMAR"
)

correct_kommun_cov <- c(
  "Uppsala Län (323 111 inv.)",
  "Enköping (38 088 inv.)",
  "Heby (11 715 inv.)",
  "Håbo (17 940 inv.)",
  "Knivsta (14 334 inv.)",
  "Tierp (17 681 inv.)",
  "Uppsala (196 491 inv.)",
  "Älvkarleby (8 006 inv.)",
  "Östhammar (18 856 inv.)"
)

final_vacc_cover$Kommun[final_vacc_cover$Kommun %in% wrong_kommun_cov] <- correct_kommun_cov[
  match(final_vacc_cover$Kommun[final_vacc_cover$Kommun %in% wrong_kommun_cov], wrong_kommun_cov)
]

tierp_fix <- final_vacc_cover$Kommun == "Tierp (21 182 inv.)" &
  final_vacc_cover$age_group == "100+" &
  final_vacc_cover$sex == "Kvinna"

final_vacc_cover[tierp_fix & final_vacc_cover$vacc_status == "unvaccinated", "population"] <- 0
final_vacc_cover[tierp_fix, "population_total"] <- 4

final_vacc_cover$percent_coverage <- (final_vacc_cover$population * 100) / final_vacc_cover$population_total
final_vacc_cover <- final_vacc_cover[!duplicated(final_vacc_cover[1:4]), ]

final_vacc_cover_write_excel <- final_vacc_cover
colnames(final_vacc_cover_write_excel) <- c(
  "Kommun", "age_group", "sex", "vacc_status",
  "n_vacc_status", "population_total", "percent_coverage"
)

create_city_data_coverage <- function(cities) {
  final_vacc_cover[final_vacc_cover$Kommun == cities, ]
}

label_genders <- c(
  icon("syringe", lib = "font-awesome"),
  icon("book", lib = "font-awesome")
)

# ==============================================================================
# Table data
# ==============================================================================

sheets_uppsala_kom_table <- readxl::excel_sheets(files_ian)
uppsala_kom_table <- rio::import(files_ian, which = match("weekly_allage", sheets_uppsala_kom_table))

sheets_uppsala_stad_table <- readxl::excel_sheets(files_stad)
uppsala_stad_tabl <- rio::import(files_stad, which = match("weekly_allage", sheets_uppsala_stad_table))

uppsala_kom_table_selected <- build_period_table(
  uppsala_kom_table,
  id_col = "kommun",
  current_week_a = non_iso_week_current - 1,
  current_week_b = non_iso_week_current
)

uppsala_kom_table_selected_old <- build_period_table(
  uppsala_kom_table,
  id_col = "kommun",
  current_week_a = non_iso_week_current - 2,
  current_week_b = non_iso_week_current - 1
)

uppsala_stad_tabl_selected <- build_period_table(
  uppsala_stad_tabl,
  id_col = "stadsdel",
  current_week_a = non_iso_week_current - 1,
  current_week_b = non_iso_week_current
)

uppsala_stad_tabl_selected_old <- build_period_table(
  uppsala_stad_tabl,
  id_col = "stadsdel",
  current_week_a = non_iso_week_current - 2,
  current_week_b = non_iso_week_current - 1
)

uppsala_stad_tabl_selected <- format_period_table(uppsala_stad_tabl_selected)
uppsala_stad_tabl_selected_old <- format_period_table(uppsala_stad_tabl_selected_old)
uppsala_kom_table_selected <- format_period_table(uppsala_kom_table_selected)
uppsala_kom_table_selected_old <- format_period_table(uppsala_kom_table_selected_old)

colnames(uppsala_stad_tabl_selected) <- c("Stadsdel", "Fall", "Tester", "Positivitet")
colnames(uppsala_kom_table_selected) <- c("Kommun", "Fall", "Tester", "Positivitet")

colnames(uppsala_stad_tabl_selected_old) <- c("Stadsdel", "Fall_old", "Tester_old", "Positivitet_old")
colnames(uppsala_kom_table_selected_old) <- c("Kommun", "Fall_old", "Tester_old", "Positivitet_old")

# Cases tables
uppsala_stad_tabl_selected_cases <- build_change_table(
  uppsala_stad_tabl_selected,
  uppsala_stad_tabl_selected_old,
  id_col = "Stadsdel",
  value_col = "Fall"
)

uppsala_kommun_table_cases <- build_change_table(
  uppsala_kom_table_selected,
  uppsala_kom_table_selected_old,
  id_col = "Kommun",
  value_col = "Fall"
)

colnames(uppsala_kommun_table_cases) <- c("Kommun", "Fall*", "Förändring")
colnames(uppsala_stad_tabl_selected_cases) <- c("Stadsdel", "Fall*", "Förändring")

change_formatter <- formatter(
  "span",
  style = x ~ style(
    font.weight = "bold",
    color = ifelse(x > 0, "#ff7f7f", ifelse(x < 0, "#71CA97", "#ee7621"))
  ),
  x ~ icontext(
    ifelse(x > 0, "arrow-up", ifelse(x < 0, "arrow-down", "grip-lines")),
    x
  )
)

create_top_data_table_stad_cases <- function(top_10_20_50) {
  uppsala_stad_tabl_selected_cases[1:top_10_20_50, ]
}

colors_cases <- csscolor(
  gradient(as.numeric(uppsala_stad_tabl_selected_cases$`Fall*`), "white", "indianred1")
)

create_colors_cases <- function(top_10_20_50) {
  colors_cases[1:top_10_20_50]
}

colors_cases_kommun <- csscolor(
  gradient(as.numeric(uppsala_kommun_table_cases$`Fall*`), "white", "indianred1")
)

# Positivity tables
uppsala_stad_tabl_selected_pos <- build_change_table(
  uppsala_stad_tabl_selected,
  uppsala_stad_tabl_selected_old,
  id_col = "Stadsdel",
  value_col = "Positivitet",
  digits_change = 1
)

uppsala_kommun_table_pos <- build_change_table(
  uppsala_kom_table_selected,
  uppsala_kom_table_selected_old,
  id_col = "Kommun",
  value_col = "Positivitet",
  digits_change = 1
)

colnames(uppsala_kommun_table_pos) <- c("Kommun", "Positivitet", "Förändring")
colnames(uppsala_stad_tabl_selected_pos) <- c("Stadsdel", "Positivitet", "Förändring")

create_top_data_table_stad_pos <- function(top_10_20_50) {
  uppsala_stad_tabl_selected_pos[1:top_10_20_50, ]
}

colors_pos <- csscolor(
  gradient(as.numeric(uppsala_stad_tabl_selected_pos$Positivitet), "white", "lightsalmon1")
)

create_colors_pos <- function(top_10_20_50) {
  colors_pos[1:top_10_20_50]
}

colors_pos_kommun <- csscolor(
  gradient(as.numeric(uppsala_kommun_table_pos$Positivitet), "white", "lightsalmon1")
)

# Testing tables
uppsala_stad_tabl_selected_test <- build_change_table(
  uppsala_stad_tabl_selected,
  uppsala_stad_tabl_selected_old,
  id_col = "Stadsdel",
  value_col = "Tester"
)

uppsala_kommun_table_test <- build_change_table(
  uppsala_kom_table_selected,
  uppsala_kom_table_selected_old,
  id_col = "Kommun",
  value_col = "Tester"
)

colnames(uppsala_kommun_table_test) <- c("Kommun", "Tester*", "Förändring")
colnames(uppsala_stad_tabl_selected_test) <- c("Stadsdel", "Tester*", "Förändring")

create_top_data_table_stad_test <- function(top_10_20_50) {
  uppsala_stad_tabl_selected_test[1:top_10_20_50, ]
}

colors_test <- csscolor(
  gradient(as.numeric(uppsala_stad_tabl_selected_test$`Tester*`), "white", "mediumseagreen")
)

create_colors_test <- function(top_10_20_50) {
  colors_test[1:top_10_20_50]
}

colors_test_kommun <- csscolor(
  gradient(as.numeric(uppsala_kommun_table_test$`Tester*`), "white", "mediumseagreen")
)

# Kommun hospital table
uppsala_kommun_table_hosp_old <- rio::import("data/top_old.xlsx", which = "pat_per_capita3_kommun")
colnames(uppsala_kommun_table_hosp_old) <- c("kommun", "pat_per_capita3_old")

uppsala_kommun_table_hosp <- rio::import(
  files_notif_recent,
  which = match("pat_per_capita3_kommun", sheets_notif_recent)
)

uppsala_kommun_table_hosp$pat_per_capita3 <- round(uppsala_kommun_table_hosp$pat_per_capita3, 2)
uppsala_kommun_table_hosp <- subset(uppsala_kommun_table_hosp, !is.na(kommun))

uppsala_kommun_table_hosp <- plyr::join(
  uppsala_kommun_table_hosp,
  uppsala_kommun_table_hosp_old[, c("kommun", "pat_per_capita3_old")],
  by = "kommun",
  type = "left",
  match = "all"
)

uppsala_kommun_table_hosp$Change <- round(
  uppsala_kommun_table_hosp$pat_per_capita3 - uppsala_kommun_table_hosp$pat_per_capita3_old,
  2
)
uppsala_kommun_table_hosp[is.na(uppsala_kommun_table_hosp$Change), "Change"] <- 0

uppsala_kommun_table_hosp <- uppsala_kommun_table_hosp[, c("kommun", "pat_per_capita3", "Change")]
colnames(uppsala_kommun_table_hosp) <- c("Kommun", "Sjukhusvårdade", "Change")

uppsala_kommun_table_hosp <- uppsala_kommun_table_hosp[
  order(uppsala_kommun_table_hosp$Sjukhusvårdade, decreasing = TRUE),
  c("Kommun", "Sjukhusvårdade", "Change")
]
rownames(uppsala_kommun_table_hosp) <- NULL
colnames(uppsala_kommun_table_hosp) <- c("Kommun", "Sjukhusvårdade**", "Förändring")

colors_hosp_kommun <- csscolor(
  gradient(as.numeric(uppsala_kommun_table_hosp$`Sjukhusvårdade**`), "white", "steelblue2")
)

# Län aggregate table
notif_before_lan_sheets <- readxl::excel_sheets(files_ian)

uppsala_ian_table_old <- rio::import(files_ian, which = match("total_allage", notif_before_lan_sheets))
uppsala_ian_table_old <- uppsala_ian_table_old[c(
  grep(paste0("positivity_w", non_iso_week_current - 2), colnames(uppsala_ian_table_old)),
  grep(paste0("positivity_w", non_iso_week_current - 1), colnames(uppsala_ian_table_old)),
  grep(paste0("cases_per_capita5_w", non_iso_week_current - 1, "_allage"), colnames(uppsala_ian_table_old)),
  grep(paste0("cases_per_capita5_w", non_iso_week_current - 2, "_allage"), colnames(uppsala_ian_table_old)),
  grep(paste0("test_per_capita5_w", non_iso_week_current - 2, "_allage"), colnames(uppsala_ian_table_old)),
  grep(paste0("test_per_capita5_w", non_iso_week_current - 1, "_allage"), colnames(uppsala_ian_table_old))
)]

uppsala_ian_table_old$cases_2w_allage <- uppsala_ian_table_old[, 3] + uppsala_ian_table_old[, 4]
uppsala_ian_table_old$tests_2w_allage <- uppsala_ian_table_old[, 5] + uppsala_ian_table_old[, 6]
uppsala_ian_table_old$positivity_2w_allage <- uppsala_ian_table_old[, 7] / uppsala_ian_table_old[, 8]

uppsala_ian_table_old[, c(1:6)] <- NULL
uppsala_ian_table_old$kommun <- "LÄNET"
uppsala_ian_table_old$v1 <- NULL

uppsala_ian_table <- rio::import(files_notif_recent, which = match("average_allage", sheets_notif_recent))

uppsala_ian_table_final <- uppsala_ian_table
uppsala_ian_table_final_old <- uppsala_ian_table_old

uppsala_ian_table_final$positivity_allage <- round(uppsala_ian_table_final$positivity_allage, 3) * 100
uppsala_ian_table_final$cases_per_capita_allage <- round(uppsala_ian_table_final$cases_per_capita_allage, 0)
uppsala_ian_table_final$tests_per_capita_allage <- round(uppsala_ian_table_final$tests_per_capita_allage, 0)
uppsala_ian_table_final$Antalpatienter_2w <- 0

uppsala_ian_table_final <- uppsala_ian_table_final[, c(2, 1, 3, 4)]
rownames(uppsala_ian_table_final) <- NULL

colnames(uppsala_ian_table_final) <- c(
  "Fall*",
  "Positivitet (%)",
  "Tester*",
  "Sjukhusvårdade**"
)

uppsala_ian_table_final2 <- uppsala_ian_table_final
uppsala_ian_table_final2$Kommun <- "Ian"

uppsala_ian_table_final2_long <- reshape2::melt(
  data = uppsala_ian_table_final2,
  id.vars = c("Kommun"),
  variable.name = "Variabel",
  value.name = "Värde"
)

uppsala_ian_table_final2_long$Värde[1:3] <- round(uppsala_ian_table_final2_long$Värde[1:3], 0)
uppsala_ian_table_final2_long$Värde[4] <- round(uppsala_ian_table_final2_long$Värde[4], 3)

uppsala_ian_table_final2_long2 <- uppsala_ian_table_final2_long[, c(2, 3)]

uppsala_ian_table_final_old[, 1] <- round(uppsala_ian_table_final_old[, 1], 0)
uppsala_ian_table_final_old[, 2] <- round(uppsala_ian_table_final_old[, 2], 0)
uppsala_ian_table_final_old[, 3] <- round(uppsala_ian_table_final_old[, 3], 3) * 100
uppsala_ian_table_final_old$Antalpatienter_2w <- 0

uppsala_ian_table_final_old <- uppsala_ian_table_final_old[, c(1, 3, 2, 4)]
rownames(uppsala_ian_table_final_old) <- NULL

colnames(uppsala_ian_table_final_old) <- c(
  "Fall*",
  "Positivitet (%)",
  "Tester*",
  "Sjukhusvårdade**"
)

uppsala_ian_table_final2_old <- uppsala_ian_table_final_old
uppsala_ian_table_final2_old$Kommun <- "Ian"

uppsala_ian_table_final2_long_old <- reshape2::melt(
  data = uppsala_ian_table_final2_old,
  id.vars = c("Kommun"),
  variable.name = "Variabel",
  value.name = "Värde"
)

uppsala_ian_table_final2_long2_old <- uppsala_ian_table_final2_long_old[, c(2, 3)]
uppsala_ian_table_final2_long2_old$Värde <- as.numeric(uppsala_ian_table_final2_long2_old$Värde)

uppsala_ian_table_final2_long2$Change <-
  uppsala_ian_table_final2_long2$Värde -
  uppsala_ian_table_final2_long2_old$Värde

uppsala_ian_table_final2_long2$Change[1] <- round(uppsala_ian_table_final2_long2$Change[1], 0)
uppsala_ian_table_final2_long2$Change[2] <- round(uppsala_ian_table_final2_long2$Change[2], 1)
uppsala_ian_table_final2_long2$Change[3] <- round(uppsala_ian_table_final2_long2$Change[3], 0)

colnames(uppsala_ian_table_final2_long2) <- c("Variabel", "Värde", "Förändring")
uppsala_ian_table_final2_long2 <- uppsala_ian_table_final2_long2[c(2, 3), ]

uppsala_LAN_table_hosp_old <- rio::import("data/top_old.xlsx", which = "average_hosp")
colnames(uppsala_LAN_table_hosp_old) <- c("average_hosp_old")

uppsala_LAN_table_hosp <- rio::import(
  files_notif_recent,
  which = match("average_hosp", sheets_notif_recent)
)

uppsala_LAN_table_hosp$Antalpatienter_2w <- round(uppsala_LAN_table_hosp$Antalpatienter_2w, 2)
uppsala_LAN_table_hosp <- cbind(uppsala_LAN_table_hosp, uppsala_LAN_table_hosp_old)

uppsala_LAN_table_hosp$Change <- round(
  uppsala_LAN_table_hosp$Antalpatienter_2w - uppsala_LAN_table_hosp$average_hosp_old,
  2
)

uppsala_LAN_table_hosp <- uppsala_LAN_table_hosp[, c(1, 3)]
uppsala_LAN_table_hosp$Variabel <- "Sjukhusvårdade**"
uppsala_LAN_table_hosp <- uppsala_LAN_table_hosp[, c(3, 1, 2)]
colnames(uppsala_LAN_table_hosp) <- c("Variabel", "Värde", "Förändring")

uppsala_ian_table_final2_long2$Värde <- as.integer(uppsala_ian_table_final2_long2$Värde)
rownames(uppsala_ian_table_final2_long2) <- c("", " ")

# ***************************************      SHINY APP structure    *************************************** #
# ********************************************************************************************************** #

# Header image for top left:
headerImage <- normalizePath(file.path(paste0('images/', 'crush_logo_new', '.png'))) 

sessionInfo()
# ***************************************      UI    *************************************** #
# ****************************************************************************************** #
# ==============================================================================
# UI helpers
# ============================================================================== 

section_title <- function(icon_name, text, size = "180%") {
  h1(icon(icon_name), strong(text), style = paste0("font-size:", size, ";"))
}

app_screenshot_button <- function(id = NULL, filename, label) {
  screenshotButton(id = id, filename = filename, label = label)
}

app_loading_overlay <- function(id = "loadmessage", text_color = "#001f3f") {
  tagList(
    tags$head(
      tags$style(
        type = "text/css",
        sprintf(
          "
          #%s {
            position: fixed;
            top: 50%%;
            left: 50%%;
            opacity: 0.50;
            text-align: center;
            font-weight: bold;
            font-size: 200%%;
            color: %s;
            z-index: 105;
            animation: blinker 1s linear infinite;
          }
          ",
          id, text_color
        )
      )
    ),
    conditionalPanel(
      condition = "$('html').hasClass('shiny-busy')",
      tags$div("Loading...", id = id),
      tags$script(HTML(sprintf(
        "
        (function blink() {
          $('#%s').fadeOut(500).fadeIn(500, blink);
        })();
        ",
        id
      )))
    )
  )
}

navy_box <- function(...,
                     width = 12,
                     title = NULL,
                     closable = FALSE,
                     collapsible = FALSE,
                     solidHeader = FALSE,
                     style = "font-size: 130%;") {
  shinydashboard::box(
    ...,
    width = width,
    title = title,
    closable = closable,
    collapsible = collapsible,
    solidHeader = solidHeader,
    status = "primary",
    background = "navy",
    style = style
  )
}

standard_city_select <- function(input_id, label, choices, width = 320) {
  selectInput(
    inputId = input_id,
    label = tags$p(label, style = "font-size: 130%;"),
    width = width,
    choices = choices
  )
}

slider_tick_css <- function() {
  tags$style(
    type = "text/css",
    ".irs-grid-text:nth-child(-2n+18) {color: #a10000}",
    ".irs-grid-text:nth-child(2n+20) {color: #a10000}",
    ".irs-grid-pol:nth-of-type(-n+18) {background: darkred}",
    ".irs-grid-pol:nth-of-type(n+18) {background: darkred}",
    ".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {",
    "  background: darkred;",
    "  border-top: 1px solid darkred;",
    "  border-bottom: 1px solid darkred;",
    "}"
  )
}

# ==============================================================================
# Global UI assets
# ============================================================================== 

app_head <- tags$head(
  tags$link(rel = "icon", type = "image/png", href = "flavicon_crush.png"),
  tags$title("CRUSH Covid"),
  tags$style(HTML(
    "
    .logo {
      background-color: #41003c !important;
    }

    .navbar {
      background-color: #41003c !important;
    }

    .box.box-solid.box-primary > .box-header {
      color: #fff;
      background: #41003c;
    }

    .box.box-solid.box-primary {
      border-bottom-color: #41003c;
      border-left-color: #41003c;
      border-right-color: #41003c;
      border-top-color: #41003c;
    }

    .skin-blue .main-sidebar {
      background-color: #001f3f;
      font-size: 15px;
    }

    .content-wrapper {
      background-color: #fbebeb;
    }

    .skin-blue .main-sidebar .sidebar .sidebar-menu .active a,
    .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover {
      background-color: #41003c;
    }

    .skin-blue .sidebar-menu > li.active > a,
    .skin-blue .sidebar-menu > li:hover > a {
      border-left-color: #ff285a;
    }

    .skin-blue .sidebar-menu li a {
      font-size: 14px;
    }

    .collapsed_text {
      color: black;
    }

    .selectize-input,
    .selectize-dropdown {
      font-size: 20px;
      line-height: 20px;
    }

    .irs-grid-text {
      font-size: 15pt;
    }

    .nav-tabs {
      background-color: #41003c;
    }

    .nav-tabs > li > a {
      color: #fbebeb !important;
      font-size: 15px !important;
      font-family: sans-serif !important;
      font-weight: bold !important;
    }

    .nav-tabs > li.active > a {
      color: #fbebeb !important;
      font-size: 17px !important;
      font-weight: bold !important;
      background-color: #f25278 !important;
      border: medium none;
      border-radius: 0;
    }

    .nav-tabs > li.active > a:hover {
      background-color: #FC94AF !important;
    }

    .navbar-default .navbar-brand {
      color: #ff285a;
    }

    .tabs-above > .nav > li[class=active] > a {
      background-color: #ff285a;
      color: #ff285a;
    }

    .nav-tabs > li.active > a:focus {
      background-color: #5b2a51;
    }

    .navbar-default {
      background-color: #ff285a;
      border-color: #ff285a;
    }

    .nav-tabs-custom .nav-tabs li.active {
      border-top-color: #FC94AF;
    }

    .leaflet-control-layers-expanded {
      font-size: 17px !important;
    }

    .tab-panel {
      background-color: #ff285a;
      color: #ff285a;
    }

    .nav-tabs-custom > .nav-tabs > li.header {
      line-height: 35px;
      padding: 0 10px;
      font-size: 20px;
      color: #fbebeb;
      font-weight: bold;
    }

    @keyframes glowing {
      0%   { text-shadow: 0 0 0px #FFFFFF; }
      50%  { text-shadow: 0 0 10px #f2f9fa; }
      100% { text-shadow: 0 0 0px #FFFFFF; }
    }

    .glow {
      animation: glowing 1.1s infinite;
    }
    "
  ))
)

analytics_head <- HTML(
  '
  <script async src="https://www.googletagmanager.com/gtag/js?id=UA-186422029-1"></script>
  <script>
    window.dataLayer = window.dataLayer || [];
    function gtag(){dataLayer.push(arguments);}
    gtag("js", new Date());
    gtag("config", "UA-186422029-1");
  </script>
  '
)

cookie_head <- HTML(
  '
  <link rel="stylesheet" type="text/css" href="https://cdn.wpcc.io/lib/1.0.2/cookieconsent.min.css"/>
  <script src="https://cdn.wpcc.io/lib/1.0.2/cookieconsent.min.js" defer></script>
  <script>
    window.addEventListener("load", function() {
      window.wpcc.init({
        "colors": {
          "popup": {
            "background": "#222222",
            "text": "#ffffff",
            "border": "#e6b3b3"
          },
          "button": {
            "background": "#e6b3b3",
            "text": "#000000"
          }
        },
        "padding": "small",
        "margin": "small",
        "transparency": "25",
        "content": {
          "message": "CRUSH Covid använder kakor (cookies) för att webbplatsen ska fungera bra för dig.&nbsp;Genom att använda vår webbplats godkänner du cookies.",
          "button": "OK",
          "href": "https://www.uu.se/om-webbplatsen/",
          "link": "Läs mer om kakor."
        },
        "corners": "large",
        "fontsize": "small",
        "position": "bottom-right"
      });
    });
  </script>
  '
)

meta_head <- tagList(
  tags$meta(name = "viewport", content = "width=1600"),
  uiOutput("body"),
  tags$meta(property = "og:image", content = "crush_url.png"),
  tags$meta(property = "og:image:width", content = "100"),
  tags$meta(property = "og:image:alt", content = "CRUSH Covid"),
  tags$meta(property = "og:image", content = "https://crush-covid.shinyapps.io/crush_covid/")
)

sidebar_contents <- shinydashboard::sidebarMenu(
  id = "crush_sidebar",
  shinydashboard::menuItem("Välkommen", tabName = "welcome", icon = icon("door-open")),
  shinydashboard::menuItem("Veckorapport", tabName = "week_report", icon = icon("file-alt")),
  shinydashboard::menuItem("Topplistor", tabName = "table_data", icon = icon("table")),
  shinydashboard::menuItem("Vaccinationkartor", tabName = "vacc_traffic", icon = icon("shield-virus")),
  shinydashboard::menuItem("Vaccinationstäckning", tabName = "vacc_barplot", icon = icon("chart-bar")),
  shinydashboard::menuItem("Trender", tabName = "trends", icon = icon("chart-line")),
  shinydashboard::menuItem("Smittspårning", tabName = "contact_trace", icon = icon("project-diagram")),
  shinydashboard::menuItem("Avlopp", tabName = "wastewater", icon = icon("faucet")),
  shinydashboard::menuItem("1177 och 112", tabName = "calls", icon = icon("phone-volume")),
  shinydashboard::menuItem("Rörelsemönster", tabName = "mobility", icon = icon("walking")),
  shinydashboard::menuItem("Om CRUSH COVID", tabName = "omCrush", icon = icon("info-circle")),
  imageOutput("crush_logo")
)

welcome_tab <- tabItem(
  tabName = "welcome",
  h2(""),
  bootstrapPage(
    fluidRow(
      column(
        width = 12,
        navy_box(
          title = span(icon("door-open"), "Välkommen", style = "font-size: 130%;"),
          fluidRow(column(width = 12, htmlOutput("welcome_1")), column(width = 2, align = "center"))
        ),
        fluidRow(
          navy_box(
            width = 5,
            title = span(icon("info-circle"), "Länkar och mer information", style = "font-size: 130%;"),
            fluidRow(column(width = 12, htmlOutput("welcome_2")), column(width = 2, align = "center"))
          ),
          navy_box(
            width = 5,
            title = span(icon("info-circle"), "Undantag och detaljer", style = "font-size: 130%;"),
            fluidRow(column(width = 12, htmlOutput("welcome_3")), column(width = 2, align = "center"))
          )
        )
      )
    ),
    app_loading_overlay(id = "loadmessage_welcome"),
    fluidRow(
      width = 12,
      align = "center",
      id = "speedometer_box_shot",
      tabBox(
        width = 11,
        side = "right",
        id = "speedometer_box",
        selected = span(icon("head-side-cough"), "Symtomatisk covid-19", style = "font-size: 120%;"),
        title = "Viktiga indikatorer för de senaste veckorna",
        tabPanel(
          span(icon("flask"), "Tester", style = "font-size: 120%;"),
          tabBox(
            width = 12,
            side = "left",
            selected = p("Tester", current_week_speed),
            tabPanel(
              p("Tester", current_week_speed),
              p(
                "Antalet covid-19-tester per 100 000 invånare (hela befolkningen) för",
                current_week_speed,
                ".",
                style = "font-size: 120%;"
              ),
              htmlOutput("speedometer_test", align = "center")
            ),
            tabPanel(
              p("Tester", past_week_speed),
              p(
                "Antalet covid-19-tester per 100 000 invånare (hela befolkningen) för",
                past_week_speed,
                ".",
                style = "font-size: 120%;"
              ),
              htmlOutput("speedometer_test_before", align = "center")
            ),
            tabPanel(
              "Procentuell förändring",
              p("Procentuell förändring"),
              htmlOutput("speedometer_w_perc_diff_speed_plot_test", align = "center")
            )
          )
        ),
        tabPanel(
          span(icon("plus-circle"), "Testpositivitet", style = "font-size: 120%;"),
          tabBox(
            width = 12,
            side = "left",
            selected = p("Testpositivitet (%)", current_week_speed),
            tabPanel(
              p("Testpositivitet (%)", current_week_speed),
              p(
                "Andelen positiva covid-19 test av alla tester för",
                current_week_speed,
                ".",
                style = "font-size: 120%;"
              ),
              htmlOutput("speedometer_pos", align = "center")
            ),
            tabPanel(
              p("Testpositivitet (%)", past_week_speed),
              p(
                "Andelen positiva covid-19 test av alla tester för",
                past_week_speed,
                ".",
                style = "font-size: 120%;"
              ),
              htmlOutput("speedometer_pos_before", align = "center")
            ),
            tabPanel(
              "Procentuell förändring",
              p("Procentuell förändring"),
              htmlOutput("speedometer_w_perc_diff_speed_plot_pos", align = "center")
            )
          )
        ),
        tabPanel(
          span(icon("head-side-cough"), "Symtomatisk covid-19", style = "font-size: 120%;"),
          tabBox(
            width = 12,
            side = "left",
            selected = p("% Uppskattad förekomst", current_week_speed),
            tabPanel(
              title = p("% Uppskattad förekomst", current_week_speed),
              p(
                "% Uppskattad förekomst av symtomatisk covid-19 för",
                current_week_speed,
                "för Länet, exkl Uppsala (2-siffriga postnummerområden 74x xx) och Uppsala (2-siffriga postnummerområden 75x xx). Data för den uppskattade förekomsten kan hittas här ",
                tags$a(href = "https://csss-resultat.shinyapps.io/csss_dashboard/", "COVID Symptom Study i Sverige"),
                style = "font-size: 120%;"
              ),
              htmlOutput("speedometer_csss", align = "center", width = 100)
            ),
            tabPanel(
              title = p("% Uppskattad förekomst", past_week_speed),
              p(
                "% Uppskattad förekomst av symtomatisk covid-19 för",
                past_week_speed,
                "för Länet, exkl Uppsala (2-siffriga postnummerområden 74x xx) och Uppsala (2-siffriga postnummerområden 75x xx). Data för den uppskattade förekomsten kan hittas här ",
                tags$a(href = "https://csss-resultat.shinyapps.io/csss_dashboard/", "COVID Symptom Study i Sverige"),
                style = "font-size: 120%;"
              ),
              htmlOutput("speedometer_csss_before", align = "center", width = 300)
            ),
            tabPanel(
              "Procentuell förändring",
              htmlOutput("speedometer_w_perc_diff_speed_plot_csss", align = "center", width = 300)
            )
          )
        )
      )
    ),
    fluidRow(
      style = "margin:3%",
      column(
        width = 12,
        imageOutput("shiny_UA_UU", height = "auto", width = "auto"),
        imageOutput("shiny_crush", height = "auto", width = "auto"),
        style = "display: block; margin-left: auto; margin-right: auto; margin-bottom: auto;",
        align = "center"
      )
    )
  ),
  app_screenshot_button(
    id = "speedometer_box_shot",
    filename = paste("CRUSH_covid_välkommen_", current_week_speed),
    label = "Take a screenshot of the speedometer"
  )
)

week_report_tab <- tabItem(
  tabName = "week_report",
  h2(""),
  section_title("digital-tachograph", paste("Veckorapport för vecka", current_week_veckarapport), size = "200%"),
  section_title("newspaper", "Nyheter"),
  fluidRow(column(width = 10, htmlOutput("summary_new"), style = "font-size: 130%;"), column(width = 2, align = "center")),
  section_title("copy", "Sammanfattning"),
  fluidRow(column(width = 10, htmlOutput("summary1"), style = "font-size: 130%;"), column(width = 2, align = "center")),
  section_title("signal", "Testoberoende indikatorer"),
  fluidRow(column(width = 10, htmlOutput("summary2"), style = "font-size: 130%;"), column(width = 2, align = "center")),
  section_title("pager", "Sjukhusinläggningar"),
  fluidRow(column(width = 10, htmlOutput("summary_kompletterande"), style = "font-size: 130%;"), column(width = 2, align = "center")),
  section_title("syringe", "Vaccination"),
  fluidRow(column(width = 10, htmlOutput("summary_vacc"), style = "font-size: 130%;"), column(width = 2, align = "center")),
  section_title("pager", "Bekräftade fall"),
  fluidRow(column(width = 10, htmlOutput("summary_bekr"), style = "font-size: 130%;"), column(width = 2, align = "center")),
  app_screenshot_button(
    filename = paste("CRUSH_covid_veckorapport_", current_week_speed),
    label = "Take a screenshot of the entire tab"
  )
)

table_data_tab <- tabItem(
  tabName = "table_data",
  h2(""),
  width = 12,
  section_title(
    "digital-tachograph",
    paste0("Topplistor de senaste 2 veckorna (avser vecka ", current_week_numeric - 1, " - ", current_week_numeric, ")"),
    size = "200%"
  ),
  section_title("city", "Uppsala Län", size = "160%"),
  fluidRow(
    shinydashboard::box(width = 2, formattableOutput("table_formattable_ian")),
    shinydashboard::box(width = 2, formattableOutput("table_formattable_ian_hosp"))
  ),
  p("*per 100 000 invånare (hela befolkningen) de senaste 2 veckorna", style = "font-size:100%;"),
  p("**vårdade covid-19/dag/1,000 senaste 2 veckorna", style = "font-size:100%;"),
  section_title("building", "Kommun"),
  fluidRow(
    shinydashboard::box(width = 3, formattableOutput("table_formattable_pos_kommun")),
    shinydashboard::box(width = 3, formattableOutput("table_formattable_test_kommun")),
    shinydashboard::box(width = 3, formattableOutput("table_formattable_hosp_kommun"))
  ),
  p("*per 100 000 invånare (hela befolkningen) de senaste 2 veckorna", style = "font-size:100%;"),
  p("**vårdade covid-19/dag/1,000 senaste 2 veckorna", style = "font-size:100%;"),
  app_screenshot_button(
    filename = paste("CRUSH_covid_topplistor_", current_week_speed),
    label = "Take a screenshot of the entire tab"
  )
)

trends_tab <- tabItem(
  tabName = "trends",
  h2(""),
  section_title("info-circle", "Förklaring"),
  p(htmlOutput("trend_text", style = "font-size: 130%;")),
  br(),
  fluidRow(
    id = "fluidRow_both_panels",
    tabBox(
      width = 6,
      side = "right",
      title = "Nya fall & testpositivitet",
      id = "tabs_trends",
      selected = "Uppsala Län",
      tabPanel(
        "Kommun",
        fluidRow(
          navy_box(
            width = 12,
            standard_city_select(
              input_id = "v_kommun_trend_selector",
              label = "Välj en kommun:",
              choices = uppsala_kom_new$area,
              width = NULL
            ),
            plotOutput("trendsKommun", height = "45vw")
          )
        )
      ),
      tabPanel(
        "Uppsala Län",
        style = "background-color: #001f3f",
        fluidRow(
          navy_box(width = 12, solidHeader = TRUE, plotOutput("trends", height = "45vw"))
        )
      )
    ),
    tabBox(
      width = 6,
      side = "right",
      title = "Tester",
      selected = "Uppsala Län",
      id = "tabs_trends_tester",
      tabPanel(
        "Kommun",
        fluidRow(
          navy_box(
            width = 12,
            solidHeader = TRUE,
            standard_city_select(
              input_id = "v_kommun_trend_selector_testing",
              label = "Välj en kommun:",
              choices = uppsala_kom_new$area,
              width = NULL
            ),
            plotOutput("trendsKommun_tester", height = "45vw")
          )
        )
      ),
      tabPanel(
        "Uppsala Län",
        style = "background-color: #001f3f",
        fluidRow(
          navy_box(width = 12, solidHeader = TRUE, plotOutput("trends_tester", height = "45vw"))
        )
      )
    )
  ),
  app_screenshot_button(
    id = "fluidRow_both_panels",
    filename = paste("CRUSH_covid_trender_", current_week_speed),
    label = "Take a screenshot of both plots side by side"
  )
)

contact_trace_tab <- tabItem(
  tabName = "contact_trace",
  app_loading_overlay(id = "loadmessage_contact"),
  h2(""),
  section_title("people-arrows", "Information"),
  fluidRow(column(width = 12, htmlOutput("contact_text"), style = "font-size: 130%;"), column(width = 2, align = "center")),
  br(),
  fluidRow(
    tabBox(
      width = 12,
      side = "left",
      title = NULL,
      id = "tabs_contacts",
      tabPanel(
        "Tid från insjuknande till provtagning",
        tags$p("Tid från första symptom till genomförd smittspårning.", style = "font-size: 130%;"),
        tabBox(
          width = 12,
          side = "left",
          title = NULL,
          id = "tabs_contacts_delay",
          tabPanel(
            "Figur",
            fluidRow(
              width = "480xmax",
              column(width = 12, imageOutput("contact_6", height = "auto", width = "auto"), align = "center")
            )
          )
        )
      ),
      tabPanel(
        "Smittspårning",
        tags$p(
          "Var personer (viktad för frekvens) som testats positivt tror att de har blivit smittade respektive var de tror att de har utsatt andra för smitta.",
          style = "font-size: 130%;"
        ),
        tabBox(
          width = 12,
          side = "left",
          title = NULL,
          id = "tabs_contacts_tracing",
          tabPanel(
            "Figur 1",
            fluidRow(
              width = "480xmax",
              column(
                width = 12,
                imageOutput("contact_4", height = "auto", width = "auto"),
                imageOutput("contact_4ii", height = "auto", width = "auto"),
                align = "center"
              )
            )
          )
        )
      )
    )
  ),
  app_screenshot_button(
    id = "tabs_contacts",
    filename = paste("CRUSH_covid_smittspårning_", current_week_speed),
    label = "Take a screenshot of the graph"
  )
)

vacc_barplot_tab <- tabItem(
  tabName = "vacc_barplot",
  slider_tick_css(),
  h2(""),
  fluidRow(
    tabBox(
      width = 12,
      side = "left",
      title = NULL,
      id = "tabs_vacc_coverage",
      selected = span(icon("chart-bar"), "Vaccinstatus per ålder"),
      tabPanel(
        span(icon("chart-bar"), "Vaccinstatus per ålder"),
        fluidRow(
          width = "480xmax",
          tags$p(
            "Vaccinstatus för personer per ålder och kön redovisat i antal. Observera att personer som vaccinerat sig i en annan region än i Region Uppsala (för närvarande ca",
            number_vacc_outside_Upps,
            "personer) ingår inte i denna statistik, se information på folkhälsomyndigheten ",
            tags$a(
              href = "https://www.folkhalsomyndigheten.se/folkhalsorapportering-statistik/statistikdatabaser-och-visualisering/vaccinationsstatistik/statistik-for-vaccination-mot-covid-19/uppfoljning-av-vaccination/statistik-om-lanens-nettofloden/",
              "här."
            ),
            style = "font-size: 130%;"
          ),
          shinydashboard::box(
            width = 10,
            id = "box_coverage",
            column(
              width = 6,
              standard_city_select(
                input_id = "v_city_selector_coverage",
                label = "Välj en kommun:",
                choices = c(
                  "Uppsala Län (323 111 inv.)", "Enköping (38 088 inv.)", "Heby (11 715 inv.)",
                  "Håbo (17 940 inv.)", "Knivsta (14 334 inv.)", "Tierp (17 681 inv.)",
                  "Uppsala (196 491 inv.)", "Älvkarleby (8 006 inv.)", "Östhammar (18 856 inv.)"
                )
              )
            ),
            materialSwitch(
              inputId = "switch_cov_per",
              label = p("Procent ", icon("percent")),
              value = FALSE,
              status = "primary"
            ),
            plotOutput("vacc_bar_lan", height = "45vw")
          )
        ),
        fluidRow(
          app_screenshot_button(
            id = "box_coverage",
            filename = paste("CRUSH_covid_vaccinationstäckning_", current_week_speed),
            label = "Take a screenshot of the graph"
          )
        )
      ),
      tabPanel(
        span(icon("lungs-virus"), "Fördelning av covid positiva provsvar"),
        fluidRow(
          width = "480xmax",
          tags$p(
            "Fördelning av covid positiva provsvar mellan ovaccinerade, full vaccination (minst två veckor efter minst två vaccindoser) och fullvaccinerad vårdpersonal. Fullvaccinerad vårdpersonal som använt egenoprov eller beställt provtagning som allmänhet i 1177 förekommer i det turkosa fältet.",
            style = "font-size: 130%;"
          ),
          imageOutput("vacc_coverage_area_plot", height = "auto", width = "auto"),
          imageOutput("vacc_coverage_area_plot2", height = "auto", width = "auto")
        )
      )
    )
  )
)

vacc_traffic_tab <- tabItem(
  tabName = "vacc_traffic",
  slider_tick_css(),
  h2(""),
  section_title("info-circle", "Förklaringar"),
  htmlOutput("vaccine_traffic_definitions", style = "font-size: 130%;"),
  br(),
  column(
    width = 6,
    navy_box(
      width = 12,
      enable_label = TRUE,
      solidHeader = FALSE,
      radioGroupButtons(
        inputId = "labels_second_dose_traffic",
        label = tags$p("Välj:", style = "font-size: 130%;"),
        choices = c("Dos 2 för minst två veckor sedan", "Dos 3 för minst en vecka sedan"),
        individual = TRUE,
        checkIcon = list(
          yes = tags$i(class = "fa fa-syringe", style = "color: steelblue"),
          no = tags$i(class = "fa fa-syringe", style = "color: steelblue")
        )
      ),
      radioGroupButtons(
        inputId = "v_agegroup_vacc",
        label = tags$p("Åldersgrupp:", style = "font-size: 130%;"),
        selected = "15plus",
        choices = c("15plus", "20-29", "30-39", "40-49", "50-59", "60-69", "70+"),
        individual = TRUE,
        checkIcon = list(
          yes = tags$i(class = "fa fa-circle", style = "color: steelblue"),
          no = tags$i(class = "fa fa-circle-o", style = "color: steelblue")
        )
      ),
      shinyWidgets::sliderTextInput(
        "v_week_vacc",
        tags$p("Välj vecka eller tryck på play. Zooma ut och dra kartan för att hitta din bostadsort.", style = "font-size: 130%;"),
        choices = vax_ecdc_all,
        selected = vax_ecdc_one,
        force_edges = TRUE,
        dragRange = TRUE,
        grid = TRUE,
        animate = animationOptions(
          interval = 2000,
          loop = TRUE,
          playButton = icon("play", "fa-2x"),
          pauseButton = icon("pause", "fa-2x")
        )
      )
    )
  ),
  column(
    width = 6,
    imageOutput("vaccine_ECDC_legend", height = "auto", width = "auto"),
    materialSwitch(
      inputId = "ecdc_vaccine_palette",
      label = tags$p("Färgblinda:", style = "font-size: 130%;"),
      status = "danger"
    )
  ),
  fluidRow(width = 12, leafletOutput("vacc_traffic_map", height = "750px")),
  app_screenshot_button(
    id = "vacc_traffic_map",
    filename = paste("CRUSH_covid_vaccinationkartor_", current_week_speed),
    label = "Take a screenshot of the current map"
  )
)

traffic_tab <- tabItem(
  tabName = "traffic",
  tags$style(
    type = "text/css",
    ".irs-grid-text:nth-child(-2n+18) {color: #fbebeb}",
    ".irs-grid-text:nth-child(2n+20) {color: #fbebeb}",
    ".irs-grid-pol:nth-of-type(-n+18) {background: darkred}",
    ".irs-grid-pol:nth-of-type(n+18) {background: darkred}",
    ".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar { background: darkred; border-top: 1px solid darkred; border-bottom: 1px solid darkred; }"
  ),
  h2(""),
  section_title("traffic-light", "Förklaringar"),
  fluidRow(column(width = 12, htmlOutput("ECDC_definitions", style = "font-size: 130%;")), column(width = 2, align = "center")),
  br(),
  fluidRow(
    box(width = 4, align = "center", myDownloadButton("downloadData_ecdc"), myDownloadButton_description("downloadData_ecdc_description"))
  ),
  br(),
  fluidRow(
    column(
      width = 5,
      navy_box(
        width = 12,
        title = span(icon("globe-europe"), "ECDC kartor", style = "font-size: 130%;"),
        solidHeader = FALSE,
        enable_label = TRUE,
        p(
          icon("exclamation-circle", class = "glow"),
          "Kartan uppdaterad i enighet med ",
          tags$a(href = "https://eur-lex.europa.eu/legal-content/EN/TXT/PDF/?uri=CELEX:32021H0961", "nya rekommendationer."),
          style = "font-size: 130%;"
        ),
        p(
          "Kartan produceras i enighet med ",
          tags$a(href = "https://ec.europa.eu/info/live-work-travel-eu/coronavirus-response/travel-during-coronavirus-pandemic/common-approach-travel-measures-eu_en", "Europeiska Kommissionens"),
          " överenskomna färgmarkering baserat på kombinationen av nya fall, testpositivitet och antalet tester.",
          style = "font-size: 130%;"
        ),
        shinyWidgets::sliderTextInput(
          "v_week",
          tags$p("Välj vecka eller tryck på play. Zooma ut och dra kartan för att hitta din bostadsort.", style = "font-size: 130%;"),
          choices = ecdc_all,
          selected = ecdc_all_one,
          force_edges = TRUE,
          grid = TRUE,
          animate = animationOptions(
            interval = 2000,
            loop = TRUE,
            playButton = icon("play", "fa-2x"),
            pauseButton = icon("pause", "fa-2x")
          )
        )
      )
    ),
    column(width = 4, imageOutput("ECDC_legend", height = "auto", width = "auto")),
    column(
      width = 2,
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
      materialSwitch(inputId = "ecdc_palette", label = tags$p("Färgblinda:", style = "font-size: 130%;"), status = "danger")
    )
  ),
  leafletOutput("mapCity", height = "750px"),
  app_screenshot_button(
    id = "mapCity",
    filename = paste("CRUSH_covid_ecdckartor_", current_week_speed),
    label = "Take a screenshot of the map"
  )
)

wastewater_tab <- tabItem(
  tabName = "wastewater",
  app_loading_overlay(id = "loadmessage_wastewater", text_color = "#000000"),
  h2(""),
  section_title("tint", "Om avloppsmätningar"),
  fluidRow(
    style = "margin-top:1%; margin-bottom:1%",
    column(width = 12, htmlOutput("water_text", style = "font-size: 130%;")),
    column(width = 6, align = "center")
  ),
  checkboxGroupButtons(
    inputId = "v_city_selector_waste",
    label = "Välj en kommun för att framhäva dess data (Uppsala är förvalt)",
    choices = c(
      "Enköping (46 326 inv.)", "Knivsta (18 447 inv.)", "Tierp (21 182 inv.)",
      "Uppsala (234 935 inv.)", "Älvkarleby (9 627 inv.)", "Östhammar (22 246 inv.)"
    ),
    individual = TRUE,
    selected = "Uppsala (234 935 inv.)",
    checkIcon = list(
      yes = tags$i(class = "fa fa-circle", style = "color: steelblue"),
      no = tags$i(class = "fa fa-circle-o", style = "color: steelblue")
    )
  ),
  prettyRadioButtons(
    inputId = "zoom_wastewater",
    label = "Välj:",
    choices = c(
      "Zooma ut – visa mig alla veckor",
      "Zooma in – visa mig de senaste veckorna!"
    ),
    icon = icon("search"),
    animation = "tada"
  ),
  fluidRow(
    width = "620xmax",
    p(icon("exclamation-circle", class = "glow"), "Tomma områden på grafen indikerar avsaknad av data", style = "font-size: 110%;"),
    plotOutput("sewage_trend_simple_tabs", height = "35vw", width = "50vw"),
    align = "center"
  ),
  br(),
  fluidRow(
    width = "12",
    imageOutput("image_avlopp_omicron_upp", height = "auto", width = "auto"),
    align = "center"
  ),
  br(),
  fluidRow(
    column(
      width = 12,
      offset = 3,
      navy_box(
        width = 6,
        title = span(icon("hands-helping"), "Samarbetspartners", style = "font-size: 130%;"),
        column(width = 12, imageOutput("logo_vatten", height = "auto", width = "auto"), p(tags$a(href = "https://www.uppsalavatten.se/", "Mer om Uppsala Vatten", style = "font-size: 120%;")), br(), align = "center"),
        column(width = 12, imageOutput("logo_roslagsvatten", height = "auto", width = "auto"), p(tags$a(href = "https://roslagsvatten.se/", "Mer om Roslagsvatten", style = "font-size: 120%;")), br(), align = "center"),
        column(width = 12, imageOutput("logo_osth", height = "auto", width = "auto"), p(tags$a(href = "https://www.gastrikevatten.se/", "Mer om Gästrike Vatten", style = "font-size: 120%;")), br(), align = "center"),
        column(width = 12, imageOutput("logo_enk", height = "auto", width = "auto"), p(tags$a(href = "https://enkoping.se/", "Mer om Enköpings Kommun", style = "font-size: 120%;")), br(), align = "center"),
        column(width = 12, imageOutput("logo_tierp", height = "auto", width = "auto"), p(tags$a(href = "https://temab.tierp.se/", "Mer om TEMAB", style = "font-size: 120%;")), br(), align = "center"),
        column(width = 12, imageOutput("logo_slu", height = "auto", width = "auto"), p(tags$a(href = "https://www.slu.se/", "Mer om SLU", style = "font-size: 120%;")), align = "center"),
        column(width = 12, imageOutput("logo_scilife", height = "auto", width = "auto"), p(tags$a(href = "https://www.scilifelab.se/", "Mer om SciLifeLab", style = "font-size: 120%;")), br(), align = "center")
      )
    )
  )
)

heatmap_tab <- tabItem(
  tabName = "heatmap",
  h2(""),
  section_title("chess-board", "Positivitet och testning över åldersgrupper"),
  p(paste("Testpositivitet, testfrekvens och incidens per åldersgrupp och vecka i Uppsala Län", lower_week_heatmap, "(2021) -", current_week_speed, "(2022)."), style = "font-size: 130%;"),
  p("Ingen cirkel: nödvändig information saknas.", style = "font-size: 130%;"),
  p("Nummer ovanpå bubblor: veckovis incidens.", style = "font-size: 130%;"),
  p(icon("exclamation-circle", class = "glow"), "Baserat på befolkningsmängd 31 januari 2021.", style = "font-size: 130%;"),
  br(),
  fluidRow(
    style = "background-color: #fbebeb",
    width = 7,
    column(
      id = "heatmap_box",
      width = 9,
      tabBox(
        width = "480xmax",
        side = "left",
        title = NULL,
        id = "tabs_heatmap",
        selected = "Uppsala Län",
        tabPanel("Uppsala Län", style = "background-color: #fbebeb", fluidRow(width = "420xmax", plotOutput("heatmap_Kommun_lan", height = "30vw", width = "45vw"), align = "center")),
        tabPanel("Enköping", style = "background-color: #fbebeb", fluidRow(width = "420xmax", plotOutput("heatmap_Kommun_enk", height = "30vw", width = "45vw"), align = "center")),
        tabPanel("Håbo", style = "background-color: #fbebeb", fluidRow(width = "400xmax", plotOutput("heatmap_Kommun_hab", height = "30vw", width = "45vw"), align = "center")),
        tabPanel("Heby", style = "background-color: #fbebeb", fluidRow(width = "480xmax", plotOutput("heatmap_Kommun_heb", height = "30vw", width = "45vw"), align = "center")),
        tabPanel("Knivsta", style = "background-color: #fbebeb", fluidRow(width = "480xmax", plotOutput("heatmap_Kommun_kni", height = "30vw", width = "45vw"), align = "center")),
        tabPanel("Tierp", style = "background-color: #fbebeb", fluidRow(width = "480xmax", plotOutput("heatmap_Kommun_tie", height = "30vw", width = "45vw"), align = "center")),
        tabPanel("Uppsala", style = "background-color: #fbebeb", fluidRow(width = "480xmax", plotOutput("heatmap_Kommun_upp", height = "30vw", width = "45vw"), align = "center")),
        tabPanel("Älvkarleby", style = "background-color: #fbebeb", fluidRow(width = "480xmax", plotOutput("heatmap_Kommun_alv", height = "30vw", width = "45vw"), align = "center")),
        tabPanel("Östhammar", style = "background-color: #fbebeb", fluidRow(width = "480xmax", plotOutput("heatmap_Kommun_ost", height = "30vw", width = "45vw"), align = "center"))
      )
    )
  ),
  app_screenshot_button(
    id = "heatmap_box",
    filename = paste("CRUSH_covid_åldersfördelning_", current_week_speed),
    label = "Take a screenshot of the heatmap"
  )
)

calls_tab <- tabItem(
  tabName = "calls",
  app_loading_overlay(id = "loadmessage_calls"),
  h2(""),
  section_title("headset", "Utryckningar och samtal för symtom överensstämmande med covid-19"),
  p("Här presenterar vi trendgrafer med antal utryckningar (källa Sjukvårdens Larmcentral (SvLc)/ambulanssjukvården) per 100 000 invånare där ambulanspersonalen bedömer patienten som misstänkt covid-19 och 1177-samtal för symtom överensstämmande med covid-19.", style = "font-size: 130%;"),
  br(),
  p(icon("chart-line"), "Rosa linje: Antal ambulansutryckningar som bedömts som misstänkt covid-19 av ambulanspersonalen, per 100 000 invånare och vecka.", style = "font-size: 130%;"),
  p(icon("chart-bar"), "Blå stapeldiagram och den högra lodräta axeln: Medelantal samtal för symtom relaterade till covid-19 till 1177 per 100 000 invånare och vecka.", style = "font-size: 130%;"),
  br(),
  p(icon("exclamation-circle", class = "glow"), "Populationen inom parentes inkluderar endast individer 15 år och äldre.", style = "font-size: 130%;"),
  p(icon("exclamation-circle", class = "glow"), "Baserat på befolkningsmängd 31 mars 2021.", style = "font-size: 130%;"),
  br(),
  fluidRow(
    shinydashboard::box(
      width = 7,
      id = "calls_plot_box",
      standard_city_select(
        input_id = "v_city_selector_calls",
        label = "Välj en kommun:",
        choices = c(
          "Uppsala Län (323 111 inv.)", "Enköping (38 088 inv.)", "Heby (11 715 inv.)",
          "Håbo (17 940 inv.)", "Knivsta (14 334 inv.)", "Tierp (17 681 inv.)",
          "Uppsala (196 491 inv.)", "Älvkarleby (8 006 inv.)", "Östhammar (18 856 inv.)"
        )
      ),
      plotOutput("calls_combined", height = "35vw")
    )
  ),
  app_screenshot_button(
    id = "calls_plot_box",
    filename = paste("CRUSH_covid_112_1177_", current_week_speed),
    label = "Take a screenshot of plot"
  )
)

mobility_tab <- tabItem(
  tabName = "mobility",
  app_loading_overlay(id = "loadmessage_mobility"),
  h2(""),
  section_title("walking", "Förändringar i rörelsemönster i samhället"),
  p("Här presenterar vi det 7-dagars glidande medelvärdet (GM) av den procentuella förändringen i rörelsemönster över tid, efter geografisk plats och för olika kategorier, till exempel detaljhandel, rekreationsområden, arbetsplatser och bostadsområden.", style = "font-size: 130%;"),
  br(),
  p("Uppgifterna visar hur besökare (eller tillbringad tid) på kategoriserade platser förändras jämfört med baslinjedagar. En baslinjedag representerar ett normalt värde för den veckodagen. Baslinjedagarna ändras inte utan är medianvärdet från femveckorsperioden 3 januari–6 februari 2020. Det finns alltså inga säsongsvariationer i baslinjedagarna. Kategorin Bostadsområden visar en förändring i varaktighet. De andra kategorierna mäter en förändring av det totala antalet besökare.", style = "font-size: 130%;"),
  br(),
  p(
    icon("exclamation-circle", class = "glow"),
    "Du kan se uppgiftsluckor för vissa kategorier i din region. Dessa luckor är avsiktliga och uppstår när uppgifterna inte uppfyller kvalitets- och sekretessgränsen – när det inte finns tillräckligt med uppgifter för att ",
    tags$a(href = "https://www.youtube.com/watch?v=FfAdemDkLsc&ab_channel=Google", "garantera anonymitet."),
    " Du kan läsa mer om hur Google Mobility beräknar dessa trender och skyddar integriteten ",
    tags$a(href = "https://www.google.com/covid19/mobility/", "här"),
    " och ",
    tags$a(href = "https://support.google.com/covid19-mobility#topic=9822927", "här."),
    style = "font-size: 130%;"
  ),
  br(),
  p(icon("exclamation-circle", class = "glow"), "Befolkningsantalet mätarna bygger på hela befolkningen.", style = "font-size: 130%;"),
  fluidRow(
    shinydashboard::box(
      width = 9,
      id = "mobility_box",
      standard_city_select(
        input_id = "v_city_selector_mobility",
        label = "Välj en kommun:",
        choices = c(
          "Uppsala Län (388 803 inv.)", "Enköping (46 326 inv.)", "Heby (13 982 inv.)",
          "Håbo (22 058 inv.)", "Knivsta (18 447 inv.)", "Tierp (21 182 inv.)",
          "Uppsala (234 935 inv.)", "Älvkarleby (9 627 inv.)", "Östhammar (22 246 inv.)"
        )
      ),
      plotOutput("mobility_plot", height = "35vw")
    )
  ),
  app_screenshot_button(
    id = "mobility_box",
    filename = paste("CRUSH_covid_rörelsemönster_", current_week_speed),
    label = "Take a screenshot of the plot"
  )
)

om_crush_tab <- tabItem(
  tabName = "omCrush",
  h2(""),
  fluidRow(
    column(
      width = 6,
      navy_box(
        width = 12,
        title = span(icon("info-circle"), "Om CRUSH COVID", style = "font-size: 130%;"),
        solidHeader = TRUE,
        fluidRow(column(width = 12, htmlOutput("omCrushEnd")), column(width = 2, align = "center"))
      ),
      navy_box(
        width = 12,
        title = span(icon("users"), "Projektledare", style = "font-size: 130%;"),
        fluidRow(column(width = 12, htmlOutput("leader")), column(width = 2, align = "center"))
      ),
      navy_box(
        width = 12,
        title = span(icon("info-circle"), "Om CRUSH COVID webbplats", style = "font-size: 130%;"),
        solidHeader = TRUE,
        "För tekniska frågor och feedback ",
        a(
          actionButton(inputId = "email1", label = "Mejla oss", icon = icon("paper-plane", lib = "font-awesome")),
          href = "mailto:contact@example.org"
        )
      )
    ),
    column(
      width = 6,
      navy_box(
        width = 12,
        title = span(icon("clipboard-check"), "Etiska tillstånd", style = "font-size: 130%;"),
        solidHeader = TRUE,
        fluidRow(column(width = 12, htmlOutput("ethical")), column(width = 2, align = "center"))
      ),
      navy_box(
        width = 12,
        title = span(icon("hands-helping"), "Samarbetspartners", style = "font-size: 130%;"),
        column(width = 12, br(), imageOutput("vinnova", height = "auto", width = "auto"), p(tags$a(href = "https://www.vinnova.se/", "Mer om Vinnova", style = "font-size: 120%;")), br(), align = "center"),
        column(width = 12, imageOutput("scilife_lab", height = "auto", width = "auto"), p(tags$a(href = "https://www.scilifelab.se/", "Mer om SciLifeLab (Science for Life Laboratory)", style = "font-size: 120%;")), align = "center")
      )
    )
  )
)

ui <- dashboardPage(
  title = app_head,
  shinydashboard::dashboardHeader(
    title = img(
      src = "https://i.ibb.co/V3mr2nB/crush-logo-new.jpg",
      height = 40,
      width = 200,
      align = "left",
      style = "margin-top: 1%;"
    ),
    dropdownMenu(
      type = "notifications",
      badgeStatus = "danger",
      headerText = span(
        icon("users"),
        "Riktlinjer för rekommenderad testning ändrades den första november. Därför är perioden före respektive efter vecka 44 inte jämförbara ",
        style = "font-size: 120%;"
      )
    )
  ),
  shinydashboard::dashboardSidebar(
    collapsed = FALSE,
    width = "210px",
    sidebarMenuOutput("Semi_collapsible_sidebar"),
    tags$script("$(document).on('click', '.sidebar-toggle', function () { Shiny.onInputChange('SideBar_col_react', Math.random())});"),
    tags$script("$(document).on('click', '.treeview.active', function () { $(this).removeClass('active'); $(this).find('ul').removeClass('menu-open'); $(this).find('ul').css('display', 'none'); });"),
    sidebar_contents
  ),
  shinydashboard::dashboardBody(
    tags$head(analytics_head),
    tags$head(cookie_head),
    tags$head(meta_head),
    shinydashboard::tabItems(
      welcome_tab,
      table_data_tab,
      week_report_tab,
      trends_tab,
      contact_trace_tab,
      vacc_barplot_tab,
      vacc_traffic_tab,
      traffic_tab,
      wastewater_tab,
      heatmap_tab,
      calls_tab,
      mobility_tab,
      om_crush_tab
    )
  )
)


# ***********************************      SERVER    *************************************** #
# ****************************************************************************************** #



server <- function(input, output, session) {
  
  observeEvent(passwordON_Off,
               if(passwordON_Off == 'ON'){
                 shinyalert("CRUSH Covid",
                            "Skriv lösenordet"
                            , type = "input"
                            , inputType = "password"
                            , time = 0
                            # , inputValue = "Enter Password"
                            , callbackR = password_accept,
                            size = "s"
                 )
                 # # }
                 # shinyalert('Välkommen')
               } else {
               } )
  
  observeEvent(input$crush_sidebar , {
    if(input$crush_sidebar == 'analysis'){
      shinyalert(title = "MoVIN sommaruppehåll",
                 text='Senast MoVIN data uppdaterad: 2021-05-30',
                 type = "warning",
                 closeOnClickOutside = TRUE,
                 animation = TRUE,
                 confirmButtonCol = '#41003c')
    }
  })
  
  shinyalert(
    title = "", # Sommaruppehåll
    text =  "CRUSH Covid kommer inte rapportera till denna hemsida från vecka 25 till vecka 32 Projektet kommer dock även under denna tid följa utvecklingen noggrant och återuppta rapporteringen om smittspridningen skulle tillta under denna period. Senast data uppdaterad: 2022-06-16",
    size = "s",   #  "xs" for extra small, "s" for small (default), "m" for medium, or "l" for larg
    closeOnEsc = TRUE,
    closeOnClickOutside = FALSE,
    html = FALSE,
    type = "success",  # "warning", "error", "success"
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = '#41003c',#"#AEDEF4",
    timer = 0,  #  Use 0 to not close the modal automatically (default).
    imageUrl = 'https://covid19dataportal.se/img/logos/crush_covid_logo.png',  #"https://i.ibb.co/pvDdXML/crush-logo2.png",
    imageHeight= '50',
    imageWidth= '200',
    animation = TRUE
  )
  
  weeklyData <- reactive(create_week_data(input$v_week))
  
  weeklyData_vacc_ecdc <- reactive(create_week_data_vacc_ecdc(input$v_week_vacc,
                                                              input$v_agegroup_vacc))
  
  cityData <- reactive(create_city_data(input$v_city_selector_kartor))
  
  stadData <- reactive(create_stad_trend_data(input$v_stad_trend_selector))
  
  kommunData <- reactive(create_kommun_trend_data(input$v_kommun_trend_selector))
  
  kommunData_tester <- reactive(create_kommun_trend_data(input$v_kommun_trend_selector_testing))
  
  stadData_tester <- reactive(create_stad_trend_data(input$v_stad_trend_selector_testing))
  
  callsData_all_kommun <- reactive(create_calls_data_all_kommun(input$v_city_selector_calls))
  
  callsData_final_plotting <- reactive(create_calls_data_final_plotting(input$v_city_selector_calls))
  
  mobility_filter <- reactive(create_mobility_data(input$v_city_selector_mobility))
  
  workspace_suitcase <- reactive(create_workspace_suitcase(input$v_city_selector_mobility))
  
  shopping_cart <- reactive(create_shopping_cart(input$v_city_selector_mobility))
  
  resident_house <- reactive(create_resident_house(input$v_city_selector_mobility))
  
  stadData_map_weight <- reactive(create_stad_trend_data_map_weight(min(input$connection_range),
                                                                    max(input$connection_range),
                                                                    input$v_stad_trend_selector_contact))
  
  stadData_map_weight_actual <- reactive(create_stad_trend_data_map_weight_actual(min(input$weight_range),
                                                                                  max(input$weight_range),
                                                                                  input$v_stad_trend_selector_contact_actual))
  
  waste_kommun_choice <- reactive(
    waste_kommun_choice <- wastewater_All_bind22_old_new_weeks2 %>%
      filter(Pump %in% kommun_names_waste_labeller(input$v_city_selector_waste)) #%>%
  )
  
  # interactive for table tops
  
  table_tops_reactive_cases <- reactive(create_top_data_table_stad_cases(input$table_tops))
  table_tops_reactive_test <- reactive(create_top_data_table_stad_test(input$table_tops))
  table_tops_reactive_pos <- reactive(create_top_data_table_stad_pos(input$table_tops))
  
  colors_cases_reactive <- reactive(create_colors_cases(input$table_tops))
  colors_pos_reactive <- reactive(create_colors_pos(input$table_tops))
  colors_test_reactive <- reactive(create_colors_test(input$table_tops))
  
  cityData_coverage <- reactive(create_city_data_coverage(input$v_city_selector_coverage))  # final_Lan_vacc_bar2
  
  zoom_waste_x_Axis <-  function(x)
    
  {
    
    if(input$zoom_wastewater=="Zooma in – visa mig de senaste veckorna!")
      
    {
      c((most_recent_monday-84),most_recent_monday+7)  #        c((most_recent_monday-77),most_recent_monday)
      
    }
    
    else
    {
      print(NULL)
    }
    
  }
  
  zoom_waste_y_Axis <-  function(x)
    
  {
    
    if(input$zoom_wastewater=="Zooma in – visa mig de senaste veckorna!")
      
    {
      
      limits= c(0,86)
      
    }
    
    else
    {
      limits= c(0,100)
    }
    
  }
  
  # "Enköping (46 326 inv.)", v_city_selector_waste
  missing_label_enk <-  function(x)
    
  {
    
    if(input$v_city_selector_waste=="Enköping (46 326 inv.)")
      
    {
      
      0.4# 0.4
      
    }
    
    else
    {
      0
    }
    
  }
  
  missing_label_upps <-  function(x)
  {
    
    if(input$v_city_selector_waste=="Uppsala (234 935 inv.)")
      
    {
      
      0.4# 0.4
      
    }
    
    else
    {
      0
    }
    
  }
  
  missing_label_knivsta <-  function(x)
    
  {
    
    if(input$v_city_selector_waste=="Knivsta (18 447 inv.)")
      
    {
      
      0.4# 0.4
      
    }
    
    else
    {
      0
    }
    
  }
  
  missing_label_alv <-  function(x)
    
  {
    
    if(input$v_city_selector_waste=="Älvkarleby (9 627 inv.)")
      
    {
      
      0.4# 0.4
      
    }
    
    else
    {
      0
    }
    
  }
  
  labels_switch_plotly <- function(x)
    
  {
    
    if(input$labels_ON== TRUE)
      
    {
      
      geom_text(
        data = labels_plotly,#labels_plotly,
        aes(x = lng, y = lat),
        label=labels_plotly$stadsdel_point,
        nudge_x = -0.02, nudge_y = 0.01,
        check_overlap = T, size=2
      )
    }
    
    else
    {
      print(NULL)
      # as.numeric(0)# data=NULL # as.numeric(0)
    }
    
  }
  
  labels_switch_plotly_switch <- function(x)
    
  {
    
    if(input$labels_ON_actual== TRUE)
    {
      
      geom_text(
        data = labels_plotly,#labels_plotly,
        aes(x = lng, y = lat),
        label=labels_plotly$stadsdel_point,
        nudge_x = -0.02, nudge_y = 0.01,
        check_overlap = T, size=2
      )
    }
    
    else
    {
      print(NULL)
      # as.numeric(0)# data=NULL # as.numeric(0)
    }
    
  }
  
  # 'type', 'visible', 'showlegend', 'legendgroup', 'opacity', 'name', 'uid', 'ids', 'customdata', 'meta', 'selectedpoints', 'hoverinfo', 'hoverlabel', 'stream', 'transforms', 'uirevision', 'x', 'x0', 'dx', 'y', 'y0', 'dy', 'stackgroup', 'orientation', 'groupnorm', 'stackgaps', 'text', 'texttemplate', 'hovertext', 'mode', 'hoveron', 'hovertemplate', 'line', 'connectgaps', 'cliponaxis', 'fill', 'fillcolor', 'marker', 'selected', 'unselected', 'textposition', 'textfont', 'r', 't', 'error_x', 'error_y', 'xcalendar', 'ycalendar', 'xaxis', 'yaxis', 'idssrc', 'customdatasrc', 'metasrc', 'hoverinfosrc', 'xsrc', 'ysrc', 'textsrc', 'texttemplatesrc', 'hovertextsrc', 'hovertemplatesrc', 'textpositionsrc', 'rsrc', 'tsrc', 'key', 'set', 'frame', 'transforms', '_isNestedKey', '_isSimpleKey', '_isGraticule', '_bbox'
  
  switch_coverage_percentage <-  #cityData_coverage()$population
    function(x)
    {
      if(input$switch_cov_per== "FALSE")   #"Antal")
      {
        cityData_coverage()$population # final_vacc_cover$population  #
      }
      else
      {
        cityData_coverage()$percent_coverage #final_vacc_cover$percent_coverage # cityData_coverage()$percent_coverage
      }
    }
  
  switch_coverage_percentage_yaxis <-  #cityData_coverage()$population
    function(x)
    {
      if(input$switch_cov_per== "FALSE")   #"Antal")
      {
        'Antal' # final_vacc_cover$population  #
      }
      else
      {
        'Procent (%)' #final_vacc_cover$percent_coverage # cityData_coverage()$percent_coverage
      }
    }
  
  labels_switch_second_dose_traffic <-
    function(x)
    {
      if(input$labels_second_dose_traffic=="Dos 2 för minst två veckor sedan")
      {
        
        # create_week_data_vacc_ecdc(as.character(max(shape_file_vacc$week2)),'15plus')$vacc_color_dose2
        weeklyData_vacc_ecdc()$vacc_color_dose2
      }
      else
      {
        # create_week_data_vacc_ecdc(as.character(max(shape_file_vacc$week2)),'15plus')$vacc_color_dose1
        weeklyData_vacc_ecdc()$vacc_color_dose3
      }
    }
  
  labels_switch_second_dose_traffic_observe <-
    function(x)
    {
      if(input$labels_second_dose_traffic=="Dos 2 för minst två veckor sedan")
      {
        
        weeklyData_vacc_ecdc()$vacc_color_dose2
        
      }
      else
      {
        weeklyData_vacc_ecdc()$vacc_color_dose3
        
      }
    }
  
  labels_switch_second_dose_traffic_bubbles1 <-
    function(x)
    {
      if(input$labels_second_dose_traffic=="Dos 2 för minst två veckor sedan")
      {
        
        "<br>Dos 2 för minst två veckor sedan (%): "
        
      }
      else
      {
        "<br>Dos 3 för minst en vecka sedan (%): "
        
      }
    }
  
  labels_switch_second_dose_traffic_bubbles2 <-
    function(x)
    {
      if(input$labels_second_dose_traffic=="Dos 2 för minst två veckor sedan")
      {
        
        # create_week_data_vacc_ecdc(as.character(max(shape_file_vacc$week2)),'15plus')$cum_freq_dose2
        weeklyData_vacc_ecdc()$cum_freq_dose2
      }
      else
      {
        # create_week_data_vacc_ecdc(as.character(max(shape_file_vacc$week2)),'15plus')$cum_freq_dose1
        weeklyData_vacc_ecdc()$cum_freq_dose3
      }
    }
  
  labels_switch_second_dose_traffic_bubbles_observe1 <-
    function(x)
    {
      if(input$labels_second_dose_traffic=="Dos 2 för minst två veckor sedan")
      {
        
        "<br>Dos 2 för minst två veckor sedan(%): "
        
      }
      else
      {
        "<br>Dos 3 för minst en vecka sedan (%): "
        
      }
    }
  
  labels_switch_second_dose_traffic_bubbles_observe2 <-
    function(x)
    {
      if(input$labels_second_dose_traffic=="Dos 2 för minst två veckor sedan")
      {
        
        weeklyData_vacc_ecdc()$cum_freq_dose2
        
      }
      else
      {
        weeklyData_vacc_ecdc()$cum_freq_dose3
        
      }
    }
  
  # ### ----------------- ********* barplots on water plot
  check_virus_copies <- function(x)
    
  {
    
    if('virus_copies' %in% x)
    {
      wastewater_pumps2_multiplot$RR_N1copesPPMoV
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  # missing values on water plot
  
  missing_water <- function(x)
    
  {
    
    if('virus_copies' %in% x)
    {
      print('Mätning saknas')#  print('')   #      print('Mätning saknas')
    }
    
    else
    {
      print('')
    }
    
  }
  
  # labels for sewage barplot
  
  label_water <- function(x)
    
  {
    
    if('virus_copies' %in% x)
    {
      print(NULL)
    }
    
    else
    {
      print(FALSE)
    }
    
  }
  
  sewage_color_axis <- function(x)
    
  {
    
    if('virus_copies' %in% x)
      
    {
      print("#b199f7")
    }
    
    else
    {
      print("#fbebeb")
    }
    
  }
  
  # ### ----------------- ********* notification on water plot
  check_notif <- function(x)
    
  {
    
    if('notification' %in% x)
    {
      uppsala_all_weeks_multi_Kommun$notification
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  label_color_lintype <- function(x)
    
  {
    
    if('notification' %in% x)
      
    {
      print(NULL)
    }
    
    else if('positivity' %in% x)
      
    {
      print(NULL)
    }
    
    else if('Uppskattning' %in% x)
      
    {
      print(NULL)
    }
    
    else if('tester' %in% x)
      
    {
      print(NULL)
    }
    
    else if('hospital' %in% x)
      
    {
      print(NULL)   }
    
    else if('calls112' %in% x)
      
    {
      print(NULL)   }
    
    else if('mobility' %in% x)
      
    {
      print(NULL)
    }
    
    else
    {
      print(FALSE)
    }
    
  }
  
  notific_color_axis <- function(x)
    
  {
    
    if('notification' %in% x)
      
    {
      print("#ca4905")
    }
    
    else
    {
      print("#fbebeb")
    }
    
  }
  
  # ### ----------------- ********* positivity on water plot
  check_pos <- function(x)
    
  {
    
    if('positivity' %in% x)
      
    {
      uppsala_all_weeks_multi_Kommun$positivity
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  # to make the positivity indicators disappear and appear
  
  check_pos_58 <- function(x)
    
  {
    
    if('positivity' %in% x)
    {
      print(as.numeric(current_week+1.7))
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  check_pos_58.5 <- function(x)
    
  {
    
    if('positivity' %in% x)
    {
      print(as.numeric(current_week+2.2))  # current_week+(1/2))
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  check_pos_57.7 <- function(x)
    
  {
    
    if('positivity' %in% x)
    {
      print(as.numeric(current_week+0.9))
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  # ### ----------------- ********* CSSS
  check_Uppskattning<- function(x)
    
  {
    
    if('Uppskattning' %in% x)
    {
      csss_75_average_w$Uppskattning
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  # check_csss_57.7
  check_csss_57.7 <- function(x)
    
  {
    
    if('Uppskattning' %in% x)
    {
      print(as.numeric(35.8))
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  check_csss_58 <- function(x)
    
  {
    
    if('Uppskattning' %in% x)
    {
      print(as.numeric(34.6))
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  check_csss_58.5 <- function(x)
    
  {
    
    if('Uppskattning' %in% x)
    {
      print(as.numeric(34.3))
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  # ### ----------------- ********* testing on water plot
  check_test <- function(x)
    
  {
    
    if('tester' %in% x)
      
    {
      uppsala_all_weeks_multi_Kommun$testing
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  # to make the positivity indicators disappear and appear
  
  check_test_58 <- function(x)
    
  {
    
    if('tester' %in% x)
    {
      print(as.numeric(current_week+1))
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  check_test_58.5 <- function(x)
    
  {
    
    if('tester' %in% x)
    {
      print(as.numeric(current_week+1.2))  # current_week+(1/2))
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  check_test_57.7 <- function(x)
    
  {
    
    if('tester' %in% x)
    {
      print(as.numeric(current_week))
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  # ### ----------------- ********* calls112 on water plot
  check_calls112 <- function(x)
    
  {
    
    if('calls112' %in% x)
      
    {
      all_kommun_lan2_upp$adjusted_freq
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  # to make the positivity indicators disappear and appear
  
  check_calls112_58 <- function(x)
    
  {
    
    if('calls112' %in% x)
    {
      print(as.numeric(34.4))
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  # # if(grep('virus_copies',(x)))
  # print(as.numeric(0))
  
  check_calls112_57.7 <- function(x)
    
  {
    
    if('calls112' %in% x)
    {
      print(as.numeric(34.9))
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  # ### ----------------- ********* calls1177 on water plot
  check_calls1177 <- function(x)
    
  {
    
    if('calls1177' %in% x)
      
    {
      final_for_plotting2_upp$adjusted_freq_100k
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  # to make the positivity indicators disappear and appear
  
  check_calls1177_58 <- function(x)
    
  {
    
    if('calls1177' %in% x)
    {
      print(as.numeric(35.1))
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  check_calls1177_58.5 <- function(x)
    
  {
    
    if('calls1177' %in% x)
    {
      print(as.numeric(34.6))  # current_week+(1/2))
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  check_calls1177_57.7 <- function(x)
    
  {
    
    if('calls1177' %in% x)
    {
      print(as.numeric(35.9))
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  # ### ----------------- ********* hospitalizations on water plot
  check_hospit <- function(x)
    
  {
    
    if('hospital' %in% x)
      
    {
      uppsala_multi_hospit_Kommun$pat_per_1k_adult
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  # to make the hospititivity indicators disappear and appear
  
  check_hospit_58 <- function(x)
    
  {
    
    if('hospital' %in% x)
    {
      print(as.numeric(current_week+1.85))
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  # # if(grep('virus_copies',(x)))
  # print(as.numeric(0))
  
  check_hospit_57.7 <- function(x)
    
  {
    
    if('hospital' %in% x)
    {
      print(as.numeric(current_week+0.6))
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  # ### ----------------- ********* mobility Uppsala on water plot
  check_mobility_Retail <- function(x)
    
  {
    
    if('mobility' %in% x)
      
    {
      mobility_weekly_avg_lis3_uppsala$retail_recreation_GM_avg
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  check_mobility_Work <- function(x)
    
  {
    
    if('mobility' %in% x)
      
    {
      mobility_weekly_avg_lis3_uppsala$workplaces_GM_avg
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  check_mobility_Resid <- function(x)
    
  {
    
    if('mobility' %in% x)
      
    {
      mobility_weekly_avg_lis3_uppsala$residential_GM_avg
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  # to make the positivity indicators disappear and appear
  
  check_mobility_58 <- function(x)
    
  {
    
    if('mobility' %in% x)
    {
      print(as.numeric(34.4))
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  check_mobility_57.7 <- function(x)
    
  {
    
    if('mobility' %in% x)
    {
      print(as.numeric(34.9))
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  # ##################################################2ND MULTIPLOT###########################################
  
  # ## ----------------- ********* barplots on water plot
  check_virus_copies_lan <- function(x)
    
  {
    
    if('notification' %in% x) # notification_lan
    {
      print(as.numeric(0))
    }
    else if('positivity' %in% x) # positivity_lan
      
    {
      print(as.numeric(0))    }
    
    else if('Uppskattning' %in% x) # Uppskattning_lan
      
    {
      print(as.numeric(0))    }
    
    else if('tester' %in% x)   # tester_lan
      
    {
      print(as.numeric(0))    }
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  # labels for sewage barplot
  
  label_water_lan <- function(x)
    
  {
    
    if('notification' %in% x)
    {
      print(FALSE)
    }
    else if('positivity' %in% x)
      
    {
      print(FALSE)   }
    
    else if('Uppskattning' %in% x)
      
    {
      print(FALSE)   }
    
    else if('tester' %in% x)
      
    {
      print(FALSE)    }
    
    else
    {
      print(FALSE)
    }
    
  }
  
  sewage_color_axis_lan <- function(x)
    
  {
    
    if('notification' %in% x)
    {
      print("#fbebeb")
    }
    else if('positivity' %in% x)
      
    {
      print("#fbebeb")   }
    
    else if('Uppskattning' %in% x)
      
    {
      print("#fbebeb")   }
    
    else if('tester' %in% x)
      
    {
      print("#fbebeb")   }
    
    else if('hospital' %in% x)
      
    {
      print("#fbebeb")   }
    
    else if('calls112' %in% x)
      
    {
      print("#fbebeb")   }
    
    else
    {
      print("#fbebeb")
    }
  }
  
  # ### ----------------- ********* notification on water plot
  check_notif_lan <- function(x)
    
  {
    
    if('notification' %in% x)
    {
      uppsala_all_weeks_multi_Lan$notification
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  label_color_lintype_lan <- function(x)
    
  {
    
    if('notification' %in% x)
      
    {
      print(NULL)
    }
    
    else if('positivity' %in% x)
      
    {
      print(NULL)
    }
    
    else if('Uppskattning' %in% x)
      
    {
      print(NULL)
    }
    
    else if('tester' %in% x)
      
    {
      print(NULL)
    }
    
    else if('hospital' %in% x)
      
    {
      print(NULL)   }
    
    else if('calls112' %in% x)
      
    {
      print(NULL)   }
    
    else if('mobility' %in% x)
      
    {
      print(NULL)
    }
    
    else
    {
      print(FALSE)
    }
    
  }
  
  notific_color_axis_lan <- function(x)
    
  {
    
    if('notification' %in% x)
      
    {
      print("#ca4905")
    }
    
    else
    {
      print("#fbebeb")
    }
    
  }
  
  # ### ----------------- ********* positivity on water plot
  check_pos_lan <- function(x)
    
  {
    
    if('positivity' %in% x)
      
    {
      uppsala_all_weeks_multi_Lan$positivity
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  # to make the positivity indicators disappear and appear
  
  check_pos_58_lan <- function(x)
    
  {
    
    if('positivity' %in% x)
    {
      print(as.numeric(current_week+1.7))
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  check_pos_58.5_lan <- function(x)
    
  {
    
    if('positivity' %in% x)
    {
      print(as.numeric(current_week+2.2))  # current_week+(1/2))
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  check_pos_57.7_lan <- function(x)
    
  {
    
    if('positivity' %in% x)
    {
      print(as.numeric(current_week+0.9))
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  # ### ----------------- ********* CSSS
  check_Uppskattning_lan <- function(x)
    
  {
    
    if('Uppskattning' %in% x)
    {
      csss_lan_average_w$Uppskattning
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  # csss_lan_trends_w High_CI Low_CI check_Uppskattning_lan_high check_Uppskattning_lan_low
  
  check_Uppskattning_lan_low <- function(x)
    
  {
    
    if('Uppskattning' %in% x)
    {
      csss_lan_trends_w$Low_CI
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  check_Uppskattning_lan_high <- function(x)
    
  {
    
    if('Uppskattning' %in% x)
    {
      csss_lan_trends_w$High_CI
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  # check_csss_57.7
  check_csss_57.7_lan <- function(x)
    
  {
    
    if('Uppskattning' %in% x)
    {
      print(as.numeric(35.8))
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  check_csss_58_lan <- function(x)
    
  {
    
    if('Uppskattning' %in% x)
    {
      print(as.numeric(34.6))
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  # # if(grep('virus_copies',(x)))
  # print(as.numeric(34.3))
  # print(as.numeric(0))
  
  # ### ----------------- ********* positivity on water plot
  check_test_lan <- function(x)
    
  {
    
    if('tester' %in% x)
      
    {
      uppsala_all_weeks_multi_Lan$testing
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  # to make the positivity indicators disappear and appear
  
  check_test_58_lan <- function(x)
    
  {
    
    if('tester' %in% x)
    {
      print(as.numeric(current_week+0.7))
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  check_test_58.5_lan <- function(x)
    
  {
    
    if('tester' %in% x)
    {
      print(as.numeric(current_week+1.2))  # current_week+(1/2))
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  check_test_57.7_lan <- function(x)
    
  {
    
    if('tester' %in% x)
    {
      print(as.numeric(current_week-0.2))
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  # ### ----------------- ********* calls112 on water plot
  check_calls112_lan <- function(x)
    
  {
    
    if('calls112' %in% x)
      
    {
      all_kommun_lan2_lan$adjusted_freq
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  # to make the positivity indicators disappear and appear
  
  check_calls112_58_lan <- function(x)
    
  {
    
    if('calls112' %in% x)
    {
      print(as.numeric(34.4))
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  # # if(grep('virus_copies',(x)))
  # print(as.numeric(0))
  
  check_calls112_57.7_lan <- function(x)
    
  {
    
    if('calls112' %in% x)
    {
      print(as.numeric(34.9))
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  # ### ----------------- ********* calls1177 on water plot
  check_calls1177_lan <- function(x)
    
  {
    
    if('calls1177' %in% x)
      
    {
      final_for_plotting2_lan$adjusted_freq_100k
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  # to make the positivity indicators disappear and appear
  
  check_calls1177_58_lan <- function(x)
    
  {
    
    if('calls1177' %in% x)
    {
      print(as.numeric(35.1))
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  check_calls1177_58.5_lan <- function(x)
    
  {
    
    if('calls1177' %in% x)
    {
      print(as.numeric(34.6))  # current_week+(1/2))
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  check_calls1177_57.7_lan <- function(x)
    
  {
    
    if('calls1177' %in% x)
    {
      print(as.numeric(35.9))
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  # ### ----------------- ********* hospitalizarion on water plot
  check_hospit_lan <- function(x)
    
  {
    
    if('hospital' %in% x)
      
    {
      uppsala_multi_hospit_Lan$pat_per_1k_adult
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  # to make the positivity indicators disappear and appear
  
  check_hospit_58_lan <- function(x)
    
  {
    
    if('hospital' %in% x)
    {
      print(as.numeric(current_week+1.85))
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  # # if(grep('virus_copies',(x)))
  # print(as.numeric(0))
  
  check_hospit_57.7_lan <- function(x)
    
  {
    
    if('hospital' %in% x)
    {
      print(as.numeric(current_week+0.6))
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  # ### ----------------- ********* mobility Uppsala on water plot
  check_mobility_lan_Retail <- function(x)
    
  {
    
    if('mobility' %in% x)
      
    {
      mobility_weekly_avg_lis3_lan$retail_recreation_GM_avg
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  check_mobility_lan_Work <- function(x)
    
  {
    
    if('mobility' %in% x)
      
    {
      mobility_weekly_avg_lis3_lan$workplaces_GM_avg
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  check_mobility_lan_Resid <- function(x)
    
  {
    
    if('mobility' %in% x)
      
    {
      mobility_weekly_avg_lis3_lan$residential_GM_avg
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  # to make the positivity indicators disappear and appear
  
  check_mobility_lan_58 <- function(x)
    
  {
    
    if('mobility' %in% x)
    {
      print(as.numeric(34.4))
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  check_mobility_lan_57.7 <- function(x)
    
  {
    
    if('mobility' %in% x)
    {
      print(as.numeric(34.9))
    }
    
    else
    {
      print(as.numeric(0))
    }
    
  }
  
  lift_Google_plot_up <- function(x)
    
  {
    
    if('mobility' %in% x)
    {
      print(16*0.0002*25, quote=FALSE) # paste0(as.numeric(1)) #print(as.numeric(1))  mobility_weekly_sma_lis3_uppsala
    }
    
    else
    {
      print(0, quote = FALSE)   # print('#', quote = FALSE)
    }
    
  }
  
  palette_ECDC_vaccine <- function(x)
    
  {
    
    if(input$ecdc_vaccine_palette== TRUE)
    {
      c('#666666','#c2d9e7', '#7cbdc3','#508fa1','#003c4f')
      
    }
    
    else
    {
      c('#666666', '#828282', '#65B32E', '#F2A72E','#b02d04','#730c03')
    }
    
  }
  
  palette_ECDC <- function(x)
    
  {
    
    if(input$ecdc_palette== TRUE)
    {
      c('#666666', '#828282', '#c2d9e7', '#7cbdc3','#508fa1','#003c4f')
    }
    
    else
    {
      c('#666666', '#828282', '#65B32E', '#F2A72E','#b02d04','#730c03')
    }
    
  }
  
  filename_ECDC <- function(x)
    
  {
    
    if(input$ecdc_palette== TRUE)
      
    {
      list(src = normalizePath(file.path(paste0('images/', 'ecdc_2021_labels_cb', '.png'))),  width=400, height = 400 )
      
    }
    
    else
    {
      list(src = normalizePath(file.path(paste0('images/', 'ecdc_2021_labels', '.png'))),  width=400, height = 400 )
    }
  }
  
  filename_vaccine_ECDC <- function(x)
    
  {
    
    if(input$ecdc_vaccine_palette== TRUE)
      
    {
      list(src = normalizePath(file.path(paste0('images/', 'ECDC_vacc_leg_se_colorblind', '.png'))),  width=400, height = 120 )#  width=300, height = 450 )
      
    }
    
    else
    {
      list(src = normalizePath(file.path(paste0('images/', 'ECDC_leg_se_vacc', '.png'))),  width=400, height = 120 )  #  width=400, height = 700
    }
  }
  
  filename_analysis <- function(x)
    
  {
    
    if(input$analysis_palette== TRUE)
      
    {
      list(src = normalizePath(file.path(paste0('images/', 'R_Sweden_cb_wLogo', '.png'))),  width= 1040, height= 1000 )
      
    }
    
    else
    {
      list(src = normalizePath(file.path(paste0('images/', 'R_Sweden_wLogo', '.png'))),  width= 1040, height= 1000 )
    }
  }
  
  # ******************* OUTPUTS **********************
  
  # ---------- CRUSH LOGO SIDEBAR ------------------------------------
  output$crush_logo <- renderImage({
    filename <- normalizePath(file.path(paste0('images/', 'crush_logo_shiny', '.png')))
    list(
      src = filename,
      style="display: block; margin-left: auto; margin-right: auto; margin-bottom: auto;",
      height = "40%"
    )
  }, deleteFile = FALSE)
  
  # ---------- WECLOME PAGE ------------------------------------
  
  addClass(selector = "body", class = "sidebar-collapse")
  
  output$welcome_1 <- renderUI({
    str1 <- paste("Välkommen till CRUSH Covids dashboard med interaktiv grafik. På vänster sida ser du menyn med de olika analyserna. På den här sidan kan du följa utvecklingen av covid-19 pandemin i Uppsala Län. ")
    str2 <- paste("")
    str3 <- paste("CRUSH Covid är ett innovativt tvärvetenskapligt forskningsprojekt i samverkan mellan Region Uppsala och forskare från fem olika institutioner vid Uppsala universitet. Syftet med projektet är att kartlägga och försöka dämpa ökad smittspridning samt kraftiga lokala utbrott av covid-19 i Uppsala län.")
    str4 <- paste("")
    str5 <- paste(icon('exclamation-circle', class="glow"),"Webbplatsen uppdateras med de senaste tillgängliga uppgifterna varje torsdag.")#, icon('exclamation-triangle', class="glow"))
    str6 <- paste("")
    str7 <- paste(icon('sync-alt'),"Senast data uppdaterad: ", lastDataUpdate)   #lastDataUpdate.
    str8 <- paste(icon('sync-alt'),"Nästa schemalagda uppdatering: ", nextDataUpdate)
    
    HTML(paste(str1, str2, str3,str4, str5,str6,str7,str8, sep = '<br/>'))# str7,str8,str9,str10,
  })
  
  output$welcome_2 <- renderUI({
    str1 <- paste("-Information om covid-19 och provtagning: ", tags$a(href="https://www.1177.se/Uppsala-lan", '1177'))
    str2 <- paste("-Forskningsprojektets hemsida: ", tags$a(href="https://www.uu.se/crushcovid","CRUSH COVID"))
    str3 <- paste("-Uppdateringar om forskningsprojektet: CRUSH Covids på ", tags$a(href="https://www.instagram.com/crushcovid_uppsala/", 'Instagram'))
    str4 <- paste("-Korta informationsfilmer om covid-19: CRUSH Covids kanal på ", tags$a(href="https://www.youtube.com/channel/UCjiO9OZDNaxSWmX8PfyfWsw/featured?view_as=subscriber", 'YouTube'))
    str5 <- paste("")
    
    HTML(paste(str1,  str2,  str3,  str4,str5, sep = '<br/>')) # str2, str4, str6, str9,  str10,
  })
  
  output$welcome_3 <- renderUI({
    str1 <- paste(icon('exclamation-circle', class="glow"),'Riktlinjer för rekommenderad testning ändrades den första november. Därför är perioden före respektive efter vecka 44 inte jämförbara.')
    
    str2 <- paste(icon('exclamation-circle', class="glow"),"Uppdatering 6 oktober 2021: Mätarna visar nu information om testning i hela befolkningen.") #är invånare 15 år eller äldre.")
    str3 <- paste(icon('exclamation-circle', class="glow"),'Baserat på befolkningsmängd 31 mars 2021.')
    HTML(paste( str1, str2,  str3, sep = '<br/>')) # str2, str4, str6, str9,  str10,
  })
  
  
  output$speedometer_pos <-  renderGvis({
    
    gvisGauge(speedometer_pos,
              options=list(min=0, max=40,
                           greenFrom=0, greenTo=0.9,
                           yellowFrom=0.9, yellowTo=3.9,
                           redFrom=3.9, redTo=40,
                           greenColor= '#64b529' ,
                           yellowColor='#fad254',
                           redColor='#ae500a',
                           width=600, height=600))
    
  })
  
  output$speedometer_pos_before <-  renderGvis({
    
    gvisGauge(speedometer_w_last_pos,
              options=list(min=0, max=40,
                           greenFrom=0, greenTo=0.9,
                           yellowFrom=0.9, yellowTo=3.9,
                           redFrom=3.9, redTo=40,
                           greenColor= '#64b529' ,
                           yellowColor='#fad254',
                           redColor='#ae500a',
                           width=600, height=600))
    
  })
  
  output$speedometer_w_perc_diff_speed_plot_pos <-  renderGvis({
    
    gvisGauge(speedometer_w_perc_diff_speed_pos,
              options=list(min=-80, max=80,
                           greenFrom=-80, greenTo=0,
                           greenColor= '#64b529' ,
                           yellowFrom=0, yellowTo=3,
                           yellowColor='#fad254',
                           redFrom=3, redTo=80,#redTo=1200,
                           width=600, height=600))
    
  })
  
  output$speedometer_test <-  renderGvis({
    
    gvisGauge(speedometer_test,
              options=list(
                min=0, max=5000,
                greenFrom=1000, greenTo=5000,
                yellowFrom=300, yellowTo=1000,
                redFrom=0, redTo=300,
                greenColor= '#64b529' ,
                yellowColor='#b3d06f',
                redColor='#e8e8ba',  #e8e8ba
                width=600, height=600))
    
  })
  
  output$speedometer_test_before <-  renderGvis({
    
    gvisGauge(speedometer_w_last_test,
              options=list(
                min=0, max=5000,
                greenFrom=1000, greenTo=5000,
                yellowFrom=300, yellowTo=1000,
                redFrom=0, redTo=300,
                greenColor= '#64b529' ,
                yellowColor='#b3d06f',
                redColor='#e8e8ba',  #e8e8ba
                width=600, height=600))
    
  })
  
  output$speedometer_w_perc_diff_speed_plot_test <-  renderGvis({
    
    gvisGauge(speedometer_w_perc_diff_speed_test,
              options=list(min=-200, max=200,
                           greenFrom=-200, greenTo=-50,
                           greenColor= '#FF0000' ,
                           yellowFrom=-50, yellowTo=0,
                           yellowColor='#fad254',
                           redFrom=0, redTo=200,#redTo=1200,
                           redColor='#64b529',
                           width=600, height=600))
    
  })
  
  output$speedometer_csss <-  renderGvis({
    
    gvisGauge(current_week_csss,
              options=list(min=0, max=1,
                           greenFrom=0, greenTo=0.1,
                           yellowFrom=0.1, yellowTo=0.3,
                           redFrom=0.3, redTo=1,
                           width=400, height=400))   # success = c(0, 0.05), warning = c(0.05,0.1), danger = c(0.1, 1))
    
  })
  
  output$speedometer_csss_before <-  renderGvis({
    
    gvisGauge(last_week_csss,
              options=list(min=0, max=1,
                           greenFrom=0, greenTo=0.1,
                           yellowFrom=0.1, yellowTo=0.3,
                           redFrom=0.3, redTo=1,
                           width=400, height=400))
    
  })
  
  output$speedometer_w_perc_diff_speed_plot_csss <-  renderGvis({
    
    gvisGauge(current_week_csss_change,
              options=list(min=-100, max=100,
                           greenFrom=-100, greenTo=0,
                           greenColor= '#64b529' ,
                           yellowFrom=0, yellowTo=3,
                           yellowColor='#fad254',
                           redFrom=3, redTo=100,#redTo=1200,
                           width=400, height=400))
    
  })
  
  # gaugeSectors(
  # speed_csss[[i]] <- speed_i
  # speed_csss[[1]]})
  # speed_csss[[2]]})
  
  # ---------- CRUSH LOGOS AT THE BOTTOM OF WELCOME PAGE ------------------------------------
  
  output$shiny_UA_UU <- renderImage({
    filename <- normalizePath(file.path(paste0('images/', 'shiny_logos_RU_UU', '.png')))
    list(src = filename,
         width='400ymax', height = '140xmax')
    
  }, deleteFile = FALSE)
  
  output$shiny_crush <- renderImage({
    filename <- normalizePath(file.path(paste0('images/', 'shiny_logos_crush', '.png')))
    list(src = filename, width='230ymax', height = '140xmax')
  }, deleteFile = FALSE)
  
  # ------------ WEEKLY REPORT ------------------------------------ # smittspridning
  
  output$summary_new <- renderUI({
    str0 <- paste('I löpande text hänvisar vi till grafiska illustrationer på vår resultatsida. Flikar hittar ni till vänster på hemsidan, knappar är valmöjligheter som ges på respektive flik. Vi rekommenderar därför att ni redan innan ni läser vidare öppnar upp resultatsidan.') # Vi rekommenderar därför att ni redan innan ni läser vidare öppnar upp resultatsidan
    
    str1i <- paste('')
    str2 <-paste(icon('newspaper', class='glow'),'När behov av testing inte möts och riktlinjerna ändras så att urval av tester inte återspeglar befolkningen i sin helhet, blir testoberoende indikatorer viktigare. CRUSH Covid använder 112, 1177, avloppsmätningar, Google mobility och Covid Symptom Study (CSS) som testoberoende variabler..')
    str3i <- paste('')
    str3 <- paste('Under vecka 5 minskade testningen med 41% jämfört med vecka 4. Samtidigt visar våra testoberoende indikatorer en fortsatt hög smittspridning. Vi har därför bestämt att inte publicera incidens eller trender som är baserade på PCR provtagningen.')
    str4 <- paste('')
    str4i <- paste('I CSS kan man registrera resultat av snabbtester (antigen). Ju fler som deltar i CSS desto bättre blir CRUSH Covid.')
    str5ii <- paste('')
    
    HTML(paste(str0, str1i, str2, str3i, str3, str4,str4i,str5ii, sep = '<br/>')) # , str3, str4, str5, str6, str7,  str6, str7, str8, str9,
    
  })
  
  output$summary1 <- renderUI({
    str2 <-paste('Smittläget är mycket svårbedömt men de flesta datapunkterna visar dock tecken på fortsatt förbättrat eller stabilt läge på länsnivå. Sjukhusinläggningarna på grund av/med covid-19 fortsätter att sjunka i samtliga kommuner. 1177 och 112 ligger jämförbart med föregående vecka på länsnivå och Covid Symptom Study visar tecken på något färre med Covid-symtom. Genomsnittliga positiviteten under vecka 18 och 19 sjönk i alla kommuner utom i Håbo och Östhammar kommun. Testningen ligger oförändrad på mycket låga nivåer (245 tester/100,000 inv/7d).')
    str5 <- paste('Avloppsmätningar och testpositivitet minskar på länsnivå och data från 1177, 112 och symtommätning i Covid Symptom Study ligger oförändrat på länsnivå. På kommunnivå är bilden mer heterogen.')
    HTML(paste( str2, str5, sep = '<br/>')) # , str3, str4, str5, str6, str7,  str6, str7, str8, str9,
    
  })
  
  output$summary_bekr <- renderUI({
    
    str00 <- ('Att notera är att testnivåerna nu är så låga att också positiviteten får en stor variation vecka till vecka. Den över 14 dagar rullande testpositiviteten minskade med ytterligare tre procentenheter till 10%, men det är en varierande bild i länets kommuner. Antal tester per 100 000 invånare var vecka 18-19 knappt 10% av testerna vecka 4-5, flik Topplistor.')
    str1 <- paste("")
    
    HTML(paste( str00, str1,  sep = '<br/>')) # , str3, str4, str5, str6, str7,  str6, str7, str8, str9,
    
  })
  
  output$summary_kompletterande <- renderUI({
    
    str11 <- paste('På länsnivå fortsätter sjukhusinläggningar med covid-19 att minska i samtliga kommuner, se flik Topplistor. Under vecka 18 och 19 vårdades flest patienter per capita från Heby, Älvkarleby och Östhammar. Om covid-19 är huvudorsak till inläggningen framgår inte av data. ')
    str1 <- paste("")
    HTML(paste( str11, str1, sep = '<br/>')) # , str3, str4, str5, str6, str7,  str6, str7, str8, str9,
    
  })
  
  output$summary_smit <- renderUI({
    
    str3 <- paste('På grund av litet underlag kan vi denna vecka inte redovisa smittspårningsstatistik
.')
    str4 <- paste("")
    
    HTML(paste( str3,str4, sep = '<br/>')) # ,str5i,str5, str3, str4, str5, str6, str7,  str6, str7, str8, str9,
    
  })
  
  output$summary_vacc <- renderUI({
    
    str3 <- paste('Majoriteten i samtliga åldersgrupper över 40 år är nu vaccinerade med dos 3. Vi noterar också att majoriteten 65 år och äldre har nu fått sin fjärde dos, Flik Vaccinationstäckning. Det är ytterst få som nu påbörjar vaccinationsprogrammet med första dos (rosa), se flik Vaccinationstäckning.')
    str4 <- paste("")
    
    HTML(paste( str3,str4, sep = '<br/>')) # ,str5i,str5, str3, str4, str5, str6, str7,  str6, str7, str8, str9,
    
  })
  
  output$summary2 <- renderUI({
    
    str0 <- paste('Frånsett avloppsvattenmätning, kan de testoberoende indikatorerna påverkas av andra orsaker till luftvägssymtom och blir under pollensäsong än svårare att värdera.

i Uppsala kommun registrerade under vecka 19 ungefär lika många personer som under vecka 18 covidassocierade symptom i Covid Symptom Study, se tabell 1.

Från vecka 19 har 4 av 8 kommuner i länet lämnat prover från avloppsvattnet. I Tierp, Älvkarleby och Östhammars kommun har halterna nu sjunkit till mycket låga koncentrationer. I Uppsala är halten högre och oförändrad jämfört med vecka 18, se flik Avlopp och tabell 1. 1177 och 112 ligger jämförbart med föregående vecka på länsnivå, med en liten uppgång i 112 samtal. I Håbo pekar samtliga tillgängliga indikatorer uppåt, men sett över de senaste två veckorna ligger kommunen mycket bra till, flik Topplistor.')
    str0i <- paste("")
    
    HTML(paste(str0, str0i, sep = '<br/>')) # str1, str2,,,str8, str12, str2, str3, str4,, str6, str7,  str10,str11, str12
  })
  
  # top 10 table
  
  # ###### Stadsdel
  
  output$table_formattable_cases <- renderFormattable({formattable(align=c("l","c","c"),
                                                                   table_tops_reactive_cases(), #uppsala_stad_tabl_selected_cases,
                                                                   list(
                                                                     Stadsdel= formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
                                                                     `Förändring` = change_formatter,
                                                                     'Fall*'= formatter("span",
                                                                                        style = function(x){
                                                                                          style(display            = "block",
                                                                                                padding            = "0 4px",
                                                                                                `border-radius`    = "4px",
                                                                                                `background-color` = colors_cases_reactive()
                                                                                          )})
                                                                   ))})#  color_tile("white", "orange")))})
  
  output$table_formattable_pos <- renderFormattable({formattable(align=c("l","c","c"),
                                                                 table_tops_reactive_pos(), #,
                                                                 list(Stadsdel= formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
                                                                      `Förändring` = change_formatter,
                                                                      Positivitet= formatter("span",
                                                                                             
                                                                                             style = function(x){
                                                                                               style(display            = "block",
                                                                                                     padding            = "0 4px",
                                                                                                     `border-radius`    = "4px",
                                                                                                     `background-color` = colors_pos_reactive()
                                                                                               )})
                                                                 ))})
  
  output$table_formattable_test <- renderFormattable({formattable(align=c("l","c","c"),
                                                                  table_tops_reactive_test(), #,
                                                                  list(Stadsdel= formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
                                                                       `Förändring` = change_formatter,
                                                                       'Tester*'= formatter("span",
                                                                                            style = function(x){
                                                                                              style(display            = "block",
                                                                                                    padding            = "0 4px",
                                                                                                    `border-radius`    = "4px",
                                                                                                    `background-color` = colors_test_reactive()
                                                                                              )})
                                                                  ))})
  
  # ###### KOMMUN
  
  output$table_formattable_cases_kommun <- renderFormattable({formattable(align=c("l","c","c"),
                                                                          uppsala_kommun_table_cases, #,
                                                                          
                                                                          list(
                                                                            Kommun= formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
                                                                            `Förändring` = change_formatter,
                                                                            'Fall*'= formatter("span",
                                                                                               style = function(x){
                                                                                                 style(display            = "block",
                                                                                                       padding            = "0 4px",
                                                                                                       `border-radius`    = "4px",
                                                                                                       `background-color` = colors_cases_kommun
                                                                                                 )})
                                                                          ))})#  color_tile("white", "orange")))})
  
  
  output$table_formattable_pos_kommun <- renderFormattable({formattable(align=c("l","c","c"),
                                                                        uppsala_kommun_table_pos, #,
                                                                        list(Kommun= formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
                                                                             `Förändring` = change_formatter,
                                                                             Positivitet= formatter("span",
                                                                                                    
                                                                                                    style = function(x){
                                                                                                      style(display            = "block",
                                                                                                            padding            = "0 4px",
                                                                                                            `border-radius`    = "4px",
                                                                                                            `background-color` = colors_pos_kommun
                                                                                                      )})
                                                                        ))})
  
  output$table_formattable_test_kommun <- renderFormattable({formattable(align=c("l","c","c"),
                                                                         uppsala_kommun_table_test, #,
                                                                         list(Kommun= formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
                                                                              `Förändring` = change_formatter,
                                                                              'Tester*'= formatter("span",
                                                                                                   style = function(x){
                                                                                                     style(display            = "block",
                                                                                                           padding            = "0 4px",
                                                                                                           `border-radius`    = "4px",
                                                                                                           `background-color` = colors_test_kommun
                                                                                                     )})
                                                                         ))})
  # uppsala_kommun_table_hosp colors_hosp_kommun Inläggningar
  
  output$table_formattable_hosp_kommun <- renderFormattable({formattable(align=c("l","c","c"),
                                                                         uppsala_kommun_table_hosp, #,
                                                                         list(Kommun= formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
                                                                              `Förändring` = change_formatter,
                                                                              'Sjukhusvårdade**'= formatter("span",
                                                                                                            style = function(x){
                                                                                                              style(display            = "block",
                                                                                                                    padding            = "0 4px",
                                                                                                                    `border-radius`    = "4px",
                                                                                                                    `background-color` = colors_hosp_kommun
                                                                                                              )})
                                                                         ))})
  
  # ###### LAN
  
  output$table_formattable_ian <- renderFormattable({formattable(align=c("l","c","c"),
                                                                 uppsala_ian_table_final2_long2,
                                                                 # uppsala_ian_table_final#, #,
                                                                 
                                                                 list(Variabel= formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
                                                                      # Värde=change_formatter_decimals,
                                                                      `Förändring` = change_formatter
                                                                      # style(display = "block",
                                                                      # `border-radius` = "4px",
                                                                      # `background-color` = colors_hosp_kommun
                                                                 )
  )})#  color_tile("white", "orange")))})
  
  output$table_formattable_ian_hosp <- renderFormattable({formattable(align=c("l","c","c"),
                                                                      uppsala_LAN_table_hosp,
                                                                      # uppsala_ian_table_final#, #,
                                                                      
                                                                      list(Variabel= formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
                                                                           `Förändring` = change_formatter
                                                                           # style(display = "block",
                                                                           # `border-radius` = "4px",
                                                                           # `background-color` = colors_hosp_kommun
                                                                      )
  )})#  color_tile("white", "orange")))})
  
  # ---------- TRENDS ------------------------------------
  
  output$trend_text <- renderUI({
    str1 <- paste('  Nya fall & testpostivitet:')
    str2 <- paste("  -Nya fall (färgade linjer, vänster trender): antalet bekräftade covid-19-fall per 100 000 invånare per vecka. ")
    str3 <- paste("  -Testpositivitet (%, streckade linjer, vänster trender) : andelen positiva covid-19 test av alla tester per vecka.")
    str4i <- paste("  ")
    str4 <- paste("  Tester:")
    str5 <- paste("  -Antalet (färgade linjer, höger trender): antalet covid-19-tester per 100 000 invånare per vecka.")
    str6 <- paste("                                             ")
    str7 <- paste(icon('exclamation-circle', class='glow'), 'Siffran inom parentes är hela befolkningen i valt område.')
    str8 <- paste(icon('exclamation-circle', class="glow"),"Baserat på befolkningsmängd 31 mars 2021.")
    
    HTML(paste(str1, str2, str3, str4i, str4, str5, str6, str7, str8,sep = '<br/>')) # str4, ,str6
    
  })
  
  cols_vacc_coverage <- c("unvaccinated"='#CA0020',
                          "class1"="#F4A582",
                          "class2"="#92C5DE",
                          "class3"= "#0571B0",
                          "class4"= 'gold1',
                          "class5"= '#ff7b00'
  )    # 001f7a fcfbe3    f5e725  goldenrod1   gold1
  
  output$vacc_bar_lan  <- renderPlot({
    
    ggplot(cityData_coverage(), aes(fill=vacc_status, switch_coverage_percentage(), # y=population, #switch_coverage_percentage(),
                                    x=sex))+#sex))+#interaction(sex,age_group))) +  final_Lan_vacc_bar2   y=population
      
      geom_bar(position="stack",
               stat="identity") +
      
      scale_fill_manual(name='',labels = c("Ej vaccinerade",
                                           "Mindre tre veckor efter dos 1",
                                           "Tre veckor eller mer efter dos 1",
                                           "Två veckor eller mer efter dos 2",
                                           "En vecka eller mer efter dos 3",
                                           "En vecka eller mer efter dos 4"),
                        values= cols_vacc_coverage )+ #c( "#F4A582" ,"#92C5DE", "#0571B0",'#001f7a', '#CA0020') )+
      
      # ggtitle("") +
      theme_bw()+
      labs(x='Åldersgrupp', y=switch_coverage_percentage_yaxis(), #'Antal',  # Invånare
           caption =  c('Åldersgrupp'))+
      facet_wrap(~age_group, strip.position = "bottom",nrow=1) +
      theme(
        strip.background = element_rect(color="gray89",
                                        fill = 'gray99', size = 0.5),
        axis.text.x=element_text(angle = 90,size= 14),
        axis.title.y = element_text(size= 16),
        axis.text.y=element_text(size= 13),
        strip.text = element_text(size = 16),
        panel.grid.major.x = element_blank() ,
        plot.caption = element_text(hjust=0.5, size=15), # face = "bold",
        panel.border = element_rect(colour = "gray89", fill=NA, size=1),#element_blank(), # remove side vertical lines
        legend.position = "top",
        panel.spacing = unit(2, "mm"),
        axis.ticks.x = element_blank(),
        legend.box = "horizontal", # horizontal
        legend.text = element_text( size=15),
        strip.placement = "outside")+
      scale_x_discrete(name = NULL, labels = label_genders)#+#,family='fontawesome-webfont')) +
    # xlab("Åldersgrupp")
    
  })
  
  output$trends <- renderPlot({
    
    ggplot(data=uppsala_iann_new,aes(x=week))+
      geom_line(aes(y= notification), size=2.2, alpha = 1, color='forestgreen') +
      geom_line(aes(y= positivity*5000),  # ( you divide the two max of the two y axis)
                size=1.5, alpha = 0.8, color='black', linetype=2) +
      
      geom_point(aes(y= notification), color='#018219') +
      scale_x_continuous(limits = c(current_week-9,current_week), expand=c(0,0),
                         breaks= breaks_weeks,
                         labels = paste0(labels_weeks)
      )+
      
      scale_y_continuous(limits=c(0,3000),expand=c(0,0),  # 500/30
                         sec.axis = sec_axis(trans=~./50.00, name ='Testpositivitet (%)')) +
      labs(x='Vecka', y='Nya fall' )+
      theme(axis.title = element_text(),panel.spacing = unit(1, "lines"),
            legend.position = "none",
            panel.background = element_rect(fill='#fbebeb', colour='#fbebeb'),
            plot.background = element_rect(fill = '#fbebeb', colour = '#fbebeb'),
            axis.text.x=element_text(angle = 40, vjust = 0.8, hjust=1,size= 20),
            axis.text.y=element_text(size= 20),
            axis.title.x = element_text(size=20), axis.title.y = element_text(size=20))
    
  })
  
  output$trendsKommun <- renderPlot({
    ggplot(data=kommunData(),aes(x=week))+
      geom_line(aes(y= notification), size=2.2, alpha = 1, color='magenta') +
      geom_line(aes(y= positivity*5000),
                size=1.5, alpha = 0.8, color='black', linetype=2) +
      geom_point(aes(y= notification), color='magenta') +
      scale_x_continuous(limits = c(current_week-9,current_week), expand=c(0,0),
                         breaks= breaks_weeks,  #c(40,45,50,53,54,current_week),
                         labels = paste0(labels_weeks)
      )+ # labels = function(x) round(as.numeric(x), digits=0))+
      # xlim(39, 52)+
      scale_y_continuous(limits=c(0,4000), expand=c(0,0),
                         sec.axis = sec_axis(trans=~./50.0, name ='Testpositivitet (%)')) +
      labs(x='Vecka', y='Nya fall' )+
      theme(axis.title = element_text(),panel.spacing = unit(1, "lines"),
            legend.position = "none",
            panel.background = element_rect(fill='#fbebeb', colour='#fbebeb'),
            plot.background = element_rect(fill='#fbebeb', colour='#fbebeb'),
            axis.text.x=element_text(angle = 40, vjust = 0.8, hjust=1,size= 20), axis.text.y=element_text(size= 20),
            axis.title.x = element_text(size= 20), axis.title.y = element_text(size= 20))
  })
  
  
  output$trends_tester <- renderPlot({
    ggplot(data=uppsala_iann_new,aes(x=week))+
      geom_line(aes(y= testing), size=2.2, alpha = 1, color='goldenrod2') +
      geom_point(aes(y= testing), color='goldenrod2') +
      scale_x_continuous(limits = c(current_week-9,current_week), expand=c(0,0),
                         breaks= breaks_weeks,  #c(40,45,50,53,54,current_week),
                         labels = paste0(labels_weeks)
      )+                          #labels = function(x) round(as.numeric(x), digits=0),
      # xlim(39, 52)+
      scale_y_continuous(limits=c(0,7000),  expand=c(0,0))+
      labs(x='Vecka',  y='Tester')+
      theme(axis.title = element_text(),panel.spacing = unit(1, "lines"),
            legend.position = "none",
            panel.background = element_rect(fill='#fbebeb', colour='#fbebeb'),
            plot.background = element_rect(fill='#fbebeb', colour='#fbebeb'),
            panel.grid.major.x = element_blank(), # REMOVE THE VERTICAL GUIDE LINES FOR X AXIS
            axis.text.x=element_text(angle = 40, vjust = 0.8, hjust=1,size= 20),  axis.text.y=element_text(size= 20),
            axis.title.x = element_text(size= 20), axis.title.y = element_text(size= 20))
  })
  
  output$trendsKommun_tester <- renderPlot({
    ggplot(data=kommunData_tester(),aes(x=week))+
      geom_line(aes(y= testing), size=2.2, alpha = 1, color='darkmagenta') +
      geom_point(aes(y= testing), color='darkmagenta') +
      scale_x_continuous(limits = c(current_week-9,current_week), expand=c(0,0),
                         breaks= breaks_weeks,  #c(40,45,50,53,54,current_week),
                         labels = paste0(labels_weeks)
      )+                    # breaks = scales::pretty_breaks(n = 6))+
      # xlim(39, 52)+
      scale_y_continuous(limits=c(0,8000),
                         expand=c(0,0))+
      labs(x='Vecka', y='Tester')+
      theme(axis.title = element_text(),panel.spacing = unit(1, "lines"),
            legend.position = "none",
            panel.background = element_rect(fill='#fbebeb', colour='#fbebeb'),
            plot.background = element_rect(fill='#fbebeb', colour='#fbebeb'),
            panel.grid.major.x = element_blank(), # REMOVE THE VERTICAL GUIDE LINES FOR X AXIS
            axis.text.x=element_text(angle = 40, vjust = 0.8, hjust=1,size= 20), axis.text.y=element_text(size= 20),
            axis.title.x = element_text(size= 20), axis.title.y = element_text(size= 20))
  })
  
  
  # ---------- TESTING MAPS ------------------------------------
  # removed_kartor
  
  
  # Our dataset for the ECDC maps
  
  data.ecdc.download <- data.ecdc.download_scilife_final2  # import(files_uppsala, which = 1)
  data.ecdc.download[is.na(data.ecdc.download)] <- 0
  
  output$downloadData_ecdc <- downloadHandler(
    filename = function() {
      paste("data_ECDC_kartor", lastDataUpdate, ".csv", sep="")
      
    },
    content = function(file) {
      write.csv(data.ecdc.download_scilife_final2, file)   # data.ecdc.download_scilife_final2  file_data_scilife
      # write.csv(data.ecdc.download, file) # data.ecdc.download_scilife_final2 file_data_scilife
      
    }
  )
  
  output$downloadData_ecdc_description <- downloadHandler(
    filename = function() {
      paste("data_ECDC_kartor_description", lastDataUpdate, ".xlsx", sep="")
      
    },
    content = function(file) {
      write.xlsx(data.ecdc.download_description, file)   # data.ecdc.download_scilife_final2  file_data_scilife
      # write.csv(data.ecdc.download, file) # data.ecdc.download_scilife_final2 file_data_scilife
      
    }
  )
  
  # ----------------------------------------- test
  output$gradient_green <- renderImage({
    filename <- normalizePath(file.path(paste0('images/', 'green_gradient_categ', '.png')))
    list(src = filename,
         width=375, height = 50 )# good ratio: 7.5
  }, deleteFile = FALSE)
  
  output$gradient_orange <- renderImage({
    filename <- normalizePath(file.path(paste0('images/', 'orange_gradient_categ', '.png')))
    list(src = filename,
         width=375, height = 50 )# good ratio:
  }, deleteFile = FALSE)
  
  output$gradient_red <- renderImage({
    filename <- normalizePath(file.path(paste0('images/', 'red_gradient_categ', '.png')))
    list(src = filename,
         width=375, height = 50 )#
  }, deleteFile = FALSE)
  
  
  # ---------- CONTACT TRACING ------------------------------------
  
  output$contact_text <- renderUI({
    str1 <- paste("Denna data har insamlats av smittspårare på NVH Provtagningsenhet och utvalda delar lagras i en helt avidentifierad databas för statistiksyfte.")
    str4 <- paste("")
    str2 <- paste('Smittspårarna frågar var de tror att de kan ha smittats (-14 till -2 dagar före insjuknande) och var de tror att de kan ha smittat andra (nära kontakter) f.o.m. dagen före insjuknande. Resultaten är viktade för frekvens, d.v.s. alla index få 1 poäng, om de uppger fler än en uppgift delas poängen med antal uppgifter de lämnat (ex. om de lämnar 5 uppgifter är varje uppgift värd 0.2 poäng etc).')
    str5 <- paste("")
    str3 <- paste("Underlaget har begränsningar. Dels är det i många fall omöjligt att veta var de smittats, vilket vi till del korrigerar för mha viktningen. Sedan finns det en tydlig risk för att uppgiftslämningen påverkas av bias (t. ex. kanske man miss den trånga festen men inte att man satt nära någon på bussen dit och hem, eller att man väljer att inte berätta om pubrundan efter vecka 44 eftersom det känns pinsamt). Smittspårarna är dock tränade att ställa dessa följdfrågor.")
    
    HTML(paste(str1, str4, str2, str5, str3,  sep = '<br/>')) # str4, str5, str6, str7, str8, str9, str10,
    
  })
  
  output$contact_1 <- renderImage({
    filename <- normalizePath(file.path(paste0('images/', 'boxplot_debut_test', '.png')))
    list(src = filename, width= 900, height= 650# 323 # ratio:1.7  width= 500, height= 460
    )
  }, deleteFile = FALSE)
  
  # list(src = filename, width= 1000, height= 610# 323 # ratio:1.7 width= 500, height= 460
  
  output$contact_2ii <- renderImage({
    filename <- normalizePath(file.path(paste0('images/', 'backward_agegroup_contact_tracing', '.png')))
    list(src = filename, width= 1000, height= 610# 323 # ratio:1.7  width= 500, height= 460
    )
  }, deleteFile = FALSE)
  
  output$contact_2 <- renderImage({
    filename <- normalizePath(file.path(paste0('images/', 'forward_agegroup_contact_tracing', '.png')))
    list(src = filename, width= 1000, height= 610# 323 # ratio:1.7  width= 500, height= 460
    )
  }, deleteFile = FALSE)
  
  output$contact_3 <- renderImage({
    filename <- normalizePath(file.path(paste0('images/', 'age_sub_cat_contact_tracing', '.png')))
    list(src = filename, width= 1000, height= 610# 323 # ratio:1.7  width= 500, height= 460
    )
  }, deleteFile = FALSE)
  
  # list(src = filename, width= 1000, height= 610# 323 # ratio:1.7 width= 500, height= 460 width= 500, height= 1000
  
  output$contact_4 <- renderImage({
    filename <- normalizePath(file.path(paste0('images/', 'forward_contact_tracing', '.png')))
    
    list(src = filename, width= 1000, height= 610# 323 # ratio:1.7  width= 500, height= 460  width= 500, height= 1000
    )
  }, deleteFile = FALSE)
  
  output$contact_4ii <- renderImage({
    filename <- normalizePath(file.path(paste0('images/', 'backward_contact_tracing', '.png')))
    
    list(src = filename, width= 1000, height= 610# 323 # ratio:1.7  width= 500, height= 460  width= 500, height= 1000
    )
  }, deleteFile = FALSE)
  
  output$contact_5 <- renderImage({
    filename <- normalizePath(file.path(paste0('images/', 'tables_contact_new', '.png')))
    list(src = filename, width= 500, height= 1000# 323 # ratio:1.7  width= 500, height= 460
    )
  }, deleteFile = FALSE)
  
  # list(src = filename, width= 1000, height= 680# 323 # ratio:1.7 width= 500, height= 460
  output$contact_6 <- renderImage({ # sub_cat_contact_tracing
    filename <- normalizePath(file.path(paste0('images/', 'time to trace', '.png')))
    
    list(src = filename, width= 1000, height= 680# 323 # ratio:1.7  width= 500, height= 460
    )
  }, deleteFile = FALSE)
  
  output$contact_symptoms <- renderImage({ # sub_cat_contact_tracing
    filename <- normalizePath(file.path(paste0('images/', 'symptom_bar_percent', '.png')))
    
    list(src = filename, width= 1000, height= 610# 323 # ratio:1.7  width= 500, height= 460  width= 500, height= 1000
    )
  }, deleteFile = FALSE)
  
  output$contact_neuro <- renderImage({
    filename <- normalizePath(file.path(paste0('images/', 'neuro_symptom_bar_percent', '.png')))
    list(src = filename, width= 1000, height= 610# 323 # ratio:1.7  width= 500, height= 460  width= 500, height= 1000
    )
  }, deleteFile = FALSE)
  
  output$contact_vark <- renderImage({
    filename <- normalizePath(file.path(paste0('images/', 'ache_symptom_bar_percent', '.png')))
    list(src = filename, width= 1000, height= 610# 323 # ratio:1.7  width= 500, height= 460  width= 500, height= 1000
    )
  }, deleteFile = FALSE)
  
  output$contact_luft <- renderImage({
    filename <- normalizePath(file.path(paste0('images/', 'airway_symptom_bar_percent', '.png')))
    list(src = filename, width= 1000, height= 610# 323 # ratio:1.7  width= 500, height= 460  width= 500, height= 1000
    )
  }, deleteFile = FALSE)
  
  output$vacc_coverage_area_plot <- renderImage({
    filename <- normalizePath(file.path(paste0('images/', 'vaccination_sex_bar_percent', '.png')))
    list(src = filename, width= 840, height= 550# 323 # ratio:1.7  width= 500, height= 460
    )
  }, deleteFile = FALSE)
  
  output$vacc_coverage_area_plot2 <- renderImage({
    filename <- normalizePath(file.path(paste0('images/', 'vaccination_sex_bar_antal', '.png')))
    list(src = filename, width= 840, height= 550# 323 # ratio:1.7  width= 500, height= 460
    )
  }, deleteFile = FALSE)
  
  # VACCINATION MAPS
  
  output$vacc_traffic_map <- renderLeaflet({     # interactive maps (it needs an observe value or just the filtered variables here)
    leaflet() %>%
      addTiles() %>%
      
      setView(lat = 59.9445, lng = 17.6794, zoom = 9) %>%
      addPolygons(group = "Traffic Vacc 50 plus", color = '#786865',
                  
                  data = weeklyData_vacc_ecdc(),
                  fillColor = ~colorFactor(palette = palette_ECDC_vaccine(input$ecdc_palette_vacc), #c('#666666', '#828282', '#65B32E', '#F2A72E','#b02d04','#730c03'), # new '#b02d04','#730c03'     oldnew :  '#990000', '#3d0101'
                                           levels = c('Dark_Grey','Light_Grey', 'Green', 'Orange','Red', 'DarkRed'),
                                           # unique(weeklyData_vacc_ecdc()$vacc_color_dose2))(weeklyData_vacc_ecdc()$vacc_color_dose2),
                                           unique(labels_switch_second_dose_traffic_observe()))(labels_switch_second_dose_traffic_observe()),
                  
                  
                  popup = paste0(#"Postnummer: ", weeklyData_vacc_ecdc()$POSTALCODE,
                    "<br>Stadsdel: ", weeklyData_vacc_ecdc()$stad,
                    # "<br>Cum vaccinated dose 2(n): ", weeklyData_vacc_ecdc()$cum_freq_dose2,
                    labels_switch_second_dose_traffic_bubbles_observe1(),labels_switch_second_dose_traffic_bubbles_observe2(),
                    # labels_switch_second_dose_traffic_bubbles_observe weeklyData_vacc_ecdc()$cum_freq_dose2,
                    "<br>Befolkning (åldersgrupper): ", weeklyData_vacc_ecdc()$population),
                  
                  smoothFactor = 0.2, fillOpacity = 0.9,  weight = 1)
    
    # str(vacc_traffic_data)
  })
  
  # ------------ ECDC MAPS ------------------------------------
  
  output$mapCity <- renderLeaflet({     # interactive maps (it needs an observe value or just the filtered variables here)
    leaflet() %>%
      addTiles() %>%
      setView(lat = 59.9445, lng = 17.6794, zoom = 9) %>%
      
      addPolygons(group = "Traffic Light", color = '#786865',
                  # "black",
                  data = weeklyData(),
                  fillColor = ~colorFactor(palette = palette_ECDC(input$ecdc_palette), #c('#666666', '#828282', '#65B32E', '#F2A72E','#b02d04','#730c03'), # new '#b02d04','#730c03'     oldnew :  '#990000', '#3d0101'
                                           levels = c('Dark_Grey','Light_Grey', 'Green', 'Orange','Red', 'DarkRed'),
                                           unique(weeklyData()$ECDC_Colour))(weeklyData()$ECDC_Colour),
                  # "<br>Nya fall (de senaste 14 dagarna): ", round(weeklyData()$notification_rate, digits = 0),
                  # "<br>Testpositivitet (%): ", round(weeklyData()$positivity_rate*100, digits = 2),
                  # "<br>Testnivå: ", round(weeklyData()$testing_rate, digits = 0)),
                  popup = paste0("Stadsdel: ", weeklyData()$stad,
                                 "<br>Nya fall (de senaste 14 dagarna): ", round(weeklyData()$notification_rate, digits = 0),
                                 "<br>Testpositivitet (%): ", round(weeklyData()$positivity_rate*100, digits = 2),
                                 "<br>Testnivå: ", round(weeklyData()$testing_rate, digits = 0)),
                  smoothFactor = 0.2, fillOpacity = 0.8,  weight = 1)
    
  })
  
  output$ECDC_legend <- renderImage({
    filename_ECDC()
    # list(src = filename_ECDC(), width=480, height = 500 )
  }, deleteFile = FALSE)
  
  output$vaccine_ECDC_legend <- renderImage({
    filename_vaccine_ECDC()
    # list(src = filename_ECDC(), width=480, height = 500 )
  }, deleteFile = FALSE)
  
  output$ECDC_definitions <- renderUI({
    
    str1 <- paste("- Nya fall: antalet bekräftade covid-19-fall per 100 000 invånare de senaste 14 dagarna på regional nivå.")
    str2 <- paste("- Testpositivitet (%): procenten av antalet positiva covid-19 test senaste veckan.")
    str3 <- paste("- Testnivå: antalet tester för pågående covid-19 infektion per 100 000 invånare genomförda den senaste veckan.")
    str4ii <- paste(icon = icon('exclamation-circle', class="glow"), ' Befolkningsantalet på kartan är hela befolkningen.')
    
    str4i <- paste(icon = icon('exclamation-circle', class="glow"), 'Baserat på befolkningsmängd 31 mars 2021.')
    str4 <- paste("")
    str5 <- paste("Färger:")
    str6 <- paste("- Grön: nya fall < 50 och testpositivitet < 4%")
    str7 <- paste("- Gul: nya fall < 75 och testpositivitet \u2265 4% eller om nya fall är mellan 50 till 200 men positiviteten < 4%.")
    str8 <- paste("- Orange: nya fall mellan 75 till 200 och testpositivitet \u2265 4%, eller om nya fall är mellan 201 till 499.")  # högre än
    str9 <- paste("- Mörkröd: nya fall \u2265 500.")
    str10 <- paste("- Grå: om nödvändig information saknas, eller om testnivån är 300 eller lägre.")
    
    HTML(paste(str1, str2, str3,str4ii,str4i, str4, str5, str6, str7, str8, str9, sep = '<br/>'))
  })
  
  output$vaccine_traffic_definitions <- renderUI({
    
    str0 <- paste('Här redovisar vi data från vaccinationer utförda i Region Uppsala per stadsdel för befolkningen över 20 år. Enbart områden med minst 100 invånare i gruppen redovisas.')
    str1 <- paste(icon = icon('exclamation-circle', class="glow"),"Populationen bygger på mars 2021 års befolkning, i områden med stora befolkningsförändringar kan siffrorna vara missvisande.")
    str3 <- paste(icon = icon('exclamation-circle', class="glow"),"Utomlänsvaccinationer ingår inte i statistiken, utan enbart vaccinationer utförda i Region Uppsala. Läs mer ", tags$a(href="https://www.folkhalsomyndigheten.se/folkhalsorapportering-statistik/statistikdatabaser-och-visualisering/vaccinationsstatistik/statistik-for-vaccination-mot-covid-19/uppfoljning-av-vaccination/statistik-om-lanens-nettofloden/", 'här.'))
    
    HTML(paste(str0, str1,  str3, sep = '<br/>')) # str3,str4i, str4, str5, str6, str7, str8, str9,
  })
  
  
  # -------------- WASTEWATER -----------------------------------------
  
  output$water_text <- renderUI({
    str1 <- paste("Coronavirusets arvsmassa kan hittas i avföringen från patienter med covid-19 med så kallad PCR-teknik (referens",tags$a(href="https://www.thelancet.com/journals/langas/article/PIIS2468-1253(20)30083-2/fulltext", 'Wu et al. 2020'),"). Detta gör det möjligt att följa infektionen hos befolkningen genom så kallad avloppsvattenbaserad epidemiologi (",tags$a(href="https://www.sciencedirect.com/science/article/pii/S0048969720344399", 'Corpuz et al. 2020'),"). Det har visat sig att virusinnehållet i avloppsvattnet kan förutsäga infektionsökningar i befolkningen och följer den epidemiska trend som mäts av fall och sjukhusvistelse (",tags$a(href="https://doi.org/10.1101/2020.05.19.20105999",'Peccia et al. 2020'),").")
    str3 <- paste("I CRUSH Covid leder",tags$a(href="https://katalog.uu.se/profile/?id=N9-812", 'Anna J. Székely'), "arbetet med att mäta virushalten i Uppsala stad, detta möjliggörs tack vare samarbetet med Uppsala Vatten.")
    str4 <- paste(icon('exclamation-circle', class='glow'), 'Den population inom parentes inkluderar hela befolkningen.')
    
    HTML(paste(str1, str3, str4, sep = '<br/>'))  # str0, str01, str00, str001,
  })
  
  output$omicron_avlopp <- renderImage({
    filename <- normalizePath(file.path(paste0('images/', 'omicron_avlopp', '.png')))
    list(
      src = filename,height = '680', width='1100') #'800', width='1300')  # 540, height= 470) # 800, height= 580)  # 1.61
  }, deleteFile = FALSE)
  
  output$logo_scilife <- renderImage({
    filename <- normalizePath(file.path(paste0('images/', 'scilife_lab', '.png')))
    list(
      src = filename,width= 260, height= 65)  # 540, height= 470), height= 680)  # 800, height= 580)  # 1.61
  }, deleteFile = FALSE)
  
  output$logo_vatten <- renderImage({
    filename <- normalizePath(file.path(paste0('images/', 'uppsala_vatten', '.png')))
    list(
      src = filename,width= 300, height= 85)  # 540, height= 470)0, height= 680)  # 800, height= 580)  # 1.61
  }, deleteFile = FALSE)
  
  output$logo_roslagsvatten <- renderImage({
    filename <- normalizePath(file.path(paste0('images/', 'roslagsvatten', '.png')))
    list(
      src = filename,width= 300, height= 65)  # 540, height= 470)height= 680)  # 800, height= 580)  # 1.61
  }, deleteFile = FALSE)
  
  output$logo_osth <- renderImage({
    filename <- normalizePath(file.path(paste0('images/', 'osth_vatten', '.png')))
    list(
      src = filename,width= 200, height= 65)  # 540, height= 470) # 800, height= 580)  # 1.61
  }, deleteFile = FALSE)
  
  output$logo_enk <- renderImage({
    filename <- normalizePath(file.path(paste0('images/', 'enk', '.png')))
    list(
      src = filename,width= 230, height= 80)  # 540, height= 470) height= 680)  # 800, height= 580)  # 1.61
  }, deleteFile = FALSE)
  
  output$logo_tierp <- renderImage({
    filename <- normalizePath(file.path(paste0('images/', 'tierp_water_logo', '.png')))
    list(
      src = filename,width= 170, height= 85)  # 540, height= 470)100, height= 680)  # 800, height= 580)  # 1.61
  }, deleteFile = FALSE)
  
  output$logo_slu <- renderImage({
    filename <- normalizePath(file.path(paste0('images/', 'slu_logo', '.png')))
    list(
      src = filename,width= 80, height= 75)  # 540, height= 470)ight= 680)  # 800, height= 580)  # 1.61
  }, deleteFile = FALSE)
  
  output$image_avlopp_omicron_upp <- renderImage({
    filename <- normalizePath(file.path(paste0('images/', 'avlopp_omicron_Uppsala', '.png')))
    list(
      src = filename,width= 800, height= 600)  # 540, height= 470)ight= 680)  # 800, height= 580)  # 1.61
  }, deleteFile = FALSE)
  
  output$sewage_trend_simple_tabs <-  renderPlot({
    
    ggplot() +#data=waste_kommun_choice(), #wastewater_All_bind22_old_new_weeks2,  wastewater_All_bind22_old_new_weeks2
      # aes(x= monday_dates#, #week_new, #x=week,))+
      
      geom_area(data=wastewater_All_bind22_old_new_weeks2,
                mapping=aes( x=  monday_dates,
                             y=RR_N1copesPPMoV*100, fill=Pump), # color=Pump
                # aes(color = variable, fill = variable),
                alpha = 0.1,
                position = position_dodge(0.8)) +
      geom_area(data=waste_kommun_choice(),
                mapping=aes( x=  monday_dates,
                             y=RR_N1copesPPMoV*100, fill=Pump),#color=Pump),
                # aes(color = variable, fill = variable),
                alpha = 0.8, position = position_dodge(0.8)) +
      
      geom_text(data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Uppsala',],
                aes(x=as.Date('2020-12-14', format='%Y-%m-%d'),
                    y=0.8),
                label= 'Mätning saknas', angle = 90, color='#ff31a3',
                alpha = missing_label_upps(), #0.4,
                size=3)+
      
      geom_text(data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Uppsala',],
                aes(x=as.Date('2020-12-28', format='%Y-%m-%d'),
                    y=0.8),
                label= 'Mätning saknas', angle = 90, color='#ff31a3',
                alpha = missing_label_upps(), #0.4,
                size=3)+
      
      geom_text(data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Enköping',],
                aes(x=as.Date('2021-10-11', format='%Y-%m-%d'),
                    y=0.8),
                label= 'Mätning saknas', angle = 90, color='#00b28d',
                alpha = missing_label_enk(), #0.4,
                size=3)+
      
      geom_text(data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Enköping',],
                aes(x=as.Date('2021-12-20', format='%Y-%m-%d'),
                    y=0.8),
                label= 'Mätning saknas', angle = 90, color='#00b28d',
                alpha = missing_label_enk(), #0.4,
                size=3)+
      
      geom_text(data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Enköping',],
                aes(x=as.Date('2021-12-27', format='%Y-%m-%d'),
                    y=0.8),
                label= 'Mätning saknas', angle = 90, color='#00b28d',
                alpha = missing_label_enk(), #0.4,
                size=3)+
      
      geom_text(data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Enköping',],
                # aes(x=c( as.Date('2021-10-11', format='%Y-%m-%d'), as.Date('2021-10-18', format='%Y-%m-%d'),as.Date('2021-10-25', format='%Y-%m-%d')),
                aes(x= as.Date('2022-04-04', format='%Y-%m-%d'),
                    y=0.8),#alpha=0.5,
                label= 'Mätning saknas', angle = 90, color='#f58b0a',
                alpha = missing_label_knivsta(), #0.4,
                size=3)+
      
      geom_text(data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Enköping',],
                # aes(x=c( as.Date('2021-10-11', format='%Y-%m-%d'), as.Date('2021-10-18', format='%Y-%m-%d'),as.Date('2021-10-25', format='%Y-%m-%d')),
                aes(x= as.Date('2022-04-11', format='%Y-%m-%d'),
                    y=0.8),#alpha=0.5,
                label= 'Mätning saknas', angle = 90, color='#f58b0a',
                alpha = missing_label_knivsta(), #0.4,
                size=3)+
      
      geom_text(data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Enköping',],
                # aes(x=c( as.Date('2021-10-11', format='%Y-%m-%d'), as.Date('2021-10-18', format='%Y-%m-%d'),as.Date('2021-10-25', format='%Y-%m-%d')),
                aes(x= as.Date('2022-04-18', format='%Y-%m-%d'),
                    y=0.8),#alpha=0.5,
                label= 'Mätning saknas', angle = 90, color='#f58b0a',
                alpha = missing_label_knivsta(), #0.4,
                size=3)+
      
      geom_text(data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Enköping',],
                # aes(x=c( as.Date('2021-10-11', format='%Y-%m-%d'), as.Date('2021-10-18', format='%Y-%m-%d'),as.Date('2021-10-25', format='%Y-%m-%d')),
                aes(x= as.Date('2022-04-25', format='%Y-%m-%d'),
                    y=0.8),#alpha=0.5,
                label= 'Mätning saknas', angle = 90, color='#f58b0a',
                alpha = missing_label_knivsta(), #0.4,
                size=3)+
      
      geom_text(data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Enköping',],
                # aes(x=c( as.Date('2021-10-11', format='%Y-%m-%d'), as.Date('2021-10-18', format='%Y-%m-%d'),as.Date('2021-10-25', format='%Y-%m-%d')),
                aes(x= as.Date('2022-05-02', format='%Y-%m-%d'),
                    y=0.8),#alpha=0.5,
                label= 'Mätning saknas', angle = 90, color='#f58b0a',
                alpha = missing_label_knivsta(), #0.4,
                size=3)+
      
      geom_text(data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Enköping',],
                # aes(x=c( as.Date('2021-10-11', format='%Y-%m-%d'), as.Date('2021-10-18', format='%Y-%m-%d'),as.Date('2021-10-25', format='%Y-%m-%d')),
                aes(x= as.Date('2022-05-09', format='%Y-%m-%d'),
                    y=0.8),#alpha=0.5,
                label= 'Mätning saknas', angle = 90, color='#f58b0a',
                alpha = missing_label_knivsta(), #0.4,
                size=3)+
      
      geom_text(data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Knivsta',],
                # aes(x=c( as.Date('2021-10-11', format='%Y-%m-%d'), as.Date('2021-10-18', format='%Y-%m-%d'),as.Date('2021-10-25', format='%Y-%m-%d')),
                aes(x= as.Date('2021-10-11', format='%Y-%m-%d'),
                    y=0.8),#alpha=0.5,
                label= 'Mätning saknas', angle = 90, color='#f58b0a',
                alpha = missing_label_knivsta(), #0.4,
                size=3)+
      geom_text(data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Knivsta',],
                # aes(x=c( as.Date('2021-10-11', format='%Y-%m-%d'), as.Date('2021-10-18', format='%Y-%m-%d'),as.Date('2021-10-25', format='%Y-%m-%d')),
                aes(x= as.Date('2021-10-18', format='%Y-%m-%d'),
                    y=0.8),#alpha=0.5,
                label= 'Mätning saknas', angle = 90, color='#f58b0a',
                alpha = missing_label_knivsta(), #0.4,
                size=3)+
      geom_text(data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Knivsta',],
                # aes(x=c( as.Date('2021-10-11', format='%Y-%m-%d'), as.Date('2021-10-18', format='%Y-%m-%d'),as.Date('2021-10-25', format='%Y-%m-%d')),
                aes(x= as.Date('2021-10-25', format='%Y-%m-%d'),
                    y=0.8),#alpha=0.5,
                label= 'Mätning saknas', angle = 90, color='#f58b0a',
                alpha = missing_label_knivsta(), #0.4,
                size=3)+
      
      geom_text(data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Knivsta',],
                # aes(x=c( as.Date('2021-10-11', format='%Y-%m-%d'), as.Date('2021-10-18', format='%Y-%m-%d'),as.Date('2021-10-25', format='%Y-%m-%d')),
                aes(x= as.Date('2021-11-01', format='%Y-%m-%d'),
                    y=0.8),#alpha=0.5,
                label= 'Mätning saknas', angle = 90, color='#f58b0a',
                alpha = missing_label_knivsta(), #0.4,
                size=3)+
      
      geom_text(data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Knivsta',],
                # aes(x=c( as.Date('2021-10-11', format='%Y-%m-%d'), as.Date('2021-10-18', format='%Y-%m-%d'),as.Date('2021-10-25', format='%Y-%m-%d')),
                aes(x= as.Date('2021-11-08', format='%Y-%m-%d'),
                    y=0.8),#alpha=0.5,
                label= 'Mätning saknas', angle = 90, color='#f58b0a',
                alpha = missing_label_knivsta(), #0.4,
                size=3)+
      
      geom_text(data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Knivsta',],
                # aes(x=c( as.Date('2021-10-11', format='%Y-%m-%d'), as.Date('2021-10-18', format='%Y-%m-%d'),as.Date('2021-10-25', format='%Y-%m-%d')),
                aes(x= as.Date('2021-11-15', format='%Y-%m-%d'),
                    y=0.8),#alpha=0.5,
                label= 'Mätning saknas', angle = 90, color='#f58b0a',
                alpha = missing_label_knivsta(), #0.4,
                size=3)+
      
      geom_text(data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Knivsta',],
                # aes(x=c( as.Date('2021-10-11', format='%Y-%m-%d'), as.Date('2021-10-18', format='%Y-%m-%d'),as.Date('2021-10-25', format='%Y-%m-%d')),
                aes(x= as.Date('2021-11-22', format='%Y-%m-%d'),
                    y=0.8),#alpha=0.5,
                label= 'Mätning saknas', angle = 90, color='#f58b0a',
                alpha = missing_label_knivsta(), #0.4,
                size=3)+
      
      geom_text(data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Knivsta',],
                # aes(x=c( as.Date('2021-10-11', format='%Y-%m-%d'), as.Date('2021-10-18', format='%Y-%m-%d'),as.Date('2021-10-25', format='%Y-%m-%d')),
                aes(x= as.Date('2021-11-29', format='%Y-%m-%d'),
                    y=0.8),#alpha=0.5,
                label= 'Mätning saknas', angle = 90, color='#f58b0a',
                alpha = missing_label_knivsta(), #0.4,
                size=3)+
      
      geom_text(data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Knivsta',],
                # aes(x=c( as.Date('2021-10-11', format='%Y-%m-%d'), as.Date('2021-10-18', format='%Y-%m-%d'),as.Date('2021-10-25', format='%Y-%m-%d')),
                aes(x= as.Date('2021-12-06', format='%Y-%m-%d'),
                    y=0.8),#alpha=0.5,
                label= 'Mätning saknas', angle = 90, color='#f58b0a',
                alpha = missing_label_knivsta(), #0.4,
                size=3)+
      
      
      geom_text(data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Knivsta',],
                # aes(x=c( as.Date('2021-10-11', format='%Y-%m-%d'), as.Date('2021-10-18', format='%Y-%m-%d'),as.Date('2021-10-25', format='%Y-%m-%d')),
                aes(x= as.Date('2022-03-07', format='%Y-%m-%d'),
                    y=0.8),#alpha=0.5,
                label= 'Mätning saknas', angle = 90, color='#f58b0a',
                alpha = missing_label_knivsta(), #0.4,
                size=3)+
      
      geom_text(data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Knivsta',],
                # aes(x=c( as.Date('2021-10-11', format='%Y-%m-%d'), as.Date('2021-10-18', format='%Y-%m-%d'),as.Date('2021-10-25', format='%Y-%m-%d')),
                aes(x= as.Date('2022-03-14', format='%Y-%m-%d'),
                    y=0.8),#alpha=0.5,
                label= 'Mätning saknas', angle = 90, color='#f58b0a',
                alpha = missing_label_knivsta(), #0.4,
                size=3)+
      
      geom_text(data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Knivsta',],
                # aes(x=c( as.Date('2021-10-11', format='%Y-%m-%d'), as.Date('2021-10-18', format='%Y-%m-%d'),as.Date('2021-10-25', format='%Y-%m-%d')),
                aes(x= as.Date('2022-03-21', format='%Y-%m-%d'),
                    y=0.8),#alpha=0.5,
                label= 'Mätning saknas', angle = 90, color='#f58b0a',
                alpha = missing_label_knivsta(), #0.4,
                size=3)+
      
      geom_text(data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Knivsta',],
                # aes(x=c( as.Date('2021-10-11', format='%Y-%m-%d'), as.Date('2021-10-18', format='%Y-%m-%d'),as.Date('2021-10-25', format='%Y-%m-%d')),
                aes(x= as.Date('2022-03-28', format='%Y-%m-%d'),
                    y=0.8),#alpha=0.5,
                label= 'Mätning saknas', angle = 90, color='#f58b0a',
                alpha = missing_label_knivsta(), #0.4,
                size=3)+
      
      geom_text(data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Knivsta',],
                # aes(x=c( as.Date('2021-10-11', format='%Y-%m-%d'), as.Date('2021-10-18', format='%Y-%m-%d'),as.Date('2021-10-25', format='%Y-%m-%d')),
                aes(x= as.Date('2022-04-04', format='%Y-%m-%d'),
                    y=0.8),#alpha=0.5,
                label= 'Mätning saknas', angle = 90, color='#f58b0a',
                alpha = missing_label_knivsta(), #0.4,
                size=3)+
      
      geom_text(data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Knivsta',],
                # aes(x=c( as.Date('2021-10-11', format='%Y-%m-%d'), as.Date('2021-10-18', format='%Y-%m-%d'),as.Date('2021-10-25', format='%Y-%m-%d')),
                aes(x= as.Date('2022-04-11', format='%Y-%m-%d'),
                    y=0.8),#alpha=0.5,
                label= 'Mätning saknas', angle = 90, color='#f58b0a',
                alpha = missing_label_knivsta(), #0.4,
                size=3)+
      
      geom_text(data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Knivsta',],
                # aes(x=c( as.Date('2021-10-11', format='%Y-%m-%d'), as.Date('2021-10-18', format='%Y-%m-%d'),as.Date('2021-10-25', format='%Y-%m-%d')),
                aes(x= as.Date('2022-04-18', format='%Y-%m-%d'),
                    y=0.8),#alpha=0.5,
                label= 'Mätning saknas', angle = 90, color='#f58b0a',
                alpha = missing_label_knivsta(), #0.4,
                size=3)+
      
      geom_text(data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Knivsta',],
                # aes(x=c( as.Date('2021-10-11', format='%Y-%m-%d'), as.Date('2021-10-18', format='%Y-%m-%d'),as.Date('2021-10-25', format='%Y-%m-%d')),
                aes(x= as.Date('2022-04-25', format='%Y-%m-%d'),
                    y=0.8),#alpha=0.5,
                label= 'Mätning saknas', angle = 90, color='#f58b0a',
                alpha = missing_label_knivsta(), #0.4,
                size=3)+
      
      geom_text(data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Knivsta',],
                # aes(x=c( as.Date('2021-10-11', format='%Y-%m-%d'), as.Date('2021-10-18', format='%Y-%m-%d'),as.Date('2021-10-25', format='%Y-%m-%d')),
                aes(x= as.Date('2022-05-02', format='%Y-%m-%d'),
                    y=0.8),#alpha=0.5,
                label= 'Mätning saknas', angle = 90, color='#f58b0a',
                alpha = missing_label_knivsta(), #0.4,
                size=3)+
      
      geom_text(data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Knivsta',],
                # aes(x=c( as.Date('2021-10-11', format='%Y-%m-%d'), as.Date('2021-10-18', format='%Y-%m-%d'),as.Date('2021-10-25', format='%Y-%m-%d')),
                aes(x= as.Date('2022-05-09', format='%Y-%m-%d'),
                    y=0.8),#alpha=0.5,
                label= 'Mätning saknas', angle = 90, color='#f58b0a',
                alpha = missing_label_knivsta(), #0.4,
                size=3)+
      
      # # aes(x=c( as.Date('2021-10-11', format='%Y-%m-%d'), as.Date('2021-10-18', format='%Y-%m-%d'),as.Date('2021-10-25', format='%Y-%m-%d')),
      # aes(x= as.Date('2022-01-10', format='%Y-%m-%d'),
      
      geom_text(data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Älvkarleby',],
                # aes(x=c( as.Date('2021-10-11', format='%Y-%m-%d'), as.Date('2021-10-18', format='%Y-%m-%d'),as.Date('2021-10-25', format='%Y-%m-%d')),
                aes(x= as.Date('2021-12-27', format='%Y-%m-%d'),
                    y=0.8),#alpha=0.5,
                label= 'Mätning saknas', angle = 90, color='#76a863',
                alpha = missing_label_alv(), #0.4,
                size=3)+
      
      geom_text(data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Älvkarleby',],
                # aes(x=c( as.Date('2021-10-11', format='%Y-%m-%d'), as.Date('2021-10-18', format='%Y-%m-%d'),as.Date('2021-10-25', format='%Y-%m-%d')),
                aes(x= as.Date('2022-01-03', format='%Y-%m-%d'),
                    y=0.8),#alpha=0.5,
                label= 'Mätning saknas', angle = 90, color='#76a863',
                alpha = missing_label_alv(), #0.4,
                size=3)+
      
      geom_text(data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Älvkarleby',],
                # aes(x=c( as.Date('2021-10-11', format='%Y-%m-%d'), as.Date('2021-10-18', format='%Y-%m-%d'),as.Date('2021-10-25', format='%Y-%m-%d')),
                aes(x= as.Date('2022-01-10', format='%Y-%m-%d'),
                    y=0.8),#alpha=0.5,
                label= 'Mätning saknas', angle = 90, color='#76a863',
                alpha = missing_label_alv(), #0.4,
                size=3)+
      
      geom_text(data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Älvkarleby',],
                # aes(x=c( as.Date('2021-10-11', format='%Y-%m-%d'), as.Date('2021-10-18', format='%Y-%m-%d'),as.Date('2021-10-25', format='%Y-%m-%d')),
                aes(x= as.Date('2022-01-17', format='%Y-%m-%d'),
                    y=0.8),#alpha=0.5,
                label= 'Mätning saknas', angle = 90, color='#76a863',
                alpha = missing_label_alv(), #0.4,
                size=3)+
      
      
      geom_text(data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Östhammar',],
                # aes(x=c( as.Date('2021-10-11', format='%Y-%m-%d'), as.Date('2021-10-18', format='%Y-%m-%d'),as.Date('2021-10-25', format='%Y-%m-%d')),
                aes(x= as.Date('2021-12-27', format='%Y-%m-%d'),
                    y=0.8),#alpha=0.5,
                label= 'Mätning saknas', angle = 90, color='#f58b0a',
                alpha = missing_label_alv(), #0.4,
                size=3)+
      
      geom_text(data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Östhammar',],
                # aes(x=c( as.Date('2021-10-11', format='%Y-%m-%d'), as.Date('2021-10-18', format='%Y-%m-%d'),as.Date('2021-10-25', format='%Y-%m-%d')),
                aes(x= as.Date('2022-01-03', format='%Y-%m-%d'),
                    y=0.8),#alpha=0.5,
                label= 'Mätning saknas', angle = 90, color='#f58b0a',
                alpha = missing_label_alv(), #0.4,
                size=3)+
      
      geom_text(data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Östhammar',],
                # aes(x=c( as.Date('2021-10-11', format='%Y-%m-%d'), as.Date('2021-10-18', format='%Y-%m-%d'),as.Date('2021-10-25', format='%Y-%m-%d')),
                aes(x= as.Date('2022-01-10', format='%Y-%m-%d'),
                    y=0.8),#alpha=0.5,
                label= 'Mätning saknas', angle = 90, color='#f58b0a',
                alpha = missing_label_alv(), #0.4,
                size=3)+
      
      geom_text(data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Östhammar',],
                # aes(x=c( as.Date('2021-10-11', format='%Y-%m-%d'), as.Date('2021-10-18', format='%Y-%m-%d'),as.Date('2021-10-25', format='%Y-%m-%d')),
                aes(x= as.Date('2022-01-17', format='%Y-%m-%d'),
                    y=0.8),#alpha=0.5,
                label= 'Mätning saknas', angle = 90, color='#f58b0a',
                alpha = missing_label_alv(), #0.4,
                size=3)+
      
      scale_x_continuous(limits= zoom_waste_x_Axis(),   # c((most_recent_monday-77),most_recent_monday), #
                         breaks = c(wastewater_All_bind22_old_new_weeks2$monday_dates),
                         labels = wastewater_All_bind22_old_new_weeks2$week_new) +
      
      scale_fill_manual(name="Pump",values=brewer.pal(6, 'Dark2'),
                        breaks=c("Enköping","Knivsta" ,   "Tierp" ,  "Uppsala","Älvkarleby",  'Östhammar'),
                        labels=c("Enköping","Knivsta" ,   "Tierp" ,  "Uppsala","Älvkarleby",  'Östhammar'))+   #    Spectral     values=c('#282a5a', '#ab4641','#dba93b', '#4573a7', '#db843d','#a64d79')) +
      
      scale_y_continuous(
        
        limits= zoom_waste_y_Axis(), #c(0,100),
        trans = scales::pseudo_log_trans(),
        breaks = c(0,0.1,1,2,10,20,100),
        labels=c('','0.1','1','2','10','20','100'))+
      labs(x='Vecka',
           y='SARS-CoV-2 standardiserat för mängd avföring (%-log scale)  '
           
      )+#"Producerad av CRUSH Covid"))+
      theme(#axis.title = element_text(size=16),
        legend.text=element_text(size=15),
        legend.title = element_text(size=15),
        panel.spacing = unit(1, "lines"),
        axis.text.y = element_text(size=15),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=16),
        axis.title.y = element_text(size=16),
        legend.position = "top",
        
        axis.line.y.left = element_line(color = 'gray80',size=1),
        axis.ticks.y.left = element_line(color = 'gray80'),
        
        panel.background = element_rect(fill='#FFFFFF', colour='#FFFFFF'),
        plot.background = element_rect(fill='#FFFFFF', colour='#FFFFFF'),
        legend.key = element_rect(colour = "#FFFFFF", fill = "#FFFFFF"), # gia gurw ap tous kuklous
        legend.background=element_rect(fill = alpha("#FFFFFF", 0.5)),
        
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust=c(0,0.95), face = "bold"))
    
  })
  
  # ---------- ANALYSIS TAB ------------------------------------
  
  # -------------- C A L L S
  
  output$calls_combined <-  renderPlot({
    
    ggplot(data=callsData_all_kommun(),aes(x=yearweek_floor, y= adjusted_freq))+ # , color=kommun
      
      geom_text(#data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Enköping',],
        aes(x=as.Date('2021-11-22', format='%Y-%m-%d'),
            y=25),
        label= 'Mätning saknas', angle = 90, color='#619cff',
        alpha = 0.2,
        size=5)+
      
      geom_text(#data=wastewater_All_bind22_old_new_weeks2[wastewater_All_bind22_old_new_weeks2$Pump=='Enköping',],
        aes(x=as.Date('2022-01-03', format='%Y-%m-%d'),
            y=25),
        label= 'Mätning saknas', angle = 90, color='#619cff',
        alpha = 0.2,
        size=5)+
      
      geom_bar(data=callsData_final_plotting(), aes(y= adjusted_freq_100k*2.5),stat='identity',
               
               alpha = 0.3, color='#619cff',fill='#619cff') + #619cff
      geom_point(data=callsData_all_kommun(),aes(y= adjusted_freq), alpha= 0.1, size=0.8, color='#ff61c3') + # color= 'gray1'
      geom_smooth(method = "loess", span = 0.4, se = FALSE, size=1.4, color='#ff61c3' ) + # , color='#ff61c3'   #ff61c3
      
      scale_x_date(
        limits=c(max(as.Date(week_mondays, format="%Y-%m-%d"))-70,#as.Date('2020-11-18',format="%Y-%m-%d"),
                 as.Date(lastDataUpdate,format="%Y-%m-%d")),  # you want to have one full extra week as the upper limit of your plot so that the barplots of last week can appear!
        breaks = week_mondays,
        labels = paste0(labels_weeks_calls)   ) +
      
      scale_y_continuous(
        limits=c(0,150) ,
        sec.axis = sec_axis(trans=~./2.5, name ='1177 samtal (blå stapeldiagram)')
      ) +
      labs(x='Vecka',
           y='Ambulansutryckningar  (rosa linje)'
      )+
      theme(axis.title = element_text(),panel.spacing = unit(1, "lines"),
            legend.position = "none",
            panel.background = element_rect(fill='#fbebeb', colour='#fbebeb'),
            plot.background = element_rect(fill='#fbebeb', colour='#fbebeb'),
            strip.text.x = element_text(size = 13, face = "bold"), # this is for the Kommun names above each grid
            axis.text.x=element_text(size= 20),  axis.text.y=element_text(size= 20),
            axis.title.x = element_text(size= 20), axis.title.y = element_text(size= 20)
      )#+
    
  })
  
  # -------------- M O B I I L I T Y
  
  output$mobility_plot <-  renderPlot({
    
    ggplot(data=mobility_filter(),aes(x=date))  +
      
      geom_line(aes(y= retail_recreation_GM_sma, color="Shopping och nöje (förändring av antalet besökare)"),  size=1.5, alpha = 1, linetype=2)+#color='darkblue', , color='darkgreen') +  ##0c695d #a81a24   2b9686 #fed766  indianred4  #4203c9
      
      geom_line(aes(y= workplaces_GM_sma, color='Arbetsplatser (förändring av antalet besökare)'),         size=1.4, alpha = 1,linetype=2)+#  WORKSPACE  color='#66a182'
      geom_line(aes(y= residential_GM_sma, color="Bostadsområden (förändring i varaktighet)"),     size=1.6, alpha = 1,linetype=2)+#   #   Time spent at home   color='#edae49'
      
      geom_text(data=workspace_suitcase(),aes(x=date, y= workplaces_GM_sma), label=fontawesome('fa-briefcase'),family="fontawesome-webfont", size=11, color='#4286f5')+   # '#4286f5'   old   # 66a182
      
      geom_text(data=shopping_cart(),aes(x=date, y= retail_recreation_GM_sma), label=fontawesome('fa-shopping-cart'),family="fontawesome-webfont", size=12, color='#dc4437')+   #  '#dc4437'    darkblue
      
      geom_text(data=resident_house(),aes(x=date, y= residential_GM_sma), label=fontawesome('fa-home'),family="fontawesome-webfont", size=12, color='#f5c027')+   # home:   '#f5c027'     old :  edae49
      
      geom_hline(yintercept = 0, color='gray70', size=0.5)+
      
      scale_x_date(date_labels=labelISOweek2, # date_labels = "%W",
                   breaks=c(current_date-70,current_date-63,current_date-56,current_date-49,current_date-42,current_date-35,current_date-28,current_date-21,current_date-14,current_date-7,current_date),
                   limits = c(current_date-77,current_date))+
      
      scale_y_continuous(
        labels=scales::percent_format(accuracy = 1, scale = 1, prefix = "", suffix = "%",  big.mark = " ", decimal.mark = ".", trim = TRUE))+
      
      scale_colour_manual(name="Förändringar gällande (7-dagars glidande medelvärde): ",values=cols_mob) +
      
      labs(x='Vecka',
           y='7-dagars GM av den % förändringen i rörelsemönster' # movement trends over time, # across workplaces
      )+
      theme(
        panel.grid.major.x=element_line(color = 'gray80', linetype = 3),  #   panel.grid.minor.x=element_line(color = 'black'),
        legend.direction = 'vertical',
        # # caption = c('*Ingen cirkel: nödvändig information saknas.', "Producerad av CRUSH Covid"),
        axis.title.x = element_text(size= 16),  axis.title.y = element_text(size= 16),    axis.text = element_text(size=13),
        strip.background = element_blank(),   strip.text.x = element_text(size = 12),
        panel.background = element_rect(fill='white', colour='white'),
        legend.text=element_text(size=15), legend.title = element_text(size=15, face = 'bold'),
        legend.position = "top",
        axis.text.y = element_text(size=12),
        legend.box.background = element_rect(colour = "gray70", linetype = 3),
        legend.key = element_rect(colour = "transparent", fill = "white"), # gia gurw ap tous kuklous
        legend.background=element_rect(fill = alpha("white", 0.5)) )#+
    
  })
  
  # -------------- OM CRUSH
  
  output$ethical <- renderUI({
    str1 <- paste("CRUSH Covid har etiskt tillstånd från Etikprövningsmyndigheten (DNR 2020-04210, 2020-06315 och 2020-06501).")
    str2 <- paste('')
    str3 <- paste("COVID Symptom Study Sverige är en separat forskningsstudie med etiskt tillstånd från Etikprövningsmyndigheten (DNR 2020-01803, 2020-04006, 2020-04145 och 2020-04451).")
    
    HTML(paste(str1, str2, str3, sep = '<br/>'))
  })
  
  output$omCrushEnd <- renderUI({
    str1 <- paste("CRUSH Covid är ett innovativt tvärvetenskapligt forskningsprojekt i samverkan mellan Region Uppsala och forskare från fem olika institutioner vid Uppsala universitet.")
    str2 <- paste('')
    str3 <- paste("Syftet med projektet är att kartlägga och försöka dämpa lokala utbrott av covid-19 i Uppsala län.")
    
    HTML(paste(str1, str2, str3, sep = '<br/>'))
  })
  
  output$leader <- renderUI({
    str1 <- paste(tags$a(href="https://katalog.uu.se/empinfo?id=N9-642", 'Mats Martinell:'))
    str2 <- paste("Universitetslektor vid Institutionen för folkhälso- och vårdvetenskap, Allmänmedicin och Preventivmedicin.",
                  a(actionButton(inputId = "email3", label = "Mejla Mats Martinell",
                                 icon = icon("paper-plane", lib = "font-awesome")),
                    href="mailto:contact@example.org"))
    str3 <- paste("")
    str4 <- paste(tags$a(href="https://katalog.uu.se/empinfo?id=N9-1007", 'Tove Fall:'))
    str5 <- paste("Professor i Molekylär Epidemiologi vid Institutionen för medicinska vetenskaper, Molekylär epidemiologi.",
                  a(actionButton(inputId = "email2", label = "Mejla Tove Fall",
                                 icon = icon("paper-plane", lib = "font-awesome")),
                    href="mailto:contact@example.org"))
    
    HTML(paste( str1, str2, str3, str4, str5, sep = '<br/>'))
  })
  
  output$vinnova <- renderImage({
    filename <- normalizePath(file.path(paste0('images/', 'vinnova', '.png')))
    list(
      src = filename,width= 350, height= 70)  # 540, height= 470)
  }, deleteFile = FALSE)
  
  output$uppsala_vatten <- renderImage({
    filename <- normalizePath(file.path(paste0('images/', 'uppsala_vatten', '.png')))
    list(
      src = filename,width= 300, height= 100)  # 540, height= 470)
  }, deleteFile = FALSE)
  
  output$scilife_lab <- renderImage({
    filename <- normalizePath(file.path(paste0('images/', 'scilife_lab', '.png')))
    list(
      src = filename,width= 300, height= 80)  # 540, height= 470)
  }, deleteFile = FALSE)
  
  # -------------- OBSERVE (Here is the script that allows for interactive change of the maps' colors -------------
  
  # leafletProxy("mapTove") %>%
  # setView(lat = cityData()$lati,
  
  observe({
    leafletProxy("mapCity") %>%
      clearShapes()%>%
      
      addPolygons(group = "Traffic Light",
                  color = '#786865',#'#963f00',#"#b1a6a4",
                  data = weeklyData(),
                  fillColor = ~colorFactor(palette = palette_ECDC(input$ecdc_palette), #c('#666666', '#828282', '#65B32E', '#F2A72E','#b02d04','#730c03'), # new '#b02d04','#730c03'     oldnew :  '#990000', '#3d0101'
                                           levels = c('Dark_Grey','Light_Grey', 'Green', 'Orange','Red', 'DarkRed'),
                                           unique(weeklyData()$ECDC_Colour))(weeklyData()$ECDC_Colour),
                  # "<br>Nya fall (de senaste 14 dagarna): ", round(weeklyData()$notification_rate, digits = 0),
                  # "<br>Testpositivitet (%): ", round(weeklyData()$positivity_rate*100, digits = 2),
                  # "<br>Testnivå: ", round(weeklyData()$testing_rate, digits = 0)),
                  popup = paste0("Stadsdel: ", weeklyData()$stad,
                                 "<br>Nya fall (de senaste 14 dagarna): ", round(weeklyData()$notification_rate, digits = 0),
                                 "<br>Testpositivitet (%): ", round(weeklyData()$positivity_rate*100, digits = 2),
                                 "<br>Testnivå: ", round(weeklyData()$testing_rate, digits = 0)),
                  
                  smoothFactor = 0.2, fillOpacity = 0.8,  weight = 1)
  })
  
  observe({
    leafletProxy("vacc_traffic_map") %>%
      clearShapes()%>%
      
      addPolygons(group = "Traffic Vacc 50 plus",
                  color = "#786865",
                  data = weeklyData_vacc_ecdc(),
                  fillColor = ~colorFactor(palette = palette_ECDC_vaccine(input$ecdc_palette_vacc), #c('#666666', '#828282', '#65B32E', '#F2A72E','#b02d04','#730c03'), # new '#b02d04','#730c03'     oldnew :  '#990000', '#3d0101'
                                           levels = c('Dark_Grey','Light_Grey', 'Green', 'Orange','Red', 'DarkRed'),
                                           # unique(weeklyData_vacc_ecdc()$vacc_color_dose2))(weeklyData_vacc_ecdc()$vacc_color_dose2),
                                           unique(labels_switch_second_dose_traffic_observe()))(labels_switch_second_dose_traffic_observe()),
                  
                  # unique(weeklyData_vacc_ecdc()$labels_switch_second_dose_traffic()))(weeklyData_vacc_ecdc()$labels_switch_second_dose_traffic()),
                  
                  # unique(create_week_data_vacc_ecdc(as.character(max(shape_file_vacc$week2)),'70+')$labels_switch_second_dose_traffic()))(create_week_data_vacc_ecdc(as.character(max(shape_file_vacc$week2)),'70+')$labels_switch_second_dose_traffic()),
                  
                  popup = paste0(#"Postnummer: ", weeklyData_vacc_ecdc()$POSTALCODE,
                    "<br>Stadsdel: ", weeklyData_vacc_ecdc()$stad,
                    # "<br>Cum vaccinated dose 2(n): ", weeklyData_vacc_ecdc()$cum_freq_dose2,
                    labels_switch_second_dose_traffic_bubbles_observe1(),labels_switch_second_dose_traffic_bubbles_observe2(),
                    # labels_switch_second_dose_traffic_bubbles_observe weeklyData_vacc_ecdc()$cum_freq_dose2,
                    "<br>Befolkning (åldersgrupper): ", weeklyData_vacc_ecdc()$population
                    
                    
                  ),
                  
                  smoothFactor = 0.2, fillOpacity = 0.9,  weight = 1)
  })
  
  # -------------- HEATMAP images:
  
  output$heatmap_Kommun_lan <- renderPlot({
    ggplot(create_heatmap_data('Uppsala Län'))+#, #ageweek_All[ageweek_All$kommun=="ENKÖPING",],#heatmapData(),
      # aes(x = week, y = age_cat)) +
      
      geom_point(aes(x = week, y = age_cat,size =test_per_capita5, color = positivity_10))  +
      
      geom_text(aes(x = week, y = age_cat, label = round(cases_per_capita5, digits=0)), nudge_y=0.4, size=5, alpha=0.7)+
      # rename_geom_aes(new_aes = c("size" = "population_emoji"))) +
      
      # rename_geom_aes(new_aes = c("size" = "population_emoji"))) +
      
      scale_size(range = c(3, 14),
                 breaks= c(500, 6000, 12000, 16000),# 11000),#c(500,1500, 3000, 4500, 6000, 8000),
                 limits = c(0,16000))+
      
      scale_color_gradientn(colours = c("#55a6b9","#e7c31e","#f22e0d","red1","darkred"),
                            values = c(0,0.19,0.39,0.40,1), # c(0,0.32,0.42,0.43,1),
                            limits=c(0,100))+
      
      scale_y_discrete(expand = c(0, 0.5))+
      
      scale_x_continuous(limits = c(current_week-9,current_week),
                         breaks= breaks_weeks,# c(40,45,50,54,current_week),
                         labels = paste0(labels_weeks) #c("v.40", "v.45", "v.50",'v.01', current_week_speed)) )+
      )+
      
      theme_classic()+
      labs(
        color= 'Testpositivitet (%): ',
        size= 'Tester per 100 000 invånare:',
        x = ' Vecka',
        y = ' Åldersgrupp'
      )+
      theme(legend.position="bottom", #"bottom"
            legend.key = element_rect(colour = "transparent", fill = "#fbebeb"), # gia gurw ap tous kuklous
            legend.background=element_rect(fill = alpha("#fbebeb", 0.5)), # gia ola to panel tou legend
            
            panel.background = element_rect(fill='#fbebeb', colour='#fbebeb'),
            plot.background = element_rect(fill='#fbebeb', colour='#fbebeb'),
            axis.text.x=element_text(angle = 40, size= 20, vjust = 0.8, hjust=1 ),
            axis.text.y=element_text(size= 20),
            
            legend.key.size = unit(0.8, 'cm'),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 16),
            legend.box = 'vertical',
            axis.title.x = element_text(size= 20),  axis.title.y = element_text(size= 20))
    
  })
  
  output$heatmap_Kommun_enk <- renderPlot({
    ggplot(create_heatmap_data('Enköping'), #ageweek_All[ageweek_All$kommun=="ENKÖPING",],#heatmapData(),
           aes(x = week, y = age_cat)) +
      
      geom_point(aes(size =test_per_capita5, color = positivity_10))  +
      geom_text(aes(x = week, y = age_cat, label = round(cases_per_capita5, digits=0)), nudge_y=0.4, size=5, alpha=0.7)+
      
      scale_size(range = c(3, 14),
                 breaks= c(500, 6000, 12000, 16000),# 11000),#c(500,1500, 3000, 4500, 6000, 8000),
                 limits = c(0,16000))+
      
      scale_color_gradientn(colours = c("#55a6b9","#e7c31e","#f22e0d","red1","darkred"),
                            values = c(0,0.19,0.39,0.40,1), # c(0,0.32,0.42,0.43,1),
                            limits=c(0,100))+
      
      scale_y_discrete(expand = c(0, 0.5))+
      
      scale_x_continuous(limits = c(current_week-9,current_week),
                         breaks= breaks_weeks,# c(40,45,50,54,current_week),
                         labels = paste0(labels_weeks) #c("v.40", "v.45", "v.50",'v.01', current_week_speed)) )+
      )+      #   labels = function(x) round(as.numeric(x), digits=0))+# expand=c(0,0))+
      theme_classic()+
      labs(
        color= 'Testpositivitet (%): ',
        size= 'Tester per 100 000 invånare:',
        x = ' Vecka',
        y = ' Åldersgrupp'
      )+
      theme(legend.position="bottom", #"bottom"
            legend.key = element_rect(colour = "transparent", fill = "#fbebeb"), # gia gurw ap tous kuklous
            legend.background=element_rect(fill = alpha("#fbebeb", 0.5)), # gia ola to panel tou legend
            
            panel.background = element_rect(fill='#fbebeb', colour='#fbebeb'),
            plot.background = element_rect(fill='#fbebeb', colour='#fbebeb'),
            axis.text.x=element_text(angle = 40, size= 20, vjust = 0.8, hjust=1 ),
            axis.text.y=element_text(size= 20),
            legend.key.size = unit(0.8, 'cm'),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 16),
            legend.box = 'vertical',
            axis.title.x = element_text(size= 20),  axis.title.y = element_text(size= 20))
    
  })
  
  output$heatmap_Kommun_upp <- renderPlot({
    ggplot(create_heatmap_data('Uppsala'), #ageweek_All[ageweek_All$kommun=="ENKÖPING",],#heatmapData(),
           aes(x = week, y = age_cat)) +
      
      geom_point(aes(size =test_per_capita5, color = positivity_10))  +
      geom_text(aes(x = week, y = age_cat, label = round(cases_per_capita5, digits=0)), nudge_y=0.4, size=5, alpha=0.7)+
      
      scale_size(range = c(3, 14),
                 breaks= c(500, 6000, 12000, 16000),# 11000),#c(500,1500, 3000, 4500, 6000, 8000),
                 limits = c(0,16000))+
      
      scale_color_gradientn(colours = c("#55a6b9","#e7c31e","#f22e0d","red1","darkred"),
                            values = c(0,0.19,0.39,0.40,1), # c(0,0.32,0.42,0.43,1),
                            limits=c(0,100))+
      
      scale_y_discrete(expand = c(0, 0.5))+
      
      scale_x_continuous(limits = c(current_week-9,current_week),
                         breaks= breaks_weeks,# c(40,45,50,54,current_week),
                         labels = paste0(labels_weeks) #c("v.40", "v.45", "v.50",'v.01', current_week_speed)) )+
      )+      #   labels = function(x) round(as.numeric(x), digits=0))+# expand=c(0,0))+
      theme_classic()+
      labs(
        color= 'Testpositivitet (%): ',
        size= 'Tester per 100 000 invånare:',
        x = ' Vecka',
        y = ' Åldersgrupp'
      )+
      theme(legend.position="bottom", #"bottom"
            legend.key = element_rect(colour = "transparent", fill = "#fbebeb"), # gia gurw ap tous kuklous
            legend.background=element_rect(fill = alpha("#fbebeb", 0.5)), # gia ola to panel tou legend
            
            panel.background = element_rect(fill='#fbebeb', colour='#fbebeb'),
            plot.background = element_rect(fill='#fbebeb', colour='#fbebeb'),
            axis.text.x=element_text(angle = 40, size= 20, vjust = 0.8, hjust=1 ),
            axis.text.y=element_text(size= 20),
            legend.key.size = unit(0.8, 'cm'),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 16),
            legend.box = 'vertical',
            axis.title.x = element_text(size= 20),  axis.title.y = element_text(size= 20))
    
  })
  
  output$heatmap_Kommun_kni <- renderPlot({
    ggplot(create_heatmap_data('Knivsta'), #ageweek_All[ageweek_All$kommun=="ENKÖPING",],#heatmapData(),
           aes(x = week, y = age_cat)) +
      
      geom_point(aes(size =test_per_capita5, color = positivity_10))  +
      geom_text(aes(x = week, y = age_cat, label = round(cases_per_capita5, digits=0)), nudge_y=0.4, size=5, alpha=0.7)+
      
      scale_size(range = c(3, 14),
                 breaks= c(500, 6000, 12000, 16000),# 11000),#c(500,1500, 3000, 4500, 6000, 8000),
                 limits = c(0,16000))+
      
      scale_color_gradientn(colours = c("#55a6b9","#e7c31e","#f22e0d","red1","darkred"),
                            values = c(0,0.19,0.39,0.40,1), # c(0,0.32,0.42,0.43,1),
                            limits=c(0,100))+
      
      scale_y_discrete(expand = c(0, 0.5))+
      
      scale_x_continuous(limits = c(current_week-9,current_week),
                         breaks= breaks_weeks,# c(40,45,50,54,current_week),
                         labels = paste0(labels_weeks) #c("v.40", "v.45", "v.50",'v.01', current_week_speed)) )+
      )+      #   labels = function(x) round(as.numeric(x), digits=0))+# expand=c(0,0))+
      theme_classic()+
      labs(
        color= 'Testpositivitet (%): ',
        size= 'Tester per 100 000 invånare:',
        x = ' Vecka',
        y = ' Åldersgrupp'
      )+
      theme(legend.position="bottom", #"bottom"
            legend.key = element_rect(colour = "transparent", fill = "#fbebeb"), # gia gurw ap tous kuklous
            legend.background=element_rect(fill = alpha("#fbebeb", 0.5)), # gia ola to panel tou legend
            
            panel.background = element_rect(fill='#fbebeb', colour='#fbebeb'),
            plot.background = element_rect(fill='#fbebeb', colour='#fbebeb'),
            axis.text.x=element_text(angle = 40, size= 20, vjust = 0.8, hjust=1 ),
            axis.text.y=element_text(size= 20),
            legend.key.size = unit(0.8, 'cm'),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 16),
            legend.box = 'vertical',
            axis.title.x = element_text(size= 20),  axis.title.y = element_text(size= 20))
    
  })
  
  output$heatmap_Kommun_tie <- renderPlot({
    ggplot(create_heatmap_data('Tierp'), #ageweek_All[ageweek_All$kommun=="ENKÖPING",],#heatmapData(),
           aes(x = week, y = age_cat)) +
      
      geom_point(aes(size =test_per_capita5, color = positivity_10))  +
      geom_text(aes(x = week, y = age_cat, label = round(cases_per_capita5, digits=0)), nudge_y=0.4, size=5, alpha=0.7)+
      
      scale_size(range = c(3, 14),
                 breaks= c(500, 6000, 12000, 16000),# 11000),#c(500,1500, 3000, 4500, 6000, 8000),
                 limits = c(0,16000))+
      
      scale_color_gradientn(colours = c("#55a6b9","#e7c31e","#f22e0d","red1","darkred"),
                            values = c(0,0.19,0.39,0.40,1), # c(0,0.32,0.42,0.43,1),
                            limits=c(0,100))+
      
      scale_y_discrete(expand = c(0, 0.5))+
      
      scale_x_continuous(limits = c(current_week-9,current_week),
                         breaks= breaks_weeks,# c(40,45,50,54,current_week),
                         labels = paste0(labels_weeks) #c("v.40", "v.45", "v.50",'v.01', current_week_speed)) )+
      )+      #   labels = function(x) round(as.numeric(x), digits=0))+# expand=c(0,0))+
      theme_classic()+
      labs(
        color= 'Testpositivitet (%): ',
        size= 'Tester per 100 000 invånare:',
        x = ' Vecka',
        y = ' Åldersgrupp'
      )+
      theme(legend.position="bottom", #"bottom"
            legend.key = element_rect(colour = "transparent", fill = "#fbebeb"), # gia gurw ap tous kuklous
            legend.background=element_rect(fill = alpha("#fbebeb", 0.5)), # gia ola to panel tou legend
            
            panel.background = element_rect(fill='#fbebeb', colour='#fbebeb'),
            plot.background = element_rect(fill='#fbebeb', colour='#fbebeb'),
            axis.text.x=element_text(angle = 40, size= 20, vjust = 0.8, hjust=1 ),
            axis.text.y=element_text(size= 20),
            legend.key.size = unit(0.8, 'cm'),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 16),
            legend.box = 'vertical',
            axis.title.x = element_text(size= 20),  axis.title.y = element_text(size= 20))
    
  })
  
  output$heatmap_Kommun_ost <- renderPlot({
    ggplot(create_heatmap_data('Östhammar'), #ageweek_All[ageweek_All$kommun=="ENKÖPING",],#heatmapData(),
           aes(x = week, y = age_cat)) +
      
      geom_point(aes(size =test_per_capita5, color = positivity_10))  +
      geom_text(aes(x = week, y = age_cat, label = round(cases_per_capita5, digits=0)), nudge_y=0.4, size=5, alpha=0.7)+
      
      scale_size(range = c(3, 14),
                 breaks= c(500, 6000, 12000, 16000),# 11000),#c(500,1500, 3000, 4500, 6000, 8000),
                 limits = c(0,16000))+
      
      scale_color_gradientn(colours = c("#55a6b9","#e7c31e","#f22e0d","red1","darkred"),
                            values = c(0,0.19,0.39,0.40,1), # c(0,0.32,0.42,0.43,1),
                            limits=c(0,100))+
      
      scale_y_discrete(expand = c(0, 0.5))+
      
      scale_x_continuous(limits = c(current_week-9,current_week),
                         breaks= breaks_weeks,# c(40,45,50,54,current_week),
                         labels = paste0(labels_weeks) #c("v.40", "v.45", "v.50",'v.01', current_week_speed)) )+
      )+      #   labels = function(x) round(as.numeric(x), digits=0))+# expand=c(0,0))+
      theme_classic()+
      labs(
        color= 'Testpositivitet (%): ',
        size= 'Tester per 100 000 invånare:',
        x = ' Vecka',
        y = ' Åldersgrupp'
      )+
      theme(legend.position="bottom", #"bottom"
            legend.key = element_rect(colour = "transparent", fill = "#fbebeb"), # gia gurw ap tous kuklous
            legend.background=element_rect(fill = alpha("#fbebeb", 0.5)), # gia ola to panel tou legend
            
            panel.background = element_rect(fill='#fbebeb', colour='#fbebeb'),
            plot.background = element_rect(fill='#fbebeb', colour='#fbebeb'),
            axis.text.x=element_text(angle = 40, size= 20, vjust = 0.8, hjust=1),
            axis.text.y=element_text(size= 20),
            legend.key.size = unit(0.8, 'cm'),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 16),
            legend.box = 'vertical',
            axis.title.x = element_text(size= 20),  axis.title.y = element_text(size= 20))
    
  })
  
  output$heatmap_Kommun_alv <- renderPlot({
    ggplot(create_heatmap_data('Älvkarleby'), #ageweek_All[ageweek_All$kommun=="ENKÖPING",],#heatmapData(),
           aes(x = week, y = age_cat)) +
      
      geom_point(aes(size =test_per_capita5, color = positivity_10))  +
      geom_text(aes(x = week, y = age_cat, label = round(cases_per_capita5, digits=0)), nudge_y=0.4, size=5, alpha=0.7)+
      
      scale_size(range = c(3, 14),
                 breaks= c(500, 6000, 12000, 16000),# 11000),#c(500,1500, 3000, 4500, 6000, 8000),
                 limits = c(0,16000))+
      
      scale_color_gradientn(colours = c("#55a6b9","#e7c31e","#f22e0d","red1","darkred"),
                            values = c(0,0.19,0.39,0.40,1), # c(0,0.32,0.42,0.43,1),
                            limits=c(0,100))+
      
      scale_y_discrete(expand = c(0, 0.5))+
      
      scale_x_continuous(limits = c(current_week-9,current_week),
                         breaks= breaks_weeks,# c(40,45,50,54,current_week),
                         labels = paste0(labels_weeks) #c("v.40", "v.45", "v.50",'v.01', current_week_speed)) )+
      )+      #   labels = function(x) round(as.numeric(x), digits=0))+# expand=c(0,0))+
      theme_classic()+
      labs(
        color= 'Testpositivitet (%): ',
        size= 'Tester per 100 000 invånare:',
        x = ' Vecka',
        y = ' Åldersgrupp'
      )+
      theme(legend.position="bottom", #"bottom"
            legend.key = element_rect(colour = "transparent", fill = "#fbebeb"), # gia gurw ap tous kuklous
            legend.background=element_rect(fill = alpha("#fbebeb", 0.5)), # gia ola to panel tou legend
            
            panel.background = element_rect(fill='#fbebeb', colour='#fbebeb'),
            plot.background = element_rect(fill='#fbebeb', colour='#fbebeb'),
            axis.text.x=element_text(angle = 40, size= 20, vjust = 0.8, hjust=1),
            axis.text.y=element_text(size= 20),
            legend.key.size = unit(0.8, 'cm'),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 16),
            legend.box = 'vertical',
            axis.title.x = element_text(size= 20),  axis.title.y = element_text(size= 20))
    
  })
  
  output$heatmap_Kommun_heb <- renderPlot({
    ggplot(create_heatmap_data('Heby'), #ageweek_All[ageweek_All$kommun=="ENKÖPING",],#heatmapData(),
           aes(x = week, y = age_cat)) +
      
      geom_point(aes(size =test_per_capita5, color = positivity_10))  +
      geom_text(aes(x = week, y = age_cat, label = round(cases_per_capita5, digits=0)), nudge_y=0.4, size=5, alpha=0.7)+
      
      scale_size(range = c(3, 14),
                 breaks= c(500, 6000, 12000, 16000),# 11000),#c(500,1500, 3000, 4500, 6000, 8000),
                 limits = c(0,16000))+
      
      scale_color_gradientn(colours = c("#55a6b9","#e7c31e","#f22e0d","red1","darkred"),
                            values = c(0,0.19,0.39,0.40,1), # c(0,0.32,0.42,0.43,1),
                            limits=c(0,100))+
      
      scale_y_discrete(expand = c(0, 0.5))+
      
      scale_x_continuous(limits = c(current_week-9,current_week),
                         breaks= breaks_weeks,# c(40,45,50,54,current_week),
                         labels = paste0(labels_weeks) #c("v.40", "v.45", "v.50",'v.01', current_week_speed)) )+
      )+      #   labels = function(x) round(as.numeric(x), digits=0))+# expand=c(0,0))+
      theme_classic()+
      labs(
        color= 'Testpositivitet (%): ',
        size= 'Tester per 100 000 invånare:',
        x = ' Vecka',
        y = ' Åldersgrupp'
      )+
      theme(legend.position="bottom", #"bottom"
            legend.key = element_rect(colour = "transparent", fill = "#fbebeb"), # gia gurw ap tous kuklous
            legend.background=element_rect(fill = alpha("#fbebeb", 0.5)), # gia ola to panel tou legend
            
            panel.background = element_rect(fill='#fbebeb', colour='#fbebeb'),
            plot.background = element_rect(fill='#fbebeb', colour='#fbebeb'),
            axis.text.x=element_text(angle = 40, size= 20, vjust = 0.8, hjust=1),
            axis.text.y=element_text(size= 20),
            legend.key.size = unit(0.8, 'cm'),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 16),
            legend.box = 'vertical',
            axis.title.x = element_text(size= 20),  axis.title.y = element_text(size= 20))
    
  })
  
  output$heatmap_Kommun_hab <- renderPlot({
    ggplot(create_heatmap_data('Håbo'), #ageweek_All[ageweek_All$kommun=="ENKÖPING",],#heatmapData(),
           aes(x = week, y = age_cat)) +
      
      geom_point(aes(size =test_per_capita5, color = positivity_10))  +
      geom_text(aes(x = week, y = age_cat, label = round(cases_per_capita5, digits=0)), nudge_y=0.4, size=5, alpha=0.7)+
      
      scale_size(range = c(3, 14),
                 breaks= c(500, 6000, 12000, 16000),# 11000),#c(500,1500, 3000, 4500, 6000, 8000),
                 limits = c(0,16000))+
      
      scale_color_gradientn(colours = c("#55a6b9","#e7c31e","#f22e0d","red1","darkred"),
                            values = c(0,0.19,0.39,0.40,1), # c(0,0.32,0.42,0.43,1),
                            limits=c(0,100))+
      
      scale_y_discrete(expand = c(0, 0.5))+
      
      scale_x_continuous(limits = c(current_week-9,current_week),
                         breaks= breaks_weeks,# c(40,45,50,54,current_week),
                         labels = paste0(labels_weeks) #c("v.40", "v.45", "v.50",'v.01', current_week_speed)) )+
      )+      #   labels = function(x) round(as.numeric(x), digits=0))+# expand=c(0,0))+
      theme_classic()+
      labs(
        color= 'Testpositivitet (%): ',
        size= 'Tester per 100 000 invånare:',
        x = ' Vecka',
        y = ' Åldersgrupp'
      )+
      theme(legend.position="bottom", #"bottom"
            legend.key = element_rect(colour = "transparent", fill = "#fbebeb"), # gia gurw ap tous kuklous
            legend.background=element_rect(fill = alpha("#fbebeb", 0.5)), # gia ola to panel tou legend
            
            panel.background = element_rect(fill='#fbebeb', colour='#fbebeb'),
            plot.background = element_rect(fill='#fbebeb', colour='#fbebeb'),
            axis.text.x=element_text(angle = 40, size= 20, vjust = 0.8, hjust=1),
            axis.text.y=element_text(size= 20),
            legend.key.size = unit(0.8, 'cm'),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 16),
            legend.box = 'vertical',
            axis.title.x = element_text(size= 20),  axis.title.y = element_text(size= 20))
    
  })
  
  
  cols <- c("Nya fall" = "#ca4905",#'Nya fall (hela bef.)' = "#8f3100",
            'Symtomatisk (%)' = "#de8e09", "Testpositivitet (%)" = "#252326",
            'Tester' ='#009e73','Utryckningar (112)'='#c72cbd','Samtal (1177)'='#878787', 'Symtomatisk (%) ' = 'darkred',
            'Antal sjukhusinlagda'='#0a5da1', "Rörelsemönster i shopping och nöje"= '#dc4437', 'Rörelsemönster i arbetsplatser'='#4286f5',
            "Rörelsemönster i bostadsområden"='#f5c027') ##ede135  #48a4e3 'Calls 112'='#ede135','Calls 1177'='#48a4e3',
  
  cols_order <-  factor(cols, ordered = TRUE,
                        levels = c("Nya fall", #'Nya fall (hela bef.)',
                                   'Symtomatisk (%)', "Testpositivitet (%)", 'Tester','Utryckningar (112)','Samtal (1177)',
                                   "Rörelsemönster i shopping och nöje", 'Rörelsemönster i arbetsplatser',
                                   "Rörelsemönster i bostadsområden"))
  
  output$multi_plot_upp <-  renderPlot({
    
    ggplot(data=uppsala_all_weeks_multi_Kommun,aes(x=week), order=cols_order) + # week   uppsala_svt
      
      geom_vline(xintercept = 73, linetype= 2, size=2, color='grey74', alpha = 0.5 )+ # positivity y axis line
      
      # Positivity
      geom_line(data=uppsala_all_weeks_multi_Kommun, aes(y= check_pos(input$checkbox_uppsala)*0.0002*1666.66, # 500/0.3
                                                         color= 'Testpositivitet (%)'
      ),#colour="darkblue",
      size=1.5, alpha = 1,linetype=2)+ #,color= "darkblue" )+
      
      # Cases per capita
      geom_line(data=uppsala_all_weeks_multi_Kommun,aes(y= check_notif(input$checkbox_uppsala)*0.0002, # 1.7e+12/500
                                                        color= 'Nya fall'
      ),#colour='#d55e00',
      size=1.5, alpha = 1, linetype=1)+ #,color= c('Nya fall'="#d55e00")) + # 'darkred'  , colour= '#d55e00'
      
      # Hospital
      geom_line(data=uppsala_multi_hospit_Kommun, aes(y= check_hospit(input$checkbox_uppsala)*0.0002*500*3, # 500/0.3   *0.0008*500, # 500/0.3
                                                      color= 'Antal sjukhusinlagda'
      ),#colour="darkblue",
      size=1.5, alpha = 1,linetype=1)+ #,color= "darkblue" )+
      
      # CSSS indicator
      geom_line(data=csss_75_average_w,aes(y= check_Uppskattning(input$checkbox_uppsala)*0.0002*625,  # 500/1.5
                                           color= 'Symtomatisk (%)'
      ),#"#009e73",
      
      size=1.6, alpha = 1, linetype=2)+#,color='#009e73') +  # '#009e73'
      
      # Tester
      geom_line(data=uppsala_all_weeks_multi_Kommun, aes(y= check_test(input$checkbox_uppsala)*0.0002*0.125,
                                                         color= 'Tester'
      ),#colour="darkblue",
      size=1.5, alpha = 1,linetype=1)+ #,color= "darkblue" )+
      
      # # 112
      geom_line(data=all_kommun_lan2_upp, aes(y= check_calls112(input$checkbox_uppsala)*0.0002*7,
                                              color= 'Utryckningar (112)'
      ), size=1.5, alpha = 1,linetype=3)+ #,color= "darkblue" )
      
      # 1177 final_for_plotting2_upp$freq_totalPop
      geom_line(data=final_for_plotting2_upp, aes(y= check_calls1177(input$checkbox_uppsala)*0.0002*14,
                                                  color= 'Samtal (1177)'
      ), size=1.5, alpha = 1,linetype=1)+ #,color= "darkblue" )
      
      # # Google mobility
      geom_line(data=mobility_weekly_avg_lis3_uppsala, aes(y= check_mobility_Retail(input$checkbox_uppsala)*(0.0002*25*0.4)+ lift_Google_plot_up(input$checkbox_uppsala), color="Rörelsemönster i shopping och nöje"),  size=1, alpha = 1, linetype=3)+#color='darkblue', , color='darkgreen') +  ##0c695d #a81a24   2b9686 #fed766  indianred4  #4203c9
      # disappear_down(input$checkbox_uppsala)+geom_line(data=mobility_weekly_avg_lis3_uppsala, aes(y= check_mobility_Retail(input$checkbox_uppsala)*(0.0002*25*0.3)+16*0.0002*25, color="Visitors in retail & recreation"), size=1, alpha = 1, linetype=4)+#color='darkblue', , color='darkgreen') + ##0c695d #a81a24 2b9686 #fed766 indianred4 #4203c9
      
      geom_line(data=mobility_weekly_avg_lis3_uppsala, aes(y= check_mobility_Work(input$checkbox_uppsala)*(0.0002*25*0.3)+ lift_Google_plot_up(input$checkbox_uppsala), color='Rörelsemönster i arbetsplatser'),         size=1, alpha = 1,linetype=3)+#   color='#66a182'
      geom_line(data=mobility_weekly_avg_lis3_uppsala, aes(y= check_mobility_Resid(input$checkbox_uppsala)*0.0002*25, color="Rörelsemönster i bostadsområden"),     size=1, alpha = 1,linetype=3)+#   #  color='#edae49'   "Rörelsemönster i shopping och nöje", 'Rörelsemönster i arbetsplatser',
      
      # check_mobility_Retail check_mobility_Work check_mobility_Resid
      # do a summary of the mobility_weekly_avg_lis3_uppsala$retail_recreation_GM_avg and take the minimum value, multiply it by two and that is what you add on the check_mobility_Resid(input$checkbox_uppsala)*0.0002*25+70. But if you just add 70, that is nothing on the water scale. so you need to add70
      
      # range(wastewater_pumps2_multiplot$week)
      # Sewage barplots:
      geom_bar(data=wastewater_pumps2_multiplot,
               mapping=aes(fill=Pump, y=check_virus_copies(input$checkbox_uppsala)),  # y=RR_N1copesPPMoV
               position="dodge", stat="identity", alpha=1)+
      # Missing values of barplots:
      
      geom_text(x=53, y=0.04, label=missing_water(input$checkbox_uppsala), angle = 90, color='black',size=3)+ # #7cb5fc
      
      geom_text(x=52, y=0.04, label=missing_water(input$checkbox_uppsala), angle = 90, color='black',size=3)+
      
      geom_text(x=50, y=0.04, label=missing_water(input$checkbox_uppsala), angle = 90, color='black',size=3)+
      
      scale_x_continuous(limits = c(34,current_week+2), # add 2 extra weeks after the week that your data stop
                         expand=c(0,0),
                         breaks= week_number_year_break, # c(35:current_week),   # (25, 30, 40,45,50,54,57),
                         labels = paste0(week_number_year))+ #paste0(c('week 25',"week 40", "week 45", "week 50", "week 01", "week 04")))+
      
      scale_y_continuous(limits=c(0,0.6),  #   0.4/500=  0.0008    .   old calculations: 1.7e+12/500 = 0.0008   for upper notification of 500 # 1.736105e+12
                         expand=c(0,0),
                         labels=scales::percent_format(
                           accuracy = 1, scale = 100, prefix = "", suffix = "%",
                           big.mark = " ", decimal.mark = ".", trim = TRUE
                         ),
                         sec.axis = sec_axis(trans=~./0.0002, name ='')) +  # Nya fall per 100 000 invånare per vecka (mörkröd linje)
      labs(x='Vecka',
           colors='Line',
           y='SARS-CoV-2 i avloppsvatten (stapeldiagram)')+   #  (stapeldiagram
      
      # Colors for pump in the sewage barplto
      scale_fill_manual(values = c('#c8b6fe'))+
      
      scale_colour_manual(name="Linje",values=cols) +
      
      guides(fill=label_water(input$checkbox_uppsala), colour =label_color_lintype(input$checkbox_uppsala))+
      
      geom_text(x=check_csss_57.7(input$checkbox_uppsala), y=1.45*0.0002*625, label="1.5%", angle = 0, color='#de8e09', alpha = 1, size=5)+ # the upper limit is actuallt 1.5, but I write 1.4 otherwise the '1.5' label does nto appear on the plot
      
      geom_text(x=check_csss_57.7(input$checkbox_uppsala), y=1*0.0002*625, label="1%", angle = 0, color='#de8e09', alpha = 1, size=5)+
      
      geom_text(x=check_csss_57.7(input$checkbox_uppsala), y=0.75*0.0002*625, label="0.75%", angle = 0, color='#de8e09', alpha = 1, size=5)+
      
      geom_text(x=check_csss_57.7(input$checkbox_uppsala), y=0.5*0.0002*625, label="0.5%", angle = 0, color='#de8e09', alpha = 1, size=5)+
      
      geom_text(x=check_csss_57.7(input$checkbox_uppsala), y=0.25*0.0002*625, label="0.25%", angle = 0, color='#de8e09', alpha = 1, size=5)+
      
      geom_text(x=check_csss_57.7(input$checkbox_uppsala), y=0.018*0.0002*625, label="0%", angle = 0, color='#de8e09', alpha = 1, size=5)+
      
      geom_vline(xintercept = check_csss_58(input$checkbox_uppsala), linetype=2, size=1.1, color='#de8e09', alpha = 1 )+ # positivity y axis line
      
      geom_text(x=check_test_57.7(input$checkbox_uppsala), y=3950*0.0002*0.125, label="4000", angle = 0, color='#009e73', alpha = 1, size=5)+ # the upper limit is actuallt 4000, but I write 3950 otherwise the '4000' label does nto appear on the plot
      
      geom_text(x=check_test_57.7(input$checkbox_uppsala), y=3000*0.0002*0.125, label="3000", angle = 0, color='#009e73', alpha = 1, size=5)+
      
      geom_text(x=check_test_57.7(input$checkbox_uppsala), y=2000*0.0002*0.125, label="2000", angle = 0, color='#009e73', alpha = 1, size=5)+
      
      geom_text(x=check_test_57.7(input$checkbox_uppsala), y=1000*0.0002*0.125, label="1000", angle = 0, color='#009e73', alpha = 1, size=5)+
      
      geom_text(x=check_test_57.7(input$checkbox_uppsala), y=100*0.0002*0.125, label="0", angle = 0, color='#009e73', alpha = 1, size=5)+
      
      geom_vline(xintercept = check_test_58(input$checkbox_uppsala), linetype=1, size=1.1, color='#009e73', alpha = 1 )+ # positivity y axis line
      
      geom_text(x=check_hospit_57.7(input$checkbox_uppsala), y=0.1*0.0002*500*3, label="0.1", angle = 0, color='#0a5da1', alpha = 1, size=5)+
      
      geom_text(x=check_hospit_57.7(input$checkbox_uppsala), y=0.2*0.0002*500*3, label="0.2", angle = 0, color='#0a5da1', alpha = 1, size=5)+
      
      geom_text(x=check_hospit_57.7(input$checkbox_uppsala), y=0.4*0.0002*500*3, label="0.4", angle = 0, color='#0a5da1', alpha = 1, size=5)+
      
      geom_text(x=check_hospit_57.7(input$checkbox_uppsala), y=0.009*0.0002*500*3, label="0", angle = 0, color='#0a5da1', alpha = 1, size=5)+
      
      geom_vline(xintercept = check_hospit_58(input$checkbox_uppsala), linetype=1, size=1.1, color='#0a5da1', alpha = 1 )+ # positivity y axis line
      
      geom_text(x=check_pos_57.7(input$checkbox_uppsala), y=0.295*0.0002*1666.66, label="30%", angle = 0, color='#252326', alpha = 1, size=5)+ # the upper limit is actuallt 0.3
      
      geom_text(x=check_pos_57.7(input$checkbox_uppsala), y=0.225*0.0002*1666.66, label="22.5%", angle = 0, color='#252326', alpha = 1, size=5)+
      
      geom_text(x=check_pos_57.7(input$checkbox_uppsala), y=0.15*0.0002*1666.66, label="15%", angle = 0, color='#252326', alpha = 1, size=5)+
      
      geom_text(x=check_pos_57.7(input$checkbox_uppsala), y=0.075*0.0002*1666.66, label="7.5%", angle = 0, color='#252326', alpha = 1, size=5)+
      
      geom_text(x=check_pos_57.7(input$checkbox_uppsala), y=0.009*0.0002*1666.66, label="0%", angle = 0, color='#252326', alpha = 1, size=5)+
      
      geom_vline(xintercept = check_pos_58(input$checkbox_uppsala), linetype=2, size=1.1, color='#252326', alpha = 1 )+ # positivity y axis line
      
      geom_text(x=check_calls112_57.7(input$checkbox_uppsala), y=70*0.0002*7, label="70", angle = 0, color='#c72cbd', alpha = 1, size=5)+
      
      geom_text(x=check_calls112_57.7(input$checkbox_uppsala), y=50*0.0002*7, label="50", angle = 0, color='#c72cbd', alpha = 1, size=5)+
      
      geom_text(x=check_calls112_57.7(input$checkbox_uppsala), y=30*0.0002*7, label="30", angle = 0, color='#c72cbd', alpha = 1, size=5)+
      
      geom_text(x=check_calls112_57.7(input$checkbox_uppsala), y=10*0.0002*7, label="10", angle = 0, color='#c72cbd', alpha = 1, size=5)+
      
      geom_vline(xintercept = check_calls112_58(input$checkbox_uppsala), linetype=3, size=1.1, color='#c72cbd', alpha = 1 )+ # positivity y axis line
      
      geom_text(x=check_calls1177_57.7(input$checkbox_uppsala), y=35*0.0002*14, label="35", angle = 0, color='#878787', alpha = 1, size=5)+
      
      geom_text(x=check_calls1177_57.7(input$checkbox_uppsala), y=25*0.0002*14, label="25", angle = 0, color='#878787', alpha = 1, size=5)+
      
      geom_text(x=check_calls1177_57.7(input$checkbox_uppsala), y=15*0.0002*14, label="15", angle = 0, color='#878787', alpha = 1, size=5)+
      
      geom_text(x=check_calls1177_57.7(input$checkbox_uppsala), y=5*0.0002*14, label="5", angle = 0, color='#878787', alpha = 1, size=5)+
      
      geom_vline(xintercept = check_calls1177_58(input$checkbox_uppsala), linetype=1, size=1.1, color='#878787', alpha = 1 )+ # positivity y axis line
      
      # ## GOOGLE WORK - BLUE
      geom_text(x=check_mobility_57.7(input$checkbox_uppsala)+0.9, y=-2*0.0002*25*0.3+16*0.0002*25,  label="0%", angle = 0, color='#4286f5', alpha = 1, size=5)+
      
      geom_text(x=check_mobility_57.7(input$checkbox_uppsala)+0.9, y=-25*0.0002*25*0.3+16*0.0002*25,  label="-25%", angle = 0, color='#4286f5', alpha = 1, size=5)+ #  label="25",
      
      geom_text(x=check_mobility_57.7(input$checkbox_uppsala)+0.9, y=-45*0.0002*25*0.3+16*0.0002*25, label="-45%", angle = 0, color='#4286f5', alpha = 1, size=5)+
      
      geom_vline(xintercept = check_mobility_58(input$checkbox_uppsala)+0.2, linetype=4, size=1.1, color='black', alpha = 1 )+ # positivity y axis line
      
      # ## GOOOGLE RESIDENCE - YELLOW
      
      geom_text(x=check_mobility_57.7(input$checkbox_uppsala)+0.9, y=18*0.0002*25, label="+18%", angle = 0, color='#f5c027', alpha = 1, size=5)+
      
      geom_text(x=check_mobility_57.7(input$checkbox_uppsala)+0.9, y=12*0.0002*25, label="+12%", angle = 0, color='#f5c027', alpha = 1, size=5)+
      
      geom_text(x=check_mobility_57.7(input$checkbox_uppsala)+0.9, y=6*0.0002*25, label="+6%", angle = 0, color='#f5c027', alpha = 1, size=5)+
      
      # ## GOOGLE RETAIL -- RED
      
      geom_text(x=check_mobility_57.7(input$checkbox_uppsala)+0.9, y=-0*0.0002*25*0.4+16*0.0002*25, label="0%", angle = 0, color='#dc4437', alpha = 1, size=5)+
      
      geom_text(x=check_mobility_57.7(input$checkbox_uppsala)+0.9, y=-15*0.0002*25*0.4+16*0.0002*25, label="-15%", angle = 0, color='#dc4437', alpha = 1, size=5)+
      
      geom_text(x=check_mobility_57.7(input$checkbox_uppsala)+0.9, y=-30*0.0002*25*0.4+16*0.0002*25, label="-30%", angle = 0, color='#dc4437', alpha = 1, size=5)+
      
      theme_clean()+
      # theme_fivethirtyeight() +
      theme(#axis.title = element_text(size=17),
        plot.subtitle = element_text(size=16),
        
        panel.spacing = unit(1, "lines"),
        
        axis.text.y = element_text(size=14),
        axis.text.x = element_text(angle = 40, vjust = 0.5, hjust=1, size=14),
        
        axis.title.x = element_text(size=16, margin = margin(t = 15, r = 20, b = 0, l = 0), face = "bold"),
        axis.title.y = element_text(size=14, face = "bold"),
        
        legend.position = "bottom",
        legend.direction = 'vertical',
        
        legend.text=element_text(size=15),
        legend.title = element_blank(),
        
        legend.key = element_rect(colour = "transparent", fill = "#fbebeb"), # gia gurw ap tous kuklous
        legend.background=element_rect(fill = alpha("#fbebeb", 0.5)), # gia ola to panel tou legend
        
        panel.background = element_rect(fill='#fbebeb', colour='#fbebeb'),
        plot.background = element_rect(fill='#fbebeb', colour='#fbebeb'),
        
        axis.line.y.right = element_line(color = notific_color_axis(input$checkbox_uppsala),size=1),
        axis.ticks.y.right = element_line(color = notific_color_axis(input$checkbox_uppsala)),
        axis.text.y.right = element_text(color = notific_color_axis(input$checkbox_uppsala)),
        axis.title.y.right = element_text(color = notific_color_axis(input$checkbox_uppsala)),
        
        axis.line.y.left = element_line(color = sewage_color_axis(input$checkbox_uppsala),size=1),
        axis.ticks.y.left = element_line(color = sewage_color_axis(input$checkbox_uppsala)),
        axis.text.y.left = element_text(color = sewage_color_axis(input$checkbox_uppsala)),
        axis.title.y.left = element_text(color = sewage_color_axis(input$checkbox_uppsala)),
        
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
      )
    
  })
  
  output$multi_plot_lan <-  renderPlot({
    
    ggplot(data=uppsala_all_weeks_multi_Lan,aes(x=week)) + # week   uppsala_svt
      
      geom_vline(xintercept = 73, linetype= 2, size=2, color='grey74', alpha = 0.5 )+ # positivity y axis line
      
      # Sewage barplots:
      geom_bar(data=wastewater_pumps2_multiplot, mapping=aes(fill=Pump, y=check_virus_copies_lan(input$checkbox_uppsala)),position="dodge", stat="identity")+  #wastewater_pumps3$virus_copies check_virus_copies_lan(input$checkbox_Lan)
      
      # Positivity
      geom_line(data=uppsala_all_weeks_multi_Lan, aes(y= check_pos_lan(input$checkbox_uppsala)*0.0002*1666.66, # 500/0.3
                                                      color= 'Testpositivitet (%)'
      ),#colour="darkblue",
      size=1.5, alpha = 1,linetype=2)+ #,color= "darkblue" )+
      
      # Hospital
      geom_line(data=uppsala_multi_hospit_Lan, aes(y= check_hospit_lan(input$checkbox_uppsala)*0.0002*500*3, # 500/0.3
                                                   color= 'Antal sjukhusinlagda'
      ),#colour="darkblue",
      size=1.5, alpha = 1,linetype=1)+ #,color= "darkblue" )+
      
      # Cases per capita
      geom_line(data=uppsala_all_weeks_multi_Lan,aes(y= check_notif_lan(input$checkbox_uppsala)*0.0002, # 1.7e+12/500
                                                     color= 'Nya fall'
      ),#colour='#d55e00',
      size=1.5, alpha = 1, linetype=1)+ #,color= c('Nya fall'="#d55e00")) + # 'darkred'  , colour= '#d55e00'
      
      # # Cases per capita entire
      
      # CSSS indicator
      geom_line(data=csss_lan_average_w,aes(y= check_Uppskattning_lan(input$checkbox_uppsala)*0.0002*625,  # new: 500/0.8=625          old: 500/1.5
                                            color= 'Symtomatisk (%)'
      ),#"#009e73",
      
      size=1.6, alpha = 1, linetype=2)+#,color='#009e73') +  # '#009e73'
      
      # Tester
      geom_line(data=uppsala_all_weeks_multi_Lan, aes(y= check_test_lan(input$checkbox_uppsala)*0.0002*0.125,
                                                      color= 'Tester'
      ),#colour="darkblue",
      size=1.5, alpha = 1,linetype=1)+ #,color= "darkblue" )+
      
      # ## 112
      geom_line(data=all_kommun_lan2_lan, aes(y= check_calls112_lan(input$checkbox_uppsala)*0.0002*7,  # 500/100=5
                                              color= 'Utryckningar (112)'
      ), size=1.5, alpha = 1,linetype=3)+ #,color= "darkblue" )
      
      # ## 1177 final_for_plotting2_upp$freq_totalPop
      geom_line(data=final_for_plotting2_lan, aes(y= check_calls1177_lan(input$checkbox_uppsala)*0.0002*14,  # 500/100=5
                                                  color= 'Samtal (1177)'
      ), size=1.5, alpha = 1,linetype=1)+ #,color= "darkblue" )
      
      # Google mobility
      geom_line(data=mobility_weekly_avg_lis3_lan, aes(y= check_mobility_lan_Retail(input$checkbox_uppsala)*(0.0002*25*0.3)+ lift_Google_plot_up(input$checkbox_uppsala), color="Rörelsemönster i shopping och nöje"),  size=1, alpha = 1, linetype=3)+#color='darkblue', , color='darkgreen') +  ##0c695d #a81a24   2b9686 #fed766  indianred4  #4203c9
      geom_line(data=mobility_weekly_avg_lis3_lan, aes(y= check_mobility_lan_Work(input$checkbox_uppsala)*(0.0002*25*0.3)+ lift_Google_plot_up(input$checkbox_uppsala), color='Rörelsemönster i arbetsplatser'),         size=1, alpha = 1,linetype=3)+#   color='#66a182'
      geom_line(data=mobility_weekly_avg_lis3_lan, aes(y= check_mobility_lan_Resid(input$checkbox_uppsala)*0.0002*25, color="Rörelsemönster i bostadsområden"),     size=1, alpha = 1,linetype=3)+#   #  color='#edae49'   "Rörelsemönster i shopping och nöje", 'Rörelsemönster i arbetsplatser',
      
      scale_x_continuous(limits = c(34,current_week+2), # add 2 extra weeks after the week that your data stop
                         expand=c(0,0),
                         breaks= week_number_year_break, # c(35:current_week),   # (25, 30, 40,45,50,54,57),
                         labels = paste0(week_number_year))+ #paste0(c('week 25',"week 40", "week 45", "week 50", "week 01", "week 04")))+
      
      scale_y_continuous(limits=c(0,0.6),  #    1.7e+12/500 = 0.0008   for upper notification of 500 # 1.736105e+12
                         expand=c(0,0),
                         sec.axis = sec_axis(trans=~./0.0002, name ='')) +  # Nya fall per 100 000 invånare per vecka (mörkröd linje)
      labs(x='Vecka',
           colors='Line',
           y='Mängd SARS-CoV-2 virus korrigerat för mängd feces (stapeldiagram)')+   #  (stapeldiagram
      
      # Colors for pump in the sewage barplto
      scale_fill_manual(values = c("#FFFFFF", "#FFFFFF"))+   # #7cb5fc = ciel for C
      
      scale_colour_manual(name="Linje",values=cols) +
      
      guides(fill='none', colour =label_color_lintype_lan(input$checkbox_uppsala))+
      
      geom_text(x=check_csss_57.7_lan(input$checkbox_uppsala), y=1.45*0.0002*625, label="1.5%", angle = 0, color='#de8e09', alpha = 1, size=5)+ # the upper limit is actuallt 1.5, but I write 1.4 otherwise the '1.5' label does nto appear on the plot
      
      geom_text(x=check_csss_57.7_lan(input$checkbox_uppsala), y=1*0.0002*625, label="1%", angle = 0, color='#de8e09', alpha = 1, size=5)+
      
      geom_text(x=check_csss_57.7_lan(input$checkbox_uppsala), y=0.75*0.0002*625, label="0.75%", angle = 0, color='#de8e09', alpha = 1, size=5)+
      
      geom_text(x=check_csss_57.7_lan(input$checkbox_uppsala), y=0.5*0.0002*625, label="0.5%", angle = 0, color='#de8e09', alpha = 1, size=5)+
      
      geom_text(x=check_csss_57.7_lan(input$checkbox_uppsala), y=0.25*0.0002*625, label="0.25%", angle = 0, color='#de8e09', alpha = 1, size=5)+
      
      geom_text(x=check_csss_57.7_lan(input$checkbox_uppsala), y=0.018*0.0002*625, label="0%", angle = 0, color='#de8e09', alpha = 1, size=5)+
      
      geom_vline(xintercept = check_csss_58_lan(input$checkbox_uppsala), linetype=2, size=1.1, color='#de8e09', alpha = 1 )+ # positivity y axis line
      
      geom_text(x=check_test_57.7_lan(input$checkbox_uppsala), y=3950*0.0002*0.125, label="4000", angle = 0, color='#009e73', alpha = 1, size=5)+ # the upper limit is actuallt 4000, but I write 3950 otherwise the '4000' label does nto appear on the plot
      
      geom_text(x=check_test_57.7_lan(input$checkbox_uppsala), y=3000*0.0002*0.125, label="3000", angle = 0, color='#009e73', alpha = 1, size=5)+
      
      geom_text(x=check_test_57.7_lan(input$checkbox_uppsala), y=2000*0.0002*0.125, label="2000", angle = 0, color='#009e73', alpha = 1, size=5)+
      
      geom_text(x=check_test_57.7_lan(input$checkbox_uppsala), y=1000*0.0002*0.125, label="1000", angle = 0, color='#009e73', alpha = 1, size=5)+
      
      geom_text(x=check_test_57.7_lan(input$checkbox_uppsala), y=100*0.0002*0.125, label="0", angle = 0, color='#009e73', alpha = 1, size=5)+
      
      geom_vline(xintercept = check_test_58_lan(input$checkbox_uppsala), linetype=1, size=1.1, color='#009e73', alpha = 1 )+ # positivity y axis line
      
      geom_text(x=check_hospit_57.7_lan(input$checkbox_uppsala), y=0.1*0.0002*500*3, label="0.1", angle = 0, color='#0a5da1', alpha = 1, size=5)+
      
      geom_text(x=check_hospit_57.7_lan(input$checkbox_uppsala), y=0.2*0.0002*500*3, label="0.2", angle = 0, color='#0a5da1', alpha = 1, size=5)+
      
      geom_text(x=check_hospit_57.7_lan(input$checkbox_uppsala), y=0.4*0.0002*500*3, label="0.4", angle = 0, color='#0a5da1', alpha = 1, size=5)+
      
      geom_text(x=check_hospit_57.7_lan(input$checkbox_uppsala), y=0.009*0.0002*500*3, label="0", angle = 0, color='#0a5da1', alpha = 1, size=5)+
      
      geom_vline(xintercept = check_hospit_58_lan(input$checkbox_uppsala), linetype=1, size=1.1, color='#0a5da1', alpha = 1 )+ # positivity y axis line
      
      geom_text(x=check_pos_57.7_lan(input$checkbox_uppsala), y=0.295*0.0002*1666.66, label="30%", angle = 0, color='#252326', alpha = 1, size=5)+ # the upper limit is actuallt 0.3
      
      geom_text(x=check_pos_57.7_lan(input$checkbox_uppsala), y=0.225*0.0002*1666.66, label="22.5%", angle = 0, color='#252326', alpha = 1, size=5)+
      
      geom_text(x=check_pos_57.7_lan(input$checkbox_uppsala), y=0.15*0.0002*1666.66, label="15%", angle = 0, color='#252326', alpha = 1, size=5)+
      
      geom_text(x=check_pos_57.7_lan(input$checkbox_uppsala), y=0.075*0.0002*1666.66, label="7.5%", angle = 0, color='#252326', alpha = 1, size=5)+
      
      geom_text(x=check_pos_57.7_lan(input$checkbox_uppsala), y=0.009*0.0002*1666.66, label="0%", angle = 0, color='#252326', alpha = 1, size=5)+
      
      geom_vline(xintercept = check_pos_58_lan(input$checkbox_uppsala), linetype=2, size=1.1, color='#252326', alpha = 1 )+ # positivity y axis line
      
      geom_text(x=check_calls112_57.7_lan(input$checkbox_uppsala), y=70*0.0002*7, label="70", angle = 0, color='#c72cbd', alpha = 1, size=5)+
      
      geom_text(x=check_calls112_57.7_lan(input$checkbox_uppsala), y=50*0.0002*7, label="50", angle = 0, color='#c72cbd', alpha = 1, size=5)+
      
      geom_text(x=check_calls112_57.7_lan(input$checkbox_uppsala), y=30*0.0002*7, label="30", angle = 0, color='#c72cbd', alpha = 1, size=5)+
      
      geom_text(x=check_calls112_57.7_lan(input$checkbox_uppsala), y=10*0.0002*7, label="10", angle = 0, color='#c72cbd', alpha = 1, size=5)+
      
      geom_vline(xintercept = check_calls112_58_lan(input$checkbox_uppsala), linetype=3, size=1.1, color='#c72cbd', alpha = 1 )+ # positivity y axis line
      
      geom_text(x=check_calls1177_57.7_lan(input$checkbox_uppsala), y=35*0.0002*14, label="35", angle = 0, color='#878787', alpha = 1, size=5)+
      
      geom_text(x=check_calls1177_57.7_lan(input$checkbox_uppsala), y=25*0.0002*14, label="25", angle = 0, color='#878787', alpha = 1, size=5)+
      
      geom_text(x=check_calls1177_57.7_lan(input$checkbox_uppsala), y=15*0.0002*14, label="15", angle = 0, color='#878787', alpha = 1, size=5)+
      
      geom_text(x=check_calls1177_57.7_lan(input$checkbox_uppsala), y=5*0.0002*14, label="5", angle = 0, color='#878787', alpha = 1, size=5)+
      
      geom_vline(xintercept = check_calls1177_58_lan(input$checkbox_uppsala), linetype=1, size=1.1, color='#878787', alpha = 1 )+ # positivity y axis line
      
      # ## GOOGLE WORK - BLUE
      geom_text(x=check_mobility_lan_57.7(input$checkbox_uppsala)+0.9, y=-2*0.0002*25*0.3+16*0.0002*25,  label="0%", angle = 0, color='#4286f5', alpha = 1, size=5)+
      
      geom_text(x=check_mobility_lan_57.7(input$checkbox_uppsala)+0.9, y=-25*0.0002*25*0.3+16*0.0002*25,  label="-25%", angle = 0, color='#4286f5', alpha = 1, size=5)+ #  label="25",
      
      geom_text(x=check_mobility_lan_57.7(input$checkbox_uppsala)+0.9, y=-45*0.0002*25*0.3+16*0.0002*25, label="-45%", angle = 0, color='#4286f5', alpha = 1, size=5)+
      
      geom_vline(xintercept = check_mobility_lan_58(input$checkbox_uppsala)+0.2, linetype=4, size=1.1, color='black', alpha = 1 )+ # positivity y axis line
      
      # ## GOOOGLE RESIDENCE - YELLOW
      
      geom_text(x=check_mobility_lan_57.7(input$checkbox_uppsala)+0.9, y=18*0.0002*25, label="+18%", angle = 0, color='#f5c027', alpha = 1, size=5)+
      
      geom_text(x=check_mobility_lan_57.7(input$checkbox_uppsala)+0.9, y=12*0.0002*25, label="+12%", angle = 0, color='#f5c027', alpha = 1, size=5)+
      
      geom_text(x=check_mobility_lan_57.7(input$checkbox_uppsala)+0.9, y=6*0.0002*25, label="+6%", angle = 0, color='#f5c027', alpha = 1, size=5)+
      
      # ## GOOGLE RETAIL -- RED
      
      geom_text(x=check_mobility_lan_57.7(input$checkbox_uppsala)+0.9, y=-0*0.0002*25*0.4+16*0.0002*25, label="0%", angle = 0, color='#dc4437', alpha = 1, size=5)+
      
      geom_text(x=check_mobility_lan_57.7(input$checkbox_uppsala)+0.9, y=-15*0.0002*25*0.4+16*0.0002*25, label="-15%", angle = 0, color='#dc4437', alpha = 1, size=5)+
      
      geom_text(x=check_mobility_lan_57.7(input$checkbox_uppsala)+0.9, y=-30*0.0002*25*0.4+16*0.0002*25, label="-30%", angle = 0, color='#dc4437', alpha = 1, size=5)+
      
      theme_clean()+
      # theme_fivethirtyeight() +
      theme(#axis.title = element_text(size=17),
        plot.subtitle = element_text(size=16),
        
        panel.spacing = unit(1, "lines"),
        
        axis.text.y = element_text(size=14),
        axis.text.x = element_text(angle = 40, vjust = 0.5, hjust=1, size=14),
        
        axis.title.x = element_text(size=16, margin = margin(t = 15, r = 20, b = 0, l = 0), face = "bold"),
        axis.title.y = element_text(size=16, face = "bold"),
        
        legend.position = "bottom",
        legend.direction = 'vertical',
        legend.text=element_text(size=15),
        legend.title = element_blank(),
        
        legend.key = element_rect(colour = "transparent", fill = "#fbebeb"), # gia gurw ap tous kuklous
        legend.background=element_rect(fill = alpha("#fbebeb", 0.5)), # gia ola to panel tou legend
        
        panel.background = element_rect(fill='#fbebeb', colour='#fbebeb'),
        plot.background = element_rect(fill='#fbebeb', colour='#fbebeb'),
        
        axis.line.y.right = element_line(color = notific_color_axis_lan(input$checkbox_uppsala),size=1),
        axis.ticks.y.right = element_line(color = notific_color_axis_lan(input$checkbox_uppsala)),
        axis.text.y.right = element_text(color = notific_color_axis_lan(input$checkbox_uppsala)),
        axis.title.y.right = element_text(color = notific_color_axis_lan(input$checkbox_uppsala)),
        
        axis.line.y.left = element_line(color = "#fbebeb"),#sewage_color_axis_lan(input$checkbox_Lan),size=1),
        axis.ticks.y.left = element_line(color = "#fbebeb"),#sewage_color_axis_lan(input$checkbox_Lan)),
        axis.text.y.left = element_text(color = "#fbebeb"),#sewage_color_axis_lan(input$checkbox_Lan)),
        axis.title.y.left = element_text(color = "#fbebeb"),#sewage_color_axis_lan(input$checkbox_Lan)),
        
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
      )
    
  })
  
  output$multi_text <- renderUI({
    str0 <- paste(icon('hand-pointer', class='glow'),'Vi rekommenderar att endast välja ett fåtal datakällor per visning för bättre överblick.')
    str1 <- paste("")
    str2 <- paste(icon('exclamation-circle', class='glow'), 'Nya fall, tester och testpositivitet per 100 000 invånare anges för populationen 15 år och äldre fram till v. 20, därefter för hela befolkningen.') # Den population inkluderar endast individer 15 år och äldre.
    str5 <- paste('')
    str6 <- paste(icon = icon('exclamation-circle', class="glow"), 'Inga data för 1177 samtal finns tillgängliga före vecka 48.')
    
    str7 <- paste("")
    str8 <- paste(icon('exclamation-circle', class='glow')," Mätningar av SARS-CoV-2 i avloppsvatten är endast tillgängligt för Uppsala Kommun.")
    
    HTML(paste(str0, str1, str2, str5, str6, str7, str8, sep = '<br/>')) # , str3, str4, str5, str6, str7,  str6, str7, str8, str9,
    
  })
  
}




# ***********************************      RUN THE APP    *************************************** #
# ****************************************************************************************** #
# Let's run the app now:
shinyApp(ui, server) # , options = list(height = 1080)
