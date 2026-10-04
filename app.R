# ============================================================
# INSTAGRAM ANALYTICS
# Big Data Analytics Project
# Interactive R Shiny Dashboard
# ============================================================


# ============================================================
# 1. REQUIRED PACKAGES
# ============================================================

required_packages <- c(
  "shiny",
  "bslib",
  "dplyr",
  "readr",
  "lubridate",
  "plotly",
  "DT",
  "scales"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(
    missing_packages,
    dependencies = TRUE
  )
}

library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(lubridate)
library(plotly)
library(DT)
library(scales)


# ============================================================
# 2. LOAD DATA
# ============================================================

DATA_FILE <- "Instagram_Analytics.csv"

if (!file.exists(DATA_FILE)) {
  
  stop(
    paste0(
      "\nInstagram_Analytics.csv was not found.\n\n",
      "Make sure the file is in the same folder as app.R.\n\n",
      "Expected structure:\n",
      "Instagram_Analytics_Big_Data/\n",
      "  app.R\n",
      "  Instagram_Analytics.csv\n",
      "  www/\n",
      "    instagram-logo.png\n"
    )
  )
}

instagram <- read_csv(
  DATA_FILE,
  show_col_types = FALSE
)


# ============================================================
# 3. CHECK REQUIRED COLUMNS
# ============================================================

required_columns <- c(
  "post_id",
  "account_id",
  "account_type",
  "follower_count",
  "media_type",
  "content_category",
  "traffic_source",
  "has_call_to_action",
  "post_datetime",
  "post_date",
  "post_hour",
  "day_of_week",
  "likes",
  "comments",
  "shares",
  "saves",
  "reach",
  "impressions",
  "engagement_rate",
  "followers_gained",
  "caption_length",
  "hashtags_count",
  "performance_bucket_label"
)

missing_columns <- setdiff(
  required_columns,
  names(instagram)
)

if (length(missing_columns) > 0) {
  
  stop(
    paste0(
      "The following required columns are missing:\n",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  )
}


# ============================================================
# 4. DATA CLEANING
# ============================================================

numeric_columns <- c(
  "follower_count",
  "post_hour",
  "likes",
  "comments",
  "shares",
  "saves",
  "reach",
  "impressions",
  "engagement_rate",
  "followers_gained",
  "caption_length",
  "hashtags_count"
)

instagram <- instagram %>%
  mutate(
    across(
      all_of(numeric_columns),
      ~ suppressWarnings(
        as.numeric(.x)
      )
    )
  )


# ------------------------------------------------------------
# Date conversion
# ------------------------------------------------------------

instagram$post_date <- suppressWarnings(
  as.Date(
    instagram$post_date
  )
)

# If post_date failed, try post_datetime
if (all(is.na(instagram$post_date))) {
  
  instagram$post_date <- as.Date(
    suppressWarnings(
      parse_date_time(
        instagram$post_datetime,
        orders = c(
          "ymd HMS",
          "ymd HM",
          "mdy HMS",
          "mdy HM",
          "dmy HMS",
          "dmy HM"
        ),
        quiet = TRUE
      )
    )
  )
}


# ------------------------------------------------------------
# Character columns
# ------------------------------------------------------------

character_columns <- c(
  "account_type",
  "media_type",
  "content_category",
  "traffic_source",
  "day_of_week",
  "performance_bucket_label"
)

instagram <- instagram %>%
  mutate(
    across(
      all_of(character_columns),
      as.character
    )
  )


# ============================================================
# 5. DERIVED ANALYTICS
# ============================================================

instagram <- instagram %>%
  mutate(
    
    total_engagement =
      coalesce(likes, 0) +
      coalesce(comments, 0) +
      coalesce(shares, 0) +
      coalesce(saves, 0),
    
    engagement_per_reach =
      if_else(
        !is.na(reach) &
          reach > 0,
        total_engagement / reach,
        NA_real_
      ),
    
    save_rate =
      if_else(
        !is.na(reach) &
          reach > 0,
        saves / reach,
        NA_real_
      ),
    
    share_rate =
      if_else(
        !is.na(reach) &
          reach > 0,
        shares / reach,
        NA_real_
      ),
    
    comment_rate =
      if_else(
        !is.na(reach) &
          reach > 0,
        comments / reach,
        NA_real_
      ),
    
    follower_conversion =
      if_else(
        !is.na(reach) &
          reach > 0,
        followers_gained / reach,
        NA_real_
      )
  )


# ============================================================
# 6. HELPER FUNCTIONS
# ============================================================

safe_unique <- function(x) {
  
  x <- as.character(x)
  
  x <- x[
    !is.na(x) &
      trimws(x) != ""
  ]
  
  sort(
    unique(x)
  )
}


clean_label <- function(x) {
  
  x <- as.character(x)
  
  x[is.na(x)] <- ""
  
  x <- gsub(
    "_",
    " ",
    x
  )
  
  x <- gsub(
    "-",
    " ",
    x
  )
  
  tools::toTitleCase(x)
}


# ------------------------------------------------------------
# Scalar compact formatter
# ------------------------------------------------------------

fmt_compact <- function(x) {
  
  if (
    length(x) == 0 ||
    is.na(x) ||
    !is.finite(x)
  ) {
    return("0")
  }
  
  if (x >= 1000000000) {
    
    return(
      paste0(
        format(
          round(
            x / 1000000000,
            1
          ),
          trim = TRUE,
          scientific = FALSE
        ),
        "B"
      )
    )
  }
  
  if (x >= 1000000) {
    
    return(
      paste0(
        format(
          round(
            x / 1000000,
            1
          ),
          trim = TRUE,
          scientific = FALSE
        ),
        "M"
      )
    )
  }
  
  if (x >= 1000) {
    
    return(
      paste0(
        format(
          round(
            x / 1000,
            1
          ),
          trim = TRUE,
          scientific = FALSE
        ),
        "K"
      )
    )
  }
  
  comma(
    round(x)
  )
}


# ------------------------------------------------------------
# Scalar percentage formatter
# ------------------------------------------------------------

fmt_percent <- function(
    x,
    digits = 2
) {
  
  if (
    length(x) == 0 ||
    is.na(x) ||
    !is.finite(x)
  ) {
    return("0%")
  }
  
  paste0(
    formatC(
      x * 100,
      format = "f",
      digits = digits
    ),
    "%"
  )
}


# ------------------------------------------------------------
# VECTOR-SAFE percentage formatter
# This is important for Plotly.
# ------------------------------------------------------------

fmt_percent_vec <- function(
    x,
    digits = 2
) {
  
  result <- rep(
    "0%",
    length(x)
  )
  
  valid <- !is.na(x) &
    is.finite(x)
  
  if (any(valid)) {
    
    result[valid] <- paste0(
      formatC(
        x[valid] * 100,
        format = "f",
        digits = digits
      ),
      "%"
    )
  }
  
  result
}


# ------------------------------------------------------------
# Safe mean
# ------------------------------------------------------------

safe_mean <- function(x) {
  
  value <- mean(
    x,
    na.rm = TRUE
  )
  
  if (
    length(value) == 0 ||
    is.nan(value) ||
    !is.finite(value)
  ) {
    
    return(0)
  }
  
  value
}


# ------------------------------------------------------------
# Empty plot message
# ------------------------------------------------------------

empty_plot <- function(
    message = "No data available"
) {
  
  plot_ly() %>%
    
    layout(
      
      xaxis = list(
        visible = FALSE
      ),
      
      yaxis = list(
        visible = FALSE
      ),
      
      annotations = list(
        list(
          text = message,
          x = 0.5,
          y = 0.5,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(
            size = 14,
            color = "#98A2B3"
          )
        )
      ),
      
      paper_bgcolor = "rgba(0,0,0,0)",
      
      plot_bgcolor = "rgba(0,0,0,0)"
    )
}


# ============================================================
# 7. THEME
# ============================================================

theme <- bs_theme(
  
  version = 5,
  
  bg = "#F5F6FA",
  
  fg = "#172033",
  
  primary = "#7C3AED",
  
  secondary = "#64748B",
  
  success = "#10B981",
  
  info = "#0EA5E9",
  
  warning = "#F59E0B",
  
  danger = "#EF4444",
  
  border_radius = "14px",
  
  font_scale = 0.94
)


# ============================================================
# 8. USER INTERFACE
# ============================================================

ui <- page_fluid(
  
  theme = theme,
  
  # ==========================================================
  # HEAD
  # ==========================================================
  
  tags$head(
    
    tags$title(
      "Instagram Analytics | Big Data Analytics"
    ),
    
    tags$link(
      rel = "preconnect",
      href = "https://fonts.googleapis.com"
    ),
    
    tags$link(
      rel = "preconnect",
      href = "https://fonts.gstatic.com",
      crossorigin = "anonymous"
    ),
    
    tags$link(
      href =
        "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&family=Plus+Jakarta+Sans:wght@600;700;800&display=swap",
      rel = "stylesheet"
    ),
    
    tags$style(
      HTML(
        
        "
        /* ===================================================
           GLOBAL
        =================================================== */

        html,
        body {

          background: #F5F6FA !important;

          color: #172033;

          font-family: 'Inter', sans-serif;

          margin: 0;

          padding: 0;
        }

        body {

          min-height: 100vh;

          overflow-x: hidden;
        }

        .container-fluid {

          padding-left: 30px !important;

          padding-right: 30px !important;
        }

        * {

          box-sizing: border-box;
        }


        /* ===================================================
           HERO
        =================================================== */

        .hero {

          position: relative;

          overflow: hidden;

          margin:
            0 -30px 20px -30px;

          padding:
            30px 34px 28px 34px;

          background:
            linear-gradient(
              115deg,
              #16142A 0%,
              #241940 47%,
              #3A1459 100%
            );

          color: white;

          border-radius:
            0 0 24px 24px;

          box-shadow:
            0 12px 32px
            rgba(25, 18, 48, 0.14);
        }

        .hero::before {

          content: '';

          position: absolute;

          width: 500px;

          height: 500px;

          right: -190px;

          top: -280px;

          border-radius: 50%;

          background:
            radial-gradient(
              circle,
              rgba(236,72,153,0.22),
              transparent 67%
            );
        }

        .hero::after {

          content: '';

          position: absolute;

          width: 300px;

          height: 300px;

          left: 40%;

          bottom: -260px;

          border-radius: 50%;

          background:
            radial-gradient(
              circle,
              rgba(124,58,237,0.24),
              transparent 68%
            );
        }

        .hero-inner {

          position: relative;

          z-index: 2;

          display: flex;

          align-items: center;

          justify-content: space-between;

          gap: 25px;
        }

        .brand-area {

          display: flex;

          align-items: center;

          gap: 16px;
        }

        .brand-logo {

          width: 54px;

          height: 54px;

          object-fit: contain;

          padding: 7px;

          border-radius: 15px;

          background:
            rgba(255,255,255,0.09);

          border:
            1px solid
            rgba(255,255,255,0.16);
        }

        .ig-fallback {

          width: 54px;

          height: 54px;

          display: flex;

          align-items: center;

          justify-content: center;

          border-radius: 15px;

          background:
            linear-gradient(
              135deg,
              #F58529,
              #DD2A7B,
              #8134AF
            );

          color: white;

          font-size: 28px;

          font-weight: 700;
        }

        .eyebrow {

          margin: 0 0 5px 0;

          color:
            rgba(255,255,255,0.58);

          font-size: 10px;

          font-weight: 800;

          letter-spacing: 0.16em;

          text-transform: uppercase;
        }

        .hero-title {

          margin: 0;

          font-family:
            'Plus Jakarta Sans',
            'Inter',
            sans-serif;

          font-size: 29px;

          line-height: 1.15;

          font-weight: 800;

          letter-spacing: -0.04em;
        }

        .hero-subtitle {

          margin: 8px 0 0 0;

          max-width: 680px;

          color:
            rgba(255,255,255,0.68);

          font-size: 12px;

          line-height: 1.55;
        }

        .hero-meta {

          display: flex;

          align-items: center;

          gap: 8px;

          white-space: nowrap;
        }

        .status-pill {

          display: inline-flex;

          align-items: center;

          gap: 7px;

          padding:
            9px 12px;

          border-radius: 999px;

          background:
            rgba(255,255,255,0.08);

          border:
            1px solid
            rgba(255,255,255,0.12);

          color:
            rgba(255,255,255,0.83);

          font-size: 10px;

          font-weight: 700;
        }

        .status-dot {

          width: 7px;

          height: 7px;

          border-radius: 50%;

          background: #34D399;

          box-shadow:
            0 0 0 4px
            rgba(52,211,153,0.12);
        }


        /* ===================================================
           FILTERS
        =================================================== */

        .filter-panel {

          background: #FFFFFF;

          border:
            1px solid #E7E9EF;

          border-radius: 17px;

          padding:
            16px 17px 15px 17px;

          margin-bottom: 18px;

          box-shadow:
            0 5px 18px
            rgba(30,41,59,0.045);
        }

        .filter-header {

          display: flex;

          justify-content: space-between;

          align-items: center;

          margin-bottom: 12px;
        }

        .filter-title {

          font-size: 12px;

          font-weight: 800;

          color: #202A3B;
        }

        .filter-caption {

          color: #98A2B3;

          font-size: 10px;
        }

        .filter-row {

          display: grid;

          grid-template-columns:
            1fr
            1fr
            1.3fr
            1.1fr
            1fr
            1.3fr
            auto;

          gap: 10px;

          align-items: end;
        }

        .filter-item label {

          color: #64748B !important;

          font-size: 9px !important;

          font-weight: 800 !important;

          letter-spacing: 0.07em;

          text-transform: uppercase;
        }

        .form-control,
        .selectize-input {

          min-height: 38px !important;

          border:
            1px solid #E0E4EB !important;

          border-radius:
            10px !important;

          background:
            #FAFBFC !important;

          color: #263246 !important;

          box-shadow: none !important;

          font-size: 11px !important;
        }

        .selectize-input.focus {

          border-color:
            #9B7BEA !important;

          box-shadow:
            0 0 0 3px
            rgba(124,58,237,0.08)
            !important;
        }

        .selectize-dropdown {

          border:
            1px solid #E2E5EC !important;

          border-radius:
            10px !important;

          box-shadow:
            0 12px 30px
            rgba(15,23,42,0.12)
            !important;

          font-size: 11px !important;

          z-index: 9999 !important;
        }

        .selectize-dropdown
        .option.active {

          background:
            #F1ECFF !important;

          color:
            #6D28D9 !important;
        }

        .date-input {

          width: 100%;
        }


        /* ===================================================
           BUTTONS
        =================================================== */

        .action-btn {

          min-height: 38px !important;

          padding:
            0 13px !important;

          border-radius:
            10px !important;

          border:
            1px solid #E0E4EB !important;

          background:
            #FFFFFF !important;

          color:
            #344054 !important;

          font-size:
            10px !important;

          font-weight:
            800 !important;

          transition:
            all .16s ease;
        }

        .action-btn:hover {

          transform:
            translateY(-1px);

          border-color:
            #B7A2E7 !important;

          box-shadow:
            0 5px 15px
            rgba(124,58,237,0.10);
        }

        .download-btn {

          background:
            #201B37 !important;

          color:
            white !important;

          border-color:
            #201B37 !important;
        }

        .download-btn:hover {

          background:
            #322B51 !important;

          color: white !important;
        }


        /* ===================================================
           KPI CARDS
        =================================================== */

        .kpi-grid {

          display: grid;

          grid-template-columns:
            repeat(6, minmax(0, 1fr));

          gap: 12px;

          margin-bottom: 19px;
        }

        .kpi-card {

          position: relative;

          min-height: 115px;

          padding:
            16px 16px 14px 17px;

          overflow: hidden;

          background: #FFFFFF;

          border:
            1px solid #E7E9EF;

          border-radius: 16px;

          box-shadow:
            0 5px 17px
            rgba(30,41,59,0.04);

          transition:
            transform .18s ease,
            box-shadow .18s ease;
        }

        .kpi-card:hover {

          transform:
            translateY(-2px);

          box-shadow:
            0 11px 25px
            rgba(30,41,59,0.075);
        }

        .kpi-card::before {

          content: '';

          position: absolute;

          top: 0;

          left: 0;

          width: 4px;

          height: 100%;

          background: #7C3AED;
        }

        .kpi-card.orange::before {
          background: #F97316;
        }

        .kpi-card.pink::before {
          background: #EC4899;
        }

        .kpi-card.blue::before {
          background: #0EA5E9;
        }

        .kpi-card.green::before {
          background: #10B981;
        }

        .kpi-card.amber::before {
          background: #F59E0B;
        }

        .kpi-label {

          color: #7A8699;

          font-size: 9px;

          font-weight: 800;

          letter-spacing: .08em;

          text-transform: uppercase;
        }

        .kpi-value {

          margin-top: 8px;

          color: #1D2738;

          font-family:
            'Plus Jakarta Sans',
            'Inter',
            sans-serif;

          font-size: 24px;

          line-height: 1;

          font-weight: 800;

          letter-spacing: -0.04em;
        }

        .kpi-note {

          margin-top: 9px;

          color: #98A2B3;

          font-size: 9px;

          line-height: 1.35;
        }


        /* ===================================================
           SECTION HEADERS
        =================================================== */

        .section-heading {

          display: flex;

          align-items: center;

          justify-content: space-between;

          margin:
            25px 2px 10px 2px;
        }

        .section-heading h3 {

          margin: 0;

          color: #202A3B;

          font-family:
            'Plus Jakarta Sans',
            'Inter',
            sans-serif;

          font-size: 14px;

          font-weight: 800;

          letter-spacing: -0.025em;
        }

        .section-heading span {

          color: #98A2B3;

          font-size: 9px;
        }


        /* ===================================================
           CHART CARDS
        =================================================== */

        .chart-grid {

          display: grid;

          grid-template-columns:
            minmax(0, 1.55fr)
            minmax(300px, .85fr);

          gap: 14px;

          margin-bottom: 14px;
        }

        .chart-grid-equal {

          display: grid;

          grid-template-columns:
            repeat(2, minmax(0, 1fr));

          gap: 14px;

          margin-bottom: 14px;
        }

        .chart-card {

          min-width: 0;

          background: #FFFFFF;

          border:
            1px solid #E7E9EF;

          border-radius: 17px;

          padding:
            14px 15px 9px 15px;

          box-shadow:
            0 5px 18px
            rgba(30,41,59,0.04);
        }

        .chart-card.full {

          margin-bottom: 14px;
        }

        .chart-card-header {

          display: flex;

          align-items: center;

          justify-content: space-between;

          gap: 15px;

          margin-bottom: 4px;
        }

        .chart-card-title {

          color: #273246;

          font-size: 11px;

          font-weight: 800;
        }

        .chart-card-subtitle {

          margin-top: 3px;

          color: #98A2B3;

          font-size: 9px;
        }

        .chart-control {

          width: 170px;
        }

        .chart-control .selectize-input {

          min-height:
            32px !important;

          padding:
            7px 10px !important;

          font-size:
            10px !important;
        }


        /* ===================================================
           INSIGHTS
        =================================================== */

        .insight-panel {

          margin-bottom: 15px;

          padding:
            18px 19px;

          background:
            linear-gradient(
              135deg,
              #201B37,
              #2D2450
            );

          color: white;

          border-radius: 17px;

          box-shadow:
            0 8px 22px
            rgba(30,27,53,0.12);
        }

        .insight-title {

          display: flex;

          align-items: center;

          gap: 8px;

          margin-bottom: 12px;

          font-size: 11px;

          font-weight: 800;
        }

        .insight-icon {

          width: 27px;

          height: 27px;

          display: flex;

          align-items: center;

          justify-content: center;

          border-radius: 8px;

          background:
            rgba(255,255,255,0.10);

          font-size: 13px;
        }

        .insight-grid {

          display: grid;

          grid-template-columns:
            repeat(3, 1fr);

          gap: 9px;
        }

        .insight-item {

          padding:
            12px 13px;

          background:
            rgba(255,255,255,0.055);

          border:
            1px solid
            rgba(255,255,255,0.08);

          border-radius: 11px;
        }

        .insight-item strong {

          display: block;

          margin-bottom: 5px;

          color: #FFFFFF;

          font-size: 10px;
        }

        .insight-item span {

          display: block;

          color:
            rgba(255,255,255,0.61);

          font-size: 9px;

          line-height: 1.5;
        }


        /* ===================================================
           TABLE
        =================================================== */

        .table-card {

          margin-bottom: 20px;

          padding: 15px;

          background: #FFFFFF;

          border:
            1px solid #E7E9EF;

          border-radius: 17px;

          box-shadow:
            0 5px 18px
            rgba(30,41,59,0.04);
        }

        .dataTables_wrapper {

          font-size: 10px;
        }

        .dataTables_wrapper
        .dataTables_filter input {

          padding:
            6px 9px !important;

          border:
            1px solid #E0E4EB !important;

          border-radius:
            8px !important;

          outline: none !important;
        }

        table.dataTable
        thead th {

          padding:
            10px !important;

          background:
            #F8F9FC !important;

          color:
            #667085 !important;

          border-bottom:
            1px solid #E6E8ED !important;

          font-size:
            9px !important;

          letter-spacing:
            .05em;

          text-transform:
            uppercase;
        }

        table.dataTable
        tbody td {

          padding:
            9px 10px !important;

          color:
            #344054;

          font-size:
            9px;

          border-bottom:
            1px solid #F0F1F4 !important;
        }

        table.dataTable
        tbody tr:hover {

          background:
            #FAF8FF !important;
        }


        /* ===================================================
           FOOTER
        =================================================== */

        .footer {

          display: flex;

          align-items: center;

          justify-content: space-between;

          margin:
            7px -30px 0 -30px;

          padding:
            16px 30px;

          background: #FFFFFF;

          border-top:
            1px solid #E5E7EB;

          color: #98A2B3;

          font-size: 9px;
        }

        .footer strong {

          color: #667085;
        }


        /* ===================================================
           RESPONSIVE
        =================================================== */

        @media (max-width: 1250px) {

          .kpi-grid {

            grid-template-columns:
              repeat(3, 1fr);
          }

          .filter-row {

            grid-template-columns:
              repeat(3, 1fr);
          }

          .chart-grid {

            grid-template-columns:
              1fr;
          }
        }


        @media (max-width: 850px) {

          .container-fluid {

            padding-left:
              16px !important;

            padding-right:
              16px !important;
          }

          .hero {

            margin-left:
              -16px;

            margin-right:
              -16px;

            padding:
              25px 20px;
          }

          .hero-inner {

            align-items:
              flex-start;

            flex-direction:
              column;
          }

          .hero-title {

            font-size:
              23px;
          }

          .hero-meta {

            display:
              none;
          }

          .filter-row {

            grid-template-columns:
              1fr;
          }

          .kpi-grid {

            grid-template-columns:
              repeat(2, 1fr);
          }

          .chart-grid-equal {

            grid-template-columns:
              1fr;
          }

          .insight-grid {

            grid-template-columns:
              1fr;
          }

          .footer {

            margin-left:
              -16px;

            margin-right:
              -16px;

            padding-left:
              16px;

            padding-right:
              16px;

            flex-direction:
              column;

            gap:
              5px;
          }
        }


        @media (max-width: 480px) {

          .kpi-grid {

            grid-template-columns:
              1fr;
          }

          .hero-title {

            font-size:
              21px;
          }
        }
        "
      )
    )
  ),
  
  
  # ==========================================================
  # HERO
  # ==========================================================
  
  div(
    class = "hero",
    
    div(
      class = "hero-inner",
      
      div(
        class = "brand-area",
        
        if (
          file.exists(
            "www/instagram-logo.png"
          )
        ) {
          
          tags$img(
            src = "instagram-logo.png",
            class = "brand-logo",
            alt = "Instagram"
          )
          
        } else {
          
          div(
            class = "ig-fallback",
            "◎"
          )
        },
        
        div(
          
          div(
            class = "eyebrow",
            "BIG DATA ANALYTICS PROJECT"
          ),
          
          h1(
            class = "hero-title",
            "Instagram Analytics"
          ),
          
          p(
            class = "hero-subtitle",
            "Content performance, audience engagement and publishing intelligence — all in one interactive dashboard."
          )
        )
      ),
      
      div(
        class = "hero-meta",
        
        div(
          class = "status-pill",
          
          span(
            class = "status-dot"
          ),
          
          "Live dataset"
        ),
        
        div(
          class = "status-pill",
          
          paste0(
            comma(
              nrow(instagram)
            ),
            " posts analysed"
          )
        )
      )
    )
  ),
  
  
  # ==========================================================
  # FILTER PANEL
  # ==========================================================
  
  div(
    class = "filter-panel",
    
    div(
      class = "filter-header",
      
      div(
        class = "filter-title",
        "Explore the dataset"
      ),
      
      div(
        class = "filter-caption",
        
        textOutput(
          "filter_status",
          inline = TRUE
        )
      )
    ),
    
    div(
      class = "filter-row",
      
      div(
        class = "filter-item",
        
        selectInput(
          "account_type",
          "Account type",
          
          choices = c(
            "All account types" = "ALL",
            safe_unique(
              instagram$account_type
            )
          ),
          
          selected = "ALL"
        )
      ),
      
      div(
        class = "filter-item",
        
        selectInput(
          "media_type",
          "Media type",
          
          choices = c(
            "All media types" = "ALL",
            safe_unique(
              instagram$media_type
            )
          ),
          
          selected = "ALL"
        )
      ),
      
      div(
        class = "filter-item",
        
        selectInput(
          "content_category",
          "Content category",
          
          choices = c(
            "All categories" = "ALL",
            safe_unique(
              instagram$content_category
            )
          ),
          
          selected = "ALL"
        )
      ),
      
      div(
        class = "filter-item",
        
        selectInput(
          "traffic_source",
          "Traffic source",
          
          choices = c(
            "All sources" = "ALL",
            safe_unique(
              instagram$traffic_source
            )
          ),
          
          selected = "ALL"
        )
      ),
      
      div(
        class = "filter-item",
        
        selectInput(
          "performance",
          "Performance",
          
          choices = c(
            "All performance" = "ALL",
            safe_unique(
              instagram$performance_bucket_label
            )
          ),
          
          selected = "ALL"
        )
      ),
      
      div(
        class = "filter-item",
        
        dateRangeInput(
          "date_range",
          
          "Date range",
          
          start = min(
            instagram$post_date,
            na.rm = TRUE
          ),
          
          end = max(
            instagram$post_date,
            na.rm = TRUE
          ),
          
          min = min(
            instagram$post_date,
            na.rm = TRUE
          ),
          
          max = max(
            instagram$post_date,
            na.rm = TRUE
          ),
          
          format = "dd M yyyy",
          
          separator = " → "
        )
      ),
      
      div(
        
        style =
          "display:flex; gap:6px;",
        
        actionButton(
          "reset_filters",
          "Reset",
          class = "action-btn"
        ),
        
        downloadButton(
          "download_data",
          "Export",
          class = "action-btn download-btn"
        )
      )
    )
  ),
  
  
  # ==========================================================
  # KPI CARDS
  # ==========================================================
  
  div(
    class = "kpi-grid",
    
    div(
      class = "kpi-card",
      
      div(
        class = "kpi-label",
        "Posts"
      ),
      
      div(
        class = "kpi-value",
        
        textOutput(
          "kpi_posts",
          inline = TRUE
        )
      ),
      
      div(
        class = "kpi-note",
        "Filtered publications"
      )
    ),
    
    div(
      class = "kpi-card orange",
      
      div(
        class = "kpi-label",
        "Reach"
      ),
      
      div(
        class = "kpi-value",
        
        textOutput(
          "kpi_reach",
          inline = TRUE
        )
      ),
      
      div(
        class = "kpi-note",
        "Total accounts reached"
      )
    ),
    
    div(
      class = "kpi-card pink",
      
      div(
        class = "kpi-label",
        "Impressions"
      ),
      
      div(
        class = "kpi-value",
        
        textOutput(
          "kpi_impressions",
          inline = TRUE
        )
      ),
      
      div(
        class = "kpi-note",
        "Total content views"
      )
    ),
    
    div(
      class = "kpi-card blue",
      
      div(
        class = "kpi-label",
        "Engagement"
      ),
      
      div(
        class = "kpi-value",
        
        textOutput(
          "kpi_engagement",
          inline = TRUE
        )
      ),
      
      div(
        class = "kpi-note",
        "Likes + comments + shares + saves"
      )
    ),
    
    div(
      class = "kpi-card green",
      
      div(
        class = "kpi-label",
        "Engagement rate"
      ),
      
      div(
        class = "kpi-value",
        
        textOutput(
          "kpi_rate",
          inline = TRUE
        )
      ),
      
      div(
        class = "kpi-note",
        "Average across filtered posts"
      )
    ),
    
    div(
      class = "kpi-card amber",
      
      div(
        class = "kpi-label",
        "Followers gained"
      ),
      
      div(
        class = "kpi-value",
        
        textOutput(
          "kpi_followers",
          inline = TRUE
        )
      ),
      
      div(
        class = "kpi-note",
        "Attributed follower growth"
      )
    )
  ),
  
  
  # ==========================================================
  # TREND
  # ==========================================================
  
  div(
    class = "section-heading",
    
    h3(
      "Performance over time"
    ),
    
    span(
      "Daily aggregation"
    )
  ),
  
  div(
    class = "chart-card full",
    
    div(
      class = "chart-card-header",
      
      div(
        
        div(
          class = "chart-card-title",
          "Publishing performance trend"
        ),
        
        div(
          class = "chart-card-subtitle",
          "Change the metric to explore different performance dimensions."
        )
      ),
      
      div(
        class = "chart-control",
        
        selectInput(
          "trend_metric",
          NULL,
          
          choices = c(
            "Engagement rate" =
              "engagement_rate",
            
            "Total engagement" =
              "total_engagement",
            
            "Reach" =
              "reach",
            
            "Impressions" =
              "impressions",
            
            "Followers gained" =
              "followers_gained"
          ),
          
          selected =
            "engagement_rate"
        )
      )
    ),
    
    plotlyOutput(
      "trend_plot",
      height = "300px"
    )
  ),
  
  
  # ==========================================================
  # CONTENT PERFORMANCE
  # ==========================================================
  
  div(
    class = "section-heading",
    
    h3(
      "Content performance"
    ),
    
    span(
      "Compare formats and performance tiers"
    )
  ),
  
  div(
    class = "chart-grid",
    
    div(
      class = "chart-card",
      
      div(
        class = "chart-card-header",
        
        div(
          
          div(
            class = "chart-card-title",
            "Performance distribution"
          ),
          
          div(
            class = "chart-card-subtitle",
            "Share of filtered posts"
          )
        )
      ),
      
      plotlyOutput(
        "performance_plot",
        height = "290px"
      )
    ),
    
    div(
      class = "chart-card",
      
      div(
        class = "chart-card-header",
        
        div(
          
          div(
            class = "chart-card-title",
            "Media type performance"
          ),
          
          div(
            class = "chart-card-subtitle",
            "Average engagement rate"
          )
        )
      ),
      
      plotlyOutput(
        "media_plot",
        height = "290px"
      )
    )
  ),
  
  
  # ==========================================================
  # AUDIENCE INTELLIGENCE
  # ==========================================================
  
  div(
    class = "section-heading",
    
    h3(
      "Audience & content intelligence"
    ),
    
    span(
      "Where performance is coming from"
    )
  ),
  
  div(
    class = "chart-grid-equal",
    
    div(
      class = "chart-card",
      
      div(
        class = "chart-card-header",
        
        div(
          
          div(
            class = "chart-card-title",
            "Traffic source"
          ),
          
          div(
            class = "chart-card-subtitle",
            "Average engagement rate by source"
          )
        )
      ),
      
      plotlyOutput(
        "traffic_plot",
        height = "325px"
      )
    ),
    
    div(
      class = "chart-card",
      
      div(
        class = "chart-card-header",
        
        div(
          
          div(
            class = "chart-card-title",
            "Top content categories"
          ),
          
          div(
            class = "chart-card-subtitle",
            "Top 10 categories by engagement rate"
          )
        )
      ),
      
      plotlyOutput(
        "category_plot",
        height = "325px"
      )
    )
  ),
  
  
  # ==========================================================
  # HEATMAP
  # ==========================================================
  
  div(
    class = "section-heading",
    
    h3(
      "Best publishing windows"
    ),
    
    span(
      "Day × hour engagement analysis"
    )
  ),
  
  div(
    class = "chart-card full",
    
    plotlyOutput(
      "heatmap_plot",
      height = "370px"
    )
  ),
  
  
  # ==========================================================
  # ENGAGEMENT COMPOSITION
  # ==========================================================
  
  div(
    class = "section-heading",
    
    h3(
      "Engagement composition"
    ),
    
    span(
      "How audiences interact with content"
    )
  ),
  
  div(
    class = "chart-card full",
    
    plotlyOutput(
      "engagement_plot",
      height = "300px"
    )
  ),
  
  
  # ==========================================================
  # INSIGHTS
  # ==========================================================
  
  div(
    class = "section-heading",
    
    h3(
      "Key insights"
    ),
    
    span(
      "Automatically generated from current filters"
    )
  ),
  
  div(
    class = "insight-panel",
    
    div(
      class = "insight-title",
      
      div(
        class = "insight-icon",
        "✦"
      ),
      
      "What the filtered data is telling you"
    ),
    
    uiOutput(
      "insights"
    )
  ),
  
  
  # ==========================================================
  # TOP POSTS
  # ==========================================================
  
  div(
    class = "section-heading",
    
    h3(
      "Top performing posts"
    ),
    
    span(
      "Sorted by engagement rate"
    )
  ),
  
  div(
    class = "table-card",
    
    DTOutput(
      "top_posts"
    )
  ),
  
  
  # ==========================================================
  # FOOTER
  # ==========================================================
  
  div(
    class = "footer",
    
    div(
      strong(
        "Instagram Analytics"
      ),
      
      " · Big Data Analytics Project"
    ),
    
    div(
      "R · Shiny · Plotly"
    )
  )
)


# ============================================================
# 9. SERVER
# ============================================================

server <- function(
    input,
    output,
    session
) {
  
  
  # ==========================================================
  # FILTERED DATA
  # ==========================================================
  
  filtered_data <- reactive({
    
    d <- instagram
    
    # --------------------------------------------------------
    # Account type
    # --------------------------------------------------------
    
    if (
      !is.null(input$account_type) &&
      length(input$account_type) == 1 &&
      input$account_type != "ALL"
    ) {
      
      d <- d %>%
        filter(
          account_type ==
            input$account_type
        )
    }
    
    
    # --------------------------------------------------------
    # Media type
    # --------------------------------------------------------
    
    if (
      !is.null(input$media_type) &&
      length(input$media_type) == 1 &&
      input$media_type != "ALL"
    ) {
      
      d <- d %>%
        filter(
          media_type ==
            input$media_type
        )
    }
    
    
    # --------------------------------------------------------
    # Content category
    # --------------------------------------------------------
    
    if (
      !is.null(input$content_category) &&
      length(input$content_category) == 1 &&
      input$content_category != "ALL"
    ) {
      
      d <- d %>%
        filter(
          content_category ==
            input$content_category
        )
    }
    
    
    # --------------------------------------------------------
    # Traffic source
    # --------------------------------------------------------
    
    if (
      !is.null(input$traffic_source) &&
      length(input$traffic_source) == 1 &&
      input$traffic_source != "ALL"
    ) {
      
      d <- d %>%
        filter(
          traffic_source ==
            input$traffic_source
        )
    }
    
    
    # --------------------------------------------------------
    # Performance
    # --------------------------------------------------------
    
    if (
      !is.null(input$performance) &&
      length(input$performance) == 1 &&
      input$performance != "ALL"
    ) {
      
      d <- d %>%
        filter(
          performance_bucket_label ==
            input$performance
        )
    }
    
    
    # --------------------------------------------------------
    # Date
    # --------------------------------------------------------
    
    if (
      !is.null(input$date_range) &&
      length(input$date_range) == 2 &&
      all(!is.na(input$date_range))
    ) {
      
      d <- d %>%
        filter(
          post_date >=
            input$date_range[1],
          
          post_date <=
            input$date_range[2]
        )
    }
    
    
    d
  })
  
  
  # ==========================================================
  # RESET
  # ==========================================================
  
  observeEvent(
    input$reset_filters,
    {
      
      updateSelectInput(
        session,
        "account_type",
        selected = "ALL"
      )
      
      updateSelectInput(
        session,
        "media_type",
        selected = "ALL"
      )
      
      updateSelectInput(
        session,
        "content_category",
        selected = "ALL"
      )
      
      updateSelectInput(
        session,
        "traffic_source",
        selected = "ALL"
      )
      
      updateSelectInput(
        session,
        "performance",
        selected = "ALL"
      )
      
      updateDateRangeInput(
        session,
        "date_range",
        
        start = min(
          instagram$post_date,
          na.rm = TRUE
        ),
        
        end = max(
          instagram$post_date,
          na.rm = TRUE
        )
      )
      
      updateSelectInput(
        session,
        "trend_metric",
        selected =
          "engagement_rate"
      )
    }
  )
  
  
  # ==========================================================
  # FILTER STATUS
  # ==========================================================
  
  output$filter_status <- renderText({
    
    d <- filtered_data()
    
    paste0(
      comma(
        nrow(d)
      ),
      " posts currently selected"
    )
  })
  
  
  # ==========================================================
  # KPI: POSTS
  # ==========================================================
  
  output$kpi_posts <- renderText({
    
    fmt_compact(
      nrow(
        filtered_data()
      )
    )
  })
  
  
  # ==========================================================
  # KPI: REACH
  # ==========================================================
  
  output$kpi_reach <- renderText({
    
    d <- filtered_data()
    
    fmt_compact(
      sum(
        d$reach,
        na.rm = TRUE
      )
    )
  })
  
  
  # ==========================================================
  # KPI: IMPRESSIONS
  # ==========================================================
  
  output$kpi_impressions <- renderText({
    
    d <- filtered_data()
    
    fmt_compact(
      sum(
        d$impressions,
        na.rm = TRUE
      )
    )
  })
  
  
  # ==========================================================
  # KPI: ENGAGEMENT
  # ==========================================================
  
  output$kpi_engagement <- renderText({
    
    d <- filtered_data()
    
    fmt_compact(
      sum(
        d$total_engagement,
        na.rm = TRUE
      )
    )
  })
  
  
  # ==========================================================
  # KPI: ENGAGEMENT RATE
  # ==========================================================
  
  output$kpi_rate <- renderText({
    
    d <- filtered_data()
    
    fmt_percent(
      safe_mean(
        d$engagement_rate
      ),
      2
    )
  })
  
  
  # ==========================================================
  # KPI: FOLLOWERS
  # ==========================================================
  
  output$kpi_followers <- renderText({
    
    d <- filtered_data()
    
    fmt_compact(
      sum(
        d$followers_gained,
        na.rm = TRUE
      )
    )
  })
  
  
  # ==========================================================
  # COMMON PLOTLY CONFIG
  # ==========================================================
  
  plot_config <- list(
    
    responsive = TRUE,
    
    displaylogo = FALSE,
    
    modeBarButtonsToRemove = c(
      "lasso2d",
      "select2d"
    )
  )
  
  
  # ==========================================================
  # TREND CHART
  # ==========================================================
  
  output$trend_plot <- renderPlotly({
    
    d <- filtered_data()
    
    if (nrow(d) == 0) {
      
      return(
        empty_plot(
          "No posts match the selected filters."
        )
      )
    }
    
    metric <- input$trend_metric
    
    if (
      is.null(metric) ||
      length(metric) != 1 ||
      !metric %in% c(
        "engagement_rate",
        "total_engagement",
        "reach",
        "impressions",
        "followers_gained"
      )
    ) {
      
      metric <-
        "engagement_rate"
    }
    
    
    trend <- d %>%
      
      group_by(
        post_date
      ) %>%
      
      summarise(
        
        engagement_rate =
          safe_mean(
            engagement_rate
          ),
        
        total_engagement =
          sum(
            total_engagement,
            na.rm = TRUE
          ),
        
        reach =
          sum(
            reach,
            na.rm = TRUE
          ),
        
        impressions =
          sum(
            impressions,
            na.rm = TRUE
          ),
        
        followers_gained =
          sum(
            followers_gained,
            na.rm = TRUE
          ),
        
        .groups = "drop"
      ) %>%
      
      arrange(
        post_date
      )
    
    
    if (nrow(trend) == 0) {
      
      return(
        empty_plot(
          "No trend data available."
        )
      )
    }
    
    
    y_values <-
      trend[[metric]]
    
    
    metric_labels <- c(
      
      engagement_rate =
        "Engagement rate",
      
      total_engagement =
        "Total engagement",
      
      reach =
        "Reach",
      
      impressions =
        "Impressions",
      
      followers_gained =
        "Followers gained"
    )
    
    
    if (
      metric ==
      "engagement_rate"
    ) {
      
      hover_text <- paste0(
        
        "<b>",
        format(
          trend$post_date,
          "%d %b %Y"
        ),
        "</b>",
        
        "<br>Engagement rate: ",
        
        fmt_percent_vec(
          y_values,
          2
        ),
        
        "<extra></extra>"
      )
      
      y_axis <- list(
        
        title = NULL,
        
        tickformat = ".1%",
        
        gridcolor =
          "#EEF0F4",
        
        zeroline = FALSE
      )
      
    } else {
      
      hover_text <- paste0(
        
        "<b>",
        format(
          trend$post_date,
          "%d %b %Y"
        ),
        "</b>",
        
        "<br>",
        metric_labels[[metric]],
        ": ",
        
        comma(
          round(
            y_values
          )
        ),
        
        "<extra></extra>"
      )
      
      y_axis <- list(
        
        title = NULL,
        
        separatethousands =
          TRUE,
        
        gridcolor =
          "#EEF0F4",
        
        zeroline = FALSE
      )
    }
    
    
    plot_ly(
      
      x = trend$post_date,
      
      y = y_values,
      
      type = "scatter",
      
      mode =
        "lines+markers",
      
      text =
        hover_text,
      
      hovertemplate =
        "%{text}",
      
      line = list(
        
        color =
          "#7C3AED",
        
        width = 3,
        
        shape = "spline"
      ),
      
      marker = list(
        
        color =
          "#7C3AED",
        
        size = 5
      ),
      
      fill =
        "tozeroy",
      
      fillcolor =
        "rgba(124,58,237,0.07)"
    ) %>%
      
      layout(
        
        margin = list(
          
          l = 48,
          
          r = 18,
          
          t = 10,
          
          b = 42
        ),
        
        xaxis = list(
          
          title = NULL,
          
          showgrid = FALSE,
          
          zeroline = FALSE
        ),
        
        yaxis = y_axis,
        
        hovermode =
          "x unified",
        
        paper_bgcolor =
          "rgba(0,0,0,0)",
        
        plot_bgcolor =
          "rgba(0,0,0,0)"
      ) %>%
      
      config(
        plot_config
      )
  })
  
  
  # ==========================================================
  # PERFORMANCE DONUT
  # ==========================================================
  
  output$performance_plot <- renderPlotly({
    
    d <- filtered_data()
    
    if (nrow(d) == 0) {
      
      return(
        empty_plot(
          "No performance data available."
        )
      )
    }
    
    perf <- d %>%
      
      filter(
        !is.na(
          performance_bucket_label
        )
      ) %>%
      
      count(
        performance_bucket_label,
        name = "posts"
      ) %>%
      
      arrange(
        desc(posts)
      )
    
    
    if (nrow(perf) == 0) {
      
      return(
        empty_plot(
          "No performance categories available."
        )
      )
    }
    
    
    perf$label <-
      clean_label(
        perf$performance_bucket_label
      )
    
    
    perf$share <-
      perf$posts /
      sum(perf$posts)
    
    
    perf$hover <- paste0(
      
      "<b>",
      perf$label,
      "</b>",
      
      "<br>Posts: ",
      comma(
        perf$posts
      ),
      
      "<br>Share: ",
      
      fmt_percent_vec(
        perf$share,
        1
      ),
      
      "<extra></extra>"
    )
    
    
    plot_ly(
      
      data = perf,
      
      labels = ~label,
      
      values = ~posts,
      
      type = "pie",
      
      hole = 0.64,
      
      textinfo =
        "label+percent",
      
      textposition =
        "outside",
      
      hovertemplate =
        ~hover,
      
      marker = list(
        
        colors = c(
          "#7C3AED",
          "#F59E0B",
          "#10B981",
          "#EF4444"
        )
      )
    ) %>%
      
      layout(
        
        margin = list(
          l = 10,
          r = 10,
          t = 10,
          b = 10
        ),
        
        showlegend =
          FALSE,
        
        paper_bgcolor =
          "rgba(0,0,0,0)"
      ) %>%
      
      config(
        plot_config
      )
  })
  
  
  # ==========================================================
  # MEDIA TYPE
  # ==========================================================
  
  output$media_plot <- renderPlotly({
    
    d <- filtered_data()
    
    if (nrow(d) == 0) {
      
      return(
        empty_plot(
          "No media data available."
        )
      )
    }
    
    media <- d %>%
      
      filter(
        !is.na(media_type)
      ) %>%
      
      group_by(
        media_type
      ) %>%
      
      summarise(
        
        engagement_rate =
          safe_mean(
            engagement_rate
          ),
        
        posts =
          n(),
        
        .groups =
          "drop"
      ) %>%
      
      arrange(
        engagement_rate
      )
    
    
    if (nrow(media) == 0) {
      
      return(
        empty_plot(
          "No media types available."
        )
      )
    }
    
    
    media$label <-
      clean_label(
        media$media_type
      )
    
    
    media$hover <- paste0(
      
      "<b>",
      media$label,
      "</b>",
      
      "<br>Engagement rate: ",
      
      fmt_percent_vec(
        media$engagement_rate,
        2
      ),
      
      "<br>Posts: ",
      
      comma(
        media$posts
      ),
      
      "<extra></extra>"
    )
    
    
    plot_ly(
      
      data = media,
      
      x =
        ~engagement_rate,
      
      y =
        ~reorder(
          label,
          engagement_rate
        ),
      
      type =
        "bar",
      
      orientation =
        "h",
      
      text =
        ~hover,
      
      hovertemplate =
        "%{text}",
      
      marker = list(
        
        color =
          "#EC4899"
      )
    ) %>%
      
      layout(
        
        margin = list(
          
          l = 95,
          
          r = 20,
          
          t = 10,
          
          b = 40
        ),
        
        xaxis = list(
          
          title = NULL,
          
          tickformat = ".1%",
          
          gridcolor =
            "#EEF0F4",
          
          zeroline = FALSE
        ),
        
        yaxis = list(
          
          title = NULL
        ),
        
        paper_bgcolor =
          "rgba(0,0,0,0)",
        
        plot_bgcolor =
          "rgba(0,0,0,0)"
      ) %>%
      
      config(
        plot_config
      )
  })
  
  
  # ==========================================================
  # TRAFFIC SOURCE
  # ==========================================================
  
  output$traffic_plot <- renderPlotly({
    
    d <- filtered_data()
    
    if (nrow(d) == 0) {
      
      return(
        empty_plot(
          "No traffic-source data available."
        )
      )
    }
    
    traffic <- d %>%
      
      filter(
        !is.na(
          traffic_source
        )
      ) %>%
      
      group_by(
        traffic_source
      ) %>%
      
      summarise(
        
        engagement_rate =
          safe_mean(
            engagement_rate
          ),
        
        reach =
          sum(
            reach,
            na.rm = TRUE
          ),
        
        posts =
          n(),
        
        .groups =
          "drop"
      ) %>%
      
      arrange(
        engagement_rate
      )
    
    
    if (nrow(traffic) == 0) {
      
      return(
        empty_plot(
          "No traffic sources available."
        )
      )
    }
    
    
    traffic$label <-
      clean_label(
        traffic$traffic_source
      )
    
    
    traffic$hover <- paste0(
      
      "<b>",
      traffic$label,
      "</b>",
      
      "<br>Engagement rate: ",
      
      fmt_percent_vec(
        traffic$engagement_rate,
        2
      ),
      
      "<br>Reach: ",
      
      fmt_compact(
        traffic$reach
      ),
      
      "<br>Posts: ",
      
      comma(
        traffic$posts
      ),
      
      "<extra></extra>"
    )
    
    
    plot_ly(
      
      data = traffic,
      
      x =
        ~engagement_rate,
      
      y =
        ~reorder(
          label,
          engagement_rate
        ),
      
      type =
        "bar",
      
      orientation =
        "h",
      
      text =
        ~hover,
      
      hovertemplate =
        "%{text}",
      
      marker = list(
        
        color =
          "#0EA5E9"
      )
    ) %>%
      
      layout(
        
        margin = list(
          
          l = 105,
          
          r = 20,
          
          t = 10,
          
          b = 40
        ),
        
        xaxis = list(
          
          title = NULL,
          
          tickformat = ".1%",
          
          gridcolor =
            "#EEF0F4",
          
          zeroline = FALSE
        ),
        
        yaxis = list(
          
          title = NULL
        ),
        
        paper_bgcolor =
          "rgba(0,0,0,0)",
        
        plot_bgcolor =
          "rgba(0,0,0,0)"
      ) %>%
      
      config(
        plot_config
      )
  })
  
  
  # ==========================================================
  # CONTENT CATEGORY
  # ==========================================================
  
  output$category_plot <- renderPlotly({
    
    d <- filtered_data()
    
    if (nrow(d) == 0) {
      
      return(
        empty_plot(
          "No category data available."
        )
      )
    }
    
    category <- d %>%
      
      filter(
        !is.na(
          content_category
        )
      ) %>%
      
      group_by(
        content_category
      ) %>%
      
      summarise(
        
        engagement_rate =
          safe_mean(
            engagement_rate
          ),
        
        posts =
          n(),
        
        .groups =
          "drop"
      ) %>%
      
      arrange(
        desc(
          engagement_rate
        )
      ) %>%
      
      slice_head(
        n = 10
      ) %>%
      
      arrange(
        engagement_rate
      )
    
    
    if (nrow(category) == 0) {
      
      return(
        empty_plot(
          "No content categories available."
        )
      )
    }
    
    
    category$label <-
      clean_label(
        category$content_category
      )
    
    
    category$hover <- paste0(
      
      "<b>",
      category$label,
      "</b>",
      
      "<br>Engagement rate: ",
      
      fmt_percent_vec(
        category$engagement_rate,
        2
      ),
      
      "<br>Posts: ",
      
      comma(
        category$posts
      ),
      
      "<extra></extra>"
    )
    
    
    plot_ly(
      
      data = category,
      
      x =
        ~engagement_rate,
      
      y =
        ~reorder(
          label,
          engagement_rate
        ),
      
      type =
        "bar",
      
      orientation =
        "h",
      
      text =
        ~hover,
      
      hovertemplate =
        "%{text}",
      
      marker = list(
        
        color =
          "#7C3AED"
      )
    ) %>%
      
      layout(
        
        margin = list(
          
          l = 115,
          
          r = 20,
          
          t = 10,
          
          b = 40
        ),
        
        xaxis = list(
          
          title = NULL,
          
          tickformat = ".1%",
          
          gridcolor =
            "#EEF0F4",
          
          zeroline = FALSE
        ),
        
        yaxis = list(
          
          title = NULL
        ),
        
        paper_bgcolor =
          "rgba(0,0,0,0)",
        
        plot_bgcolor =
          "rgba(0,0,0,0)"
      ) %>%
      
      config(
        plot_config
      )
  })
  
  
  # ==========================================================
  # POSTING HEATMAP
  # ==========================================================
  
  output$heatmap_plot <- renderPlotly({
    
    d <- filtered_data()
    
    if (nrow(d) == 0) {
      
      return(
        empty_plot(
          "No heatmap data available."
        )
      )
    }
    
    
    day_order <- c(
      
      "Monday",
      "Tuesday",
      "Wednesday",
      "Thursday",
      "Friday",
      "Saturday",
      "Sunday"
    )
    
    
    d <- d %>%
      
      mutate(
        
        day_of_week =
          factor(
            day_of_week,
            levels =
              day_order
          ),
        
        post_hour =
          as.numeric(
            post_hour
          )
      )
    
    
    heat <- d %>%
      
      filter(
        !is.na(day_of_week),
        !is.na(post_hour),
        post_hour >= 0,
        post_hour <= 23
      ) %>%
      
      group_by(
        
        day_of_week,
        
        post_hour
      ) %>%
      
      summarise(
        
        engagement_rate =
          safe_mean(
            engagement_rate
          ),
        
        posts =
          n(),
        
        .groups =
          "drop"
      )
    
    
    grid <- expand.grid(
      
      day_of_week =
        factor(
          day_order,
          levels =
            day_order
        ),
      
      post_hour =
        0:23
    )
    
    
    heat <- grid %>%
      
      left_join(
        
        heat,
        
        by = c(
          "day_of_week",
          "post_hour"
        )
      )
    
    
    z_matrix <- matrix(
      
      heat$engagement_rate,
      
      nrow = 7,
      
      ncol = 24,
      
      byrow = TRUE
    )
    
    
    hover_values <-
      fmt_percent_vec(
        heat$engagement_rate,
        2
      )
    
    
    hover_text <- paste0(
      
      "<b>Day: </b>",
      as.character(
        heat$day_of_week
      ),
      
      "<br><b>Hour: </b>",
      sprintf(
        "%02d:00",
        heat$post_hour
      ),
      
      "<br><b>Engagement rate: </b>",
      hover_values,
      
      "<br><b>Posts: </b>",
      
      ifelse(
        is.na(heat$posts),
        "0",
        comma(
          heat$posts
        )
      ),
      
      "<extra></extra>"
    )
    
    
    # Handle completely empty numeric grid
    valid_rates <-
      heat$engagement_rate[
        is.finite(
          heat$engagement_rate
        )
      ]
    
    
    if (
      length(valid_rates) == 0
    ) {
      
      return(
        empty_plot(
          "No engagement-rate observations available."
        )
      )
    }
    
    
    plot_ly(
      
      x = 0:23,
      
      y = day_order,
      
      z = z_matrix,
      
      type = "heatmap",
      
      text =
        matrix(
          hover_text,
          nrow = 7,
          ncol = 24,
          byrow = TRUE
        ),
      
      hovertemplate =
        "%{text}",
      
      colorscale = list(
        
        c(
          0,
          "#F4F1FB"
        ),
        
        c(
          0.25,
          "#DDD2F6"
        ),
        
        c(
          0.50,
          "#B9A5EA"
        ),
        
        c(
          0.75,
          "#9472D9"
        ),
        
        c(
          1,
          "#6D28D9"
        )
      ),
      
      zmin =
        min(
          valid_rates
        ),
      
      zmax =
        max(
          valid_rates
        ),
      
      colorbar = list(
        
        title =
          "Engagement",
        
        tickformat =
          ".1%"
      )
    ) %>%
      
      layout(
        
        margin = list(
          
          l = 75,
          
          r = 65,
          
          t = 20,
          
          b = 45
        ),
        
        xaxis = list(
          
          title =
            "Posting hour",
          
          dtick =
            2,
          
          gridcolor =
            "rgba(255,255,255,0)"
        ),
        
        yaxis = list(
          
          title =
            NULL,
          
          categoryorder =
            "array",
          
          categoryarray =
            day_order
        ),
        
        paper_bgcolor =
          "rgba(0,0,0,0)",
        
        plot_bgcolor =
          "rgba(0,0,0,0)"
      ) %>%
      
      config(
        plot_config
      )
  })
  
  
  # ==========================================================
  # ENGAGEMENT COMPOSITION
  # ==========================================================
  
  output$engagement_plot <- renderPlotly({
    
    d <- filtered_data()
    
    if (nrow(d) == 0) {
      
      return(
        empty_plot(
          "No engagement data available."
        )
      )
    }
    
    
    engagement <- data.frame(
      
      type = c(
        "Likes",
        "Comments",
        "Shares",
        "Saves"
      ),
      
      value = c(
        
        sum(
          d$likes,
          na.rm = TRUE
        ),
        
        sum(
          d$comments,
          na.rm = TRUE
        ),
        
        sum(
          d$shares,
          na.rm = TRUE
        ),
        
        sum(
          d$saves,
          na.rm = TRUE
        )
      )
    )
    
    
    total_value <-
      sum(
        engagement$value,
        na.rm = TRUE
      )
    
    
    if (
      total_value <= 0
    ) {
      
      return(
        empty_plot(
          "No engagement interactions available."
        )
      )
    }
    
    
    engagement$share <-
      engagement$value /
      total_value
    
    
    engagement$hover <- paste0(
      
      "<b>",
      engagement$type,
      "</b>",
      
      "<br>Total: ",
      
      comma(
        engagement$value
      ),
      
      "<br>Share: ",
      
      fmt_percent_vec(
        engagement$share,
        1
      ),
      
      "<extra></extra>"
    )
    
    
    plot_ly(
      
      data = engagement,
      
      x = ~type,
      
      y = ~value,
      
      type = "bar",
      
      text = ~hover,
      
      hovertemplate =
        "%{text}",
      
      marker = list(
        
        color = c(
          "#7C3AED",
          "#EC4899",
          "#0EA5E9",
          "#10B981"
        )
      )
    ) %>%
      
      layout(
        
        margin = list(
          
          l = 55,
          
          r = 20,
          
          t = 15,
          
          b = 55
        ),
        
        xaxis = list(
          
          title = NULL
        ),
        
        yaxis = list(
          
          title =
            "Interactions",
          
          gridcolor =
            "#EEF0F4",
          
          zeroline =
            FALSE,
          
          separatethousands =
            TRUE
        ),
        
        paper_bgcolor =
          "rgba(0,0,0,0)",
        
        plot_bgcolor =
          "rgba(0,0,0,0)"
      ) %>%
      
      config(
        plot_config
      )
  })
  
  
  # ==========================================================
  # AUTOMATED INSIGHTS
  # ==========================================================
  
  output$insights <- renderUI({
    
    d <- filtered_data()
    
    if (nrow(d) == 0) {
      
      return(
        
        div(
          
          class =
            "insight-grid",
          
          div(
            
            class =
              "insight-item",
            
            strong(
              "No data selected"
            ),
            
            span(
              "Change the filters to generate data-driven insights."
            )
          )
        )
      )
    }
    
    
    # --------------------------------------------------------
    # Best media
    # --------------------------------------------------------
    
    media_summary <- d %>%
      
      filter(
        !is.na(
          media_type
        )
      ) %>%
      
      group_by(
        media_type
      ) %>%
      
      summarise(
        
        rate =
          safe_mean(
            engagement_rate
          ),
        
        .groups =
          "drop"
      ) %>%
      
      arrange(
        desc(rate)
      )
    
    
    if (nrow(media_summary) > 0) {
      
      best_media <-
        clean_label(
          media_summary$media_type[1]
        )
      
      best_media_rate <-
        media_summary$rate[1]
      
    } else {
      
      best_media <-
        "N/A"
      
      best_media_rate <-
        0
    }
    
    
    # --------------------------------------------------------
    # Best category
    # --------------------------------------------------------
    
    category_summary <- d %>%
      
      filter(
        !is.na(
          content_category
        )
      ) %>%
      
      group_by(
        content_category
      ) %>%
      
      summarise(
        
        rate =
          safe_mean(
            engagement_rate
          ),
        
        .groups =
          "drop"
      ) %>%
      
      arrange(
        desc(rate)
      )
    
    
    if (
      nrow(category_summary) > 0
    ) {
      
      best_category <-
        clean_label(
          category_summary$content_category[1]
        )
      
    } else {
      
      best_category <-
        "N/A"
    }
    
    
    # --------------------------------------------------------
    # Best traffic source
    # --------------------------------------------------------
    
    source_summary <- d %>%
      
      filter(
        !is.na(
          traffic_source
        )
      ) %>%
      
      group_by(
        traffic_source
      ) %>%
      
      summarise(
        
        rate =
          safe_mean(
            engagement_rate
          ),
        
        .groups =
          "drop"
      ) %>%
      
      arrange(
        desc(rate)
      )
    
    
    if (
      nrow(source_summary) > 0
    ) {
      
      best_source <-
        clean_label(
          source_summary$traffic_source[1]
        )
      
    } else {
      
      best_source <-
        "N/A"
    }
    
    
    # --------------------------------------------------------
    # Best day/hour
    # --------------------------------------------------------
    
    timing_summary <- d %>%
      
      filter(
        !is.na(day_of_week),
        !is.na(post_hour)
      ) %>%
      
      group_by(
        
        day_of_week,
        
        post_hour
      ) %>%
      
      summarise(
        
        rate =
          safe_mean(
            engagement_rate
          ),
        
        posts =
          n(),
        
        .groups =
          "drop"
      ) %>%
      
      filter(
        is.finite(rate)
      ) %>%
      
      arrange(
        desc(rate)
      )
    
    
    if (
      nrow(timing_summary) > 0
    ) {
      
      best_day <-
        clean_label(
          timing_summary$day_of_week[1]
        )
      
      best_hour <-
        timing_summary$post_hour[1]
      
    } else {
      
      best_day <-
        "N/A"
      
      best_hour <-
        NA
    }
    
    
    # --------------------------------------------------------
    # Dominant performance
    # --------------------------------------------------------
    
    performance_summary <- d %>%
      
      filter(
        !is.na(
          performance_bucket_label
        )
      ) %>%
      
      count(
        performance_bucket_label,
        sort = TRUE
      )
    
    
    if (
      nrow(performance_summary) > 0
    ) {
      
      dominant_performance <-
        clean_label(
          performance_summary$performance_bucket_label[1]
        )
      
    } else {
      
      dominant_performance <-
        "N/A"
    }
    
    
    # --------------------------------------------------------
    # Best hour text
    # --------------------------------------------------------
    
    if (
      is.na(best_hour)
    ) {
      
      best_time_text <-
        "N/A"
      
    } else {
      
      best_time_text <-
        paste0(
          best_day,
          " at ",
          sprintf(
            "%02d:00",
            as.integer(
              best_hour
            )
          )
        )
    }
    
    
    # --------------------------------------------------------
    # UI
    # --------------------------------------------------------
    
    div(
      
      class =
        "insight-grid",
      
      div(
        
        class =
          "insight-item",
        
        strong(
          paste0(
            "Best format · ",
            best_media
          )
        ),
        
        span(
          paste0(
            "Average engagement rate: ",
            fmt_percent(
              best_media_rate,
              2
            ),
            "."
          )
        )
      ),
      
      div(
        
        class =
          "insight-item",
        
        strong(
          paste0(
            "Top category · ",
            best_category
          )
        ),
        
        span(
          "This category currently leads the filtered content groups by average engagement rate."
        )
      ),
      
      div(
        
        class =
          "insight-item",
        
        strong(
          paste0(
            "Strongest source · ",
            best_source
          )
        ),
        
        span(
          "This source currently produces the highest average engagement rate."
        )
      ),
      
      div(
        
        class =
          "insight-item",
        
        strong(
          paste0(
            "Best publishing window · ",
            best_time_text
          )
        ),
        
        span(
          "Based on the highest observed average engagement rate across day and posting hour."
        )
      ),
      
      div(
        
        class =
          "insight-item",
        
        strong(
          paste0(
            "Dominant performance · ",
            dominant_performance
          )
        ),
        
        span(
          "This performance bucket contains the largest number of posts in the current selection."
        )
      ),
      
      div(
        
        class =
          "insight-item",
        
        strong(
          paste0(
            "Current sample · ",
            comma(
              nrow(d)
            ),
            " posts"
          )
        ),
        
        span(
          "All metrics and insights update automatically when filters change."
        )
      )
    )
  })
  
  
  # ==========================================================
  # TOP POSTS TABLE
  # ==========================================================
  
  output$top_posts <- renderDT({
    
    d <- filtered_data()
    
    if (nrow(d) == 0) {
      
      return(
        datatable(
          data.frame(
            Message =
              "No posts match the selected filters."
          ),
          rownames = FALSE,
          options = list(
            dom = "t"
          )
        )
      )
    }
    
    
    top <- d %>%
      
      arrange(
        desc(
          engagement_rate
        )
      ) %>%
      
      slice_head(
        n = 10
      ) %>%
      
      select(
        
        post_id,
        
        account_type,
        
        media_type,
        
        content_category,
        
        engagement_rate,
        
        total_engagement,
        
        reach,
        
        likes,
        
        comments,
        
        shares,
        
        saves,
        
        followers_gained
      )
    
    
    names(top) <- c(
      
      "Post ID",
      
      "Account",
      
      "Media",
      
      "Category",
      
      "Engagement Rate",
      
      "Total Engagement",
      
      "Reach",
      
      "Likes",
      
      "Comments",
      
      "Shares",
      
      "Saves",
      
      "Followers Gained"
    )
    
    
    datatable(
      
      top,
      
      rownames = FALSE,
      
      filter = "top",
      
      selection = "none",
      
      class =
        "stripe hover nowrap",
      
      options = list(
        
        pageLength = 10,
        
        lengthChange = FALSE,
        
        searching = TRUE,
        
        ordering = TRUE,
        
        info = TRUE,
        
        autoWidth = TRUE,
        
        scrollX = TRUE,
        
        dom = "frtip",
        
        language = list(
          
          search =
            "Search posts:",
          
          info =
            "_START_–_END_ of _TOTAL_ posts"
        )
      )
    ) %>%
      
      formatPercentage(
        
        "Engagement Rate",
        
        digits = 2
      ) %>%
      
      formatRound(
        
        c(
          
          "Total Engagement",
          
          "Reach",
          
          "Likes",
          
          "Comments",
          
          "Shares",
          
          "Saves",
          
          "Followers Gained"
        ),
        
        digits = 0,
        
        mark = ","
      )
  })
  
  
  # ==========================================================
  # DOWNLOAD FILTERED DATA
  # ==========================================================
  
  output$download_data <-
    downloadHandler(
      
      filename = function() {
        
        paste0(
          "Instagram_Analytics_Filtered_",
          Sys.Date(),
          ".csv"
        )
      },
      
      content = function(file) {
        
        write_csv(
          filtered_data(),
          file
        )
      }
    )
}


# ============================================================
# 10. RUN APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)