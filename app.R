# ============================================================
# INSTAGRAM ANALYTICS
# BIG DATA APPROACH TO CONTENT PERFORMANCE
# AND AUDIENCE ENGAGEMENT
# ============================================================

# ============================================================
# 1. PACKAGES
# ============================================================

required_packages <- c(
  "shiny",
  "bslib",
  "dplyr",
  "readr",
  "lubridate",
  "plotly",
  "DT",
  "scales",
  "fontawesome"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0) {
  install.packages(
    missing_packages,
    repos = "https://cloud.r-project.org"
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
library(fontawesome)


# ============================================================
# 2. LOAD DATA
# ============================================================

data_file <- "Instagram_Analytics.csv"

if (!file.exists(data_file)) {
  stop(
    paste0(
      "Instagram_Analytics.csv was not found.\n\n",
      "Keep the CSV file in the same folder as app.R."
    )
  )
}

instagram_data <- read_csv(
  data_file,
  show_col_types = FALSE
)


# ============================================================
# 3. REQUIRED COLUMNS
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
  names(instagram_data)
)

if (length(missing_columns) > 0) {
  stop(
    paste0(
      "Missing required columns:\n",
      paste(missing_columns, collapse = ", ")
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

for (col in numeric_columns) {
  instagram_data[[col]] <- suppressWarnings(
    as.numeric(instagram_data[[col]])
  )
}

instagram_data$post_date <- suppressWarnings(
  as.Date(instagram_data$post_date)
)

if (all(is.na(instagram_data$post_date))) {
  
  parsed_datetime <- suppressWarnings(
    parse_date_time(
      instagram_data$post_datetime,
      orders = c(
        "ymd HMS",
        "ymd HM",
        "dmy HMS",
        "dmy HM",
        "mdy HMS",
        "mdy HM"
      )
    )
  )
  
  instagram_data$post_date <- as.Date(
    parsed_datetime
  )
}

character_columns <- c(
  "account_type",
  "media_type",
  "content_category",
  "traffic_source",
  "day_of_week",
  "performance_bucket_label"
)

for (col in character_columns) {
  instagram_data[[col]] <- trimws(
    as.character(instagram_data[[col]])
  )
}

instagram_data$has_call_to_action <- tolower(
  trimws(
    as.character(
      instagram_data$has_call_to_action
    )
  )
)


# ============================================================
# 5. DERIVED METRICS
# ============================================================

instagram_data <- instagram_data %>%
  mutate(
    
    total_engagement =
      coalesce(likes, 0) +
      coalesce(comments, 0) +
      coalesce(shares, 0) +
      coalesce(saves, 0),
    
    engagement_per_reach = ifelse(
      !is.na(reach) & reach > 0,
      total_engagement / reach,
      NA_real_
    ),
    
    save_rate = ifelse(
      !is.na(reach) & reach > 0,
      saves / reach,
      NA_real_
    ),
    
    share_rate = ifelse(
      !is.na(reach) & reach > 0,
      shares / reach,
      NA_real_
    ),
    
    comment_rate = ifelse(
      !is.na(reach) & reach > 0,
      comments / reach,
      NA_real_
    ),
    
    follower_conversion = ifelse(
      !is.na(reach) & reach > 0,
      followers_gained / reach,
      NA_real_
    )
  )


# ============================================================
# 6. HELPER FUNCTIONS
# ============================================================

fmt_compact <- function(x) {
  
  if (length(x) == 0 || is.na(x[1])) {
    return("0")
  }
  
  x <- x[1]
  
  if (abs(x) >= 1000000000) {
    return(
      paste0(
        formatC(
          x / 1000000000,
          format = "f",
          digits = 1
        ),
        "B"
      )
    )
  }
  
  if (abs(x) >= 1000000) {
    return(
      paste0(
        formatC(
          x / 1000000,
          format = "f",
          digits = 1
        ),
        "M"
      )
    )
  }
  
  if (abs(x) >= 1000) {
    return(
      paste0(
        formatC(
          x / 1000,
          format = "f",
          digits = 1
        ),
        "K"
      )
    )
  }
  
  formatC(
    x,
    format = "f",
    digits = 0,
    big.mark = ","
  )
}


fmt_integer <- function(x) {
  
  if (length(x) == 0 || is.na(x[1])) {
    return("0")
  }
  
  formatC(
    x[1],
    format = "f",
    digits = 0,
    big.mark = ","
  )
}


fmt_pct <- function(x, digits = 2) {
  
  if (length(x) == 0 || is.na(x[1])) {
    return("0%")
  }
  
  paste0(
    formatC(
      x[1] * 100,
      format = "f",
      digits = digits
    ),
    "%"
  )
}


fmt_pct_vec <- function(x, digits = 2) {
  
  result <- rep(
    "0%",
    length(x)
  )
  
  valid <- !is.na(x)
  
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


safe_mean <- function(x) {
  
  if (
    length(x) == 0 ||
    all(is.na(x))
  ) {
    return(0)
  }
  
  mean(
    x,
    na.rm = TRUE
  )
}


safe_sum <- function(x) {
  
  if (
    length(x) == 0 ||
    all(is.na(x))
  ) {
    return(0)
  }
  
  sum(
    x,
    na.rm = TRUE
  )
}


clean_label <- function(x) {
  
  x <- as.character(x)
  
  x <- gsub(
    "_",
    " ",
    x
  )
  
  tools::toTitleCase(x)
}


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
            color = "#8b8790"
          )
        )
      ),
      margin = list(
        l = 10,
        r = 10,
        t = 10,
        b = 10
      ),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)"
    )
}


# ============================================================
# 7. USER INTERFACE
# ============================================================

ui <- page_fillable(
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#C13584",
    secondary = "#833AB4",
    success = "#2E7D32",
    info = "#1565C0",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  
  tags$head(
    
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1"
    ),
    
    tags$style(
      HTML(
        "
        body {
          margin: 0;
          background:
            linear-gradient(
              180deg,
              #faf9fc 0%,
              #f5f3f7 100%
            );
          color: #252229;
          font-family: Inter, Arial, sans-serif;
        }

        .container-fluid {
          max-width: 1640px;
          margin: 0 auto;
          padding-left: 24px;
          padding-right: 24px;
        }

        .row {
          --bs-gutter-x: 14px;
          --bs-gutter-y: 14px;
        }

        /* HERO */

        .hero {
          margin-top: 20px;
          margin-bottom: 15px;
          border-radius: 24px;
          padding: 26px 30px;
          position: relative;
          overflow: hidden;
          color: white;
          background:
            linear-gradient(
              115deg,
              #833AB4 0%,
              #C13584 34%,
              #E1306C 67%,
              #F77737 100%
            );
          box-shadow:
            0 18px 42px rgba(
              91,
              43,
              112,
              0.18
            );
        }

        .hero:before {
          content: '';
          position: absolute;
          width: 330px;
          height: 330px;
          border-radius: 50%;
          right: -105px;
          top: -170px;
          background: rgba(
            255,
            255,
            255,
            0.10
          );
        }

        .hero:after {
          content: '';
          position: absolute;
          width: 180px;
          height: 180px;
          border-radius: 50%;
          left: 48%;
          bottom: -145px;
          background: rgba(
            255,
            255,
            255,
            0.06
          );
        }

        .hero-inner {
          position: relative;
          z-index: 2;
          display: flex;
          align-items: center;
          gap: 18px;
        }

        .hero-logo {
          width: 68px;
          height: 68px;
          object-fit: contain;
          padding: 10px;
          border-radius: 18px;
          background: rgba(
            255,
            255,
            255,
            0.15
          );
          border: 1px solid rgba(
            255,
            255,
            255,
            0.18
          );
        }

        .hero-logo-fallback {
          width: 68px;
          height: 68px;
          border-radius: 18px;
          display: flex;
          align-items: center;
          justify-content: center;
          background: rgba(
            255,
            255,
            255,
            0.15
          );
          font-size: 31px;
        }

        .hero-title {
          margin: 0;
          font-size: 30px;
          font-weight: 850;
          letter-spacing: -0.9px;
        }

        .hero-subtitle {
          margin: 5px 0 0 0;
          font-size: 13px;
          opacity: 0.88;
        }

        .hero-meta {
          margin-left: auto;
          display: flex;
          gap: 8px;
          align-items: center;
        }

        .hero-pill {
          padding: 8px 11px;
          border-radius: 999px;
          background: rgba(
            255,
            255,
            255,
            0.14
          );
          border: 1px solid rgba(
            255,
            255,
            255,
            0.18
          );
          font-size: 10px;
          font-weight: 750;
          letter-spacing: 0.35px;
        }

        /* FILTERS */

        .filter-panel {
          background: #ffffff;
          border: 1px solid #ebe7ef;
          border-radius: 18px;
          padding: 16px 18px 12px;
          box-shadow:
            0 5px 18px rgba(
              31,
              24,
              39,
              0.045
            );
          margin-bottom: 14px;
        }

        .filter-heading {
          display: flex;
          align-items: center;
          justify-content: space-between;
          margin-bottom: 10px;
        }

        .filter-title {
          font-size: 11px;
          text-transform: uppercase;
          letter-spacing: 1.15px;
          font-weight: 850;
          color: #69636f;
        }

        .filter-note {
          font-size: 10px;
          color: #a19ba5;
        }

        .control-label {
          font-size: 10px;
          font-weight: 750;
          color: #66616b;
          margin-bottom: 4px;
        }

        .form-group {
          margin-bottom: 7px;
        }

        .form-control,
        .selectize-input {
          min-height: 37px !important;
          border-radius: 9px !important;
          border: 1px solid #ded9e3 !important;
          box-shadow: none !important;
          font-size: 12px !important;
          background: #fff !important;
        }

        .selectize-input.focus {
          border-color: #C13584 !important;
          box-shadow:
            0 0 0 3px rgba(
              193,
              53,
              132,
              0.08
            ) !important;
        }

        .reset-btn,
        .download-btn {
          width: 100%;
          min-height: 37px;
          border-radius: 9px;
          font-size: 11px;
          font-weight: 750;
          margin-top: 21px;
        }

        .download-btn {
          background: #252229 !important;
          border-color: #252229 !important;
          color: white !important;
        }

        /* KPI */

        .kpi-grid {
          display: grid;
          grid-template-columns:
            repeat(6, minmax(0, 1fr));
          gap: 11px;
          margin-bottom: 14px;
        }

        .kpi-card {
          background: #ffffff;
          border: 1px solid #ebe7ef;
          border-radius: 15px;
          min-height: 96px;
          padding: 14px 15px;
          position: relative;
          overflow: hidden;
          box-shadow:
            0 4px 15px rgba(
              28,
              22,
              35,
              0.04
            );
        }

        .kpi-card:before {
          content: '';
          position: absolute;
          left: 0;
          top: 0;
          bottom: 0;
          width: 3px;
          background:
            linear-gradient(
              180deg,
              #833AB4,
              #E1306C,
              #F77737
            );
        }

        .kpi-top {
          display: flex;
          align-items: center;
          justify-content: space-between;
        }

        .kpi-label {
          font-size: 9px;
          text-transform: uppercase;
          letter-spacing: 0.9px;
          color: #89838e;
          font-weight: 850;
        }

        .kpi-icon {
          width: 25px;
          height: 25px;
          border-radius: 8px;
          display: flex;
          align-items: center;
          justify-content: center;
          background: #faf2f7;
          color: #C13584;
          font-size: 11px;
        }

        .kpi-value {
          margin-top: 7px;
          font-size: 23px;
          line-height: 1;
          font-weight: 850;
          letter-spacing: -0.7px;
          color: #27232b;
        }

        .kpi-description {
          margin-top: 6px;
          font-size: 9px;
          color: #aaa4ae;
        }

        /* SECTION CARDS */

        .section-card {
          height: 100%;
          background: #ffffff;
          border: 1px solid #ebe7ef;
          border-radius: 17px;
          padding: 15px 16px 11px;
          box-shadow:
            0 4px 16px rgba(
              28,
              22,
              35,
              0.04
            );
        }

        .section-header {
          display: flex;
          justify-content: space-between;
          align-items: flex-start;
          margin-bottom: 4px;
        }

        .section-title {
          font-size: 13px;
          font-weight: 850;
          color: #2b2730;
          margin: 0;
        }

        .section-subtitle {
          margin-top: 3px;
          font-size: 10px;
          color: #96909b;
        }

        .section-tag {
          padding: 4px 8px;
          border-radius: 999px;
          background: #f8f4f8;
          color: #8a637e;
          font-size: 8px;
          text-transform: uppercase;
          letter-spacing: 0.7px;
          font-weight: 850;
        }

        /* INSIGHTS */

        .insight-grid {
          display: grid;
          grid-template-columns:
            repeat(5, minmax(0, 1fr));
          gap: 9px;
          margin-top: 10px;
        }

        .insight-box {
          min-height: 105px;
          padding: 13px;
          border-radius: 13px;
          background:
            linear-gradient(
              135deg,
              #fff8fc,
              #faf7ff
            );
          border: 1px solid #eee0eb;
        }

        .insight-icon {
          color: #C13584;
          font-size: 12px;
          margin-bottom: 7px;
        }

        .insight-title {
          font-size: 10px;
          font-weight: 850;
          text-transform: uppercase;
          letter-spacing: 0.6px;
          color: #77707c;
        }

        .insight-value {
          margin-top: 5px;
          font-size: 14px;
          font-weight: 850;
          color: #322c35;
        }

        .insight-description {
          margin-top: 4px;
          font-size: 9px;
          line-height: 1.4;
          color: #918b95;
        }

        /* TABLE */

        .dataTables_wrapper {
          font-size: 11px;
        }

        .dataTables_filter input {
          border-radius: 8px !important;
          border: 1px solid #ddd8e1 !important;
          padding: 5px 9px !important;
        }

        table.dataTable thead th {
          background: #faf9fb !important;
          color: #68626d !important;
          border-bottom: 1px solid #e8e3eb !important;
          font-size: 9px !important;
          text-transform: uppercase;
          letter-spacing: 0.5px;
        }

        table.dataTable tbody td {
          border-bottom: 1px solid #f0edf2 !important;
          vertical-align: middle !important;
        }

        table.dataTable tbody tr:hover {
          background: #fff9fc !important;
        }

        /* FOOTER */

        .footer {
          text-align: center;
          padding: 22px 0 30px;
          font-size: 10px;
          color: #aaa4ae;
        }

        /* RESPONSIVE */

        @media (max-width: 1300px) {

          .kpi-grid {
            grid-template-columns:
              repeat(3, minmax(0, 1fr));
          }

          .insight-grid {
            grid-template-columns:
              repeat(3, minmax(0, 1fr));
          }

          .hero-meta {
            display: none;
          }
        }

        @media (max-width: 800px) {

          .container-fluid {
            padding-left: 12px;
            padding-right: 12px;
          }

          .hero {
            padding: 21px;
          }

          .hero-title {
            font-size: 22px;
          }

          .hero-logo,
          .hero-logo-fallback {
            width: 55px;
            height: 55px;
          }

          .kpi-grid {
            grid-template-columns:
              repeat(2, minmax(0, 1fr));
          }

          .insight-grid {
            grid-template-columns:
              1fr;
          }
        }
        "
      )
    )
  ),
  
  
  # ==========================================================
  # PAGE
  # ==========================================================
  
  div(
    class = "container-fluid",
    
    # HERO
    div(
      class = "hero",
      
      div(
        class = "hero-inner",
        
        uiOutput("hero_logo"),
        
        div(
          
          h1(
            class = "hero-title",
            "Instagram Analytics"
          ),
          
          p(
            class = "hero-subtitle",
            "A Big Data Approach to Content Performance and Audience Engagement"
          )
        ),
        
        div(
          class = "hero-meta",
          
          div(
            class = "hero-pill",
            icon("database"),
            " BIG DATA"
          ),
          
          div(
            class = "hero-pill",
            icon("chart-line"),
            " LIVE ANALYTICS"
          )
        )
      )
    ),
    
    
    # FILTER PANEL
    div(
      class = "filter-panel",
      
      div(
        class = "filter-heading",
        
        div(
          class = "filter-title",
          icon("sliders"),
          " Analytics Controls"
        ),
        
        div(
          class = "filter-note",
          "All visualizations update automatically"
        )
      ),
      
      fluidRow(
        
        column(
          width = 2,
          
          dateRangeInput(
            "date_filter",
            "Date Range",
            start = min(
              instagram_data$post_date,
              na.rm = TRUE
            ),
            end = max(
              instagram_data$post_date,
              na.rm = TRUE
            ),
            min = min(
              instagram_data$post_date,
              na.rm = TRUE
            ),
            max = max(
              instagram_data$post_date,
              na.rm = TRUE
            ),
            format = "dd M yyyy",
            separator = " → "
          )
        ),
        
        column(
          width = 2,
          
          selectInput(
            "account_type_filter",
            "Account Type",
            choices = c(
              "All",
              sort(
                unique(
                  instagram_data$account_type
                )
              )
            ),
            selected = "All"
          )
        ),
        
        column(
          width = 2,
          
          selectInput(
            "media_type_filter",
            "Media Type",
            choices = c(
              "All",
              sort(
                unique(
                  instagram_data$media_type
                )
              )
            ),
            selected = "All"
          )
        ),
        
        column(
          width = 2,
          
          selectInput(
            "category_filter",
            "Content Category",
            choices = c(
              "All",
              sort(
                unique(
                  instagram_data$content_category
                )
              )
            ),
            selected = "All"
          )
        ),
        
        column(
          width = 2,
          
          selectInput(
            "traffic_filter",
            "Traffic Source",
            choices = c(
              "All",
              sort(
                unique(
                  instagram_data$traffic_source
                )
              )
            ),
            selected = "All"
          )
        ),
        
        column(
          width = 2,
          
          selectInput(
            "performance_filter",
            "Performance",
            choices = c(
              "All",
              sort(
                unique(
                  instagram_data$performance_bucket_label
                )
              )
            ),
            selected = "All"
          )
        )
      ),
      
      fluidRow(
        
        column(
          width = 2,
          
          selectInput(
            "trend_metric",
            "Trend Metric",
            choices = c(
              "Total Engagement" = "total_engagement",
              "Likes" = "likes",
              "Comments" = "comments",
              "Shares" = "shares",
              "Saves" = "saves",
              "Reach" = "reach",
              "Impressions" = "impressions",
              "Followers Gained" = "followers_gained"
            ),
            selected = "total_engagement"
          )
        ),
        
        column(
          width = 2,
          
          actionButton(
            "reset_filters",
            "Reset Filters",
            icon = icon("rotate-left"),
            class = "btn btn-outline-secondary reset-btn"
          )
        ),
        
        column(
          width = 2,
          
          downloadButton(
            "download_data",
            "Download Data",
            class = "btn btn-dark download-btn"
          )
        ),
        
        column(
          width = 6,
          
          div(
            style = "
              text-align:right;
              padding-top:30px;
              font-size:10px;
              color:#99939e;
            ",
            
            icon("circle-check"),
            " ",
            
            strong(
              textOutput(
                "record_count",
                inline = TRUE
              )
            ),
            
            " records in current view"
          )
        )
      )
    ),
    
    
    # KPI CARDS
    div(
      class = "kpi-grid",
      
      div(
        class = "kpi-card",
        
        div(
          class = "kpi-top",
          
          div(
            class = "kpi-label",
            "Posts"
          ),
          
          div(
            class = "kpi-icon",
            icon("file-lines")
          )
        ),
        
        div(
          class = "kpi-value",
          textOutput("kpi_posts")
        ),
        
        div(
          class = "kpi-description",
          "Filtered records"
        )
      ),
      
      div(
        class = "kpi-card",
        
        div(
          class = "kpi-top",
          
          div(
            class = "kpi-label",
            "Reach"
          ),
          
          div(
            class = "kpi-icon",
            icon("users")
          )
        ),
        
        div(
          class = "kpi-value",
          textOutput("kpi_reach")
        ),
        
        div(
          class = "kpi-description",
          "Total audience reached"
        )
      ),
      
      div(
        class = "kpi-card",
        
        div(
          class = "kpi-top",
          
          div(
            class = "kpi-label",
            "Impressions"
          ),
          
          div(
            class = "kpi-icon",
            icon("eye")
          )
        ),
        
        div(
          class = "kpi-value",
          textOutput("kpi_impressions")
        ),
        
        div(
          class = "kpi-description",
          "Total content views"
        )
      ),
      
      div(
        class = "kpi-card",
        
        div(
          class = "kpi-top",
          
          div(
            class = "kpi-label",
            "Engagement"
          ),
          
          div(
            class = "kpi-icon",
            icon("heart")
          )
        ),
        
        div(
          class = "kpi-value",
          textOutput("kpi_engagement")
        ),
        
        div(
          class = "kpi-description",
          "All engagement actions"
        )
      ),
      
      div(
        class = "kpi-card",
        
        div(
          class = "kpi-top",
          
          div(
            class = "kpi-label",
            "Engagement Rate"
          ),
          
          div(
            class = "kpi-icon",
            icon("percent")
          )
        ),
        
        div(
          class = "kpi-value",
          textOutput("kpi_engagement_rate")
        ),
        
        div(
          class = "kpi-description",
          "Average engagement rate"
        )
      ),
      
      div(
        class = "kpi-card",
        
        div(
          class = "kpi-top",
          
          div(
            class = "kpi-label",
            "Followers Gained"
          ),
          
          div(
            class = "kpi-icon",
            icon("user-plus")
          )
        ),
        
        div(
          class = "kpi-value",
          textOutput("kpi_followers")
        ),
        
        div(
          class = "kpi-description",
          "Total followers gained"
        )
      )
    ),
    
    
    # TREND + PERFORMANCE
    fluidRow(
      
      column(
        width = 8,
        
        div(
          class = "section-card",
          
          div(
            class = "section-header",
            
            div(
              
              div(
                class = "section-title",
                "Engagement Trend"
              ),
              
              div(
                class = "section-subtitle",
                "Daily performance based on the selected metric"
              )
            ),
            
            div(
              class = "section-tag",
              "Time Series"
            )
          ),
          
          plotlyOutput(
            "trend_plot",
            height = "350px"
          )
        )
      ),
      
      column(
        width = 4,
        
        div(
          class = "section-card",
          
          div(
            class = "section-header",
            
            div(
              
              div(
                class = "section-title",
                "Performance Mix"
              ),
              
              div(
                class = "section-subtitle",
                "Distribution of posts by performance bucket"
              )
            ),
            
            div(
              class = "section-tag",
              "Distribution"
            )
          ),
          
          plotlyOutput(
            "performance_plot",
            height = "350px"
          )
        )
      )
    ),
    
    br(),
    
    
    # MEDIA + TRAFFIC
    fluidRow(
      
      column(
        width = 6,
        
        div(
          class = "section-card",
          
          div(
            class = "section-header",
            
            div(
              
              div(
                class = "section-title",
                "Media Type Performance"
              ),
              
              div(
                class = "section-subtitle",
                "Average engagement rate by media format"
              )
            ),
            
            div(
              class = "section-tag",
              "Comparison"
            )
          ),
          
          plotlyOutput(
            "media_plot",
            height = "320px"
          )
        )
      ),
      
      column(
        width = 6,
        
        div(
          class = "section-card",
          
          div(
            class = "section-header",
            
            div(
              
              div(
                class = "section-title",
                "Traffic Source Performance"
              ),
              
              div(
                class = "section-subtitle",
                "Average engagement rate by traffic source"
              )
            ),
            
            div(
              class = "section-tag",
              "Acquisition"
            )
          ),
          
          plotlyOutput(
            "traffic_plot",
            height = "320px"
          )
        )
      )
    ),
    
    br(),
    
    
    # CATEGORY + HEATMAP
    fluidRow(
      
      column(
        width = 6,
        
        div(
          class = "section-card",
          
          div(
            class = "section-header",
            
            div(
              
              div(
                class = "section-title",
                "Content Category Ranking"
              ),
              
              div(
                class = "section-subtitle",
                "Categories ranked by average engagement rate"
              )
            ),
            
            div(
              class = "section-tag",
              "Ranking"
            )
          ),
          
          plotlyOutput(
            "category_plot",
            height = "400px"
          )
        )
      ),
      
      column(
        width = 6,
        
        div(
          class = "section-card",
          
          div(
            class = "section-header",
            
            div(
              
              div(
                class = "section-title",
                "Posting Time Heatmap"
              ),
              
              div(
                class = "section-subtitle",
                "Average engagement rate by day and posting hour"
              )
            ),
            
            div(
              class = "section-tag",
              "Timing"
            )
          ),
          
          plotlyOutput(
            "heatmap_plot",
            height = "400px"
          )
        )
      )
    ),
    
    br(),
    
    
    # COMPOSITION + SCATTER
    fluidRow(
      
      column(
        width = 5,
        
        div(
          class = "section-card",
          
          div(
            class = "section-header",
            
            div(
              
              div(
                class = "section-title",
                "Engagement Composition"
              ),
              
              div(
                class = "section-subtitle",
                "Contribution of individual engagement actions"
              )
            ),
            
            div(
              class = "section-tag",
              "Composition"
            )
          ),
          
          plotlyOutput(
            "composition_plot",
            height = "320px"
          )
        )
      ),
      
      column(
        width = 7,
        
        div(
          class = "section-card",
          
          div(
            class = "section-header",
            
            div(
              
              div(
                class = "section-title",
                "Reach vs Engagement"
              ),
              
              div(
                class = "section-subtitle",
                "Relationship between audience reach and total engagement"
              )
            ),
            
            div(
              class = "section-tag",
              "Relationship"
            )
          ),
          
          plotlyOutput(
            "scatter_plot",
            height = "320px"
          )
        )
      )
    ),
    
    br(),
    
    
    # CTA + ACCOUNT
    fluidRow(
      
      column(
        width = 6,
        
        div(
          class = "section-card",
          
          div(
            class = "section-header",
            
            div(
              
              div(
                class = "section-title",
                "Call-to-Action Impact"
              ),
              
              div(
                class = "section-subtitle",
                "Average engagement rate with and without CTA"
              )
            ),
            
            div(
              class = "section-tag",
              "CTA Analysis"
            )
          ),
          
          plotlyOutput(
            "cta_plot",
            height = "300px"
          )
        )
      ),
      
      column(
        width = 6,
        
        div(
          class = "section-card",
          
          div(
            class = "section-header",
            
            div(
              
              div(
                class = "section-title",
                "Account Performance"
              ),
              
              div(
                class = "section-subtitle",
                "Top accounts by average engagement rate"
              )
            ),
            
            div(
              class = "section-tag",
              "Top 10"
            )
          ),
          
          plotlyOutput(
            "account_plot",
            height = "300px"
          )
        )
      )
    ),
    
    br(),
    
    
    # AUTOMATED INSIGHTS
    div(
      class = "section-card",
      
      div(
        class = "section-header",
        
        div(
          
          div(
            class = "section-title",
            "Automated Insights"
          ),
          
          div(
            class = "section-subtitle",
            "Key observations generated from the current filtered dataset"
          )
        ),
        
        div(
          class = "section-tag",
          "Intelligence"
        )
      ),
      
      uiOutput("insights")
    ),
    
    br(),
    
    
    # TOP POSTS
    div(
      class = "section-card",
      
      div(
        class = "section-header",
        
        div(
          
          div(
            class = "section-title",
            "Top 10 Performing Posts"
          ),
          
          div(
            class = "section-subtitle",
            "Highest-engagement posts within the current filter selection"
          )
        ),
        
        div(
          class = "section-tag",
          "Detailed View"
        )
      ),
      
      DTOutput("top_posts")
    ),
    
    
    # FOOTER
    div(
      class = "footer",
      "Instagram Analytics • Big Data Analytics Project • R + Shiny + Plotly + DT"
    )
  )
)


# ============================================================
# 8. SERVER
# ============================================================

server <- function(input, output, session) {
  
  
  # ==========================================================
  # HERO LOGO
  # ==========================================================
  
  output$hero_logo <- renderUI({
    
    logo_path <- file.path(
      "www",
      "instagram-logo.png"
    )
    
    if (file.exists(logo_path)) {
      
      tags$img(
        src = "instagram-logo.png",
        class = "hero-logo",
        alt = "Instagram"
      )
      
    } else {
      
      div(
        class = "hero-logo-fallback",
        icon("instagram")
      )
    }
  })
  
  
  # ==========================================================
  # FILTERED DATA
  # ==========================================================
  
  filtered_data <- reactive({
    
    d <- instagram_data
    
    if (
      !is.null(input$date_filter) &&
      length(input$date_filter) == 2 &&
      all(!is.na(input$date_filter))
    ) {
      
      d <- d %>%
        filter(
          post_date >= input$date_filter[1],
          post_date <= input$date_filter[2]
        )
    }
    
    if (
      !is.null(input$account_type_filter) &&
      input$account_type_filter != "All"
    ) {
      
      d <- d %>%
        filter(
          account_type ==
            input$account_type_filter
        )
    }
    
    if (
      !is.null(input$media_type_filter) &&
      input$media_type_filter != "All"
    ) {
      
      d <- d %>%
        filter(
          media_type ==
            input$media_type_filter
        )
    }
    
    if (
      !is.null(input$category_filter) &&
      input$category_filter != "All"
    ) {
      
      d <- d %>%
        filter(
          content_category ==
            input$category_filter
        )
    }
    
    if (
      !is.null(input$traffic_filter) &&
      input$traffic_filter != "All"
    ) {
      
      d <- d %>%
        filter(
          traffic_source ==
            input$traffic_filter
        )
    }
    
    if (
      !is.null(input$performance_filter) &&
      input$performance_filter != "All"
    ) {
      
      d <- d %>%
        filter(
          performance_bucket_label ==
            input$performance_filter
        )
    }
    
    d
  })
  
  
  # ==========================================================
  # RESET FILTERS
  # ==========================================================
  
  observeEvent(
    input$reset_filters,
    {
      
      updateDateRangeInput(
        session,
        "date_filter",
        start = min(
          instagram_data$post_date,
          na.rm = TRUE
        ),
        end = max(
          instagram_data$post_date,
          na.rm = TRUE
        )
      )
      
      updateSelectInput(
        session,
        "account_type_filter",
        selected = "All"
      )
      
      updateSelectInput(
        session,
        "media_type_filter",
        selected = "All"
      )
      
      updateSelectInput(
        session,
        "category_filter",
        selected = "All"
      )
      
      updateSelectInput(
        session,
        "traffic_filter",
        selected = "All"
      )
      
      updateSelectInput(
        session,
        "performance_filter",
        selected = "All"
      )
      
      updateSelectInput(
        session,
        "trend_metric",
        selected = "total_engagement"
      )
    }
  )
  
  
  # ==========================================================
  # RECORD COUNT
  # ==========================================================
  
  output$record_count <- renderText({
    
    format(
      nrow(filtered_data()),
      big.mark = ","
    )
  })
  
  
  # ==========================================================
  # KPI CARDS
  # ==========================================================
  
  output$kpi_posts <- renderText({
    
    fmt_compact(
      nrow(filtered_data())
    )
  })
  
  
  output$kpi_reach <- renderText({
    
    fmt_compact(
      safe_sum(
        filtered_data()$reach
      )
    )
  })
  
  
  output$kpi_impressions <- renderText({
    
    fmt_compact(
      safe_sum(
        filtered_data()$impressions
      )
    )
  })
  
  
  output$kpi_engagement <- renderText({
    
    fmt_compact(
      safe_sum(
        filtered_data()$total_engagement
      )
    )
  })
  
  
  output$kpi_engagement_rate <- renderText({
    
    fmt_pct(
      safe_mean(
        filtered_data()$engagement_rate
      )
    )
  })
  
  
  output$kpi_followers <- renderText({
    
    fmt_compact(
      safe_sum(
        filtered_data()$followers_gained
      )
    )
  })
  
  
  # ==========================================================
  # ENGAGEMENT TREND
  # ==========================================================
  
  output$trend_plot <- renderPlotly({
    
    d <- filtered_data()
    
    if (nrow(d) == 0) {
      return(
        empty_plot(
          "No data available for the selected filters"
        )
      )
    }
    
    metric <- input$trend_metric
    
    if (
      is.null(metric) ||
      !metric %in% names(d)
    ) {
      metric <- "total_engagement"
    }
    
    trend_data <- d %>%
      group_by(post_date) %>%
      summarise(
        value = sum(
          .data[[metric]],
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      arrange(post_date)
    
    metric_label <- switch(
      metric,
      total_engagement = "Total Engagement",
      likes = "Likes",
      comments = "Comments",
      shares = "Shares",
      saves = "Saves",
      reach = "Reach",
      impressions = "Impressions",
      followers_gained = "Followers Gained",
      "Metric"
    )
    
    plot_ly(
      trend_data,
      x = ~post_date,
      y = ~value,
      type = "scatter",
      mode = "lines+markers",
      line = list(
        width = 2.5
      ),
      marker = list(
        size = 4.5
      ),
      hovertemplate =
        paste0(
          "<b>%{x|%d %b %Y}</b><br>",
          metric_label,
          ": %{y:,}",
          "<extra></extra>"
        )
    ) %>%
      
      layout(
        xaxis = list(
          title = "",
          showgrid = FALSE
        ),
        yaxis = list(
          title = metric_label,
          gridcolor = "#eeeaf0"
        ),
        hovermode = "x unified",
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        margin = list(
          l = 65,
          r = 20,
          t = 10,
          b = 45
        )
      )
  })
  
  
  # ==========================================================
  # PERFORMANCE MIX
  # ==========================================================
  
  output$performance_plot <- renderPlotly({
    
    d <- filtered_data()
    
    if (nrow(d) == 0) {
      return(
        empty_plot(
          "No performance data available"
        )
      )
    }
    
    perf_data <- d %>%
      count(
        performance_bucket_label,
        name = "posts"
      ) %>%
      arrange(
        desc(posts)
      )
    
    perf_data$label <- clean_label(
      perf_data$performance_bucket_label
    )
    
    plot_ly(
      perf_data,
      labels = ~label,
      values = ~posts,
      type = "pie",
      hole = 0.62,
      textinfo = "label+percent",
      sort = FALSE,
      
      hovertemplate =
        "<b>%{label}</b><br>%{value:,} posts<br>%{percent}<extra></extra>"
    ) %>%
      
      layout(
        showlegend = TRUE,
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.05,
          font = list(
            size = 10
          )
        ),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        margin = list(
          l = 5,
          r = 5,
          t = 5,
          b = 35
        )
      )
  })
  
  
  # ==========================================================
  # MEDIA TYPE PERFORMANCE
  # ==========================================================
  
  output$media_plot <- renderPlotly({
    
    d <- filtered_data()
    
    if (nrow(d) == 0) {
      return(
        empty_plot(
          "No media type data available"
        )
      )
    }
    
    media_data <- d %>%
      group_by(media_type) %>%
      summarise(
        avg_engagement =
          safe_mean(engagement_rate),
        posts = n(),
        .groups = "drop"
      ) %>%
      arrange(
        avg_engagement
      )
    
    media_data$label <- clean_label(
      media_data$media_type
    )
    
    plot_ly(
      media_data,
      x = ~avg_engagement,
      y = ~reorder(
        label,
        avg_engagement
      ),
      type = "bar",
      orientation = "h",
      text = ~fmt_pct_vec(
        avg_engagement
      ),
      textposition = "outside",
      
      hovertemplate =
        paste0(
          "<b>%{y}</b><br>",
          "Average Engagement: %{x:.2%}<br>",
          "Posts: %{customdata:,}",
          "<extra></extra>"
        ),
      
      customdata = ~posts
    ) %>%
      
      layout(
        xaxis = list(
          title = "Average Engagement Rate",
          tickformat = ".1%",
          gridcolor = "#eeeaf0"
        ),
        yaxis = list(
          title = ""
        ),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        margin = list(
          l = 105,
          r = 65,
          t = 10,
          b = 45
        )
      )
  })
  
  
  # ==========================================================
  # TRAFFIC SOURCE PERFORMANCE
  # ==========================================================
  
  output$traffic_plot <- renderPlotly({
    
    d <- filtered_data()
    
    if (nrow(d) == 0) {
      return(
        empty_plot(
          "No traffic source data available"
        )
      )
    }
    
    traffic_data <- d %>%
      group_by(traffic_source) %>%
      summarise(
        avg_engagement =
          safe_mean(engagement_rate),
        total_engagement =
          safe_sum(total_engagement),
        posts = n(),
        .groups = "drop"
      ) %>%
      arrange(
        avg_engagement
      )
    
    traffic_data$label <- clean_label(
      traffic_data$traffic_source
    )
    
    plot_ly(
      traffic_data,
      x = ~avg_engagement,
      y = ~reorder(
        label,
        avg_engagement
      ),
      type = "bar",
      orientation = "h",
      
      hovertemplate =
        paste0(
          "<b>%{y}</b><br>",
          "Average Engagement: %{x:.2%}<br>",
          "Posts: %{customdata[1]:,}<br>",
          "Total Engagement: %{customdata[2]:,}",
          "<extra></extra>"
        ),
      
      customdata = ~cbind(
        posts,
        total_engagement
      )
    ) %>%
      
      layout(
        xaxis = list(
          title = "Average Engagement Rate",
          tickformat = ".1%",
          gridcolor = "#eeeaf0"
        ),
        yaxis = list(
          title = ""
        ),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        margin = list(
          l = 110,
          r = 55,
          t = 10,
          b = 45
        )
      )
  })
  
  
  # ==========================================================
  # CONTENT CATEGORY RANKING
  # ==========================================================
  
  output$category_plot <- renderPlotly({
    
    d <- filtered_data()
    
    if (nrow(d) == 0) {
      return(
        empty_plot(
          "No category data available"
        )
      )
    }
    
    category_data <- d %>%
      group_by(content_category) %>%
      summarise(
        avg_engagement =
          safe_mean(engagement_rate),
        total_engagement =
          safe_sum(total_engagement),
        posts = n(),
        .groups = "drop"
      ) %>%
      arrange(
        avg_engagement
      )
    
    category_data$label <- clean_label(
      category_data$content_category
    )
    
    plot_ly(
      category_data,
      x = ~avg_engagement,
      y = ~reorder(
        label,
        avg_engagement
      ),
      type = "bar",
      orientation = "h",
      
      hovertemplate =
        paste0(
          "<b>%{y}</b><br>",
          "Average Engagement: %{x:.2%}<br>",
          "Posts: %{customdata[1]:,}<br>",
          "Total Engagement: %{customdata[2]:,}",
          "<extra></extra>"
        ),
      
      customdata = ~cbind(
        posts,
        total_engagement
      )
    ) %>%
      
      layout(
        xaxis = list(
          title = "Average Engagement Rate",
          tickformat = ".1%",
          gridcolor = "#eeeaf0"
        ),
        yaxis = list(
          title = ""
        ),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        margin = list(
          l = 130,
          r = 55,
          t = 10,
          b = 45
        )
      )
  })
  
  
  # ==========================================================
  # POSTING TIME HEATMAP
  # ==========================================================
  
  output$heatmap_plot <- renderPlotly({
    
    d <- filtered_data()
    
    if (nrow(d) == 0) {
      return(
        empty_plot(
          "No posting-time data available"
        )
      )
    }
    
    days <- c(
      "Monday",
      "Tuesday",
      "Wednesday",
      "Thursday",
      "Friday",
      "Saturday",
      "Sunday"
    )
    
    heat_data <- d %>%
      mutate(
        day_of_week =
          as.character(day_of_week),
        post_hour =
          as.numeric(post_hour)
      ) %>%
      group_by(
        day_of_week,
        post_hour
      ) %>%
      summarise(
        engagement =
          safe_mean(engagement_rate),
        posts = n(),
        .groups = "drop"
      )
    
    grid <- expand.grid(
      day_of_week = days,
      post_hour = 0:23,
      stringsAsFactors = FALSE
    )
    
    heat_data <- grid %>%
      left_join(
        heat_data,
        by = c(
          "day_of_week",
          "post_hour"
        )
      )
    
    heat_data$engagement[
      is.na(heat_data$engagement)
    ] <- 0
    
    heat_data$posts[
      is.na(heat_data$posts)
    ] <- 0
    
    z_matrix <- matrix(
      heat_data$engagement,
      nrow = 7,
      ncol = 24,
      byrow = FALSE
    )
    
    post_matrix <- matrix(
      heat_data$posts,
      nrow = 7,
      ncol = 24,
      byrow = FALSE
    )
    
    plot_ly(
      x = 0:23,
      y = days,
      z = z_matrix,
      type = "heatmap",
      customdata = post_matrix,
      
      hovertemplate =
        paste0(
          "<b>%{y}</b><br>",
          "Hour: %{x}:00<br>",
          "Average Engagement: %{z:.2%}<br>",
          "Posts: %{customdata:,}",
          "<extra></extra>"
        )
    ) %>%
      
      layout(
        xaxis = list(
          title = "Posting Hour",
          dtick = 2
        ),
        yaxis = list(
          title = ""
        ),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        margin = list(
          l = 85,
          r = 15,
          t = 10,
          b = 50
        )
      )
  })
  
  
  # ==========================================================
  # ENGAGEMENT COMPOSITION
  # ==========================================================
  
  output$composition_plot <- renderPlotly({
    
    d <- filtered_data()
    
    if (nrow(d) == 0) {
      return(
        empty_plot(
          "No engagement data available"
        )
      )
    }
    
    composition_data <- data.frame(
      
      type = c(
        "Likes",
        "Comments",
        "Shares",
        "Saves"
      ),
      
      value = c(
        safe_sum(d$likes),
        safe_sum(d$comments),
        safe_sum(d$shares),
        safe_sum(d$saves)
      )
    )
    
    plot_ly(
      composition_data,
      labels = ~type,
      values = ~value,
      type = "pie",
      hole = 0.58,
      
      hovertemplate =
        "<b>%{label}</b><br>%{value:,} interactions<br>%{percent}<extra></extra>"
    ) %>%
      
      layout(
        showlegend = TRUE,
        
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.05,
          font = list(
            size = 10
          )
        ),
        
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        
        margin = list(
          l = 5,
          r = 5,
          t = 5,
          b = 35
        )
      )
  })
  
  
  # ==========================================================
  # REACH VS ENGAGEMENT
  # ==========================================================
  
  output$scatter_plot <- renderPlotly({
    
    d <- filtered_data()
    
    if (nrow(d) == 0) {
      return(
        empty_plot(
          "No scatter data available"
        )
      )
    }
    
    scatter_data <- d %>%
      filter(
        !is.na(reach),
        !is.na(total_engagement),
        reach > 0,
        total_engagement > 0
      )
    
    if (nrow(scatter_data) == 0) {
      return(
        empty_plot(
          "No valid reach/engagement data"
        )
      )
    }
    
    if (nrow(scatter_data) > 7000) {
      
      set.seed(42)
      
      scatter_data <- scatter_data %>%
        slice_sample(
          n = 7000
        )
    }
    
    scatter_data$hover_text <- paste0(
      "<b>Post:</b> ",
      scatter_data$post_id,
      "<br><b>Media:</b> ",
      clean_label(scatter_data$media_type),
      "<br><b>Category:</b> ",
      clean_label(scatter_data$content_category),
      "<br><b>Reach:</b> ",
      fmt_integer(scatter_data$reach),
      "<br><b>Engagement:</b> ",
      fmt_integer(scatter_data$total_engagement)
    )
    
    plot_ly(
      scatter_data,
      x = ~reach,
      y = ~total_engagement,
      type = "scatter",
      mode = "markers",
      
      marker = list(
        size = 6,
        opacity = 0.48
      ),
      
      text = ~hover_text,
      
      hovertemplate =
        "%{text}<extra></extra>"
    ) %>%
      
      layout(
        xaxis = list(
          title = "Reach",
          type = "log",
          gridcolor = "#eeeaf0"
        ),
        yaxis = list(
          title = "Total Engagement",
          type = "log",
          gridcolor = "#eeeaf0"
        ),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        margin = list(
          l = 65,
          r = 20,
          t = 10,
          b = 50
        )
      )
  })
  
  
  # ==========================================================
  # CTA IMPACT
  # ==========================================================
  
  output$cta_plot <- renderPlotly({
    
    d <- filtered_data()
    
    if (nrow(d) == 0) {
      return(
        empty_plot(
          "No CTA data available"
        )
      )
    }
    
    cta_data <- d %>%
      group_by(has_call_to_action) %>%
      summarise(
        avg_engagement =
          safe_mean(engagement_rate),
        posts = n(),
        .groups = "drop"
      )
    
    cta_data$label <- ifelse(
      cta_data$has_call_to_action %in%
        c("true", "yes", "1"),
      "With CTA",
      "Without CTA"
    )
    
    plot_ly(
      cta_data,
      x = ~label,
      y = ~avg_engagement,
      type = "bar",
      
      text = ~fmt_pct_vec(
        avg_engagement
      ),
      
      textposition = "outside",
      
      hovertemplate =
        paste0(
          "<b>%{x}</b><br>",
          "Average Engagement: %{y:.2%}<br>",
          "Posts: %{customdata:,}",
          "<extra></extra>"
        ),
      
      customdata = ~posts
    ) %>%
      
      layout(
        xaxis = list(
          title = ""
        ),
        yaxis = list(
          title = "Average Engagement Rate",
          tickformat = ".1%",
          gridcolor = "#eeeaf0"
        ),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        margin = list(
          l = 70,
          r = 35,
          t = 15,
          b = 50
        )
      )
  })
  
  
  # ==========================================================
  # ACCOUNT PERFORMANCE
  # ==========================================================
  
  output$account_plot <- renderPlotly({
    
    d <- filtered_data()
    
    if (nrow(d) == 0) {
      return(
        empty_plot(
          "No account data available"
        )
      )
    }
    
    account_data <- d %>%
      group_by(account_id) %>%
      summarise(
        avg_engagement =
          safe_mean(engagement_rate),
        posts = n(),
        .groups = "drop"
      ) %>%
      arrange(
        desc(avg_engagement)
      ) %>%
      slice_head(
        n = 10
      ) %>%
      arrange(
        avg_engagement
      )
    
    plot_ly(
      account_data,
      x = ~avg_engagement,
      y = ~reorder(
        as.character(account_id),
        avg_engagement
      ),
      type = "bar",
      orientation = "h",
      
      text = ~fmt_pct_vec(
        avg_engagement
      ),
      
      textposition = "outside",
      
      hovertemplate =
        paste0(
          "<b>Account %{y}</b><br>",
          "Average Engagement: %{x:.2%}<br>",
          "Posts: %{customdata:,}",
          "<extra></extra>"
        ),
      
      customdata = ~posts
    ) %>%
      
      layout(
        xaxis = list(
          title = "Average Engagement Rate",
          tickformat = ".1%",
          gridcolor = "#eeeaf0"
        ),
        yaxis = list(
          title = ""
        ),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        margin = list(
          l = 90,
          r = 60,
          t = 10,
          b = 45
        )
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
          class = "insight-grid",
          
          div(
            class = "insight-box",
            
            div(
              class = "insight-icon",
              icon("circle-info")
            ),
            
            div(
              class = "insight-title",
              "No Data"
            ),
            
            div(
              class = "insight-description",
              "No records match the selected filters."
            )
          )
        )
      )
    }
    
    media_summary <- d %>%
      group_by(media_type) %>%
      summarise(
        engagement =
          safe_mean(engagement_rate),
        .groups = "drop"
      ) %>%
      arrange(
        desc(engagement)
      )
    
    category_summary <- d %>%
      group_by(content_category) %>%
      summarise(
        engagement =
          safe_mean(engagement_rate),
        .groups = "drop"
      ) %>%
      arrange(
        desc(engagement)
      )
    
    traffic_summary <- d %>%
      group_by(traffic_source) %>%
      summarise(
        engagement =
          safe_mean(engagement_rate),
        .groups = "drop"
      ) %>%
      arrange(
        desc(engagement)
      )
    
    hour_summary <- d %>%
      group_by(post_hour) %>%
      summarise(
        engagement =
          safe_mean(engagement_rate),
        .groups = "drop"
      ) %>%
      arrange(
        desc(engagement)
      )
    
    day_summary <- d %>%
      group_by(day_of_week) %>%
      summarise(
        engagement =
          safe_mean(engagement_rate),
        .groups = "drop"
      ) %>%
      arrange(
        desc(engagement)
      )
    
    best_media <- if (
      nrow(media_summary) > 0
    ) {
      as.character(
        media_summary$media_type[1]
      )
    } else {
      "N/A"
    }
    
    best_media_rate <- if (
      nrow(media_summary) > 0
    ) {
      media_summary$engagement[1]
    } else {
      0
    }
    
    best_category <- if (
      nrow(category_summary) > 0
    ) {
      as.character(
        category_summary$content_category[1]
      )
    } else {
      "N/A"
    }
    
    best_category_rate <- if (
      nrow(category_summary) > 0
    ) {
      category_summary$engagement[1]
    } else {
      0
    }
    
    best_traffic <- if (
      nrow(traffic_summary) > 0
    ) {
      as.character(
        traffic_summary$traffic_source[1]
      )
    } else {
      "N/A"
    }
    
    best_hour <- if (
      nrow(hour_summary) > 0
    ) {
      hour_summary$post_hour[1]
    } else {
      NA
    }
    
    best_day <- if (
      nrow(day_summary) > 0
    ) {
      as.character(
        day_summary$day_of_week[1]
      )
    } else {
      "N/A"
    }
    
    overall_rate <- safe_mean(
      d$engagement_rate
    )
    
    div(
      class = "insight-grid",
      
      div(
        class = "insight-box",
        
        div(
          class = "insight-icon",
          icon("chart-line")
        ),
        
        div(
          class = "insight-title",
          "Overall Rate"
        ),
        
        div(
          class = "insight-value",
          fmt_pct(overall_rate)
        ),
        
        div(
          class = "insight-description",
          paste0(
            format(
              nrow(d),
              big.mark = ","
            ),
            " posts in current view"
          )
        )
      ),
      
      div(
        class = "insight-box",
        
        div(
          class = "insight-icon",
          icon("images")
        ),
        
        div(
          class = "insight-title",
          "Best Media"
        ),
        
        div(
          class = "insight-value",
          clean_label(best_media)
        ),
        
        div(
          class = "insight-description",
          paste0(
            fmt_pct(best_media_rate),
            " average engagement"
          )
        )
      ),
      
      div(
        class = "insight-box",
        
        div(
          class = "insight-icon",
          icon("layer-group")
        ),
        
        div(
          class = "insight-title",
          "Best Category"
        ),
        
        div(
          class = "insight-value",
          clean_label(best_category)
        ),
        
        div(
          class = "insight-description",
          paste0(
            fmt_pct(best_category_rate),
            " average engagement"
          )
        )
      ),
      
      div(
        class = "insight-box",
        
        div(
          class = "insight-icon",
          icon("bolt")
        ),
        
        div(
          class = "insight-title",
          "Best Traffic"
        ),
        
        div(
          class = "insight-value",
          clean_label(best_traffic)
        ),
        
        div(
          class = "insight-description",
          "Highest average engagement among sources"
        )
      ),
      
      div(
        class = "insight-box",
        
        div(
          class = "insight-icon",
          icon("clock")
        ),
        
        div(
          class = "insight-title",
          "Best Timing"
        ),
        
        div(
          class = "insight-value",
          
          ifelse(
            is.na(best_hour),
            "N/A",
            paste0(
              best_hour,
              ":00"
            )
          )
        ),
        
        div(
          class = "insight-description",
          paste0(
            "Highest engagement observed on ",
            clean_label(best_day)
          )
        )
      )
    )
  })
  
  
  # ==========================================================
  # TOP 10 POSTS
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
    
    top_data <- d %>%
      arrange(
        desc(total_engagement)
      ) %>%
      slice_head(
        n = 10
      ) %>%
      transmute(
        
        `Post ID` = post_id,
        
        `Account` = account_id,
        
        `Media Type` =
          clean_label(media_type),
        
        `Category` =
          clean_label(content_category),
        
        `Likes` = likes,
        
        `Comments` = comments,
        
        `Shares` = shares,
        
        `Saves` = saves,
        
        `Total Engagement` =
          total_engagement,
        
        `Reach` = reach,
        
        `Engagement Rate` =
          fmt_pct_vec(
            engagement_rate
          ),
        
        `Performance` =
          clean_label(
            performance_bucket_label
          )
      )
    
    datatable(
      top_data,
      rownames = FALSE,
      filter = "top",
      
      options = list(
        pageLength = 10,
        lengthChange = FALSE,
        autoWidth = TRUE,
        scrollX = TRUE,
        dom = '<"top"f>rt<"bottom"ip>',
        language = list(
          search = "Search posts:"
        )
      )
    )
  })
  
  
  # ==========================================================
  # DOWNLOAD FILTERED DATA
  # ==========================================================
  
  output$download_data <- downloadHandler(
    
    filename = function() {
      
      paste0(
        "Instagram_Analytics_Filtered_",
        Sys.Date(),
        ".csv"
      )
    },
    
    content = function(file) {
      
      write.csv(
        filtered_data(),
        file,
        row.names = FALSE
      )
    }
  )
}


# ============================================================
# 9. RUN APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)