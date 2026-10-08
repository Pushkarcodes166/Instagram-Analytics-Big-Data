# ============================================================
# INSTAGRAM ANALYTICS - BIG DATA ANALYTICS DASHBOARD
# ============================================================
# Project:
# Instagram Analytics: A Big Data Approach to Content
# Performance and Audience Engagement
#
# Technology:
# R + Shiny + Plotly + DT + dplyr + bslib
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
  !sapply(required_packages, requireNamespace, quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
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
      "Dataset not found.\n\n",
      "Please place 'Instagram_Analytics.csv' in the same folder as app.R."
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
      "The following required columns are missing:\n",
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


# Date conversion
instagram_data$post_date <- suppressWarnings(
  as.Date(instagram_data$post_date)
)

if (all(is.na(instagram_data$post_date))) {
  
  instagram_data$post_date <- suppressWarnings(
    as.Date(
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
  )
}


# Factor / character cleaning
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


# CTA normalization
instagram_data$has_call_to_action <- tolower(
  trimws(as.character(instagram_data$has_call_to_action))
)


# ============================================================
# 5. DERIVED METRICS
# ============================================================

instagram_data <- instagram_data %>%
  mutate(
    
    # Total engagement
    total_engagement =
      coalesce(likes, 0) +
      coalesce(comments, 0) +
      coalesce(shares, 0) +
      coalesce(saves, 0),
    
    # Engagement per reach
    engagement_per_reach = ifelse(
      !is.na(reach) & reach > 0,
      total_engagement / reach,
      NA_real_
    ),
    
    # Save rate
    save_rate = ifelse(
      !is.na(reach) & reach > 0,
      saves / reach,
      NA_real_
    ),
    
    # Share rate
    share_rate = ifelse(
      !is.na(reach) & reach > 0,
      shares / reach,
      NA_real_
    ),
    
    # Comment rate
    comment_rate = ifelse(
      !is.na(reach) & reach > 0,
      comments / reach,
      NA_real_
    ),
    
    # Follower conversion
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
        format(round(x / 1000000000, 1), nsmall = 1),
        "B"
      )
    )
  }
  
  if (abs(x) >= 1000000) {
    return(
      paste0(
        format(round(x / 1000000, 1), nsmall = 1),
        "M"
      )
    )
  }
  
  if (abs(x) >= 1000) {
    return(
      paste0(
        format(round(x / 1000, 1), nsmall = 1),
        "K"
      )
    )
  }
  
  format(round(x, 0), big.mark = ",")
}


fmt_integer <- function(x) {
  
  if (length(x) == 0 || is.na(x[1])) {
    return("0")
  }
  
  format(
    round(x[1], 0),
    big.mark = ",",
    scientific = FALSE
  )
}


# Scalar percentage formatter
fmt_pct <- function(x, digits = 2) {
  
  if (length(x) == 0 || is.na(x[1])) {
    return("0%")
  }
  
  paste0(
    format(
      round(x[1] * 100, digits),
      nsmall = digits
    ),
    "%"
  )
}


# Vector-safe percentage formatter
fmt_pct_vec <- function(x, digits = 2) {
  
  result <- rep("0%", length(x))
  
  valid <- !is.na(x)
  
  if (any(valid)) {
    result[valid] <- paste0(
      format(
        round(x[valid] * 100, digits),
        nsmall = digits,
        trim = TRUE
      ),
      "%"
    )
  }
  
  result
}


safe_mean <- function(x) {
  
  if (length(x) == 0 || all(is.na(x))) {
    return(0)
  }
  
  mean(x, na.rm = TRUE)
}


safe_sum <- function(x) {
  
  if (length(x) == 0 || all(is.na(x))) {
    return(0)
  }
  
  sum(x, na.rm = TRUE)
}


clean_label <- function(x) {
  
  x <- as.character(x)
  
  x <- gsub("_", " ", x)
  
  tools::toTitleCase(x)
}


# ============================================================
# 7. UI
# ============================================================

ui <- page_fillable(
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#E1306C",
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
    
    tags$style(HTML("
      
      /* =====================================================
         GLOBAL
         ===================================================== */

      body {
        background:
          linear-gradient(
            180deg,
            #faf9fc 0%,
            #f5f4f8 100%
          );
        color: #202124;
        font-family: Inter, Arial, sans-serif;
      }

      .container-fluid {
        max-width: 1600px;
        margin: auto;
        padding-left: 24px;
        padding-right: 24px;
      }

      /* =====================================================
         HERO
         ===================================================== */

      .hero {
        margin-top: 22px;
        margin-bottom: 18px;
        padding: 28px 32px;
        border-radius: 22px;
        background:
          linear-gradient(
            115deg,
            #833AB4 0%,
            #C13584 38%,
            #E1306C 68%,
            #F77737 100%
          );
        color: white;
        box-shadow:
          0 14px 40px rgba(75, 35, 100, 0.18);
        position: relative;
        overflow: hidden;
      }

      .hero:after {
        content: '';
        position: absolute;
        width: 280px;
        height: 280px;
        right: -80px;
        top: -130px;
        border-radius: 50%;
        background: rgba(255,255,255,0.10);
      }

      .hero-inner {
        display: flex;
        align-items: center;
        gap: 22px;
        position: relative;
        z-index: 2;
      }

      .hero-logo {
        width: 74px;
        height: 74px;
        object-fit: contain;
        background: rgba(255,255,255,0.14);
        border-radius: 19px;
        padding: 11px;
        backdrop-filter: blur(10px);
      }

      .hero-logo-fallback {
        width: 74px;
        height: 74px;
        border-radius: 19px;
        background: rgba(255,255,255,0.16);
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 34px;
        font-weight: 800;
      }

      .hero-title {
        font-size: 31px;
        font-weight: 800;
        letter-spacing: -0.8px;
        margin: 0;
      }

      .hero-subtitle {
        font-size: 15px;
        opacity: 0.90;
        margin-top: 6px;
        margin-bottom: 0;
      }

      .hero-badge {
        margin-left: auto;
        background: rgba(255,255,255,0.15);
        border: 1px solid rgba(255,255,255,0.22);
        border-radius: 12px;
        padding: 10px 15px;
        font-size: 12px;
        font-weight: 700;
        white-space: nowrap;
      }

      /* =====================================================
         FILTER PANEL
         ===================================================== */

      .filter-panel {
        background: white;
        border: 1px solid #ebe8ef;
        border-radius: 18px;
        padding: 18px 20px 15px 20px;
        box-shadow: 0 5px 18px rgba(25,20,40,0.05);
        margin-bottom: 18px;
      }

      .filter-title {
        font-size: 13px;
        text-transform: uppercase;
        letter-spacing: 1px;
        font-weight: 800;
        color: #6d6875;
        margin-bottom: 12px;
      }

      .form-group {
        margin-bottom: 9px;
      }

      .control-label {
        font-size: 12px;
        font-weight: 700;
        color: #55515d;
        margin-bottom: 5px;
      }

      .form-control,
      .selectize-input {
        border-radius: 10px !important;
        border: 1px solid #dedbe4 !important;
        min-height: 39px;
        box-shadow: none !important;
        font-size: 13px;
      }

      .selectize-input.focus {
        border-color: #C13584 !important;
      }

      .reset-btn {
        width: 100%;
        margin-top: 23px;
        border-radius: 10px;
        font-weight: 700;
      }

      .download-btn {
        width: 100%;
        margin-top: 8px;
        border-radius: 10px;
        font-weight: 700;
      }

      /* =====================================================
         KPI CARDS
         ===================================================== */

      .kpi-grid {
        display: grid;
        grid-template-columns:
          repeat(6, minmax(0, 1fr));
        gap: 12px;
        margin-bottom: 18px;
      }

      .kpi-card {
        background: white;
        border: 1px solid #ebe8ef;
        border-radius: 16px;
        padding: 16px 17px;
        min-height: 102px;
        box-shadow: 0 4px 15px rgba(25,20,40,0.045);
        position: relative;
        overflow: hidden;
      }

      .kpi-card:before {
        content: '';
        position: absolute;
        left: 0;
        top: 0;
        bottom: 0;
        width: 4px;
        background: linear-gradient(
          180deg,
          #833AB4,
          #E1306C,
          #F77737
        );
      }

      .kpi-label {
        color: #77727e;
        font-size: 11px;
        text-transform: uppercase;
        letter-spacing: 0.65px;
        font-weight: 800;
      }

      .kpi-value {
        font-size: 25px;
        font-weight: 800;
        color: #242129;
        margin-top: 8px;
        letter-spacing: -0.7px;
      }

      .kpi-description {
        color: #96919d;
        font-size: 10px;
        margin-top: 3px;
      }

      /* =====================================================
         SECTION
         ===================================================== */

      .section-card {
        background: white;
        border: 1px solid #ebe8ef;
        border-radius: 18px;
        padding: 17px 18px 13px 18px;
        box-shadow: 0 4px 16px rgba(25,20,40,0.045);
        margin-bottom: 18px;
        height: 100%;
      }

      .section-title {
        font-size: 14px;
        font-weight: 800;
        color: #29252f;
        margin-bottom: 2px;
      }

      .section-subtitle {
        font-size: 11px;
        color: #918b97;
        margin-bottom: 8px;
      }

      .plot-container {
        width: 100%;
      }

      /* =====================================================
         INSIGHTS
         ===================================================== */

      .insight-box {
        background: linear-gradient(
          135deg,
          #fff7fb,
          #faf6ff
        );
        border: 1px solid #efdce8;
        border-radius: 15px;
        padding: 13px 15px;
        margin-bottom: 9px;
      }

      .insight-title {
        font-size: 12px;
        font-weight: 800;
        color: #7B2F62;
        margin-bottom: 3px;
      }

      .insight-text {
        font-size: 12px;
        line-height: 1.55;
        color: #4c4751;
      }

      /* =====================================================
         TABLE
         ===================================================== */

      .dataTables_wrapper {
        font-size: 12px;
      }

      table.dataTable thead th {
        background: #faf9fb !important;
        color: #5f5965 !important;
        font-size: 11px;
        text-transform: uppercase;
        letter-spacing: 0.4px;
        border-bottom: 1px solid #e8e4eb !important;
      }

      table.dataTable tbody td {
        vertical-align: middle;
      }

      /* =====================================================
         FOOTER
         ===================================================== */

      .footer {
        text-align: center;
        padding: 18px 0 28px 0;
        color: #99939f;
        font-size: 11px;
      }

      /* =====================================================
         RESPONSIVE
         ===================================================== */

      @media (max-width: 1200px) {

        .kpi-grid {
          grid-template-columns:
            repeat(3, minmax(0, 1fr));
        }

        .hero-badge {
          display: none;
        }
      }

      @media (max-width: 768px) {

        .container-fluid {
          padding-left: 12px;
          padding-right: 12px;
        }

        .hero {
          padding: 22px;
        }

        .hero-inner {
          align-items: flex-start;
        }

        .hero-title {
          font-size: 22px;
        }

        .hero-logo,
        .hero-logo-fallback {
          width: 58px;
          height: 58px;
        }

        .kpi-grid {
          grid-template-columns:
            repeat(2, minmax(0, 1fr));
        }
      }

    "))
    
  ),
  
  
  # ==========================================================
  # MAIN CONTAINER
  # ==========================================================
  
  div(
    
    class = "container-fluid",
    
    # ========================================================
    # HERO
    # ========================================================
    
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
          class = "hero-badge",
          "R • SHINY • BIG DATA"
        )
      )
    ),
    
    
    # ========================================================
    # FILTERS
    # ========================================================
    
    div(
      class = "filter-panel",
      
      div(
        class = "filter-title",
        "Analytics Filters"
      ),
      
      fluidRow(
        
        column(
          width = 2,
          
          dateRangeInput(
            "date_filter",
            "Date Range",
            start = min(instagram_data$post_date, na.rm = TRUE),
            end = max(instagram_data$post_date, na.rm = TRUE),
            min = min(instagram_data$post_date, na.rm = TRUE),
            max = max(instagram_data$post_date, na.rm = TRUE),
            format = "dd M yyyy",
            separator = " → ",
            width = "100%"
          )
        ),
        
        column(
          width = 2,
          
          selectInput(
            "account_type_filter",
            "Account Type",
            choices = c(
              "All",
              sort(unique(instagram_data$account_type))
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
              sort(unique(instagram_data$media_type))
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
              sort(unique(instagram_data$content_category))
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
              sort(unique(instagram_data$traffic_source))
            ),
            selected = "All"
          )
        ),
        
        column(
          width = 2,
          
          selectInput(
            "performance_filter",
            "Performance Bucket",
            choices = c(
              "All",
              sort(unique(instagram_data$performance_bucket_label))
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
            "Download Filtered Data",
            class = "btn btn-dark download-btn"
          )
        ),
        
        column(
          width = 6,
          
          div(
            style = "
              text-align:right;
              padding-top:30px;
              color:#8b8591;
              font-size:11px;
            ",
            
            strong(textOutput("record_count", inline = TRUE)),
            " records currently included in the analysis"
          )
        )
      )
    ),
    
    
    # ========================================================
    # KPI CARDS
    # ========================================================
    
    div(
      class = "kpi-grid",
      
      div(
        class = "kpi-card",
        div(class = "kpi-label", "Posts"),
        div(class = "kpi-value", textOutput("kpi_posts")),
        div(class = "kpi-description", "Filtered records")
      ),
      
      div(
        class = "kpi-card",
        div(class = "kpi-label", "Reach"),
        div(class = "kpi-value", textOutput("kpi_reach")),
        div(class = "kpi-description", "Total audience reached")
      ),
      
      div(
        class = "kpi-card",
        div(class = "kpi-label", "Impressions"),
        div(class = "kpi-value", textOutput("kpi_impressions")),
        div(class = "kpi-description", "Total content views")
      ),
      
      div(
        class = "kpi-card",
        div(class = "kpi-label", "Engagement"),
        div(class = "kpi-value", textOutput("kpi_engagement")),
        div(class = "kpi-description", "Likes + comments + shares + saves")
      ),
      
      div(
        class = "kpi-card",
        div(class = "kpi-label", "Engagement Rate"),
        div(class = "kpi-value", textOutput("kpi_engagement_rate")),
        div(class = "kpi-description", "Average engagement rate")
      ),
      
      div(
        class = "kpi-card",
        div(class = "kpi-label", "Followers Gained"),
        div(class = "kpi-value", textOutput("kpi_followers")),
        div(class = "kpi-description", "Total followers gained")
      )
    ),
    
    
    # ========================================================
    # ROW 1
    # ========================================================
    
    fluidRow(
      
      column(
        width = 8,
        
        div(
          class = "section-card",
          
          div(
            class = "section-title",
            "Engagement Trend"
          ),
          
          div(
            class = "section-subtitle",
            "Daily performance based on the selected metric"
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
            class = "section-title",
            "Performance Mix"
          ),
          
          div(
            class = "section-subtitle",
            "Distribution of posts by performance bucket"
          ),
          
          plotlyOutput(
            "performance_plot",
            height = "350px"
          )
        )
      )
    ),
    
    
    # ========================================================
    # ROW 2
    # ========================================================
    
    fluidRow(
      
      column(
        width = 6,
        
        div(
          class = "section-card",
          
          div(
            class = "section-title",
            "Media Type Performance"
          ),
          
          div(
            class = "section-subtitle",
            "Average engagement rate by media format"
          ),
          
          plotlyOutput(
            "media_plot",
            height = "330px"
          )
        )
      ),
      
      column(
        width = 6,
        
        div(
          class = "section-card",
          
          div(
            class = "section-title",
            "Traffic Source Performance"
          ),
          
          div(
            class = "section-subtitle",
            "Engagement generated from different traffic sources"
          ),
          
          plotlyOutput(
            "traffic_plot",
            height = "330px"
          )
        )
      )
    ),
    
    
    # ========================================================
    # ROW 3
    # ========================================================
    
    fluidRow(
      
      column(
        width = 6,
        
        div(
          class = "section-card",
          
          div(
            class = "section-title",
            "Content Category Ranking"
          ),
          
          div(
            class = "section-subtitle",
            "Top content categories by average engagement"
          ),
          
          plotlyOutput(
            "category_plot",
            height = "390px"
          )
        )
      ),
      
      column(
        width = 6,
        
        div(
          class = "section-card",
          
          div(
            class = "section-title",
            "Posting Time Heatmap"
          ),
          
          div(
            class = "section-subtitle",
            "Average engagement rate by day and posting hour"
          ),
          
          plotlyOutput(
            "heatmap_plot",
            height = "390px"
          )
        )
      )
    ),
    
    
    # ========================================================
    # ROW 4
    # ========================================================
    
    fluidRow(
      
      column(
        width = 5,
        
        div(
          class = "section-card",
          
          div(
            class = "section-title",
            "Engagement Composition"
          ),
          
          div(
            class = "section-subtitle",
            "Contribution of individual engagement actions"
          ),
          
          plotlyOutput(
            "composition_plot",
            height = "330px"
          )
        )
      ),
      
      column(
        width = 7,
        
        div(
          class = "section-card",
          
          div(
            class = "section-title",
            "Reach vs Engagement"
          ),
          
          div(
            class = "section-subtitle",
            "Relationship between audience reach and engagement"
          ),
          
          plotlyOutput(
            "scatter_plot",
            height = "330px"
          )
        )
      )
    ),
    
    
    # ========================================================
    # ROW 5
    # ========================================================
    
    fluidRow(
      
      column(
        width = 6,
        
        div(
          class = "section-card",
          
          div(
            class = "section-title",
            "Call-to-Action Impact"
          ),
          
          div(
            class = "section-subtitle",
            "Average engagement rate with and without CTA"
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
            class = "section-title",
            "Account Performance"
          ),
          
          div(
            class = "section-subtitle",
            "Average engagement rate across accounts"
          ),
          
          plotlyOutput(
            "account_plot",
            height = "300px"
          )
        )
      )
    ),
    
    
    # ========================================================
    # INSIGHTS
    # ========================================================
    
    div(
      class = "section-card",
      
      div(
        class = "section-title",
        "Automated Insights"
      ),
      
      div(
        class = "section-subtitle",
        "Automatically generated observations from the filtered dataset"
      ),
      
      uiOutput("insights")
    ),
    
    
    # ========================================================
    # TOP POSTS
    # ========================================================
    
    div(
      class = "section-card",
      
      div(
        class = "section-title",
        "Top 10 Performing Posts"
      ),
      
      div(
        class = "section-subtitle",
        "Posts ranked by total engagement within the selected filters"
      ),
      
      DTOutput("top_posts")
    ),
    
    
    # ========================================================
    # FOOTER
    # ========================================================
    
    div(
      class = "footer",
      
      "Instagram Analytics • Big Data Analytics Project • ",
      "Built using R, Shiny, Plotly and DT"
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
    
    # Date
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
    
    
    # Account type
    if (
      !is.null(input$account_type_filter) &&
      input$account_type_filter != "All"
    ) {
      
      d <- d %>%
        filter(
          account_type == input$account_type_filter
        )
    }
    
    
    # Media type
    if (
      !is.null(input$media_type_filter) &&
      input$media_type_filter != "All"
    ) {
      
      d <- d %>%
        filter(
          media_type == input$media_type_filter
        )
    }
    
    
    # Content category
    if (
      !is.null(input$category_filter) &&
      input$category_filter != "All"
    ) {
      
      d <- d %>%
        filter(
          content_category == input$category_filter
        )
    }
    
    
    # Traffic source
    if (
      !is.null(input$traffic_filter) &&
      input$traffic_filter != "All"
    ) {
      
      d <- d %>%
        filter(
          traffic_source == input$traffic_filter
        )
    }
    
    
    # Performance bucket
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
    
    paste0(
      format(
        nrow(filtered_data()),
        big.mark = ","
      )
    )
  })
  
  
  # ==========================================================
  # KPI 1 - POSTS
  # ==========================================================
  
  output$kpi_posts <- renderText({
    
    fmt_compact(
      nrow(filtered_data())
    )
  })
  
  
  # ==========================================================
  # KPI 2 - REACH
  # ==========================================================
  
  output$kpi_reach <- renderText({
    
    d <- filtered_data()
    
    fmt_compact(
      safe_sum(d$reach)
    )
  })
  
  
  # ==========================================================
  # KPI 3 - IMPRESSIONS
  # ==========================================================
  
  output$kpi_impressions <- renderText({
    
    d <- filtered_data()
    
    fmt_compact(
      safe_sum(d$impressions)
    )
  })
  
  
  # ==========================================================
  # KPI 4 - ENGAGEMENT
  # ==========================================================
  
  output$kpi_engagement <- renderText({
    
    d <- filtered_data()
    
    fmt_compact(
      safe_sum(d$total_engagement)
    )
  })
  
  
  # ==========================================================
  # KPI 5 - ENGAGEMENT RATE
  # ==========================================================
  
  output$kpi_engagement_rate <- renderText({
    
    d <- filtered_data()
    
    fmt_pct(
      safe_mean(d$engagement_rate)
    )
  })
  
  
  # ==========================================================
  # KPI 6 - FOLLOWERS
  # ==========================================================
  
  output$kpi_followers <- renderText({
    
    d <- filtered_data()
    
    fmt_compact(
      safe_sum(d$followers_gained)
    )
  })
  
  
  # ==========================================================
  # EMPTY PLOT HELPER
  # ==========================================================
  
  empty_plot <- function(message = "No data available") {
    
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
              color = "#888888"
            )
          )
        ),
        margin = list(
          l = 10,
          r = 10,
          t = 10,
          b = 10
        )
      )
  }
  
  
  # ==========================================================
  # 1. ENGAGEMENT TREND
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
        width = 3
      ),
      marker = list(
        size = 5
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
          gridcolor = "#eeeeee"
        ),
        hovermode = "x unified",
        margin = list(
          l = 65,
          r = 20,
          t = 10,
          b = 45
        )
      )
  })
  
  
  # ==========================================================
  # 2. PERFORMANCE MIX
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
      
      # ======================================================
      # CORRECTED HOVERTEMPLATE
      # ======================================================
      
      hovertemplate =
        "<b>%{label}</b><br>%{value:,} posts<br>%{percent}<extra></extra>"
    ) %>%
      
      layout(
        showlegend = TRUE,
        
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = -0.05
        ),
        
        margin = list(
          l = 10,
          r = 10,
          t = 10,
          b = 40
        )
      )
  })
  
  
  # ==========================================================
  # 3. MEDIA TYPE PERFORMANCE
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
          mean(
            engagement_rate,
            na.rm = TRUE
          ),
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
      y = ~reorder(label, avg_engagement),
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
          gridcolor = "#eeeeee"
        ),
        yaxis = list(
          title = ""
        ),
        margin = list(
          l = 100,
          r = 65,
          t = 15,
          b = 50
        )
      )
  })
  
  
  # ==========================================================
  # 4. TRAFFIC SOURCE PERFORMANCE
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
          mean(
            engagement_rate,
            na.rm = TRUE
          ),
        total_engagement =
          sum(
            total_engagement,
            na.rm = TRUE
          ),
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
          "Total Engagement: %{customdata:,}",
          "<extra></extra>"
        ),
      
      customdata = ~total_engagement
    ) %>%
      
      layout(
        xaxis = list(
          title = "Average Engagement Rate",
          tickformat = ".1%",
          gridcolor = "#eeeeee"
        ),
        yaxis = list(
          title = ""
        ),
        margin = list(
          l = 105,
          r = 50,
          t = 15,
          b = 50
        )
      )
  })
  
  
  # ==========================================================
  # 5. CATEGORY RANKING
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
          mean(
            engagement_rate,
            na.rm = TRUE
          ),
        total_engagement =
          sum(
            total_engagement,
            na.rm = TRUE
          ),
        posts = n(),
        .groups = "drop"
      ) %>%
      arrange(
        desc(avg_engagement)
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
          "Total Engagement: %{customdata[1]:,}<br>",
          "Posts: %{customdata[2]:,}",
          "<extra></extra>"
        ),
      
      customdata = ~cbind(
        total_engagement,
        posts
      )
    ) %>%
      
      layout(
        xaxis = list(
          title = "Average Engagement Rate",
          tickformat = ".1%",
          gridcolor = "#eeeeee"
        ),
        yaxis = list(
          title = ""
        ),
        margin = list(
          l = 125,
          r = 50,
          t = 15,
          b = 50
        )
      )
  })
  
  
  # ==========================================================
  # 6. POSTING TIME HEATMAP
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
        day_of_week = as.character(day_of_week),
        post_hour = as.numeric(post_hour)
      ) %>%
      group_by(
        day_of_week,
        post_hour
      ) %>%
      summarise(
        engagement =
          mean(
            engagement_rate,
            na.rm = TRUE
          ),
        posts = n(),
        .groups = "drop"
      )
    
    heat_data <- expand.grid(
      day_of_week = days,
      post_hour = 0:23,
      stringsAsFactors = FALSE
    ) %>%
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
      nrow = length(days),
      ncol = 24,
      byrow = FALSE
    )
    
    post_matrix <- matrix(
      heat_data$posts,
      nrow = length(days),
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
          "Engagement Rate: %{z:.2%}<br>",
          "Posts: %{customdata:,}",
          "<extra></extra>"
        )
    ) %>%
      
      layout(
        xaxis = list(
          title = "Posting Hour",
          dtick = 1
        ),
        yaxis = list(
          title = ""
        ),
        margin = list(
          l = 90,
          r = 20,
          t = 15,
          b = 55
        )
      )
  })
  
  
  # ==========================================================
  # 7. ENGAGEMENT COMPOSITION
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
          y = -0.05
        ),
        
        margin = list(
          l = 10,
          r = 10,
          t = 10,
          b = 40
        )
      )
  })
  
  
  # ==========================================================
  # 8. REACH VS ENGAGEMENT
  # ==========================================================
  
  output$scatter_plot <- renderPlotly({
    
    d <- filtered_data()
    
    if (nrow(d) == 0) {
      return(
        empty_plot(
          "No scatter plot data available"
        )
      )
    }
    
    scatter_data <- d %>%
      filter(
        !is.na(reach),
        !is.na(total_engagement),
        reach > 0
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
    
    plot_ly(
      scatter_data,
      x = ~reach,
      y = ~total_engagement,
      type = "scatter",
      mode = "markers",
      
      marker = list(
        size = 6,
        opacity = 0.55
      ),
      
      text = ~paste(
        "Post:", post_id,
        "<br>Media:", media_type,
        "<br>Category:", content_category,
        "<br>Reach:", fmt_integer(reach),
        "<br>Engagement:", fmt_integer(total_engagement)
      ),
      
      hovertemplate =
        "%{text}<extra></extra>"
    ) %>%
      
      layout(
        xaxis = list(
          title = "Reach",
          type = "log",
          gridcolor = "#eeeeee"
        ),
        yaxis = list(
          title = "Total Engagement",
          type = "log",
          gridcolor = "#eeeeee"
        ),
        margin = list(
          l = 65,
          r = 20,
          t = 15,
          b = 55
        )
      )
  })
  
  
  # ==========================================================
  # 9. CTA IMPACT
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
          mean(
            engagement_rate,
            na.rm = TRUE
          ),
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
          gridcolor = "#eeeeee"
        ),
        margin = list(
          l = 70,
          r = 30,
          t = 15,
          b = 55
        )
      )
  })
  
  
  # ==========================================================
  # 10. ACCOUNT PERFORMANCE
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
          mean(
            engagement_rate,
            na.rm = TRUE
          ),
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
          gridcolor = "#eeeeee"
        ),
        yaxis = list(
          title = ""
        ),
        margin = list(
          l = 90,
          r = 60,
          t = 15,
          b = 50
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
          class = "insight-box",
          
          div(
            class = "insight-title",
            "No Insights Available"
          ),
          
          div(
            class = "insight-text",
            "There are no records matching the selected filters. Try expanding the date range or removing one or more filters."
          )
        )
      )
    }
    
    
    # --------------------------------------------------------
    # Best media
    # --------------------------------------------------------
    
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
    
    best_media <- if (nrow(media_summary) > 0) {
      as.character(
        media_summary$media_type[1]
      )
    } else {
      "N/A"
    }
    
    best_media_rate <- if (nrow(media_summary) > 0) {
      media_summary$engagement[1]
    } else {
      0
    }
    
    
    # --------------------------------------------------------
    # Best category
    # --------------------------------------------------------
    
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
    
    best_category <- if (nrow(category_summary) > 0) {
      as.character(
        category_summary$content_category[1]
      )
    } else {
      "N/A"
    }
    
    best_category_rate <- if (nrow(category_summary) > 0) {
      category_summary$engagement[1]
    } else {
      0
    }
    
    
    # --------------------------------------------------------
    # Best traffic source
    # --------------------------------------------------------
    
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
    
    best_traffic <- if (nrow(traffic_summary) > 0) {
      as.character(
        traffic_summary$traffic_source[1]
      )
    } else {
      "N/A"
    }
    
    
    # --------------------------------------------------------
    # Best posting hour
    # --------------------------------------------------------
    
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
    
    best_hour <- if (nrow(hour_summary) > 0) {
      hour_summary$post_hour[1]
    } else {
      NA
    }
    
    
    # --------------------------------------------------------
    # Best day
    # --------------------------------------------------------
    
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
    
    best_day <- if (nrow(day_summary) > 0) {
      as.character(
        day_summary$day_of_week[1]
      )
    } else {
      "N/A"
    }
    
    
    # --------------------------------------------------------
    # Overall engagement
    # --------------------------------------------------------
    
    overall_rate <- safe_mean(
      d$engagement_rate
    )
    
    
    # --------------------------------------------------------
    # Insight UI
    # --------------------------------------------------------
    
    tagList(
      
      div(
        class = "insight-box",
        
        div(
          class = "insight-title",
          icon("bullseye"),
          " Overall Performance"
        ),
        
        div(
          class = "insight-text",
          
          paste0(
            "The selected dataset contains ",
            format(
              nrow(d),
              big.mark = ","
            ),
            " posts with an average engagement rate of ",
            fmt_pct(overall_rate),
            "."
          )
        )
      ),
      
      
      div(
        class = "insight-box",
        
        div(
          class = "insight-title",
          icon("image"),
          " Best Media Type"
        ),
        
        div(
          class = "insight-text",
          
          paste0(
            clean_label(best_media),
            " currently has the highest average engagement rate at ",
            fmt_pct(best_media_rate),
            "."
          )
        )
      ),
      
      
      div(
        class = "insight-box",
        
        div(
          class = "insight-title",
          icon("layer-group"),
          " Best Content Category"
        ),
        
        div(
          class = "insight-text",
          
          paste0(
            clean_label(best_category),
            " is the strongest content category with an average engagement rate of ",
            fmt_pct(best_category_rate),
            "."
          )
        )
      ),
      
      
      div(
        class = "insight-box",
        
        div(
          class = "insight-title",
          icon("bolt"),
          " Traffic Source"
        ),
        
        div(
          class = "insight-text",
          
          paste0(
            clean_label(best_traffic),
            " is currently the strongest traffic source based on average engagement."
          )
        )
      ),
      
      
      div(
        class = "insight-box",
        
        div(
          class = "insight-title",
          icon("clock"),
          " Posting Timing"
        ),
        
        div(
          class = "insight-text",
          
          paste0(
            "The highest average engagement is observed around ",
            ifelse(
              is.na(best_hour),
              "N/A",
              paste0(
                best_hour,
                ":00"
              )
            ),
            " on ",
            clean_label(best_day),
            "."
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
          options = list(
            dom = "t"
          ),
          rownames = FALSE
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
        
        `Likes` =
          likes,
        
        `Comments` =
          comments,
        
        `Shares` =
          shares,
        
        `Saves` =
          saves,
        
        `Total Engagement` =
          total_engagement,
        
        `Reach` =
          reach,
        
        `Engagement Rate` =
          fmt_pct_vec(engagement_rate),
        
        `Performance` =
          clean_label(
            performance_bucket_label
          )
      )
    
    
    datatable(
      top_data,
      
      rownames = FALSE,
      
      filter = "top",
      
      extensions = "Buttons",
      
      options = list(
        
        pageLength = 10,
        
        lengthChange = FALSE,
        
        autoWidth = TRUE,
        
        scrollX = TRUE,
        
        dom =
          '<"top"f>rt<"bottom"ip>',
        
        columnDefs = list(
          list(
            className = "dt-center",
            targets = c(
              0, 1, 2, 3,
              4, 5, 6, 7,
              8, 9, 10, 11
            )
          )
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