# 📊 Instagram Analytics – Big Data Dashboard

## 🚀 How to Run

### 1️⃣ Install R & RStudio

Install:

- **R:** https://cran.r-project.org/
- **RStudio:** https://posit.co/download/rstudio-desktop/

### 2️⃣ Clone the Repository

Open Git Bash / Terminal and run:

```bash
git clone https://github.com/Pushkarcodes166/Instagram-Analytics-Big-Data.git
cd Instagram-Analytics-Big-Data
```

### 3️⃣ Open the Project

Open the project folder in **RStudio** and open the `app.R` file.

### 4️⃣ Install Required Packages

Run the following command in the RStudio Console:

```r
install.packages(c(
  "shiny",
  "bslib",
  "dplyr",
  "readr",
  "lubridate",
  "plotly",
  "DT",
  "scales",
  "fontawesome"
))
```

### 5️⃣ Run the Dashboard

Click **Run App** in RStudio, or run:

```r
shiny::runApp()
```

The dashboard will open automatically in your browser. 🎉

---

## 📌 Project Overview

An interactive **Big Data Analytics Dashboard** developed using R to analyze Instagram content performance, audience engagement, reach, impressions, follower growth, traffic sources, and posting-time patterns.

The dashboard converts raw Instagram analytics data into meaningful visualizations and insights for understanding content performance and audience behavior.

---

## ✨ Features

- 📈 Interactive engagement trends
- 📊 Real-time KPI cards
- 🎯 Dynamic data filters
- 📱 Media type performance analysis
- 🚦 Traffic source analysis
- 🗓️ Posting-time heatmap
- 🏆 Performance bucket analysis
- 📂 Content category analysis
- 💡 Automated insights
- 🔝 Top 10 performing posts
- 🔍 Search and sorting
- 📥 Download filtered data
- 🖱️ Interactive Plotly visualizations
- 🔄 Reset filters functionality

---

## 🛠️ Technology Used

- **R**
- **Shiny**
- **bslib**
- **dplyr**
- **readr**
- **lubridate**
- **Plotly**
- **DT**
- **scales**
- **Font Awesome**

---

## 📊 Dataset

The project uses `Instagram_Analytics.csv` containing approximately **30,000 Instagram posts**.

The dataset includes:

- ❤️ Likes
- 💬 Comments
- 🔄 Shares
- 🔖 Saves
- 👥 Reach
- 👁️ Impressions
- 📈 Engagement Rate
- 👤 Followers Gained
- 📱 Media Type
- 🗂️ Content Category
- 🚦 Traffic Source
- 🏆 Performance Bucket

---

## 📁 Project Structure

```text
Instagram-Analytics-Big-Data/
│
├── app.R
├── Instagram_Analytics.csv
├── README.md
├── .gitignore
│
└── www/
    └── instagram-logo.png
```

---

## 📈 Dashboard Analysis

The dashboard helps analyze:

- Overall Instagram performance
- Engagement trends
- Content performance
- Media type effectiveness
- Traffic sources
- Best posting times
- Content categories
- Engagement composition
- Top-performing posts

---

## 👨‍💻 Author

**Pushkar Potnis**

🎓 Computer Engineering  
🏫 Rajiv Gandhi Institute of Technology (RGIT), Mumbai

---

## 📜 License

This project is developed for **academic and educational purposes**.
