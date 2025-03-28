# studentperformace
developing using R programming
# 📘 Student Performance and Data Dashboard  

## 🎯 Overview  
This project is an interactive student dashboard built with R Shiny, designed to manage student details, track performance, and analyze data visually.  

---

## 💡 Features  
- **Add/Update/Delete Students:** Includes Name, College, Branch, GPA, Pass/Fail, Internship, and Projects  
- **Performance Analysis:** GPA distribution, Pass rate by branch, Internship and Project insights  
- **Data Table:** View student records and download them as an Excel file  
- **Interactive Dashboard:** Real-time plots and performance metrics  

---

## 🔧 Technologies Used  
- **R Programming**  
- **Shiny** (Dashboard UI)  
- **SQLite** (Database)  
- **ggplot2** for data visualization  
- **writexl** for exporting data  

---

## 🔨 Setup Instructions  

1️⃣ **Install required libraries:**  
```r
install.packages(c("shiny", "shinydashboard", "RSQLite", "ggplot2", "readr", "writexl", "dplyr"))

CREATE TABLE IF NOT EXISTS student_details (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  name TEXT,
  college TEXT,
  branch TEXT,
  gpa REAL,
  pass TEXT,
  internship TEXT,
  projects TEXT
);
shiny::runApp("app.R")
