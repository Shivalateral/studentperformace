# Load required libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(RSQLite)
library(readr)
library(writexl)
library(dplyr)

# Connect to SQLite and ensure table exists
conn <- dbConnect(SQLite(), "new_education_dashboard.db")
dbExecute(conn, "
  CREATE TABLE IF NOT EXISTS student_details (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT,
    college TEXT,
    branch TEXT,
    gpa REAL,
    pass TEXT,
    internship TEXT,
    projects TEXT
  )
")
dbDisconnect(conn)

# Define UI layout
ui <- dashboardPage(
  dashboardHeader(title = "Enhanced Student Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Add Student", tabName = "add_student", icon = icon("user-plus")),
      menuItem("Update/Delete Student", tabName = "update_delete", icon = icon("edit")),
      menuItem("Performance Analysis", tabName = "performance", icon = icon("chart-bar")),
      menuItem("Student Data", tabName = "data", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                box(title = "Total Students", width = 4, status = "info", textOutput("total_students")),
                box(title = "Average GPA", width = 4, status = "primary", textOutput("avg_gpa")),
                box(title = "Pass Percentage", width = 4, status = "success", textOutput("pass_percentage"))
              )
      ),
      # Add Student Tab
      tabItem(tabName = "add_student",
              fluidRow(
                box(title = "Add New Student", width = 6, solidHeader = TRUE, status = "primary",
                    textInput("name", "Name"),
                    textInput("college", "College"),
                    selectInput("branch", "Branch", choices = c("CSE", "DS", "IT", "ECE", "EEE", "MECH", "Other")),
                    numericInput("gpa", "GPA", min = 0, max = 10, value = 5),
                    selectInput("pass", "Pass/Fail", choices = c("Pass", "Fail")),
                    textInput("internship", "Internship (e.g., Google Cloud, AI/ML, etc.)"),
                    textInput("projects", "Project (e.g., EV Slot Booking, SNA, etc.)"),
                    actionButton("add", "Add Student", icon = icon("plus"))
                )
              )
      ),
      # Update/Delete Student Tab
      tabItem(tabName = "update_delete",
              fluidRow(
                box(title = "Update or Delete Student", width = 6, solidHeader = TRUE, status = "warning",
                    numericInput("student_id", "Student ID", value = 1, min = 1),
                    textInput("update_name", "New Name"),
                    textInput("update_college", "New College"),
                    selectInput("update_branch", "New Branch", choices = c("CSE", "DS", "IT", "ECE", "EEE", "MECH", "Other")),
                    numericInput("update_gpa", "New GPA", min = 0, max = 10, value = 5),
                    selectInput("update_pass", "New Pass/Fail", choices = c("Pass", "Fail")),
                    textInput("update_internship", "New Internship"),
                    textInput("update_projects", "New Project"),
                    actionButton("update", "Update Student", icon = icon("refresh")),
                    actionButton("delete", "Delete Student", icon = icon("trash"))
                )
              )
      ),
      # Performance Analysis Tab
      tabItem(tabName = "performance",
              fluidRow(
                box(title = "GPA Distribution", width = 6, plotOutput("gpaPlot")),
                box(title = "Pass Rate by Branch", width = 6, plotOutput("branchPlot")),
                box(title = "Internships Count", width = 6, plotOutput("internshipPlot")),
                box(title = "Top Projects", width = 6, plotOutput("projectPlot"))
              )
      ),
      # Student Data Tab
      tabItem(tabName = "data",
              fluidRow(
                box(title = "Student Details", width = 12, tableOutput("student_data_table")),
                downloadButton("download_data", "Download Data as Excel")
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  get_data <- reactive({
    conn <- dbConnect(SQLite(), "new_education_dashboard.db")
    data <- dbGetQuery(conn, "SELECT * FROM student_details")
    dbDisconnect(conn)
    return(data)
  })
  
  # Add student event
  observeEvent(input$add, {
    conn <- dbConnect(SQLite(), "new_education_dashboard.db")
    dbExecute(conn,
              "INSERT INTO student_details (name, college, branch, gpa, pass, internship, projects) VALUES (?, ?, ?, ?, ?, ?, ?)",
              params = list(input$name, input$college, input$branch, input$gpa, input$pass, input$internship, input$projects))
    dbDisconnect(conn)
    session$reload()
  })
  
  # Update/Delete events
  observeEvent(input$update, {
    conn <- dbConnect(SQLite(), "new_education_dashboard.db")
    dbExecute(conn,
              "UPDATE student_details SET name=?, college=?, branch=?, gpa=?, pass=?, internship=?, projects=? WHERE id=?",
              params = list(input$update_name, input$update_college, input$update_branch, input$update_gpa, input$update_pass,
                            input$update_internship, input$update_projects, input$student_id))
    dbDisconnect(conn)
    session$reload()
  })
  
  observeEvent(input$delete, {
    conn <- dbConnect(SQLite(), "new_education_dashboard.db")
    dbExecute(conn, "DELETE FROM student_details WHERE id=?", params = list(input$student_id))
    dbDisconnect(conn)
    session$reload()
  })
  
  # Overview stats
  output$total_students <- renderText({ nrow(get_data()) })
  output$avg_gpa <- renderText({ round(mean(get_data()$gpa), 2) })
  output$pass_percentage <- renderText({ paste0(round(mean(get_data()$pass == "Pass") * 100, 2), "%") })
  
  # Performance plots
  output$gpaPlot <- renderPlot({ ggplot(get_data(), aes(x = gpa)) + geom_histogram(binwidth = 0.5, fill = "steelblue", color = "black") })
  output$branchPlot <- renderPlot({ ggplot(get_data() %>% group_by(branch) %>% summarise(pass_rate = mean(pass == "Pass") * 100), aes(x = branch, y = pass_rate, fill = branch)) + geom_bar(stat = "identity") })
  output$internshipPlot <- renderPlot({ ggplot(get_data(), aes(x = internship)) + geom_bar(fill = "purple") })
  output$projectPlot <- renderPlot({ ggplot(get_data(), aes(x = projects)) + geom_bar(fill = "orange") })
  
  # Data table and download
  output$student_data_table <- renderTable({ get_data() })
  output$download_data <- downloadHandler(
    filename = function() { "student_data.xlsx" },
    content = function(file) { write_xlsx(get_data(), file) }
  )
}

# Run the app
shinyApp(ui, server)

 
 