# Gym Lifting Progress Tracker - Shiny App

library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(DT)
library(readxl)

Sys.setlocale("LC_TIME", "English")

# UI Definition
ui <- fluidPage(
  titlePanel("\U0001f4aa Gym Lifting Progress Tracker"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload data file (.csv or .xlsx)",
                accept = c("text/csv", ".csv", ".xlsx",
                           "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")),
      
      hr(),
      
      selectInput("bodypart", "Select Body Part:",
                  choices = NULL, multiple = FALSE),
      
      selectInput("split", "Select Split:",
                  choices = NULL, multiple = FALSE),
      
      dateRangeInput("daterange", "Date Range:",
                     start = NULL, end = NULL),
      
      hr(),
      
      selectInput("exercise_filter", "Filter by Exercise (optional):",
                  choices = NULL, multiple = TRUE),
      
      hr(),
      
      downloadButton("download_summary", "Download Summary Stats"),
      br(), br(),
      downloadButton("download_data", "Download Full Dataset"),
      br(), br(),
      actionButton("reset_config", "Change data file\u2026",
                   icon = icon("folder-open"), width = "100%",
                   style = "color: #888; background: none; border: 1px solid #ccc;"),
      
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Weight Progression",
                 div(style = "overflow-y: auto; height: 600px;",
                     uiOutput("weight_plot_ui"))),
        tabPanel("Volume Progression",
                 div(style = "overflow-y: auto; height: 600px;",
                     uiOutput("volume_plot_ui"))),
        tabPanel("Repetitions",
                 div(style = "overflow-y: auto; height: 600px;",
                     uiOutput("reps_plot_ui"))),
        tabPanel("Sets",
                 div(style = "overflow-y: auto; height: 600px;",
                     uiOutput("sets_plot_ui"))),
        tabPanel("Progress Rate",
                 div(style = "overflow-y: auto; height: 500px;",
                     uiOutput("progress_rate_plot_ui")),
                 hr(),
                 DTOutput("progress_rate_table")),
        tabPanel("Monthly Frequency",
                 plotOutput("frequency_plot", height = "500px")),
        tabPanel("Training Consistency",
                 plotOutput("consistency_plot", height = "500px")),
        tabPanel("Volume by Split",
                 plotOutput("volume_bodypart_plot", height = "500px")),
        tabPanel("Summary Stats",
                 DTOutput("summary_table")),
        tabPanel("Personal Records",
                 DTOutput("pr_table")),
        tabPanel("Session View",
                 fluidRow(
                   column(4,
                     dateInput("session_date", "Select Date:", value = Sys.Date()),
                     actionButton("session_prev", "\u25c0 Previous Session"),
                     actionButton("session_next", "Next Session \u25b6")
                   ),
                   column(8,
                     h4(textOutput("session_title")),
                     DTOutput("session_table")
                   )
                 )),
        
        tabPanel("Manage Entries",
                 fluidRow(
                   column(4,
                     h4("New Entry"),
                     dateInput("new_date", "Date:", value = Sys.Date()),
                     selectizeInput("new_exercise", "Exercise:",
                                    choices = NULL,
                                    options = list(create = TRUE,
                                                   placeholder = "Type or select exercise")),
                     selectizeInput("new_bodypart_entry", "Body Part:",
                                    choices = NULL,
                                    options = list(create = TRUE,
                                                   placeholder = "Type or select body part")),
                     selectizeInput("new_split_entry", "Split:",
                                    choices = NULL,
                                    options = list(create = TRUE,
                                                   placeholder = "Type or select split")),
                     numericInput("new_sets", "Sets:", value = 3, min = 1, step = 1),
                     numericInput("new_reps", "Repetitions:", value = 10, min = 1, step = 1),
                     numericInput("new_weight", "Weight (kg):", value = 0, min = 0, step = 0.5),
                     textInput("new_notes", "Notes:", placeholder = "e.g. used belt, paused reps"),
                     actionButton("add_entry", "\u2795 Add Entry", class = "btn-primary"),
                     br(), br(),
                     textOutput("entry_status")
                   ),
                   column(8,
                     h4("All Data"),
                     helpText("Select a row to edit or delete it."),
                     DTOutput("all_data_table"),
                     br(),
                     actionButton("edit_entry", "\u270f\ufe0f Edit Selected", class = "btn-warning"),
                     actionButton("delete_entry", "\u274c Delete Selected", class = "btn-danger")
                   )
                 )
        )
      ),
      width = 9
    )
  )
)

# Remembers the chosen save file path between sessions
CONFIG_FILE <- "tracker_config.txt"

RAW_COLS <- c("Date", "Exercise", "Body.Part", "Split", "Sets",
              "Repetitions", "Weight", "Notes")

# Server Logic
server <- function(input, output, session) {
  
  save_file    <- reactiveVal(NULL)
  workout_data <- reactiveVal(NULL)
  
  # Helper: parse and enrich a raw data frame
  prepare_df <- function(df) {
    # Add Notes column if missing (backward compat with old CSV files)
    if (is.null(df$Notes)) df$Notes <- ""
    df$Notes[is.na(df$Notes)] <- ""
    
    if (!inherits(df$Date, "Date")) {
      parsed <- as.Date(df$Date, format = "%Y-%m-%d")
      if (any(is.na(parsed))) parsed <- as.Date(df$Date, format = "%d.%m.%Y")
      df$Date <- parsed
    } else {
      df$Date <- as.Date(df$Date)
    }
    df$Volume    <- df$Sets * df$Repetitions * df$Weight
    df$YearMonth <- floor_date(df$Date, "month")
    df
  }
  
  # Helper: write current data to the save file (raw columns only)
  save_data <- function(df) {
    req(save_file())
    write.csv(df |> select(all_of(RAW_COLS)), save_file(), row.names = FALSE)
  }
  
  # Welcome modal
  welcome_modal <- function() {
    modalDialog(
      title = "Welcome to Gym Lifting Tracker \U0001f4aa",
      p("How would you like to get started?"),
      radioButtons("startup_choice", NULL,
        choices = c("Upload an existing file (.csv or .xlsx)" = "upload",
                    "Start fresh"                             = "new"),
        selected = "upload"
      ),
      conditionalPanel(
        condition = "input.startup_choice == 'upload'",
        fileInput("startup_file", "Choose file:",
                  accept = c("text/csv", ".csv", ".xlsx",
                             "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"))
      ),
      conditionalPanel(
        condition = "input.startup_choice == 'new'",
        textInput("new_filename", "Name your save file:", value = "my_workouts",
                  placeholder = "e.g. gym_log"),
        helpText("A .csv file with this name will be created in the app folder.")
      ),
      footer = actionButton("startup_confirm", "Continue", class = "btn-primary"),
      easyClose = FALSE
    )
  }
  
  # On startup: load from config if it exists, otherwise show modal
  if (file.exists(CONFIG_FILE)) {
    path <- readLines(CONFIG_FILE, warn = FALSE)[1]
    save_file(path)
    if (file.exists(path)) {
      workout_data(prepare_df(read.csv(path, stringsAsFactors = FALSE)))
    }
  } else {
    showModal(welcome_modal())
  }
  
  # Reset: delete config, clear state, re-show welcome modal
  observeEvent(input$reset_config, {
    if (file.exists(CONFIG_FILE)) file.remove(CONFIG_FILE)
    save_file(NULL)
    workout_data(NULL)
    showModal(welcome_modal())
  })
  
  # Handle the welcome modal confirmation
  observeEvent(input$startup_confirm, {
    if (input$startup_choice == "new") {
      name <- trimws(input$new_filename)
      if (name == "") name <- "my_workouts"
      if (!endsWith(tolower(name), ".csv")) name <- paste0(name, ".csv")
      
      empty <- data.frame(
        Date = character(), Exercise = character(), Body.Part = character(),
        Split = character(), Sets = numeric(), Repetitions = numeric(),
        Weight = numeric(), Notes = character()
      )
      write.csv(empty, name, row.names = FALSE)
      writeLines(name, CONFIG_FILE)
      save_file(name)
      
    } else {
      req(input$startup_file)
      path <- input$startup_file$datapath
      ext  <- tools::file_ext(input$startup_file$name)
      
      df <- if (tolower(ext) == "xlsx") {
        read_excel(path)
      } else {
        read.csv(path, stringsAsFactors = FALSE)
      }
      df <- prepare_df(as.data.frame(df))
      
      name <- paste0(tools::file_path_sans_ext(input$startup_file$name), ".csv")
      write.csv(df |> select(all_of(RAW_COLS)), name, row.names = FALSE)
      writeLines(name, CONFIG_FILE)
      save_file(name)
      workout_data(df)
    }
    
    removeModal()
  })
  
  # Load file via sidebar
  observeEvent(input$file, {
    path <- input$file$datapath
    ext  <- tools::file_ext(input$file$name)
    
    df <- if (tolower(ext) == "xlsx") {
      read_excel(path)
    } else {
      read.csv(path, stringsAsFactors = FALSE)
    }
    
    df <- prepare_df(as.data.frame(df))
    save_data(df)
    workout_data(df)
  })
  
  # Update filter dropdowns whenever data changes
  observe({
    req(workout_data())
    df <- workout_data()
    
    updateSelectInput(session, "bodypart",
                      choices = c("All", unique(df$Body.Part)), selected = "All")
    updateSelectInput(session, "split",
                      choices = c("All", unique(df$Split)), selected = "All")
    updateSelectInput(session, "exercise_filter",
                      choices = unique(df$Exercise))
    updateDateRangeInput(session, "daterange",
                         start = min(df$Date), end = max(df$Date))
    
    updateSelectizeInput(session, "new_exercise",
                         choices = sort(unique(df$Exercise)), server = TRUE)
    updateSelectizeInput(session, "new_bodypart_entry",
                         choices = sort(unique(df$Body.Part)), server = TRUE)
    updateSelectizeInput(session, "new_split_entry",
                         choices = sort(unique(df$Split)), server = TRUE)
  })
  
  # ---- Add / Edit / Delete entries ----
  
  observeEvent(input$add_entry, {
    req(input$new_exercise, input$new_bodypart_entry, input$new_split_entry)
    
    new_row <- data.frame(
      Date        = as.Date(input$new_date),
      Exercise    = input$new_exercise,
      Body.Part   = input$new_bodypart_entry,
      Split       = input$new_split_entry,
      Sets        = input$new_sets,
      Repetitions = input$new_reps,
      Weight      = input$new_weight,
      Notes       = input$new_notes,
      stringsAsFactors = FALSE
    )
    new_row <- prepare_df(new_row)
    
    current <- workout_data()
    updated <- if (is.null(current)) new_row else bind_rows(current, new_row)
    save_data(updated)
    workout_data(updated)
    
    output$entry_status <- renderText({
      paste0("\u2705 Added: ", input$new_exercise, " on ", format(input$new_date))
    })
  })
  
  # Delete selected row
  observeEvent(input$delete_entry, {
    req(workout_data())
    sel <- input$all_data_table_rows_selected
    if (is.null(sel) || length(sel) == 0) {
      output$entry_status <- renderText("Select a row first.")
      return()
    }
    # The table is sorted desc by Date, so map selection back to that order
    df <- workout_data() |> arrange(desc(Date))
    df <- df[-sel, , drop = FALSE]
    save_data(df)
    workout_data(df)
    output$entry_status <- renderText(
      paste0("\u274c Deleted ", length(sel), " row(s).")
    )
  })
  
  # Edit selected row — open a modal with pre-filled values
  observeEvent(input$edit_entry, {
    req(workout_data())
    sel <- input$all_data_table_rows_selected
    if (is.null(sel) || length(sel) != 1) {
      output$entry_status <- renderText("Select exactly one row to edit.")
      return()
    }
    row <- (workout_data() |> arrange(desc(Date)))[sel, ]
    
    showModal(modalDialog(
      title = "Edit Entry",
      dateInput("edit_date", "Date:", value = row$Date),
      textInput("edit_exercise", "Exercise:", value = row$Exercise),
      textInput("edit_bodypart", "Body Part:", value = row$Body.Part),
      textInput("edit_split", "Split:", value = row$Split),
      numericInput("edit_sets", "Sets:", value = row$Sets, min = 1, step = 1),
      numericInput("edit_reps", "Repetitions:", value = row$Repetitions, min = 1, step = 1),
      numericInput("edit_weight", "Weight (kg):", value = row$Weight, min = 0, step = 0.5),
      textInput("edit_notes", "Notes:", value = row$Notes),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("edit_confirm", "Save Changes", class = "btn-primary")
      )
    ))
  })
  
  observeEvent(input$edit_confirm, {
    sel <- input$all_data_table_rows_selected
    df <- workout_data() |> arrange(desc(Date))
    
    df$Date[sel]        <- as.Date(input$edit_date)
    df$Exercise[sel]    <- input$edit_exercise
    df$Body.Part[sel]   <- input$edit_bodypart
    df$Split[sel]       <- input$edit_split
    df$Sets[sel]        <- input$edit_sets
    df$Repetitions[sel] <- input$edit_reps
    df$Weight[sel]      <- input$edit_weight
    df$Notes[sel]       <- input$edit_notes
    
    df <- prepare_df(df)
    save_data(df)
    workout_data(df)
    removeModal()
    output$entry_status <- renderText("\u2705 Entry updated.")
  })
  
  # ---- Filtered data ----
  
  filtered_data <- reactive({
    req(workout_data())
    df <- workout_data()
    
    if (!is.null(input$bodypart) && input$bodypart != "All")
      df <- df |> filter(Body.Part == input$bodypart)
    
    if (!is.null(input$split) && input$split != "All")
      df <- df |> filter(Split == input$split)
    
    if (!is.null(input$daterange) && !is.na(input$daterange[1]))
      df <- df |> filter(Date >= input$daterange[1] & Date <= input$daterange[2])
    
    if (!is.null(input$exercise_filter) && length(input$exercise_filter) > 0)
      df <- df |> filter(Exercise %in% input$exercise_filter)
    
    df
  })
  
  # ---- Dynamic plot heights (scroll support) ----
  
  facet_plot_height <- reactive({
    req(filtered_data())
    n <- n_distinct(filtered_data()$Exercise)
    max(400, ceiling(n / 2) * 300)
  })
  
  output$weight_plot_ui        <- renderUI(plotOutput("weight_plot",        height = paste0(facet_plot_height(), "px")))
  output$volume_plot_ui        <- renderUI(plotOutput("volume_plot",        height = paste0(facet_plot_height(), "px")))
  output$reps_plot_ui          <- renderUI(plotOutput("reps_plot",          height = paste0(facet_plot_height(), "px")))
  output$sets_plot_ui          <- renderUI(plotOutput("sets_plot",          height = paste0(facet_plot_height(), "px")))
  output$progress_rate_plot_ui <- renderUI({
    req(filtered_data())
    n <- filtered_data() |> group_by(Exercise) |> filter(n() >= 2) |> ungroup() |> pull(Exercise) |> n_distinct()
    plotOutput("progress_rate_plot", height = paste0(max(400, ceiling(n / 2) * 300), "px"))
  })
  
  # ---- Plots ----
  
  output$weight_plot <- renderPlot({
    req(filtered_data())
    ggplot(filtered_data(), aes(x = Date, y = Weight, color = Exercise)) +
      geom_point(size = 3) + geom_line(linewidth = 1) +
      facet_wrap(~ Exercise, scales = "free_y", ncol = 2) +
      theme_minimal() +
      theme(legend.position = "none",
            strip.text = element_text(face = "bold", size = 12),
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 12)) +
      labs(title = paste("Weight Progression -", input$bodypart),
           x = "Date", y = "Weight (kg)")
  })
  
  output$volume_plot <- renderPlot({
    req(filtered_data())
    ggplot(filtered_data(), aes(x = Date, y = Volume, color = Exercise)) +
      geom_point(size = 3) + geom_line(linewidth = 1) +
      facet_wrap(~ Exercise, scales = "free_y", ncol = 2) +
      theme_minimal() +
      theme(legend.position = "none",
            strip.text = element_text(face = "bold", size = 12),
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 12)) +
      labs(title = paste("Volume Progression -", input$bodypart),
           subtitle = "Volume = Sets \u00d7 Reps \u00d7 Weight",
           x = "Date", y = "Volume")
  })
  
  output$reps_plot <- renderPlot({
    req(filtered_data())
    ggplot(filtered_data(), aes(x = Date, y = Repetitions, color = Exercise)) +
      geom_point(size = 3) + geom_line(linewidth = 1) +
      facet_wrap(~ Exercise, scales = "free_y", ncol = 2) +
      theme_minimal() +
      theme(legend.position = "none",
            strip.text = element_text(face = "bold", size = 12),
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 12)) +
      labs(title = paste("Repetitions Over Time -", input$bodypart),
           x = "Date", y = "Repetitions per Set")
  })
  
  output$sets_plot <- renderPlot({
    req(filtered_data())
    ggplot(filtered_data(), aes(x = Date, y = Sets, color = Exercise)) +
      geom_point(size = 3) + geom_line(linewidth = 1) +
      facet_wrap(~ Exercise, scales = "free_y", ncol = 2) +
      theme_minimal() +
      theme(legend.position = "none",
            strip.text = element_text(face = "bold", size = 12),
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 12)) +
      labs(title = paste("Sets Over Time -", input$bodypart),
           x = "Date", y = "Number of Sets")
  })
  
  # Progress Rate: kg/month trend per exercise (linear fit)
  output$progress_rate_plot <- renderPlot({
    req(filtered_data())
    df <- filtered_data() |>
      group_by(Exercise) |>
      filter(n() >= 2) |>
      ungroup()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = Date, y = Weight, color = Exercise)) +
      geom_point(size = 2, alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
      facet_wrap(~ Exercise, scales = "free_y", ncol = 2) +
      theme_minimal() +
      theme(legend.position = "none",
            strip.text = element_text(face = "bold", size = 12),
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 12)) +
      labs(title = "Weight Progress Rate (trend line)",
           x = "Date", y = "Weight (kg)")
  })
  
  output$progress_rate_table <- renderDT({
    req(filtered_data())
    rates <- filtered_data() |>
      group_by(Exercise) |>
      filter(n() >= 2) |>
      summarise(
        Sessions = n(),
        First_Weight = Weight[which.min(Date)],
        Last_Weight  = Weight[which.max(Date)],
        Total_Change = Last_Weight - First_Weight,
        Days_Span    = as.numeric(max(Date) - min(Date)),
        # Linear slope in kg per 30 days
        kg_per_month = if (Days_Span > 0) {
          coef(lm(Weight ~ as.numeric(Date)))[2] * 30
        } else {
          NA_real_
        },
        .groups = "drop"
      ) |>
      mutate(kg_per_month = round(kg_per_month, 2)) |>
      arrange(desc(kg_per_month))
    
    datatable(rates, options = list(pageLength = 25),
              colnames = c("Exercise", "Sessions", "First Weight",
                           "Last Weight", "Total Change (kg)",
                           "Days Span", "kg / month"))
  })
  
  # Monthly Frequency Plot
  output$frequency_plot <- renderPlot({
    req(workout_data())
    frequency_monthly <- workout_data() |>
      filter(Date >= input$daterange[1] & Date <= input$daterange[2]) |>
      group_by(YearMonth, Split) |>
      summarise(Exercises = n_distinct(Exercise), .groups = "drop")
    
    ggplot(frequency_monthly, aes(x = YearMonth, y = Exercises, fill = Split)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      theme(legend.position = "bottom",
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 12)) +
      labs(title = "Number of Exercises per Month by Split",
           x = "Month", y = "Number of Exercises", fill = "Split") +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")
  })
  
  # Training Consistency: sessions per week
  output$consistency_plot <- renderPlot({
    req(workout_data())
    df <- workout_data() |>
      filter(Date >= input$daterange[1] & Date <= input$daterange[2])
    
    weekly <- df |>
      mutate(Week = floor_date(Date, "week", week_start = 1)) |>
      group_by(Week) |>
      summarise(Training_Days = n_distinct(Date), .groups = "drop")
    
    ggplot(weekly, aes(x = Week, y = Training_Days)) +
      geom_col(fill = "steelblue") +
      geom_hline(aes(yintercept = mean(Training_Days)),
                 linetype = "dashed", color = "firebrick") +
      scale_y_continuous(breaks = seq(0, 7, 1)) +
      theme_minimal() +
      theme(axis.text = element_text(size = 10),
            axis.title = element_text(size = 12)) +
      labs(title = "Training Consistency",
           subtitle = paste("Average:", round(mean(weekly$Training_Days), 1),
                            "days / week"),
           x = "Week starting", y = "Training Days")
  })
  
  # Volume by Split Plot
  output$volume_bodypart_plot <- renderPlot({
    req(workout_data())
    volume_split <- workout_data() |>
      filter(Date >= input$daterange[1] & Date <= input$daterange[2]) |>
      group_by(Date, Split) |>
      summarise(Total_Volume = sum(Volume), .groups = "drop")
    
    ggplot(volume_split, aes(x = Date, y = Total_Volume, fill = Split)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      theme(legend.position = "bottom",
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 12)) +
      labs(title = "Training Volume by Split Over Time",
           x = "Date", y = "Total Volume (sets \u00d7 reps \u00d7 weight)",
           fill = "Split")
  })
  
  # Summary Statistics Table
  output$summary_table <- renderDT({
    req(filtered_data())
    summary_stats <- filtered_data() |>
      group_by(Exercise) |>
      summarise(
        Sessions      = n(),
        Avg_Weight    = round(mean(Weight), 2),
        Max_Weight    = max(Weight),
        Avg_Sets      = round(mean(Sets), 2),
        Avg_Reps      = round(mean(Repetitions), 2),
        Avg_Volume    = round(mean(Volume), 2),
        Max_Volume    = max(Volume),
        First_Session = min(Date),
        Last_Session  = max(Date),
        .groups = "drop"
      ) |>
      arrange(desc(Max_Weight))
    datatable(summary_stats, options = list(pageLength = 25))
  })
  
  # Personal Records Table
  output$pr_table <- renderDT({
    req(workout_data())
    pr_by_exercise <- workout_data() |>
      filter(Date >= input$daterange[1] & Date <= input$daterange[2]) |>
      group_by(Exercise) |>
      filter(Weight == max(Weight)) |>
      select(Exercise, Weight, Date, Sets, Repetitions, Volume, Body.Part) |>
      arrange(desc(Weight))
    datatable(pr_by_exercise, options = list(pageLength = 25))
  })
  
  # ---- Session View ----
  
  # All unique training dates (sorted)
  training_dates <- reactive({
    req(workout_data())
    sort(unique(workout_data()$Date))
  })
  
  # Navigate to previous/next session
  observeEvent(input$session_prev, {
    dates <- training_dates()
    current <- input$session_date
    earlier <- dates[dates < current]
    if (length(earlier) > 0) {
      updateDateInput(session, "session_date", value = max(earlier))
    }
  })
  
  observeEvent(input$session_next, {
    dates <- training_dates()
    current <- input$session_date
    later <- dates[dates > current]
    if (length(later) > 0) {
      updateDateInput(session, "session_date", value = min(later))
    }
  })
  
  output$session_title <- renderText({
    req(input$session_date)
    format(input$session_date, "%A, %B %d, %Y")
  })
  
  output$session_table <- renderDT({
    req(workout_data(), input$session_date)
    df <- workout_data() |>
      filter(Date == input$session_date) |>
      select(Exercise, Body.Part, Split, Sets, Repetitions, Weight, Volume, Notes)
    
    if (nrow(df) == 0) {
      datatable(data.frame(Message = "No entries for this date."),
                options = list(dom = "t"), rownames = FALSE)
    } else {
      datatable(df, options = list(dom = "t", pageLength = 50),
                rownames = FALSE)
    }
  })
  
  # All data table (Manage Entries tab) — with row selection
  output$all_data_table <- renderDT({
    req(workout_data())
    datatable(
      workout_data() |>
        arrange(desc(Date)) |>
        select(Date, Exercise, Body.Part, Split, Sets, Repetitions, Weight, Notes, Volume),
      selection = "single",
      options = list(pageLength = 15, scrollX = TRUE)
    )
  })
  
  # ---- Downloads ----
  
  output$download_summary <- downloadHandler(
    filename = function() paste0("exercise_summary_", Sys.Date(), ".csv"),
    content = function(file) {
      summary_stats <- filtered_data() |>
        group_by(Exercise) |>
        summarise(
          Sessions      = n(),
          Avg_Weight    = round(mean(Weight), 2),
          Max_Weight    = max(Weight),
          Avg_Sets      = round(mean(Sets), 2),
          Avg_Reps      = round(mean(Repetitions), 2),
          Avg_Volume    = round(mean(Volume), 2),
          Max_Volume    = max(Volume),
          First_Session = min(Date),
          Last_Session  = max(Date),
          .groups = "drop"
        )
      write.csv(summary_stats, file, row.names = FALSE)
    }
  )
  
  output$download_data <- downloadHandler(
    filename = function() paste0("lifting_data_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(
        workout_data() |> select(all_of(RAW_COLS)),
        file, row.names = FALSE
      )
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
