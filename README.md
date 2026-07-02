# 💪 Gym Lifting Progress Tracker

A personal Shiny app for tracking and visualizing weightlifting progress over time.

## Features

- **Upload** an existing workout log (`.csv` or `.xlsx`) or start a fresh log
- **Visualize** weight, volume, reps, and sets over time — faceted by exercise
- **Track progress rate** — linear trend (kg/month) per exercise
- **Monthly frequency** and **training consistency** charts
- **Personal records** table
- **Session view** — browse any training day
- **Manage entries** — add, edit, or delete rows directly in the app
- **Download** your full dataset or a summary stats CSV at any time

## Data Format

Your input file should have these columns (a header row is required):

| Column | Type | Example |
|---|---|---|
| `Date` | `YYYY-MM-DD` or `DD.MM.YYYY` | `2025-01-15` |
| `Exercise` | text | `Bench Press` |
| `Body.Part` | text | `Chest` |
| `Split` | text | `Push` |
| `Sets` | integer | `4` |
| `Repetitions` | integer | `8` |
| `Weight` | numeric (kg) | `80` |
| `Notes` | text (optional) | `used belt` |

## Running Locally

```r
# Install required packages (first time only)
install.packages(c("shiny", "ggplot2", "dplyr", "lubridate", "DT", "readxl"))

# Run the app
shiny::runApp("app.R")
```

## Privacy Note

Your data is stored **locally only** — no data is sent anywhere. The app saves a `tracker_config.txt` file in its directory to remember which CSV file you were using between sessions. Neither this file nor your CSV data should be committed to version control (both are excluded via `.gitignore`).
