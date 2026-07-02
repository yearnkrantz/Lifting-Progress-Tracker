# 💪 Gym Lifting Progress Tracker

A personal Shiny app for logging, visualizing, and analyzing weightlifting progress over time. Upload an existing workout log or start fresh — all data is stored locally on your machine.

---

## Features

| Tab | What it shows |
|---|---|
| **Weight Progression** | Weight lifted over time, faceted by exercise |
| **Volume Progression** | Sets × Reps × Weight per session |
| **Repetitions / Sets** | Rep and set counts over time |
| **Progress Rate** | Linear trend line (kg / month) with a summary table |
| **Monthly Frequency** | Exercises per month, broken down by split |
| **Training Consistency** | Training days per week vs. your average |
| **Volume by Split** | Total session volume grouped by training split |
| **Summary Stats** | Per-exercise averages, maxes, and session counts |
| **Personal Records** | Heaviest lift ever recorded per exercise |
| **Session View** | Browse any training day with prev/next navigation |
| **Manage Entries** | Add, edit, or delete individual log rows |

**Sidebar filters** — slice any view by body part, split, date range, or specific exercises.  
**Downloads** — export your full dataset or a summary stats CSV at any time.

---

## Data Format

Your input file (`.csv` or `.xlsx`) must include these columns:

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

A header row is required. The `Notes` column is optional — the app will add it automatically if it is missing.

---

## Setup

### Requirements

- [R](https://cran.r-project.org/) ≥ 4.1
- The following R packages:

```r
install.packages(c("shiny", "ggplot2", "dplyr", "lubridate", "DT", "readxl"))
```

### Run from GitHub

```r
# Option 1 — run directly from GitHub (no clone needed)
shiny::runGitHub("Lifting-Progress-Tracker", "yearnkrantz")
```

```bash
# Option 2 — clone and run locally
git clone https://github.com/yearnkrantz/Lifting-Progress-Tracker.git
cd Lifting-Progress-Tracker
```

```r
shiny::runApp()
```

### First launch

On first run the app will ask whether you want to:
- **Upload** an existing `.csv` or `.xlsx` workout log, or
- **Start fresh** and name a new save file.

Your save file path is remembered in `tracker_config.txt` so the app reopens your log automatically on subsequent launches.

---

## Privacy

All data is stored **locally only** — nothing is sent to any server. Both `tracker_config.txt` and your `.csv` data files are excluded from version control via `.gitignore`.
