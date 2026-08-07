# 🏀 Thompson Basketball Intelligence

A modular NBA Basketball Operations decision-support platform built with **R Shiny**, designed to simulate the types of tools used by professional basketball front offices.

> Built as part of my basketball analytics portfolio to demonstrate practical applications of roster management, salary cap strategy, contract analysis, and executive reporting.

---

# Features

## 📊 Executive Dashboard
- Team payroll overview
- Salary cap summary
- Contract distribution
- Roster composition
- Executive-level KPIs

<img width="1902" height="802" alt="image" src="https://github.com/user-attachments/assets/b4a46862-99fc-4a57-9dcf-7a99aaf23cae" />


## 👥 Roster Intelligence
- Complete roster management
- Contract status
- Years remaining
- Remaining guaranteed salary
- Two-Way Contracts
- Exhibit 10 Contracts
- Qualifying Offers
- Restricted Free Agents

<img width="1565" height="1046" alt="image" src="https://github.com/user-attachments/assets/f6eddc89-41d8-4f67-a4ca-629f611b106c" />


## 💰 Salary Cap Intelligence
- Team payroll
- Salary Cap
- Luxury Tax
- First Apron
- Second Apron
- Player salary as % of cap
- Payroll threshold visualization

<img width="1552" height="1057" alt="image" src="https://github.com/user-attachments/assets/90a50d29-a69d-4d89-a39c-9a5ddb73e359" />


## 🔄 Trade Intelligence
- Trade matching
- Incoming / outgoing salary
- Cap impact
- Trade simulation framework

## 📝 Draft Intelligence
- Draft asset tracking
- Future first-round picks
- Future second-round picks

## ✍️ Extension Simulator
- Contract extension projections
- Payroll impact
- Multi-year planning

## 📈 Five-Year Outlook
- Future payroll projections
- Contract expirations
- Long-term cap planning

## 🏀 Depth Chart
- Position-by-position roster view
- Starter visualization
- Rotation planning
- Position eligibility

---

# Tech Stack

- R
- Shiny
- golem
- SQLite
- DuckDB
- tidyverse
- DT
- bs4Dash

---

# Project Roadmap
✅ Executive Dashboard
✅ Salary Cap Intelligence
✅ Trade Analyzer
✅ Roster Intelligence
✅ Draft Assets
✅ Extension Simulator
✅ Five-Year Outlook
✅ Depth Chart

🚧 Free Agency Planner
🚧 Trade Machine 2.0
🚧 Player Similarity Engine
🚧 Draft Pick Valuation
🚧 Lineup Optimization

# Future Enhancements
- Player similarity search
- CBA Worded Explanations 
- Trade optimization
- Draft pick valuation
- Lineup optimization
- Injury impact analysis
- Free agency planner
- AI-powered executive summaries

# Project Structure

```
basketball-operations-platform
│
├── R/
│   ├── Executive Dashboard
│   ├── Salary Cap Intelligence
│   ├── Trade Intelligence
│   ├── Roster Intelligence
│   ├── Draft Intelligence
│   ├── Extension Simulator
│   ├── Five-Year Outlook
│   └── Depth Chart
│
├── inst/
├── data/
├── tests/
├── docs/
└── renv/
```

---

# Installation and Setup

## Requirements

Before running Thompson Basketball Intelligence, install:

- R 4.3 or newer
- RStudio
- Git

## 1. Clone the Repository

Open Git Bash or a terminal and run:

```bash
git clone https://github.com/DannyTData/basketball-operations-platform.git
```

Move into the project folder:

```bash
cd basketball-operations-platform
```

## 2. Open the RStudio Project

Open:

```text
basketball-operations-platform.Rproj
```

RStudio should automatically recognize the project environment.

## 3. Restore the Required Packages

In the RStudio Console, run:

```r
install.packages("renv")
renv::restore()
```

The restoration process may take several minutes because it installs the package versions used to build the application.

## 4. Run the Application

From the RStudio Console, run:

```r
golem::run_dev()
```

The application should open in the RStudio Viewer or your default web browser.

## Alternative Run Method

The application can also be started with:

```r
source("dev/run_dev.R")
```

## Troubleshooting

### Missing `golem`

```r
install.packages("golem")
library(golem)
golem::run_dev()
```

### Missing `renv`

```r
install.packages("renv")
renv::restore()
```

### Missing database packages

```r
install.packages(c(
  "DBI",
  "RSQLite",
  "duckdb"
))
```

### Package restoration problems

Restart the R session:

```text
Session → Restart R
```

Then run:

```r
renv::restore()
```

### Application does not open automatically

Run:

```r
options(shiny.launch.browser = TRUE)
golem::run_dev()
```

## Data Disclaimer

TBI is a portfolio and educational project. Salary, roster, contract, draft, and transaction information may include demonstration data or planning assumptions and should not be treated as an official NBA source.

Clone the repository

```bash
git clone https://github.com/DannyTData/basketball-operations-platform.git
```

Open the project

```
basketball-operations-platform.Rproj
```

Restore packages

```r
renv::restore()
```

Run the application

```r
golem::run_dev()
```

---

# Purpose

This project was created to demonstrate the types of analytical tools used inside an NBA Basketball Operations department.

Rather than building isolated analytics models, this platform integrates multiple basketball decision-support modules into a single application.

The project emphasizes:

- Basketball Operations
- Salary Cap Management
- Roster Construction
- Contract Planning
- Executive Reporting
- Decision Support

---

# Future Enhancements

- Player similarity models
- Trade value model
- Draft pick valuation
- Lineup optimization
- Player projection models
- Injury impact analysis
- Team comparison dashboard
- Automated roster updates

---

# About Me

I'm passionate about combining basketball knowledge with data analytics to build tools that support basketball decision-making.

My interests include:

- Basketball Operations
- Basketball Analytics
- Salary Cap Strategy
- R Programming
- SQL
- Data Visualization
- Predictive Modeling

GitHub:
https://github.com/DannyTData

LinkedIn:
(https://www.linkedin.com/in/danny-f-thompson/)

---

# License

This project is provided for educational and portfolio purposes.

## Phase 1 cap engine

The salary-cap page now uses a reusable calculation engine for team salary, cap room, tax and apron position, minimum-team-salary shortfall, guarantees, and concentration. The engine explicitly labels data assumptions and is covered by calculation tests.
