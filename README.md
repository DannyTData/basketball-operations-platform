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

## 👥 Roster Intelligence
- Complete roster management
- Contract status
- Years remaining
- Remaining guaranteed salary
- Two-Way Contracts
- Exhibit 10 Contracts
- Qualifying Offers
- Restricted Free Agents

## 💰 Salary Cap Intelligence
- Team payroll
- Salary Cap
- Luxury Tax
- First Apron
- Second Apron
- Player salary as % of cap
- Payroll threshold visualization

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

# Installation

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
