# UVA Baseball: Pitching Optimizaiton Project

This repository contains data analysis and dashboard tools for Major League Baseball (MLB) and college baseball at the University of Virginia (UVA). The repository is divided into three main sections:

1. **MLB Exploratory Data Analysis (EDA)**
2. **UVA Dashboard Template**
3. **UVA Predictive Shiny App**

---

## 1. MLB EDA

In this section, we performed an exploratory data analysis (EDA) using two datasets:
- **Expected Value vs. Actual Value Data**
- **Pitch Metric Data**

### Methodology:
- We began by analyzing pitcher value, clustering the data into four groups using **KMeans Clustering**. This helped identify which pitchers fell into the upper tier of performance value.
- Once we isolated the top-performing cluster, we further segmented this group based on **height** and **weight** to define archetypes of pitchers.
- Interestingly, 3 out of the 4 identified categories demonstrated what could be considered genetic advantages in terms of body composition, such as height and weight.
- Our primary focus shifted to a subset of **seven pitchers** from the top cluster who exhibited **elite results** despite having **average body types**.

### Insights:
- The goal was to analyze how these pitchers achieved outstanding performance without relying on genetic advantages.
- Using advanced EDA techniques such as movement plots and comparisons of pitching metrics (velocity, spin rate, vertical and horizontal break), we examined their success.
- By analyzing **actual vs. expected performance data**, we aimed to derive actionable insights on replicating these elite results for pitchers on the UVA baseball team.

---

## 2. UVA Dashboard Template

This dashboard aims to provide a seamless and interactive tool for UVA's baseball team to track daily pitching performance.

### Features:
- The dashboard allows the team to **upload daily pitching data** in CSV format.
- Once the data is uploaded, players can access an **interactive interface** to reflect on their individual performance for the day.
- The dashboard offers flexibility, allowing each pitcher to **toggle through metrics** and view data visualizations tailored to their specific needs.

### Why this is helpful for college athletes:
- **Performance Tracking**: College baseball players, especially pitchers, need to track their daily progress closely. This dashboard provides a clear, visual representation of performance metrics, helping athletes understand their strengths and areas for improvement.
- **Data-Driven Decision Making**: By visualizing data such as pitch velocity, spin rate, and location, athletes and coaches can make more informed decisions regarding practice strategies and game-day adjustments.
- **Logistics**: The app's integration with CSV uploads ensures that the dashboard can be updated daily without much effort. The team's data analysts or coaches can quickly upload the day's data, allowing players immediate access to feedback.
- **Customization**: Each pitcher has the ability to focus on specific areas of their performance—whether it's spin rate, velocity, or pitch accuracy—giving them tailored feedback that enhances their training sessions.

---

## 3. UVA Statistical Analysis & Prediction

This notebook, `UVA_Baseball_Prediction_Models`,  implements the **Pitch Efficiency Score (PES)** creation and subsequent **predictive modeling pipeline** designed for UVA baseball’s pitching optimization initiative.

### Overview

The central problem addressed here is **quantifying the true value of individual pitch outcomes** in a way that can drive predictive modeling and decision-making for pitchers and coaches.
To accomplish this, we developed a **hard-coded run value attribution system** that assigns a numeric value to every possible pitch result. These values capture the run-producing and run-preventing impact of an event, enabling us to calculate a preliminary **Pitch Efficiency Score (PES)** for every pitch thrown.


### Methodology

#### 1. **Data Integration**

* Ingests **comprehensive play-by-play datasets** collected from UVA baseball’s game tracking system.
* Cleans, standardizes, and merges pitch-level data with batter/pitcher handedness, pitch type, and situational context.

#### 2. **Run Value Attribution & PES Calculation**

* Assigns **pre-determined run values** to all pitch outcomes (e.g., strikeout, hard contact, groundout, walk).
* Calculates a **Pitch Efficiency Score** for every recorded pitch, producing a single, interpretable measure of pitch success.

#### 3. **Pitch Clustering by Matchup Context**

* Groups pitches by:

  * Pitcher handedness vs. batter handedness (e.g., **L vs R**, **R vs L**, etc.)
  * Pitch type (e.g., Slider, Fastball, Changeup)
* Clusters pitches within these groups based on their PES values to identify **profiles of successful pitches** in specific matchups.
* Example: *In the L/R matchup, sliders buried down-and-in to the batter outperform sliders left over the outer half, which tend to enter the hitter’s barrel path.*

#### 4. **Decision Tree Modeling**

* Builds **separate decision tree models** for each **matchup x pitch type** combination.
* Uses pitcher-specific average metrics (velocity, spin rate, movement, location) as input features.
* Outputs an **interpretable tree** showing:

  * Key metric thresholds where a pitch shifts from being advantageous for the pitcher (**white node**) to advantageous for the hitter (**orange node**).
  * Contextual benchmarks for adjusting pitch execution to improve outcomes.
