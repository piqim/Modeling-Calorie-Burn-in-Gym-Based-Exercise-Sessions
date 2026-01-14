# 🏋️ Gym Members Exercise Data Analysis & Interactive Web App

This project transforms the **Gym Members Exercise** dataset into a complete **data analysis + statistical inference** workflow, culminating in an **interactive R Shiny web application**.

The project is structured in three progressive phases:
1. **Descriptive Analysis** – understanding what is happening in the data  
2. **Statistical Inference** – testing whether differences or relationships exist  
3. **Interactive Web App** – allowing users to explore, analyze, and model the data dynamically  

---

## 📊 Dataset Overview

- **Source:** :contentReference[oaicite:0]{index=0}  
- **Dataset:** Gym Members Exercise Dataset  
- **Format:** CSV  
- **Key Variables:**  
  - Demographics: Age, Gender, Experience Level  
  - Physical metrics: Height, Weight, BMI  
  - Workout metrics: Session Duration, Workout Type  
  - Performance metrics: Calories Burned, Max BPM, Avg BPM, Resting BPM  

Dataset link:  
https://www.kaggle.com/datasets/valakhorasani/gym-members-exercise-dataset/data  

---

## 🧩 Project Structure

| Part | Focus Area | Goal |
|-----|-----------|------|
| **Part 1** | Descriptive Analysis | Understand distributions and summary statistics |
| **Part 2** | Statistical Inference | Test relationships and group differences |
| **Part 3** | R Shiny Web App | Make the analysis interactive and reproducible |

---

## 🔍 Part 1: Descriptive Analysis  
### *“What is happening in the data?”*

### Step 1 → Summarize Each Variable

For **continuous variables**, compute:

| Statistic | Description |
|---------|-------------|
| Mean | Average value |
| Median | Middle value |
| Standard Deviation | Variability |
| Minimum | Smallest observed value |
| Maximum | Largest observed value |

**Continuous fields analyzed:**
- Age  
- Weight  
- Height  
- BMI  
- Session Duration  
- Calories Burned  
- Max BPM  
- Avg BPM  
- Resting BPM  

---

### Step 2 → Explore Distributions

#### 📈 Histograms
- Age  
- BMI  
- Calories Burned  
- Session Duration  

#### 📊 Bar Charts
- Gender  
- Workout Type  
- Experience Level  

These plots help identify:
- Skewness and outliers  
- Common workout patterns  
- Differences between demographic groups  

---

## 📐 Part 2: Statistical Inference  
### *“Does a difference or relationship exist?”*

Choosing the **right question** and the **correct statistical test** is critical.

---

### Research Questions & Methods

| Question | Statistical Test | Variables |
|--------|-----------------|-----------|
| Can calories burned be predicted?Can we predict calories burned from weight, session duration, and Avg BPM? | Multiple Linear Regression | DV: Calories Burned<br>IVs: Weight, Session Duration, Avg BPM |
| ⇒ Will the gender affect the calories burned from the following fields (weight, session duration, and Avg BPM)? | Multiple Linear Regression (with Gender) | DV: Calories Burned<br>IVs: Weight, Session Duration, Avg BPM, Gender |
| ⇒ Which session type burns the most amount of calories assuming the session duration remains constant for two different genders? | Two-Way ANOVA | DV: Calories Burned<br>Factors: Gender, Session Type<br>Covariate: Session Duration |

---

### Hypotheses

#### 🔹 Regression Model
- **Null (H₀):** Weight, session duration, and average BPM do *not* significantly predict calories burned  
- **Alternative (H₁):** At least one predictor significantly affects calories burned  

#### 🔹 Gender Effect
- **Null (H₀):** Gender has no effect on calories burned when controlling for other variables  
- **Alternative (H₁):** Gender significantly affects calories burned  

#### 🔹 Session Type Comparison
- **Null (H₀):** Mean calories burned is equal across all session types  
- **Alternative (H₁):** At least one session type differs in mean calories burned  

---

## 🌐 Part 3: Building the Interactive Web App with R Shiny

### Framework
- **R Shiny**  
- Integrates naturally with **tidyverse pipelines** used for data cleaning and analysis  

---

### 🧱 Core App Components

#### 1️⃣ Data Upload & Preview
- Upload CSV files  
- Preview:
  - First *N* rows  
  - Column names  
  - Basic structure  

---

#### 2️⃣ Descriptive Statistics Tab
- Interactive summary tables  
- Dynamic plots:
  - Histograms  
  - Box plots  
- Filters:
  - Variable selection  
  - Subgroup selection (e.g., gender, workout type)  

---

#### 3️⃣ Statistical Inference Tab
- Dropdown menu to select a hypothesis  
- Automatically runs the correct test  
- Displays:
  - Test statistic  
  - p-value  
  - Plain-language conclusion  

---

#### 4️⃣ Modeling Tab
- Fit regression models dynamically  
- Outputs:
  - Model coefficients  
  - Diagnostic plots  
  - “What-if” calorie predictions based on user inputs  

---

#### 5️⃣ Download Section
- Export:
  - Summary tables  
  - Model results  
- Formats:
  - CSV  
  - PDF  

---

## 📚 References & Sources

- :contentReference[oaicite:1]{index=1} TritonGPT (shared workspace)  
  https://tritongpt.ucsd.edu/chat/shared/776926eb-edd8-4141-b03f-b9d3826830b6  

- Kaggle Dataset  
  https://www.kaggle.com/datasets/valakhorasani/gym-members-exercise-dataset/data  

---

## ✅ Final Outcome

By the end of this project, users will be able to:
- Understand gym exercise patterns through descriptive statistics  
- Test meaningful hypotheses using formal statistical methods  
- Interactively explore, model, and export results via a polished R Shiny app  

---

💡 *This project demonstrates a full data-science pipeline: data → insight → inference → application.*
