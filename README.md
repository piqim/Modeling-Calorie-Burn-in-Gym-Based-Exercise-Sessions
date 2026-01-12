# Descriptive \& Statistical Fitness-Data Analysis with R Shiny Dashboard


\*\*How to turn the “Gym Members Exercise” data set into a descriptive‑analysis + statistical‑inference project that ends with an interactive web app\*\*

Src: https://tritongpt.ucsd.edu/chat/shared/776926eb-edd8-4141-b03f-b9d3826830b6



---

\### 1. Descriptive analysis (the “what is happening?”)



\- \*\*Summarise each variable\*\* – mean, median, standard deviation, min/max for continuous fields (Age, Weight, Height, BMI, Session Duration, Calories Burned, Max/Avg/Resting BPM).  

\- \*\*Explore distributions\*\* – histograms or density plots for Age, BMI, Calories Burned, and Session Duration; bar charts for categorical fields (Gender, Workout Type, Experience Level).  

\- \*\*Cross‑tabulate\*\* – e.g., average calories burned by Workout Type, or average BMI by Gender.  

\- \*\*Spot relationships\*\* – scatterplots of Weight vs. Calories Burned, or Session Duration vs. Avg BPM, plus correlation coefficients.



These steps give you a clear picture of the population and are directly aligned with the descriptive‑statistics topics covered in MATH 181A.



---

\### 2. Statistical inference (the “does a difference or relationship exist?”)



| Question | Typical test (MATH 181A) | What you’ll learn |

|---|---|---|

| Do men and women differ in average calories burned per session? | Two‑sample \*\*t‑test\*\* (or Welch’s test if variances differ) | Whether gender influences energy expenditure. |

| Does workout type affect session duration? | \*\*One‑way ANOVA\*\* (followed by post‑hoc Tukey if significant) | Which exercise modalities are longer or shorter. |

| Is there a linear relationship between BMI and Max BPM? | \*\*Simple linear regression\*\* (β ≈ slope, p‑value) | How body composition relates to cardiovascular response. |

| Are experience levels associated with the choice of workout type? | \*\*Chi‑square test of independence\*\* | Whether beginners, intermediates, and experts prefer different activities. |

| Can we predict calories burned from weight, session duration, and Avg BPM? | \*\*Multiple regression\*\* (adjusted R², coefficient significance) | A practical model for estimating energy expenditure. |



Report each test with the test statistic, p‑value, confidence interval, and an interpretation that ties the result back to the fitness context.



---

\### 3. Building the interactive web app



\*\*Choose a framework\*\*



\- \*\*R Shiny\*\* – works naturally with the tidyverse pipelines you’ll use for cleaning and analysis.  

\- \*\*Python Dash\*\* or \*\*Streamlit\*\* – if you prefer Python’s pandas/scikit‑learn stack.



\*\*Core app components\*\*



1\. \*\*Data upload/preview\*\* – let users explore the raw CSV (first N rows, column names).  

2\. \*\*Descriptive‑stats tab\*\* – display summary tables and interactive plots (histograms, box‑plots) that update when the user selects a variable or subgroup.  

3\. \*\*Inference tab\*\* – dropdowns to pick a hypothesis (e.g., “Compare calories by gender”) and automatically run the appropriate test, showing the statistic, p‑value, and a brief plain‑language conclusion.  

4\. \*\*Modeling tab\*\* – fit a regression model on the fly and display coefficients, diagnostic plots, and predicted calories for a user‑entered “what‑if” scenario.  

5\. \*\*Download section\*\* – allow users to export the summary tables or model results as CSV/PDF.

