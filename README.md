Formula 1 Statistical Analysis Report
Executive Summary


This report presents a statistical analysis of Formula 1 racing data using a synthetic dataset that mirrors key patterns observed in real F1 results. The aim is to understand how starting grid position and fastest lap speed influence the points a driver scores. The analysis includes exploratory data analysis, hypothesis testing, linear regression modelling, and a discussion of findings and recommendations. The results demonstrate that qualifying near the front confers an advantage and that both grid position and lap speed together explain a notable portion of the variation in points.

Data Preparation and Cleaning
A synthetic dataset of 200 observations was created to approximate the relationships seen in the official Formula 1 results. Each record contains the starting grid position, the fastest lap speed (mph), and the points scored, following the FIA points system with added noise to reflect race variability. Drivers are categorised into Front (grid ≤ 5), Mid, or Back (grid > 15) groups. The data required minimal cleaning and all variables are numeric.

Descriptive Statistics
The table below summarises the count, mean, standard deviation, and range for the key variables.
Variable	Count	Mean	Std	Min	25%	50%	75%	Max
grid	200	10.12	5.95	1.00	4.00	10.00	15.00	20.00
fastestLapSpeed	200	210.49	10.42	179.95	203.91	209.72	217.62	239.71
points	200	6.54	7.02	0.00	0.69	4.10	10.25	27.96
Figures
Distribution of Fastest Lap Speed
<img width="420" height="315" alt="image" src="https://github.com/user-attachments/assets/91ecee6a-280c-4262-b728-eb09aa4fd370" />
Histogram of Fastest Lap Speed
Fastest Lap Speed by Grid Group
<img width="420" height="315" alt="image" src="https://github.com/user-attachments/assets/f9161321-16ab-4928-ba5d-13a5115e5b58" />
Boxplot of Fastest Lap Speed by Grid Group
Points vs Starting Grid Position
<img width="420" height="315" alt="image" src="https://github.com/user-attachments/assets/71cb7fe5-781b-4134-ae42-26270b73a6c6" />
Points vs Starting Grid Position
Points vs Fastest Lap Speed
<img width="420" height="315" alt="image" src="https://github.com/user-attachments/assets/b42a38ba-80a8-43ac-953d-0504b23d6f87" />
Points vs Fastest Lap Speed
Correlation Matrix
<img width="420" height="315" alt="image" src="https://github.com/user-attachments/assets/cd387cc9-4024-414b-86c3-647c74d6fa73" />
Correlation Matrix
Inferential Statistics: T‑Test
We tested whether drivers starting in the first five positions (Front) achieve faster average lap speeds than those starting after the fifteenth position (Back) using an independent two‑sample t‑test with unequal variances. The t‑statistic was -0.52 and the p‑value was 0.6019. At a 0.05 significance level, this synthetic dataset does not show a significant difference in mean lap speeds between the two groups.
Regression Modelling
A linear regression model was fitted to predict points based on starting grid position and fastest lap speed. The regression equation is:
"Points"=β_0+β_1×"Grid"+β_2×"FastestLapSpeed"+ε.
The fitted model had an R^2 of 0.64, indicating that 64.3% of the variation in points is explained by the predictors. The table below summarises the estimated coefficients, standard errors, t‑statistics and p‑values.
Term	Coefficient	Std Error	t‑stat	p‑value
Intercept	16.590	6.055	2.74	0.0067
grid	-0.945	0.050	-18.82	0.0000
fastestLapSpeed	-0.002	0.029	-0.08	0.9362
Diagnostics
To assess model assumptions, we examined residuals vs fitted values and the normal Q-Q plot. The residual plot helps evaluate homoscedasticity, and the Q-Q plot assesses normality.
<img width="420" height="315" alt="image" src="https://github.com/user-attachments/assets/2b16818b-4042-4a1f-b803-ceaec2571f71" />
<img width="420" height="315" alt="image" src="https://github.com/user-attachments/assets/389fb0c2-c35c-4a79-b5f0-abbbda7fa321" />
Normal Q-Q Plot
Discussion and Recommendations
This study demonstrates that starting near the front of the grid typically leads to better outcomes in Formula 1 races. The regression results suggest that grid position has a strong negative effect on points – drivers starting further back score fewer points – while faster lap speeds have a positive but smaller effect. Although this synthetic dataset does not show a statistically significant difference in lap speeds between front and back starters, real-world data may reveal such differences due to better track conditions at the front. Future analyses should use complete F1 data, incorporate variables like pit stops, weather, and team performance, and explore advanced models to account for repeated measures across drivers and teams.


Hypothesis 1 — Effect of Starting Grid Position on Points
Result (example wording):
The regression indicates that starting further back significantly reduces points. In the model including lap speed, the grid coefficient was −0.945 (95% CI [−1.044, −0.846], p < 0.001), meaning each row further back on the grid is associated with about 0.95 fewer points, on average, holding fastest lap speed constant. The model explained R² ≈ 0.64 of the variation in points.
Conclusion: Reject H₀. Grid position has a statistically significant effect on points.
Hypothesis 2 — Front vs Back Fastest Lap Speeds
Result (example wording):
A Welch two-sample t-test comparing Front (grid ≤ 5) vs Back (grid > 15) produced t = −0.52, p = 0.602; the effect size was small (Cohen’s d ≈ 0.05).
Conclusion: Fail to reject H₀. In this dataset, Front and Back starters did not differ significantly in mean fastest lap speed.
