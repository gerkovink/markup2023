This Shiny application is a platform to explore and visualize data on risk factors associated with low birth weight. Developed using R and integrating libraries like ggplot2, shinydashboard, and tidyverse, the app provides a dashboard to analyze birth weight data from a study by Venables and Ripley (2002).

The app’s dashboard interface allows for the selection of two variables, one categorical and one continuous, which include mother's age, weight, race, smoking status, and medical conditions like hypertension. It then generates three plots: a boxplot, a violin plot, and a scatter plot, each offering insights into different aspects of the data and selected variables.

- Boxplot: Explores the relations between continuous variables (e.g., mother’s age or weight) and birth weight dichotomized as either "Low" or "Normal".
- Violin Plot: Examine the distribution of birth weight in grams across different levels of the chosen categorical variable.
- Scatter Plot: Visualize correlations between the selected continuous variable and birth weight in grams, divided by the selected categorical variable.

These visualizations make the data easily interpretable. This Shiny app is a suitable tool to explore the factors influencing neonatal birth weight.