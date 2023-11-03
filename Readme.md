

# Municipal Property Tax Assessment Dashboard

## Overview

This project focuses on creating an interactive dashboard to perform property assessments of existing estates in Boston. The dashboard aims to provide insights into the current market scenario with respect to gross tax and assessed values of properties. Additionally, it helps define areas that can be considered as feasible abodes in Boston.

## Features

- Utilizes R and the Shiny library to build an interactive web application.
- Enables users to select and visualize property assessment data based on various parameters such as building value, gross tax, land use, and year of remodeling.
- Provides dynamic scatterplots and line charts to visualize relationships and trends in the property data.
- Incorporates a heatmap to display the correlation matrix between continuous variables.
- Allows users to click on cells in the heatmap to generate additional scatterplots.

## Prerequisites

- R
- Shiny library
- ggplot2 library
- dplyr library
- plotly library

## Usage

1. Clone the repository to your local machine.
2. Ensure you have R and the necessary libraries installed.
3. Run the Shiny application using R.

```R
# Run Shiny App
shiny::runApp()
```

4. The application will launch in your default web browser. Use the provided inputs to interact with the dashboard.

## Project Structure

- `app.R`: Contains the code for the Shiny application.
- `data_cleaning.R`: Includes data cleaning and preprocessing steps.
- `README.md`: This file, providing an overview of the project.

## Contributing

Contributions to this project are welcome! Please follow the standard guidelines for contributions.



## Acknowledgements

- [Shiny Documentation](https://shiny.rstudio.com/)
- [ggplot2 Documentation](https://ggplot2.tidyverse.org/)

