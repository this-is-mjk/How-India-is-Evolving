# Overview

This Shiny application provides an interactive way to explore and analyze various development indicators, 
crime data, and the role of women in elections across Indian states. The UI is organized into three main sections, each focusing on a specific aspect:

* Comparing States – Compares different Indian states based on various development indicators.
* Crime Against Women – Analyzes crime data, specifically related to crimes against women, in a comprehensive way.
* Women & Elections – Investigates the relationship between women’s participation in elections and its effect on crime rates.

## Requirements

Run the following com,and for installing the required libraries
```
install.packages(c("shiny", "bslib", "shinythemes", "dplyr", "tidyr", "ggplot2", 
                   "bsicons", "reshape2", "sf", "rvest", "viridis", "ggrepel", "ggthemes"))
```

## Motivation

### To understand the demographics and current situation in India, and make it easy for everyone to do so.

* Provide insights into state-level performance across various development indicators (e.g., area, literacy, etc.).
* Help policymakers, researchers, and citizens make data-driven decisions.

* Visualize and analyze crime data, particularly crimes against women, across Indian states.
* Understanding the Link Between Women’s Electoral Participation and Crime

* Simplify access to complex datasets through an interactive, user-friendly interface.

## UI Structure
The user interface of the application is divided into three main sections, all accessible via a navbar. 
Each section contains interactive elements like dropdown menus, plots, and tables that allow users to explore the data in depth.

### Comparing States
This section allows users to compare the development parameters of various Indian states across different indicators.
We can also find where a State is leading, and lagging behind?

### Crime In India
This section provides a detailed analysis of crime data, its composition, evolution with time, particularly related to crimes against women, across various states in India.

### Elections & Women
The final section focuses on the role of women in Indian elections, there representation, and its potential impact on criminal activity.





### How do the code works?

* This is a standard R Shiny app, and our UI components can be found here:
  - https://shiny.posit.co/r/layouts/

* We have implemented custom functions for each plot, can follow it in the file

* We have primarily used ggplot2 for plotting, and the code can be understood by following the comments in the file.

