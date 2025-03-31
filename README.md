# The Longitudinal Health, Income, and Employment Model (LHIEM): A Discrete-Time Microsimulation Model for Policy Analysis

This repo contains the code associated with the paper:

Propp, Adrienne, Vardavas, Raffaele, Price, Carter and Kapinos, Kandice (2025) 'The Longitudinal Health, Income, and Employment Model (LHIEM): A Discrete-Time Microsimulation Model for Policy Analysis' Journal of Artificial Societies and Social Simulation 28 (2) 1 <http://jasss.soc.surrey.ac.uk/28/2/1.html>. doi: 10.18564/jasss.5591.

## Description

LHIEM was designed to assess a health care financing proposal that would allow individuals to borrow from the U.S. government to cover health care costs, requiring careful tracking of medical expenditures and medical debt over time. However, LHIEM is flexible enough to be used for a range of modeling needs related to predicting health care spending and income over time. The paper linked above describes the model and all dynamic modules in depth, and includes a case study to demonstrate how LHIEM can be used to evaluate proposed policy changes.

## Usage

To run the model, navigate to `LHIEM/Integration/10Plan_RunFile.R` and run the program. The file `InputOutputFile.xlsx` contains the parameters used for the main experiments described in the paper. `10Plan_RunFile.R` loads this file, then executes `10Plan_Integrated_loanextracted.R` with the relevant specifications. The file `10Plan_Integrated_loanextracted.R` contains the main mechanics of the model, including the application of the proposed policy. The policy analysis is applied in a modular fashion (separate from the rest of the core model mechanics) to allow for easy modification of the policy analysis piece.

The file `10Plan_RunFile_COVID.R` is a version of the model that was adapted to analyze the 10Plan policy under COVID conditions.

The folder `LHIEM/Integration/FunctionScripts/` contains all of the functions written for the model, organized by submodel. This is intended to streamline adaptation for other policy questions. For three of these (fertility, health status, and mortality), there are tutorial notebooks that walk through the overall logic of these submodels. These can be found at `LHIEM/Fertility/Modeling Brirths.Rmd`, `LHIEM/Health Status/Predicting The Dynamics of Health Status-v3.Rmd`, and `LHIEM/Mortality/Predicting_Mortality_from_Health_Spending.Rmd`, respectively.

## Submodel Tutorials

To provide deeper insight into some of the submodels' logic, we provide three R notebooks that can be used as tutorials. The R notebooks can be found at:

*`LHIEM/Fertility/Modeling Brirths.Rmd`

*`LHIEM/Health Status/Predicting The Dynamics of Health Status-v3.Rmd`

*`LHIEM/Mortality/Predicting_Mortality_from_Health_Spending.Rmd`

They can be viewed as HTML files or as interactive R markdown files.
