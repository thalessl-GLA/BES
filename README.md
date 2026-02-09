Project overview

This repository contains the data processing and modelling pipeline used to test the moderation framework linking nationalism and attitudes toward immigration (ATI) 
in the British Election Study (BES) Internet Panel. The project examines whether the effect of national identity on immigration attitudes varies across micro, meso, 
and macro economic contexts.

What this repository does: 

Merges BES panel data with:

-- UK parliamentary constituency indicators and national macroeconomic data (GDP, unemployment, claimant count and inflation)

Implements:

-- exploratory analysis across waves

-- panel and interaction econometric modelling

-- factor analysis of ethnic vs civic nationalism (Wave 11)

-- Produces reproducible outputs using Quarto + R

Structure

.qmd files – main analysis documents (micro/meso/macro models)

*.R scripts – data preparation, merging, and modelling

_files/ – Quarto outputs (ignored by git)

.rds – processed datasets (generated locally)

Data

Core survey: British Election Study Internet Panel (2014–2025)

External context:

parliamentary contituency data + national level data (ONS)

Data are not included in the repository; scripts assume local access to BES files and administrative sources.

Methods

-- Panel regressions and interaction models

-- Micro/meso/macro moderation strategy

-- Correlation trends across waves

-- Exploratory and confirmatory factor analysis (civic vs ethnic nationalism)

Reproducibility

Prepare BES data locally

Run merging scripts

Execute Quarto documents for models and figures

Purpose

The repository supports a PhD project on how economic environments condition the nationalism–ATI relationship, providing a transparent workflow from raw data to empirical results.
