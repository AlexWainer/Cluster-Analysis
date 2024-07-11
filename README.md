# Cluster-Analysis

## Overview
This repository contains the code and analysis for a clustering analysis project. The project involves exploratory data analysis, outlier detection, and the application of K-means and K-medoids clustering methods to identify natural groupings in the dataset. The optimal number of clusters is determined through silhouette analysis and the gap statistic method.

## Features
Exploratory Data Analysis (EDA): Summary statistics, data visualization, and outlier detection.
Distance Matrix Calculation: Computation of Euclidean distance matrix and analysis.
Outlier Detection: Identification and handling of outliers based on mean distances.
Data Normalization: Scaling the data for clustering.
Clustering Methods: Application of K-means and K-medoids (CLARA) clustering methods.
Optimal Cluster Determination: Determination of the optimal number of clusters using silhouette analysis and the gap statistic.
Parallel Processing: Utilization of parallel processing for efficient computation.
Silhouette Analysis: Visualization and interpretation of silhouette scores.
Cluster Visualization: Graphical representation of the identified clusters.

## Project Structure
Data Loading and Preparation: Loading the dataset, handling missing values, and renaming columns.
Exploratory Data Analysis: Summary statistics, boxplots, and pair plots for EDA.
Distance Matrix Calculation: Calculation and visualization of the Euclidean distance matrix.
Outlier Detection: Identification of outliers based on mean distances.
Data Normalization: Scaling the data for clustering.
Optimal Cluster Determination: Finding the optimal number of clusters for K-means and K-medoids using silhouette analysis.
Parallel Processing: Implementation of parallel processing for clustering.
Clustering and Visualization: Application of K-means and K-medoids clustering, silhouette analysis, and visualization of clusters.
Reassignment of Outliers: Handling of observations with negative silhouette scores and recalculating silhouette scores.

## Shout-Out
A special shout-out to Sam Matisonn for working with me on this project. Your insights and contributions were invaluable!
