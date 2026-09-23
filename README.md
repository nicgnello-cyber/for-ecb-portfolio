### 1. Financial Time Series & Volatility Modeling (`nasdaq analysis.R`)
I want to point out that this was a group project (3 people).
The file thst interest this project is nasdaq analysis.
Empirical study analyzing volatility clustering and heavy-tailed dynamics on daily NASDAQ returns (2020–2025) using score-driven and conditional heteroskedasticity models.
We coded custom the update mechanism from scratch for standard GARCH(1,1), Student-t GARCH, and Beta-t-EGARCH without relying on already existing functions in packages.
Parameters were estimated using constrained quasi-Newton optimization (`nlminb`) with parameter bounds to ensure strict stationarity and positivity conditions.
Recursive conditional variance and score trajectories were extracted, we verified score densities against normality assumptions, and evaluated 1-step-ahead out-of-sample forecast accuracy using both Mean Squared Error (MSE) and robust Quasi-Likelihood (QLIKE) loss functions.

---

### 2. Survey Harmonization & Explainable Machine Learning (`match-prediction.R`)
This was not a group effort, it was done by me individually as an exercise.
The files that interest this project are match-prediction.R, Speed Dating.csv and Speed Dating Data Key.doc (it explains how the data was obtained and what the various value represent).
First step was to addressed missing values and then structural inconsistencies across survey waves (different attribute scoring scales between waves 1–5 and 6–9) standardizing attribute allocations to common percentage bases.
A subject-level grouped train/test sampling was implemented (splitting by participant ID `iid`) to prevent data leakage across paired interactions, and addressed severe class imbalance using dynamic scale-positive weighting.
XGBoost binary classifier with early stopping and AUC monitoring was trained on the training set.
Extracted TreeSHAP values (`SHAPforxgboost`) to calculate exact feature attributions and quantify marginal effects on predictions.
Last step was retrain a classifier on the whole dataset

---

### 3. Graph Analytics & Network Topology (`routes analysis.R`)
This was a group effort ( 3 people ) and it was done as part of an exam project.
The files in the repository that interest this project are routes analysis.R and routes.csv.
`igraph` package was used to analyze structural connectivity, routing efficiency, and hub criticality of the OpenFlights dataset.
Built a directed, weighted multigraph across global airport  (3,400+ vertices, 37,000+ simplified edges) integrating external metadata (IATA codes and continental groupings).
Various metrics were computed, such as degree distributions (confirming heavy-tailed scale-free behavior), normalized closeness, edge and vertex betweenness, and eigenvector centrality.
As expected the implementation of the HITS algorithm to separate hub and authority scores across critical nodes, proved the network had small-world properties, these properties (transitivity, diameter, average path length) were then analyzed.
Properties such as connectiviy and reciprocity were investigated and we also constructed ego-graph.
