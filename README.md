![Version:1.0.0](https://img.shields.io/static/v1?label=Version&message=1.0.0&color=blue&?style=plastic)
[![PRsWelcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=default)](http://makeapullrequest.com)

<img src="https://raw.githubusercontent.com/AdrianAntico/Rodeo/master/inst/RodeoLogo.PNG" align="center" width="800" />

### Motivation
I want to make building the best plots as easy as possible. I've never really been a fan of incrementally building a plot by calling function after function, mostly because I have to keep going to stackoverflow to get the syntax or flip through entire documentation just to see what's possible. I'm sorry but that is a gigantic waste of everyone's time, especially when a simple API solution is possible.

This package is intended to reduce or eliminate that behavior (hence the "Auto" part of the name "AutoPlots"). The plots returned in AutoPlots are sufficiently good for 99% of plotting purposes. There are two broad classes of plots available in AutoPlots: Standard Plots and Model Evaluation Plots. If other users find additional plots that this package can support I'm open to having them incorporated.

# Rodeo

## Automated feature engineering using data.table and collapse

### Character Type Data

##### CategoricalEncoding
- Nested random effects
- Actuarial buhlmann credibility
- Target encoding
- Weight of Evidence 
- m-estimator
- poly encode
- backward_difference
- helmert

##### DummifyDT
- All levels
- Partital set of levels

### Numeric Type Data
- Numeric transformations
- Interactions

### Calendar Type Data
- Calendar variables
- Holiday variables

### Cross Row Operations
- Lags and Rolling stats for numeric variables
- Differencing for numeric, date, and categorical variables
- Rolling modes for categorical variables

### Data sets
- Partitioning
- Type conversion for modeling

### Model Based Features
- Dimensionality reduction
- Clustering
- Word2Vec
- Anomaly detection



