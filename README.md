# Human need satisfaction enables decoupling of well-being from income

Lea A. Tamberg (1*), Julia K. Steinberger (1), Viktoria Cologna (2), Vivien Fisch-Romito (1), Joel Millward-Hopkins (1), Flavio Calvo (3), Naomi Oreskes (2)

(1) Faculty of Geosciences and Environment, University of Lausanne, Géopolis, Chavannes-près-Renens, 1022, Switzerland  
(2) Department of the History of Science, Harvard University, 1 Oxford Street, Cambridge, 02138, Massachusetts, United States of America  
(3) Scientific Computing and Research Support Unit, University of Lausanne, Amphimax, Chavannes-près-Renens, 1022, Switzerland

(*) Corresponding author. E-mail: lea.tamberg@unil.ch
 
## Abstract
Understanding the link between well-being and income is crucial for assessing the viability of sustainability strategies that limit affluence. Here, we examine the extent to which the satisfaction of basic human needs can explain the correlation between income and life evaluation, an indicator of subjective well-being. Using more than 1.3 million responses from the Gallup World Poll, we estimate the effect of absolute and relative income, GDP per capita, and GDP growth on life evaluation, while controlling for individual and country-level need satisfaction, and governance quality. We find that a hypothetical country achieving population-wide need satisfaction would, even at a GDP per capita of $10,000, match today’s happiest countries in terms of life evaluation. Beyond this income level, the remaining effects of absolute income are marginal. Our results suggest that basic human needs play a central role in decoupling subjective well-being from economic growth.

 ## Contents of this repository
* the publically available [datasets](public_data)
* the [R notebook](prepare_dataset.Rmd) used to clean and assemble the dataset used in the study
* the [model configuration files](model_specifications)
* the [R notebook](analyse_results.Rmd) used to analyse the fitted models
* [summary csv files](model_summaries) of the fitted models, containing the coefficient and random effect estimates 
* several [utility functions](helper_functions.R), mostly for generating interaction plots
* the code for the [simulation study](simulation_vcov_estimator) we conducted to justify the change of variance-covariance estimator
