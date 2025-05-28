# Human need satisfaction enables decoupling of well-being from income

Lea A. Tamberg (1*), Julia K. Steinberger (1), Viktoria Cologna (2), Vivien Fisch-Romito (1), Joel Millward-Hopkins (1), Flavio Calvo (3), Naomi Oreskes (2)

(1) Faculty of Geosciences and Environment, University of Lausanne, Géopolis, Chavannes-près-Renens, 1022, Switzerland  
(2) Department of the History of Science, Harvard University, 1 Oxford Street, Cambridge, 02138, Massachusetts, United States of America  
(3) Scientific Computing and Research Support Unit, University of Lausanne, Amphimax, Chavannes-près-Renens, 1022, Switzerland

(*) Corresponding author. E-mail: lea.tamberg@unil.ch
 
## Abstract of the paper
Understanding the link between well-being and income is crucial for assessing the viability of sustainability strategies that limit affluence. Here, we examine the extent to which the satisfaction of basic human needs can explain the correlation between income and life evaluation, an indicator of subjective well-being. Using more than 1.3 million responses from the Gallup World Poll, we estimate the effect of absolute and relative income, GDP per capita, and GDP growth on life evaluation, while controlling for individual and country-level need satisfaction, and governance quality. We find that a hypothetical country achieving population-wide need satisfaction would, even at a GDP per capita of $10,000, match today’s happiest countries in terms of life evaluation. Beyond this income level, the remaining effects of absolute income are marginal. Our results suggest that basic human needs play a central role in decoupling subjective well-being from economic growth.

 ## Contents of this repository
* the publically available [datasets](public_data) that we combined with Gallup World Poll dataset
* a [demo dataset](data_demo.csv)
* the [R notebook](prepare_dataset.Rmd) used to clean and assemble the dataset used in the study
* the [model configuration files](model_specifications)
* the [R notebook](analyse_results.Rmd) used to analyse the fitted models
* [summary csv files](model_summaries) of the fitted models, containing the coefficient and random effect estimates 
* a documentation of the [expected results](expected_results_demo) when using the demo dataset
* several [utility functions](helper_functions.R), mostly for generating interaction plots
* the code for the [simulation study](simulation_vcov_estimator) we conducted to justify the change of variance-covariance estimator
* files and folders used by the R package renv to ensure consistency in R and package versions

## Instructions for using the code
When opening the [R project](decouple-wellbeing-income.Rproj) for the first time, renv (a package ensuring consistency in R and package versions) will bootstrap itself and then inform you that one or more packages recorded in the lockfile are not installed. In order to install the required packages in the local environment, execute renv::restore() in the console (see the renv documentation for more details).

Since the dataset is not publicly available, we provide a randomly generated demo dataset that can be used to retrace the code. It is also much smaller than the true dataset, which allows running the model fitting on a normal computer. For the true dataset, fitting the models requires more computing power. (We developed the code on a Windows platform (x86_64-w64-mingw32/x64 (64-bit)) and outsourced the computationally heavy model fitting to UNIL's [HPC cluster](https://wiki.unil.ch/ci/books/high-performance-computing-hpc/page/curnagl).)

Each model can be fitted by executing the corresponding R script under [model_specifications](model_specifications). The called [fitting routine](fitting_function.R) uses the dataset stored under "data.csv" in the root folder. If you want to use the demo dataset, you can rename it accordingly and use [this script](model_specifications/run_all_models_demo_mode.R) to automatically fit all models. 
If you get access to the Gallup dataset, you first need to process it following the data cleaning and integration [notebook](prepare_dataset.Rmd). 

Once the models are fitted, the results are stored in the same folder as the model specification script and can be analysed with [this notebook](analyse_results.Rmd). In [expected_results_demo](expected_results_demo), you can find the model fits, figures, and notebook outputs expected when using the demo dataset.




