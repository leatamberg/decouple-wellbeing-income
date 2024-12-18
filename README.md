# Human need satisfaction enables decoupling of well-being from income

Lea A. Tamberg (1*), Julia K. Steinberger (1), Viktoria Cologna (2), Vivien Fisch-Romito (1), Joel Millward-Hopkins (1), Flavio Calvo (3), Naomi Oreskes (2)

(1) Faculty of Geosciences and Environment, University of Lausanne, Géopolis, Chavannes-près-Renens, 1022, Switzerland  
(2) Department of the History of Science, Harvard University, 1 Oxford Street, Cambridge, 02138, Massachusetts, United States of America  
(3) Scientific Computing and Research Support Unit, University of Lausanne, Amphimax, Chavannes-près-Renens, 1022, Switzerland

(*) Corresponding author. E-mail: lea.tamberg@unil.ch
 
## Abstract of the paper
Understanding the link between well-being and income is crucial for assessing the viability of sustainability strategies that limit affluence. Here, we examine the extent to which the satisfaction of basic human needs can explain the correlation between income and life evaluation, an indicator of subjective well-being. Using more than 1.3 million responses from the Gallup World Poll, we estimate the effect of absolute and relative income, GDP per capita, and GDP growth on life evaluation, while controlling for individual and country-level need satisfaction, and governance quality. We find that a hypothetical country achieving population-wide need satisfaction would, even at a GDP per capita of $10,000, match today’s happiest countries in terms of life evaluation. Beyond this income level, the remaining effects of absolute income are marginal. Our results suggest that basic human needs play a central role in decoupling subjective well-being from economic growth.

 ## Contents of this repository
* the publically available [datasets](public_data)
* the [R notebook](prepare_dataset.Rmd) used to clean and assemble the dataset used in the study
* the [model configuration files](model_specifications)
* the [R notebook](analyse_results.Rmd) used to analyse the fitted models
* [summary csv files](model_summaries) of the fitted models, containing the coefficient and random effect estimates 
* several [utility functions](helper_functions.R), mostly for generating interaction plots
* the code for the [simulation study](simulation_vcov_estimator) we conducted to justify the change of variance-covariance estimator

## Instructions for using the code


## Sytem requirements
The code is based on R version 4.3.2 and uses the following R packages:

showtext_0.9-6      
showtextdb_3.0      
sysfonts_0.8.8      
broom_1.0.4         
scales_1.2.1        
patchwork_1.2.0    
rcartocolor_2.1.1   
viridis_0.6.4       
viridisLite_0.4.2   
stringr_1.5.0       
ggpubr_0.6.0        
ggsci_3.0.0        
car_3.1-2           
carData_3.0-5       
polycor_0.8-1       
corrplot_0.92       
ggplot2_3.4.2       
tibble_3.2.1       
lmtest_0.9-40       
zoo_1.8-12          
lmerTest_3.1-3      
MuMIn_1.47.5        
countrycode_1.5.0   
RColorBrewer_1.1-3         
sp_2.1-2            
interactions_1.1.5  
stargazer_5.2.3     
tidyr_1.3.0         
parameters_0.21.2              
clubSandwich_0.5.10        
lme4_1.1-34         
Matrix_1.6-1.1      
readr_2.1.4         
dplyr_1.1.2 

We developed the code on a Windows platform (x86_64-w64-mingw32/x64 (64-bit)) and outsourced the computationally heavy model fitting to UNIL's [HPC cluster](https://wiki.unil.ch/ci/books/high-performance-computing-hpc/page/curnagl).
