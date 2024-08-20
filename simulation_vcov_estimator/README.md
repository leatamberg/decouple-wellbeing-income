Simulation study to justify use of different variance-covariance estimator compared to pre-registration
===
In the pre-registration of the study, we announced to use for the estimation of the cluster-robust variance-covariance matrices an estimator (here called `CR2`) that uses the "bias-reduced linearization" adjustment proposed by `Bell and McCaffrey (2002)` and further developed in `Pustejovsky and Tipton (2017)` .
However, the calculation of the `CR2` estimators on the whole dataset is untractable with the current available hardware (AMD EPYC 7402 24-Core Processor nodes with 1T of memory). Therefore, we have resorted to a simpler estimator (`CR1`) using as adjustment factor  `m / (m - 1)`, where `m` is the number of clusters (see`Liang & Zeger (1986)` for more details).

To justify this change, we decided to compare the `CR1` and `CR2` estimators on smaller subsamples with increasing sample size. Our aim is to demostrate that for the purpose of our work the `CR1` estimator is close enough to the initially intended `CR2` estimator and that the difference between the two does not increase when increasing the size of the subsample.

The model `H1-c-f` was chosen in order to make this comparison, which we believe to be representative for all of the other models, and the metric for comparison we chose is the coefficient p-values based on the two different resulting variance-covariance matrices.

In the following, we describe the procedure used in order to obtain the data subsamples and to retrieve the p-values of the model estimates based on the two different variance-covariance estimators.


## Data resampling

We prepared 11 subsamples, with sample size ranging from 5% up to 55% of the size of the whole dataset, in steps of 5%. To this aim, we were carefull at randomly selecting subsamples in each `data-year` cluster, so that the size of each cluster in the subsampled data relative to the size of the same cluster in the original data is a constant across all clusters.

To this aim we installed `python 3.9.18` with the following requirements:

```
numpy==1.26.4
pandas ==2.2.1
```

and ran:

```bash
mkdir resamples
python3 resample.py data_all.csv
```

Since the dataset is not open data, we only provide here the `md5` checksums of the full dataset `data_all.csv` of the subsampled files `resamples.md5` and of the results after the several fits and the variance-covariance estimator calculations for the `H1c-f` model `results.H1c_f.md5`.

## Model estimation and variance-covariance matrix calculation

We performed the calculations with R version `4.2.1` and `clubSandwich` version `0.5.10`.

This calculation can be performed with the bash script

```bash
mkdir -p results.H1c_f
for SLURM_ARRAY_TASK_ID in $(seq 1 11); do
  cat scale_pipeline_H1c_f.R | sed -e "s/{level}/$(printf '%02d\n' $((5*${SLURM_ARRAY_TASK_ID})))/" | R --slave
done
```

This command is replacing the `{level}` pattern inside `scale_pipeline_H1c_f.R` with the values `05`, `10`, `15`, ... , `55` and launching each of the runs.

In practice each of those iterations were launched as a separate SLURM job within the UNIL cluster, hence the name of the variable `SLURM_ARRAY_TASK_ID`.

The results are then written inside `.rds` files inside the folder `results.H1c_f` (not uploaded as the rds objects contain copies of the licensed dataset).

## p-values calculations

p-values were then computed with the command

```bash
mkdir -p p_values
Rscript p_values.R
```

## Output plot

The plot inside the `p_values` folder was made with the R package `ggplot2` version `3.5.1` loaded from `tidyverse` version `2.0.0`:

```bash
Rscript plot.R
```

![p-values](https://github.com/calvofl0/GWP-postprocessing/blob/main/p_values/p_values_ggplot2.png?raw=true)
