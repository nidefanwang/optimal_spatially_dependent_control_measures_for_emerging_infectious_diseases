## R code
* **main.R** contains code for parameter estimation and preparation for other programs
* **model.R** contains code for the epidemic model
* **cal_popflow.R** contains code of geographical location data, population and GDP data, and the function to calculate the movement rate matrix
* **sol.R** contains code for solving the epidemic model in parameter estimation
* **distr_post.R** contains code of posterior distribution of unknown parameters
* **CI.R** contains code for obtaining the credible interval of fitted value of data
* **sol_ocp.R** contains code for solving the epidemic model in optimal control problem
* **SA_det.R** contains code for conducting simulated annealing algorithm to solve optimal control problem
* **parallel_SA.R** contains code for conducting **SA_det.R** in parallel
* **Events.R** contains code of all events of the stochasitc epidemic model
* **sol_sto.R** contains code for generating sample paths of the stochastic epidemic model
* **ctmc_simu.R** contains code for conducting simulation of the stochastic epidemic model
* **estimation_sigma&gamma.R** contains code for estimation of parameters sigma, gamma in the epidemic model
* **ZZ_fig_*.R** contains code for generating figures

## Data
* **data.csv** contains epidemic data obtained by organizing raw data
* txt files in folder 'raw_data' contains raw data of the COVID-19 outbreak in Xi'an City, Shaanxi Province, China during December, 2021 to January, 2022, which was copied from the epidemic announcements of Health Commission of Shaanxi province (available at http://sxwjw.shaanxi.gov.cn/sy/wjyw/index_1.html)

## Output
* **samp.Rdata** is the posterior sample of estimated parameters in the epidemic model and is from **main.R**
* **SA_*.Rdata** are results of simulated annealing algorithm and are from **parallel_SA.R**
* **Mat_*.Rdata** and **r*.Rdata** are results from **ZZ_fig_simulation.R**

## Figures
Contains pdf files for the figures in the manuscript
