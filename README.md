## R code
* **main.R** contains code for parameter estimation and preparation for other programs
* **model.R** contains code for the epidemic model
* **cal_popflow.R** contains code of geographical location data, population and GDP data, and the function to calculate the movement rate matrix
* **sol.R** contains code for solving the epidemic model in parameter estimation
* **distr_post.R** contains code of posterior distribution of unknown parameters
* **CI.R** contains code for obtaining the credible interval of fitted value of data
* **sol_ocp.R** contains code for solving the epidemic model in optimal control problem
* **SA_det.R** contains code for conducting simulated annealing algorithm
* **parallel_SA.R** contains code for conducting parallel computing of simulated annealing algorithm
* **Events.R** contains code of all events of the stochasitc epidemic model
* **sol_sto.R** contains code for generating sample paths of the stochastic epidemic model
* **ctmc_simu.R** contains code for conducting simulation of the stochastic epidemic model
* **estimation_sigma&gamma.R** contains code for estimation of parameters sigma, gamma in the epidemic model
* **ZZ_fig_*.R** contains code for generating figures

## Data
* **data.csv** contains epidemic data obtained by organizing raw data
* txt files in folder 'raw_data' contains raw data of the COVID-19 outbreak in districts and counties of Xi'an City, Shaanxi Province, China during December, 2021 to January, 2022, which was copied from the epidemic announcements of Health Commission of Shaanxi province (http://sxwjw.shaanxi.gov.cn/sy/wjyw/index_1.html)

## Figures
Contains pdf files for the figures in the manuscript
