NSC and Radiocarbon Project: https://github.com/cchambe12/nscradiocarbon


/docs: includes methods and tables for statistics
/data: raw data from Morgan Furze
/bib: references needed for methods section
/analyses: where code lives

Order of operations in analyses folder:
fakedata_nsc.R: builds fake data and test models to make sure they are accurate 

models_stan_nsc.R: builds all nsc models. This file imputes data for spring and autumn in 8-pith and then builds models for total, sugar and starch concentrations across the ring- and diffuse-porous species

models_plotting_alt.R: makes muplots used in main manuscript. Sources exp_muplot_alt.R

	source/exp_muplot_alt.R: is a source file to make the my-lots from 		brms models

models_stan_radiocarbon,R: builds radiocarbon models for ring- and diffuse-porous species and then builds my-lots

###### Addressing reviewer concerns on Bayesian: 18 May 2020
rawdata_plotting.R: uses raw data mean estimates plus standard error to build the same muplots to compare the posterior estimates to the real data. Uses the source file "source/rawdataprep.R"

	This script also runs a series of linear mixed models and anovas to compare results to bayesian

