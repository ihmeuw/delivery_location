# ST-GPR Covariate Selection and Stage 1 Custom Prior Generation

## Overview

This directory contains code for generating a stage 1 custom prior for ST-GPR. The stage 1 custom prior is an ensemble method of weighting all possible candidate linear random effects submodels by their OOS RMSE (out-of-sample root mean squared error) on a holdout set. This is an optional step for running ST-GPR, but it can be useful for improving the accuracy of the model, as well as further understanding relationships between covariates.

This code was originally developed by Will Gardner for the MNCH Team [here](https://stash.ihme.washington.edu/projects/MNCH/repos/st_gpr/browse/linear_oos).

## Directory Structure

| Directory or File | Description |
| --- | --- |
| [`assemble_prior.R`](https://stash.ihme.washington.edu/projects/HS/repos/pcp/browse/mortality_by_delivery_location/pipeline/04_stgpr/linear_oos/assemble_prior.R) | Averages the results of the test_prior() function and create ensemble linear predictions |
| [`bind_covariates.R`](https://stash.ihme.washington.edu/projects/HS/repos/pcp/browse/mortality_by_delivery_location/pipeline/04_stgpr/linear_oos/bind_covariates.R) | Loads in and binds together covariates for use in ensemble first-stage models |
| [`db_tools.R`](https://stash.ihme.washington.edu/projects/HS/repos/pcp/browse/mortality_by_delivery_location/pipeline/04_stgpr/linear_oos/db_tools.R) | Functions for interacting with the database |
| [`helpers.R`](https://stash.ihme.washington.edu/projects/HS/repos/pcp/browse/mortality_by_delivery_location/pipeline/04_stgpr/linear_oos/helpers.R) | Helper functions used in ST-GPR custom linear ensemble code |
| [`launch_prior.R`](https://stash.ihme.washington.edu/projects/HS/repos/pcp/browse/mortality_by_delivery_location/pipeline/04_stgpr/linear_oos/launch_prior.R) | **THIS SCRIPT** is the one that will most commonly be edited by the user to indicate desired covariates, adjust parameters, etc. This script launches the function `launch_prior()` that in turn runs two functions: test_prior() and assemble_prior(). Together they allow the creation of an ensemble linear prior for STGPR |
| [`launch.R`](https://stash.ihme.washington.edu/projects/HS/repos/pcp/browse/mortality_by_delivery_location/pipeline/04_stgpr/linear_oos/launch.R) | Contains the actual code of `launch_prior()`, called from `launch_prior.R` (inherited code sorry ^^;), that in turn runs two functions: test_prior() and assemble_prior() |
| [`linear_oos_process.R`](https://stash.ihme.washington.edu/projects/HS/repos/pcp/browse/mortality_by_delivery_location/pipeline/04_stgpr/linear_oos/linear_oos_process.R) | Calculates out of sample RMSE (and/or other metric, if specified) |
| [`make_ko.R`](https://stash.ihme.washington.edu/projects/HS/repos/pcp/browse/mortality_by_delivery_location/pipeline/04_stgpr/linear_oos/make_ko.R) | Create holdouts/knockouts from a dataset |
| [`plot_mod_aves.R`](https://stash.ihme.washington.edu/projects/HS/repos/pcp/browse/mortality_by_delivery_location/pipeline/04_stgpr/linear_oos/plot_mod_aves.R) | Child script for plotting model averaging results |
| [`plot_prior_beta.R`](https://stash.ihme.washington.edu/projects/HS/repos/pcp/browse/mortality_by_delivery_location/pipeline/04_stgpr/linear_oos/plot_prior_beta.R) | Explores linear prior covariates from the test_prior() function. These plots are useful for exploring covariate relationships and sanity checks prior to formally running ST-GPR |
| [`README.md`](https://stash.ihme.washington.edu/projects/HS/repos/pcp/browse/mortality_by_delivery_location/pipeline/04_stgpr/linear_oos/README.md) | Overview of the linear_oos directory and its contents |
| [`test_prior.R`](https://stash.ihme.washington.edu/projects/HS/repos/pcp/browse/mortality_by_delivery_location/pipeline/04_stgpr/linear_oos/test_prior.R) | Test combinations of priors, calculate OOS validity, and generate weights |

## Getting Started

1. **Edit `launch_prior.R`** to specify the desired covariates, adjust parameters, etc.
2. **Run `launch_prior.R`** to generate the ensemble linear prior for ST-GPR (may take some time, depending on the size of the dataset and the number of covariates specified).
3. Outputs and plots will be saved to the directory specified by `linear_oos_dir` in `launch_prior.R`. By convention, this will likely be a subdirectory of PCP scratch, e.g. `/ihme/scratch/projects/hssa/pcp/mortality_by_delivery_location/stgpr/linear_oos/`.

## Notes

* It seems unlikely, but if this directory will be used long term then it would be more convenient to edit parameters in a configuration file rather than within `launch_prior.R` itself. This would allow for easier version control and reproducibility.
