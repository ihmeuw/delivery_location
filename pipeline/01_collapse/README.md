# Collapse

## Overview

The collapse directory contains code to collapse the extractions for the creation of an aggregated Delivery Location Analysis dataset. This collapse code is in fact identical (at the time of this writing) to that of GBD Pregnancy Care collapse for IFD, apart from the indicators themselves and the fact that this collapse code has been modified to run as a jobmon workflow, if so desired.

The scripts `01_collapse_prep.R` and `02_collapse.R` may be run standalone from the command line, but more commonly the `run_collapse_workflow.py` script is used to launch a jobmon workflow to collapse extracted data and concatenate the results into a single dataset for analysis and model prep. Please see the [jobmon documentation](https://scicomp-docs.ihme.washington.edu/jobmon/current/index.html) for more information on how to edit and use the `run_collapse_workflow.py` script.

## Directory structure

| Directory or File | Description |
| --- | --- |
| [`01_collapse_prep.R`](https://stash.ihme.washington.edu/projects/HS/repos/pcp/browse/delivery_location_remapping/collapse/01_collapse_prep.py) | Prepares maternal extractions for collapse |
| [`02_collapse.R`](https://stash.ihme.washington.edu/projects/HS/repos/pcp/browse/delivery_location_remapping/collapse/02_collapse.R) | Collapses maternal extractions |
| [`03_concat.R`](https://stash.ihme.washington.edu/projects/HS/repos/pcp/browse/delivery_location_remapping/collapse/03_concat.R) | Concatenates collapsed data into a single dataset |
| [`config.csv`](https://stash.ihme.washington.edu/projects/HS/repos/pcp/browse/delivery_location_remapping/collapse/config.csv) | Configuration file for the collapse scripts. See [the UBCOV Collapse docs](https://hub.ihme.washington.edu/pages/viewpage.action?pageId=37930009) for more information on the configuration file and params |
| [`README.md`](https://stash.ihme.washington.edu/projects/HS/repos/pcp/browse/delivery_location_remapping/collapse/README.md) | Overview of the prep directory and its contents |
| [`run_collapse_workflow.py`](https://stash.ihme.washington.edu/projects/HS/repos/pcp/browse/delivery_location_remapping/collapse/run_collapse_workflow.py) | Runs the jobmon `pcp_dla_collapse` workflow to collapse maternal extractions |

## Getting Started

1. **Edit `run_collapse_workflow.py`** to specify the desired NIDs, adjust parameters, etc. Be sure to update `PREP_VERSION` as needed.
2. **Run `run_collapse_workflow.py`** to launch the jobmon `pcp_dla_collapse` workflow.
3. Review the jobmon output for any errors and warnings, rinse and repeat.

## Notes

* `02_collapse.R` relies on the [ubcov_central](https://stash.ihme.washington.edu/projects/UBCOV/repos/ubcov_central/browse/modules/collapse/collapse.r) repo for the actual collapse functionality. You may need to set the `ubcov_central_root` setting in [config.yml](https://stash.ihme.washington.edu/projects/HS/repos/pcp/browse/config.yml) to point to the location of the ubcov_central repo (or your potentially modified clone of it), if it is not already set.
* After `02_collapse.R` the data will be in aggregated form, so it is at this stage of the pipeline that Limited Use restrictions should generally be less of an issue (and so the collapse and/or concat outputs may be stored in J and have more permissive access internally)
