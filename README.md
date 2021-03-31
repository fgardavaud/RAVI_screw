# RAVI_screw

In our Radiology interventional room, to evaluate the feasibility, safety and outcomes of percutaneous screw fixation of coxal bone metastases in case of steep angulation using cone-beam computed tomography (CBCT) navigation software (NS)

To do that, script allows to get details of scan times and dose at different angulations during an interventionnal procedure.
Publication is currently under submitting process. The link to access to the paper will be set as soon as possible.

Project lead by Pr. François Cornelis, CHU Tenon, APHP, France.

# Prerequisite

Severals R packages are mandatory but the script analyze your system and installs, if necessary, missing packages.
In your project root path you have to create one folder named "data" (and put your data here) and another folder named "output" where the output files will be placed there.

# test environmeent

This script have been tested in R 4.0.4 version in Rstudio v1.4.1106 in macOS Mojave and Catalina version. But it sould work on any OS system if your R program have the ability to work with additional R packages

# Additional R packages list available from CRAN

- lubridate
- doMC
- tictoc
- openxlsx
- tidyverse
- rprojroot
