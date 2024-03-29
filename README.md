
[![Build Status](https://travis-ci.org/adamdsmith/pinpoint.png)](https://travis-ci.org/adamdsmith/pinpoint)

USFWS Disclaimer
================

This United States Fish & Wildlife Service (USFWS) code is provided on an "as is" basis and the user assumes responsibility for its use. USFWS has relinquished control of the information and no longer has responsibility to protect the integrity , confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recomendation or favoring by USFWS. The USFWS seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by USFWS or the United States Government.

Installing `pinpoint`
=====================

The `pinpoint` package requires you to have [R](https://www.r-project.org/) (&gt;= 3.4) installed on your computer as well as [Rtools](https://cran.r-project.org/bin/windows/Rtools/). Both will require administrative priveleges but the installation of packages after this initial install will not.

With R and Rtools installed, it's simple to install and load the `pinpoint` package to access its functionality. If you receive an SSL or CA Certificate error, you may need to take the extra step documented below.

    # If remotes package is not installed
    install.packages("remotes", dependencies = TRUE)

    # Now install and load pinpoint
    remotes::install_github("adamdsmith/pinpoint")
    library("pinpoint")

**Demonstration coming soon**
=============================
