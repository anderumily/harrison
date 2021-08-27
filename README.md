## Installation

Direct install (requires admin privileges):
1.	[Download and install R](https://cran.rstudio.com/)
2.	[Download and install RStudio](https://rstudio.com/products/rstudio/download/#download)

Software Centre (requires VPN connection):
1.	In the Software Centre, in the top right hand search bar type “R for Windows”

    a.	Click on “R for Windows 3.6.2” and install – accept the default settings
    
    b.	Do not install Microsoft R Open 3.4.2
2.	In the Software Centre, in the top right hand search bar type “RStudio”

    a.	Click on RStudio 1.2.5001 and install – accept the default settings
    
## Webinars

- **Start here!** [CSHS hydRology Introduction to R](https://www.youtube.com/watch?reload=9&v=obXb9MAlZ-M)
- [Intro to Working with Canadian Data](https://www.youtube.com/watch?v=56mrlRvTmao)
- [CSHS Webinar R Markdown with Dr Kevin Shook](https://www.youtube.com/watch?v=TH3oDhRrEy0)
- [Using Git and GitHub with R](https://register.gotowebinar.com/register/861710757767612429) (June 3, 2020) 

## Sessions

### April 20, 2020
- RStudio tour
- [Basic commands](https://github.com/CentreForHydrology/Introduction_to_R)
- Intro to packages: dplyr, ggplot2, tidyhydat, weathercan
- [Cheatsheets](https://rstudio.com/resources/cheatsheets/): start with Base, RStudio IDE, dplyr, ggplot2
- [rseek.org](https://rseek.org/)
- Sources: [Centre for Hydrology](https://github.com/CentreForHydrology), [tidyhydat](https://github.com/ropensci/tidyhydat), [weathercan](https://github.com/rchlumsk/tRaining)

### April 30, 2020
- More [examples](https://github.com/anderumily/R_Resources/tree/master/code)
- Useful snippets
- Organizing your code

## Intro R & RStudio
[R for Data Science](https://r4ds.had.co.nz/)

## Hydrology Packages

- [Hydrology packages on CRAN](https://cran.r-project.org/web/views/Hydrology.html)
- [tidyhydat](https://docs.ropensci.org/tidyhydat/): Canadian water data
- [weathercan](https://docs.ropensci.org/weathercan/): Canadian weather data
- [waterData](https://cran.rstudio.com/web/packages/waterData/index.html): USGS water data
- [CSHShydRology](https://github.com/CSHS-CWRA/CSHShydRology)
- [fasstr](https://github.com/bcgov/fasstr): Flow Analysis Summary Statistics Tool for R
- remember to cite packages e.g.
```r 
citation("tidyhydat")
```
## Useful Guides

- [Style Guide](https://style.tidyverse.org/)
- [Geocomputation with R](https://geocompr.robinlovelace.net/)
- [Using Version Control with RStudio](https://support.rstudio.com/hc/en-us/articles/200532077?version=1.2.5001&mode=desktop)
- [Happy Git with R](https://happygitwithr.com/index.html)

## Papers

- [Why Watershed Analysts Should Use R for Data Processing and Analysis](https://www.researchgate.net/publication/326597102_Why_Watershed_Analysts_Should_Use_R_for_Data_Processing_and_Analysis)
- [R-functions for Canadian hydrologists: a Canada-wide collaboration](https://www.tandfonline.com/doi/full/10.1080/07011784.2018.1492884)
- [Data Organization in Spreadsheets](https://www.tandfonline.com/doi/full/10.1080/00031305.2017.1375989)

## Organizing your project

- use this repo as a template
- use relative paths, not absolute paths
- never alter original data files

## Tips

- [Add a header snippet](http://timfarewell.co.uk/my-r-script-header-template/)

```r
snippet header
	# -------------------------------------------------------------------------
	# Date created:      `r paste(Sys.Date())`
	# Author(s):         Your name
	# Description:       Write a description.
	#   
	# -------------------------------------------------------------------------
	
	# set up ------------------------------------------------------------------
	
	# set working directory
	setwd("")
	
	# load libaries
	library()
	
```

- Use [styler](https://styler.r-lib.org/) for easy formatting
- Get help more easily using [reprex](https://www.tidyverse.org/help/)
- Make pasting paths [easier](https://stackoverflow.com/questions/17605563/efficiently-convert-backslash-to-forward-slash-in-r): 

```r
snippet pp
    "`r gsub('"', "", gsub("\\\\", "/", readClipboard()))`"
```




