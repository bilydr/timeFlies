# timeFlies

## Purpose
A project to use historical flight data or flight schedule data to uncover airline industry evolution and competitive landscape changes at three levels: airport, airline and route. An example to use R to process large datasets, infer business intelligence and build business applications.

## Live App for Demo
A deployed version of this repo is available at [TimeFlies Shiny App](http://godata.xyz:3838/apps/timeFlies/)

## Data Source
This version uses publicly available data from
- [US Department of Transportation](http://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236)

- [IATA Codes](http://iatacodes.org/)

## Installation
This repo doesn't contain the data files supporting the app but does have all the scripts to download, transform, and produce such data files.

### System requirements
- A computer with at least 1G RAM and 8G disk space installed with R and Sqlite
- The app is designed to be viewed in computer web browsers rather than on mobile devices, even though the web pages do respect responsive design as much as possible
- My R `sessionInfo()` info as below:

```R
R version 3.2.4 (2016-03-10)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 14.04.4 LTS

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
 [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
 [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
 [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
 [9] LC_ADDRESS=C               LC_TELEPHONE=C            
[11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] splitstackshape_1.4.2 data.table_1.9.6      dplyr_0.4.3          
[4] ggplot2_2.1.0         googleVis_0.5.10      shinydashboard_0.5.1 
[7] shiny_0.13.1          RSQLite_1.0.0         DBI_0.3.1            

loaded via a namespace (and not attached):
 [1] Rcpp_0.12.3      magrittr_1.5     munsell_0.4.3    colorspace_1.2-6
 [5] xtable_1.8-2     R6_2.1.2         plyr_1.8.3       tools_3.2.4     
 [9] parallel_3.2.4   grid_3.2.4       gtable_0.2.0     htmltools_0.3   
[13] lazyeval_0.1.10  assertthat_0.1   digest_0.6.9     RJSONIO_1.3-0   
[17] mime_0.4         scales_0.4.0     jsonlite_0.9.19  httpuv_1.3.3    
[21] chron_2.3-47    
```

### Setup
1. Download all files in this repo
2. Create two subfolders 'data' and 'downloads'
3. Run `getdata.R`
4. Run `precalculation.R`
5. The app is ready to be run in R or to be deployed to a [Shiny Server](https://www.rstudio.com/products/shiny/shiny-server/)


## License
The author is an employee of [Carlson Wagonlit Travel](http://www.carlsonwagonlit.com/). The license is to be specified. 