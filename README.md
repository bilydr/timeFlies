# timeFlies

## Purpose
A project to use historical flight data or flight schedule data to uncover airline industry evolution and competitive landscape changes at three levels: airport, carrier and route. An example to use R to process large datasets, infer business intelligence and build business applications.

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
R version 3.3.0 (2016-05-03)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 14.04.4 LTS

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=en_US.UTF-8       
 [4] LC_COLLATE=en_US.UTF-8     LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
 [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                  LC_ADDRESS=C              
[10] LC_TELEPHONE=C             LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] splitstackshape_1.4.2 data.table_1.9.6      tidyr_0.5.1           dplyr_0.4.3          
 [5] plotly_3.6.0          ggthemes_3.0.3        ggplot2_2.1.0         googleVis_0.5.10     
 [9] shinydashboard_0.5.1  shiny_0.13.2         

loaded via a namespace (and not attached):
 [1] Rcpp_0.12.5      magrittr_1.5     munsell_0.4.3    colorspace_1.2-6 xtable_1.8-2     R6_2.1.2        
 [7] httr_1.2.0       plyr_1.8.4       tools_3.3.0      parallel_3.3.0   grid_3.3.0       gtable_0.2.0    
[13] DBI_0.4-1        htmltools_0.3.5  digest_0.6.9     assertthat_0.1   RJSONIO_1.3-0    gridExtra_2.2.1 
[19] viridis_0.3.4    base64enc_0.1-3  htmlwidgets_0.6  mime_0.4         scales_0.4.0     jsonlite_0.9.22 
[25] chron_2.3-47     httpuv_1.3.3  
```

### Setup
1. Download all files in this repo
2. Run `setup.R`
3. Run `app.R` in local R session, or to deploy to a [Shiny Server](https://www.rstudio.com/products/shiny/shiny-server/). 


## License
The author is an employee of [Carlson Wagonlit Travel](http://www.carlsonwagonlit.com/). The license is to be specified. 