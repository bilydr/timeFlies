# setup file to be run only once to get data and run precalculations
# produces sqlite database and .Rdata files to support the app

dir.create('data')
dir.create('downloads')

source('getdata.R')
source('precalYears.R')
source('precalAirport.R')
source('precalCarrier.R')
source('precalRoute.R')