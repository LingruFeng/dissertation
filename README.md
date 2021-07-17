# dissertation
## data_raw
This folder contains raw data that will be used in dissertation obtained from open source, one file for data preprocessing and one file for calculating driving distance cost between MSOAs to vaccination sites.

Population data (2019 csv):

https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/middlesuperoutputareamidyearpopulationestimates.

MSOA boundary:

https://geoportal.statistics.gov.uk/datasets/ons::middle-layer-super-output-areas-december-2011-boundaries-super-generalised-clipped-bsc-ew-v3/about.

MSOA population weighted centroids:

https://geoportal.statistics.gov.uk/datasets/ons::middle-layer-super-output-areas-december-2011-population-weighted-centroids/about.

Vaccination sites:

https://www.england.nhs.uk/coronavirus/publication/vaccination-sites/.

Vaccination population:

https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-vaccinations/covid-19-vaccinations-archive/.

MSOA-level IMD (Index of Multiple Deprivation) 2019:

https://research.mysociety.org/sites/imd2019/about/.

Ethnic component:

https://www.nomisweb.co.uk/query/select/getdatasetbytheme.asp?opt=3&theme=&subgrp=.

Car ownership:

https://www.nomisweb.co.uk/census/2011/qs416ew.

## Data_github
Folder contains England base map and vaccination site geodata generated from data_raw and will be used in analysis and map plot. 

IMD_ethnic_car_mapplot.ipynb plot the map of ethnic component, car ownership and IMD decile in England.

Distance_decay_function_plot.ipynb plot the cutoff distance decay function for 2SFCA and Gaussian distance decay function for E2SFCA and 3SFCA.

Research_area_plot.ipynb plot the map of research area.

MSOA_test_60_69_OSRM_exclude.ipynb used 2SFCA/E2SFCA/3SFCA method, excluding London, setting 10/15/20/25/30 miles as threshold, calculated the accessibility of vaccination service and calculated the correlation coddficients between accessibility and vaccination rate, and generated the maps. Also generate the file accessibility_imd_ethnic_exclude.csv for further regression in R.

MSOA_test_60_69_OSRM_include.ipynb used 2SFCA/E2SFCA/3SFCA method, including London, setting 10/15/20/25/30 miles as threshold, calculated the accessibility of vaccination service and calculated the correlation coddficients between accessibility and vaccination rate, and generated the maps. Also generated the scatter plot map of vaccination site and map of MSOA that are lack of vaccination site within 10 miles driving distance.

Beta_regression_exclude_London_imdint_factor_include_car.md build a Beta regression: vaccination rate ~ IMD decile + car ownership + ethnic component + accessibility.

Beta_regression_exclude_London_imdint_factor_include_car.Rmd is the output of Beta_regression_exclude_London_imdint_factor.md.
