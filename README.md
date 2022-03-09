# FeralHorseManagement
Code and other such nonsense for REN R 476


A project in fulfillment of REN R 476: Advanced Fisheries and Wildlife Management - Macs MacLeod


Data Source

The main dataset was downloaded from the Alberta Open Data Portal and loaded into ArcGIS Pro. Once loaded into ArcGIS Pro, the attribute table and corresponding layers were examined. The attribute table was exported into a CSV file for further analysis via RStudio. A secondary dataset was then retrieved from Alberta's website, which displayed province-wide minimum horse counts from 2013-2021 (excluding 2020) from across the province. 

Summary

Our data was collected from the aerial surveys. The data collected shows different age categories of feral horses, including Adults ≥ 4 years of age (yoa), Sub Adults (1-3 yoa), and Foals (0-1yoa). The compiled age structure will allow us to run an analysis to determine if different age classes use different habitat types over others. It will also allow us to determine if they occupy different zones in certain life stages. The data were collected using aerial surveys; once a group of horses is spotted, the count is divided into adults, yearlings, and foals. Since the observers are in a helicopter, we must assume there is a bias in determining the age of any given individual, as it can be difficult to tell the difference between small adults and yearlings from the air [9]. The surveyors adjusted for potential confusion and the feral horses were classified as small adults to not over-estimate the number of yearlings present in the population. The habitat data was recorded based on where the horses were spotted. This included habitat types such as cut blocks, meadows, industrial, etc. The dominant plant type is listed along with the habitat. For example, a meadow is described by the dominant plant type meadow_shrub or meadow_grassland. The Equine Zone is the area where an observation took place.
