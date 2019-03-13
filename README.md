# Historical-Typhoid
Data and code for "Changes in historical typhoid transmission across 16 U.S. cities, 1889-1931: Quantifying the impact of investments in water and sewer infrastructures"

Instructions to reproduce estimates and figures from manuscript:

1)	Run TSIR model code (“./TSIR models/TSIR [city_name].R”) for all cities, making sure that you set your working directory to the “Historical-Typhoid-master” folder.

2)	Adjust long-term beta estimates to be per capita using “./per capita beta.lts.R” code, making sure that you set your working directory to the “Historical-Typhoid-master” folder.

3)	Fit linear regression models for each city using “./regression/[city_name] regression.R”, making sure that you set your working directory to the “Historical-Typhoid-master” folder.


Microsoft Word document "Data and R code descriptions" contains details on code and datasets used in this analysis. For further information, see full manuscript "Changes in historical typhoid transmission across 16 U.S. cities, 1889-1931: Quantifying the impact of investments in water and sewer infrastructures."
