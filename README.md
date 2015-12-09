#README

Title: Smoke and Die 
Course: Statistics 133
Authors: Leanne (Yuen Wan) Lee, Yeonghwan Son (Tony)

Description of the Project:
The main objective of the project is to find out, if any, the relationship between lung cancer and smoking cigarettes by looking at past data. 
We first plan to look at the population of smokers and those who suffer/suffered from lung cancer in each state within the United States. 
Then we will be taking a step further to determine the effects of one’s age, race, past smoking habits, etc, on the chance of raising lung cancer. 

skeleton.R: This R Script file contains codes that create the folders below. 

Subdirectories (under .Rproject)
	- code: All lines of code used for data cleaning and preparation, data analysis, plotting, etc,. are stored in the 'code' folder. The files are in either .R or .Rmd format.

	- rawdata: The 'rawdata' folder contains .csv and .txt files from various websites that are necessary for the analysis of our research topic. 
			   These are the files downloaded directly from the website as 'raw' data and have not been cleaned to be used for analysis.

	- data: The 'data' folder contains cleaned raw data files that are ready to be used for analysis.

	- images: The 'images' folder contains images of the plots used for analysis. They range from simple bar plots to linear regression plots.

	- resources: The 'resources' folder contains files for additional information which were included in the analysis of the topic. 
			     All information from the files in this folder were 
	             added manually. 

	- report: The 'report' folder contains the final outcome of the project as a .pdf file. 

libraries needed
	- readr
	- ggplot2
	- knitr