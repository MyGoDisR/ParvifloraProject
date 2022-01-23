			ParvifloraProject
1. Description
The project for "Introduction to programming in R" course at Kozminski University. The aim of the project was to provide the retailer management with information regarding total sales for any given number of months. Unfortunately data comes from different systems and the challenge the team faced was to integrate it. The technology in use is R programming language. 
In the future if the management decides that it would like to see other data than the totals, the application would have to be a bit tweaked to return this data (at the moment it is preprocessed, but not returned and analysed).

2. Requirements to run
	* R in version 4.x with libraries installed:
	  - tidyverse (version 1.3.1, some functions available from dplyr 1.0.0 up)
	  - readxl
	  - ggplot2

3. Project structure
This section provides overview of the structure and explains components

ParvifloraProject  
    ├── R_project_script.R
    ├── LICENSE
    ├── README.txt
    ├── data 
    |     ├── (.xls, .csv .xlsx) input data files
    |     └── processed   
    ├── output
    |     └── plots  
    └── utilities
          ├── 1_process_sales_summaries.R
	  ├── 2_process_Daffodils.r
	  ├── 3_process_Stores.R
	  └── 4_analysis.R

  * R_project_script.R - the main orchestrating script, the only 	one which executes any code. It imports functions from 		scripts in /utilities directory and uses them to process 	data. If one is interested to see the pipeline steps - 		it's here. Not all the code was 'hidden' tough. Some 		crucial steps like joining data from different sources 		was kept here as it helps to understand the logic.

  * data - directory where raw files with data should be placed, 		the program reads data from this location
	 - /processed sub-directory - intended to store 			intermediate results of operation here in .Rds 			format for "checkpoint" access
  
  * output - directory where all the output from program will be 	stored .xlsx or .html reports can go directly here. 		Plots should be saved to /plots sub-dir.
  
  * utilities - scripts that define functions and constants used 	by those functions (e.g. column names for reading). 		Example: "process_Daffodils.r" will define functions 		that are used to process data from Daffodils sales 		system.

4. How to use the project

  1) Import sales data from company systems into /data directory
	- 1 "Stores.xlsx" file
	- 1 "Daffodilsyyyy.xls" file with as many months as 		  sheets as needed e.g.
	- x "Summary of Sales MMMM yyyy" files - format matters 	  as month and year information are extracted from name

  2) Run the code in R_project_script.R file
	* check if the file runs from terminal
	* if no add here that only possible in RStudio

  3) The outputs are in the output folder: 
	- plots showing
	? .html document with summary information
	? SummaryRaport.xlsx file with totals data after it has been processed - for the user to have access and possibility to make own, more detailed analysis


#### Authors
* Maciej Golik (46827@kozminski.edu.pl)
* Jakub Zapaśnik (38401@kozminski.edu.pl)
* Paweł Jędrzejczak (46471@kozminski.edu.pl)
* Michał Kloska (46341@kozminski.edu.pl)
* Daniel Lilla (38963@kozminski.edu.pl)