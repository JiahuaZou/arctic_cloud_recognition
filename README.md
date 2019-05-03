# arctic cloud recognition
Jiahua Zou & Zhengyi Sui    
Team: ML Boys    
UC Berkeley Statistics 154 "Machine Learning and Statistical Modeling" Project 2

# Files   
      
projet2.pdf, yu2008.pdf, macros.tex: Files provided by the faculty to gives a background of our project.    
    
proj2code.Rmd: This RMarkdown file contains all the codes used to generate all the results, tables, and figures from our report.pdf. It is the backbone of this project, and it is reproducible. There are comments and markdown inside that will help anyone insterested to understand what we did and reproduce our result.       
    
CVgeneric.R: This R file contains the code we use to customize a cross validation function. Please note thta this is already contained in the proj2code.Rmd.
    
image_data: contains all the data we used to produce our analysis, provided by the faculty.       
    
report.doc: This is a word version of the report. This contains our analysis produced from proj2code.Rmd. We edit the writing portion of our report on this file and export it to a pdf report.    
    
report.pdf: This is the pdf version of the report exported from report.doc
           
# Reproduce report            
            
To reproduce our work, please follow the steps below:
            
1. Pull this repository and install every package we have listed in proj2code.Rmd.        
2. Follow the comment and writing in Proj2code, run everything, make sure you use our data preprocessing pipeline
3. KNN and Random Forest take a lot of time, make sure you do not run too many times.       
4. Every variable ends with a 1 means this is based on the first data split, every variable ends with 2 means this is based on the second data split.       
