# social-configural-dynamics

Social and configural effects on the cognitive dynamics of perspective-taking

This repo contains the data and code for the analyses presented in our manuscript, Galati, A., Dale, R., & Duran, N. D. (submitted). Social and configural effects on the cognitive dynamics of perspective-taking.

A pre-print can be found at: https://psyarxiv.com/k2byq/

If you have any questions, please email me at alexia.galati@gmail.com

Link to OSF repository : https://osf.io/56rqw/

<b>Overview</b>

The "Data" folder contains:
<ul><li>subfolders for each of the 3 experiments in this study (Exp 1A, Exp 1B, Exp 2) with the raw data as .txt files from each participant</li>
<li>a subfolder with the data of the follow-up experiments (follow-ups to Exp 1 and to Exp 2) in a single .csv file.</li> 

The "Code" folder contains: 
<ul><li> batchTrajectoryAnalysis.R files: code for processing the raw data from mouse-tracking to generate the dependent measures</li>
<li>churnedRawTrajectoryData.R files: output files generated from batchTrajectoryAnalysis.R files, which are loaded to run linear mixed effects modes</li>
<li> Code_LMERs.R files: code for obtaining the descriptives and running the linear mixed effects models reported in the paper</li> 
<li> code for analyzing the data of the follow-up experiments</li></ul>


<b>Notes</b>

For viewing the files in a browser, we recommend selecting the .md files, rather than the corresponding .Rmd files. (Analyses should be run using the .Rmd file of the same name in RStudio.) 


<b>Acknowledgments</b>
This project has received funding from the European Union’s Horizon 2020 research and innovation programme under the Marie Sklodowska-Curie grant agreement No 705037 to Alexia Galati. We are participating in the Pilot for Open Research Data in Horizon 2020.