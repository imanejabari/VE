# Vaccine Effectiveness study

## Context
This repository was created in the context of my master's thesis at UCLouvain:

**Title:** "A posteriori analysis of COVID-19 public interventions through vaccine effectiveness "

**Supervisor:** Raphaël Jungers

## Contents

The repository is composed of three R project files:
- _StudyDesigns_: an R project that gathers all the code that has been used in the VE analysis within the Case-Control study design and the Test-Negative study design in France and in Belgium as well as the different analyses.
- _RRR_: an R project for the computation of the Relative Risk Reduction in France and in Belgium
- _Timelines_: an R project for generating the timeline plots that are displayed (this is just an added bonus)

One can run each project by opening the corresponding .Rproj file in RStudio, which sets the working directory to the project directory and then run the .R codes.

And it contains a folder gathering all the data collected for the study, which is presented in Part II.


### Data
- _CI_BELGIUM_: Cumulative incidence in Belgium (raw version) - Data available in Sciensano reports presented in Section 3.3 and Appendix B.3 (Data transcribed from reports)
- _COVID19BE_: Open data for Belgium, containing multiple sheets, corresponding to the datasets presented in Section 3.1 and Appendix B.1 (Original data downloaded on https://epistat.sciensano.be/covid/, on March 17, 2023
- _Data_France_: Matched data for France (raw version), presented in Section 4.1 and Appendix C.1. (Original data downloaded on https://data.drees.solidarites-sante.gouv.fr/explore/dataset/covid-19-resultats-par-age-issus-des-appariements-entre-si-vic-si-dep-et-vac-si/information/, Updated on January 27, 2023)
- _vacsi_FR_: Vaccination data in France (raw version), presented in Section 4.2 and Appendix C.2 (Original data downloaded on https://datavaccin-covid.ameli.fr/explore/dataset/donnees-vaccination-par-tranche-dage-type-de-vaccin-et-departement/information/?sort=departement_residence, Updated on January 13, 2023)

### Additional Data
- _RR_BEL_: Relative Risk Reduction reported by Sciensano in the reports.
- _VE_TND_Sci_: Estimation of the Vaccine Effectiveness (VE) against symptomatic infection (through TND), from the graphs in Sciensano reports.
- _VE_TND_BEL_hospi_: Estimation of the Vaccine Effectiveness (VE) against hospitalization and ICU admission (through TND), from the graphs in Sciensano reports.


### StudyDesigns project

This R project is for the analysis of vaccine effectiveness through study designs using logistic regression.

It contains one folder for _Codes_ and one folder for _Data_.

The _Data_ folder contains a _Data_FR_ file which is a reworked version of the initial _Data_France_ file for the purpose of logistic regression. It contains data about PCR tests in France, by date, vaccination status, and age.

Data regarding Belgium, can not be published because of confidentiality reasons. However, it is available in the Appendix 1 file attached to the thesis, the data used in the code is the one in the _Data_BEL_ sheet (which is a slightly modified version of the _VE_REQUEST_ original data). To use it in the code, please extract the _Data_BEL_ sheet into a new .xlsx file called _Data_BEL_ and add it to the _Data_ folder where the _Data_FR_ file is also placed.

The _Codes_ folder contains four files:
- _Analysis - 14 days.R_: This code is for the 14-day case counting window analysis for primo-vaccination in France
- _Analysis.R_: This code is for the the Case-Control and TND analyses in Belgium and France, with the comparison between both countries. The comments should guide you into the code.
- _Data.R_: This code is used for data management, to put the .xlsx files in a special format
- _Library.R_: This code gathers all the libraries that are used (some may be not used)


### RRR project

This project is for the computation of vaccine effectiveness through Relative Risk Reduction.

It also contains a folder for _Codes_ and a folder for _Data_.

The _Data_ folder contains three files:
- _Data_FR_: another modified version of the initial _Data_France_ file, containing only the necessary info for the computation of Relative Risk Reduction
- _CI_BEL_ : modified version of the initial _CI_BELGIUM_, containing the 14-day cumulative incidence by age group and vaccination status 
- _Vaccin_FR_ : modified version of the initial _vacsi_FR_ file, containing only the necessary info for the computation of Relative Risk Reduction

The _Code_ folder contains two files for RRR computation, one for Belgium and one for France. 

___

This README provides an overview of the different R codes implemented for the analysis and the datasets available for analysis. Please refer to Part II of the thesis and Appendix B & C, for more detailed information on each dataset.
