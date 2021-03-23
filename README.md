Analysis code for <a href="https://github.com/BDI-pathogens/covid-19_instant_tracing/blob/master/Epidemiological_Impact_of_the_NHS_COVID_19_App_Public_Release_V1.pdf" target="_blank">Wymant & Ferretti et al. 2021</a> (under review; correspondence to <a href="https://www.bdi.ox.ac.uk/Team/christophe-fraser" target="_blank"> Christophe Fraser</a>).  
- `regress_uptake_vs_cases_generate_data_V1paper_cleaned.R` is how we collated, wrangled and transformed data at the level of lower-tier local authorities (LTLAs), in preparation for the regressions reported in our statistical analysis.
The input data for this is in the `data_for_paper_public` subdirectory.
THE APP UPTAKE DATA IN HERE IS SIMULATED, NOT REAL! It is provided to give data in the format we used, so that the code can be run for illustration. It is also somewhat unrealistic in that it is constant over time for each local authority, and wholly uncorrelated with COVID-19 case numbers, unlike our findings with real data.  
- `regress_uptake_vs_cases_regressions_V1paper_cleaned.R` contains the regressions reported for our main-text statistical analysis, and the variant analyses of the Supplementary Information.
Code here should be executed after running the regression preparation code (i.e. keeping the objects in memory).
Remember - the simulated app uptake data provided is fake and uncorrelated with cases, so these illustratory regressions will not show the real effect of the app.
- `ConvolutionsSAR.R` is how we calculated the secondary attack rate (SAR).
Input data, not provided, consisted of the number of notifications received each day and the number of positive test results reported among those recently notified.
- `code_effect_app_impact_notifications_V1paper_cleaned.R` is how we estimated the number of cases averted with our modelling analysis. Input data not provided.
