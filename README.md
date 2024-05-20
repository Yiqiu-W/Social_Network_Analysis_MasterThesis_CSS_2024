#### Social_Network_Analysis_MasterThesis_CSS_2024

##### During adolescence, individuals place increasing emphasis on peer relations. Both positive and negative relationships contribute to adolescents’ academic performances and well-being. Adolescence is also a period when individuals actively engage in gossiping. Despite the various benefits brought by gossip such as information exchange, norm regulation and amusement, those who are labelled as “gossipers” tend to suffer from bad reputation and thus becoming less favored in friendship selection. 

##### Instead of focusing on evident gossiping behaviors, the current study takes an innovative approach of looking at gossip perception and the friendship and disliking relationship between perceived gossipers who are not necessarily real gossipers but are nominated when others were being asked “Who do you think talks you out with other classmates behind your back?” and the self-perceived targets. 

##### The research is conducted by applying Meta Analysis and Bayesian Multilevel Random Coefficients Analysis to 11 Hungarian classrooms of the RECENS project “Competition and Negative Networks” (2017) using Stochastic Actor Oriented Modelling (SAOM) in R. 

##### Results from Meta Analysis are in line with the expectation that gossip perception should make the self-perceived target less likely to befriend the perceived gossiper and more likely to dislike him/her. Interestingly, it is also found that being perceived as a gossiper should make one more likely to like the self-perceived target. No evidence is found to support that having a shared perceived gossiper should bring two self-perceived targets closer or a self-perceived target should be more likely to hate the friend of the perceived gossiper following Heider’s Balance Theory (1946). However, no gossip perception related effects are found statistically significant according to the results of the Bayesian approach. Overall, gossip perception is not powerful enough to affect adolescents’ attitude, whether positive or negative, towards one another.

##### The research is conducted in R (R Development Core Team, v 4.2.3, 2023) with packages RSiena (Snijders et al., v1.4.7, 2024), sna (Butts, v 2.7-2,2023), tidyverse (Wickham et al., v 2.0.0, 2019), haven (Wickham et al., v 2.5.4, 2023), ggplot2(Wickham, v 3.5.0, 2016), kabelExtra (Zhu, v1.4.0, 2024), igraph (Csárdi et al., v 2.0.2 , 2024), multiSiena (Snijders et al., v 1.2.16, 2023) and parallel (R Development Core Team, v 4.2.3, 2023).

##### Analysis was conducted with reference to example code from RSiena website (https://www.stats.ox.ac.uk/~snijders/siena/siena.html). 

##### Links to code for basic operation using RSiena:
##### stats.ox.ac.uk/~snijders/siena/basicRSiena.r
##### stats.ox.ac.uk/~snijders/siena/RSienaSNADescriptives.R
##### stats.ox.ac.uk/~snijders/siena/Rscript02SienaVariableFormat.R
##### stats.ox.ac.uk/~snijders/siena/Rscript03SienaRunModel.R

##### Links to code for meta-analysis:
##### stats.ox.ac.uk/~snijders/siena/RscriptMultipleGroups_meta2.R
##### stats.ox.ac.uk/~snijders/siena/sienaGOF_vdB.R

##### Links to code for Bayesian method:
##### stats.ox.ac.uk/~snijders/siena/RscriptsienaBayes.r
##### stats.ox.ac.uk/~snijders/siena/RscriptsienaBayes_3.r
##### stats.ox.ac.uk/~snijders/siena/BayesPlots.r

#### References
##### Butts, C.T. (2023). sna: Tools for Social Network Analysis. R package version 2.7-2, https://CRAN.R-project.org/package=sna
##### Csárdi G, Nepusz T, Traag V, Horvát Sz, Zanini F, Noom D, Müller K (2024). igraph: Network Analysis and Visualization in R.doi:10.5281/zenodo.7682609 <https://doi.org/10.5281/zenodo.7682609>, R package version 2.0.2, https://CRAN.R-project.org/package=igraph
##### R Core Team (2023). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.
##### Snijders T, Koskinen J, Ripley R (2023). multiSiena: multiSiena -Simulation Investigation for Multilevel Empirical Network Analysis. R package version 1.2.16, http://www.stats.ox.ac.uk/~snijders/siena
##### Snijders, T.A.B., Ripley, R.M., Boitmanis, K., Steglich, C., Niezink, N.M.D., Schoenenberger, F., & Amati, V. (2024). RSiena – Simulation Investigation for Empirical Network Analysis, R package version 1.4.7. https://www.stats.ox.ac.uk/~snijders/siena/
##### Wickham, H. (2016). ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016.
##### Wickham, H., Averick, M., Bryan, J., Chang, W., McGowan, L.D., François, R., Grolemund, G., Hayes, A., Henry, L., Hester, J., Kuhn, M., Pedersen, T.L., Miller, E., Bache, S.M., Müller, K., Ooms, J., Robinson, D., Seidel, D.P., Spinu, V., Takahashi, K., Vaughan, D., Wilke, C., Woo, K. & Yutani, H. (2019). “Welcome to the tidyverse.” _Journal of Open Source Software_, *4*(43), 1686. doi:10.21105/joss.01686 https://doi.org/10.21105/joss.01686
##### Wickham, H., Miller, E. & Smith, D. (2023). haven: Import and Export 'SPSS', 'Stata' and 'SAS' Files. R package version 2.5.4, https://CRAN.R-project.org/package=haven
##### Zhu, H. (2024). kableExtra: Construct Complex Table with 'kable' and Pipe Syntax. R package version 1.4.0, https://CRAN.R-project.org/package=kableExtra
