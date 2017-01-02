## About

The dpcReport web-server is a part of the [dpcR](http://cran.r-project.org/web/packages/dpcR/index.html) R package. All functionalities of the web-server are based on functions included in the package.

<img src="dpcR_logo.png" alt="HTML5 Icon" style="width:224px;height:120px">

dpcReport is a part of [pcRuniveRsum](http://michbur.github.io/pcRuniveRsum/).

### Structure of dpcReport

dpcReport consists of seven panels which can be chosen using the navigation bar above and are described below.

Tables allow multiconditional filtering as well as sorting. The content of each table may be downloaded or printed using the menu bar at the top of the table.  

Most of the figures in GUI are interactive. Static versions may be downloaded separately as .svg files.

1. **Input file**: Responsible for importing data into GUI. In case if data is not properly read, make sure that you have chosen a proper data format. In this panel you can also change name of the experiments, replicates and assays in loaded data. Your original file will be NOT modified in this manner. The modified file in a dpcR exchange format may be downloaded using **Save report** panel. The volume of the droplet and its uncertainty is already predifined for most of the popular dPCR systems, but it may be also altered here.
Since data formats for dPCR systems can change very rapidly, we provide users of the web server with [exemplary data files](https://github.com/michbur/dpcR_data). If a specific user-provided data set is not working with a specific data set, please reformat it accordin to exemplary files or use REDF, broader described in the [dpcR manual](http://michbur.github.io/dpcR_manual/articles/overview.html#redf).

2. **Data summary**: This panel contains two subpanels. *Summary table* is a tabular summary of dPCR experiments in the imported data. *Summary charts* present the summary in the graphical form: as a boxplot and a scatterchart. The summary consists of &lambda; values computed using Bhat's (Bhat et al. 2009) and Dube's (Dube et al. 2008) methods as well as the concentration of the template in the sample.   

3. **Comparison of runs**: Compares &lambda; values of runs using Multiple Ratio Test (Burdukiewicz et. al., 2016). The first table contains results of pairwise comparisions between runs. Second table assignes runs to the groups (Piepho, 2004). The figure presents values of &lambda; for individual runs and their groups.    

4. **Advanced analysis**: Contains specialized analytics tools.  
  + *Array spatial analysis* - analyses of spatial randomness of positive partitions distributed over the dPCR array. Presents arrays in graphical form. Allows regional analysis of the array.    
  + *Probability distribution* - studies distribution of dPCR data as well as the distribution of the &lambda;.    

5. **Save report**: Interactively builds report from the conducted analysis.    

6. **About**: Basic information about the GUI.    

7. **Quit**: Press the button to quit the GUI.

### References

The application was created using the methods derived from following publications: 

[1] L. D. Brown, T. T. Cai and A. DasGupta. "Interval Estimation
for a Binomial Proportion". In: _Statist. Sci._ 16.2 (maj. 2001),
pp. 101-133. DOI: 10.1214/ss/1009213286. <URL:
http://dx.doi.org/10.1214/ss/1009213286>.

[2] H. Piepho. "An Algorithm for a Letter-Based Representation of
All-Pairwise Comparisons". In: _Journal of Computational and
Graphical Statistics_ 13.2 (2004), pp. 456-466. ISSN: 10618600.
<URL: http://www.jstor.org/stable/1391186>.

[3] S. Dube, J. Qin and R. Ramakrishnan. "Mathematical analysis of
copy number variation in a DNA sample using digital PCR on a
nanofluidic device". Eng. In: _PloS one_ 3.8 (2008), p. e2876.
ISSN: 1932-6203. DOI: 10.1371/journal.pone.0002876.

[4] S. Bhat, J. Herrmann, P. Armishaw, et al. "Single molecule
detection in nanofluidic digital array enables accurate
measurement of DNA copy number". Eng. In: _Analytical and
bioanalytical chemistry_ 394.2 (2009), pp. 457-467. ISSN:
1618-2650. DOI: 10.1007/s00216-009-2729-5.

[5] R. M. Dorazio and M. E. Hunter. "Statistical Models for the
Analysis and Design of Digital Polymerase Chain Reaction (dPCR)
Experiments". In: _Analytical Chemistry_ 87.21 (2015), pp.
10886-10893. ISSN: 0003-2700. DOI: 10.1021/acs.analchem.5b02429.
<URL: http://dx.doi.org/10.1021/acs.analchem.5b02429> (visited on
02/29/2016).

[6] S. Rödiger, M. Burdukiewicz, K. Blagodatskikh, et al. "R as an
Environment for the Reproducible Analysis of DNA Amplification
Experiments". In: _The R Journal_ 7.2 (2015), pp. 127-150.

[7] M. Burdukiewicz, S. Rödiger, P. Sobczyk, et al. "Methods of
comparing digital PCR experiments". Eng. In: _Biomolecular
Detection and Quantification_ 28.9 (2016), pp. 14-19. ISSN:
2214-7535. DOI: 10.1016/j.bdq.2016.06.004.

### Contact us

[Michal Burdukiewicz](https://github.com/michbur)  

[Stefan Roediger](https://www.researchgate.net/profile/Stefan_Roediger)  
