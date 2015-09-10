---
output: 
  html_document: 
    keep_md: yes
---
## About

The dpcReport is a part of the [dpcR](http://cran.r-project.org/web/packages/dpcR/index.html) R package.

<img src="dpcR_logo.png" alt="HTML5 Icon" style="width:224px;height:120px">

### Structure of dpcReport

dpcReport consists of seven panels. You can freely change panels using the navigation panel above.  

All tables allow multiconditional filtering as well as sorting. The content of each table may be downloaded or printed using the menu bar at the top of the table.  

Most of the figures in GUI are interactive and yield additional information after a mouse click. The may be downloaded as .svg files.

1. **Input file**: It is responsible for loading your data. In case if data is not properly read, make sure that you have chosen a proper data format. In this panel you can also change name of the experiments and runs in loaded data. Your original file will be NOT modified in this manner, changes affect only dpcReport analysis.    

2. **Data summary**: This panel contains two subpanels. *Summary table* is a tabular summary of dPCR experiments extracted from your data. *Summary charts* present the summary in the graphical form: as a boxplot and a scatterchart.   

3. **Comparison of runs**: Compares &lambda; values of runs. The first table contains results of pairwise comparisions between runs. Second table assignes runs to the groups. The figure presents values of &lambda; for individual runs and their groups.    

4. **Advanced analysis**: Contains specialized analytics tools.  
  + *Array spatial analysis* - analyses of spatial randomness of positive partitions distributed over the dPCR array. Presents arrays in graphical form. Allows regional analysis of the array.    
  + *Probability distribution* - studies distribution of dPCR data as well as the distribution of the &lambda;.    

5. **Save report**: Interactively builds report from the conducted analysis.    

6. **About**: Basic information about the GUI.    

7. **Quit**: Press the button to quit the GUI.

### References

The application was created using the results derived from following publications: 

[1] L. D. Brown, T. T. Cai and A. DasGupta. "Interval Estimation
for a Binomial Proportion". In: _Statist. Sci._ 16.2 (2001), pp.
101-133. DOI: 10.1214/ss/1009213286. <URL:
http://dx.doi.org/10.1214/ss/1009213286>.

[2] S. Dube, J. Qin and R. Ramakrishnan. "Mathematical analysis of
copy number variation in a DNA sample using digital PCR on a
nanofluidic device". Eng. In: _PloS one_ 3.8 (2008), p. e2876.
ISSN: 1932-6203. DOI: 10.1371/journal.pone.0002876.

[3] S. Bhat, J. Herrmann, P. Armishaw, et al. "Single molecule
detection in nanofluidic digital array enables accurate
measurement of DNA copy number". Eng. In: _Analytical and
bioanalytical chemistry_ 394.2 (2009), pp. 457-467. ISSN:
1618-2650. DOI: 10.1007/s00216-009-2729-5.

[4] S. Roediger, M. Burdukiewicz, K. Blagodatskikh, et al. "R as an
Environment for the Reproducible Analysis of DNA Amplification
Experiments". In: _The R Journal_ 7.2 (2015), pp. 127-150.

### Authors

[Michal Burdukiewicz](https://github.com/michbur)  

[Stefan Roediger](http://www.hs-lausitz.de/groups/multiplex-assays/bildbasierte-assays-imagebased-assays/members.html)  
