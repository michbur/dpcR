---
title: "dpcReport"
author: "Michal Burdukiewicz, Stefan Roediger"
date: ""
output:
html_document:
css: report.css
toc: true
---

# dpcReport



Report generated on 2015-08-09 11:07:42 using [dpcR](http://github.com/michbur/dpcR) R package.  

Detected input file: none.  



















## Compare runs  

## Compare digital PCR experiments    
    
Significance codes:  0 â<U+0080><U+0098>\*\*\*â<U+0080><U+0099> 0.001 â<U+0080><U+0098>\*\*â<U+0080><U+0099> 0.01 â<U+0080><U+0098>\*â<U+0080><U+0099> 0.05 â<U+0080><U+0098>.â<U+0080><U+0099> 0.1 â<U+0080><U+0098> â<U+0080><U+0099> 1    
    
For example, if p-value has value between 0 and 0.001, it has significance code \*\*\*.

<!-- html table generated in R 3.2.1 by xtable 1.7-4 package -->
<!-- Sun Aug 09 11:07:42 2015 -->
<table border=1>
<tr> <th>  </th> <th> Compared pair of runs </th> <th> p-value </th> <th> Significance </th>  </tr>
  <tr> <td align="right"> Experiment1.2 - Experiment1.1 </td> <td> Experiment1.2 - Experiment1.1 </td> <td align="right"> 0.89 </td> <td>   </td> </tr>
  <tr> <td align="right"> Experiment1.3 - Experiment1.1 </td> <td> Experiment1.3 - Experiment1.1 </td> <td align="right"> 0.84 </td> <td>   </td> </tr>
  <tr> <td align="right"> Experiment2.1 - Experiment1.1 </td> <td> Experiment2.1 - Experiment1.1 </td> <td align="right"> 0.00 </td> <td> ** </td> </tr>
  <tr> <td align="right"> Experiment2.2 - Experiment1.1 </td> <td> Experiment2.2 - Experiment1.1 </td> <td align="right"> 0.01 </td> <td> * </td> </tr>
  <tr> <td align="right"> Experiment2.3 - Experiment1.1 </td> <td> Experiment2.3 - Experiment1.1 </td> <td align="right"> 0.00 </td> <td> ** </td> </tr>
  <tr> <td align="right"> Experiment1.3 - Experiment1.2 </td> <td> Experiment1.3 - Experiment1.2 </td> <td align="right"> 0.89 </td> <td>   </td> </tr>
  <tr> <td align="right"> Experiment2.1 - Experiment1.2 </td> <td> Experiment2.1 - Experiment1.2 </td> <td align="right"> 0.00 </td> <td> ** </td> </tr>
  <tr> <td align="right"> Experiment2.2 - Experiment1.2 </td> <td> Experiment2.2 - Experiment1.2 </td> <td align="right"> 0.02 </td> <td> * </td> </tr>
  <tr> <td align="right"> Experiment2.3 - Experiment1.2 </td> <td> Experiment2.3 - Experiment1.2 </td> <td align="right"> 0.00 </td> <td> ** </td> </tr>
  <tr> <td align="right"> Experiment2.1 - Experiment1.3 </td> <td> Experiment2.1 - Experiment1.3 </td> <td align="right"> 0.00 </td> <td> ** </td> </tr>
  <tr> <td align="right"> Experiment2.2 - Experiment1.3 </td> <td> Experiment2.2 - Experiment1.3 </td> <td align="right"> 0.04 </td> <td> * </td> </tr>
  <tr> <td align="right"> Experiment2.3 - Experiment1.3 </td> <td> Experiment2.3 - Experiment1.3 </td> <td align="right"> 0.01 </td> <td> ** </td> </tr>
  <tr> <td align="right"> Experiment2.2 - Experiment2.1 </td> <td> Experiment2.2 - Experiment2.1 </td> <td align="right"> 0.43 </td> <td>   </td> </tr>
  <tr> <td align="right"> Experiment2.3 - Experiment2.1 </td> <td> Experiment2.3 - Experiment2.1 </td> <td align="right"> 0.84 </td> <td>   </td> </tr>
  <tr> <td align="right"> Experiment2.3 - Experiment2.2 </td> <td> Experiment2.3 - Experiment2.2 </td> <td align="right"> 0.70 </td> <td>   </td> </tr>
   </table>

### The mean number of template molecules per partition    
    
Table below contains the mean number of template molecules per partition (&lambda;) and its confidence intervals. The confidence intervals were adjusted to assure stable 0.95 simultaneous coverage probability, which offers more reliable comparision of technical repeats.    
    
The group is assigned automatically to the run by the statistical test and is represented by a single letter (in case of the intermediate groups, a combination of letters). Groups contain experiments which are assessed as similar considering the value of &lambda;.

<!-- html table generated in R 3.2.1 by xtable 1.7-4 package -->
<!-- Sun Aug 09 11:07:42 2015 -->
<table border=1>
<tr> <th>  </th> <th> Run </th> <th> Experiment name </th> <th> Replicate ID </th> <th> Assigned group </th> <th> &lambda; </th> <th> &lambda; (lower CI) </th> <th> &lambda; (upper CI) </th> <th> k </th> <th> n </th>  </tr>
  <tr> <td align="right"> 2 </td> <td> Experiment1.1 </td> <td> Experiment1 </td> <td> 1 </td> <td> a </td> <td align="right"> 0.13 </td> <td align="right"> 0.10 </td> <td align="right"> 0.17 </td> <td align="right"> 93.00 </td> <td align="right"> 765 </td> </tr>
  <tr> <td align="right"> 4 </td> <td> Experiment1.2 </td> <td> Experiment1 </td> <td> 2 </td> <td> a </td> <td align="right"> 0.13 </td> <td align="right"> 0.10 </td> <td align="right"> 0.17 </td> <td align="right"> 96.00 </td> <td align="right"> 765 </td> </tr>
  <tr> <td align="right"> 6 </td> <td> Experiment1.3 </td> <td> Experiment1 </td> <td> 3 </td> <td> a </td> <td align="right"> 0.14 </td> <td align="right"> 0.11 </td> <td align="right"> 0.18 </td> <td align="right"> 99.00 </td> <td align="right"> 765 </td> </tr>
  <tr> <td align="right"> 8 </td> <td> Experiment2.1 </td> <td> Experiment2 </td> <td> 1 </td> <td> b </td> <td align="right"> 0.22 </td> <td align="right"> 0.18 </td> <td align="right"> 0.28 </td> <td align="right"> 154.00 </td> <td align="right"> 765 </td> </tr>
  <tr> <td align="right"> 10 </td> <td> Experiment2.2 </td> <td> Experiment2 </td> <td> 2 </td> <td> b </td> <td align="right"> 0.19 </td> <td align="right"> 0.15 </td> <td align="right"> 0.24 </td> <td align="right"> 135.00 </td> <td align="right"> 765 </td> </tr>
  <tr> <td align="right"> 12 </td> <td> Experiment2.3 </td> <td> Experiment2 </td> <td> 3 </td> <td> b </td> <td align="right"> 0.21 </td> <td align="right"> 0.17 </td> <td align="right"> 0.26 </td> <td align="right"> 147.00 </td> <td align="right"> 765 </td> </tr>
   </table>

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 

The chart above shows to which groups runs were assigned using the statistical test. The group is marked by the letter on the left to the point representing the value of lambda.
