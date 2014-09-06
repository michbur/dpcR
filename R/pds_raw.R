#' Plasmid dilution series raw data
#' 
#' These are the raw data from the \code{pds_raw} data set as measured by the
#' BioRad QX100 Droplet Digital PCR System.
#' 
#' The results can be as calculated by the BioRad QX100 Droplet Digital PCR
#' System are to be found in \code{pds}.
#' 
#' Setup: Duplex assay with constant amount of genomic DNA and six 10-fold
#' dilutions of plasmid DNA with 4 replicates, ranging theoretically from ~
#' 10^4 to 10^-1 copies/ micro L plus 4 replicates without plasmid DNA.
#' Included are No-gDNA-control and No-template-control, 2 replicates each.
#' 
#' Annotation: FX.Y (X = dilution number, Y = replicate number). Hardware:
#' Bio-Rad QX100 Droplet digital PCR system Details: Genomic DNA isolated from
#' Pseudomonas putida KT2440. Plasmid is pCOM10-StyA::EGFP StyB [Jahn et al.,
#' 2013, Curr Opin Biotechnol, Vol. 24 (1): 79-87]. Template DNA was heat
#' treated at 95 degree Celsius for 5 min prior to PCR. Channel 1, primers for
#' genomic DNA marker ileS, Taqman probes (FAM labelled). Channel 2, primers
#' for plasmid DNA marker styA, Taqman probes (HEX labelled).
#' 
#' 
#' @name pds_raw
#' @docType data
#' @format The format is: List of 32 $ A01:'data.frame': 11964 obs. of 3
#' variables: ..$ Assay1.Amplitude: num [1:11964] 397 399 402 416 417 ...  ..$
#' Assay2.Amplitude: num [1:11964] 3732 3808 4007 3778 3685 ...  ..$ Cluster :
#' int [1:11964] 4 4 4 4 4 4 4 4 4 4 ...  $ A02:'data.frame': 11198 obs. of 3
#' variables: ..$ Assay1.Amplitude: num [1:11198] 310 429 433 445 445 ...  ..$
#' Assay2.Amplitude: num [1:11198] 605 1092 994 1092 1140 ...  ..$ Cluster :
#' int [1:11198] 1 1 1 1 1 1 1 1 1 1 ...  $ A03:'data.frame': 9672 obs. of 3
#' variables: ..$ Assay1.Amplitude: num [1:9672] 413 469 477 480 489 ...  ..$
#' Assay2.Amplitude: num [1:9672] 781 1160 1205 1117 1098 ...  ..$ Cluster :
#' int [1:9672] 1 1 1 1 1 1 1 1 1 1 ...  $ A04:'data.frame': 11901 obs. of 3
#' variables: ..$ Assay1.Amplitude: num [1:11901] 442 459 468 469 470 ...  ..$
#' Assay2.Amplitude: num [1:11901] 3169 1161 1098 1064 1107 ...  ..$ Cluster :
#' int [1:11901] 4 1 1 1 1 1 1 1 1 1 ...  $ B01:'data.frame': 11592 obs. of 3
#' variables: ..$ Assay1.Amplitude: num [1:11592] 438 449 451 453 454 ...  ..$
#' Assay2.Amplitude: num [1:11592] 3996 4237 3910 3648 3832 ...  ..$ Cluster :
#' int [1:11592] 4 4 4 4 4 4 4 4 4 4 ...  $ B02:'data.frame': 11715 obs. of 3
#' variables: ..$ Assay1.Amplitude: num [1:11715] 341 404 411 417 465 ...  ..$
#' Assay2.Amplitude: num [1:11715] 705 1013 892 936 996 ...  ..$ Cluster : int
#' [1:11715] 1 1 1 1 1 1 1 1 1 1 ...  $ B03:'data.frame': 11194 obs. of 3
#' variables: ..$ Assay1.Amplitude: num [1:11194] 31.2 266.9 281.6 286.1 300.3
#' ...  ..$ Assay2.Amplitude: num [1:11194] 668 555 508 585 571 ...  ..$
#' Cluster : int [1:11194] 1 1 1 1 1 1 1 1 1 1 ...  $ B04:'data.frame': 12813
#' obs. of 3 variables: ..$ Assay1.Amplitude: num [1:12813] 380 464 474 483 487
#' ...  ..$ Assay2.Amplitude: num [1:12813] 830 913 1143 1157 1032 ...  ..$
#' Cluster : int [1:12813] 1 1 1 1 1 1 1 1 1 1 ...  $ C01:'data.frame': 10903
#' obs. of 3 variables: ..$ Assay1.Amplitude: num [1:10903] 427 442 443 444 445
#' ...  ..$ Assay2.Amplitude: num [1:10903] 3803 3832 3634 3899 3932 ...  ..$
#' Cluster : int [1:10903] 4 4 4 4 4 4 4 4 4 4 ...  $ C02:'data.frame': 9638
#' obs. of 3 variables: ..$ Assay1.Amplitude: num [1:9638] 442 446 454 454 457
#' ...  ..$ Assay2.Amplitude: num [1:9638] 1107 1131 3644 881 3460 ...  ..$
#' Cluster : int [1:9638] 1 1 4 1 4 4 1 1 1 1 ...  $ C03:'data.frame': 12194
#' obs. of 3 variables: ..$ Assay1.Amplitude: num [1:12194] 461 466 470 475 475
#' ...  ..$ Assay2.Amplitude: num [1:12194] 842 1089 1156 1115 1194 ...  ..$
#' Cluster : int [1:12194] 1 1 1 1 1 1 1 1 1 1 ...  $ C04:'data.frame': 10889
#' obs. of 3 variables: ..$ Assay1.Amplitude: num [1:10889] 343 448 456 466 474
#' ...  ..$ Assay2.Amplitude: num [1:10889] 633 1149 1073 1161 1089 ...  ..$
#' Cluster : int [1:10889] 1 1 1 1 1 1 1 1 1 1 ...  $ D01:'data.frame': 11196
#' obs. of 3 variables: ..$ Assay1.Amplitude: num [1:11196] 110 413 417 435 444
#' ...  ..$ Assay2.Amplitude: num [1:11196] 734 3752 3736 3885 3720 ...  ..$
#' Cluster : int [1:11196] 1 4 4 4 4 4 4 4 4 4 ...  $ D02:'data.frame': 12013
#' obs. of 3 variables: ..$ Assay1.Amplitude: num [1:12013] 453 457 459 463 462
#' ...  ..$ Assay2.Amplitude: num [1:12013] 1113 1178 1054 1088 3108 ...  ..$
#' Cluster : int [1:12013] 1 1 1 1 4 1 4 1 1 4 ...  $ D03:'data.frame': 11126
#' obs. of 3 variables: ..$ Assay1.Amplitude: num [1:11126] 323 460 463 471 473
#' ...  ..$ Assay2.Amplitude: num [1:11126] 661 1138 1103 1091 1138 ...  ..$
#' Cluster : int [1:11126] 1 1 1 1 1 1 1 1 1 1 ...  $ D04:'data.frame': 12793
#' obs. of 3 variables: ..$ Assay1.Amplitude: num [1:12793] 363 433 442 459 460
#' ...  ..$ Assay2.Amplitude: num [1:12793] 703 1065 1071 1060 1119 ...  ..$
#' Cluster : int [1:12793] 1 1 1 1 1 1 1 1 1 1 ...  $ E01:'data.frame': 11823
#' obs. of 3 variables: ..$ Assay1.Amplitude: num [1:11823] 368 434 448 453 462
#' ...  ..$ Assay2.Amplitude: num [1:11823] 778 3751 3585 1125 3797 ...  ..$
#' Cluster : int [1:11823] 1 4 4 1 4 4 4 1 1 4 ...  $ E02:'data.frame': 12046
#' obs. of 3 variables: ..$ Assay1.Amplitude: num [1:12046] 268 413 414 454 455
#' ...  ..$ Assay2.Amplitude: num [1:12046] 582 3738 2412 1071 1076 ...  ..$
#' Cluster : int [1:12046] 1 4 4 1 1 1 1 1 1 1 ...  $ E03:'data.frame': 11026
#' obs. of 3 variables: ..$ Assay1.Amplitude: num [1:11026] 357 446 456 456 460
#' ...  ..$ Assay2.Amplitude: num [1:11026] 675 1138 1095 1145 1138 ...  ..$
#' Cluster : int [1:11026] 1 1 1 1 1 1 1 1 1 1 ...  $ E04:'data.frame': 12838
#' obs. of 3 variables: ..$ Assay1.Amplitude: num [1:12838] 460 467 472 477 482
#' ...  ..$ Assay2.Amplitude: num [1:12838] 1207 1238 1143 3754 1153 ...  ..$
#' Cluster : int [1:12838] 1 1 1 4 1 1 1 4 4 1 ...  $ F01:'data.frame': 12173
#' obs. of 3 variables: ..$ Assay1.Amplitude: num [1:12173] 354 389 457 456 466
#' ...  ..$ Assay2.Amplitude: num [1:12173] 739 2714 3888 3775 3857 ...  ..$
#' Cluster : int [1:12173] 1 4 4 4 4 4 4 1 1 4 ...  $ F02:'data.frame': 13786
#' obs. of 3 variables: ..$ Assay1.Amplitude: num [1:13786] 338 359 439 459 461
#' ...  ..$ Assay2.Amplitude: num [1:13786] 525 638 674 3891 1046 ...  ..$
#' Cluster : int [1:13786] 1 1 1 4 1 1 1 1 1 1 ...  $ F03:'data.frame': 11249
#' obs. of 3 variables: ..$ Assay1.Amplitude: num [1:11249] 431 445 447 448 455
#' ...  ..$ Assay2.Amplitude: num [1:11249] 1090 3330 1048 1187 1098 ...  ..$
#' Cluster : int [1:11249] 1 4 1 1 1 1 1 1 1 1 ...  $ F04:'data.frame': 12076
#' obs. of 3 variables: ..$ Assay1.Amplitude: num [1:12076] 407 424 436 446 447
#' ...  ..$ Assay2.Amplitude: num [1:12076] 699 1047 3683 1085 1088 ...  ..$
#' Cluster : int [1:12076] 1 1 4 1 1 1 1 1 1 1 ...  $ G01:'data.frame': 10188
#' obs. of 3 variables: ..$ Assay1.Amplitude: num [1:10188] 460 468 470 471 482
#' ...  ..$ Assay2.Amplitude: num [1:10188] 3813 3213 3949 3658 2202 ...  ..$
#' Cluster : int [1:10188] 4 4 4 4 4 4 4 1 4 4 ...  $ G02:'data.frame': 11018
#' obs. of 3 variables: ..$ Assay1.Amplitude: num [1:11018] 445 452 460 460 461
#' ...  ..$ Assay2.Amplitude: num [1:11018] 1064 1090 1054 1087 1116 ...  ..$
#' Cluster : int [1:11018] 1 1 1 1 1 1 1 1 1 1 ...  $ G03:'data.frame': 12073
#' obs. of 3 variables: ..$ Assay1.Amplitude: num [1:12073] 294 459 460 468 489
#' ...  ..$ Assay2.Amplitude: num [1:12073] 658 1105 1168 1160 1115 ...  ..$
#' Cluster : int [1:12073] 1 1 1 1 1 1 1 1 1 1 ...  $ G04:'data.frame': 12320
#' obs. of 3 variables: ..$ Assay1.Amplitude: num [1:12320] 444 476 484 491 498
#' ...  ..$ Assay2.Amplitude: num [1:12320] 831 1158 984 1237 1325 ...  ..$
#' Cluster : int [1:12320] 1 1 1 1 1 1 1 1 1 1 ...  $ H01:'data.frame': 12271
#' obs. of 3 variables: ..$ Assay1.Amplitude: num [1:12271] 377 388 404 409 433
#' ...  ..$ Assay2.Amplitude: num [1:12271] 2884 917 3679 830 2780 ...  ..$
#' Cluster : int [1:12271] 4 1 4 1 4 4 4 1 4 4 ...  $ H02:'data.frame': 12595
#' obs. of 3 variables: ..$ Assay1.Amplitude: num [1:12595] 364 411 419 421 424
#' ...  ..$ Assay2.Amplitude: num [1:12595] 691 836 2885 812 1177 ...  ..$
#' Cluster : int [1:12595] 1 1 4 1 1 1 1 1 1 1 ...  $ H03:'data.frame': 13905
#' obs. of 3 variables: ..$ Assay1.Amplitude: num [1:13905] 314 400 426 431 433
#' ...  ..$ Assay2.Amplitude: num [1:13905] 509 987 1037 1074 1053 ...  ..$
#' Cluster : int [1:13905] 1 1 1 1 1 1 1 1 1 1 ...  $ H04:'data.frame': 12972
#' obs. of 3 variables: ..$ Assay1.Amplitude: num [1:12972] 344 446 461 467 482
#' ...  ..$ Assay2.Amplitude: num [1:12972] 641 869 987 789 1117 ...  ..$
#' Cluster : int [1:12972] 1 1 1 1 1 1 1 1 1 1 ...
#' @author Michael Jahn, Stefan Roediger, Michal Burdukiewcz
#' @references Jahn et al., 2013, \emph{Curr Opin Biotechnol}, Vol. 24 (1):
#' 79-87
#' @source Michael Jahn Flow cytometry group / Environmental microbiology
#' Helmholtz Centre for Environmental Research - UFZ Permoserstrasse 15 / 04318
#' Leipzig / Germany phone +49 341 235 1318 michael.jahn [at] ufz.de /
#' www.ufz.de
#' @keywords datasets
#' @examples
#' 
#' #str(pds_raw)
#' bioamp(data = pds_raw[["A01"]], main = "Well A01", pch = 19)
#' 
NULL