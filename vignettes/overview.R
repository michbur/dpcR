## ----eval=TRUE,echo=FALSE------------------------------------------------
library(knitr)
opts_chunk$set(fig.width=6, fig.height=6)

library(ggplot2)
library(xtable)

size_mod <- -2
cool_theme <- theme(plot.background=element_rect(fill = "transparent",
                                                 colour = "transparent"),
                    panel.grid.major = element_line(colour="lightgrey", linetype = "dashed"),
                    panel.background = element_rect(fill = "white", colour = "black"),
                    legend.background = element_rect(fill="NA"),
                    legend.position = "bottom",
                    axis.text = element_text(size=12 + size_mod),
                    axis.title.x = element_text(size=15 + size_mod, vjust = -0.1), 
                    axis.title.y = element_text(size=15 + size_mod, vjust = 1),
                    strip.text = element_text(size=15 + size_mod, face = "bold"),
                    strip.background = element_rect(fill = "#9ecae1", colour = "black"),
                    legend.text = element_text(size=12 + size_mod), 
                    legend.title = element_text(size=15 + size_mod),
                    plot.title = element_text(size=20 + size_mod))

load("vig.RData")

## ----eval=TRUE-----------------------------------------------------------
library(dpcR)
#generate some data from 15x16 array. Let's presume, that we have results from two plates
sample_runs <- matrix(rpois(480, lambda = 1.5), ncol = 2)
#check its class - it's a typical R structure
class(sample_runs)
#save it to adpcr object
adpcr_experiments <- create_dpcr(sample_runs, n = c(240L, 240L), type = "nm", adpcr = TRUE)
class(adpcr_experiments)

## ----eval=TRUE-----------------------------------------------------------
#remember, you can plot only single panel at once 
plot_panel(extract_dpcr(adpcr_experiments, 1), nx_a = 15, ny_a = 16, main = "Experiment 1")

## ----eval=TRUE-----------------------------------------------------------
#remember, you can plot only single panel at once 
plot_panel(binarize(extract_dpcr(adpcr_experiments, 1)), nx_a = 15, ny_a = 16, main = "Experiment 1")

## ----eval=TRUE-----------------------------------------------------------
#compare experiments using GLM
#set a random seed for simulations
set.seed(54321)

#1. Simulate results of three different experiments with two repetitions each
#experiment 2 differs significantly from others
adpcr1 <- sim_adpcr(m = 20, n = 765, times = 1e5, pos_sums = FALSE, n_panels = 2)
adpcr2 <- sim_adpcr(m = 130, n = 765, times = 1e5, pos_sums = FALSE, n_panels = 2)
adpcr2 <- rename(adpcr2, exper = "Experiment2")
adpcr3 <- sim_adpcr(m = 50, n = 765, times = 1e5, pos_sums = FALSE, n_panels = 2)
adpcr3 <- rename(adpcr3, exper = "Experiment3")
#2. Join results for convenience
adpcrs <- bind_dpcr(adpcr1, adpcr2, adpcr3)

#3. Perform test
comp <- test_counts(adpcrs)

#4. See summary of the test
summary(comp)

#5. Plot results of the test 
plot(comp, aggregate = FALSE)

#6. Aggregate runs to their groups
plot(comp, aggregate = TRUE)

#7. extract coefficients for the further usage
coef(comp)

## ----eval=TRUE-----------------------------------------------------------
#1. Perform multiple test comparison using data from the previous example
comp_ratio <- test_counts(adpcrs, model = "ratio")

#2. See summary of the test
summary(comp_ratio)

#3. Plot results of the test 
plot(comp_ratio, aggregate = FALSE)

#4. Aggregate runs to their groups
plot(comp_ratio, aggregate = TRUE)

#5. extract coefficients for the further usage
coef(comp)

#compare results of two methods
par(mfrow=c(2,1))
plot(comp, aggregate = FALSE)
title("GLM")
plot(comp_ratio, aggregate = FALSE)
title("Ratio")
par(mfrow=c(1,1))



## ----eval=TRUE,echo=FALSE------------------------------------------------
ggplot(data=madpcr_comp,aes(x = value, fill = method)) +
  geom_density(alpha = 0.3) + 
  scale_fill_discrete("Confidence intervals:") + 
  scale_y_continuous("Density") + 
  scale_x_continuous("Fraction of wrongly assigned experiments") + 
  cool_theme

## ----eval=TRUE,echo=FALSE------------------------------------------------
ggplot(m_coverage2, aes(x = prop, y = value, fill = method)) +
  geom_bar(stat="identity", position = "dodge") +
  scale_y_continuous("Probability coverage") + 
  scale_x_discrete(expression(lambda)) +
  scale_fill_discrete("Confidence intervals:") + 
  geom_hline(y = 0.95, colour = "black", size = 1, linetype = 5) +
  facet_wrap(~ coverage, nrow = 2) + 
  cool_theme

## ----eval=TRUE,echo=FALSE,results="asis"---------------------------------
dat <- as.data.frame(aggregate(value ~ method + coverage, m_coverage2, mean))

colnames(dat) <- c("Method name", "Type of coverage", "Value")
print(xtable(dat), 
      include.rownames = FALSE, type = "html")

