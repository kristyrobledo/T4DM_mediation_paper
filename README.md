---
title: "T4DM_mediation_paper"
author: "Kristy Robledo"
date: "2023-05-16"
output: html_document
---


```r
knitr::opts_chunk$set(echo = TRUE)

## read in packages

library(knitr)
library(officedown)
library(officer)
library(gtsummary)
library(flextable)
library(tidyverse)
library(janitor)
library(haven)
library(labelled)
library(glue)
library(cowplot)
library(medflex)

## dataset of all variables used in paper
df<-readRDS("mediationdf.rds")
```

# T4DM data structure

## Outcomes at two years

- categorical outcome of diabetes `ogtt_gt11` (`1`=diabetes, `0`=no diabetes) 
- continuous outcome of change in 2hour glucose by OGTT `change_ogtt` (year 2 minus baseline)

## Baseline covariates

As included in adjusted analysis in original paper ([Wittert et al 2021](https://doi.org/10.1016/S2213-8587(20)30367-3)):

- `ogtt_base`: 2hour glucose at baseline
- `siteid`: study site
- `waist_gp`: waist circumference stratification variable (`1`=95–100 cm, `2`=101–115 cm, `3`>115 cm )
- `age_ge60`: age stratification variable, age greater than or equal to 60 (`1`=yes, `0`=no)
- `dm_hist`: first-degree family history of type 2 diabetes (`1`=yes, `0`=no)
- `ssri_base`: SSRI use at baseline (`1`=yes, `0`=no)
- `t_gp`: grouped as `1`= Low (<8.0 nmol/L), `2`=Medium (8.0 to <11.0 nmol/L), `3`= High (>=11.0 nmol/L)
- `smoker`: current smoker (`1`=yes, `0`=no)

plus 

- `treatment`: with testosterone `1`=Testosterone, `0`=placebo

## Baseline mediators

- `bmleanms_base`: baseline lean mass
- `bmfatms_base`: baseline fat mass
- `bmapfm_base`: baseline abdominal fat percentage
- `grip_base`: baseline grip strength
- `e2_base`: baseline E2
- `shbg_base`: baseline SHBG

## Mediators

- `leanmass_change`: change in lean mass
- `fatmass_change`: change in fat mass
- `abdomfat_change`: change in abdominal fat percentage
- `grip_change`: change in grip strength
- `e2_change`: change in E2
- `shbg_change`: change in SHBG

So, the dataset looks like so, using the first 2 patients as an example:


```r
df %>%
  head(n=2) %>%
  flextable()
```

<div class="tabwid"><style>.cl-eff7c910{}.cl-efe92306{font-family:'Arial';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-efef64a0{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-efef64a1{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-efefb2c0{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-efefb2c1{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 1.5pt solid rgba(102, 102, 102, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-efefb2c2{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-efefb2c3{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-efefb2c4{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-efefb2c5{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 1.5pt solid rgba(102, 102, 102, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.tabwid {
  font-size: initial;
  padding-bottom: 1em;
}

.tabwid table{
  border-spacing:0px !important;
  border-collapse:collapse;
  line-height:1;
  margin-left:auto;
  margin-right:auto;
  border-width: 0;
  border-color: transparent;
  caption-side: top;
}
.tabwid-caption-bottom table{
  caption-side: bottom;
}
.tabwid_left table{
  margin-left:0;
}
.tabwid_right table{
  margin-right:0;
}
.tabwid td, .tabwid th {
    padding: 0;
}
.tabwid a {
  text-decoration: none;
}
.tabwid thead {
    background-color: transparent;
}
.tabwid tfoot {
    background-color: transparent;
}
.tabwid table tr {
background-color: transparent;
}
.katex-display {
    margin: 0 0 !important;
}</style><table data-quarto-disable-processing='true' class='cl-eff7c910'><thead><tr style="overflow-wrap:break-word;"><th class="cl-efefb2c0"><p class="cl-efef64a0"><span class="cl-efe92306">siteid</span></p></th><th class="cl-efefb2c0"><p class="cl-efef64a0"><span class="cl-efe92306">waist_gp</span></p></th><th class="cl-efefb2c0"><p class="cl-efef64a0"><span class="cl-efe92306">age_ge60</span></p></th><th class="cl-efefb2c0"><p class="cl-efef64a0"><span class="cl-efe92306">dm_hist</span></p></th><th class="cl-efefb2c0"><p class="cl-efef64a0"><span class="cl-efe92306">ssri_base</span></p></th><th class="cl-efefb2c0"><p class="cl-efef64a0"><span class="cl-efe92306">t_gp</span></p></th><th class="cl-efefb2c0"><p class="cl-efef64a0"><span class="cl-efe92306">smoker</span></p></th><th class="cl-efefb2c1"><p class="cl-efef64a1"><span class="cl-efe92306">ogtt_gt11</span></p></th><th class="cl-efefb2c1"><p class="cl-efef64a1"><span class="cl-efe92306">ogtt_base</span></p></th><th class="cl-efefb2c1"><p class="cl-efef64a1"><span class="cl-efe92306">treat</span></p></th><th class="cl-efefb2c1"><p class="cl-efef64a1"><span class="cl-efe92306">bmleanms_base</span></p></th><th class="cl-efefb2c1"><p class="cl-efef64a1"><span class="cl-efe92306">leanmass_change</span></p></th><th class="cl-efefb2c1"><p class="cl-efef64a1"><span class="cl-efe92306">bmfatms_base</span></p></th><th class="cl-efefb2c1"><p class="cl-efef64a1"><span class="cl-efe92306">fatmass_change</span></p></th><th class="cl-efefb2c1"><p class="cl-efef64a1"><span class="cl-efe92306">bmapfm_base</span></p></th><th class="cl-efefb2c1"><p class="cl-efef64a1"><span class="cl-efe92306">abdomfat_change</span></p></th><th class="cl-efefb2c1"><p class="cl-efef64a1"><span class="cl-efe92306">grip_base</span></p></th><th class="cl-efefb2c1"><p class="cl-efef64a1"><span class="cl-efe92306">grip_change</span></p></th><th class="cl-efefb2c1"><p class="cl-efef64a1"><span class="cl-efe92306">e2_base</span></p></th><th class="cl-efefb2c1"><p class="cl-efef64a1"><span class="cl-efe92306">e2_change</span></p></th><th class="cl-efefb2c1"><p class="cl-efef64a1"><span class="cl-efe92306">shbg_base</span></p></th><th class="cl-efefb2c1"><p class="cl-efef64a1"><span class="cl-efe92306">shbg_change</span></p></th><th class="cl-efefb2c1"><p class="cl-efef64a1"><span class="cl-efe92306">change_ogtt</span></p></th><th class="cl-efefb2c0"><p class="cl-efef64a0"><span class="cl-efe92306">treatment</span></p></th></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-efefb2c2"><p class="cl-efef64a0"><span class="cl-efe92306">61219</span></p></td><td class="cl-efefb2c2"><p class="cl-efef64a0"><span class="cl-efe92306">3</span></p></td><td class="cl-efefb2c2"><p class="cl-efef64a0"><span class="cl-efe92306">1</span></p></td><td class="cl-efefb2c2"><p class="cl-efef64a0"><span class="cl-efe92306">0</span></p></td><td class="cl-efefb2c2"><p class="cl-efef64a0"><span class="cl-efe92306">0</span></p></td><td class="cl-efefb2c2"><p class="cl-efef64a0"><span class="cl-efe92306">3</span></p></td><td class="cl-efefb2c2"><p class="cl-efef64a0"><span class="cl-efe92306">0</span></p></td><td class="cl-efefb2c3"><p class="cl-efef64a1"><span class="cl-efe92306">0</span></p></td><td class="cl-efefb2c3"><p class="cl-efef64a1"><span class="cl-efe92306">8.8</span></p></td><td class="cl-efefb2c3"><p class="cl-efef64a1"><span class="cl-efe92306">1</span></p></td><td class="cl-efefb2c3"><p class="cl-efef64a1"><span class="cl-efe92306">61,760</span></p></td><td class="cl-efefb2c3"><p class="cl-efef64a1"><span class="cl-efe92306">0.992</span></p></td><td class="cl-efefb2c3"><p class="cl-efef64a1"><span class="cl-efe92306">36,217</span></p></td><td class="cl-efefb2c3"><p class="cl-efef64a1"><span class="cl-efe92306">-3.181</span></p></td><td class="cl-efefb2c3"><p class="cl-efef64a1"><span class="cl-efe92306">46.0</span></p></td><td class="cl-efefb2c3"><p class="cl-efef64a1"><span class="cl-efe92306">-2.3</span></p></td><td class="cl-efefb2c3"><p class="cl-efef64a1"><span class="cl-efe92306">39</span></p></td><td class="cl-efefb2c3"><p class="cl-efef64a1"><span class="cl-efe92306">-1</span></p></td><td class="cl-efefb2c3"><p class="cl-efef64a1"><span class="cl-efe92306">210.864</span></p></td><td class="cl-efefb2c3"><p class="cl-efef64a1"><span class="cl-efe92306">49.312</span></p></td><td class="cl-efefb2c3"><p class="cl-efef64a1"><span class="cl-efe92306">57.72</span></p></td><td class="cl-efefb2c3"><p class="cl-efef64a1"><span class="cl-efe92306">5.62</span></p></td><td class="cl-efefb2c3"><p class="cl-efef64a1"><span class="cl-efe92306">-1.2</span></p></td><td class="cl-efefb2c2"><p class="cl-efef64a0"><span class="cl-efe92306">Testosterone</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-efefb2c4"><p class="cl-efef64a0"><span class="cl-efe92306">61219</span></p></td><td class="cl-efefb2c4"><p class="cl-efef64a0"><span class="cl-efe92306">3</span></p></td><td class="cl-efefb2c4"><p class="cl-efef64a0"><span class="cl-efe92306">1</span></p></td><td class="cl-efefb2c4"><p class="cl-efef64a0"><span class="cl-efe92306">0</span></p></td><td class="cl-efefb2c4"><p class="cl-efef64a0"><span class="cl-efe92306">0</span></p></td><td class="cl-efefb2c4"><p class="cl-efef64a0"><span class="cl-efe92306">3</span></p></td><td class="cl-efefb2c4"><p class="cl-efef64a0"><span class="cl-efe92306">0</span></p></td><td class="cl-efefb2c5"><p class="cl-efef64a1"><span class="cl-efe92306">0</span></p></td><td class="cl-efefb2c5"><p class="cl-efef64a1"><span class="cl-efe92306">10.8</span></p></td><td class="cl-efefb2c5"><p class="cl-efef64a1"><span class="cl-efe92306">0</span></p></td><td class="cl-efefb2c5"><p class="cl-efef64a1"><span class="cl-efe92306">69,431</span></p></td><td class="cl-efefb2c5"><p class="cl-efef64a1"><span class="cl-efe92306">1.926</span></p></td><td class="cl-efefb2c5"><p class="cl-efef64a1"><span class="cl-efe92306">44,372</span></p></td><td class="cl-efefb2c5"><p class="cl-efef64a1"><span class="cl-efe92306">-2.434</span></p></td><td class="cl-efefb2c5"><p class="cl-efef64a1"><span class="cl-efe92306">47.5</span></p></td><td class="cl-efefb2c5"><p class="cl-efef64a1"><span class="cl-efe92306">-2.9</span></p></td><td class="cl-efefb2c5"><p class="cl-efef64a1"><span class="cl-efe92306">45</span></p></td><td class="cl-efefb2c5"><p class="cl-efef64a1"><span class="cl-efe92306">0</span></p></td><td class="cl-efefb2c5"><p class="cl-efef64a1"><span class="cl-efe92306">249.136</span></p></td><td class="cl-efefb2c5"><p class="cl-efef64a1"><span class="cl-efe92306">68.080</span></p></td><td class="cl-efefb2c5"><p class="cl-efef64a1"><span class="cl-efe92306">46.25</span></p></td><td class="cl-efefb2c5"><p class="cl-efef64a1"><span class="cl-efe92306">-2.81</span></p></td><td class="cl-efefb2c5"><p class="cl-efef64a1"><span class="cl-efe92306">-1.6</span></p></td><td class="cl-efefb2c4"><p class="cl-efef64a0"><span class="cl-efe92306">Placebo</span></p></td></tr></tbody></table></div>

# Figure 2: unadjusted and adjusted analyses


```r
unadj.or<- glm(ogtt_gt11 ~treatment, data=df,
             family = binomial(link="logit")) %>%
  tbl_regression(exponentiate = TRUE, show_single_row="treatment") 

# run each of the categorical models
adj.risk.or <-glm(ogtt_gt11 ~ogtt_base+treatment+ 
                    siteid+ waist_gp+  age_ge60+dm_hist+ ssri_base+t_gp+smoker,
                  data=df,
                  family = binomial(link="logit"), 
                  control=list(maxit=10000)) %>%
  tbl_regression(exponentiate = TRUE,show_single_row="treatment",
                 include = c("treatment"))


adj.allbase.or <-glm(ogtt_gt11 ~ogtt_base+treatment+ 
                       #baseline mediators
                       bmleanms_base+ 
                       bmfatms_base+ 
                       bmapfm_base+  
                       grip_base+ e2_base+shbg_base+ 
                       siteid+ waist_gp+  age_ge60+dm_hist+ ssri_base+t_gp+smoker,
                     data=df,
                     family = binomial(link="logit")) %>%
  tbl_regression(exponentiate = TRUE,
                 show_single_row="treatment",
                 include = c("treatment"))

adj.full.or <-glm(ogtt_gt11 ~ogtt_base+treatment+ 
                    #baseline + change covariates
                    bmleanms_base+leanmass_change+ 
                    bmfatms_base+ fatmass_change+
                    bmapfm_base+ abdomfat_change+ 
                    grip_base+ grip_change+
                    e2_base+e2_change+
                    shbg_base+ shbg_change+
                    siteid+ waist_gp+  age_ge60+dm_hist+ ssri_base+t_gp+smoker,
                  data=df,
                  family = binomial(link="logit")) %>%
  tbl_regression(exponentiate = TRUE,
                 show_single_row="treatment",
                 include = c("treatment"))

# stick them together 
models.a<-tbl_stack(list(unadj.or, 
                         adj.risk.or, 
                         adj.allbase.or,
                         adj.full.or))

# plot A
models.a$table_body %>%
  mutate(name = case_when(tbl_id1==1 ~ "Unadjusted", 
                          tbl_id1==2 ~ "Adjusted for baseline risk factors" , 
                          tbl_id1==3 ~ "Adjusted for all baseline covariates*" ,
                          tbl_id1==4 ~ "Mediation adjusted") %>%
           as.factor()) %>%
  ggplot(aes(y=fct_reorder(name, -tbl_id1), x=estimate)) +
  geom_point(size=3)+
  geom_errorbar(aes(xmax=conf.high, xmin=conf.low), size=0.5, width=0.1) +
  labs(x="Odds ratio (95% CI)", y="")+
  geom_vline(aes(xintercept= 1), linetype="dotted")+
  coord_trans(x = scales:::log_trans(base = exp(1))) +
  theme_minimal() -> plot.a

# run each of the continuous models
unadj.lm<- glm(change_ogtt ~treatment, data=df) %>%
  tbl_regression(show_single_row="treatment") 

adj.risk.lm <-glm(change_ogtt ~ogtt_base+treatment+ 
                    siteid+ waist_gp+  age_ge60+dm_hist+ ssri_base+t_gp+smoker,
                  data=df) %>%
  tbl_regression(show_single_row="treatment",
                 include = c("treatment"))


adj.allbase.lm <-glm(change_ogtt ~ogtt_base+treatment+ 
                       bmleanms_base+ 
                       bmfatms_base+ 
                       bmapfm_base+  
                       grip_base+ e2_base+shbg_base+ 
                       siteid+ waist_gp+  age_ge60+dm_hist+ ssri_base+t_gp+smoker,
                     data=df) %>%
  tbl_regression(show_single_row="treatment",
                 include = c("treatment"))


adj.full.lm <-glm(change_ogtt ~ogtt_base+treatment+ 
                    bmleanms_base+leanmass_change+  
                    bmfatms_base+ fatmass_change+
                    bmapfm_base+  abdomfat_change+ e2_base+e2_change+shbg_base+ shbg_change+
                    grip_base+ grip_change+
                    siteid+ waist_gp+  age_ge60+dm_hist+ ssri_base+t_gp+smoker,
                  data=df) %>%
  tbl_regression(show_single_row="treatment",
                 include = c("treatment"))

# stick them together

models.b<-tbl_stack(list(unadj.lm, 
                         adj.risk.lm, 
                         adj.allbase.lm,
                         adj.full.lm))

# plot B
models.b$table_body %>%
  mutate(name = case_when(tbl_id1==1 ~ "Unadjusted", 
                          tbl_id1==2 ~ "Adjusted for baseline risk factors" , 
                          tbl_id1==3 ~ "Adjusted for all baseline covariates*" ,
                          tbl_id1==4 ~ "Mediation adjusted") %>%
           as.factor()) %>%
  ggplot(aes(y=fct_reorder(name, -tbl_id1), x=estimate)) +
  geom_point(size=3)+
  geom_errorbar(aes(xmax=conf.high, xmin=conf.low), size=0.5, width=0.1) +
  labs(x="Change in OGTT (95% CI)", y="")+
  theme(axis.ticks.y = element_blank())+
  geom_vline(aes(xintercept= 0), linetype="dotted") +
  theme_minimal()+
  scale_y_discrete(labels=c("Unadjusted" = "",
                            "Adjusted for baseline risk factors" = "",
                            "Adjusted for all baseline covariates*"="",
                            "Mediation adjusted"="")) -> plot.b

# stick the two plots together
plot_grid(plot.a, plot.b, 
          labels="AUTO", 
          label_x = c(0.4,0),
          rel_widths = c(1.1,0.6))
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)


# Mediation analyses

Performed using `medflex` package:

 Johan Steen, Tom Loeys, Beatrijs Moerkerke, Stijn Vansteelandt (2017). medflex: An R
  Package for Flexible Mediation Analysis using Natural Effect Models. Journal of
  Statistical Software, 76(11), 1-46. doi:10.18637/jss.v076.i11

For the underlying methods please cite the following papers:

  Lange, T., Vansteelandt, S., & Bekaert, M. (2012). A Simple Unified Approach for
  Estimating Natural Direct and Indirect Effects. American Journal of Epidemiology,
  176(3), 190-195.

  Vansteelandt, S., Bekaert, M., & Lange, T. (2012). Imputation Strategies for the
  Estimation of Natural Direct and Indirect Effects. Epidemiologic Methods, 1(1),
  Article 7.

  Loeys, T., Moerkerke, B., De Smet, O., Buysse, A., Steen, J., & Vansteelandt, S.
  (2013). Flexible Mediation Analysis in the Presence of Nonlinear Relations: Beyond the
  Mediation Formula. Multivariate Behavioral Research, 48(6), 871-894.


## Continuous outcome


```r
# continuous outcome model
expData<-neImpute(change_ogtt  ~ factor(treat)+
                    #mediators
                    fatmass_change+leanmass_change+  
                    abdomfat_change+ grip_change+e2_change+ shbg_change+
                    #adjustments
                    ogtt_base+ 
                    bmleanms_base+ bmfatms_base  + 
                    bmapfm_base+  grip_base+ e2_base+shbg_base+ 
                    siteid+ waist_gp+  age_ge60+dm_hist+ ssri_base+t_gp+smoker,
                  nMed = 6,
                  data=df)

neMod.extra.cont<-neModel(change_ogtt ~ treat0+treat1+
                            ogtt_base+ 
                            bmleanms_base + bmfatms_base+  
                            bmapfm_base+ grip_base+ e2_base+shbg_base+ 
                            siteid+ waist_gp+  age_ge60+dm_hist+ ssri_base+t_gp+smoker, 
                          family = gaussian(link = "identity"),
                          se="robust",
                          expData=expData)

cont.extra<-data.frame(est = neMod.extra.cont$neModelFit$coefficients, confint(neMod.extra.cont))

lht <- neLht(neMod.extra.cont, linfct = c("treat01 = 0", 
                                          "treat11  = 0", 
                                          "treat01 + treat11  = 0"))

t<-summary(lht)

cont.extra<-data.frame(est = t$coefficients[,1], confint(lht))

#pte.direct.cont<-(t$coefficients[1,1]/t$coefficients[3,1])*100
pte.indirect.cont<-(t$coefficients[2,1]/t$coefficients[3,1])*100
```



## Categorical outcome


```r
#catgeorical outcome model
expData2<-neImpute(ogtt_gt11 ~factor(treat)+
                     #mediators
                     fatmass_change+leanmass_change+  
                     abdomfat_change+ grip_change+e2_change+ shbg_change+ 
                     #adjustments
                     ogtt_base+ 
                     bmleanms_base+ bmfatms_base  + 
                     bmapfm_base+  grip_base+ e2_base+shbg_base+ 
                     siteid+ waist_gp+  age_ge60+dm_hist+ ssri_base+t_gp+smoker,
                   family = binomial(link="logit"),
                   nMed = 6,
                   data=df)

neMod.extra.cat<-neModel(ogtt_gt11 ~ treat0+treat1+
                           ogtt_base+
                           bmleanms_base + bmfatms_base+  
                           bmapfm_base+  grip_base+ e2_base+shbg_base+ 
                           siteid+ waist_gp+  age_ge60+dm_hist+ ssri_base+t_gp+smoker,
                         family = binomial(link = "logit"),
                         control=list(maxit=10000),
                         se="robust",
                         expData=expData2)

cat.extra<-data.frame(OR = exp(neMod.extra.cat$neModelFit$coefficients),
                      exp(confint(neMod.extra.cat)))

lht <- neLht(neMod.extra.cat, linfct = c("treat01 = 0", 
                                         "treat11  = 0", 
                                         "treat01 + treat11  = 0"))

t<-summary(lht)

cat.extra<-data.frame(est = exp(t$coefficients[,1]), exp(confint(lht)))

#pte.direct.cat<-(t$coefficients[1,1]/t$coefficients[3,1])*100
pte.indirect.cat<-(t$coefficients[2,1]/t$coefficients[3,1])*100
```

## Figure 3


```r
#using the mediation models run up above in the results chunk
cat.extra %>%
  mutate(name = c("Direct effect", "Indirect effect", "Total effect")) %>%
  ggplot(aes(y=fct_rev(name), x=est)) +
  geom_point(size=3)+
  geom_errorbar(aes(xmax=X95..UCL, xmin=X95..LCL), size=0.5, width=0.1) +
  labs(x="Odds ratio (95% CI)", y="")+
  geom_vline(aes(xintercept= 1), linetype="dotted")+
  coord_trans(x = scales:::log_trans(base = exp(1))) +
  theme_minimal() -> plot.a2

cont.extra %>%
  mutate(name = c("Direct effect", "Indirect effect", "Total effect")) %>%
  ggplot(aes(y=fct_rev(name), x=est)) +
  geom_point(size=3)+
  geom_errorbar(aes(xmax=X95..UCL, xmin=X95..LCL), size=0.5, width=0.1) +
  labs(x="Change in OGTT (95% CI)", y="")+
  theme(axis.ticks.y = element_blank())+
  geom_vline(aes(xintercept= 0), linetype="dotted") +
  theme_minimal()+
  scale_y_discrete(labels=c("Direct effect" = "",
                            "Indirect effect" = "",
                            "Total effect"=""))  -> plot.b2

# stick together
plot_grid(plot.a2, plot.b2, 
          labels="AUTO", 
          label_x = c(0.2,0.05),
          rel_widths = c(1.1,0.8))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

# Table 1:  Treatment mediation: estimates of effects from models of 2-hour glucose ≥ 11.1mmol/L and change in OGTT at two years


```r
allcat <- glm(ogtt_gt11 ~ogtt_base+treatment+ 
                bmleanms_base+leanmass_change+ 
                bmfatms_base+ fatmass_change+
                bmapfm_base+ abdomfat_change+ 
                grip_base+ grip_change+
                e2_base+e2_change+shbg_base+ shbg_change+
                siteid+ waist_gp+  age_ge60+dm_hist+ ssri_base+t_gp+smoker,
              data=df,
              family = binomial(link="logit")) %>%
  tbl_regression(exponentiate = TRUE,
                 show_single_row="treatment", 
                 include = c("treatment", contains("change"))) 

allcont<-glm(change_ogtt ~ogtt_base+treatment+ 
               bmleanms_base+leanmass_change+ 
               bmfatms_base+ fatmass_change+
               bmapfm_base+ abdomfat_change+ 
               grip_base+ grip_change+
               e2_base+e2_change+shbg_base+ shbg_change+
               siteid+ waist_gp+  age_ge60+dm_hist+ ssri_base+t_gp+smoker, 
             data=df) %>%
  tbl_regression(show_single_row="treatment", 
                 include = c(treatment, contains("change"))) 

tbl_merge(list(allcat, allcont), 
          tab_spanner = c("2hr glucose \U2265 11.1", "2hr glucose change from baseline"))  %>%
  modify_header(estimate_2 = "**Mean change**") %>%
  modify_footnote(c(estimate_1, estimate_2) ~ 
                    "Models are adjusted for all baseline covariates (baseline risk factors and baseline mediators)") %>%
  modify_footnote(c(estimate_2) ~ 
                    "Change is calculated as two years minus baseline, with positive values indicating increases from baseline and negative as decreases from baseline. ") %>%
  as_flex_table()
```

<div class="tabwid"><style>.cl-ff3c3bc2{}.cl-ff327574{font-family:'Arial';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-ff327575{font-family:'Arial';font-size:11pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-ff327576{font-family:'Arial';font-size:6.6pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;position: relative;bottom:3.3pt;}.cl-ff361ef4{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:2pt;padding-top:2pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-ff361ef5{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:2pt;padding-top:2pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-ff361ef6{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-ff361ef7{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-ff361ef8{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-ff364d02{width:3.156in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ff364d03{width:0.583in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ff364d04{width:0.965in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ff364d05{width:0.82in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ff364d06{width:1.253in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ff364d07{width:1.016in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ff364d08{width:3.156in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ff364d09{width:0.583in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ff364d0a{width:0.965in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ff364d0b{width:0.82in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ff364d0c{width:1.253in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ff36741c{width:1.016in;background-color:transparent;vertical-align: middle;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 1pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ff36741d{width:3.156in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ff36741e{width:0.583in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ff36741f{width:0.965in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ff367420{width:0.82in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ff367421{width:1.253in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ff367422{width:1.016in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ff367423{width:3.156in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ff367424{width:0.583in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ff367425{width:0.965in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ff367426{width:0.82in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ff369b2c{width:1.253in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ff369b2d{width:1.016in;background-color:transparent;vertical-align: top;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ff369b2e{width:3.156in;background-color:transparent;vertical-align: top;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ff369b2f{width:0.583in;background-color:transparent;vertical-align: top;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ff369b30{width:0.965in;background-color:transparent;vertical-align: top;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ff369b31{width:0.82in;background-color:transparent;vertical-align: top;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ff369b32{width:1.253in;background-color:transparent;vertical-align: top;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ff369b33{width:1.016in;background-color:transparent;vertical-align: top;border-bottom: 1pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ff369b34{width:3.156in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ff369b35{width:0.583in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ff369b36{width:0.965in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ff36c23c{width:0.82in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ff36c23d{width:1.253in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-ff36c23e{width:1.016in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(255, 255, 255, 0.00);border-top: 0 solid rgba(255, 255, 255, 0.00);border-left: 0 solid rgba(255, 255, 255, 0.00);border-right: 0 solid rgba(255, 255, 255, 0.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.tabwid {
  font-size: initial;
  padding-bottom: 1em;
}

.tabwid table{
  border-spacing:0px !important;
  border-collapse:collapse;
  line-height:1;
  margin-left:auto;
  margin-right:auto;
  border-width: 0;
  border-color: transparent;
  caption-side: top;
}
.tabwid-caption-bottom table{
  caption-side: bottom;
}
.tabwid_left table{
  margin-left:0;
}
.tabwid_right table{
  margin-right:0;
}
.tabwid td, .tabwid th {
    padding: 0;
}
.tabwid a {
  text-decoration: none;
}
.tabwid thead {
    background-color: transparent;
}
.tabwid tfoot {
    background-color: transparent;
}
.tabwid table tr {
background-color: transparent;
}
.katex-display {
    margin: 0 0 !important;
}</style><table data-quarto-disable-processing='true' class='cl-ff3c3bc2'><thead><tr style="overflow-wrap:break-word;"><th class="cl-ff364d02"><p class="cl-ff361ef4"><span class="cl-ff327574"> </span></p></th><th  colspan="3"class="cl-ff364d03"><p class="cl-ff361ef5"><span class="cl-ff327574">2hr glucose ≥ 11.1</span></p></th><th  colspan="3"class="cl-ff364d06"><p class="cl-ff361ef5"><span class="cl-ff327574">2hr glucose change from baseline</span></p></th></tr><tr style="overflow-wrap:break-word;"><th class="cl-ff364d08"><p class="cl-ff361ef4"><span class="cl-ff327575">Characteristic</span></p></th><th class="cl-ff364d09"><p class="cl-ff361ef5"><span class="cl-ff327575">OR</span><span class="cl-ff327576">1</span><span class="cl-ff327576">2</span></p></th><th class="cl-ff364d0a"><p class="cl-ff361ef5"><span class="cl-ff327575">95% CI</span><span class="cl-ff327576">2</span></p></th><th class="cl-ff364d0b"><p class="cl-ff361ef5"><span class="cl-ff327575">p-value</span></p></th><th class="cl-ff364d0c"><p class="cl-ff361ef5"><span class="cl-ff327575">Mean change</span><span class="cl-ff327576">3</span></p></th><th class="cl-ff36741c"><p class="cl-ff361ef5"><span class="cl-ff327575">95% CI</span><span class="cl-ff327576">2</span></p></th><th class="cl-ff364d0b"><p class="cl-ff361ef5"><span class="cl-ff327575">p-value</span></p></th></tr></thead><tbody><tr style="overflow-wrap:break-word;"><td class="cl-ff36741d"><p class="cl-ff361ef6"><span class="cl-ff327574">Treatment with testosterone</span></p></td><td class="cl-ff36741e"><p class="cl-ff361ef7"><span class="cl-ff327574">0.75</span></p></td><td class="cl-ff36741f"><p class="cl-ff361ef7"><span class="cl-ff327574">0.42, 1.36</span></p></td><td class="cl-ff367420"><p class="cl-ff361ef7"><span class="cl-ff327574">0.4</span></p></td><td class="cl-ff367421"><p class="cl-ff361ef7"><span class="cl-ff327574">-0.26</span></p></td><td class="cl-ff367422"><p class="cl-ff361ef7"><span class="cl-ff327574">-0.68, 0.16</span></p></td><td class="cl-ff367420"><p class="cl-ff361ef7"><span class="cl-ff327574">0.2</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ff367423"><p class="cl-ff361ef6"><span class="cl-ff327574">Change in skeletal muscle mass (kg)</span></p></td><td class="cl-ff367424"><p class="cl-ff361ef7"><span class="cl-ff327574">1.01</span></p></td><td class="cl-ff367425"><p class="cl-ff361ef7"><span class="cl-ff327574">0.91, 1.13</span></p></td><td class="cl-ff367426"><p class="cl-ff361ef7"><span class="cl-ff327574">0.8</span></p></td><td class="cl-ff369b2c"><p class="cl-ff361ef7"><span class="cl-ff327574">0.04</span></p></td><td class="cl-ff369b2d"><p class="cl-ff361ef7"><span class="cl-ff327574">-0.04, 0.11</span></p></td><td class="cl-ff367426"><p class="cl-ff361ef7"><span class="cl-ff327574">0.3</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ff367423"><p class="cl-ff361ef6"><span class="cl-ff327574">Change in Fat mass (kg)</span></p></td><td class="cl-ff367424"><p class="cl-ff361ef7"><span class="cl-ff327574">1.23</span></p></td><td class="cl-ff367425"><p class="cl-ff361ef7"><span class="cl-ff327574">1.09, 1.39</span></p></td><td class="cl-ff367426"><p class="cl-ff361ef7"><span class="cl-ff327574">&lt;0.001</span></p></td><td class="cl-ff369b2c"><p class="cl-ff361ef7"><span class="cl-ff327574">0.20</span></p></td><td class="cl-ff369b2d"><p class="cl-ff361ef7"><span class="cl-ff327574">0.12, 0.28</span></p></td><td class="cl-ff367426"><p class="cl-ff361ef7"><span class="cl-ff327574">&lt;0.001</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ff367423"><p class="cl-ff361ef6"><span class="cl-ff327574">Change in Abdominal fat (%)</span></p></td><td class="cl-ff367424"><p class="cl-ff361ef7"><span class="cl-ff327574">1.02</span></p></td><td class="cl-ff367425"><p class="cl-ff361ef7"><span class="cl-ff327574">0.90, 1.15</span></p></td><td class="cl-ff367426"><p class="cl-ff361ef7"><span class="cl-ff327574">0.8</span></p></td><td class="cl-ff369b2c"><p class="cl-ff361ef7"><span class="cl-ff327574">0.00</span></p></td><td class="cl-ff369b2d"><p class="cl-ff361ef7"><span class="cl-ff327574">-0.09, 0.08</span></p></td><td class="cl-ff367426"><p class="cl-ff361ef7"><span class="cl-ff327574">&gt;0.9</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ff367423"><p class="cl-ff361ef6"><span class="cl-ff327574">Change in non-dominant grip strength (kg)</span></p></td><td class="cl-ff367424"><p class="cl-ff361ef7"><span class="cl-ff327574">1.01</span></p></td><td class="cl-ff367425"><p class="cl-ff361ef7"><span class="cl-ff327574">0.97, 1.06</span></p></td><td class="cl-ff367426"><p class="cl-ff361ef7"><span class="cl-ff327574">0.5</span></p></td><td class="cl-ff369b2c"><p class="cl-ff361ef7"><span class="cl-ff327574">0.00</span></p></td><td class="cl-ff369b2d"><p class="cl-ff361ef7"><span class="cl-ff327574">-0.03, 0.03</span></p></td><td class="cl-ff367426"><p class="cl-ff361ef7"><span class="cl-ff327574">0.8</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ff367423"><p class="cl-ff361ef6"><span class="cl-ff327574">Change in E2</span></p></td><td class="cl-ff367424"><p class="cl-ff361ef7"><span class="cl-ff327574">1.00</span></p></td><td class="cl-ff367425"><p class="cl-ff361ef7"><span class="cl-ff327574">1.00, 1.00</span></p></td><td class="cl-ff367426"><p class="cl-ff361ef7"><span class="cl-ff327574">0.066</span></p></td><td class="cl-ff369b2c"><p class="cl-ff361ef7"><span class="cl-ff327574">0.00</span></p></td><td class="cl-ff369b2d"><p class="cl-ff361ef7"><span class="cl-ff327574">0.00, 0.00</span></p></td><td class="cl-ff367426"><p class="cl-ff361ef7"><span class="cl-ff327574">0.2</span></p></td></tr><tr style="overflow-wrap:break-word;"><td class="cl-ff369b2e"><p class="cl-ff361ef6"><span class="cl-ff327574">Change in SHBG</span></p></td><td class="cl-ff369b2f"><p class="cl-ff361ef7"><span class="cl-ff327574">1.00</span></p></td><td class="cl-ff369b30"><p class="cl-ff361ef7"><span class="cl-ff327574">0.97, 1.03</span></p></td><td class="cl-ff369b31"><p class="cl-ff361ef7"><span class="cl-ff327574">0.8</span></p></td><td class="cl-ff369b32"><p class="cl-ff361ef7"><span class="cl-ff327574">0.02</span></p></td><td class="cl-ff369b33"><p class="cl-ff361ef7"><span class="cl-ff327574">-0.01, 0.04</span></p></td><td class="cl-ff369b31"><p class="cl-ff361ef7"><span class="cl-ff327574">0.14</span></p></td></tr></tbody><tfoot><tr style="overflow-wrap:break-word;"><td  colspan="7"class="cl-ff369b34"><p class="cl-ff361ef8"><span class="cl-ff327576">1</span><span class="cl-ff327574">Models are adjusted for all baseline covariates (baseline risk factors and baseline mediators)</span></p></td></tr><tr style="overflow-wrap:break-word;"><td  colspan="7"class="cl-ff369b34"><p class="cl-ff361ef8"><span class="cl-ff327576">2</span><span class="cl-ff327574">OR = Odds Ratio, CI = Confidence Interval</span></p></td></tr><tr style="overflow-wrap:break-word;"><td  colspan="7"class="cl-ff369b34"><p class="cl-ff361ef8"><span class="cl-ff327576">3</span><span class="cl-ff327574">Change is calculated as two years minus baseline, with positive values indicating increases from baseline and negative as decreases from baseline. </span></p></td></tr></tfoot></table></div>

