# Social and configural effects on the cognitive dynamics of perspective-taking
Alexia Galati, Rick Dale, & Nick Duran  
4/15/2017  





## Preliminaries for Exp 2

Here we are loading in prior data analyzed using the scripts "GDD2_batchTrajectoryAnalysis.R". 
These include data from both the "error" (control) trials and the "ambiguous" (critical) trials of the experiment. 

As with Exp 1A and 1B, we trim the data by removing trials that took more than 6000 ms from the initiation of the trial to selection of an initial folder or over 1000 pixels of total distance. 


In Exp 2, the configural cue (the alignment of the configuration) is manipulated within participants.



```r
load('GDD2_churnedRawTrajectoryData.Rd') 

### Data cleaning and trimming ###

# Clean by approx. 3 SDs of M for control trials
resAllError = resAllError[resAllError$RTDV<6000&resAllError$totalDistanceDV<1000,]
resAll = resAll[resAll$RTDV<6000&resAll$totalDistanceDV<1000,]


# Let's reverse all true/false for front/back
resAllError[resAllError$sound %in% c('fron','back'), ]$err = !resAllError[resAllError$sound %in% c('fron','back'), ]$err
```

Given the pattern of performance on sagittal (front-back) trials (with very high errors on control trials), we reverse their assignment in the data. In Exp 2, listeners appear to be interpreting front-back terms according to the opposite mapping that we had assigned. See manuscript for further explanation.



```r
# let's reverse front and back (since their interpretation is now the reverse of what we thought)
resAll$ego2 = 0
resAll[resAll$sound %in% c('fron','back'),]$ego2 = resAll[resAll$sound %in% c('fron','back'),]$other # new ego value with the the other one
resAll[resAll$sound %in% c('fron','back'),]$other = resAll[resAll$sound %in% c('fron','back'),]$ego
resAll[resAll$sound %in% c('fron','back'),]$ego = resAll[resAll$sound %in% c('fron','back'),]$ego2
```

As with the previous Experiments, we plot a histogram for the proportion of egocentric responses across participants. 

Again, participants are classified as egocentric, other-centric, and mixed responders based on their proportion of egocentric responses on ambiguous/critical trials, using the same criteria as Exp 1A and 1B. 



```r
# Create an aggregate variable to see distribution of egocentrism across subjects
#resAll$egoChosen = 1*(resAll$chosen==resAll$ego)
egoChosen = 1*(resAll$chosen==resAll$ego)
perspectiveDistribution = aggregate(egoChosen~resAll$fl,FUN=mean)
hist(perspectiveDistribution$egoChosen,100, main = paste("Histogram of", "proportion of egocentrism"), 
     xlab='Proportion of egocentrism',ylab='Number of subjects', ylim = range(0:60))
```

![](GDD2_Code_LMERs_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
# Constructing perspective preference variables...
egoSubjects = perspectiveDistribution[perspectiveDistribution$egoChosen>.7,]$resAll
otherSubjects = perspectiveDistribution[perspectiveDistribution$egoChosen<.3,]$resAll
mixedSubjects = perspectiveDistribution[(perspectiveDistribution$egoChosen>=.3 & perspectiveDistribution$egoChosen<=.7) ,]$resAll

length(egoSubjects)
```

```
## [1] 34
```

```r
length(otherSubjects)
```

```
## [1] 129
```

```r
length(mixedSubjects)
```

```
## [1] 21
```

```r
#Label perspectivePreference levels
resAll$perspectivePreference = 'mixed'
resAll$perspectivePreference[resAll$fl %in% egoSubjects]='ego'
resAll$perspectivePreference[resAll$fl %in% otherSubjects]='other'

#for Control trials let's trasfer the perspectivePreference variable over 
resAllError$perspectivePreference = 'mixed'
resAllError$perspectivePreference[resAllError$fl %in% egoSubjects]='ego'
resAllError$perspectivePreference[resAllError$fl %in% otherSubjects]='other'
```

## Preliminaries: Variable recoding 

As with Exp 1A and 1B, we create an "axis" variable, for which we combine "left-right" instructions to refer to the "lateral" axis, and "front-back" instructions to refer to the "sagittal" axis. Again, we create an "offset" variable, for which we assign "90"" degree offset to speaker positions 90 and 270. For ambiguous trials this level contrasts with the "180" level of offset; for control trials, with the "0" offset.

Control trials also include "ego-reversed" and "partner-reversed" configurations (where the apex of the triangle is pointing toward the ego or partner, respectively); there are 2 trials for each of these types. We combine these with the "ego" and "partner" categories, respectively.


```r
#Let's create axis variable to compress front-back and left-right instuctions into a sagittal and lateral axis
resAll$axis = 'sagittal'
resAll$axis[resAll$sound %in% c('righ','left')]='lateral'

#Let's compress 90 and 270 speaker positions into a 90 offset
resAll$offset = '180'
resAll$offset[resAll$ppos %in% c('init_90','init_270')]='90'

#Create egocentric choice variable
resAll$egoChoice = 1*(resAll$chosen==resAll$ego)

#let's create the axis variable for control trials
resAllError$axis = 'sagittal'
resAllError$axis[resAllError$sound %in% c('righ','left')]='lateral'

#Let's compress 90 and 270 speaker positions into a 90 offset
resAllError$offset = '0'
resAllError$offset[resAllError$ppos %in% c('init_90','init_270')]='90'

#compress ego and ego-reversed configurations into a single category
#compress other and other-reversed configurations into a single category
resAllError$config[resAllError$config %in% c('ego', 'ego-reversed')]='ego'
resAllError$config[resAllError$config %in% c('partner', 'partner-reversed')]='partner'
resAllError$config = as.factor(as.matrix(resAllError$config))
#str(resAllError) #confirm 4 levels of config factor
```

## Preliminaries: Create divergence from optimal distance variable

In contrast to Exp 1A and 1B, instead of using total distance in pixels, we computed the divergence from the optimal distance to the selected folder. In the GDD2_batchTrajectoryAnalysis.R code, we computed the optimal distance to be the distance in pixels from the trial initiation to their object selection. This straight line reflects an optimal distance because it is the minimal possible distance needed to reach that point. Here, we subtract the observed distance from this optimal distance. This captured the divergence from optimal distance in pixels. 

This was done because, in the three types of configurations (ego-aligned, other-aligned, and neither-aligned), the average distance from the start point of the trial to the selected folder differed.



```r
resAll$divergDist = resAll$totalDistanceDV-resAll$optimalDist 
resAllError$divergDist = resAllError$totalDistanceDV-resAllError$optimalDist
#raw pixel distance from optimal

#some info about optimal distance
```

## Descriptives

Let's get some descriptives for ambiguous and control trials 


```r
resAll$egoChoice = as.numeric(as.matrix(resAll$egoChoice)) #if egoChoice has been converted to factor for LMERs

# Ambiguous/critical trials (excluding offset, to simplify)
pander(aggregate(egoChoice~config+axis,data=resAll,FUN=mean))
```


-----------------------------
 config    axis    egoChoice 
-------- -------- -----------
  ego    lateral    0.3052   

neither  lateral    0.3384   

partner  lateral    0.2839   

  ego    sagittal   0.2434   

neither  sagittal   0.2201   

partner  sagittal   0.2528   
-----------------------------

```r
pander(aggregate(RTDV~config+perspectivePreference+axis,data=resAll,FUN=mean))
```


------------------------------------------------
 config   perspectivePreference    axis    RTDV 
-------- ----------------------- -------- ------
  ego              ego           lateral   1343 

neither            ego           lateral   1281 

partner            ego           lateral   1402 

  ego             mixed          lateral   1530 

neither           mixed          lateral   1689 

partner           mixed          lateral   1603 

  ego             other          lateral   1733 

neither           other          lateral   2106 

partner           other          lateral   1708 

  ego              ego           sagittal  1253 

neither            ego           sagittal  1385 

partner            ego           sagittal  1328 

  ego             mixed          sagittal  1300 

neither           mixed          sagittal  1478 

partner           mixed          sagittal  1379 

  ego             other          sagittal  1528 

neither           other          sagittal  1493 

partner           other          sagittal  1439 
------------------------------------------------

```r
#pander(aggregate(xFlipDV~config+perspectivePreference+axis,data=resAll,FUN=mean))
#pander(aggregate(divergDist~config+perspectivePreference+axis,data=resAll,FUN=mean))

#some info about optimal distance
#summary(lm(optimalDist~egoChosen*config,data=resAll))
#pander(aggregate(optimalDist~config+egoChosen,data=resAll,FUN=mean))

#Consider the errors made by axis by responder 
#pander(aggregate(egoChoice~axis+fl+perspectivePreference,data=resAll,FUN=mean))

#This is to investigate what mixed responders are doing across trials in the two exps.
#resAllmixed = resAll[resAll$perspectivePreference == 'mixed',]
#options(max.print=999999)
#aggregate(egoChoice~trial+fl,data=resAllmixed,FUN=mean)


#resAllError$err = as.numeric(as.matrix(resAllError$err)) #if err has been converted to factor for LMERs
pander(aggregate(err~perspectivePreference+offset+axis+config,data=resAllError,FUN=mean)) 
```


----------------------------------------------------------
 perspectivePreference   offset    axis    config    err  
----------------------- -------- -------- -------- -------
          ego              0     lateral    both   0.0315 

         mixed             0     lateral    both   0.1558 

         other             0     lateral    both   0.02459

          ego              0     sagittal   both   0.04478

         mixed             0     sagittal   both   0.2593 

         other             0     sagittal   both   0.07784

          ego              90    lateral    ego    0.06154

         mixed             90    lateral    ego     0.225 

         other             90    lateral    ego    0.02823

          ego              90    sagittal   ego    0.06154

         mixed             90    sagittal   ego      0.2  

         other             90    sagittal   ego     0.044 

          ego              90    lateral  neither  0.01471

         mixed             90    lateral  neither  0.1579 

         other             90    lateral  neither  0.02372

          ego              90    sagittal neither  0.04478

         mixed             90    sagittal neither  0.3077 

         other             90    sagittal neither  0.03162

          ego              90    lateral  partner  0.01515

         mixed             90    lateral  partner    0.1  

         other             90    lateral  partner  0.04032

          ego              90    sagittal partner  0.06061

         mixed             90    sagittal partner  0.2683 

         other             90    sagittal partner  0.06827
----------------------------------------------------------

```r
pander(aggregate(RTDV~perspectivePreference+axis+config,data=resAllError,FUN=mean))
```


------------------------------------------------
 perspectivePreference    axis    config   RTDV 
----------------------- -------- -------- ------
          ego           lateral    both    1683 

         mixed          lateral    both    1575 

         other          lateral    both    2005 

          ego           sagittal   both    1469 

         mixed          sagittal   both    1615 

         other          sagittal   both    1740 

          ego           lateral    ego     1362 

         mixed          lateral    ego     1352 

         other          lateral    ego     2015 

          ego           sagittal   ego     1380 

         mixed          sagittal   ego     1537 

         other          sagittal   ego     1807 

          ego           lateral  neither   1350 

         mixed          lateral  neither   1641 

         other          lateral  neither   1726 

          ego           sagittal neither   1312 

         mixed          sagittal neither   1267 

         other          sagittal neither   1473 

          ego           lateral  partner   1480 

         mixed          lateral  partner   1743 

         other          lateral  partner   1934 

          ego           sagittal partner   1531 

         mixed          sagittal partner   1678 

         other          sagittal partner   1640 
------------------------------------------------

```r
#pander(aggregate(xFlipDV~perspectivePreference+axis+config,data=resAllError,FUN=mean))
#pander(aggregate(divergDist~perspectivePreference+axis+config,data=resAllError,FUN=mean))
```

## Linear mixed effects models for ambiguous (critical) trials

We create separate linear mixed effects models for each of the dependent variables (proportion of egocentric choices, RT, total divergence from optimal distance, x-flips) on ambiguous/critical trials. 

We start with some pre-processing of the variables, setting the reference categories where relevant, and inspecting the data structure.


```r
resAll = as.data.frame(as.matrix(resAll))
#Defining as factors in order to set reference categories next
resAll$config = as.factor(as.matrix(resAll$config))
resAll$offset = as.factor(as.matrix(resAll$offset))
resAll$axis = as.factor(as.matrix(resAll$axis))
resAll$perspectivePreference = as.factor(as.matrix(resAll$perspectivePreference))
resAll$trial = as.integer(as.matrix(resAll$trial))

##Check for any "holes" in the design
with(resAll, table(config, offset, axis, perspectivePreference))
```

```
## , , axis = lateral, perspectivePreference = ego
## 
##          offset
## config    180  90
##   ego       0 134
##   neither 134  67
##   partner   0 135
## 
## , , axis = sagittal, perspectivePreference = ego
## 
##          offset
## config    180  90
##   ego       0 130
##   neither 135  66
##   partner   0 134
## 
## , , axis = lateral, perspectivePreference = mixed
## 
##          offset
## config    180  90
##   ego       0  80
##   neither  81  40
##   partner   0  84
## 
## , , axis = sagittal, perspectivePreference = mixed
## 
##          offset
## config    180  90
##   ego       0  82
##   neither  80  40
##   partner   0  84
## 
## , , axis = lateral, perspectivePreference = other
## 
##          offset
## config    180  90
##   ego       0 497
##   neither 481 246
##   partner   0 503
## 
## , , axis = sagittal, perspectivePreference = other
## 
##          offset
## config    180  90
##   ego       0 503
##   neither 501 250
##   partner   0 502
```

```r
#when offset is 180, configurations cannot be ego-aligned or other-aligned, maybe remove offset as factor?
#with(resAll, table(config, axis, perspectivePreference))

#Make sure DVs are of the right type
resAll$RTDV = as.numeric(as.matrix(resAll$RTDV))
resAll$divergDist = as.numeric(as.matrix(resAll$divergDist))
resAll$xFlipDV = as.integer(as.matrix(resAll$xFlipDV))
resAll$egoChoice = as.factor(as.matrix(resAll$egoChoice))

#str(resAll)
```

## Step 1: 
Let's set up the contrast structure for each factor of interest (axis, preference, offset)


```r
resAll.cont <- within(resAll, {
  
  # Factor 1: instruction orientation 
  axis.latVsag <- ifelse( axis=="lateral", 1/2, 
                          ifelse( axis=="sagittal", -1/2, NA ) ) 
  
  # Factor 2: Perspective preference
  PP.othVego <- ifelse( perspectivePreference=="other", 1/2,
                        ifelse( perspectivePreference=="mixed", 0,
                                ifelse( perspectivePreference=="ego", -1/2, NA )))
  
  PP.mixVego <- ifelse( perspectivePreference=="other", 0,
                        ifelse( perspectivePreference=="mixed", 1/2,
                                ifelse( perspectivePreference=="ego", -1/2, NA )))    
  
  # Factor 3: Configuation of folders
  config.neiVego <- ifelse( config=="neither", 1/2,
                            ifelse( config=="partner", 0,
                                    ifelse( config=="ego", -1/2, NA )))
  
  config.parVego <- ifelse( config=="neither", 0,
                            ifelse( config=="partner", 1/2,
                                    ifelse( config=="ego", -1/2, NA )))
  
} )
```

##Step 2: 
Let's build the omnibus model for each DV with all effects of interest for research questions


```r
## Response time ##

RTModel.omni <- lmer( log(RTDV) ~ (1 | fl) + (0 + (config.neiVego + config.parVego) | fl) + (0 + axis.latVsag | fl) +
                        # perspectivePreference*config*axis
                        axis.latVsag * (PP.othVego + PP.mixVego) * (config.neiVego + config.parVego),        
                      data=resAll.cont, REML=FALSE )
summary(RTModel.omni)
```

```
## Linear mixed model fit by maximum likelihood t-tests use Satterthwaite
##   approximations to degrees of freedom [lmerMod]
## Formula: log(RTDV) ~ (1 | fl) + (0 + (config.neiVego + config.parVego) |  
##     fl) + (0 + axis.latVsag | fl) + axis.latVsag * (PP.othVego +  
##     PP.mixVego) * (config.neiVego + config.parVego)
##    Data: resAll.cont
## 
##      AIC      BIC   logLik deviance df.resid 
##   4541.5   4697.9  -2246.8   4493.5     4965 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.9131 -0.6377 -0.1444  0.4699  4.7720 
## 
## Random effects:
##  Groups   Name           Variance  Std.Dev. Corr 
##  fl       (Intercept)    5.693e-02 0.238601      
##  fl.1     config.neiVego 1.172e-03 0.034241      
##           config.parVego 4.748e-05 0.006891 -1.00
##  fl.2     axis.latVsag   1.089e-02 0.104345      
##  Residual                1.288e-01 0.358859      
## Number of obs: 4989, groups:  fl, 184
## 
## Fixed effects:
##                                          Estimate Std. Error         df
## (Intercept)                               7.19222    0.02413  184.00000
## axis.latVsag                              0.11924    0.01694  186.00000
## PP.othVego                                0.26271    0.05450  184.00000
## PP.mixVego                               -0.05844    0.07909  184.00000
## config.neiVego                            0.06682    0.01831  210.00000
## config.parVego                           -0.03448    0.01971 3994.00000
## axis.latVsag:PP.othVego                   0.16080    0.03828  187.00000
## axis.latVsag:PP.mixVego                   0.02511    0.05552  186.00000
## axis.latVsag:config.neiVego               0.03528    0.03601 4457.00000
## axis.latVsag:config.parVego              -0.02031    0.03939 4446.00000
## PP.othVego:config.neiVego                 0.14349    0.04138  211.00000
## PP.othVego:config.parVego                -0.12530    0.04456 3997.00000
## PP.mixVego:config.neiVego                 0.04112    0.06010  212.00000
## PP.mixVego:config.parVego                -0.01177    0.06452 3993.00000
## axis.latVsag:PP.othVego:config.neiVego    0.40082    0.08139 4459.00000
## axis.latVsag:PP.othVego:config.parVego   -0.12047    0.08907 4447.00000
## axis.latVsag:PP.mixVego:config.neiVego   -0.13388    0.11819 4461.00000
## axis.latVsag:PP.mixVego:config.parVego    0.02998    0.12896 4446.00000
##                                        t value Pr(>|t|)    
## (Intercept)                            298.014  < 2e-16 ***
## axis.latVsag                             7.039 3.61e-11 ***
## PP.othVego                               4.821 2.99e-06 ***
## PP.mixVego                              -0.739 0.460896    
## config.neiVego                           3.650 0.000331 ***
## config.parVego                          -1.750 0.080216 .  
## axis.latVsag:PP.othVego                  4.201 4.11e-05 ***
## axis.latVsag:PP.mixVego                  0.452 0.651643    
## axis.latVsag:config.neiVego              0.980 0.327272    
## axis.latVsag:config.parVego             -0.516 0.606169    
## PP.othVego:config.neiVego                3.468 0.000637 ***
## PP.othVego:config.parVego               -2.812 0.004950 ** 
## PP.mixVego:config.neiVego                0.684 0.494569    
## PP.mixVego:config.parVego               -0.182 0.855252    
## axis.latVsag:PP.othVego:config.neiVego   4.925 8.76e-07 ***
## axis.latVsag:PP.othVego:config.parVego  -1.352 0.176286    
## axis.latVsag:PP.mixVego:config.neiVego  -1.133 0.257351    
## axis.latVsag:PP.mixVego:config.parVego   0.232 0.816185    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```
## 
## Correlation matrix not shown by default, as p = 18 > 12.
## Use print(x, correlation=TRUE)  or
## 	 vcov(x)	 if you need it
```

```r
## Divergence from Optimal Distance ##

DistanceModel.omni <- lmer( log(divergDist) ~ (1 | fl) + (0 + (config.neiVego + config.parVego) | fl) + (0 + axis.latVsag | fl) +
                              # perspectivePreference*config*axis
                              axis.latVsag * (PP.othVego + PP.mixVego) * (config.neiVego + config.parVego),        
                            data=resAll.cont, REML=FALSE )

summary(DistanceModel.omni)
```

```
## Linear mixed model fit by maximum likelihood t-tests use Satterthwaite
##   approximations to degrees of freedom [lmerMod]
## Formula: 
## log(divergDist) ~ (1 | fl) + (0 + (config.neiVego + config.parVego) |  
##     fl) + (0 + axis.latVsag | fl) + axis.latVsag * (PP.othVego +  
##     PP.mixVego) * (config.neiVego + config.parVego)
##    Data: resAll.cont
## 
##      AIC      BIC   logLik deviance df.resid 
##  12311.4  12467.7  -6131.7  12263.4     4965 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.7888 -0.6975 -0.0628  0.7034  3.1411 
## 
## Random effects:
##  Groups   Name           Variance  Std.Dev.  Corr
##  fl       (Intercept)    1.305e-01 3.612e-01     
##  fl.1     config.neiVego 0.000e+00 0.000e+00     
##           config.parVego 2.106e-15 4.589e-08  NaN
##  fl.2     axis.latVsag   1.245e-02 1.116e-01     
##  Residual                6.353e-01 7.970e-01     
## Number of obs: 4989, groups:  fl, 184
## 
## Fixed effects:
##                                          Estimate Std. Error         df
## (Intercept)                               4.23301    0.03816  183.00000
## axis.latVsag                              0.11824    0.03203  189.00000
## PP.othVego                                0.31430    0.08618  183.00000
## PP.mixVego                                0.09501    0.12507  183.00000
## config.neiVego                            0.09712    0.03998 4626.00000
## config.parVego                            0.02649    0.04374 4619.00000
## axis.latVsag:PP.othVego                   0.05129    0.07240  189.00000
## axis.latVsag:PP.mixVego                   0.09550    0.10502  189.00000
## axis.latVsag:config.neiVego              -0.20509    0.07995 4626.00000
## axis.latVsag:config.parVego               0.29880    0.08747 4621.00000
## PP.othVego:config.neiVego                 0.23126    0.09036 4627.00000
## PP.othVego:config.parVego                -0.24484    0.09891 4620.00000
## PP.mixVego:config.neiVego                -0.06658    0.13124 4626.00000
## PP.mixVego:config.parVego                 0.18416    0.14321 4620.00000
## axis.latVsag:PP.othVego:config.neiVego    0.91119    0.18073 4627.00000
## axis.latVsag:PP.othVego:config.parVego   -0.56441    0.19781 4621.00000
## axis.latVsag:PP.mixVego:config.neiVego   -0.75404    0.26242 4629.00000
## axis.latVsag:PP.mixVego:config.parVego    0.57265    0.28640 4621.00000
##                                        t value Pr(>|t|)    
## (Intercept)                            110.921  < 2e-16 ***
## axis.latVsag                             3.691 0.000292 ***
## PP.othVego                               3.647 0.000346 ***
## PP.mixVego                               0.760 0.448415    
## config.neiVego                           2.429 0.015161 *  
## config.parVego                           0.606 0.544690    
## axis.latVsag:PP.othVego                  0.709 0.479493    
## axis.latVsag:PP.mixVego                  0.909 0.364321    
## axis.latVsag:config.neiVego             -2.565 0.010346 *  
## axis.latVsag:config.parVego              3.416 0.000640 ***
## PP.othVego:config.neiVego                2.559 0.010519 *  
## PP.othVego:config.parVego               -2.475 0.013344 *  
## PP.mixVego:config.neiVego               -0.507 0.611959    
## PP.mixVego:config.parVego                1.286 0.198534    
## axis.latVsag:PP.othVego:config.neiVego   5.042 4.79e-07 ***
## axis.latVsag:PP.othVego:config.parVego  -2.853 0.004345 ** 
## axis.latVsag:PP.mixVego:config.neiVego  -2.873 0.004080 ** 
## axis.latVsag:PP.mixVego:config.parVego   1.999 0.045614 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```
## 
## Correlation matrix not shown by default, as p = 18 > 12.
## Use print(x, correlation=TRUE)  or
## 	 vcov(x)	 if you need it
```

```r
#print('Distance:'); pander(print_stats(DistanceModel.omni))


## Directional shifts ##

xFlip.omni <- lmer( xFlipDV ~ axis.latVsag * (PP.othVego + PP.mixVego) * (config.neiVego + config.parVego)
                    + (1 | fl),    
                    #+ (0 + (config.neiVego + config.parVego) | fl) 
                    #+ (0 + axis.latVsag | fl),
                    data=resAll.cont, 
                    #family = "poisson", 
                    REML=FALSE )
summary(xFlip.omni)
```

```
## Linear mixed model fit by maximum likelihood t-tests use Satterthwaite
##   approximations to degrees of freedom [lmerMod]
## Formula: 
## xFlipDV ~ axis.latVsag * (PP.othVego + PP.mixVego) * (config.neiVego +  
##     config.parVego) + (1 | fl)
##    Data: resAll.cont
## 
##      AIC      BIC   logLik deviance df.resid 
##  17324.9  17455.2  -8642.4  17284.9     4969 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.9468 -0.6753 -0.1358  0.5100  8.3577 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  fl       (Intercept) 0.4145   0.6438  
##  Residual             1.7379   1.3183  
## Number of obs: 4989, groups:  fl, 184
## 
## Fixed effects:
##                                          Estimate Std. Error         df
## (Intercept)                             2.009e+00  6.728e-02  1.850e+02
## axis.latVsag                           -4.901e-02  4.986e-02  4.807e+03
## PP.othVego                              4.988e-01  1.519e-01  1.860e+02
## PP.mixVego                             -1.196e-02  2.205e-01  1.860e+02
## config.neiVego                          6.447e-02  6.612e-02  4.807e+03
## config.parVego                          1.748e-02  7.234e-02  4.806e+03
## axis.latVsag:PP.othVego                 4.343e-01  1.127e-01  4.807e+03
## axis.latVsag:PP.mixVego                -3.822e-02  1.635e-01  4.807e+03
## axis.latVsag:config.neiVego            -1.747e-02  1.322e-01  4.807e+03
## axis.latVsag:config.parVego             3.618e-03  1.447e-01  4.806e+03
## PP.othVego:config.neiVego               5.050e-01  1.494e-01  4.807e+03
## PP.othVego:config.parVego              -5.481e-01  1.636e-01  4.806e+03
## PP.mixVego:config.neiVego               1.331e-01  2.171e-01  4.808e+03
## PP.mixVego:config.parVego               7.233e-02  2.369e-01  4.806e+03
## axis.latVsag:PP.othVego:config.neiVego  9.318e-01  2.989e-01  4.807e+03
## axis.latVsag:PP.othVego:config.parVego -7.225e-01  3.272e-01  4.806e+03
## axis.latVsag:PP.mixVego:config.neiVego -7.853e-01  4.340e-01  4.807e+03
## axis.latVsag:PP.mixVego:config.parVego  5.033e-01  4.737e-01  4.806e+03
##                                        t value Pr(>|t|)    
## (Intercept)                             29.858  < 2e-16 ***
## axis.latVsag                            -0.983 0.325618    
## PP.othVego                               3.283 0.001227 ** 
## PP.mixVego                              -0.054 0.956789    
## config.neiVego                           0.975 0.329558    
## config.parVego                           0.242 0.809112    
## axis.latVsag:PP.othVego                  3.854 0.000118 ***
## axis.latVsag:PP.mixVego                 -0.234 0.815152    
## axis.latVsag:config.neiVego             -0.132 0.894899    
## axis.latVsag:config.parVego              0.025 0.980047    
## PP.othVego:config.neiVego                3.379 0.000732 ***
## PP.othVego:config.parVego               -3.350 0.000813 ***
## PP.mixVego:config.neiVego                0.613 0.539818    
## PP.mixVego:config.parVego                0.305 0.760100    
## axis.latVsag:PP.othVego:config.neiVego   3.118 0.001834 ** 
## axis.latVsag:PP.othVego:config.parVego  -2.209 0.027253 *  
## axis.latVsag:PP.mixVego:config.neiVego  -1.810 0.070434 .  
## axis.latVsag:PP.mixVego:config.parVego   1.063 0.288013    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```
## 
## Correlation matrix not shown by default, as p = 18 > 12.
## Use print(x, correlation=TRUE)  or
## 	 vcov(x)	 if you need it
```

```r
#print('Directional Shifts:'); pander(print_stats(xFlip.omni))


## Proportion of Egocentric responses ##

EgoChoiceModel.omni = glmer(egoChoice ~ (config.neiVego + config.parVego) * axis.latVsag
                     + (1 | fl)
                     #+ (0 + (config.neiVego + config.parVego) | fl) #did not converge
                     + (0 + axis.latVsag | fl), 
                     data=resAll.cont,
                     family = "binomial",
                     method = "Laplace",
                     nAGQ = 1,
                     REML = FALSE)
```

```
## Warning: Argument 'method' is deprecated. Use the nAGQ argument to specify
## Laplace (nAGQ=1) or adaptive Gauss-Hermite quadrature (nAGQ>1). PQL is no
## longer available.
```

```
## Warning: extra argument(s) 'REML' disregarded
```

```r
summary(EgoChoiceModel.omni)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: egoChoice ~ (config.neiVego + config.parVego) * axis.latVsag +  
##     (1 | fl) + (0 + axis.latVsag | fl)
##    Data: resAll.cont
## 
##      AIC      BIC   logLik deviance df.resid 
##   2911.7   2963.8  -1447.8   2895.7     4981 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -5.0124 -0.2758 -0.1098  0.1256  5.7138 
## 
## Random effects:
##  Groups Name         Variance Std.Dev.
##  fl     (Intercept)  12.626   3.553   
##  fl.1   axis.latVsag  2.491   1.578   
## Number of obs: 4989, groups:  fl, 184
## 
## Fixed effects:
##                             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                 -2.31873    0.29545  -7.848 4.22e-15 ***
## config.neiVego               0.02294    0.14751   0.156  0.87642    
## config.parVego              -0.13143    0.16117  -0.815  0.41481    
## axis.latVsag                 0.99729    0.19302   5.167 2.38e-07 ***
## config.neiVego:axis.latVsag  1.35209    0.29653   4.560 5.12e-06 ***
## config.parVego:axis.latVsag -1.05581    0.32313  -3.267  0.00109 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) cnfg.nV cnfg.pV axs.lV cnfg.nV:.V
## config.neVg -0.034                                  
## config.prVg  0.020 -0.460                           
## axis.latVsg -0.068 -0.030   0.020                   
## cnfg.nVg:.V -0.029 -0.109   0.034  -0.078           
## cnfg.prV:.V  0.022  0.036  -0.046   0.032 -0.464
```

##Step 3: 
Get the effect sizes of omnibus based on the MuMIn package - designed for mixed effects models


```r
##// Effect Size

r.squaredGLMM(RTModel.omni)[1]
```

```
##       R2m 
## 0.1047252
```

```r
r.squaredGLMM(RTModel.omni)[2] #we report this
```

```
##       R2c 
## 0.3887335
```

```r
r.squaredGLMM(DistanceModel.omni)[1]
```

```
##        R2m 
## 0.04359131
```

```r
r.squaredGLMM(DistanceModel.omni)[2] #we report this
```

```
##     R2c 
## 0.20974
```

```r
r.squaredGLMM(xFlip.omni)[1]
```

```
##        R2m 
## 0.03295435
```

```r
r.squaredGLMM(xFlip.omni)[2]
```

```
##       R2c 
## 0.2191791
```

```r
r.squaredGLMM(EgoChoiceModel.omni)[1]
```

```
## Warning: Argument 'method' is deprecated. Use the nAGQ argument to specify
## Laplace (nAGQ=1) or adaptive Gauss-Hermite quadrature (nAGQ>1). PQL is no
## longer available.
```

```
## Warning: extra argument(s) 'REML' disregarded
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
## $checkConv, : Model failed to converge with max|grad| = 0.0506444 (tol =
## 0.001, component 1)
```

```
## The result is correct only if all data used by the model has not changed since model was fitted.
```

```
##        R2m 
## 0.02166323
```

```r
r.squaredGLMM(EgoChoiceModel.omni)[2]
```

```
## Warning: Argument 'method' is deprecated. Use the nAGQ argument to specify
## Laplace (nAGQ=1) or adaptive Gauss-Hermite quadrature (nAGQ>1). PQL is no
## longer available.
```

```
## Warning: extra argument(s) 'REML' disregarded
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
## $checkConv, : Model failed to converge with max|grad| = 0.0506444 (tol =
## 0.001, component 1)
```

```
## The result is correct only if all data used by the model has not changed since model was fitted.
```

```
##      R2c 
## 0.806247
```

##STEP 4a: 
In order to interpret the individual coefficients of the 2-way and 3-way interactions, need to first establish that the overall interactions are statistically significant
Note: Testing the 2-way interaction between perspectivePreference and config


```r
#This function cleans up the output of the likelihood-ratio tests
modComp = function(model1,model2,modname){
  
  llcomp = anova(model1,model2)
  df1 = llcomp$"Chi Df"[2]
  chi1 = llcomp$"Chisq"[2]
  pr1 = llcomp$"Pr(>Chisq)"[2]
  
  test1 = data.frame(c(df1,chi1,pr1))
  colnames(test1) = modname
  rownames(test1) = c("df","chi","p")
  return(test1)
}

## Note: what this does below is remove the critical 2-way interaction coefficients from the omnibus and then compares to the omni with likelihood ratio test

## what are the effects associated with the interaction between perspectivePreference and config?
# PP.othVego:config.neiVego 
# PP.othVego:config.parVego
# PP.mixVego:config.neiVego
# PP.mixVego:config.parVego

twoway.RTModel.PPvsConfig = update(RTModel.omni,.~.-PP.othVego:config.neiVego-PP.othVego:config.parVego-PP.mixVego:config.neiVego-PP.mixVego:config.parVego)
modComp(RTModel.omni,twoway.RTModel.PPvsConfig,"twoway.RTModel.PPvsConfig")
```

```
##     twoway.RTModel.PPvsConfig
## df               4.000000e+00
## chi              2.222610e+01
## p                1.806828e-04
```

```r
twoway.DistanceModel.PPvsConfig = update(DistanceModel.omni,.~.-PP.othVego:config.neiVego-PP.othVego:config.parVego-PP.mixVego:config.neiVego-PP.mixVego:config.parVego)
modComp(DistanceModel.omni,twoway.DistanceModel.PPvsConfig,"twoway.DistanceModel.PPvsConfig")
```

```
##     twoway.DistanceModel.PPvsConfig
## df                       4.00000000
## chi                      9.87740089
## p                        0.04254427
```

```r
twoway.xFlipModel.PPvsConfig = update(xFlip.omni,.~.-PP.othVego:config.neiVego-PP.othVego:config.parVego-PP.mixVego:config.neiVego-PP.mixVego:config.parVego)
modComp(xFlip.omni,twoway.xFlipModel.PPvsConfig,"twoway.xFlipModel.PPvsConfig")
```

```
##     twoway.xFlipModel.PPvsConfig
## df                  4.000000e+00
## chi                 2.357615e+01
## p                   9.712017e-05
```

##STEP 4b: 
In order to interpret the individual coefficients of the 2-way and 3-way interactions, need to first establish that the overall interactions are statisticall significant
Note: Testing the 2-way interaction between perspectivePreference and axis


```r
## what are the effects associated with the interaction between perspectivePreference and axis?
# axis.latVsag:PP.othVego 
# axis.latVsag:PP.mixVego

twoway.RTModel.PPvsAxis = update(RTModel.omni,.~.-axis.latVsag:PP.othVego-axis.latVsag:PP.mixVego)
modComp(RTModel.omni,twoway.RTModel.PPvsAxis,"twoway.RTModel.PPvsAxis")
```

```
##     twoway.RTModel.PPvsAxis
## df             2.000000e+00
## chi            2.542970e+01
## p              3.006148e-06
```

```r
twoway.DistanceModel.PPvsAxis = update(DistanceModel.omni,.~.-axis.latVsag:PP.othVego-axis.latVsag:PP.mixVego)
modComp(DistanceModel.omni,twoway.DistanceModel.PPvsAxis,"twoway.DistanceModel.PPvsAxis")
```

```
##     twoway.DistanceModel.PPvsAxis
## df                       2.000000
## chi                      2.725241
## p                        0.255989
```

```r
twoway.xFlip.PPvsAxis = update(xFlip.omni,.~.-axis.latVsag:PP.othVego-axis.latVsag:PP.mixVego)
modComp(xFlip.omni,twoway.xFlip.PPvsAxis,"twoway.xFlipModel.PPvsAxis")
```

```
##     twoway.xFlipModel.PPvsAxis
## df                2.000000e+00
## chi               1.912743e+01
## p                 7.023157e-05
```

##STEP 4c: 
Examining what are the effects associated with the interaction between config and axis?
axis.latVsag:config.neiVego
axis.latVsag:config.parVego 


```r
twoway.RTModel.ConfigvsAxis = update(RTModel.omni,.~.-axis.latVsag:config.neiVego-axis.latVsag:config.parVego)
modComp(RTModel.omni,twoway.RTModel.ConfigvsAxis,"twoway.RTModel.ConfigvsAxis")
```

```
##     twoway.RTModel.ConfigvsAxis
## df                    2.0000000
## chi                   0.9669669
## p                     0.6166316
```

```r
twoway.DistanceModel.ConfigvsAxis = update(DistanceModel.omni,.~.-axis.latVsag:config.neiVego-axis.latVsag:config.parVego)
modComp(DistanceModel.omni,twoway.DistanceModel.ConfigvsAxis,"twoway.DistanceModel.ConfigvsAxis")
```

```
##     twoway.DistanceModel.ConfigvsAxis
## df                        2.000000000
## chi                      12.986258746
## p                         0.001513804
```

```r
twoway.xFlip.ConfigvsAxis = update(xFlip.omni,.~.-axis.latVsag:config.neiVego-axis.latVsag:config.parVego)
modComp(xFlip.omni,twoway.xFlip.ConfigvsAxis,"twoway.xFlipModel.ConfigvsAxis")
```

```
##     twoway.xFlipModel.ConfigvsAxis
## df                      2.00000000
## chi                     0.01892551
## p                       0.99058188
```

##STEP 4d: 
In order to interpret the individual coefficients of the 2-way and 3-way interactions, need to first establish that the overall interactions are statisticall significant
Testing the 3-way interaction between perspectivePreference, config, and axis


```r
threeway.RTModel = update(RTModel.omni,.~.-axis.latVsag:PP.othVego:config.neiVego-axis.latVsag:PP.othVego:config.parVego-axis.latVsag:PP.mixVego:config.neiVego-axis.latVsag:PP.mixVego:config.parVego)
modComp(RTModel.omni,threeway.RTModel,"threeway.RTModel")
```

```
##     threeway.RTModel
## df      4.000000e+00
## chi     2.803447e+01
## p       1.227390e-05
```

```r
threeway.DistanceModel = update(DistanceModel.omni,.~.-axis.latVsag:PP.othVego:config.neiVego-axis.latVsag:PP.othVego:config.parVego-axis.latVsag:PP.mixVego:config.neiVego-axis.latVsag:PP.mixVego:config.parVego)
modComp(DistanceModel.omni,threeway.DistanceModel,"threeway.DistanceModel")
```

```
##     threeway.DistanceModel
## df            4.000000e+00
## chi           2.613591e+01
## p             2.970916e-05
```

```r
threeway.xFlipModel = update(xFlip.omni,.~.-axis.latVsag:PP.othVego:config.neiVego-axis.latVsag:PP.othVego:config.parVego-axis.latVsag:PP.mixVego:config.neiVego-axis.latVsag:PP.mixVego:config.parVego)
modComp(xFlip.omni,threeway.xFlipModel,"threeway.xFlipModel")
```

```
##     threeway.xFlipModel
## df           4.00000000
## chi         10.61225141
## p            0.03128539
```

##STEP 5: 
We have an overall significant 3-way interaction, so we need to decompose holding each level of axis constant and rerun. Essentially, this is a simple effects follow-up of the 3-way


```r
# **Simple effects follow-up (assuming sig 3-way):** 
# Important note: Only pay attention to effects in the following models that do NOT interact with the dummy codes (axis.lat and axis.sag)...
# (i.e., ignore any fixed effect that includes "axis.lat" or "axis.sag," whether it is an interaction or main effect)"

resAll.cont <- within(resAll.cont, {
  
  axis.lat <- ifelse( axis=="lateral", 0, 
                      ifelse( axis=="sagittal", 1, NA ) ) 
  
  axis.sag <- ifelse( axis=="lateral", 1, 
                      ifelse( axis=="sagittal", 0, NA ) ) 
} )

#######
##// Axis: Lateral
#######

## RT ##

lat.omnibus.RT <- lmer( log(RTDV) ~ (1 | fl) + (0 + (config.neiVego + config.parVego) | fl) +
                          axis.lat * (PP.othVego + PP.mixVego) * (config.neiVego + config.parVego),        
                        data=resAll.cont, REML=FALSE )

##// test for two-way interaction of perspectivePreference and config in order to follow-up
lat.RT = update( lat.omnibus.RT, .~.-PP.othVego:config.neiVego-PP.othVego:config.parVego-PP.mixVego:config.neiVego-PP.mixVego:config.parVego)
modComp(lat.omnibus.RT,lat.RT,"lat.omnibus.RT")
```

```
##     lat.omnibus.RT
## df    4.000000e+00
## chi   4.356409e+01
## p     7.902612e-09
```

```r
## Distance ##

lat.omnibus.Distance <- lmer( log(divergDist) ~ (1 | fl) + (0 + (config.neiVego + config.parVego) | fl) +
                                axis.lat * (PP.othVego + PP.mixVego) * (config.neiVego + config.parVego),        
                              data=resAll.cont, REML=FALSE )

##// test for two-way interaction of perspectivePreference and config in order to follow-up
lat.Distance = update( lat.omnibus.Distance, .~.-PP.othVego:config.neiVego-PP.othVego:config.parVego-PP.mixVego:config.neiVego-PP.mixVego:config.parVego)
modComp(lat.omnibus.Distance,lat.Distance,"lat.omnibus.Distance")
```

```
##     lat.omnibus.Distance
## df          4.000000e+00
## chi         3.132401e+01
## p           2.629087e-06
```

```r
## Directional Shifts ##

lat.omnibus.xFlip <- lmer( xFlipDV ~ (1 | fl) +
                                axis.lat * (PP.othVego + PP.mixVego) * (config.neiVego + config.parVego),        
                              data=resAll.cont, REML=FALSE )

##// test for two-way interaction of perspectivePreference and config in order to follow-up
lat.xFlip = update( lat.omnibus.xFlip, .~.-PP.othVego:config.neiVego-PP.othVego:config.parVego-PP.mixVego:config.neiVego-PP.mixVego:config.parVego)
modComp(lat.omnibus.xFlip,lat.xFlip,"lat.omnibus.xFlip")
```

```
##     lat.omnibus.xFlip
## df       4.000000e+00
## chi      2.892021e+01
## p        8.114645e-06
```

```r
#######
##// Axis: Sagittal
#######

## RT ##

sag.omnibus.RT <- lmer( log(RTDV) ~ (1 | fl) + (0 + (config.neiVego + config.parVego) | fl) +
                          axis.sag * (PP.othVego + PP.mixVego) * (config.neiVego + config.parVego),        
                        data=resAll.cont, REML=FALSE )

##// test for two-way interaction of perspectivePreference and config in order to follow-up
sag.RT = update( sag.omnibus.RT, .~.-PP.othVego:config.neiVego-PP.othVego:config.parVego-PP.mixVego:config.neiVego-PP.mixVego:config.parVego)
modComp(sag.omnibus.RT,sag.RT,"sag.omnibus.RT")
```

```
##     sag.omnibus.RT
## df        4.000000
## chi       4.857639
## p         0.302219
```

```r
## Distance ##

sag.omnibus.Distance <- lmer( log(divergDist) ~ (1 | fl) + (0 + (config.neiVego + config.parVego) | fl) +
                                axis.sag * (PP.othVego + PP.mixVego) * (config.neiVego + config.parVego),        
                              data=resAll.cont, REML=FALSE )

##// test for two-way interaction of perspectivePreference and config in order to follow-up
sag.Distance = update( sag.omnibus.Distance, .~.-PP.othVego:config.neiVego-PP.othVego:config.parVego-PP.mixVego:config.neiVego-PP.mixVego:config.parVego)
modComp(sag.omnibus.Distance,sag.Distance,"sag.omnibus.Distance")
```

```
##     sag.omnibus.Distance
## df             4.0000000
## chi            4.1837373
## p              0.3817104
```

```r
summary(lat.omnibus.Distance)
```

```
## Linear mixed model fit by maximum likelihood t-tests use Satterthwaite
##   approximations to degrees of freedom [lmerMod]
## Formula: 
## log(divergDist) ~ (1 | fl) + (0 + (config.neiVego + config.parVego) |  
##     fl) + axis.lat * (PP.othVego + PP.mixVego) * (config.neiVego +  
##     config.parVego)
##    Data: resAll.cont
## 
##      AIC      BIC   logLik deviance df.resid 
##  12310.8  12460.6  -6132.4  12264.8     4966 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.7756 -0.7019 -0.0672  0.7099  3.1679 
## 
## Random effects:
##  Groups   Name           Variance  Std.Dev.  Corr
##  fl       (Intercept)    1.303e-01 3.609e-01     
##  fl.1     config.neiVego 0.000e+00 0.000e+00     
##           config.parVego 5.235e-14 2.288e-07  NaN
##  Residual                6.385e-01 7.991e-01     
## Number of obs: 4989, groups:  fl, 184
## 
## Fixed effects:
##                                      Estimate Std. Error         df
## (Intercept)                         4.292e+00  4.104e-02  2.450e+02
## axis.lat                           -1.183e-01  3.022e-02  4.804e+03
## PP.othVego                          3.396e-01  9.270e-02  2.450e+02
## PP.mixVego                          1.427e-01  1.345e-01  2.450e+02
## config.neiVego                     -5.216e-03  5.666e-02  4.804e+03
## config.parVego                      1.760e-01  6.198e-02  4.804e+03
## axis.lat:PP.othVego                -5.066e-02  6.830e-02  4.805e+03
## axis.lat:PP.mixVego                -9.549e-02  9.908e-02  4.804e+03
## axis.lat:config.neiVego             2.047e-01  8.015e-02  4.805e+03
## axis.lat:config.parVego            -2.990e-01  8.768e-02  4.804e+03
## PP.othVego:config.neiVego           6.859e-01  1.282e-01  4.805e+03
## PP.othVego:config.parVego          -5.271e-01  1.402e-01  4.804e+03
## PP.mixVego:config.neiVego          -4.430e-01  1.860e-01  4.804e+03
## PP.mixVego:config.parVego           4.705e-01  2.031e-01  4.804e+03
## axis.lat:PP.othVego:config.neiVego -9.103e-01  1.812e-01  4.805e+03
## axis.lat:PP.othVego:config.parVego  5.644e-01  1.983e-01  4.804e+03
## axis.lat:PP.mixVego:config.neiVego  7.523e-01  2.631e-01  4.804e+03
## axis.lat:PP.mixVego:config.parVego -5.722e-01  2.871e-01  4.804e+03
##                                    t value Pr(>|t|)    
## (Intercept)                        104.590  < 2e-16 ***
## axis.lat                            -3.913 9.23e-05 ***
## PP.othVego                           3.663 0.000305 ***
## PP.mixVego                           1.061 0.289736    
## config.neiVego                      -0.092 0.926657    
## config.parVego                       2.840 0.004530 ** 
## axis.lat:PP.othVego                 -0.742 0.458301    
## axis.lat:PP.mixVego                 -0.964 0.335214    
## axis.lat:config.neiVego              2.554 0.010669 *  
## axis.lat:config.parVego             -3.410 0.000655 ***
## PP.othVego:config.neiVego            5.349 9.24e-08 ***
## PP.othVego:config.parVego           -3.759 0.000172 ***
## PP.mixVego:config.neiVego           -2.382 0.017277 *  
## PP.mixVego:config.parVego            2.316 0.020574 *  
## axis.lat:PP.othVego:config.neiVego  -5.024 5.24e-07 ***
## axis.lat:PP.othVego:config.parVego   2.846 0.004440 ** 
## axis.lat:PP.mixVego:config.neiVego   2.860 0.004259 ** 
## axis.lat:PP.mixVego:config.parVego  -1.993 0.046334 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```
## 
## Correlation matrix not shown by default, as p = 18 > 12.
## Use print(x, correlation=TRUE)  or
## 	 vcov(x)	 if you need it
```

```r
#print('Lat.Distance:'); pander(print_stats(lat.omnibus.Distance))

## Directional Shifts ##

sag.omnibus.xFlip <- lmer( xFlipDV ~ (1 | fl) +
                                axis.sag * (PP.othVego + PP.mixVego) * (config.neiVego + config.parVego),        
                              data=resAll.cont, REML=FALSE )

##// test for two-way interaction of perspectivePreference and config in order to follow-up
sag.xFlip = update( sag.omnibus.xFlip, .~.-PP.othVego:config.neiVego-PP.othVego:config.parVego-PP.mixVego:config.neiVego-PP.mixVego:config.parVego)
modComp(sag.omnibus.xFlip,sag.xFlip,"sag.omnibus.xFlip")
```

```
##     sag.omnibus.xFlip
## df          4.0000000
## chi         5.2228751
## p           0.2651839
```

```r
summary(lat.omnibus.xFlip)
```

```
## Linear mixed model fit by maximum likelihood t-tests use Satterthwaite
##   approximations to degrees of freedom [lmerMod]
## Formula: 
## xFlipDV ~ (1 | fl) + axis.lat * (PP.othVego + PP.mixVego) * (config.neiVego +  
##     config.parVego)
##    Data: resAll.cont
## 
##      AIC      BIC   logLik deviance df.resid 
##  17324.9  17455.2  -8642.4  17284.9     4969 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.9468 -0.6753 -0.1358  0.5100  8.3577 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  fl       (Intercept) 0.4145   0.6438  
##  Residual             1.7379   1.3183  
## Number of obs: 4989, groups:  fl, 184
## 
## Fixed effects:
##                                      Estimate Std. Error         df
## (Intercept)                         1.984e+00  7.175e-02  2.400e+02
## axis.lat                            4.901e-02  4.986e-02  4.807e+03
## PP.othVego                          7.160e-01  1.621e-01  2.400e+02
## PP.mixVego                         -3.107e-02  2.352e-01  2.400e+02
## config.neiVego                      5.574e-02  9.347e-02  4.807e+03
## config.parVego                      1.929e-02  1.023e-01  4.806e+03
## axis.lat:PP.othVego                -4.343e-01  1.127e-01  4.807e+03
## axis.lat:PP.mixVego                 3.822e-02  1.635e-01  4.807e+03
## axis.lat:config.neiVego             1.747e-02  1.322e-01  4.807e+03
## axis.lat:config.parVego            -3.618e-03  1.447e-01  4.806e+03
## PP.othVego:config.neiVego           9.709e-01  2.115e-01  4.807e+03
## PP.othVego:config.parVego          -9.094e-01  2.313e-01  4.806e+03
## PP.mixVego:config.neiVego          -2.596e-01  3.069e-01  4.807e+03
## PP.mixVego:config.parVego           3.240e-01  3.351e-01  4.806e+03
## axis.lat:PP.othVego:config.neiVego -9.318e-01  2.989e-01  4.807e+03
## axis.lat:PP.othVego:config.parVego  7.225e-01  3.272e-01  4.806e+03
## axis.lat:PP.mixVego:config.neiVego  7.853e-01  4.340e-01  4.807e+03
## axis.lat:PP.mixVego:config.parVego -5.033e-01  4.737e-01  4.806e+03
##                                    t value Pr(>|t|)    
## (Intercept)                         27.656  < 2e-16 ***
## axis.lat                             0.983 0.325618    
## PP.othVego                           4.418 1.51e-05 ***
## PP.mixVego                          -0.132 0.895023    
## config.neiVego                       0.596 0.551019    
## config.parVego                       0.189 0.850427    
## axis.lat:PP.othVego                 -3.854 0.000118 ***
## axis.lat:PP.mixVego                  0.234 0.815152    
## axis.lat:config.neiVego              0.132 0.894899    
## axis.lat:config.parVego             -0.025 0.980047    
## PP.othVego:config.neiVego            4.590 4.55e-06 ***
## PP.othVego:config.parVego           -3.931 8.58e-05 ***
## PP.mixVego:config.neiVego           -0.846 0.397636    
## PP.mixVego:config.parVego            0.967 0.333674    
## axis.lat:PP.othVego:config.neiVego  -3.118 0.001834 ** 
## axis.lat:PP.othVego:config.parVego   2.209 0.027253 *  
## axis.lat:PP.mixVego:config.neiVego   1.810 0.070434 .  
## axis.lat:PP.mixVego:config.parVego  -1.063 0.288013    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```
## 
## Correlation matrix not shown by default, as p = 18 > 12.
## Use print(x, correlation=TRUE)  or
## 	 vcov(x)	 if you need it
```

```r
#print('Lat.xFlip:'); pander(print_stats(lat.omnibus.xFlip))
```

## Now let's examine the effect of trial


```r
resAll.cont$centered_trial =scale(resAll.cont$trial)

RTModel.time <- lmer( log(RTDV) ~ (1 | fl) + 
                        (0 + (config.neiVego + config.parVego) | fl) + (0 + axis.latVsag | fl) + (0 + centered_trial | fl) +
                        centered_trial * axis.latVsag * (PP.othVego + PP.mixVego) * (config.neiVego + config.parVego),        
                      data=resAll.cont, REML=FALSE )

summary(RTModel.time)
```

```
## Linear mixed model fit by maximum likelihood t-tests use Satterthwaite
##   approximations to degrees of freedom [lmerMod]
## Formula: log(RTDV) ~ (1 | fl) + (0 + (config.neiVego + config.parVego) |  
##     fl) + (0 + axis.latVsag | fl) + (0 + centered_trial | fl) +  
##     centered_trial * axis.latVsag * (PP.othVego + PP.mixVego) *  
##         (config.neiVego + config.parVego)
##    Data: resAll.cont
## 
##      AIC      BIC   logLik deviance df.resid 
##   3878.1   4158.2  -1896.0   3792.1     4946 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.2552 -0.6181 -0.1115  0.4967  5.1049 
## 
## Random effects:
##  Groups   Name           Variance  Std.Dev. Corr 
##  fl       (Intercept)    0.0583811 0.24162       
##  fl.1     config.neiVego 0.0062495 0.07905       
##           config.parVego 0.0006161 0.02482  -1.00
##  fl.2     axis.latVsag   0.0145771 0.12074       
##  fl.3     centered_trial 0.0081785 0.09043       
##  Residual                0.1050340 0.32409       
## Number of obs: 4989, groups:  fl, 184
## 
## Fixed effects:
##                                                         Estimate
## (Intercept)                                            7.194e+00
## centered_trial                                        -9.989e-02
## axis.latVsag                                           1.197e-01
## PP.othVego                                             2.592e-01
## PP.mixVego                                            -5.743e-02
## config.neiVego                                         6.382e-02
## config.parVego                                        -3.916e-02
## centered_trial:axis.latVsag                            3.123e-02
## centered_trial:PP.othVego                             -4.540e-02
## centered_trial:PP.mixVego                              2.159e-02
## axis.latVsag:PP.othVego                                1.672e-01
## axis.latVsag:PP.mixVego                                2.493e-02
## centered_trial:config.neiVego                         -1.955e-02
## centered_trial:config.parVego                          2.429e-03
## axis.latVsag:config.neiVego                            4.447e-02
## axis.latVsag:config.parVego                           -1.734e-02
## PP.othVego:config.neiVego                              1.467e-01
## PP.othVego:config.parVego                             -1.097e-01
## PP.mixVego:config.neiVego                              3.882e-02
## PP.mixVego:config.parVego                             -3.954e-02
## centered_trial:axis.latVsag:PP.othVego                -2.178e-02
## centered_trial:axis.latVsag:PP.mixVego                 6.070e-02
## centered_trial:axis.latVsag:config.neiVego             2.433e-02
## centered_trial:axis.latVsag:config.parVego            -1.044e-02
## centered_trial:PP.othVego:config.neiVego               4.301e-02
## centered_trial:PP.othVego:config.parVego               3.581e-02
## centered_trial:PP.mixVego:config.neiVego               5.947e-02
## centered_trial:PP.mixVego:config.parVego              -7.396e-02
## axis.latVsag:PP.othVego:config.neiVego                 4.294e-01
## axis.latVsag:PP.othVego:config.parVego                -1.681e-01
## axis.latVsag:PP.mixVego:config.neiVego                -1.547e-01
## axis.latVsag:PP.mixVego:config.parVego                 8.538e-02
## centered_trial:axis.latVsag:PP.othVego:config.neiVego -8.373e-02
## centered_trial:axis.latVsag:PP.othVego:config.parVego  1.599e-02
## centered_trial:axis.latVsag:PP.mixVego:config.neiVego  1.932e-01
## centered_trial:axis.latVsag:PP.mixVego:config.parVego -1.024e-01
##                                                       Std. Error
## (Intercept)                                            2.425e-02
## centered_trial                                         1.075e-02
## axis.latVsag                                           1.700e-02
## PP.othVego                                             5.476e-02
## PP.mixVego                                             7.947e-02
## config.neiVego                                         1.806e-02
## config.parVego                                         1.805e-02
## centered_trial:axis.latVsag                            1.239e-02
## centered_trial:PP.othVego                              2.428e-02
## centered_trial:PP.mixVego                              3.521e-02
## axis.latVsag:PP.othVego                                3.839e-02
## axis.latVsag:PP.mixVego                                5.573e-02
## centered_trial:config.neiVego                          1.658e-02
## centered_trial:config.parVego                          1.782e-02
## axis.latVsag:config.neiVego                            3.284e-02
## axis.latVsag:config.parVego                            3.593e-02
## PP.othVego:config.neiVego                              4.082e-02
## PP.othVego:config.parVego                              4.080e-02
## PP.mixVego:config.neiVego                              5.928e-02
## PP.mixVego:config.parVego                              5.913e-02
## centered_trial:axis.latVsag:PP.othVego                 2.802e-02
## centered_trial:axis.latVsag:PP.mixVego                 4.060e-02
## centered_trial:axis.latVsag:config.neiVego             3.358e-02
## centered_trial:axis.latVsag:config.parVego             3.592e-02
## centered_trial:PP.othVego:config.neiVego               3.751e-02
## centered_trial:PP.othVego:config.parVego               4.034e-02
## centered_trial:PP.mixVego:config.neiVego               5.434e-02
## centered_trial:PP.mixVego:config.parVego               5.830e-02
## axis.latVsag:PP.othVego:config.neiVego                 7.426e-02
## axis.latVsag:PP.othVego:config.parVego                 8.125e-02
## axis.latVsag:PP.mixVego:config.neiVego                 1.077e-01
## axis.latVsag:PP.mixVego:config.parVego                 1.177e-01
## centered_trial:axis.latVsag:PP.othVego:config.neiVego  7.593e-02
## centered_trial:axis.latVsag:PP.othVego:config.parVego  8.131e-02
## centered_trial:axis.latVsag:PP.mixVego:config.neiVego  1.099e-01
## centered_trial:axis.latVsag:PP.mixVego:config.parVego  1.174e-01
##                                                               df t value
## (Intercept)                                            1.840e+02 296.667
## centered_trial                                         1.860e+02  -9.295
## axis.latVsag                                           1.850e+02   7.045
## PP.othVego                                             1.840e+02   4.734
## PP.mixVego                                             1.840e+02  -0.723
## config.neiVego                                         1.850e+02   3.534
## config.parVego                                         1.530e+03  -2.170
## centered_trial:axis.latVsag                            4.378e+03   2.520
## centered_trial:PP.othVego                              1.870e+02  -1.870
## centered_trial:PP.mixVego                              1.870e+02   0.613
## axis.latVsag:PP.othVego                                1.850e+02   4.354
## axis.latVsag:PP.mixVego                                1.860e+02   0.447
## centered_trial:config.neiVego                          4.401e+03  -1.179
## centered_trial:config.parVego                          4.408e+03   0.136
## axis.latVsag:config.neiVego                            4.331e+03   1.354
## axis.latVsag:config.parVego                            4.319e+03  -0.483
## PP.othVego:config.neiVego                              1.860e+02   3.593
## PP.othVego:config.parVego                              1.534e+03  -2.688
## PP.mixVego:config.neiVego                              1.870e+02   0.655
## PP.mixVego:config.parVego                              1.531e+03  -0.669
## centered_trial:axis.latVsag:PP.othVego                 4.390e+03  -0.777
## centered_trial:axis.latVsag:PP.mixVego                 4.361e+03   1.495
## centered_trial:axis.latVsag:config.neiVego             4.481e+03   0.725
## centered_trial:axis.latVsag:config.parVego             4.415e+03  -0.291
## centered_trial:PP.othVego:config.neiVego               4.412e+03   1.147
## centered_trial:PP.othVego:config.parVego               4.412e+03   0.888
## centered_trial:PP.mixVego:config.neiVego               4.388e+03   1.094
## centered_trial:PP.mixVego:config.parVego               4.404e+03  -1.269
## axis.latVsag:PP.othVego:config.neiVego                 4.338e+03   5.782
## axis.latVsag:PP.othVego:config.parVego                 4.322e+03  -2.069
## axis.latVsag:PP.mixVego:config.neiVego                 4.323e+03  -1.437
## axis.latVsag:PP.mixVego:config.parVego                 4.315e+03   0.726
## centered_trial:axis.latVsag:PP.othVego:config.neiVego  4.487e+03  -1.103
## centered_trial:axis.latVsag:PP.othVego:config.parVego  4.416e+03   0.197
## centered_trial:axis.latVsag:PP.mixVego:config.neiVego  4.451e+03   1.759
## centered_trial:axis.latVsag:PP.mixVego:config.parVego  4.408e+03  -0.873
##                                                       Pr(>|t|)    
## (Intercept)                                            < 2e-16 ***
## centered_trial                                         < 2e-16 ***
## axis.latVsag                                          3.53e-11 ***
## PP.othVego                                            4.39e-06 ***
## PP.mixVego                                            0.470785    
## config.neiVego                                        0.000517 ***
## config.parVego                                        0.030145 *  
## centered_trial:axis.latVsag                           0.011780 *  
## centered_trial:PP.othVego                             0.063110 .  
## centered_trial:PP.mixVego                             0.540535    
## axis.latVsag:PP.othVego                               2.21e-05 ***
## axis.latVsag:PP.mixVego                               0.655164    
## centered_trial:config.neiVego                         0.238441    
## centered_trial:config.parVego                         0.891596    
## axis.latVsag:config.neiVego                           0.175729    
## axis.latVsag:config.parVego                           0.629382    
## PP.othVego:config.neiVego                             0.000418 ***
## PP.othVego:config.parVego                             0.007268 ** 
## PP.mixVego:config.neiVego                             0.513435    
## PP.mixVego:config.parVego                             0.503814    
## centered_trial:axis.latVsag:PP.othVego                0.436961    
## centered_trial:axis.latVsag:PP.mixVego                0.135005    
## centered_trial:axis.latVsag:config.neiVego            0.468765    
## centered_trial:axis.latVsag:config.parVego            0.771347    
## centered_trial:PP.othVego:config.neiVego              0.251561    
## centered_trial:PP.othVego:config.parVego              0.374717    
## centered_trial:PP.mixVego:config.neiVego              0.273876    
## centered_trial:PP.mixVego:config.parVego              0.204674    
## axis.latVsag:PP.othVego:config.neiVego                7.88e-09 ***
## axis.latVsag:PP.othVego:config.parVego                0.038581 *  
## axis.latVsag:PP.mixVego:config.neiVego                0.150922    
## axis.latVsag:PP.mixVego:config.parVego                0.468158    
## centered_trial:axis.latVsag:PP.othVego:config.neiVego 0.270167    
## centered_trial:axis.latVsag:PP.othVego:config.parVego 0.844088    
## centered_trial:axis.latVsag:PP.mixVego:config.neiVego 0.078717 .  
## centered_trial:axis.latVsag:PP.mixVego:config.parVego 0.382780    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```
## 
## Correlation matrix not shown by default, as p = 36 > 12.
## Use print(x, correlation=TRUE)  or
## 	 vcov(x)	 if you need it
```

```r
#print('RT:'); pander(print_stats(RTModel.time))

## Divergence from Optimal Distance ##

DistanceModel.time <- lmer( log(divergDist) ~ (1 | fl) +
                              (0 + (config.neiVego + config.parVego) | fl) + (0 + axis.latVsag | fl) + (0 + centered_trial | fl) +
                              centered_trial * axis.latVsag * (PP.othVego + PP.mixVego) * (config.neiVego + config.parVego),        
                            data=resAll.cont, REML=FALSE )

summary(DistanceModel.time)
```

```
## Linear mixed model fit by maximum likelihood t-tests use Satterthwaite
##   approximations to degrees of freedom [lmerMod]
## Formula: 
## log(divergDist) ~ (1 | fl) + (0 + (config.neiVego + config.parVego) |  
##     fl) + (0 + axis.latVsag | fl) + (0 + centered_trial | fl) +  
##     centered_trial * axis.latVsag * (PP.othVego + PP.mixVego) *  
##         (config.neiVego + config.parVego)
##    Data: resAll.cont
## 
##      AIC      BIC   logLik deviance df.resid 
##  12317.9  12598.0  -6115.9  12231.9     4946 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.7593 -0.6890 -0.0568  0.7033  2.9170 
## 
## Random effects:
##  Groups   Name           Variance  Std.Dev.  Corr 
##  fl       (Intercept)    1.304e-01 3.611e-01      
##  fl.1     config.neiVego 1.603e-11 4.003e-06      
##           config.parVego 2.126e-12 1.458e-06 -1.00
##  fl.2     axis.latVsag   1.479e-02 1.216e-01      
##  fl.3     centered_trial 6.292e-03 7.932e-02      
##  Residual                6.248e-01 7.905e-01      
## Number of obs: 4989, groups:  fl, 184
## 
## Fixed effects:
##                                                         Estimate
## (Intercept)                                            4.235e+00
## centered_trial                                        -2.869e-02
## axis.latVsag                                           1.178e-01
## PP.othVego                                             3.110e-01
## PP.mixVego                                             1.001e-01
## config.neiVego                                         9.297e-02
## config.parVego                                         2.178e-02
## centered_trial:axis.latVsag                            6.637e-02
## centered_trial:PP.othVego                             -1.311e-02
## centered_trial:PP.mixVego                             -4.494e-02
## axis.latVsag:PP.othVego                                5.274e-02
## axis.latVsag:PP.mixVego                                8.987e-02
## centered_trial:config.neiVego                          9.284e-03
## centered_trial:config.parVego                          1.735e-02
## axis.latVsag:config.neiVego                           -1.917e-01
## axis.latVsag:config.parVego                            3.032e-01
## PP.othVego:config.neiVego                              2.388e-01
## PP.othVego:config.parVego                             -2.256e-01
## PP.mixVego:config.neiVego                             -7.552e-02
## PP.mixVego:config.parVego                              1.625e-01
## centered_trial:axis.latVsag:PP.othVego                -2.953e-02
## centered_trial:axis.latVsag:PP.mixVego                 7.651e-02
## centered_trial:axis.latVsag:config.neiVego            -3.015e-03
## centered_trial:axis.latVsag:config.parVego            -5.003e-02
## centered_trial:PP.othVego:config.neiVego               1.061e-01
## centered_trial:PP.othVego:config.parVego              -2.531e-02
## centered_trial:PP.mixVego:config.neiVego               1.392e-01
## centered_trial:PP.mixVego:config.parVego              -7.124e-02
## axis.latVsag:PP.othVego:config.neiVego                 8.950e-01
## axis.latVsag:PP.othVego:config.parVego                -5.953e-01
## axis.latVsag:PP.mixVego:config.neiVego                -7.253e-01
## axis.latVsag:PP.mixVego:config.parVego                 6.237e-01
## centered_trial:axis.latVsag:PP.othVego:config.neiVego -2.330e-01
## centered_trial:axis.latVsag:PP.othVego:config.parVego  3.376e-01
## centered_trial:axis.latVsag:PP.mixVego:config.neiVego  1.305e-01
## centered_trial:axis.latVsag:PP.mixVego:config.parVego -2.790e-01
##                                                       Std. Error
## (Intercept)                                            3.813e-02
## centered_trial                                         1.691e-02
## axis.latVsag                                           3.222e-02
## PP.othVego                                             8.610e-02
## PP.mixVego                                             1.250e-01
## config.neiVego                                         3.975e-02
## config.parVego                                         4.350e-02
## centered_trial:axis.latVsag                            3.011e-02
## centered_trial:PP.othVego                              3.822e-02
## centered_trial:PP.mixVego                              5.544e-02
## axis.latVsag:PP.othVego                                7.280e-02
## axis.latVsag:PP.mixVego                                1.057e-01
## centered_trial:config.neiVego                          4.016e-02
## centered_trial:config.parVego                          4.322e-02
## axis.latVsag:config.neiVego                            7.962e-02
## axis.latVsag:config.parVego                            8.713e-02
## PP.othVego:config.neiVego                              8.984e-02
## PP.othVego:config.parVego                              9.835e-02
## PP.mixVego:config.neiVego                              1.306e-01
## PP.mixVego:config.parVego                              1.425e-01
## centered_trial:axis.latVsag:PP.othVego                 6.805e-02
## centered_trial:axis.latVsag:PP.mixVego                 9.871e-02
## centered_trial:axis.latVsag:config.neiVego             8.089e-02
## centered_trial:axis.latVsag:config.parVego             8.695e-02
## centered_trial:PP.othVego:config.neiVego               9.082e-02
## centered_trial:PP.othVego:config.parVego               9.778e-02
## centered_trial:PP.mixVego:config.neiVego               1.317e-01
## centered_trial:PP.mixVego:config.parVego               1.414e-01
## axis.latVsag:PP.othVego:config.neiVego                 1.800e-01
## axis.latVsag:PP.othVego:config.parVego                 1.970e-01
## axis.latVsag:PP.mixVego:config.neiVego                 2.613e-01
## axis.latVsag:PP.mixVego:config.parVego                 2.854e-01
## centered_trial:axis.latVsag:PP.othVego:config.neiVego  1.830e-01
## centered_trial:axis.latVsag:PP.othVego:config.parVego  1.968e-01
## centered_trial:axis.latVsag:PP.mixVego:config.neiVego  2.649e-01
## centered_trial:axis.latVsag:PP.mixVego:config.parVego  2.843e-01
##                                                               df t value
## (Intercept)                                            1.830e+02 111.082
## centered_trial                                         1.870e+02  -1.697
## axis.latVsag                                           1.900e+02   3.656
## PP.othVego                                             1.830e+02   3.612
## PP.mixVego                                             1.830e+02   0.801
## config.neiVego                                         4.506e+03   2.339
## config.parVego                                         4.502e+03   0.501
## centered_trial:axis.latVsag                            4.504e+03   2.204
## centered_trial:PP.othVego                              1.870e+02  -0.343
## centered_trial:PP.mixVego                              1.880e+02  -0.811
## axis.latVsag:PP.othVego                                1.900e+02   0.724
## axis.latVsag:PP.mixVego                                1.910e+02   0.850
## centered_trial:config.neiVego                          4.640e+03   0.231
## centered_trial:config.parVego                          4.602e+03   0.402
## axis.latVsag:config.neiVego                            4.568e+03  -2.408
## axis.latVsag:config.parVego                            4.567e+03   3.480
## PP.othVego:config.neiVego                              4.512e+03   2.658
## PP.othVego:config.parVego                              4.502e+03  -2.293
## PP.mixVego:config.neiVego                              4.502e+03  -0.578
## PP.mixVego:config.parVego                              4.503e+03   1.140
## centered_trial:axis.latVsag:PP.othVego                 4.516e+03  -0.434
## centered_trial:axis.latVsag:PP.mixVego                 4.493e+03   0.775
## centered_trial:axis.latVsag:config.neiVego             4.706e+03  -0.037
## centered_trial:axis.latVsag:config.parVego             4.626e+03  -0.575
## centered_trial:PP.othVego:config.neiVego               4.650e+03   1.169
## centered_trial:PP.othVego:config.parVego               4.607e+03  -0.259
## centered_trial:PP.mixVego:config.neiVego               4.622e+03   1.057
## centered_trial:PP.mixVego:config.parVego               4.593e+03  -0.504
## axis.latVsag:PP.othVego:config.neiVego                 4.576e+03   4.973
## axis.latVsag:PP.othVego:config.parVego                 4.569e+03  -3.022
## axis.latVsag:PP.mixVego:config.neiVego                 4.548e+03  -2.775
## axis.latVsag:PP.mixVego:config.parVego                 4.562e+03   2.185
## centered_trial:axis.latVsag:PP.othVego:config.neiVego  4.708e+03  -1.273
## centered_trial:axis.latVsag:PP.othVego:config.parVego  4.630e+03   1.715
## centered_trial:axis.latVsag:PP.mixVego:config.neiVego  4.698e+03   0.492
## centered_trial:axis.latVsag:PP.mixVego:config.parVego  4.607e+03  -0.981
##                                                       Pr(>|t|)    
## (Intercept)                                            < 2e-16 ***
## centered_trial                                        0.091420 .  
## axis.latVsag                                          0.000331 ***
## PP.othVego                                            0.000392 ***
## PP.mixVego                                            0.424037    
## config.neiVego                                        0.019392 *  
## config.parVego                                        0.616632    
## centered_trial:axis.latVsag                           0.027586 *  
## centered_trial:PP.othVego                             0.732003    
## centered_trial:PP.mixVego                             0.418631    
## axis.latVsag:PP.othVego                               0.469655    
## axis.latVsag:PP.mixVego                               0.396359    
## centered_trial:config.neiVego                         0.817184    
## centered_trial:config.parVego                         0.688040    
## axis.latVsag:config.neiVego                           0.016076 *  
## axis.latVsag:config.parVego                           0.000505 ***
## PP.othVego:config.neiVego                             0.007878 ** 
## PP.othVego:config.parVego                             0.021874 *  
## PP.mixVego:config.neiVego                             0.562998    
## PP.mixVego:config.parVego                             0.254366    
## centered_trial:axis.latVsag:PP.othVego                0.664311    
## centered_trial:axis.latVsag:PP.mixVego                0.438358    
## centered_trial:axis.latVsag:config.neiVego            0.970273    
## centered_trial:axis.latVsag:config.parVego            0.565082    
## centered_trial:PP.othVego:config.neiVego              0.242610    
## centered_trial:PP.othVego:config.parVego              0.795803    
## centered_trial:PP.mixVego:config.neiVego              0.290776    
## centered_trial:PP.mixVego:config.parVego              0.614424    
## axis.latVsag:PP.othVego:config.neiVego                6.83e-07 ***
## axis.latVsag:PP.othVego:config.parVego                0.002524 ** 
## axis.latVsag:PP.mixVego:config.neiVego                0.005538 ** 
## axis.latVsag:PP.mixVego:config.parVego                0.028921 *  
## centered_trial:axis.latVsag:PP.othVego:config.neiVego 0.202973    
## centered_trial:axis.latVsag:PP.othVego:config.parVego 0.086323 .  
## centered_trial:axis.latVsag:PP.mixVego:config.neiVego 0.622421    
## centered_trial:axis.latVsag:PP.mixVego:config.parVego 0.326471    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```
## 
## Correlation matrix not shown by default, as p = 36 > 12.
## Use print(x, correlation=TRUE)  or
## 	 vcov(x)	 if you need it
```

```r
#print('Distance:'); pander(print_stats(DistanceModel.time))


## Directional shifts ##

xFlip.time <- lmer( xFlipDV ~ centered_trial * axis.latVsag * (PP.othVego + PP.mixVego) * (config.neiVego + config.parVego)
                    + (1 | fl)   
                    + (0 + centered_trial | fl), 
                    #+ (0 + (config.neiVego + config.parVego) | fl) 
                    #+ (0 + axis.latVsag | fl),
                    data=resAll.cont, 
                    #family = "poisson", 
                    REML=FALSE )
summary(xFlip.time)
```

```
## Linear mixed model fit by maximum likelihood t-tests use Satterthwaite
##   approximations to degrees of freedom [lmerMod]
## Formula: 
## xFlipDV ~ centered_trial * axis.latVsag * (PP.othVego + PP.mixVego) *  
##     (config.neiVego + config.parVego) + (1 | fl) + (0 + centered_trial |  
##     fl)
##    Data: resAll.cont
## 
##      AIC      BIC   logLik deviance df.resid 
##  17323.0  17577.1  -8622.5  17245.0     4950 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.1779 -0.6691 -0.1386  0.5171  7.8445 
## 
## Random effects:
##  Groups   Name           Variance Std.Dev.
##  fl       (Intercept)    0.41709  0.6458  
##  fl.1     centered_trial 0.02737  0.1654  
##  Residual                1.69977  1.3038  
## Number of obs: 4989, groups:  fl, 184
## 
## Fixed effects:
##                                                         Estimate
## (Intercept)                                            2.013e+00
## centered_trial                                        -3.662e-03
## axis.latVsag                                          -4.993e-02
## PP.othVego                                             4.862e-01
## PP.mixVego                                            -6.245e-04
## config.neiVego                                         5.858e-02
## config.parVego                                         1.238e-02
## centered_trial:axis.latVsag                           -2.986e-02
## centered_trial:PP.othVego                             -8.642e-02
## centered_trial:PP.mixVego                              5.015e-02
## axis.latVsag:PP.othVego                                4.373e-01
## axis.latVsag:PP.mixVego                               -4.849e-02
## centered_trial:config.neiVego                         -1.116e-02
## centered_trial:config.parVego                         -2.352e-02
## axis.latVsag:config.neiVego                            1.018e-02
## axis.latVsag:config.parVego                           -7.209e-03
## PP.othVego:config.neiVego                              5.310e-01
## PP.othVego:config.parVego                             -5.475e-01
## PP.mixVego:config.neiVego                              1.277e-01
## PP.mixVego:config.parVego                              7.297e-02
## centered_trial:axis.latVsag:PP.othVego                -1.291e-01
## centered_trial:axis.latVsag:PP.mixVego                 1.594e-01
## centered_trial:axis.latVsag:config.neiVego             3.332e-01
## centered_trial:axis.latVsag:config.parVego            -3.408e-01
## centered_trial:PP.othVego:config.neiVego               8.677e-02
## centered_trial:PP.othVego:config.parVego               1.216e-01
## centered_trial:PP.mixVego:config.neiVego               2.259e-01
## centered_trial:PP.mixVego:config.parVego               2.140e-01
## axis.latVsag:PP.othVego:config.neiVego                 8.620e-01
## axis.latVsag:PP.othVego:config.parVego                -6.925e-01
## axis.latVsag:PP.mixVego:config.neiVego                -7.376e-01
## axis.latVsag:PP.mixVego:config.parVego                 4.856e-01
## centered_trial:axis.latVsag:PP.othVego:config.neiVego -2.864e-01
## centered_trial:axis.latVsag:PP.othVego:config.parVego  3.073e-01
## centered_trial:axis.latVsag:PP.mixVego:config.neiVego  1.198e-01
## centered_trial:axis.latVsag:PP.mixVego:config.parVego -3.807e-01
##                                                       Std. Error
## (Intercept)                                            6.739e-02
## centered_trial                                         2.958e-02
## axis.latVsag                                           4.945e-02
## PP.othVego                                             1.522e-01
## PP.mixVego                                             2.209e-01
## config.neiVego                                         6.559e-02
## config.parVego                                         7.178e-02
## centered_trial:axis.latVsag                            4.967e-02
## centered_trial:PP.othVego                              6.685e-02
## centered_trial:PP.mixVego                              9.695e-02
## axis.latVsag:PP.othVego                                1.117e-01
## axis.latVsag:PP.mixVego                                1.623e-01
## centered_trial:config.neiVego                          6.617e-02
## centered_trial:config.parVego                          7.119e-02
## axis.latVsag:config.neiVego                            1.314e-01
## axis.latVsag:config.parVego                            1.438e-01
## PP.othVego:config.neiVego                              1.482e-01
## PP.othVego:config.parVego                              1.623e-01
## PP.mixVego:config.neiVego                              2.154e-01
## PP.mixVego:config.parVego                              2.352e-01
## centered_trial:axis.latVsag:PP.othVego                 1.122e-01
## centered_trial:axis.latVsag:PP.mixVego                 1.628e-01
## centered_trial:axis.latVsag:config.neiVego             1.336e-01
## centered_trial:axis.latVsag:config.parVego             1.435e-01
## centered_trial:PP.othVego:config.neiVego               1.496e-01
## centered_trial:PP.othVego:config.parVego               1.611e-01
## centered_trial:PP.mixVego:config.neiVego               2.171e-01
## centered_trial:PP.mixVego:config.parVego               2.330e-01
## axis.latVsag:PP.othVego:config.neiVego                 2.971e-01
## axis.latVsag:PP.othVego:config.parVego                 3.252e-01
## axis.latVsag:PP.mixVego:config.neiVego                 4.313e-01
## axis.latVsag:PP.mixVego:config.parVego                 4.712e-01
## centered_trial:axis.latVsag:PP.othVego:config.neiVego  3.020e-01
## centered_trial:axis.latVsag:PP.othVego:config.parVego  3.247e-01
## centered_trial:axis.latVsag:PP.mixVego:config.neiVego  4.374e-01
## centered_trial:axis.latVsag:PP.mixVego:config.parVego  4.691e-01
##                                                               df t value
## (Intercept)                                            1.860e+02  29.877
## centered_trial                                         1.920e+02  -0.124
## axis.latVsag                                           4.639e+03  -1.010
## PP.othVego                                             1.860e+02   3.195
## PP.mixVego                                             1.860e+02  -0.003
## config.neiVego                                         4.684e+03   0.893
## config.parVego                                         4.684e+03   0.172
## centered_trial:axis.latVsag                            4.668e+03  -0.601
## centered_trial:PP.othVego                              1.920e+02  -1.293
## centered_trial:PP.mixVego                              1.930e+02   0.517
## axis.latVsag:PP.othVego                                4.640e+03   3.914
## axis.latVsag:PP.mixVego                                4.636e+03  -0.299
## centered_trial:config.neiVego                          4.685e+03  -0.169
## centered_trial:config.parVego                          4.658e+03  -0.330
## axis.latVsag:config.neiVego                            4.744e+03   0.077
## axis.latVsag:config.parVego                            4.745e+03  -0.050
## PP.othVego:config.neiVego                              4.688e+03   3.582
## PP.othVego:config.parVego                              4.683e+03  -3.374
## PP.mixVego:config.neiVego                              4.681e+03   0.593
## PP.mixVego:config.parVego                              4.684e+03   0.310
## centered_trial:axis.latVsag:PP.othVego                 4.673e+03  -1.150
## centered_trial:axis.latVsag:PP.mixVego                 4.659e+03   0.979
## centered_trial:axis.latVsag:config.neiVego             4.808e+03   2.495
## centered_trial:axis.latVsag:config.parVego             4.745e+03  -2.375
## centered_trial:PP.othVego:config.neiVego               4.688e+03   0.580
## centered_trial:PP.othVego:config.parVego               4.661e+03   0.755
## centered_trial:PP.mixVego:config.neiVego               4.687e+03   1.041
## centered_trial:PP.mixVego:config.parVego               4.654e+03   0.918
## axis.latVsag:PP.othVego:config.neiVego                 4.750e+03   2.901
## axis.latVsag:PP.othVego:config.parVego                 4.747e+03  -2.129
## axis.latVsag:PP.mixVego:config.neiVego                 4.723e+03  -1.710
## axis.latVsag:PP.mixVego:config.parVego                 4.740e+03   1.031
## centered_trial:axis.latVsag:PP.othVego:config.neiVego  4.805e+03  -0.948
## centered_trial:axis.latVsag:PP.othVego:config.parVego  4.749e+03   0.946
## centered_trial:axis.latVsag:PP.mixVego:config.neiVego  4.802e+03   0.274
## centered_trial:axis.latVsag:PP.mixVego:config.parVego  4.728e+03  -0.812
##                                                       Pr(>|t|)    
## (Intercept)                                            < 2e-16 ***
## centered_trial                                        0.901583    
## axis.latVsag                                          0.312688    
## PP.othVego                                            0.001643 ** 
## PP.mixVego                                            0.997747    
## config.neiVego                                        0.371859    
## config.parVego                                        0.863111    
## centered_trial:axis.latVsag                           0.547850    
## centered_trial:PP.othVego                             0.197606    
## centered_trial:PP.mixVego                             0.605557    
## axis.latVsag:PP.othVego                                9.2e-05 ***
## axis.latVsag:PP.mixVego                               0.765072    
## centered_trial:config.neiVego                         0.866053    
## centered_trial:config.parVego                         0.741174    
## axis.latVsag:config.neiVego                           0.938269    
## axis.latVsag:config.parVego                           0.960034    
## PP.othVego:config.neiVego                             0.000344 ***
## PP.othVego:config.parVego                             0.000748 ***
## PP.mixVego:config.neiVego                             0.553253    
## PP.mixVego:config.parVego                             0.756373    
## centered_trial:axis.latVsag:PP.othVego                0.250248    
## centered_trial:axis.latVsag:PP.mixVego                0.327530    
## centered_trial:axis.latVsag:config.neiVego            0.012629 *  
## centered_trial:axis.latVsag:config.parVego            0.017568 *  
## centered_trial:PP.othVego:config.neiVego              0.562006    
## centered_trial:PP.othVego:config.parVego              0.450493    
## centered_trial:PP.mixVego:config.neiVego              0.298073    
## centered_trial:PP.mixVego:config.parVego              0.358420    
## axis.latVsag:PP.othVego:config.neiVego                0.003736 ** 
## axis.latVsag:PP.othVego:config.parVego                0.033292 *  
## axis.latVsag:PP.mixVego:config.neiVego                0.087308 .  
## axis.latVsag:PP.mixVego:config.parVego                0.302791    
## centered_trial:axis.latVsag:PP.othVego:config.neiVego 0.343019    
## centered_trial:axis.latVsag:PP.othVego:config.parVego 0.343970    
## centered_trial:axis.latVsag:PP.mixVego:config.neiVego 0.784197    
## centered_trial:axis.latVsag:PP.mixVego:config.parVego 0.417063    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```
## 
## Correlation matrix not shown by default, as p = 36 > 12.
## Use print(x, correlation=TRUE)  or
## 	 vcov(x)	 if you need it
```

```r
#print('Directional Shifts:'); pander(print_stats(xFlip.time))


EgoChoiceModel.time = glmer(egoChoice ~ (config.neiVego + config.parVego) * axis.latVsag * centered_trial
                            + (1 | fl)
                            + (0 + centered_trial | fl)
                            #+ (0 + (config.neiVego + config.parVego) | fl) #did not converge
                            + (0 + axis.latVsag | fl), 
                            data=resAll.cont,
                            family = "binomial",
                            method = "Laplace",
                            nAGQ = 1,
                            REML = FALSE)
```

```
## Warning: Argument 'method' is deprecated. Use the nAGQ argument to specify
## Laplace (nAGQ=1) or adaptive Gauss-Hermite quadrature (nAGQ>1). PQL is no
## longer available.
```

```
## Warning: extra argument(s) 'REML' disregarded
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
## $checkConv, : Model failed to converge with max|grad| = 0.0733708 (tol =
## 0.001, component 1)
```

```r
summary(EgoChoiceModel.time)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: egoChoice ~ (config.neiVego + config.parVego) * axis.latVsag *  
##     centered_trial + (1 | fl) + (0 + centered_trial | fl) + (0 +  
##     axis.latVsag | fl)
##    Data: resAll.cont
## 
##      AIC      BIC   logLik deviance df.resid 
##   2846.3   2944.0  -1408.1   2816.3     4974 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -5.0746 -0.2457 -0.1018  0.1041  5.6361 
## 
## Random effects:
##  Groups Name           Variance Std.Dev.
##  fl     (Intercept)    14.6040  3.8215  
##  fl.1   centered_trial  0.5754  0.7586  
##  fl.2   axis.latVsag    2.9891  1.7289  
## Number of obs: 4989, groups:  fl, 184
## 
## Fixed effects:
##                                             Estimate Std. Error z value
## (Intercept)                                -2.544105   0.322186  -7.896
## config.neiVego                              0.069870   0.156618   0.446
## config.parVego                             -0.232453   0.171475  -1.356
## axis.latVsag                                1.092992   0.208395   5.245
## centered_trial                             -0.203363   0.092185  -2.206
## config.neiVego:axis.latVsag                 1.329521   0.315612   4.213
## config.parVego:axis.latVsag                -0.937621   0.344211  -2.724
## config.neiVego:centered_trial               0.261011   0.158125   1.651
## config.parVego:centered_trial              -0.353173   0.170156  -2.076
## axis.latVsag:centered_trial                 0.146167   0.124711   1.172
## config.neiVego:axis.latVsag:centered_trial -0.009859   0.319629  -0.031
## config.parVego:axis.latVsag:centered_trial  0.251908   0.342644   0.735
##                                            Pr(>|z|)    
## (Intercept)                                2.87e-15 ***
## config.neiVego                              0.65551    
## config.parVego                              0.17522    
## axis.latVsag                               1.56e-07 ***
## centered_trial                              0.02738 *  
## config.neiVego:axis.latVsag                2.53e-05 ***
## config.parVego:axis.latVsag                 0.00645 ** 
## config.neiVego:centered_trial               0.09881 .  
## config.parVego:centered_trial               0.03793 *  
## axis.latVsag:centered_trial                 0.24118    
## config.neiVego:axis.latVsag:centered_trial  0.97539    
## config.parVego:axis.latVsag:centered_trial  0.46223    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##              (Intr) cnfg.nV cnfg.pV axs.lV cntrd_ cnfg.nV:.V cnfg.pV:.V
## config.neVg  -0.036                                                    
## config.prVg   0.029 -0.466                                             
## axis.latVsg  -0.077 -0.032   0.012                                     
## centerd_trl   0.027 -0.024   0.034  -0.027                             
## cnfg.nVg:.V  -0.031 -0.136   0.051  -0.070 -0.021                      
## cnfg.prV:.V   0.017  0.056  -0.067   0.036  0.000 -0.472               
## cnfg.nVg:c_  -0.013  0.057  -0.052  -0.003 -0.106 -0.026      0.022    
## cnfg.prVg:_   0.021 -0.050   0.095  -0.020  0.045  0.015     -0.010    
## axs.ltVsg:_  -0.020 -0.021  -0.010   0.067 -0.031 -0.014      0.039    
## cnfg.nV:.V:_ -0.009 -0.026   0.018  -0.014 -0.061  0.057     -0.048    
## cnfg.pV:.V:_ -0.002  0.017  -0.012   0.028  0.071 -0.044      0.088    
##              cnfg.nV:_ cnfg.pV:_ ax.V:_ cnfg.nV:.V:_
## config.neVg                                         
## config.prVg                                         
## axis.latVsg                                         
## centerd_trl                                         
## cnfg.nVg:.V                                         
## cnfg.prV:.V                                         
## cnfg.nVg:c_                                         
## cnfg.prVg:_  -0.455                                 
## axs.ltVsg:_  -0.079     0.092                       
## cnfg.nV:.V:_ -0.104    -0.004    -0.127             
## cnfg.pV:.V:_  0.003     0.041     0.047 -0.459      
## convergence code: 0
## Model failed to converge with max|grad| = 0.0733708 (tol = 0.001, component 1)
```

## Follow-up analyses excluding mixed responders (reported in Appendix B)


```r
resAll_NoMixed = subset(resAll, resAll$perspectivePreference %in% c('ego','other'))


resAll_NoMixed <- within(resAll_NoMixed, {
  
  # Factor 1: instruction orientation 
  axis.latVsag <- ifelse( axis=="lateral", 1/2, 
                          ifelse( axis=="sagittal", -1/2, NA ) ) 
  
  # Factor 2: Perspective preference
  PP.othVego <- ifelse( perspectivePreference=="other", 1/2,
                                ifelse( perspectivePreference=="ego", -1/2, NA ))
  
  # Factor 3: Configuation of folders
  config.neiVego <- ifelse( config=="neither", 1/2,
                            ifelse( config=="partner", 0,
                                    ifelse( config=="ego", -1/2, NA )))
  
  config.parVego <- ifelse( config=="neither", 0,
                            ifelse( config=="partner", 1/2,
                                    ifelse( config=="ego", -1/2, NA )))
  
} )


####
## STEP 2: Build the omnibus model with all effects of interest for research questions
####

## Response time ##

RTModel_NoMixed.omni <- lmer( log(RTDV) ~ (1 | fl) 
                              + (0 + (config.neiVego + config.parVego) | fl) 
                              + (0 + axis.latVsag | fl) 
                              + axis.latVsag * PP.othVego * (config.neiVego + config.parVego),        
                              data=resAll_NoMixed, REML=FALSE )
summary(RTModel_NoMixed.omni)
```

```
## Linear mixed model fit by maximum likelihood t-tests use Satterthwaite
##   approximations to degrees of freedom [lmerMod]
## Formula: log(RTDV) ~ (1 | fl) + (0 + (config.neiVego + config.parVego) |  
##     fl) + (0 + axis.latVsag | fl) + axis.latVsag * PP.othVego *  
##     (config.neiVego + config.parVego)
##    Data: resAll_NoMixed
## 
##      AIC      BIC   logLik deviance df.resid 
##   3870.7   3985.8  -1917.3   3834.7     4400 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.8558 -0.6404 -0.1364  0.4635  4.8367 
## 
## Random effects:
##  Groups   Name           Variance  Std.Dev.  Corr
##  fl       (Intercept)    4.834e-02 2.199e-01     
##  fl.1     config.neiVego 0.000e+00 0.000e+00     
##           config.parVego 2.166e-16 1.472e-08  NaN
##  fl.2     axis.latVsag   1.084e-02 1.041e-01     
##  Residual                1.253e-01 3.540e-01     
## Number of obs: 4418, groups:  fl, 163
## 
## Fixed effects:
##                                          Estimate Std. Error         df
## (Intercept)                               7.20678    0.02221  163.00000
## axis.latVsag                              0.11298    0.01665  164.00000
## PP.othVego                                0.23343    0.04443  163.00000
## config.neiVego                            0.05656    0.01758 4095.00000
## config.parVego                           -0.03158    0.01933 4090.00000
## axis.latVsag:PP.othVego                   0.17336    0.03331  164.00000
## axis.latVsag:config.neiVego               0.06867    0.03516 4091.00000
## axis.latVsag:config.parVego              -0.02775    0.03865 4091.00000
## PP.othVego:config.neiVego                 0.16403    0.03515 4095.00000
## PP.othVego:config.parVego                -0.13107    0.03866 4090.00000
## axis.latVsag:PP.othVego:config.neiVego    0.33393    0.07032 4091.00000
## axis.latVsag:PP.othVego:config.parVego   -0.10554    0.07731 4091.00000
##                                        t value Pr(>|t|)    
## (Intercept)                            324.442  < 2e-16 ***
## axis.latVsag                             6.784 2.04e-10 ***
## PP.othVego                               5.254 4.60e-07 ***
## config.neiVego                           3.218 0.001301 ** 
## config.parVego                          -1.634 0.102328    
## axis.latVsag:PP.othVego                  5.204 5.76e-07 ***
## axis.latVsag:config.neiVego              1.953 0.050893 .  
## axis.latVsag:config.parVego             -0.718 0.472846    
## PP.othVego:config.neiVego                4.666 3.16e-06 ***
## PP.othVego:config.parVego               -3.391 0.000704 ***
## axis.latVsag:PP.othVego:config.neiVego   4.749 2.12e-06 ***
## axis.latVsag:PP.othVego:config.parVego  -1.365 0.172283    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##                           (Intr) axs.lV PP.thV cnfg.nV cnfg.pV ax.V:PP.V
## axis.latVsg               -0.001                                        
## PP.othVego                -0.582  0.002                                 
## config.neVg               -0.057  0.005  0.034                          
## config.prVg                0.024  0.000 -0.014 -0.450                   
## axs.lV:PP.V                0.002 -0.579 -0.001 -0.003  -0.002           
## axs.ltVsg:cnfg.nV          0.002 -0.151 -0.001 -0.001  -0.001   0.090   
## axs.ltVsg:cnfg.pV          0.000  0.064 -0.001 -0.002  -0.004  -0.037   
## PP.thVg:cnfg.nV            0.034 -0.003 -0.057 -0.575   0.257   0.005   
## PP.thVg:cnfg.pV           -0.014 -0.002  0.024  0.257  -0.578   0.000   
## axs.ltVsg:PP.thVg:cnfg.nV -0.001  0.090  0.002  0.006   0.000  -0.151   
## axs.ltVsg:PP.thVg:cnfg.pV -0.001 -0.037  0.000  0.000   0.005   0.064   
##                           axs.ltVsg:cnfg.nV axs.ltVsg:cnfg.pV
## axis.latVsg                                                  
## PP.othVego                                                   
## config.neVg                                                  
## config.prVg                                                  
## axs.lV:PP.V                                                  
## axs.ltVsg:cnfg.nV                                            
## axs.ltVsg:cnfg.pV         -0.450                             
## PP.thVg:cnfg.nV            0.006             0.000           
## PP.thVg:cnfg.pV            0.000             0.005           
## axs.ltVsg:PP.thVg:cnfg.nV -0.575             0.257           
## axs.ltVsg:PP.thVg:cnfg.pV  0.257            -0.578           
##                           PP.thVg:cnfg.nV PP.thVg:cnfg.pV
## axis.latVsg                                              
## PP.othVego                                               
## config.neVg                                              
## config.prVg                                              
## axs.lV:PP.V                                              
## axs.ltVsg:cnfg.nV                                        
## axs.ltVsg:cnfg.pV                                        
## PP.thVg:cnfg.nV                                          
## PP.thVg:cnfg.pV           -0.450                         
## axs.ltVsg:PP.thVg:cnfg.nV -0.001          -0.001         
## axs.ltVsg:PP.thVg:cnfg.pV -0.002          -0.004         
##                           axs.ltVsg:PP.thVg:cnfg.nV
## axis.latVsg                                        
## PP.othVego                                         
## config.neVg                                        
## config.prVg                                        
## axs.lV:PP.V                                        
## axs.ltVsg:cnfg.nV                                  
## axs.ltVsg:cnfg.pV                                  
## PP.thVg:cnfg.nV                                    
## PP.thVg:cnfg.pV                                    
## axs.ltVsg:PP.thVg:cnfg.nV                          
## axs.ltVsg:PP.thVg:cnfg.pV -0.450
```

```r
#print('RT:'); pander(print_stats(RTModel_NoMixed.omni))


## Divergence from Optimal Distance ##

DistanceModel_NoMixed.omni <- lmer( log(divergDist) ~ (1 | fl) 
                                    + (0 + (config.neiVego + config.parVego) | fl) 
                                    + (0 + axis.latVsag | fl) 
                                    + axis.latVsag * PP.othVego * (config.neiVego + config.parVego),        
                                    data=resAll_NoMixed, REML=FALSE )
summary(DistanceModel_NoMixed.omni)
```

```
## Linear mixed model fit by maximum likelihood t-tests use Satterthwaite
##   approximations to degrees of freedom [lmerMod]
## Formula: 
## log(divergDist) ~ (1 | fl) + (0 + (config.neiVego + config.parVego) |  
##     fl) + (0 + axis.latVsag | fl) + axis.latVsag * PP.othVego *  
##     (config.neiVego + config.parVego)
##    Data: resAll_NoMixed
## 
##      AIC      BIC   logLik deviance df.resid 
##  10790.8  10905.9  -5377.4  10754.8     4400 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -4.8494 -0.6986 -0.0742  0.7014  3.1852 
## 
## Random effects:
##  Groups   Name           Variance  Std.Dev.  Corr
##  fl       (Intercept)    1.395e-01 3.735e-01     
##  fl.1     config.neiVego 0.000e+00 0.000e+00     
##           config.parVego 7.756e-13 8.807e-07  NaN
##  fl.2     axis.latVsag   1.291e-02 1.136e-01     
##  Residual                6.183e-01 7.863e-01     
## Number of obs: 4418, groups:  fl, 163
## 
## Fixed effects:
##                                          Estimate Std. Error         df
## (Intercept)                               4.20925    0.03892  162.00000
## axis.latVsag                              0.09443    0.03148  166.00000
## PP.othVego                                0.36177    0.07783  162.00000
## config.neiVego                            0.11379    0.03903 4096.00000
## config.parVego                           -0.01955    0.04293 4089.00000
## axis.latVsag:PP.othVego                   0.09927    0.06296  166.00000
## axis.latVsag:config.neiVego              -0.01641    0.07809 4091.00000
## axis.latVsag:config.parVego               0.15560    0.08584 4090.00000
## PP.othVego:config.neiVego                 0.19804    0.07806 4096.00000
## PP.othVego:config.parVego                -0.15280    0.08585 4089.00000
## axis.latVsag:PP.othVego:config.neiVego    0.53433    0.15617 4091.00000
## axis.latVsag:PP.othVego:config.parVego   -0.27819    0.17169 4090.00000
##                                        t value Pr(>|t|)    
## (Intercept)                            108.158  < 2e-16 ***
## axis.latVsag                             3.000 0.003120 ** 
## PP.othVego                               4.648 6.92e-06 ***
## config.neiVego                           2.916 0.003570 ** 
## config.parVego                          -0.455 0.648823    
## axis.latVsag:PP.othVego                  1.577 0.116754    
## axis.latVsag:config.neiVego             -0.210 0.833569    
## axis.latVsag:config.parVego              1.813 0.069970 .  
## PP.othVego:config.neiVego                2.537 0.011214 *  
## PP.othVego:config.parVego               -1.780 0.075185 .  
## axis.latVsag:PP.othVego:config.neiVego   3.421 0.000629 ***
## axis.latVsag:PP.othVego:config.parVego  -1.620 0.105238    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##                           (Intr) axs.lV PP.thV cnfg.nV cnfg.pV ax.V:PP.V
## axis.latVsg               -0.002                                        
## PP.othVego                -0.582  0.002                                 
## config.neVg               -0.072  0.005  0.043                          
## config.prVg                0.030  0.000 -0.018 -0.450                   
## axs.lV:PP.V                0.002 -0.578 -0.002 -0.003  -0.002           
## axs.ltVsg:cnfg.nV          0.002 -0.177 -0.002 -0.001  -0.001   0.106   
## axs.ltVsg:cnfg.pV          0.000  0.075 -0.001 -0.002  -0.004  -0.043   
## PP.thVg:cnfg.nV            0.043 -0.003 -0.072 -0.575   0.257   0.005   
## PP.thVg:cnfg.pV           -0.018 -0.002  0.030  0.257  -0.578   0.000   
## axs.ltVsg:PP.thVg:cnfg.nV -0.002  0.106  0.002  0.006   0.000  -0.177   
## axs.ltVsg:PP.thVg:cnfg.pV -0.001 -0.043  0.000  0.001   0.005   0.075   
##                           axs.ltVsg:cnfg.nV axs.ltVsg:cnfg.pV
## axis.latVsg                                                  
## PP.othVego                                                   
## config.neVg                                                  
## config.prVg                                                  
## axs.lV:PP.V                                                  
## axs.ltVsg:cnfg.nV                                            
## axs.ltVsg:cnfg.pV         -0.450                             
## PP.thVg:cnfg.nV            0.006             0.001           
## PP.thVg:cnfg.pV            0.000             0.005           
## axs.ltVsg:PP.thVg:cnfg.nV -0.575             0.257           
## axs.ltVsg:PP.thVg:cnfg.pV  0.257            -0.578           
##                           PP.thVg:cnfg.nV PP.thVg:cnfg.pV
## axis.latVsg                                              
## PP.othVego                                               
## config.neVg                                              
## config.prVg                                              
## axs.lV:PP.V                                              
## axs.ltVsg:cnfg.nV                                        
## axs.ltVsg:cnfg.pV                                        
## PP.thVg:cnfg.nV                                          
## PP.thVg:cnfg.pV           -0.450                         
## axs.ltVsg:PP.thVg:cnfg.nV -0.001          -0.001         
## axs.ltVsg:PP.thVg:cnfg.pV -0.002          -0.004         
##                           axs.ltVsg:PP.thVg:cnfg.nV
## axis.latVsg                                        
## PP.othVego                                         
## config.neVg                                        
## config.prVg                                        
## axs.lV:PP.V                                        
## axs.ltVsg:cnfg.nV                                  
## axs.ltVsg:cnfg.pV                                  
## PP.thVg:cnfg.nV                                    
## PP.thVg:cnfg.pV                                    
## axs.ltVsg:PP.thVg:cnfg.nV                          
## axs.ltVsg:PP.thVg:cnfg.pV -0.450
```

```r
#print('Distance:'); pander(print_stats(DistanceModel_NoMixed.omni))


## Directional shifts ##

xFlip_NoMixed.omni <- lmer( xFlipDV ~ axis.latVsag * PP.othVego * (config.neiVego + config.parVego)
                    + (1 | fl)    
                    + (0 + (config.neiVego + config.parVego) | fl) 
                    + (0 + axis.latVsag | fl),
                    data=resAll_NoMixed, 
                    #family = "poisson", 
                    REML=FALSE )
summary(xFlip_NoMixed.omni)
```

```
## Linear mixed model fit by maximum likelihood t-tests use Satterthwaite
##   approximations to degrees of freedom [lmerMod]
## Formula: 
## xFlipDV ~ axis.latVsag * PP.othVego * (config.neiVego + config.parVego) +  
##     (1 | fl) + (0 + (config.neiVego + config.parVego) | fl) +  
##     (0 + axis.latVsag | fl)
##    Data: resAll_NoMixed
## 
##      AIC      BIC   logLik deviance df.resid 
##  15392.6  15507.7  -7678.3  15356.6     4400 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.6580 -0.6687 -0.1382  0.5095  8.1401 
## 
## Random effects:
##  Groups   Name           Variance Std.Dev. Corr 
##  fl       (Intercept)    0.420493 0.64845       
##  fl.1     config.neiVego 0.005612 0.07491       
##           config.parVego 0.026067 0.16145  -1.00
##  fl.2     axis.latVsag   0.094520 0.30744       
##  Residual                1.733568 1.31665       
## Number of obs: 4418, groups:  fl, 163
## 
## Fixed effects:
##                                          Estimate Std. Error         df
## (Intercept)                               2.01251    0.06722  164.00000
## axis.latVsag                             -0.03873    0.05763  166.00000
## PP.othVego                                0.49357    0.13445  164.00000
## config.neiVego                            0.03193    0.06576  751.00000
## config.parVego                           -0.00156    0.07355  163.00000
## axis.latVsag:PP.othVego                   0.41665    0.11526  166.00000
## axis.latVsag:config.neiVego               0.18030    0.13076 3939.00000
## axis.latVsag:config.parVego              -0.12337    0.14375 3940.00000
## PP.othVego:config.neiVego                 0.57502    0.13151  751.00000
## PP.othVego:config.parVego                -0.51340    0.14710  163.00000
## axis.latVsag:PP.othVego:config.neiVego    0.54243    0.26151 3939.00000
## axis.latVsag:PP.othVego:config.parVego   -0.47070    0.28750 3940.00000
##                                        t value Pr(>|t|)    
## (Intercept)                             29.938  < 2e-16 ***
## axis.latVsag                            -0.672 0.502491    
## PP.othVego                               3.671 0.000326 ***
## config.neiVego                           0.486 0.627457    
## config.parVego                          -0.021 0.983108    
## axis.latVsag:PP.othVego                  3.615 0.000398 ***
## axis.latVsag:config.neiVego              1.379 0.168002    
## axis.latVsag:config.parVego             -0.858 0.390810    
## PP.othVego:config.neiVego                4.372  1.4e-05 ***
## PP.othVego:config.parVego               -3.490 0.000621 ***
## axis.latVsag:PP.othVego:config.neiVego   2.074 0.038125 *  
## axis.latVsag:PP.othVego:config.parVego  -1.637 0.101662    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##                           (Intr) axs.lV PP.thV cnfg.nV cnfg.pV ax.V:PP.V
## axis.latVsg               -0.002                                        
## PP.othVego                -0.582  0.002                                 
## config.neVg               -0.069  0.005  0.041                          
## config.prVg                0.029  0.000 -0.017 -0.460                   
## axs.lV:PP.V                0.002 -0.579 -0.002 -0.003  -0.002           
## axs.ltVsg:cnfg.nV          0.002 -0.162 -0.001 -0.001  -0.001   0.097   
## axs.ltVsg:cnfg.pV          0.000  0.068 -0.001 -0.002  -0.004  -0.039   
## PP.thVg:cnfg.nV            0.041 -0.003 -0.069 -0.575   0.264   0.005   
## PP.thVg:cnfg.pV           -0.017 -0.002  0.029  0.264  -0.578   0.000   
## axs.ltVsg:PP.thVg:cnfg.nV -0.001  0.097  0.002  0.006   0.000  -0.162   
## axs.ltVsg:PP.thVg:cnfg.pV -0.001 -0.039  0.000  0.000   0.005   0.068   
##                           axs.ltVsg:cnfg.nV axs.ltVsg:cnfg.pV
## axis.latVsg                                                  
## PP.othVego                                                   
## config.neVg                                                  
## config.prVg                                                  
## axs.lV:PP.V                                                  
## axs.ltVsg:cnfg.nV                                            
## axs.ltVsg:cnfg.pV         -0.450                             
## PP.thVg:cnfg.nV            0.006             0.000           
## PP.thVg:cnfg.pV            0.000             0.005           
## axs.ltVsg:PP.thVg:cnfg.nV -0.575             0.257           
## axs.ltVsg:PP.thVg:cnfg.pV  0.257            -0.578           
##                           PP.thVg:cnfg.nV PP.thVg:cnfg.pV
## axis.latVsg                                              
## PP.othVego                                               
## config.neVg                                              
## config.prVg                                              
## axs.lV:PP.V                                              
## axs.ltVsg:cnfg.nV                                        
## axs.ltVsg:cnfg.pV                                        
## PP.thVg:cnfg.nV                                          
## PP.thVg:cnfg.pV           -0.460                         
## axs.ltVsg:PP.thVg:cnfg.nV -0.001          -0.001         
## axs.ltVsg:PP.thVg:cnfg.pV -0.002          -0.004         
##                           axs.ltVsg:PP.thVg:cnfg.nV
## axis.latVsg                                        
## PP.othVego                                         
## config.neVg                                        
## config.prVg                                        
## axs.lV:PP.V                                        
## axs.ltVsg:cnfg.nV                                  
## axs.ltVsg:cnfg.pV                                  
## PP.thVg:cnfg.nV                                    
## PP.thVg:cnfg.pV                                    
## axs.ltVsg:PP.thVg:cnfg.nV                          
## axs.ltVsg:PP.thVg:cnfg.pV -0.450
```

```r
#print('Directional Shifts:'); pander(print_stats(xFlip_NoMixed.omni))


## Proportion of Egocentric responses ##

EgoChoiceModel_NoMixed.omni = glmer(egoChoice ~ (config.neiVego + config.parVego) * axis.latVsag
                            + (1 | fl)
                            #+ (0 + (config.neiVego + config.parVego) | fl) #did not converge
                            + (0 + axis.latVsag | fl), 
                            data=resAll_NoMixed,
                            family = "binomial",
                            method = "Laplace",
                            nAGQ = 1,
                            REML = FALSE)
```

```
## Warning: Argument 'method' is deprecated. Use the nAGQ argument to specify
## Laplace (nAGQ=1) or adaptive Gauss-Hermite quadrature (nAGQ>1). PQL is no
## longer available.
```

```
## Warning: extra argument(s) 'REML' disregarded
```

```r
summary(EgoChoiceModel_NoMixed.omni)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: egoChoice ~ (config.neiVego + config.parVego) * axis.latVsag +  
##     (1 | fl) + (0 + axis.latVsag | fl)
##    Data: resAll_NoMixed
## 
##      AIC      BIC   logLik deviance df.resid 
##   2192.7   2243.9  -1088.4   2176.7     4410 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -5.7675 -0.2586 -0.1128 -0.0622  5.9107 
## 
## Random effects:
##  Groups Name         Variance Std.Dev.
##  fl     (Intercept)  13.3007  3.6470  
##  fl.1   axis.latVsag  0.6961  0.8343  
## Number of obs: 4418, groups:  fl, 163
## 
## Fixed effects:
##                             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                  -2.6113     0.3288  -7.942 2.00e-15 ***
## config.neiVego                0.3151     0.1728   1.823  0.06829 .  
## config.parVego               -0.2162     0.1936  -1.116  0.26423    
## axis.latVsag                  0.6543     0.1663   3.935 8.32e-05 ***
## config.neiVego:axis.latVsag   1.1228     0.3472   3.234  0.00122 ** 
## config.parVego:axis.latVsag  -0.9445     0.3883  -2.432  0.01500 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) cnfg.nV cnfg.pV axs.lV cnfg.nV:.V
## config.neVg -0.054                                  
## config.prVg  0.027 -0.450                           
## axis.latVsg -0.062 -0.025   0.030                   
## cnfg.nVg:.V -0.027 -0.110   0.027  -0.157           
## cnfg.prV:.V  0.023  0.027  -0.038   0.065 -0.454
```

```r
##// Effect Size

RTModel_NoMixed.omni <- lmer( log(RTDV) ~ (1 + (config.neiVego + config.parVego) + axis.latVsag | fl) 
                              + axis.latVsag * PP.othVego * (config.neiVego + config.parVego),        
                              data=resAll_NoMixed, REML=FALSE )
r.squaredGLMM(RTModel_NoMixed.omni)[2] #we report this
```

```
##       R2c 
## 0.3716846
```

```r
DistanceModel_NoMixed.omni <- lmer( log(divergDist) ~ (1 + (config.neiVego + config.parVego) + axis.latVsag | fl) 
                                      + axis.latVsag * PP.othVego * (config.neiVego + config.parVego),        
                                    data=resAll_NoMixed, REML=FALSE )
r.squaredGLMM(DistanceModel_NoMixed.omni)[2] #we report this
```

```
##       R2c 
## 0.2256816
```

```r
xFlip_NoMixed.omni <- lmer( xFlipDV ~ axis.latVsag * PP.othVego * (config.neiVego + config.parVego)
                            + (1 + (config.neiVego + config.parVego) + axis.latVsag | fl),
                            data=resAll_NoMixed, 
                            #family = "poisson", 
                            REML=FALSE )
r.squaredGLMM(xFlip_NoMixed.omni)[2]
```

```
##       R2c 
## 0.2350782
```

```r
EgoChoiceModel_NoMixed.omni = glmer(egoChoice ~ (config.neiVego + config.parVego) * axis.latVsag
                                    + (1 + axis.latVsag| fl), 
                                    data=resAll_NoMixed,
                                    family = "binomial",
                                    method = "Laplace",
                                    nAGQ = 1,
                                    REML = FALSE)
```

```
## Warning: Argument 'method' is deprecated. Use the nAGQ argument to specify
## Laplace (nAGQ=1) or adaptive Gauss-Hermite quadrature (nAGQ>1). PQL is no
## longer available.

## Warning: extra argument(s) 'REML' disregarded
```

```r
r.squaredGLMM(EgoChoiceModel_NoMixed.omni)[2]
```

```
## Warning: Argument 'method' is deprecated. Use the nAGQ argument to specify
## Laplace (nAGQ=1) or adaptive Gauss-Hermite quadrature (nAGQ>1). PQL is no
## longer available.

## Warning: extra argument(s) 'REML' disregarded
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
## $checkConv, : Model failed to converge with max|grad| = 0.132233 (tol =
## 0.001, component 1)
```

```
## The result is correct only if all data used by the model has not changed since model was fitted.
```

```
##       R2c 
## 0.8121972
```

## LMERS FOR CONTROL TRIALS (reported in Appendix A) 
Preliminaries


```r
resAllError = as.data.frame(as.matrix(resAllError))
#Defining as factors in order to set reference categories next
resAllError$config = as.factor(as.matrix(resAllError$config))
resAllError$offset = as.factor(as.matrix(resAllError$offset))
resAllError$axis = as.factor(as.matrix(resAllError$axis))
resAllError$perspectivePreference = as.factor(as.matrix(resAllError$perspectivePreference))


##Check for any "holes" in the design
with(resAllError, table(config, offset, axis))
```

```
## , , axis = lateral
## 
##          offset
## config      0  90
##   both    692   0
##   ego       0 353
##   neither   0 359
##   partner   0 354
## 
## , , axis = sagittal
## 
##          offset
## config      0  90
##   both    716   0
##   ego       0 355
##   neither   0 359
##   partner   0 356
```

```r
#when offset is 0, configurations can only be both-aligned --> let's not include offset in the models as a factor
with(resAllError, table(config, axis, perspectivePreference))
```

```
## , , perspectivePreference = ego
## 
##          axis
## config    lateral sagittal
##   both        127      134
##   ego          65       65
##   neither      68       67
##   partner      66       66
## 
## , , perspectivePreference = mixed
## 
##          axis
## config    lateral sagittal
##   both         77       81
##   ego          40       40
##   neither      38       39
##   partner      40       41
## 
## , , perspectivePreference = other
## 
##          axis
## config    lateral sagittal
##   both        488      501
##   ego         248      250
##   neither     253      253
##   partner     248      249
```

```r
#Make sure DVs are of the right type
resAllError$RTDV = as.numeric(as.matrix(resAllError$RTDV))
resAllError$divergDist = as.numeric(as.matrix(resAllError$divergDist))
resAllError$xFlipDV = as.integer(as.matrix(resAllError$xFlipDV))
str(resAllError)
```

```
## 'data.frame':	3544 obs. of  19 variables:
##  $ fl                   : Factor w/ 184 levels "1004.txt","1005.txt",..: 1 1 1 1 1 1 1 1 1 1 ...
##   ..- attr(*, "names")= chr  "1" "2" "3" "4" ...
##  $ trial                : Factor w/ 46 levels " 1"," 2"," 3",..: 1 2 3 4 5 8 10 15 16 21 ...
##   ..- attr(*, "names")= chr  "1" "2" "3" "4" ...
##  $ sound                : Factor w/ 4 levels "back","fron",..: 2 4 1 1 3 3 3 2 1 2 ...
##   ..- attr(*, "names")= chr  "1" "2" "3" "4" ...
##  $ config               : Factor w/ 4 levels "both","ego","neither",..: 1 2 1 3 4 1 1 3 1 1 ...
##  $ ppos                 : Factor w/ 3 levels "init_0","init_270",..: 1 3 1 2 3 1 1 3 1 1 ...
##   ..- attr(*, "names")= chr  "1" "2" "3" "4" ...
##  $ RTDV                 : num  3639 783 1699 2499 1350 ...
##  $ AC                   : Factor w/ 1 level "0": 1 1 1 1 1 1 1 1 1 1 ...
##   ..- attr(*, "names")= chr  "1" "2" "3" "4" ...
##  $ optimalDist          : Factor w/ 3518 levels "  6.145934"," 18.258423",..: 1602 1512 3187 2521 506 1568 2938 639 2833 1503 ...
##   ..- attr(*, "names")= chr  "1" "2" "3" "4" ...
##  $ totalDistanceDV      : Factor w/ 3544 levels "  3.158625"," 84.383338",..: 3155 561 2493 2191 742 794 2313 146 2576 456 ...
##   ..- attr(*, "names")= chr  "1" "2" "3" "4" ...
##  $ xFlipDV              : int  4 1 2 2 1 5 1 3 1 1 ...
##  $ err                  : Factor w/ 2 levels " TRUE","FALSE": 2 2 2 2 2 2 2 2 2 2 ...
##   ..- attr(*, "names")= chr  "1" "2" "3" "4" ...
##  $ chosen               : Factor w/ 8 levels "1","2","3","4",..: 4 5 2 3 7 4 2 8 2 5 ...
##   ..- attr(*, "names")= chr  "1" "2" "3" "4" ...
##  $ cor                  : Factor w/ 7 levels "1","2","4","6",..: 2 4 3 5 6 3 2 1 4 2 ...
##   ..- attr(*, "names")= chr  "1" "2" "3" "4" ...
##  $ ego                  : Factor w/ 7 levels "1","2","4","6",..: 2 4 3 5 6 3 2 1 4 2 ...
##   ..- attr(*, "names")= chr  "1" "2" "3" "4" ...
##  $ other                : Factor w/ 7 levels "1","2","4","6",..: 2 4 3 5 6 3 2 1 4 2 ...
##   ..- attr(*, "names")= chr  "1" "2" "3" "4" ...
##  $ perspectivePreference: Factor w/ 3 levels "ego","mixed",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ axis                 : Factor w/ 2 levels "lateral","sagittal": 2 1 2 2 1 1 1 2 2 2 ...
##  $ offset               : Factor w/ 2 levels "0","90": 1 2 1 2 2 1 1 2 1 1 ...
##  $ divergDist           : num  229.5 21.9 63.2 64.2 85.7 ...
```

##Step 1: 
Set up contrast structure


```r
resAllError.cont <- within(resAllError, {
  
  # Factor 1: instruction type 
  axis.latVsag <- ifelse( axis=="lateral", 1/2, 
                          ifelse( axis=="sagittal", -1/2, NA ) ) 
  
  # Factor 2: Perspective preference
  PP.othVego <- ifelse( perspectivePreference=="other", 1/2,
                        ifelse( perspectivePreference=="mixed", 0,
                                ifelse( perspectivePreference=="ego", -1/2, NA )))
  
  PP.mixVego <- ifelse( perspectivePreference=="other", 0,
                        ifelse( perspectivePreference=="mixed", 1/2,
                                ifelse( perspectivePreference=="ego", -1/2, NA )))    
  
  # Factor 3: Configuation of folders
  config.neiVego <- ifelse( config=="neither", 1/2,
                            ifelse( config=="partner", 0,
                                    ifelse( config=="both", 0,
                                      ifelse( config=="ego", -1/2, NA ))))
  
  config.parVego <- ifelse( config=="neither", 0,
                            ifelse( config=="partner", 1/2,
                                    ifelse( config=="both", 0,
                                      ifelse( config=="ego", -1/2, NA ))))
                            
  config.bothVego <- ifelse( config=="neither", 0,
                            ifelse( config=="partner", 0,
                                    ifelse( config=="both", 1/2,
                                      ifelse( config=="ego", -1/2, NA ))))
} )
```

##Step 2: 
Build the omnibus models with all effects of interest for research questions


```r
####Error Models#####

ErrorModel.omni = glmer(err ~ (PP.othVego+PP.mixVego) * (config.neiVego+config.parVego+config.bothVego) * axis.latVsag 
                    + (1 | fl), #does not converge!
                    #+ (0 + (config.neiVego+config.parVego+config.bothVego) | fl) #does not converge
                    #+ (0 + axis.latVsag | fl), #does not converge
                    data=resAllError.cont, 
                    family = "binomial",
                    REML=FALSE)
```

```
## Warning: extra argument(s) 'REML' disregarded
```

```
## Warning in (function (fn, par, lower = rep.int(-Inf, n), upper =
## rep.int(Inf, : failure to converge in 10000 evaluations
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
## $checkConv, : Model failed to converge with max|grad| = 0.185201 (tol =
## 0.001, component 1)
```

```r
summary(ErrorModel.omni)
```

```
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## err ~ (PP.othVego + PP.mixVego) * (config.neiVego + config.parVego +  
##     config.bothVego) * axis.latVsag + (1 | fl)
##    Data: resAllError.cont
## 
##      AIC      BIC   logLik deviance df.resid 
##   1405.4   1559.8   -677.7   1355.4     3519 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -9.4929  0.0929  0.1402  0.1949  1.8828 
## 
## Random effects:
##  Groups Name        Variance Std.Dev.
##  fl     (Intercept) 1.774    1.332   
## Number of obs: 3544, groups:  fl, 184
## 
## Fixed effects:
##                                         Estimate Std. Error z value
## (Intercept)                              3.27968    0.20036  16.369
## PP.othVego                               1.42583    0.41619   3.426
## PP.mixVego                              -2.96363    0.53868  -5.502
## config.neiVego                           0.43333    0.40537   1.069
## config.parVego                           0.07084    0.40354   0.176
## config.bothVego                         -0.16877    0.29651  -0.569
## axis.latVsag                             0.76088    0.21023   3.619
## PP.othVego:config.neiVego                0.77793    0.97748   0.796
## PP.othVego:config.parVego               -1.58126    0.92627  -1.707
## PP.othVego:config.bothVego              -0.27490    0.71726  -0.383
## PP.mixVego:config.neiVego               -1.54721    1.01730  -1.521
## PP.mixVego:config.parVego                0.82512    1.04116   0.793
## PP.mixVego:config.bothVego               0.26282    0.77480   0.339
## PP.othVego:axis.latVsag                 -0.09301    0.50415  -0.184
## PP.mixVego:axis.latVsag                  0.05103    0.54419   0.094
## config.neiVego:axis.latVsag              0.17305    0.81117   0.213
## config.parVego:axis.latVsag              1.04002    0.80775   1.288
## config.bothVego:axis.latVsag             0.02908    0.59372   0.049
## PP.othVego:config.neiVego:axis.latVsag  -2.17056    1.95568  -1.110
## PP.othVego:config.parVego:axis.latVsag  -2.27552    1.85281  -1.228
## PP.othVego:config.bothVego:axis.latVsag  2.43874    1.43656   1.698
## PP.mixVego:config.neiVego:axis.latVsag   0.80243    2.03507   0.394
## PP.mixVego:config.parVego:axis.latVsag   0.67320    2.08455   0.323
## PP.mixVego:config.bothVego:axis.latVsag -0.26087    1.55121  -0.168
##                                         Pr(>|z|)    
## (Intercept)                              < 2e-16 ***
## PP.othVego                              0.000613 ***
## PP.mixVego                              3.76e-08 ***
## config.neiVego                          0.285084    
## config.parVego                          0.860656    
## config.bothVego                         0.569231    
## axis.latVsag                            0.000295 ***
## PP.othVego:config.neiVego               0.426117    
## PP.othVego:config.parVego               0.087798 .  
## PP.othVego:config.bothVego              0.701528    
## PP.mixVego:config.neiVego               0.128285    
## PP.mixVego:config.parVego               0.428068    
## PP.mixVego:config.bothVego              0.734445    
## PP.othVego:axis.latVsag                 0.853627    
## PP.mixVego:axis.latVsag                 0.925286    
## config.neiVego:axis.latVsag             0.831067    
## config.parVego:axis.latVsag             0.197906    
## config.bothVego:axis.latVsag            0.960929    
## PP.othVego:config.neiVego:axis.latVsag  0.267054    
## PP.othVego:config.parVego:axis.latVsag  0.219392    
## PP.othVego:config.bothVego:axis.latVsag 0.089578 .  
## PP.mixVego:config.neiVego:axis.latVsag  0.693359    
## PP.mixVego:config.parVego:axis.latVsag  0.746737    
## PP.mixVego:config.bothVego:axis.latVsag 0.866449    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```
## 
## Correlation matrix not shown by default, as p = 24 > 12.
## Use print(x, correlation=TRUE)  or
## 	 vcov(x)	 if you need it
```

```
## convergence code: 0
## Model failed to converge with max|grad| = 0.185201 (tol = 0.001, component 1)
## failure to converge in 10000 evaluations
```

```r
####RT Models#####

RTControl.omni = lmer(log(RTDV) ~ (PP.othVego+PP.mixVego) * (config.neiVego+config.parVego+config.bothVego) * axis.latVsag 
                  + (1 | fl) 
                  + (0 + (config.neiVego+config.parVego+config.bothVego) | fl)
                  + (0 + axis.latVsag | fl),
                  data=resAllError.cont,  
                  REML=FALSE)
#print('RT:'); pander(print_stats(RTControl.omni))
summary(RTControl.omni)
```

```
## Linear mixed model fit by maximum likelihood t-tests use Satterthwaite
##   approximations to degrees of freedom [lmerMod]
## Formula: 
## log(RTDV) ~ (PP.othVego + PP.mixVego) * (config.neiVego + config.parVego +  
##     config.bothVego) * axis.latVsag + (1 | fl) + (0 + (config.neiVego +  
##     config.parVego + config.bothVego) | fl) + (0 + axis.latVsag |      fl)
##    Data: resAllError.cont
## 
##      AIC      BIC   logLik deviance df.resid 
##   4121.6   4325.3  -2027.8   4055.6     3511 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.8648 -0.6746 -0.1505  0.5353  3.4998 
## 
## Random effects:
##  Groups   Name            Variance  Std.Dev.  Corr       
##  fl       (Intercept)     4.603e-02 2.145e-01            
##  fl.1     config.neiVego  6.942e-14 2.635e-07            
##           config.parVego  7.178e-13 8.473e-07 -1.00      
##           config.bothVego 9.110e-13 9.545e-07  0.86 -0.86
##  fl.2     axis.latVsag    7.382e-03 8.592e-02            
##  Residual                 1.654e-01 4.067e-01            
## Number of obs: 3544, groups:  fl, 184
## 
## Fixed effects:
##                                           Estimate Std. Error         df
## (Intercept)                                7.25287    0.02287  188.00000
## PP.othVego                                 0.25110    0.05163  188.00000
## PP.mixVego                                -0.06993    0.07501  189.00000
## config.neiVego                            -0.14015    0.03437 3192.00000
## config.parVego                             0.09326    0.03413 3190.00000
## config.bothVego                            0.10713    0.02773 3191.00000
## axis.latVsag                               0.06375    0.02068  219.00000
## PP.othVego:config.neiVego                 -0.11121    0.07736 3191.00000
## PP.othVego:config.parVego                 -0.16673    0.07704 3190.00000
## PP.othVego:config.bothVego                 0.01404    0.06252 3192.00000
## PP.mixVego:config.neiVego                  0.11307    0.11363 3195.00000
## PP.mixVego:config.parVego                  0.14639    0.11206 3192.00000
## PP.mixVego:config.bothVego                -0.09103    0.09115 3191.00000
## PP.othVego:axis.latVsag                    0.15818    0.04662  218.00000
## PP.mixVego:axis.latVsag                   -0.06494    0.06800  222.00000
## config.neiVego:axis.latVsag                0.09379    0.06875 3191.00000
## config.parVego:axis.latVsag                0.01186    0.06827 3190.00000
## config.bothVego:axis.latVsag               0.06906    0.05547 3188.00000
## PP.othVego:config.neiVego:axis.latVsag    -0.15838    0.15475 3190.00000
## PP.othVego:config.parVego:axis.latVsag     0.12700    0.15410 3189.00000
## PP.othVego:config.bothVego:axis.latVsag   -0.12900    0.12508 3188.00000
## PP.mixVego:config.neiVego:axis.latVsag     0.30203    0.22728 3194.00000
## PP.mixVego:config.parVego:axis.latVsag     0.05616    0.22413 3192.00000
## PP.mixVego:config.bothVego:axis.latVsag   -0.21682    0.18239 3187.00000
##                                         t value Pr(>|t|)    
## (Intercept)                             317.085  < 2e-16 ***
## PP.othVego                                4.864 2.43e-06 ***
## PP.mixVego                               -0.932 0.352394    
## config.neiVego                           -4.077 4.67e-05 ***
## config.parVego                            2.732 0.006323 ** 
## config.bothVego                           3.864 0.000114 ***
## axis.latVsag                              3.083 0.002311 ** 
## PP.othVego:config.neiVego                -1.437 0.150687    
## PP.othVego:config.parVego                -2.164 0.030527 *  
## PP.othVego:config.bothVego                0.225 0.822308    
## PP.mixVego:config.neiVego                 0.995 0.319759    
## PP.mixVego:config.parVego                 1.306 0.191532    
## PP.mixVego:config.bothVego               -0.999 0.318071    
## PP.othVego:axis.latVsag                   3.393 0.000822 ***
## PP.mixVego:axis.latVsag                  -0.955 0.340582    
## config.neiVego:axis.latVsag               1.364 0.172612    
## config.parVego:axis.latVsag               0.174 0.862087    
## config.bothVego:axis.latVsag              1.245 0.213242    
## PP.othVego:config.neiVego:axis.latVsag   -1.023 0.306168    
## PP.othVego:config.parVego:axis.latVsag    0.824 0.409914    
## PP.othVego:config.bothVego:axis.latVsag  -1.031 0.302465    
## PP.mixVego:config.neiVego:axis.latVsag    1.329 0.183973    
## PP.mixVego:config.parVego:axis.latVsag    0.251 0.802177    
## PP.mixVego:config.bothVego:axis.latVsag  -1.189 0.234617    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```
## 
## Correlation matrix not shown by default, as p = 24 > 12.
## Use print(x, correlation=TRUE)  or
## 	 vcov(x)	 if you need it
```

```r
####DivergeDist models####

DistanceControl.omni = lmer(log(divergDist) ~ (PP.othVego+PP.mixVego)* (config.neiVego+config.parVego+config.bothVego) * axis.latVsag 
                      + (1 | fl) 
                      #+ (0 + (config.neiVego+config.parVego+config.bothVego) | fl) #did not converge
                      + (0 + axis.latVsag | fl),
                      data=resAllError.cont,  
                      REML=FALSE)
```

```
## Warning in log(divergDist): NaNs produced
```

```
## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced
```

```r
#print('Divergence Distance:'); pander(print_stats(DistanceControl.omni))
summary(DistanceControl.omni)
```

```
## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced
```

```
## Linear mixed model fit by maximum likelihood t-tests use Satterthwaite
##   approximations to degrees of freedom [lmerMod]
## Formula: log(divergDist) ~ (PP.othVego + PP.mixVego) * (config.neiVego +  
##     config.parVego + config.bothVego) * axis.latVsag + (1 | fl) +  
##     (0 + axis.latVsag | fl)
##    Data: resAllError.cont
## 
##      AIC      BIC   logLik deviance df.resid 
##   8905.9   9072.5  -4425.9   8851.9     3516 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -3.7500 -0.7016 -0.0887  0.6562  3.4961 
## 
## Random effects:
##  Groups   Name         Variance  Std.Dev. 
##  fl       (Intercept)  1.302e-01 3.608e-01
##  fl.1     axis.latVsag 1.633e-13 4.041e-07
##  Residual              6.564e-01 8.102e-01
## Number of obs: 3543, groups:  fl, 184
## 
## Fixed effects:
##                                           Estimate Std. Error         df
## (Intercept)                              4.216e+00  3.977e-02  1.910e+02
## PP.othVego                               1.512e-01  8.976e-02  1.910e+02
## PP.mixVego                               1.887e-01  1.305e-01  1.920e+02
## config.neiVego                           1.372e-01  6.847e-02  3.361e+03
## config.parVego                           3.729e-01  6.798e-02  3.361e+03
## config.bothVego                         -2.264e-01  5.523e-02  3.360e+03
## axis.latVsag                             1.383e-01  3.768e-02  3.363e+03
## PP.othVego:config.neiVego               -1.428e-03  1.541e-01  3.361e+03
## PP.othVego:config.parVego               -3.506e-02  1.535e-01  3.361e+03
## PP.othVego:config.bothVego              -1.306e-04  1.245e-01  3.360e+03
## PP.mixVego:config.neiVego                1.773e-01  2.263e-01  3.362e+03
## PP.mixVego:config.parVego                2.358e-01  2.232e-01  3.361e+03
## PP.mixVego:config.bothVego              -3.177e-01  1.816e-01  3.360e+03
## PP.othVego:axis.latVsag                 -2.850e-03  8.494e-02  3.362e+03
## PP.mixVego:axis.latVsag                 -1.294e-02  1.240e-01  3.364e+03
## config.neiVego:axis.latVsag              5.152e-01  1.370e-01  3.362e+03
## config.parVego:axis.latVsag              8.533e-02  1.360e-01  3.361e+03
## config.bothVego:axis.latVsag            -9.067e-02  1.105e-01  3.362e+03
## PP.othVego:config.neiVego:axis.latVsag   2.351e-03  3.083e-01  3.362e+03
## PP.othVego:config.parVego:axis.latVsag   2.138e-01  3.069e-01  3.361e+03
## PP.othVego:config.bothVego:axis.latVsag -6.199e-02  2.492e-01  3.362e+03
## PP.mixVego:config.neiVego:axis.latVsag  -1.464e-01  4.527e-01  3.363e+03
## PP.mixVego:config.parVego:axis.latVsag  -1.752e-01  4.464e-01  3.362e+03
## PP.mixVego:config.bothVego:axis.latVsag  2.223e-02  3.633e-01  3.362e+03
##                                         t value Pr(>|t|)    
## (Intercept)                             106.014  < 2e-16 ***
## PP.othVego                                1.685 0.093686 .  
## PP.mixVego                                1.446 0.149776    
## config.neiVego                            2.004 0.045141 *  
## config.parVego                            5.485 4.45e-08 ***
## config.bothVego                          -4.100 4.24e-05 ***
## axis.latVsag                              3.669 0.000247 ***
## PP.othVego:config.neiVego                -0.009 0.992609    
## PP.othVego:config.parVego                -0.228 0.819297    
## PP.othVego:config.bothVego               -0.001 0.999163    
## PP.mixVego:config.neiVego                 0.783 0.433421    
## PP.mixVego:config.parVego                 1.056 0.290872    
## PP.mixVego:config.bothVego               -1.750 0.080217 .  
## PP.othVego:axis.latVsag                  -0.034 0.973234    
## PP.mixVego:axis.latVsag                  -0.104 0.916878    
## config.neiVego:axis.latVsag               3.762 0.000171 ***
## config.parVego:axis.latVsag               0.627 0.530385    
## config.bothVego:axis.latVsag             -0.821 0.411952    
## PP.othVego:config.neiVego:axis.latVsag    0.008 0.993917    
## PP.othVego:config.parVego:axis.latVsag    0.697 0.486137    
## PP.othVego:config.bothVego:axis.latVsag  -0.249 0.803525    
## PP.mixVego:config.neiVego:axis.latVsag   -0.323 0.746346    
## PP.mixVego:config.parVego:axis.latVsag   -0.392 0.694766    
## PP.mixVego:config.bothVego:axis.latVsag   0.061 0.951218    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```
## 
## Correlation matrix not shown by default, as p = 24 > 12.
## Use print(x, correlation=TRUE)  or
## 	 vcov(x)	 if you need it
```

```r
#### Directional Shifts models ####

xFlipControl.omni = lmer(xFlipDV ~ (PP.othVego+PP.mixVego) * (config.neiVego+config.parVego+config.bothVego) * axis.latVsag 
                            + (1 | fl) 
                            + (0 + (config.neiVego+config.parVego+config.bothVego) | fl) 
                            + (0 + axis.latVsag | fl),
                            data=resAllError.cont, 
                            #family = "poisson",
                            REML=FALSE)
#print('Directional Shifts:'); pander(print_stats(xFlipControl.omni))
summary(xFlipControl.omni)
```

```
## Linear mixed model fit by maximum likelihood t-tests use Satterthwaite
##   approximations to degrees of freedom [lmerMod]
## Formula: 
## xFlipDV ~ (PP.othVego + PP.mixVego) * (config.neiVego + config.parVego +  
##     config.bothVego) * axis.latVsag + (1 | fl) + (0 + (config.neiVego +  
##     config.parVego + config.bothVego) | fl) + (0 + axis.latVsag |      fl)
##    Data: resAllError.cont
## 
##      AIC      BIC   logLik deviance df.resid 
##  12572.3  12776.0  -6253.1  12506.3     3511 
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.8351 -0.6528 -0.1571  0.4889  5.9596 
## 
## Random effects:
##  Groups   Name            Variance Std.Dev. Corr       
##  fl       (Intercept)     0.42405  0.6512              
##  fl.1     config.neiVego  0.09717  0.3117              
##           config.parVego  0.08646  0.2940   -0.08      
##           config.bothVego 0.12875  0.3588   -0.25 -0.94
##  fl.2     axis.latVsag    0.03249  0.1802              
##  Residual                 1.79637  1.3403              
## Number of obs: 3544, groups:  fl, 184
## 
## Fixed effects:
##                                           Estimate Std. Error         df
## (Intercept)                              1.895e+00  7.049e-02  1.897e+02
## PP.othVego                               3.995e-01  1.591e-01  1.895e+02
## PP.mixVego                              -4.459e-03  2.312e-01  1.903e+02
## config.neiVego                          -1.070e-01  1.173e-01  2.147e+02
## config.parVego                           1.120e-01  1.161e-01  4.408e+02
## config.bothVego                          3.665e-01  9.782e-02  2.159e+02
## axis.latVsag                            -7.890e-03  6.477e-02  2.227e+02
## PP.othVego:config.neiVego               -7.342e-03  2.640e-01  2.127e+02
## PP.othVego:config.parVego               -4.970e-01  2.620e-01  4.401e+02
## PP.othVego:config.bothVego               1.758e-01  2.206e-01  2.150e+02
## PP.mixVego:config.neiVego                1.524e-02  3.876e-01  2.203e+02
## PP.mixVego:config.parVego                3.433e-01  3.811e-01  4.435e+02
## PP.mixVego:config.bothVego              -2.654e-02  3.215e-01  2.182e+02
## PP.othVego:axis.latVsag                  1.174e-01  1.460e-01  2.214e+02
## PP.mixVego:axis.latVsag                 -1.038e-01  2.131e-01  2.258e+02
## config.neiVego:axis.latVsag              5.234e-01  2.266e-01  2.882e+03
## config.parVego:axis.latVsag              1.981e-02  2.251e-01  2.864e+03
## config.bothVego:axis.latVsag            -4.412e-02  1.829e-01  2.868e+03
## PP.othVego:config.neiVego:axis.latVsag  -2.149e-01  5.101e-01  2.877e+03
## PP.othVego:config.parVego:axis.latVsag   2.694e-01  5.080e-01  2.863e+03
## PP.othVego:config.bothVego:axis.latVsag  1.543e-01  4.123e-01  2.866e+03
## PP.mixVego:config.neiVego:axis.latVsag   4.545e-01  7.493e-01  2.896e+03
## PP.mixVego:config.parVego:axis.latVsag  -4.042e-01  7.388e-01  2.868e+03
## PP.mixVego:config.bothVego:axis.latVsag -5.525e-01  6.013e-01  2.869e+03
##                                         t value Pr(>|t|)    
## (Intercept)                              26.882  < 2e-16 ***
## PP.othVego                                2.511  0.01286 *  
## PP.mixVego                               -0.019  0.98463    
## config.neiVego                           -0.912  0.36274    
## config.parVego                            0.965  0.33493    
## config.bothVego                           3.746  0.00023 ***
## axis.latVsag                             -0.122  0.90315    
## PP.othVego:config.neiVego                -0.028  0.97784    
## PP.othVego:config.parVego                -1.897  0.05852 .  
## PP.othVego:config.bothVego                0.797  0.42641    
## PP.mixVego:config.neiVego                 0.039  0.96867    
## PP.mixVego:config.parVego                 0.901  0.36812    
## PP.mixVego:config.bothVego               -0.083  0.93428    
## PP.othVego:axis.latVsag                   0.804  0.42230    
## PP.mixVego:axis.latVsag                  -0.487  0.62675    
## config.neiVego:axis.latVsag               2.309  0.02100 *  
## config.parVego:axis.latVsag               0.088  0.92987    
## config.bothVego:axis.latVsag             -0.241  0.80939    
## PP.othVego:config.neiVego:axis.latVsag   -0.421  0.67362    
## PP.othVego:config.parVego:axis.latVsag    0.530  0.59591    
## PP.othVego:config.bothVego:axis.latVsag   0.374  0.70829    
## PP.mixVego:config.neiVego:axis.latVsag    0.607  0.54417    
## PP.mixVego:config.parVego:axis.latVsag   -0.547  0.58437    
## PP.mixVego:config.bothVego:axis.latVsag  -0.919  0.35824    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```
## 
## Correlation matrix not shown by default, as p = 24 > 12.
## Use print(x, correlation=TRUE)  or
## 	 vcov(x)	 if you need it
```

##Step 3: 
Compute effect sizes 


```r
RTControl.omni = lmer(log(RTDV) ~ (PP.othVego+PP.mixVego) * (config.neiVego+config.parVego+config.bothVego) * axis.latVsag 
                      + (1 + (config.neiVego+config.parVego+config.bothVego) + axis.latVsag | fl),
                      data=resAllError.cont,  
                      REML=FALSE)

r.squaredGLMM(RTControl.omni)[2]
```

```
##       R2c 
## 0.2842045
```

```r
DistanceControl.omni = lmer(log(divergDist) ~ (PP.othVego+PP.mixVego) * (config.neiVego+config.parVego+config.bothVego) * axis.latVsag 
                            + (1 + axis.latVsag | fl),
                            data=resAllError.cont,  
                            REML=FALSE)
```

```
## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced

## Warning in log(divergDist): NaNs produced
```

```r
r.squaredGLMM(DistanceControl.omni)[2]
```

```
##       R2c 
## 0.2021324
```

```r
xFlipControl.omni = lmer(xFlipDV ~ (PP.othVego+PP.mixVego) * (config.neiVego+config.parVego+config.bothVego) * axis.latVsag 
                         + (1 + (config.neiVego+config.parVego+config.bothVego) + axis.latVsag | fl),
                         data=resAllError.cont, 
                         #family = "poisson",
                         REML=FALSE)
r.squaredGLMM(DistanceControl.omni)[2]
```

```
##       R2c 
## 0.2021324
```

```r
r.squaredGLMM(ErrorModel.omni)[2]
```

```
## Warning: extra argument(s) 'REML' disregarded
```

```
## Warning in (function (fn, par, lower = rep.int(-Inf, n), upper =
## rep.int(Inf, : failure to converge in 10000 evaluations
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
## $checkConv, : unable to evaluate scaled gradient
```

```
## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control
## $checkConv, : Model failed to converge: degenerate Hessian with 1 negative
## eigenvalues
```

```
## The result is correct only if all data used by the model has not changed since model was fitted.
```

```
##       R2c 
## 0.4321074
```

## Experiment comparisons


```r
###Chi-square comparison of the distribution of other, ego, mixed responders here (GDD1 -> Exp 2, in paper) vs. DDK study 1 

preferenceCounts <- matrix(c(129, 34, 21, 43, 31, 8), ncol=3, byrow=TRUE)
colnames(preferenceCounts) <- c("other", "ego", "mixed")
rownames(preferenceCounts) <- c("GDD1", "DDKstudy1")
preferenceCounts <- as.table(preferenceCounts)
summary(preferenceCounts)
```

```
## Number of cases in table: 266 
## Number of factors: 2 
## Test for independence of all factors:
## 	Chisq = 11.552, df = 2, p-value = 0.003101
```

```r
chisq.test(preferenceCounts)
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  preferenceCounts
## X-squared = 11.552, df = 2, p-value = 0.003101
```

```r
#GDD1 vs. GDD2a = DDK replication
#GDD2a: 43, 33, 17, other, ego, mixed
preferenceCounts <- matrix(c(129, 34, 21, 43, 33, 17), ncol=3, byrow=TRUE)
colnames(preferenceCounts) <- c("other", "ego", "mixed")
rownames(preferenceCounts) <- c("GDD1", "GDD2A")
preferenceCounts <- as.table(preferenceCounts)
summary(preferenceCounts)
```

```
## Number of cases in table: 277 
## Number of factors: 2 
## Test for independence of all factors:
## 	Chisq = 15.179, df = 2, p-value = 0.0005058
```

```r
chisq.test(preferenceCounts)
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  preferenceCounts
## X-squared = 15.179, df = 2, p-value = 0.0005058
```

```r
#GDD1 vs. GDD2b = other-aligned folders
#GDD2B: 59, 18, 18 : other, ego, mixed
preferenceCounts <- matrix(c(129, 34, 21, 59, 18, 18), ncol=3, byrow=TRUE)
colnames(preferenceCounts) <- c("other", "ego", "mixed")
rownames(preferenceCounts) <- c("GDD1", "GDD2b")
preferenceCounts <- as.table(preferenceCounts)
summary(preferenceCounts)
```

```
## Number of cases in table: 279 
## Number of factors: 2 
## Test for independence of all factors:
## 	Chisq = 3.1473, df = 2, p-value = 0.2073
```

```r
chisq.test(preferenceCounts)
```

```
## 
## 	Pearson's Chi-squared test
## 
## data:  preferenceCounts
## X-squared = 3.1473, df = 2, p-value = 0.2073
```
