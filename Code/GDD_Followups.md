# Social and configural effects on the cognitive dynamics of perspective-taking
Alexia Galati, Rick Dale, & Nick Duran  
4/15/2017  





## Preliminaries for Followups

Let's load the csv file of data from Mturk. 
200 participants were randomly assigned to the follow-ups of Exp1 (N=104) or Exp 2 (N=95). 
[1 participant did not submit responses]
WorkerIds are deidentified--they have been substituted with the values 1-200.

In each follow-up, participants viewed all unique configurations for that experiment and had to make a judgment about the alignment of the folders (Exp1 follow-up) or of the configuration (Exp 2 follow-up) with: "You, The Partner, Both You and the Partner, Neither You and the Partner". Each responses was coded as ego-aligned ("you")
), other-aligned ("partner"), both-aligned ("both"), and neither-aligned ("neither"), respectively.


```r
setwd("~/Documents/[github repositories]/social-configural-dynamics/Code")

FollowupData = read.csv('~/Documents/[github repositories]/social-configural-dynamics/Data/Followups/GDD_Followups.csv') 

dim(FollowupData)
```

```
## [1] 200  74
```

```r
#View(FollowupData)

AllTrials = FollowupData[27:69] #select columns containing responses 
#colnames(AllTrials)
dim(AllTrials) #43 columns total, for both Exp1 and Exp2
```

```
## [1] 200  43
```

```r
#separate Exp1 columns
Trials_Exp1 = AllTrials[ , c("Answer.1_0_Diagonal1_Both.png","Answer.2_0_Diagonal2_Both.png",
                             "Answer.3_0_Horizontal_Both.png","Answer.4_0_Vertical_Both.png",
                             "Answer.5_90_Diagonal1_Ego.png","Answer.6_90_Diagonal2_Ego.png",
                             "Answer.7_180_Diagonal1_Ego.png","Answer.8_180_Diagonal2_Ego.png",
                             "Answer.9_180_Horizontal_Ego.png","Answer.10_180_Vertical_Ego.png",
                             "Answer.11_270_Diagonal1_Ego.png","Answer.12_270_Diagonal2_Ego.png",
                             "Answer.13_90_Diagonal1_Other.png","Answer.14_90_Diagonal2_Other.png",
                             "Answer.15_180_Diagonal1_Other.png","Answer.16_180_Diagonal2_Other.png",
                             "Answer.17_180_Horizontal_Other.png","Answer.18_180_Vertical_Other.png",
                             "Answer.19_270_Diagonal1_Other.png","Answer.20_270_Diagonal2_Other.png") ] #columns for Exp1 from AllTrials
#colnames(Trials_Exp1)
dim(Trials_Exp1) #20 unique items from Exp 1 (a & b)
```

```
## [1] 200  20
```

```r
#separate Exp2 columns
Trials_Exp2 = AllTrials[ , c("Answer.1_0_both.aligned_2_4_6.png","Answer.2_0_both.aligned_2_6_4.png",
                             "Answer.3_90_ego.aligned_2_4_6.png","Answer.4_90_ego.aligned_2_6_4.png",
                             "Answer.5_90_ego.reversed_6_8_4.png","Answer.6_90_neither.aligned_1_9_3.png",
                             "Answer.7_90_neither.aligned_1_9_7.png","Answer.8_90_neither.aligned_3_7_1.png",
                             "Answer.9_90_other.aligned_2_4_8.png","Answer.10_90_other.aligned_4_8_2.png",
                             "Answer.11_90_other.reversed_6_8_2.png","Answer.12_180_neither.aligned_1_9_3.png",
                             "Answer.13_180_neither.aligned_2_8_4.png","Answer.14_180_neither.aligned_3_7_9.png",
                             "Answer.15_270_ego.aligned_2_4_6.png","Answer.16_270_ego.aligned_2_6_4.png",
                             "Answer.17_270_ego.reversed_4_8_6.png","Answer.18_270_neither.aligned_1_9_3.png",
                             "Answer.19_270_neither.aligned_3_7_1.png","Answer.20_270_neither.aligned_3_7_9.png",
                             "Answer.21_270_other.aligned_2_6_8.png","Answer.22_270_other.aligned_6_8_2.png",
                             "Answer.23_270_other.reversed_4_8_2.png") ] #columns for Exp2 from AllTrials
#colnames(Trials_Exp2)
dim(Trials_Exp2) #23 unique items from Exp 2
```

```
## [1] 200  23
```

```r
#Grab WorkerId and Exp as dataframes
#Note: Mturk Worker Ids have been substituted by values 1-200
WorkerId = FollowupData$WorkerId
Exp = FollowupData$Answer.experiment

#Add WorkerId and Exp to each of the dataframes for the two subsets of trials for Exp1 and Exp2
FollowupDataExp1 = cbind(WorkerId, Exp, Trials_Exp1)
FollowupDataExp2 = cbind(WorkerId, Exp, Trials_Exp2)
```

#Now let's separate the data by experiment



```r
FollowupDataExp1 = FollowupDataExp1[FollowupDataExp1$Exp == 1, ]
FollowupDataExp1 = FollowupDataExp1[FollowupDataExp1$Exp == 2, ]
```

# Experiment 1 Follow-up Analyses



```r
Exp1_cols = c("Answer.1_0_Diagonal1_Both.png","Answer.2_0_Diagonal2_Both.png",
  "Answer.3_0_Horizontal_Both.png","Answer.4_0_Vertical_Both.png",
  "Answer.5_90_Diagonal1_Ego.png","Answer.6_90_Diagonal2_Ego.png",
  "Answer.7_180_Diagonal1_Ego.png","Answer.8_180_Diagonal2_Ego.png",
  "Answer.9_180_Horizontal_Ego.png","Answer.10_180_Vertical_Ego.png",
  "Answer.11_270_Diagonal1_Ego.png","Answer.12_270_Diagonal2_Ego.png",
  "Answer.13_90_Diagonal1_Other.png","Answer.14_90_Diagonal2_Other.png",
  "Answer.15_180_Diagonal1_Other.png","Answer.16_180_Diagonal2_Other.png",
  "Answer.17_180_Horizontal_Other.png","Answer.18_180_Vertical_Other.png",
  "Answer.19_270_Diagonal1_Other.png","Answer.20_270_Diagonal2_Other.png")

expected = c(rep('both',4),rep('you',8),rep('partner',8))
res1 = c()
for (i in 1:length(Exp1_cols)) {
  cl = Exp1_cols[i]
  col_ix = which(colnames(FollowupData)==cl) # which column number has this picture data?
  this_res = as.character(FollowupData[FollowupData$Answer.experiment==1,col_ix]) # let's get it -- keeping it to experiment 1
  this_res = this_res[this_res!='']
  
  # gathers histrogram of responses to an item
  resp_dist = data.frame(you=0,partner=0,both=0,neither=0)
  resp_dist$you = sum(this_res=='you')
  resp_dist$partner = sum(this_res=='partner')
  resp_dist$both = sum(this_res=='both')
  resp_dist$neither = sum(this_res=='neither')
  
  # checks expected and computes mean proportion correct
  resp_dist$acc = mean(this_res==expected[i])
  res1 = rbind(res1,data.frame(expected=expected[i],resp_dist))
}


#### Some descriptives ####

min(res1$acc)
```

```
## [1] 0.3653846
```

```r
max(res1$acc)
```

```
## [1] 0.8252427
```

```r
mean(res1$acc)
```

```
## [1] 0.4730951
```

```r
sd(res1$acc)
```

```
## [1] 0.1099595
```

```r
## generate descriptives by alignement type

res1 = subset( res1, expected =='you' | expected =='partner' | expected =='both')
summt = dplyr::summarize(group_by(res1, expected),
                          mean=mean(acc),sd=sd(acc), se=sd(acc)/sqrt(length(acc)))
summt
```

```
## # A tibble: 3 x 4
##   expected      mean         sd         se
##     <fctr>     <dbl>      <dbl>      <dbl>
## 1     both 0.6286197 0.13184874 0.06592437
## 2      you 0.4033421 0.03452566 0.01220666
## 3  partner 0.4650859 0.06870823 0.02429203
```

```r
#### Some analyses ####


res1 <- within(res1, expected <- relevel(expected, ref = 'you'))
summary(lm(you~as.factor(expected),data=res1))
```

```
## 
## Call:
## lm(formula = you ~ as.factor(expected), data = res1)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
##  -5.75  -2.75  -1.75   2.25  12.00 
## 
## Coefficients:
##                            Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                  41.750      1.696   24.62 9.77e-15 ***
## as.factor(expected)both     -36.750      2.937  -12.51 5.29e-10 ***
## as.factor(expected)partner  -30.000      2.398  -12.51 5.30e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 4.796 on 17 degrees of freedom
## Multiple R-squared:  0.929,	Adjusted R-squared:  0.9206 
## F-statistic: 111.2 on 2 and 17 DF,  p-value: 1.727e-10
```

```r
res1 <- within(res1, expected <- relevel(expected, ref = 'partner'))
summary(lm(partner~as.factor(expected),data=res1))
```

```
## 
## Call:
## lm(formula = partner ~ as.factor(expected), data = res1)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -10.250  -4.625  -0.875   4.375  11.750 
## 
## Coefficients:
##                         Estimate Std. Error t value Pr(>|t|)    
## (Intercept)               48.250      2.374   20.33 2.30e-13 ***
## as.factor(expected)you   -34.875      3.357  -10.39 8.83e-09 ***
## as.factor(expected)both  -41.750      4.112  -10.15 1.24e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.714 on 17 degrees of freedom
## Multiple R-squared:  0.8981,	Adjusted R-squared:  0.8861 
## F-statistic: 74.94 on 2 and 17 DF,  p-value: 3.702e-09
```

```r
res1 <- within(res1, expected <- relevel(expected, ref = 'both'))
summary(lm(both~as.factor(expected),data=res1))
```

```
## 
## Call:
## lm(formula = both ~ as.factor(expected), data = res1)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -9.750 -8.750 -5.750  7.062 21.250 
## 
## Coefficients:
##                            Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                  64.750      5.686  11.388 2.23e-09 ***
## as.factor(expected)partner  -40.750      6.964  -5.852 1.92e-05 ***
## as.factor(expected)you      -36.000      6.964  -5.170 7.69e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 11.37 on 17 degrees of freedom
## Multiple R-squared:  0.686,	Adjusted R-squared:  0.6491 
## F-statistic: 18.57 on 2 and 17 DF,  p-value: 5.294e-05
```

```r
summary(lm(neither~as.factor(expected),data=res1))
```

```
## 
## Call:
## lm(formula = neither ~ as.factor(expected), data = res1)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -13.625  -2.219   2.312   3.375  11.250 
## 
## Coefficients:
##                            Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                  26.750      3.335   8.022 3.52e-07 ***
## as.factor(expected)partner   -7.000      4.084  -1.714   0.1047    
## as.factor(expected)you       -7.125      4.084  -1.745   0.0991 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.669 on 17 degrees of freedom
## Multiple R-squared:  0.1744,	Adjusted R-squared:  0.07722 
## F-statistic: 1.795 on 2 and 17 DF,  p-value: 0.1962
```

```r
# comparison to chance
t.test(res1$acc-.25)
```

```
## 
## 	One Sample t-test
## 
## data:  res1$acc - 0.25
## t = 9.0734, df = 19, p-value = 2.461e-08
## alternative hypothesis: true mean is not equal to 0
## 95 percent confidence interval:
##  0.1716325 0.2745578
## sample estimates:
## mean of x 
## 0.2230951
```

```r
# does "both" compete with the alternate perspective, conditioned on each
temp = res1[res1$expected=='you',] # subselect out only those 8 items that are 'you'
t.test(temp$both-temp$partner) # then do paired t-test with rate of error for partner vs. both
```

```
## 
## 	One Sample t-test
## 
## data:  temp$both - temp$partner
## t = 2.703, df = 7, p-value = 0.03051
## alternative hypothesis: true mean is not equal to 0
## 95 percent confidence interval:
##   1.924692 28.825308
## sample estimates:
## mean of x 
##    15.375
```

```r
t.test(temp$neither-temp$partner)
```

```
## 
## 	One Sample t-test
## 
## data:  temp$neither - temp$partner
## t = 2.6672, df = 7, p-value = 0.03213
## alternative hypothesis: true mean is not equal to 0
## 95 percent confidence interval:
##   0.7089676 11.7910324
## sample estimates:
## mean of x 
##      6.25
```

```r
temp = res1[res1$expected=='partner',] # subselect out only those 8 items that are 'partner'
t.test(temp$both-temp$you) # then do paired t-test with rate of error for you vs. both
```

```
## 
## 	One Sample t-test
## 
## data:  temp$both - temp$you
## t = 2.7038, df = 7, p-value = 0.03047
## alternative hypothesis: true mean is not equal to 0
## 95 percent confidence interval:
##   1.536716 22.963284
## sample estimates:
## mean of x 
##     12.25
```

```r
t.test(temp$neither-temp$you)
```

```
## 
## 	One Sample t-test
## 
## data:  temp$neither - temp$you
## t = 4.7932, df = 7, p-value = 0.001982
## alternative hypothesis: true mean is not equal to 0
## 95 percent confidence interval:
##   4.053334 11.946666
## sample estimates:
## mean of x 
##         8
```

# Experiment 2 Follow-up Analyses

For Experiment 2, we examine whether beyond our own classification of the configurations as ego-aligned, other-aligned, both-aligned, and neither-aligned, other conceptualizations of the 3-object array better account for these particpants classification judgments here. 

We therefore consider a classification of the alignment of the objects according to the orientation of an L shape (expectedL). And a classification of the alignement of the triangular configuration according to the orientation of the base of the triangle, rather than its vertex, as in our original classification (expected_Flipped).


```r
Exp2_cols = c("Answer.1_0_both.aligned_2_4_6.png","Answer.2_0_both.aligned_2_6_4.png",
  "Answer.3_90_ego.aligned_2_4_6.png","Answer.4_90_ego.aligned_2_6_4.png",
  "Answer.5_90_ego.reversed_6_8_4.png","Answer.6_90_neither.aligned_1_9_3.png",
  "Answer.7_90_neither.aligned_1_9_7.png","Answer.8_90_neither.aligned_3_7_1.png",
  "Answer.9_90_other.aligned_2_4_8.png","Answer.10_90_other.aligned_4_8_2.png",
  "Answer.11_90_other.reversed_6_8_2.png","Answer.12_180_neither.aligned_1_9_3.png",
  "Answer.13_180_neither.aligned_2_8_4.png","Answer.14_180_neither.aligned_3_7_9.png",
  "Answer.15_270_ego.aligned_2_4_6.png","Answer.16_270_ego.aligned_2_6_4.png",
  "Answer.17_270_ego.reversed_4_8_6.png","Answer.18_270_neither.aligned_1_9_3.png",
  "Answer.19_270_neither.aligned_3_7_1.png","Answer.20_270_neither.aligned_3_7_9.png",
  "Answer.21_270_other.aligned_2_6_8.png","Answer.22_270_other.aligned_6_8_2.png",
  "Answer.23_270_other.reversed_4_8_2.png")

split_list = strsplit(Exp2_cols,'_')
expected = unlist(lapply(split_list,function(x) {
  return(unlist(strsplit(x[3],'\\.'))[1]) # use double slash because . is special in regular expressions (means "any character at all"); double slash makes it a literal period (escapes it)
}))
speaker_pos = unlist(lapply(split_list,function(x) {
  return(x[2])
}))
pos_seq = do.call(rbind,lapply(split_list,function(x) {
  return(data.frame(p1=x[4],p2=x[5],p3=unlist(strsplit(x[6],'\\.'))[1]))
}))

print(expected)
```

```
##  [1] "both"    "both"    "ego"     "ego"     "ego"     "neither" "neither"
##  [8] "neither" "other"   "other"   "other"   "neither" "neither" "neither"
## [15] "ego"     "ego"     "ego"     "neither" "neither" "neither" "other"  
## [22] "other"   "other"
```

```r
#changing "ego" label with "you" and "other" label with "partner" for the next step
expected = gsub('ego','you',expected)
expected = gsub('other','partner',expected)

#classifying configurations according to the orientation of an L-shape
expectedL = c('neither', 'neither','neither','neither','neither',
              'you','you','partner','neither','neither',
              'neither','both','neither','neither','neither',
              'neither','neither','you','partner','partner',
              'neither','neither','neither')
table(expectedL)
```

```
## expectedL
##    both neither partner     you 
##       1      16       3       3
```

```r
## you and partner classifications seem to experience interference from one another based on the OLS regressions below
## what if we recode them by flipping them 

expected_Flipped = expected
expected_Flipped= gsub('you','dummy', expected_Flipped)
expected_Flipped= gsub('partner','you', expected_Flipped)
expected_Flipped= gsub('dummy','partner', expected_Flipped)

res2 = c()
for (i in 1:length(Exp2_cols)) {
  cl = Exp2_cols[i]
  col_ix = which(colnames(FollowupData)==cl) # which column number has this picture data?
  this_res = as.character(FollowupData[FollowupData$Answer.experiment==2,col_ix]) # let's get it -- keeping it to experiment 1
  this_res = this_res[this_res!='']
  
  # gathers histrogram of responses to an item
  resp_dist = data.frame(you=0,partner=0,both=0,neither=0)
  resp_dist$you = sum(this_res=='you')
  resp_dist$partner = sum(this_res=='partner')
  resp_dist$both = sum(this_res=='both')
  resp_dist$neither = sum(this_res=='neither')
  
  # checks expected and computes mean proportion correct
  resp_dist$acc = mean(this_res==expected[i])
  resp_dist$accL = mean(this_res==expectedL[i])
  resp_dist$accFlipped = mean(this_res==expected_Flipped[i])
  res2 = rbind(res2,data.frame(item=Exp2_cols[i],speaker_pos=speaker_pos[i],expected=expected[i],expectedL=expectedL[i],expected_Flipped=expected_Flipped[i],resp_dist))
}



## Getting some descriptives, from raw expectations...
min(res2$acc)
```

```
## [1] 0.09473684
```

```r
max(res2$acc)
```

```
## [1] 0.5591398
```

```r
mean(res2$acc)
```

```
## [1] 0.2443163
```

```r
sd(res2$acc)
```

```
## [1] 0.1387484
```

```r
min(res2$accL)
```

```
## [1] 0.0212766
```

```r
max(res2$accL)
```

```
## [1] 0.6595745
```

```r
mean(res2$accL)
```

```
## [1] 0.2150583
```

```r
sd(res2$accL)
```

```
## [1] 0.1475604
```

```r
min(res2$accFlipped)
```

```
## [1] 0.1368421
```

```r
max(res2$accFlipped)
```

```
## [1] 0.6702128
```

```r
mean(res2$accFlipped)
```

```
## [1] 0.3670778
```

```r
sd(res2$accFlipped)
```

```
## [1] 0.195143
```

```r
res2 = subset( res2, expected =='you' | expected =='partner' | expected =='both' | expected == 'neither')
summt = dplyr::summarize(group_by(res2, expected),
                         mean=mean(acc),sd=sd(acc), se=sd(acc)/sqrt(length(acc)))
summt
```

```
## # A tibble: 4 x 4
##   expected      mean          sd           se
##     <fctr>     <dbl>       <dbl>        <dbl>
## 1     both 0.5585173 0.000880382 0.0006225241
## 2      you 0.1552072 0.081019482 0.0330760650
## 3  neither 0.2480528 0.104587046 0.0348623486
## 4  partner 0.2230870 0.105145879 0.0429256252
```

```r
res2 = subset( res2, expectedL =='you' | expectedL =='partner' | expectedL =='both' | expectedL == 'neither')
summtL = dplyr::summarize(group_by(res2, expectedL),
                         mean=mean(accL),sd=sd(accL), se=sd(accL)/sqrt(length(accL)))
summtL
```

```
## # A tibble: 4 x 4
##   expectedL      mean        sd         se
##      <fctr>     <dbl>     <dbl>      <dbl>
## 1   neither 0.1865819 0.1010443 0.02526106
## 2       you 0.2322508 0.1210509 0.06988878
## 3   partner 0.2795819 0.3360980 0.19404628
## 4      both 0.4255319        NA         NA
```

```r
res2 = subset( res2, expected_Flipped =='you' | expected_Flipped =='partner' | expected_Flipped =='both' | expected_Flipped == 'neither')
summtFlipped = dplyr::summarize(group_by(res2, expected_Flipped),
                          mean=mean(accFlipped),sd=sd(accFlipped), se=sd(accFlipped)/sqrt(length(accFlipped)))
summtFlipped
```

```
## # A tibble: 4 x 4
##   expected_Flipped      mean          sd           se
##             <fctr>     <dbl>       <dbl>        <dbl>
## 1             both 0.5585173 0.000880382 0.0006225241
## 2          partner 0.4710340 0.238236829 0.0972597780
## 3          neither 0.2480528 0.104587046 0.0348623486
## 4              you 0.3778462 0.200073576 0.0816796955
```

```r
### Comparisons to chance

# comparison to chance, assuming 3 objects are peceived as triangle
t.test(res2$acc-.25)
```

```
## 
## 	One Sample t-test
## 
## data:  res2$acc - 0.25
## t = -0.19646, df = 22, p-value = 0.8461
## alternative hypothesis: true mean is not equal to 0
## 95 percent confidence interval:
##  -0.06568302  0.05431555
## sample estimates:
##    mean of x 
## -0.005683732
```

```r
# comparison to chance, assuming 3 objects are perceived as L-shape
t.test(res2$accL-.25)
```

```
## 
## 	One Sample t-test
## 
## data:  res2$accL - 0.25
## t = -1.1356, df = 22, p-value = 0.2683
## alternative hypothesis: true mean is not equal to 0
## 95 percent confidence interval:
##  -0.09875165  0.02886818
## sample estimates:
##   mean of x 
## -0.03494173
```

```r
# comparison to chance, assumign that ego-aligned are perceived as other-aligned (and vice-versa)
t.test(res2$accFlipped-.25)
```

```
## 
## 	One Sample t-test
## 
## data:  res2$accFlipped - 0.25
## t = 2.8773, df = 22, p-value = 0.008749
## alternative hypothesis: true mean is not equal to 0
## 95 percent confidence interval:
##  0.03269169 0.20146400
## sample estimates:
## mean of x 
## 0.1170778
```

```r
res2 <- within(res2, expected <- relevel(expected, ref = 'you'))
summary(lm(you~as.factor(expected),data=res2))
```

```
## 
## Call:
## lm(formula = you ~ as.factor(expected), data = res2)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -21.833  -9.556  -3.556   9.833  33.444 
## 
## Coefficients:
##                            Estimate Std. Error t value Pr(>|t|)  
## (Intercept)                  14.667      6.785   2.162   0.0436 *
## as.factor(expected)both     -12.667     13.570  -0.933   0.3623  
## as.factor(expected)neither    6.889      8.760   0.786   0.4413  
## as.factor(expected)partner   21.167      9.596   2.206   0.0399 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 16.62 on 19 degrees of freedom
## Multiple R-squared:  0.3021,	Adjusted R-squared:  0.1919 
## F-statistic: 2.741 on 3 and 19 DF,  p-value: 0.07178
```

```r
res2 <- within(res2, expected <- relevel(expected, ref = 'partner'))
summary(lm(partner~as.factor(expected),data=res2))
```

```
## 
## Call:
## lm(formula = partner ~ as.factor(expected), data = res2)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -31.500 -10.528  -2.167  11.667  43.111 
## 
## Coefficients:
##                            Estimate Std. Error t value Pr(>|t|)  
## (Intercept)                  21.167      7.419   2.853   0.0102 *
## as.factor(expected)you       23.333     10.492   2.224   0.0385 *
## as.factor(expected)both     -19.167     14.839  -1.292   0.2120  
## as.factor(expected)neither   -2.278      9.578  -0.238   0.8146  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 18.17 on 19 degrees of freedom
## Multiple R-squared:  0.3748,	Adjusted R-squared:  0.2761 
## F-statistic: 3.796 on 3 and 19 DF,  p-value: 0.02743
```

```r
res2 <- within(res2, expected <- relevel(expected, ref = 'both'))
summary(lm(both~as.factor(expected),data=res2))
```

```
## 
## Call:
## lm(formula = both ~ as.factor(expected), data = res2)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -21.556 -10.361  -4.333   8.944  40.444 
## 
## Coefficients:
##                            Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   52.50      11.33   4.635 0.000181 ***
## as.factor(expected)partner   -31.17      13.08  -2.383 0.027767 *  
## as.factor(expected)you       -29.33      13.08  -2.243 0.037027 *  
## as.factor(expected)neither   -21.94      12.52  -1.753 0.095810 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 16.02 on 19 degrees of freedom
## Multiple R-squared:  0.2547,	Adjusted R-squared:  0.137 
## F-statistic: 2.164 on 3 and 19 DF,  p-value: 0.1258
```

```r
res2 <- within(res2, expected <- relevel(expected, ref = 'neither'))
summary(lm(neither~as.factor(expected),data=res2))
```

```
## 
## Call:
## lm(formula = neither ~ as.factor(expected), data = res2)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -8.500 -4.944 -1.167  3.694 22.556 
## 
## Coefficients:
##                            Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                  23.444      2.610   8.982 2.88e-08 ***
## as.factor(expected)both      14.056      6.121   2.296   0.0332 *  
## as.factor(expected)partner   -6.944      4.127  -1.683   0.1088    
## as.factor(expected)you      -11.278      4.127  -2.733   0.0132 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 7.831 on 19 degrees of freedom
## Multiple R-squared:  0.4991,	Adjusted R-squared:   0.42 
## F-statistic:  6.31 on 3 and 19 DF,  p-value: 0.003752
```

```r
# does "both" compete with the alternate perspective, conditioned on each
temp = res2[res2$expected=='you',] # subselect out only those 8 items that are 'you'
t.test(temp$both-temp$partner) # then do paired t-test with rate of error for partner vs. both
```

```
## 
## 	One Sample t-test
## 
## data:  temp$both - temp$partner
## t = -1.3374, df = 5, p-value = 0.2387
## alternative hypothesis: true mean is not equal to 0
## 95 percent confidence interval:
##  -62.33749  19.67083
## sample estimates:
## mean of x 
## -21.33333
```

```r
temp = res2[res2$expected=='partner',] # subselect out only those 8 items that are 'partner'
t.test(temp$both-temp$you) # then do paired t-test with rate of error for you vs. both
```

```
## 
## 	One Sample t-test
## 
## data:  temp$both - temp$you
## t = -1.1217, df = 5, p-value = 0.313
## alternative hypothesis: true mean is not equal to 0
## 95 percent confidence interval:
##  -47.73084  18.73084
## sample estimates:
## mean of x 
##     -14.5
```

```r
# does neither compete with the alternate perspective
temp = res2[res2$expected=='you',] 
t.test(temp$neither-temp$partner) 
```

```
## 
## 	One Sample t-test
## 
## data:  temp$neither - temp$partner
## t = -3.7441, df = 5, p-value = 0.01338
## alternative hypothesis: true mean is not equal to 0
## 95 percent confidence interval:
##  -54.53247 -10.13419
## sample estimates:
## mean of x 
## -32.33333
```

```r
temp = res2[res2$expected=='partner',] 
t.test(temp$neither-temp$you) 
```

```
## 
## 	One Sample t-test
## 
## data:  temp$neither - temp$you
## t = -2.6154, df = 5, p-value = 0.04736
## alternative hypothesis: true mean is not equal to 0
## 95 percent confidence interval:
##  -38.335558  -0.331109
## sample estimates:
## mean of x 
## -19.33333
```
