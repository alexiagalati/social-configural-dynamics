library(gdata)
library(dplyr) 

setwd("~/Documents/[github repositories]/social-configural-dynamics/Code")

FollowupData = read.csv('~/Documents/[github repositories]/social-configural-dynamics/Data/Followups/GDD_Followups.csv') 

dim(FollowupData)
#View(FollowupData)

AllTrials = FollowupData[27:69] #select columns containing responses 
colnames(AllTrials)
dim(AllTrials) #43 columns total, for both Exp1 and Exp2

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

#Grab WorkerId and Exp as dataframes
#Note: Mturk Worker Ids have been substituted by values 1-200
WorkerId = FollowupData$WorkerId
Exp = FollowupData$Answer.experiment

#Add WorkerId and Exp to each of the dataframes for the two subsets of trials for Exp1 and Exp2
FollowupDataExp1 = cbind(WorkerId, Exp, Trials_Exp1)
FollowupDataExp2 = cbind(WorkerId, Exp, Trials_Exp2)

#Now filter separate them by experiment
FollowupDataExp1 = FollowupDataExp1[FollowupDataExp1$Exp == 1, ]
FollowupDataExp1 = FollowupDataExp1[FollowupDataExp1$Exp == 2, ]

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
max(res1$acc)
mean(res1$acc)
sd(res1$acc)

## generate descriptives by alignement type

res1 = subset( res1, expected =='you' | expected =='partner' | expected =='both')
summt = dplyr::summarize(group_by(res1, expected),
                          mean=mean(acc),sd=sd(acc), se=sd(acc)/sqrt(length(acc)))
summt

#### Some analyses ####


res1 <- within(res1, expected <- relevel(expected, ref = 'you'))
summary(lm(you~as.factor(expected),data=res1))

res1 <- within(res1, expected <- relevel(expected, ref = 'partner'))
summary(lm(partner~as.factor(expected),data=res1))

res1 <- within(res1, expected <- relevel(expected, ref = 'both'))
summary(lm(both~as.factor(expected),data=res1))

summary(lm(neither~as.factor(expected),data=res1))

# comparison to chance
t.test(res1$acc-.25)

# does "both" compete with the alternate perspective, conditioned on each
temp = res1[res1$expected=='you',] # subselect out only those 8 items that are 'you'
t.test(temp$both-temp$partner) # then do paired t-test with rate of error for partner vs. both
t.test(temp$neither-temp$partner)

temp = res1[res1$expected=='partner',] # subselect out only those 8 items that are 'partner'
t.test(temp$both-temp$you) # then do paired t-test with rate of error for you vs. both
t.test(temp$neither-temp$you)

####################
### experiment 2 ##
###################

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
max(res2$acc)
mean(res2$acc)
sd(res2$acc)

min(res2$accL)
max(res2$accL)
mean(res2$accL)
sd(res2$accL)

min(res2$accFlipped)
max(res2$accFlipped)
mean(res2$accFlipped)
sd(res2$accFlipped)

res2 = subset( res2, expected =='you' | expected =='partner' | expected =='both' | expected == 'neither')
summt = dplyr::summarize(group_by(res2, expected),
                         mean=mean(acc),sd=sd(acc), se=sd(acc)/sqrt(length(acc)))
summt

res2 = subset( res2, expectedL =='you' | expectedL =='partner' | expectedL =='both' | expectedL == 'neither')
summtL = dplyr::summarize(group_by(res2, expectedL),
                         mean=mean(accL),sd=sd(accL), se=sd(accL)/sqrt(length(accL)))
summtL

res2 = subset( res2, expected_Flipped =='you' | expected_Flipped =='partner' | expected_Flipped =='both' | expected_Flipped == 'neither')
summtFlipped = dplyr::summarize(group_by(res2, expected_Flipped),
                          mean=mean(accFlipped),sd=sd(accFlipped), se=sd(accFlipped)/sqrt(length(accFlipped)))
summtFlipped


### Comparisons to chance

# comparison to chance, assuming 3 objects are peceived as triangle
t.test(res2$acc-.25)

# comparison to chance, assuming 3 objects are perceived as L-shape
t.test(res2$accL-.25)

# comparison to chance, assumign that ego-aligned are perceived as other-aligned (and vice-versa)
t.test(res2$accFlipped-.25)



res2 <- within(res2, expected <- relevel(expected, ref = 'you'))
summary(lm(you~as.factor(expected),data=res2))

res2 <- within(res2, expected <- relevel(expected, ref = 'partner'))
summary(lm(partner~as.factor(expected),data=res2))

res2 <- within(res2, expected <- relevel(expected, ref = 'both'))
summary(lm(both~as.factor(expected),data=res2))

res2 <- within(res2, expected <- relevel(expected, ref = 'neither'))
summary(lm(neither~as.factor(expected),data=res2))


# does "both" compete with the alternate perspective, conditioned on each
temp = res2[res2$expected=='you',] # subselect out only those 8 items that are 'you'
t.test(temp$both-temp$partner) # then do paired t-test with rate of error for partner vs. both

temp = res2[res2$expected=='partner',] # subselect out only those 8 items that are 'partner'
t.test(temp$both-temp$you) # then do paired t-test with rate of error for you vs. both

# does neither compete with the alternate perspective
temp = res2[res2$expected=='you',] 
t.test(temp$neither-temp$partner) 

temp = res2[res2$expected=='partner',] 
t.test(temp$neither-temp$you) 


