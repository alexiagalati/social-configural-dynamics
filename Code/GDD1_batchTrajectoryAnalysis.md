# Social and configural effects on the cognitive dynamics of perspective-taking
Alexia Galati, Rick Dale, & Nick Duran  
4/15/2017  





Setting up x,y coordinates


```r
setwd("~/Documents/[github repositories]/social-configural-dynamics/Code")

sft = 60
cx = 300 - sft/2 # center x with adjustment for T/L
cy = 300 - sft/2 # center y
objPositionX = c(cx-sft/1.5,cx,cx+sft/1.5,cx-sft,cx,cx+sft,cx-sft/1.5,cx,cx+sft/1.5)
objPositionY = c(cy-sft/1.5,cy-sft,cy-sft/1.5,cy,cy,cy,cy+sft/1.5,cy+sft,cy+sft/1.5)
```

Access data files contained in .txt files for Exp1a.


```r
#for EXP 1A
fls = list.files('~/Documents/[github repositories]/social-configural-dynamics/Data/Exp1a',pattern='txt') # get list of txt files
rt = "~/Documents/[github repositories]/social-configural-dynamics/Data/Exp1a/" # root folder location
```

Run loop that computes different DVs for each trial. 


```r
resAll = c() # all results -- let's initialize an empty variable
resAllError = c()
resAllPerspectiveDistribution = c()

switch_fold = 0

for (fl in fls) {
  print(fl)
  testData = read.table(paste(rt,fl,sep=''),sep="\t",header=F) # get the raw data by taking rt+file name / separate with empty string
  # now let's give it the column names
  colnames(testData) = c("listn", "turknum", "folder_choice","cond","soundfile","ppos","f1_x", "f1_y", "f2_x", "f2_y", "absdir", "folder_config","instruction","other","trial","ms","t","x","y")  
  subjectOthercentricDistribution = c(which(fls==fl),0) # AG CHANGED THIS, since we only have a column for othercentric folder here. Keep track of the othercentric object chosen; initialize at 0; incremented for each subject
  subjectError = c(which(fls==fl),0)
  
  # check to make sure it's not a double up data set
  if (sum(diff(testData$ms)<0)>0) {
    print(paste('We have a double-up subject file in:',fl))
  } 
  else {
    for (i in 2:41) { #starting at trial #2 since the first recording (5.mp3) is not an experimental trial
      traj = testData[testData$trial==i,]
      if (traj$cond[1]=='ambig' & nrow(traj)>0) {
        rawFlashY = traj$y[1]
        rawFlashX = traj$x[1]
        traj$y = -1*(traj$y-600) # mirror image on the y coordinate
        numSamples = length(traj$x) # final entry in the column (vector)
        
        pointAtWhichFolderSelected = which(traj$folder_choice %in% c('F1init','F2init'))[1]  
        xFlipDV = xflip(traj$x[1:pointAtWhichFolderSelected],0)
        veloc = sqrt(diff(traj$x[1:pointAtWhichFolderSelected])^2+diff(traj$y[1:pointAtWhichFolderSelected])^2)
        totalDistanceDV = sum(veloc)
        accel = diff(smooth(veloc))
        AC = sum(sign(accel[2:length(accel)]) * sign(accel[1:(length(accel)-1)]) < 0)
        RTDV = traj$ms[pointAtWhichFolderSelected]-traj$ms[1]
 
        if (traj$folder_choice[pointAtWhichFolderSelected]=='F1init') {
          #positionChose = traj$f1[pointAtWhichFolderSelected] #CHANGE THIS
          positionChose = 'F1'
        } else {
          #positionChose = traj$f2[pointAtWhichFolderSelected] #CHANGES THIS
          positionChose = 'F2'
        }
        #positionPerspectiveInformation = traj[1,11:12] #CHANGE THIS
        positionPerspectiveInformation = traj[1,14] #14th column contains id of othercentric folder
        #subjectPerspectiveDistribution = subjectPerspectiveDistribution + c(0,1*(positionChose==positionPerspectiveInformation)) #CHANGE THIS
        subjectOthercentricDistribution = subjectOthercentricDistribution + c(0, 1*(positionChose==positionPerspectiveInformation))
        
        switch_fold = switch_fold + (length(unique(traj$folder_choice))>=4) #Add counts if number of labels for folder choice column in a trial is equal or larger than 4 (waiting for, F1init/F2init, F1drop/F2drop, F2init)
        
        
        # concatenating ALL trials (make sure to comment out resAll below if you want per trial results)
        resAll = rbind(resAll,data.frame(fl,trial=traj$trial[1],instruction=substr(traj$instruction[1],1,4), 
                                         folder_config=traj$folder_config[1], ppos=traj$ppos[1], RTDV,totalDistanceDV,xFlipDV,AC,
                                         chosen=positionChose,other=traj$other[1]))
      }
          
      if (traj$cond[1]=='shared' & nrow(traj)>0) {
        rawFlashY = traj$y[1]
        rawFlashX = traj$x[1]
        traj$y = -1*(traj$y-600) # mirror image on the y coordinate
        numSamples = length(traj$x) # final entry in the column (vector)
        pointAtWhichFolderSelected = which(traj$folder_choice %in% c('F1init','F2init'))[1]  
        xFlipDV = xflip(traj$x[1:pointAtWhichFolderSelected],0)
        veloc = sqrt(diff(traj$x[1:pointAtWhichFolderSelected])^2+diff(traj$y[1:pointAtWhichFolderSelected])^2)
        totalDistanceDV = sum(veloc)
        
        accel = diff(smooth(veloc))
        AC = sum(sign(accel[2:length(accel)]) * sign(accel[1:(length(accel)-1)]) < 0)
        RTDV = traj$ms[pointAtWhichFolderSelected]-traj$ms[1]   
        
        if (traj$folder_choice[pointAtWhichFolderSelected]=='F1init') {
          #positionChose = traj$f1[pointAtWhichFolderSelected] #THIS NEEDS TO CHANGE
          positionChose = "F1" 
        } else {
          #positionChose = traj$f2[pointAtWhichFolderSelected] #THIS NEEDS TO CHANGE
          positionChose = "F2"
        }      
        subjectError = subjectError + c(0,1*(traj$other[1]!=positionChose)) # CHANGED (cor --> other). increment by 1 if it is TRUE that they chose a diff't object from correct             
        
        switch_fold = switch_fold + (length(unique(traj$folder_choice))>=4) #Add counts if number of labels for folder choice column in a trial is equal or larger than 4 (waiting for, F1init/F2init, F1drop/F2drop, F2init)
        
        
        resAllError = rbind(resAllError,data.frame(fl,trial=traj$trial[1],
                                                   instruction=substr(traj$instruction[1],1,4), folder_config=traj$folder_config[1], ppos=traj$ppos[1], RTDV,AC,
                                                   totalDistanceDV,xFlipDV, chosen=positionChose,cor=traj$other[1],err=(traj$other[1]!=positionChose)))
      }
      
    }
    #resAllPerspectiveDistribution = rbind(resAllPerspectiveDistribution,subjectPerspectiveDistribution) # concatenating ALL results with the objResults of the current subject
    #resAllError = rbind(resAllError,subjectError) # concatenating ALL results with the objResults of the current subject
  }
  
}
```

```
## [1] "100.txt"
## [1] "102.txt"
## [1] "104.txt"
## [1] "105.txt"
## [1] "114.txt"
## [1] "117.txt"
## [1] "125.txt"
## [1] "130.txt"
## [1] "132.txt"
## [1] "134.txt"
## [1] "137.txt"
## [1] "144.txt"
## [1] "146.txt"
## [1] "147.txt"
## [1] "148.txt"
## [1] "149.txt"
## [1] "151.txt"
## [1] "152.txt"
## [1] "155.txt"
## [1] "156.txt"
## [1] "164.txt"
## [1] "168.txt"
## [1] "176.txt"
## [1] "180.txt"
## [1] "183.txt"
## [1] "185.txt"
## [1] "188.txt"
## [1] "190.txt"
## [1] "191.txt"
## [1] "198.txt"
## [1] "We have a double-up subject file in: 198.txt"
## [1] "199.txt"
## [1] "207.txt"
## [1] "208.txt"
## [1] "We have a double-up subject file in: 208.txt"
## [1] "209.txt"
## [1] "211.txt"
## [1] "222.txt"
## [1] "226.txt"
## [1] "233.txt"
## [1] "241.txt"
## [1] "242.txt"
## [1] "243.txt"
## [1] "247.txt"
## [1] "256.txt"
## [1] "258.txt"
## [1] "265.txt"
## [1] "266.txt"
## [1] "269.txt"
## [1] "272.txt"
## [1] "274.txt"
## [1] "284.txt"
## [1] "287.txt"
## [1] "288.txt"
## [1] "291.txt"
## [1] "295.txt"
## [1] "300.txt"
## [1] "308.txt"
## [1] "313.txt"
## [1] "315.txt"
## [1] "317.txt"
## [1] "We have a double-up subject file in: 317.txt"
## [1] "321.txt"
## [1] "322.txt"
## [1] "325.txt"
## [1] "326.txt"
## [1] "328.txt"
## [1] "331.txt"
## [1] "334.txt"
## [1] "336.txt"
## [1] "338.txt"
## [1] "339.txt"
## [1] "345.txt"
## [1] "346.txt"
## [1] "350.txt"
## [1] "352.txt"
## [1] "362.txt"
## [1] "367.txt"
## [1] "368.txt"
## [1] "370.txt"
## [1] "380.txt"
## [1] "381.txt"
## [1] "386.txt"
## [1] "390.txt"
## [1] "394.txt"
## [1] "399.txt"
## [1] "408.txt"
## [1] "414.txt"
## [1] "424.txt"
## [1] "434.txt"
## [1] "437.txt"
## [1] "439.txt"
## [1] "456.txt"
## [1] "466.txt"
## [1] "469.txt"
## [1] "471.txt"
## [1] "478.txt"
## [1] "483.txt"
## [1] "493.txt"
## [1] "We have a double-up subject file in: 493.txt"
## [1] "498.txt"
```

Save output file with churned data for Experiment 1a or 1b, as needed.


```r
save(file='GDD1B_churnedRawTrajectoryData.Rd',resAll,resAllError)
```

Compute proportion of cases for which the participant picked up a folder, dropped it, and picked up another folder (folder switch). Do so for Exp 1a or 1b. 


```r
switch_fold #get number of trials where there is a switch in folder
```

```
## [1] 97
```

```r
percent_switch_fold_Exp1a = switch_fold / (95*40) #95 final participants x 40 trials
percent_switch_fold_Exp1a
```

```
## [1] 0.02552632
```

Repeat for Exp1b. 
Access data files contained in .txt files for Exp1b.


```r
#for EXP 1B

fls = list.files('~/Documents/[github repositories]/social-configural-dynamics/Data/Exp1b',pattern='txt') # get list of txt files
rt = "~/Documents/[github repositories]/social-configural-dynamics/Data/Exp1b/" # root folder location
```

Run the same loop again to compute DVs for each trial of Exp1b data. 


```r
resAll = c() # all results -- let's initialize an empty variable
resAllError = c()
resAllPerspectiveDistribution = c()

switch_fold = 0

for (fl in fls) {
  print(fl)
  testData = read.table(paste(rt,fl,sep=''),sep="\t",header=F) # get the raw data by taking rt+file name / separate with empty string
  # now let's give it the column names
  colnames(testData) = c("listn", "turknum", "folder_choice","cond","soundfile","ppos","f1_x", "f1_y", "f2_x", "f2_y", "absdir", "folder_config","instruction","other","trial","ms","t","x","y")  
  subjectOthercentricDistribution = c(which(fls==fl),0) # AG CHANGED THIS, since we only have a column for othercentric folder here. Keep track of the othercentric object chosen; initialize at 0; incremented for each subject
  subjectError = c(which(fls==fl),0)
  
  # check to make sure it's not a double up data set
  if (sum(diff(testData$ms)<0)>0) {
    print(paste('We have a double-up subject file in:',fl))
  } 
  else {
    for (i in 2:41) { #starting at trial #2 since the first recording (5.mp3) is not an experimental trial
      traj = testData[testData$trial==i,]
      if (traj$cond[1]=='ambig' & nrow(traj)>0) {
        rawFlashY = traj$y[1]
        rawFlashX = traj$x[1]
        traj$y = -1*(traj$y-600) # mirror image on the y coordinate
        numSamples = length(traj$x) # final entry in the column (vector)
        
        pointAtWhichFolderSelected = which(traj$folder_choice %in% c('F1init','F2init'))[1]  
        xFlipDV = xflip(traj$x[1:pointAtWhichFolderSelected],0)
        veloc = sqrt(diff(traj$x[1:pointAtWhichFolderSelected])^2+diff(traj$y[1:pointAtWhichFolderSelected])^2)
        totalDistanceDV = sum(veloc)
        accel = diff(smooth(veloc))
        AC = sum(sign(accel[2:length(accel)]) * sign(accel[1:(length(accel)-1)]) < 0)
        RTDV = traj$ms[pointAtWhichFolderSelected]-traj$ms[1]
 
        if (traj$folder_choice[pointAtWhichFolderSelected]=='F1init') {
          #positionChose = traj$f1[pointAtWhichFolderSelected] #CHANGE THIS
          positionChose = 'F1'
        } else {
          #positionChose = traj$f2[pointAtWhichFolderSelected] #CHANGES THIS
          positionChose = 'F2'
        }
        #positionPerspectiveInformation = traj[1,11:12] #CHANGE THIS
        positionPerspectiveInformation = traj[1,14] #14th column contains id of othercentric folder
        #subjectPerspectiveDistribution = subjectPerspectiveDistribution + c(0,1*(positionChose==positionPerspectiveInformation)) #CHANGE THIS
        subjectOthercentricDistribution = subjectOthercentricDistribution + c(0, 1*(positionChose==positionPerspectiveInformation))
        
        switch_fold = switch_fold + (length(unique(traj$folder_choice))>=4) #Add counts if number of labels for folder choice column in a trial is equal or larger than 4 (waiting for, F1init/F2init, F1drop/F2drop, F2init)
        
        
        # concatenating ALL trials (make sure to comment out resAll below if you want per trial results)
        resAll = rbind(resAll,data.frame(fl,trial=traj$trial[1],instruction=substr(traj$instruction[1],1,4), 
                                         folder_config=traj$folder_config[1], ppos=traj$ppos[1], RTDV,totalDistanceDV,xFlipDV,AC,
                                         chosen=positionChose,other=traj$other[1]))
      }
          
      if (traj$cond[1]=='shared' & nrow(traj)>0) {
        rawFlashY = traj$y[1]
        rawFlashX = traj$x[1]
        traj$y = -1*(traj$y-600) # mirror image on the y coordinate
        numSamples = length(traj$x) # final entry in the column (vector)
        pointAtWhichFolderSelected = which(traj$folder_choice %in% c('F1init','F2init'))[1]  
        xFlipDV = xflip(traj$x[1:pointAtWhichFolderSelected],0)
        veloc = sqrt(diff(traj$x[1:pointAtWhichFolderSelected])^2+diff(traj$y[1:pointAtWhichFolderSelected])^2)
        totalDistanceDV = sum(veloc)
        
        accel = diff(smooth(veloc))
        AC = sum(sign(accel[2:length(accel)]) * sign(accel[1:(length(accel)-1)]) < 0)
        RTDV = traj$ms[pointAtWhichFolderSelected]-traj$ms[1]   
        
        if (traj$folder_choice[pointAtWhichFolderSelected]=='F1init') {
          #positionChose = traj$f1[pointAtWhichFolderSelected] #THIS NEEDS TO CHANGE
          positionChose = "F1" 
        } else {
          #positionChose = traj$f2[pointAtWhichFolderSelected] #THIS NEEDS TO CHANGE
          positionChose = "F2"
        }      
        subjectError = subjectError + c(0,1*(traj$other[1]!=positionChose)) # CHANGED (cor --> other). increment by 1 if it is TRUE that they chose a diff't object from correct             
        
        switch_fold = switch_fold + (length(unique(traj$folder_choice))>=4) #Add counts if number of labels for folder choice column in a trial is equal or larger than 4 (waiting for, F1init/F2init, F1drop/F2drop, F2init)
        
        
        resAllError = rbind(resAllError,data.frame(fl,trial=traj$trial[1],
                                                   instruction=substr(traj$instruction[1],1,4), folder_config=traj$folder_config[1], ppos=traj$ppos[1], RTDV,AC,
                                                   totalDistanceDV,xFlipDV, chosen=positionChose,cor=traj$other[1],err=(traj$other[1]!=positionChose)))
      }
      
    }
    #resAllPerspectiveDistribution = rbind(resAllPerspectiveDistribution,subjectPerspectiveDistribution) # concatenating ALL results with the objResults of the current subject
    #resAllError = rbind(resAllError,subjectError) # concatenating ALL results with the objResults of the current subject
  }
  
}
```

```
## [1] "504.txt"
## [1] "510.txt"
## [1] "511.txt"
## [1] "518.txt"
## [1] "525.txt"
## [1] "526.txt"
## [1] "527.txt"
## [1] "530.txt"
## [1] "531.txt"
## [1] "534.txt"
## [1] "536.txt"
## [1] "537.txt"
## [1] "540.txt"
## [1] "542.txt"
## [1] "545.txt"
## [1] "549.txt"
## [1] "550.txt"
## [1] "554.txt"
## [1] "562.txt"
## [1] "We have a double-up subject file in: 562.txt"
## [1] "563.txt"
## [1] "570.txt"
## [1] "576.txt"
## [1] "581.txt"
## [1] "584.txt"
## [1] "591.txt"
## [1] "593.txt"
## [1] "595.txt"
## [1] "596.txt"
## [1] "598.txt"
## [1] "We have a double-up subject file in: 598.txt"
## [1] "607.txt"
## [1] "617.txt"
## [1] "618.txt"
## [1] "620.txt"
## [1] "631.txt"
## [1] "638.txt"
## [1] "640.txt"
## [1] "643.txt"
## [1] "645.txt"
## [1] "653.txt"
## [1] "656.txt"
## [1] "659.txt"
## [1] "660.txt"
## [1] "675.txt"
## [1] "677.txt"
## [1] "681.txt"
## [1] "683.txt"
## [1] "686.txt"
## [1] "688.txt"
## [1] "690.txt"
## [1] "691.txt"
## [1] "693.txt"
## [1] "699.txt"
## [1] "701.txt"
## [1] "706.txt"
## [1] "715.txt"
## [1] "719.txt"
## [1] "727.txt"
## [1] "728.txt"
## [1] "729.txt"
## [1] "732.txt"
## [1] "745.txt"
## [1] "We have a double-up subject file in: 745.txt"
## [1] "746.txt"
## [1] "751.txt"
## [1] "752.txt"
## [1] "763.txt"
## [1] "772.txt"
## [1] "780.txt"
## [1] "788.txt"
## [1] "789.txt"
## [1] "793.txt"
## [1] "795.txt"
## [1] "799.txt"
## [1] "802.txt"
## [1] "806.txt"
## [1] "811.txt"
## [1] "813.txt"
## [1] "815.txt"
## [1] "816.txt"
## [1] "We have a double-up subject file in: 816.txt"
## [1] "818.txt"
## [1] "819.txt"
## [1] "820.txt"
## [1] "831.txt"
## [1] "834.txt"
## [1] "838.txt"
## [1] "847.txt"
## [1] "851.txt"
## [1] "854.txt"
## [1] "856.txt"
## [1] "860.txt"
## [1] "861.txt"
## [1] "866.txt"
## [1] "872.txt"
## [1] "874.txt"
## [1] "879.txt"
## [1] "886.txt"
## [1] "892.txt"
## [1] "893.txt"
## [1] "894.txt"
## [1] "897.txt"
```


Save output file with churned data for Exp1b.


```r
save(file='GDD1B_churnedRawTrajectoryData.Rd',resAll,resAllError)
```

Compute proportion of cases for which the participant picked up a folder, dropped it, and picked up another folder (folder switch).  


```r
switch_fold #get number of trials where there is a switch in folder
```

```
## [1] 63
```

```r
percent_switch_fold_Exp1b = switch_fold / (93*40) #93 final participants x 40 trials
percent_switch_fold_Exp1b
```

```
## [1] 0.01693548
```


