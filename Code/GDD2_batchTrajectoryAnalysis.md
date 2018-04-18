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
#for EXP 2
fls = list.files('~/Documents/[github repositories]/social-configural-dynamics/Data/Exp2',pattern='txt') # get list of txt files
rt = "~/Documents/[github repositories]/social-configural-dynamics/Data/Exp2/" # root folder location
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
  colnames(testData) = c("listn","folder_choice","cond","sound","ppos","f1","f2","o3","config","cor","ego","other","trial","ms","t","x","y")  
  subjectPerspectiveDistribution = c(which(fls==fl),0,0) # keep track of the object chosen by PERSPECTIVE; initialize at 0/0; incremented for each subject
  subjectError = c(which(fls==fl),0)
  
  
  # check to make sure it's not a double up data set
  if (sum(diff(testData$ms)<0)>0) {
    print(paste('We have a double-up subject file in:',fl))
  } 
  else {
    for (i in 1:48) {
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
          positionChose = traj$f1[pointAtWhichFolderSelected]
        } else {
          positionChose = traj$f2[pointAtWhichFolderSelected]
        }
        # compute optimal straight line distance
        optimalDist = sqrt(sum((rawFlashX-objPositionX[positionChose]-33.5)^2+(rawFlashY-objPositionY[positionChose]-51)^2))
        positionPerspectiveInformation = traj[1,11:12]
        subjectPerspectiveDistribution = subjectPerspectiveDistribution + c(0,1*(positionChose==positionPerspectiveInformation))
        
        switch_fold = switch_fold + (length(unique(traj$folder_choice))>=4) #Add counts if number of labels for folder choice column in a trial is equal or larger than 4 (waiting for, F1init/F2init, F1drop/F2drop, F2init)
        
        # concatenating ALL trials (make sure to comment out resAll below if you want per trial results)
        resAll = rbind(resAll,data.frame(fl,trial=traj$trial[1],sound=substr(traj$sound[1],1,4), 
                                         config=traj$config[1],RTDV,totalDistanceDV,xFlipDV,AC,optimalDist,ppos=traj$ppos[1],
                                         chosen=positionChose,ego=traj$ego[1],other=traj$other[1],f1=traj[1,6],f2=traj[1,7],o3=traj[1,8]))
      }
          
      if (traj$cond[1]=='control' & nrow(traj)>0) {
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
          positionChose = traj$f1[pointAtWhichFolderSelected]
        } else {
          positionChose = traj$f2[pointAtWhichFolderSelected]
        }      
        # compute optimal straight line distance
        optimalDist = sqrt(sum((rawFlashX-objPositionX[positionChose]-33.5)^2+(rawFlashY-objPositionY[positionChose]-51)^2))
        
        switch_fold = switch_fold + (length(unique(traj$folder_choice))>=4) #Add counts if number of folder_choice labels in a trial is equal to or larger than 4 (waiting for, F1init/F2init, F1drop/F2drop, F2init)
        
        #subjectError = subjectError + c(0,1*(traj$cor[1]!=positionChose)) # increment by 1 if it is TRUE that they chose a diff't object from correct             
        resAllError = rbind(resAllError,data.frame(fl,trial=traj$trial[1],
                                                   sound=substr(traj$sound[1],1,4), config=traj$config[1], RTDV,AC,optimalDist,ppos=traj$ppos[1],
                                                   totalDistanceDV,xFlipDV, err=(traj$cor[1]!=positionChose),
                                                   chosen=positionChose, cor=traj$cor[1], ego=traj$ego[1],
                                                   other=traj$other[1],f1=traj[1,6],f2=traj[1,7],o3=traj[1,8]))      
      }
      
    }
    resAllPerspectiveDistribution = rbind(resAllPerspectiveDistribution,subjectPerspectiveDistribution) # concatenating ALL results with the objResults of the current subject
    #resAllError = rbind(resAllError,subjectError) # concatenating ALL results with the objResults of the current subject
  }
  
}
```

```
## [1] "1004.txt"
## [1] "1005.txt"
## [1] "1009.txt"
## [1] "1010.txt"
## [1] "1012.txt"
## [1] "1015.txt"
## [1] "1018.txt"
## [1] "1021.txt"
## [1] "1023.txt"
## [1] "1035.txt"
## [1] "1036.txt"
## [1] "1042.txt"
## [1] "1043.txt"
## [1] "1049.txt"
## [1] "1056.txt"
## [1] "1067.txt"
## [1] "1079.txt"
## [1] "1082.txt"
## [1] "1091.txt"
## [1] "We have a double-up subject file in: 1091.txt"
## [1] "1095.txt"
## [1] "1099.txt"
## [1] "1100.txt"
## [1] "1106.txt"
## [1] "1109.txt"
## [1] "1118.txt"
## [1] "1123.txt"
## [1] "1128.txt"
## [1] "1135.txt"
## [1] "1136.txt"
## [1] "1138.txt"
## [1] "1141.txt"
## [1] "1155.txt"
## [1] "1162.txt"
## [1] "1166.txt"
## [1] "1171.txt"
## [1] "1178.txt"
## [1] "1181.txt"
## [1] "1182.txt"
## [1] "1183.txt"
## [1] "1184.txt"
## [1] "1191.txt"
## [1] "1192.txt"
## [1] "1196.txt"
## [1] "1197.txt"
## [1] "1213.txt"
## [1] "1215.txt"
## [1] "1216.txt"
## [1] "1228.txt"
## [1] "1231.txt"
## [1] "1233.txt"
## [1] "1235.txt"
## [1] "1240.txt"
## [1] "1244.txt"
## [1] "1250.txt"
## [1] "1251.txt"
## [1] "1257.txt"
## [1] "1264.txt"
## [1] "1271.txt"
## [1] "1273.txt"
## [1] "1277.txt"
## [1] "1284.txt"
## [1] "1285.txt"
## [1] "1294.txt"
## [1] "1301.txt"
## [1] "1311.txt"
## [1] "1313.txt"
## [1] "1314.txt"
## [1] "1316.txt"
## [1] "1323.txt"
## [1] "1328.txt"
## [1] "1334.txt"
## [1] "1337.txt"
## [1] "We have a double-up subject file in: 1337.txt"
## [1] "1338.txt"
## [1] "1339.txt"
## [1] "1341.txt"
## [1] "1345.txt"
## [1] "1352.txt"
## [1] "1363.txt"
## [1] "1365.txt"
## [1] "1374.txt"
## [1] "1376.txt"
## [1] "We have a double-up subject file in: 1376.txt"
## [1] "1404.txt"
## [1] "1409.txt"
## [1] "1416.txt"
## [1] "1419.txt"
## [1] "1434.txt"
## [1] "1442.txt"
## [1] "1446.txt"
## [1] "1452.txt"
## [1] "1455.txt"
## [1] "1460.txt"
## [1] "1461.txt"
## [1] "1464.txt"
## [1] "1472.txt"
## [1] "1481.txt"
## [1] "1482.txt"
## [1] "1489.txt"
## [1] "1512.txt"
## [1] "1515.txt"
## [1] "1520.txt"
## [1] "1539.txt"
## [1] "1541.txt"
## [1] "1549.txt"
## [1] "1550.txt"
## [1] "1551.txt"
## [1] "1553.txt"
## [1] "1563.txt"
## [1] "1566.txt"
## [1] "1583.txt"
## [1] "1588.txt"
## [1] "1592.txt"
## [1] "1594.txt"
## [1] "1595.txt"
## [1] "1596.txt"
## [1] "1601.txt"
## [1] "1605.txt"
## [1] "1612.txt"
## [1] "1624.txt"
## [1] "1629.txt"
## [1] "1631.txt"
## [1] "1642.txt"
## [1] "1650.txt"
## [1] "1652.txt"
## [1] "1654.txt"
## [1] "1662.txt"
## [1] "1663.txt"
## [1] "1676.txt"
## [1] "1677.txt"
## [1] "1679.txt"
## [1] "We have a double-up subject file in: 1679.txt"
## [1] "1680.txt"
## [1] "1685.txt"
## [1] "1689.txt"
## [1] "1695.txt"
## [1] "1698.txt"
## [1] "1704.txt"
## [1] "1711.txt"
## [1] "1718.txt"
## [1] "1722.txt"
## [1] "We have a double-up subject file in: 1722.txt"
## [1] "1723.txt"
## [1] "1736.txt"
## [1] "1737.txt"
## [1] "1745.txt"
## [1] "1754.txt"
## [1] "1758.txt"
## [1] "1761.txt"
## [1] "1762.txt"
## [1] "1764.txt"
## [1] "1768.txt"
## [1] "1769.txt"
## [1] "1771.txt"
## [1] "1778.txt"
## [1] "1797.txt"
## [1] "1808.txt"
## [1] "1814.txt"
## [1] "1815.txt"
## [1] "1816.txt"
## [1] "1817.txt"
## [1] "1821.txt"
## [1] "1825.txt"
## [1] "1826.txt"
## [1] "1828.txt"
## [1] "1836.txt"
## [1] "1847.txt"
## [1] "1859.txt"
## [1] "1865.txt"
## [1] "1872.txt"
## [1] "1876.txt"
## [1] "1884.txt"
## [1] "1890.txt"
## [1] "1897.txt"
## [1] "1899.txt"
## [1] "1901.txt"
## [1] "1903.txt"
## [1] "1905.txt"
## [1] "1909.txt"
## [1] "1915.txt"
## [1] "1923.txt"
## [1] "1928.txt"
## [1] "1938.txt"
## [1] "1942.txt"
## [1] "1946.txt"
## [1] "1950.txt"
## [1] "1958.txt"
## [1] "1973.txt"
## [1] "1975.txt"
## [1] "1978.txt"
## [1] "1981.txt"
## [1] "1986.txt"
## [1] "1994.txt"
```

Save output file with churned data for Experiment 2.


```r
save(file='churnedRawTrajectoryData_Exp2.Rd',resAll,resAllError)
```

Compute proportion of cases for which the participant picked up a folder, dropped it, and picked up another folder (folder switch).


```r
switch_fold #get number of trials where there is a switch in folder
```

```
## [1] 232
```

```r
percent_switch_fold = switch_fold / (184*48)
percent_switch_fold
```

```
## [1] 0.02626812
```






