setwd("~/Documents/[github repositories]/social-configural-dynamics/Code")

# let's laod up the xflip function
source('xflip.R')

sft = 60
cx = 300 - sft/2 # center x with adjustment for T/L
cy = 300 - sft/2 # center y
objPositionX = c(cx-sft/1.5,cx,cx+sft/1.5,cx-sft,cx,cx+sft,cx-sft/1.5,cx,cx+sft/1.5)
objPositionY = c(cy-sft/1.5,cy-sft,cy-sft/1.5,cy,cy,cy,cy+sft/1.5,cy+sft,cy+sft/1.5)

#for EXP 2
fls = list.files('~/Documents/[github repositories]/social-configural-dynamics/Data/Exp2',pattern='txt') # get list of txt files
rt = "~/Documents/[github repositories]/social-configural-dynamics/Data/Exp2/" # root folder location

resAll = c() # all results -- let's initialize an empty variable
resAllError = c()
resAllPerspectiveDistribution = c()

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
        # concatenating ALL trials (make sure to comment out resAll below if you want per trial results)
        resAll = rbind(resAll,data.frame(fl,trial=traj$trial[1],sound=substr(traj$sound[1],1,4), 
                                         config=traj$config[1],ppos=traj$ppos[1], RTDV,totalDistanceDV,xFlipDV,AC,optimalDist, #AG added ppos
                                         chosen=positionChose,ego=traj$ego[1],other=traj$other[1]))
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
        #subjectError = subjectError + c(0,1*(traj$cor[1]!=positionChose)) # increment by 1 if it is TRUE that they chose a diff't object from correct             
        resAllError = rbind(resAllError,data.frame(fl,trial=traj$trial[1],
                                                   sound=substr(traj$sound[1],1,4), config=traj$config[1], ppos=traj$ppos[1], RTDV,AC,optimalDist, 
                                                   totalDistanceDV,xFlipDV, err=(traj$cor[1]!=positionChose),
                                                   chosen=positionChose, cor=traj$cor[1], ego=traj$ego[1],
                                                   other=traj$other[1]))      
      }
      
    }
    resAllPerspectiveDistribution = rbind(resAllPerspectiveDistribution,subjectPerspectiveDistribution) # concatenating ALL results with the objResults of the current subject
  }
  
}

save(file='GDD2_churnedRawTrajectoryData.Rd',resAll,resAllError)

#load('GDD2_churnedRawTrajectoryData.Rd')









