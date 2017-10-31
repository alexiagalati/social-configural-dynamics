setwd("~/Documents/[github repositories]/social-configural-dynamics/Code")

# let's laod up the xflip function
source('xflip.R')

sft = 60
cx = 300 - sft/2 # center x with adjustment for T/L
cy = 300 - sft/2 # center y
objPositionX = c(cx-sft/1.5,cx,cx+sft/1.5,cx-sft,cx,cx+sft,cx-sft/1.5,cx,cx+sft/1.5)
objPositionY = c(cy-sft/1.5,cy-sft,cy-sft/1.5,cy,cy,cy,cy+sft/1.5,cy+sft,cy+sft/1.5)


#for EXP 1A
fls = list.files('~/Documents/[github repositories]/social-configural-dynamics/Data/Exp1a',pattern='txt') # get list of txt files
rt = "~/Documents/[github repositories]/social-configural-dynamics/Data/Exp1a/" # root folder location

#for EXP 1B
fls = list.files('~/Documents/[github repositories]/social-configural-dynamics/Data/Exp1b',pattern='txt') # get list of txt files
rt = "~/Documents/[github repositories]/social-configural-dynamics/Data/Exp1b/" # root folder location

resAll = c() # all results -- let's initialize an empty variable
resAllError = c()
resAllPerspectiveDistribution = c()

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
          positionChose = "F1" 
        } else {
          positionChose = "F2"
        }      
        subjectError = subjectError + c(0,1*(traj$other[1]!=positionChose)) # CHANGED (cor --> other). increment by 1 if it is TRUE that they chose a diff't object from correct             
        resAllError = rbind(resAllError,data.frame(fl,trial=traj$trial[1],
                                                   instruction=substr(traj$instruction[1],1,4), folder_config=traj$folder_config[1], ppos=traj$ppos[1], RTDV,AC,
                                                   totalDistanceDV,xFlipDV, chosen=positionChose,cor=traj$other[1],err=(traj$other[1]!=positionChose)))
      }
      
    }
    
  }
  
}


save(file='GDD1A_churnedRawTrajectoryData.Rd',resAll,resAllError)
save(file='GDD1B_churnedRawTrajectoryData.Rd',resAll,resAllError)

#load('GDD1A_churnedRawTrajectoryData.Rd')
#load('GDD1B_churnedRawTrajectoryData.Rd')






