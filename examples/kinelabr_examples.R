# load functions
source("R/kinelabr_functions.R")

# set kinelab path
# unix style path and no trailing / important
kinelab.path = "U:/Born In Bradford - Confidential/New Wave/Kinelab"

# subset dailydirs
dailydir <- tail(getDailyDirs(kinelab.path))

# get raw ckat data directories
ckat.dir <- getCKATDirs(kinelab.path)
ckat.dir.sub <- getCKATDirs(kinelab.path, dailydir)

# get raw WM data directories
wm.dir <- getWMDirs(kinelab.path)
wm.dir.sub <- getWMDirs(kinelab.path, dailydir)

# get summary data directories
summ.dir <- getSummaryDataDirs(kinelab.path)
summ.dir.sub <- getSummaryDataDirs(kinelab.path, dailydir)

# get feedback report directories
rep.dir <- getFeedbackReportDirs(kinelab.path)
rep.dir.sub <- getFeedbackReportDirs(kinelab.path, dailydir)

# get recorded data directories
rd.dir <- getRecDataDirs(kinelab.path)
rd.dir.sub <- getRecDataDirs(kinelab.path, dailydir)

# get ckat session info
# we need to pass a CKAT data folders data frame
ckat.info <- readDirSessInfo(ckat.dir)

# get wm session info
# we need to pass a WM data folders data frame
wm.info <- readDirSessInfo(wm.dir)

# read recorded data session info
# we need to pass a recorded_data session folders data.frame
rd.info <- readRecDataSessInfo(rd.dir)

# read ckat data
# we need to pass a vector of full paths to CKAT data
ckat.data <- readCKATData(ckat.dir$Path)

# read wm data
# we need to pass a vector of full paths to WM data
wm.data <- readWMScores(wm.dir$Path)

# read wm percentiles
# we need to pass a vector of full paths to WM percentiles
wm.percentiles <- readWMPercentiles(wm.dir$Path)

# example of combining a call to get ckat data plus session info
ckat.all <- cbind(readDirSessInfo(ckat.dir), readCKATData(ckat.dir$Path))

# example of combining a call to get WM data plus session info
wm.all <- cbind(readDirSessInfo(wm.dir), readWMScores(wm.dir$Path), readWMPercentiles(wm.dir$Path))

# add PS task accuracy count to WM scores
wm.ps <- addPSAccuracyToWMData(wm.data, wm.dir$Path)
# or to full linkable WM data frame
wm.allps <- addPSAccuracyToWMData(wm.all, wm.dir$Path)


# read summary data
# we need to pass a vector of full paths to summary data
summ.all <- readSummaryData(summ.dir)

# merge different versions of summary data to one data frame
summ.merged <- mergeSummaryDataVersions(summ.all)

# example of combining a call to get WM data, adding PS accuracy and session info
# it would be quicker to store the session info separately, but this is just for illustration
wm.pslinkable <- cbind(readDirSessInfo(wm.dir), addPSAccuracyToWMData(readWMScores(wm.dir$Path), wm.dir$Path))

# linking WM data to UPN or PSY
wm.linked <- mergeUPNorPSY(summ.merged, wm.pslinkable)

# get feedback report files info
rep.files <- readFeedbackReportInfo(rep.dir)

# add age to summary data
# pass summary data as data.frame and it returns with added age columns
summ.ages <- addAgeToSummaryData(summ.merged)

# add feedback report filename to summary data
# assumes summary data merged across header versions
summ.fb <- addFbReportToSummaryData(summ.merged, rep.files)

# add ID (UPN or Psy number) to recorded data
# pass recorded data as data.frame and match data which can be the summary data
# the match data requires First.Name, Last.Name, Date.of.Birth and UPN or PSY
rd.upn <- addIDToRecData(rd.info, summ.merged)




# run some test deletions on different data types
kinelab.path.test = "U:/Born In Bradford - Confidential/New Wave/KinelabTest"
UPN = c("M111111111111", "U222222222222", "R333333333333")
cat(deleteCKATData(kinelab.path = kinelab.path.test, UPN = UPN))
cat(deleteWMData(kinelab.path = kinelab.path.test, UPN = UPN))
cat(deleteRecData(kinelab.path = kinelab.path.test, UPN = UPN))
cat(deleteBiBData(kinelab.path = kinelab.path.test, UPN = UPN))

# test run delete all (copying only)
deleteAllByUPN(kinelab.path = kinelab.path.test, UPN = UPN, test = T)
# real thing (deleting also)
deleteAllByUPN(kinelab.path = kinelab.path.test, UPN = UPN, test = F)

