# return character vector of daily sync directories
getDailyDirs <- function(kinelab.path) {
    
    dailypaths <- list.dirs(kinelab.path, full.names = T, recursive = F)
    
    # only pickup valid date folders, i.e. ignore bin, DataIssues etc
    dailypaths <- dailypaths[grep("/(19|20)\\d\\d[- /.](0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])$", dailypaths)]

    return(dailypaths)
    
}

# return character vector of tablet directories
getTabletDirs <- function(kinelab.path = character(0), dailydir = character(0)) {
    
    if(length(dailydir) == 0) dailydir <- getDailyDirs(kinelab.path)
    
    tabletpaths <- list.dirs(dailydir, full.names = T, recursive = F)
    
    return(tabletpaths)
    
}


# return integer n path elements in path once split
getNPathElements <- function(path) {
    
    return(lengths(strsplit(path, "/", fixed = T)))
    
}

# return data.frame of Summary "Database data" directories
getSummaryDataDirs <- function(kinelab.path, dailydir = character(0)) {
    
    tabletdir <- getTabletDirs(kinelab.path, dailydir = dailydir)

    dbdatadirs <- paste0(tabletdir, "/BiB Kinelab Output/BiB data/Database data")
    
    dbdatadirs <- makePathDataFrame(dbdatadirs, kinelab.path)
    
    return(dbdatadirs)
    
}

# return data.frame of feedback report directories
getFeedbackReportDirs <- function(kinelab.path, dailydir = character(0)) {
    
    tabletdir <- getTabletDirs(kinelab.path, dailydir = dailydir)
    
    reportDirs <- paste0(tabletdir, "/BiB Kinelab Output/BiB data/Feedback reports")
    
    reportSchoolDirs <- list.dirs(reportDirs, full.names = T, recursive = F)
    
    reportSchoolDirs <- makePathDataFrame(reportSchoolDirs, kinelab.path)
    
    return(reportSchoolDirs)
    
}

# return data.frame of CKAT data directories
getCKATDirs <- function(kinelab.path, dailydir = character(0)) {
    
    tabletdir <- getTabletDirs(kinelab.path, dailydir = dailydir)

    ckatdirs <- list.dirs(paste0(tabletdir, "/BiB Kinelab Output/Processed data"), 
                           full.names = T, recursive = F)
    
    ckatdirs <- makePathDataFrame(ckatdirs, kinelab.path)
    
    return(ckatdirs)
    
}


# return data.frame of WM directories
getWMDirs <- function(kinelab.path, dailydir = character(0)) {
    
    tabletdir <- getTabletDirs(kinelab.path, dailydir = dailydir)

    # folder name changed from WM to EF Tasks in Sep 2016
    # so we search for both and union result
    WMdirs <- list.dirs(paste0(tabletdir, "/BiB Kinelab Output/WM"), 
                                full.names = T, recursive = F)
    
    EFdirs <- list.dirs(paste0(tabletdir, "/BiB Kinelab Output/EF Tasks"), 
                        full.names = T, recursive = F)

    if(length(WMdirs) > 0) WMdirs <- makePathDataFrame(WMdirs, kinelab.path)
    
    if(length(EFdirs) > 0) EFdirs <- makePathDataFrame(EFdirs, kinelab.path)
    
    return(rbind(WMdirs, EFdirs))
    
}


# return data.frame of EF Tasks directories
# alias to getWMDirs
getEFDirs <- function(kinelab.path, dailydir = character(0)) {
    
    return(getWMDirs(kinelab.path, dailydir = dailydir))
    
}


# return data.frame of recorded data directories
getRecDataDirs <- function(kinelab.path, dailydir = character(0)) {
    
    tabletdir <- getTabletDirs(kinelab.path, dailydir = dailydir)

    RDdatedirs <- list.dirs(paste0(tabletdir, "/BiB Kinelab Output/recorded_data"),
                             full.names = T, recursive = F)
    
    RDdirs <- list.dirs(RDdatedirs, full.names = T, recursive = F)
    
    RDdirs <- makePathDataFrame(RDdirs, kinelab.path)
    
    return(RDdirs)
    
}

# split vector of tablet directories into path elements and return as matrix
# selectCols is an integer vector of column positions to return
getPathElements <- function(path, selectCols = NULL) {
    
    pathelements <- strsplit(path, "/", fixed = T)
    
    # number of columns (elements) is based on the first path only
    # could check for consistency in directory structure by checking that lengths(pathelements)
    # is uniform
    len <- length(pathelements[[1]])
    
    # convert to matrix
    syncinfo <- matrix(unlist(pathelements), ncol = len, byrow = T)
    
    if(!is.null(selectCols)) syncinfo <- syncinfo[ , selectCols]
    
    return(syncinfo)
    
}

# return data.frame of DailyDir, TabletDir and Path
# needs kinelab.path as reference
makePathDataFrame <- function(convertpath, kinelab.path) {
    
    dailycol <- getNPathElements(kinelab.path) + 1
    tabletcol <- getNPathElements(kinelab.path) + 2
    
    pathelements <- getPathElements(convertpath, c(dailycol, tabletcol))
    
    pathDF <- data.frame(DailyDir = pathelements[ , 1], TabletDir = pathelements[ , 2],
                         Path = convertpath, stringsAsFactors = F)
    
    return(pathDF)
    
}

# read multiple Subject Info csv files from recorded_data session folders
# return as data.frame
# inserts a blank row (NAs) when file is missing
loadSubjectInfoCsv <- function(csvfullpath, filename, subinfDF, includePath = T) {
    
    pb <- txtProgressBar(min = 0, max = nrow(csvfullpath), style = 3)
    
    session.number.col <- 4
    test.date.col <- 3
    sub.inf.csv.col <- 13
    
    f <- paste0(csvfullpath$Path, "/", filename)
    
    for(x in 1:nrow(csvfullpath)) {
        
        setTxtProgressBar(pb, x)
        
        if(file.exists(f[x])) {
            
            # check whether the header is present by looking for the first field name
            csv.header <- substr(readLines(f[x], n = 1), 1, 9) == "Device ID"
            
            raw <- read.csv(f[x],
                            header = csv.header,
                            stringsAsFactors = F,
                            colClasses = c("character"))
            
            if (nrow(raw) > 1) warning(paste("\nFile", f[x], "has", nrow(raw), "data rows.\n"))
            
            # pad columns if necessary to fit output data frame
            length(raw) <- max(length(raw), ncol(subinfDF))
            is.na(raw) <- raw == "NULL"
            
            # add subject info file indicator
            raw[sub.inf.csv.col] <- 1
            
        } else {
            
            raw <- rep(NA, ncol(subinfDF))
            
            # get test date and session number from Path
            path.info <- unlist(strsplit(f[x], "/"))
            elements <- length(path.info)
            length.sess <- nchar(path.info[elements - 1])
            session <- substr(path.info[elements - 1], length.sess, length.sess)
            test.date <- path.info[elements - 2]
            test.date <- gsub("_", "/", test.date, fixed = T)

            # replace
            raw[session.number.col] <- session
            raw[test.date.col] <- test.date
            # add subject info file indicator
            raw[sub.inf.csv.col] <- 0
            
        }
        
        # add new row
        subinfDF[nrow(subinfDF) + 1, ] <- raw
        
    }
    
    close(pb)
    
    if(includePath) subinfDF <- cbind(subinfDF, data.frame(DailyDir = csvfullpath$DailyDir,
                                                           TabletDir = csvfullpath$TabletDir,
                                                           Path = csvfullpath$Path, 
                                                           stringsAsFactors = F))
    
    return(subinfDF)
    
    
}

# read multiple raw task column .csv and return as data.frame
# inserts a blank row (NAs) when file is missing
loadTaskCsv <- function(csvpath, csvname, rawDF) {
    
    pb <- txtProgressBar(min = 0, max = length(csvpath), style = 3)
    
    for(x in 1:length(csvpath)) {
        
        setTxtProgressBar(pb, x)
        
        taskfiles <- paste0(csvpath[x], "/", csvname)
        
        if(any(file.exists(taskfiles))) {
            
            raw <- read.csv(taskfiles[which(file.exists(taskfiles))],
                            header = F,
                            row.names = 1)
            
            t.raw <- t(raw)
            
            # pad columns if necessary to fit output data frame
            length(t.raw) <- max(length(t.raw), ncol(rawDF))
            is.na(t.raw) <- t.raw == "NULL"
            
        } else {
            
            t.raw <- rep(NA, ncol(rawDF))
            
        }
        
        # add new row
        rawDF[nrow(rawDF) + 1, ] <- t.raw
        
    }
    
    close(pb)
    
    return(rawDF)
    
}


# read multiple raw trials .csv and return as data.frame
# set tab_delim to T to read a tab delimited file
loadTrialsCsv <- function(csvpath, csvname, rawDF, tab_delim = F) {
    
    pb <- txtProgressBar(min = 0, max = length(csvpath), style = 3)
    
    for(x in 1:length(csvpath)) {
        
        setTxtProgressBar(pb, x)
        
        trialsfiles <- paste0(csvpath[x], "/", csvname)
        
        trialsfile <- trialsfiles[which(file.exists(trialsfiles))]
        
        if(length(trialsfile) > 0 && length(readLines(trialsfile)) > 2) {
            
            if(!tab_delim) raw <- readr::read_csv(trialsfile, col_types = readr::cols(.default = "c"))
            if(tab_delim) raw <- readr::read_tsv(trialsfile, col_types = readr::cols(.default = "c"))
            # add new data
            raw$Path <- csvpath[x]
            rawDF <- dplyr::bind_rows(rawDF, raw)
            
        } else {
            
            #file not found
            
        }
        
    }
    
    close(pb)
    
    return(rawDF)
    
}


# read multiple csvs extracting values from into rows as specified in filterfile and return as data.frame
# set tab_delim to T to read a tab delimited file
loadFilteredCsv <- function(csvpath, csvname, filterfile, tab_delim = F) {
    
    rawDF <- NULL
    
    filter <- readr::read_csv(filterfile)
    
    pb <- txtProgressBar(min = 0, max = length(csvpath), style = 3)
    
    for(x in 1:length(csvpath)) {
        
        setTxtProgressBar(pb, x)
        
        trialsfiles <- paste0(csvpath[x], "/", csvname)
        
        trialsfile <- trialsfiles[which(file.exists(trialsfiles))]
        
        
        if(!tab_delim) raw <- readr::read_csv(trialsfile, col_names = FALSE, col_types = readr::cols(.default = "c"))
        if(tab_delim) raw <- readr::read_tsv(trialsfile, col_names = FALSE, col_types = readr::cols(.default = "c"))
        
        rawm <- as.matrix(raw)
        
        newrow <- character(0)
        trunc <- "0"
        
        for(c in 1:nrow(filter)) {
            if(filter$row[c] <= nrow(rawm) & filter$col[c] <= ncol(rawm)) {
                newrow <- c(newrow, rawm[filter$row[c], filter$col[c]])
            } else {
                newrow <- c(newrow, NA)
                trunc <- "1"
            }
        }
        
        names(newrow) <- filter$name
        
        newrow <- c(newrow, "truncated" = trunc, "Path" = csvpath[x])
        
        rawDF <- dplyr::bind_rows(rawDF, newrow)
        
    }
    
    close(pb)
    
    return(rawDF)
    
}


# read multiple raw summary data files and return as list of data.frames
# one per version of header
loadSummaryTxt <- function(summfullpath, filename, rawDF, header, includePath = T) {
    
    pb <- txtProgressBar(min = 0, max = nrow(summfullpath), style = 3)
    
    if(includePath) rawDF <- lapply(rawDF, FUN = function(x) cbind(x, data.frame(DailyDir = character(0), TabletDir = character(0),
                                                                                 Path = character(0), stringsAsFactors = F)))

    f <- paste0(summfullpath$Path, "/", filename)
    
    for(x in 1:length(f)) {
        
        setTxtProgressBar(pb, x)
        
        if(file.exists(f[x])) {
            
            headerv <- which(header == readLines(f[x], 1))
            
            raw <- read.delim(f[x], 
                              colClasses = "character", 
                              stringsAsFactors = F)
            
        }
        
        # add path info if wanted
        if (includePath) {
            raw$DailyDir <- summfullpath$DailyDir[x]
            raw$TabletDir <- summfullpath$TabletDir[x]
            raw$Path <- summfullpath$Path[x]
        }
        
        # add new rows
        rawDF[[headerv]] <- rbind(rawDF[[headerv]], raw)
        
    }

    close(pb)
    
    # drop dummy column (there is a trailing comma in the header)
    rawDF <- lapply(rawDF, FUN = function(x) x[ , !(colnames(x) %in% c("X"))])

    return(rawDF)
    
}

# read feedback report info from all report directories and return as data.frame
loadFeedbackReportInfo <- function(rep.dir, rep.df, includePath = T) {
    
    
    pb <- txtProgressBar(min = 0, max = nrow(rep.dir), style = 3)
    
    if(includePath) rep.df <- cbind(rep.df, data.frame(DailyDir = character(0), TabletDir = character(0),
                                                       Path = character(0), stringsAsFactors = F))
    
    for(x in 1:nrow(rep.dir)) {
        
        setTxtProgressBar(pb, x)
        
        raw <- data.frame(filename = list.files(rep.dir$Path[x], pattern = "(pdf|PDF)$"), stringsAsFactors = F)
        
        raw$PSY.number <- ifelse(substr(raw$filename, 1, 3) == "PSY", substr(raw$filename, 1, 9), NA)
        
        raw$Full.Name <- ifelse(substr(raw$filename, 1, 3) == "PSY",
                               substr(raw$filename, 11, nchar(raw$filename) - 4),
                               substr(raw$filename, 1, nchar(raw$filename) - 4))
        
        # add path info if wanted
        if (includePath & nrow(raw) > 0) {
            raw$DailyDir <- rep.dir$DailyDir[x]
            raw$TabletDir <- rep.dir$TabletDir[x]
            raw$Path <- rep.dir$Path[x]
        }
        
        # add new rows
        rep.df <- rbind(rep.df, raw)
        
    }
    
    close(pb)
    
    return(rep.df)
    
    
}

# return session information from "session xxx/Subject Info.csv" files
readRecDataSessInfo <- function(dataDirs, includePath = T) {
    
    SubjInfoName <- "Subject_Info.csv"
    
    sessinfo <- data.frame(Machine.ID = character(0),
                           Full.Name = character(0),
                           Test.Date = character(0),
                           Session.Number = character(0),
                           Session.Type = character(0),
                           School.Name = character(0),
                           Handedness = character(0),
                           Gender = character(0),
                           Date.of.Birth = character(0),
                           Notes.Tracking = character(0),
                           Notes.Aiming = character(0),
                           Notes.Tracing = character(0),
                           Subject.Info.csv = character(0),
                           stringsAsFactors = F)
    
    sessinfo <- loadSubjectInfoCsv(dataDirs, SubjInfoName, sessinfo, includePath)
    
    return(sessinfo)
    
    
    
}

# return session information for a character vector of Processed data or WM directories
# returns data.frame
# can choose whether dates are returned as.Date or as.character
readDirSessInfo <- function(dataDirs, dateAsString = T, includePath = T) {
    
    pb <- txtProgressBar(min = 0, max = nrow(dataDirs), style = 3)
    
    if (includePath) {
        sessinfo <- data.frame(PSY.number = character(0), UPN = character(0), Date = character(0),
                               Time = character(0), DailyDir = character(0),
                               TabletDir = character(0), Path = character(0),
                               stringsAsFactors = F)
    } else {
        sessinfo <- data.frame(PSY.number = character(0), UPN = character(0), Date = character(0),
                               Time = character(0), stringsAsFactors = F)
    }
    
    for(x in 1:nrow(dataDirs)) {
        
        setTxtProgressBar(pb, x)

        sessDir <- unlist(strsplit(dataDirs$Path[x], "_"))
        Nelements <- length(sessDir)
        
        # get date elements
        day <- substr(sessDir[Nelements - 1],1,2)
        month <- substr(sessDir[Nelements - 1],3,4)
        year <- substr(sessDir[Nelements - 1],5,8)
        
        # get time elements
        hour <- substr(sessDir[Nelements],1,2)
        minute <- substr(sessDir[Nelements],3,4)
        second <- substr(sessDir[Nelements],5,6)
        
        # make date and time strings
        sessdate <- paste0(year,"-",month,"-",day)
        sesstime <- paste0(hour,":",minute,":",second)
        
        # is it a UPN or PSY?
        PupilID <- sessDir[Nelements - 2]
        UPN <- ifelse(substr(PupilID, 1, 3) == "PSY", NA, PupilID)
        PSY.number <- ifelse(substr(PupilID, 1, 3) == "PSY", PupilID, NA)
        
        # prepare new row
        nextsessinfo <- c(PSY.number, UPN, sessdate, sesstime)
        if(includePath) nextsessinfo <- c(nextsessinfo, NA, NA, NA)

        # add new row
        sessinfo[nrow(sessinfo) + 1, ] <- nextsessinfo
    
    }
    
    close(pb)
    
    # add path info if wanted
    if (includePath) {
        sessinfo$DailyDir <- dataDirs$DailyDir
        sessinfo$TabletDir <- dataDirs$TabletDir
        sessinfo$Path <- dataDirs$Path
    }
    
    # convert date if wanted
    if(!dateAsString) sessinfo$Date <- as.Date(sessinfo$Date)
    
    return(sessinfo)
    
}

# read raw CKAT data from a character vector of paths and return as data.frame
readCKATData <- function(sesspath) {
    
    rawCKATName <- "Battery_DVs.csv"
    
    CKATData <- data.frame(Aim_Median_Reciprocal_PLT_Embedded = character(0),
                           Aim_Median_Reciprocal_PLT_Baseline = character(0),
                           Aim_Median_Reciprocal_PLT_Jump = character(0),
                           Aim_Median_Log_NJ_Embedded = character(0),
                           Aim_Median_Log_NJ_Baseline = character(0),
                           Aim_Median_Log_NJ_Jump = character(0),
                           Trace_Reciprocal_PPA_Shape_A = character(0),
                           Trace_Reciprocal_PPA_Shape_B = character(0),
                           Track_No_Back_Recip_RMSE_Slow = character(0),
                           Track_No_Back_Recip_RMSE_Med = character(0),
                           Track_No_Back_Recip_RMSE_Fast = character(0),
                           Track_No_Back_Recip_PA_Slow = character(0),
                           Track_No_Back_Recip_PA_Med = character(0),
                           Track_No_Back_Recip_PA_Fast = character(0),
                           Track_With_Back_Recip_RMSE_Slow = character(0),
                           Track_With_Back_Recip_RMSE_Med = character(0),
                           Track_With_Back_Recip_RMSE_Fast = character(0),
                           Track_With_Back_Recip_PA_Slow = character(0),
                           Track_With_Back_Recip_PA_Med = character(0),
                           Track_With_Back_Recip_PA_Fast = character(0),
                           stringsAsFactors = F
    )
    
    CKATData <- loadTaskCsv(sesspath, rawCKATName, CKATData)
    
    # add count of numeric DVs
    CKATData$NumericCKATDVs <- apply(CKATData, 1, 
                                   FUN = function(x) {
                                       sum(ifelse(suppressWarnings(!is.na(as.numeric(x))) & x != "Inf" & x != "NaN", 1, 0))
                                       })
    
    return(CKATData)
    
}


# read CKAT session stats data from a character vector of recorded data paths and return as data.frame
readCKATSessionStats <- function(sesspath, filterfile) {
    
    rawFilename <- "Session_Stats.csv"
    
    CKATData <- loadFilteredCsv(sesspath, rawFilename, filterfile)
    
    return(CKATData)
    
}


# read raw WM data from a character vector of paths and return as data.frame
# missingness says whether we want missingness indicators
readWMScores <- function(sesspath, missingness = F) {
    
    rawWMName <- c("WM scores.csv", "EF scores.csv")
    
    WMData <- data.frame(FDR = character(0),
                         BDR = character(0),
                         Corsi = character(0),
                         PS = character(0),
                         Inhibition = character(0),
                         stringsAsFactors = F
    )
    
    WMData <- loadTaskCsv(sesspath, rawWMName, WMData)
    
    # add missingness if required
    if(missingness) {
        
        is.missing <- function(x) ifelse(suppressWarnings(is.na(as.numeric(x))) | x == "Inf" | x == "NaN", 1, 0)
        
        WMData$FDR.miss <- is.missing(WMData$FDR)
        WMData$BDR.miss <- is.missing(WMData$BDR)
        WMData$Corsi.miss <- is.missing(WMData$Corsi)
        WMData$PS.miss <- is.missing(WMData$PS)
        WMData$Inhibition.miss <- is.missing(WMData$Inhibition)
        
    }
    
    return(WMData)
    
}

# read raw WM percentiles from a character vector of paths and return as data.frame
# missingness says whether we want missingness indicators
readWMPercentiles <- function(sesspath, missingness = F) {
    
    rawWMName <- c("WM percentiles.csv", "EF percentiles.csv")
    
    WMPercentiles <- data.frame(FDR.perc = character(0),
                         BDR.perc = character(0),
                         Corsi.perc = character(0),
                         PS.perc = character(0),
                         Inhibition.perc = character(0),
                         stringsAsFactors = F
    )
    
    WMPercentiles <- loadTaskCsv(sesspath, rawWMName, WMPercentiles)
    
    # add missingness if required
    if(missingness) {
        
        is.missing <- function(x) ifelse(suppressWarnings(is.na(as.numeric(x))) | x == "Inf" | x == "NaN", 1, 0)
        
        WMPercentiles$FDR.perc.miss <- is.missing(WMPercentiles$FDR.perc)
        WMPercentiles$BDR.perc.miss <- is.missing(WMPercentiles$BDR.perc)
        WMPercentiles$Corsi.perc.miss <- is.missing(WMPercentiles$Corsi.perc)
        WMPercentiles$PS.perc.miss <- is.missing(WMPercentiles$PS.perc)
        WMPercentiles$Inhibition.perc.miss <- is.missing(WMPercentiles$Inhibition.perc)
        
    }
    
    return(WMPercentiles)
    
}

# read raw EF scores: alias to readWMScores
readEFScores <- function(sesspath, missingness = F) {
    
    return(readWMScores(sesspath, missingness))
    
}

# read raw EF percentiles: alias to readWMPercentiles
readEFPercentiles <- function(sesspath, missingness = F) {
    
    return(readWMPercentiles(sesspath, missingness))
    
}

# read raw summary data from a character vector of paths
# returns as list of data frames, one for each version of the header
readSummaryData <- function(summpath, includePath = T) {
    
    rawSummName <- "Database data.txt"
    
    # note additional column due to trailing comma in header
    SummData_v1 <- data.frame(Machine.ID = character(0),
                           Test.Date = character(0),
                           Last.Name = character(0),
                           First.Name = character(0),
                           UPN = character(0),
                           School.Name = character(0),
                           Class.Name = character(0),
                           Date.of.Birth = character(0),
                           Gender = character(0),
                           Handedness = character(0),
                           Aiming = character(0),
                           Tracing = character(0),
                           Tracking = character(0),
                           Overall = character(0),
                           fdr = character(0),
                           bdr = character(0),
                           corsi = character(0),
                           ps = character(0),
                           flanker = character(0),
                           X = integer(0),
                           stringsAsFactors = F)

    SummData_v2 <- data.frame(Machine.ID = character(0),
                              Test.Date = character(0),
                              Last.Name = character(0),
                              First.Name = character(0),
                              PSY.number = character(0),
                              School.Name = character(0),
                              Class.Name = character(0),
                              Date.of.Birth = character(0),
                              Gender = character(0),
                              Handedness = character(0),
                              Aiming = character(0),
                              Tracing = character(0),
                              Tracking = character(0),
                              CKAT.Overall = character(0),
                              FDR = character(0),
                              BDR = character(0),
                              Corsi = character(0),
                              WM.Overall = character(0),
                              PS = character(0),
                              Inhibition = character(0),
                              X = integer(0),
                              stringsAsFactors = F)
    
    Header_v1 <- "Machine ID\tTest Date\tLast Name\tFirst Name\tUPN\tSchool Name\tClass Name\tDate of Birth\tGender\tHandedness\tAiming\tTracing\tTracking\tOverall\tfdr\tbdr\tcorsi\tps\t"
    Header_v2 <- "Machine ID\tTest Date\tLast Name\tFirst Name\tPSY-number\tSchool Name\tClass Name\tDate of Birth\tGender\tHandedness\tAiming\tTracing\tTracking\tCKAT Overall\tFDR\tBDR\tCorsi\tWM Overall\tPS\tInhibition\t"
    
    SummData <- list(SummData_v1, SummData_v2)
    Header <- c(Header_v1, Header_v2)
    
    SummData <- loadSummaryTxt(summpath, rawSummName, SummData, Header, includePath)
    
    return(SummData)
    
}


# read report filenames into df
# rep.dir has to be a full path data.frame
readFeedbackReportInfo <- function(rep.dir, includePath = T) {
    
    rep.info <- data.frame(filename = character(0),
                           PSY.number = character(0),
                           Full.Name = character(0),
                           stringsAsFactors = F)
    
    rep.info <- loadFeedbackReportInfo(rep.dir, rep.info, includePath)
    
    return(rep.info)
    
    
    
}


# read BDR trials data from a character vector of EF paths and return as data.frame
readEFTrialsBDR <- function(sesspath) {
        
    rawBDRName <- "results/BDR.csv"
    
    BDRData <- data.frame(UPN = character(0),
                          PSYNO = character(0),
                          DOB = character(0),
                          dateStr = character(0),
                          testStage = character(0),
                          nTrial = character(0),
                          trialList = character(0),
                          ans = character(0),
                          press = character(0),
                          rt = character(0),
                          length = character(0),
                          Notes = character(0),
                          stringsAsFactors = F
    )
    
    BDRData <- loadTrialsCsv(sesspath, rawBDRName, BDRData)
    
    require(dplyr)
    BDRData <- BDRData %>% group_by(Path) %>% mutate(rownumber = row_number()) %>% ungroup()
    BDRData <- BDRData %>% select(-DOB)

    return(BDRData)
        
}

# read FDR trials data from a character vector of EF paths and return as data.frame
readEFTrialsFDR <- function(sesspath) {
    
    rawFDRName <- "results/FDR.csv"
    
    FDRData <- data.frame(UPN = character(0),
                          PSYNO = character(0),
                          DOB = character(0),
                          dateStr = character(0),
                          testStage = character(0),
                          nTrial = character(0),
                          trialList = character(0),
                          ans = character(0),
                          press = character(0),
                          rt = character(0),
                          length = character(0),
                          Notes = character(0),
                          stringsAsFactors = F
    )
    
    FDRData <- loadTrialsCsv(sesspath, rawFDRName, FDRData)
    
    require(dplyr)
    FDRData <- FDRData %>% group_by(Path) %>% mutate(rownumber = row_number()) %>% ungroup()
    FDRData <- FDRData %>% select(-DOB)
    
    return(FDRData)
    
}

# read Corsi trials data from a character vector of EF paths and return as data.frame
readEFTrialsCorsi <- function(sesspath) {
    
    rawCorsiName <- "results/Corsi.csv"
    
    CorsiData <- data.frame(UPN = character(0),
                          PSYNO = character(0),
                          DOB = character(0),
                          Handedness = character(0),
                          dateStr = character(0),
                          testStage = character(0),
                          nTrial = character(0),
                          trialList = character(0),
                          ans = character(0),
                          press = character(0),
                          rt = character(0),
                          length = character(0),
                          Notes = character(0),
                          stringsAsFactors = F
    )
    
    CorsiData <- loadTrialsCsv(sesspath, rawCorsiName, CorsiData)
    
    require(dplyr)
    CorsiData <- CorsiData %>% group_by(Path) %>% mutate(rownumber = row_number()) %>% ungroup()
    CorsiData <- CorsiData %>% select(-DOB, -Handedness)
    
    return(CorsiData)
    
}

# read PS trials data from a character vector of EF paths and return as data.frame
readEFTrialsPS <- function(sesspath) {
    
    rawPSName <- "results/PS.csv"
    
    PSData <- data.frame(UPN = character(0),
                          PSYNO = character(0),
                          DOB = character(0),
                          dateStr = character(0),
                          testStage = character(0),
                          nTrial = character(0),
                          trialList = character(0),
                          x = character(0),
                          ans = character(0),
                          press = character(0),
                          rt = character(0),
                          length = character(0),
                          Notes = character(0),
                          stringsAsFactors = F
    )
    
    PSData <- loadTrialsCsv(sesspath, rawPSName, PSData)
    
    require(dplyr)
    PSData <- PSData %>% group_by(Path) %>% mutate(rownumber = row_number()) %>% ungroup()
    PSData <- PSData %>% select(-DOB, -x)
    
    return(PSData)
    
}


# read Inhibition trials data from a character vector of EF paths and return as data.frame
readEFTrialsInhibition <- function(sesspath) {
    
    rawInhibitionName <- c("results/flanker.csv", "results/Inhibition.csv")
    
    InhibitionData <- data.frame(UPN = character(0),
                         PSYNO = character(0),
                         DOB = character(0),
                         hand = character(0),
                         dateStr = character(0),
                         testStage = character(0),
                         nTrial = character(0),
                         trialType = character(0),
                         trialList = character(0),
                         correctRes = character(0),
                         ans = character(0),
                         press = character(0),
                         rt = character(0),
                         length = character(0),
                         Notes = character(0),
                         stringsAsFactors = F
    )
    
    InhibitionData <- loadTrialsCsv(sesspath, rawInhibitionName, InhibitionData)
    
    require(dplyr)
    InhibitionData <- InhibitionData %>% group_by(Path) %>% mutate(rownumber = row_number()) %>% ungroup()
    InhibitionData <- InhibitionData %>% select(-DOB)
    
    return(InhibitionData)
    
}



# read CKAT position by time data from a character vector of CKAT paths and return as data.frame
# merges movement and events data files
# trial needs to be nnn, i.e. "003" or "010"
readCKATPosByTime <- function(sesspath, trial) {
    
    rawEventsName <- paste0("block001_trial", trial, "_Events.txt")
    rawMovementName <- paste0("block001_trial", trial, "_Movement Data.txt")
    
    CKATEvents <- loadTrialsCsv(sesspath, rawEventsName, NULL, tab_delim = T)
    CKATMovt <- loadTrialsCsv(sesspath, rawMovementName, NULL, tab_delim = T)
    
    CKATMovt <- CKATMovt %>% select(-Object, -`Object Event`)
    
    # reshape events
    CKATEvents <- CKATEvents %>% group_by(Path, `Event Time`) %>% mutate(Event = paste0("Event", row_number())) %>%
        ungroup()
    CKATEvents <- tidyr::spread(CKATEvents, Event, `Event Name`)
    
    CKATEvents <- rename(CKATEvents, Time = `Event Time`)
    CKATMovt <- rename(CKATMovt, HandX = `Hand X`, HandY = `Hand Y`)
    
    # truncate time var in movt as it's got a different precision level
    # done to keep as char but we will check for mismatches
    CKATMovt <- CKATMovt %>% mutate(Time = substr(Time, 1, nchar(Time) - 1))
    
    # check for mismatches
    check <- CKATEvents %>% anti_join(CKATMovt)
    if(nrow(check) > 0) stop("There were events in the CKAT data that couldn't be matched to movement data.\n")
    
    # merge
    CKAT <- CKATMovt %>% left_join(CKATEvents)
    
    require(dplyr)
    CKAT <- CKAT %>% group_by(Path) %>% mutate(rownumber = row_number()) %>% ungroup()
    
    return(CKAT)
    
}


# merge summary data frames from different header versions
# pass versions as list of data frames, one item for each version
# SummData[1] = v1, SummData[2] = v2, etc.
mergeSummaryDataVersions <- function(SummData, includePath = T) {
    
    # set up master data.frame
    NewSummDataNames <- data.frame(Machine.ID = character(0),
                                   Test.Date = character(0),
                                   Last.Name = character(0),
                                   First.Name = character(0),
                                   PSY.number = character(0),
                                   UPN = character(0),
                                   School.Name = character(0),
                                   Class.Name = character(0),
                                   Date.of.Birth = character(0),
                                   Gender = character(0),
                                   Handedness = character(0),
                                   Aiming = character(0),
                                   Tracing = character(0),
                                   Tracking = character(0),
                                   CKAT.Overall = character(0),
                                   FDR = character(0),
                                   BDR = character(0),
                                   Corsi = character(0),
                                   WM.Overall = character(0),
                                   PS = character(0),
                                   Inhibition = character(0),
                                   stringsAsFactors = F)
    
    if(includePath) NewSummDataNames <- cbind(NewSummDataNames, data.frame(DailyDir = character(0), 
                                                                           TabletDir = character(0),
                                                                           Path = character(0), 
                                                                           stringsAsFactors = F))

    # one copy per version of header to be merged
    NewSummData <- list(NewSummDataNames, NewSummDataNames)
    
    # rename v1
    names(SummData[[1]])[names(SummData[[1]]) == "Overall"] <- "CKAT.Overall"
    names(SummData[[1]])[names(SummData[[1]]) == "fdr"] <- "FDR"
    names(SummData[[1]])[names(SummData[[1]]) == "bdr"] <- "BDR"
    names(SummData[[1]])[names(SummData[[1]]) == "corsi"] <- "Corsi"
    names(SummData[[1]])[names(SummData[[1]]) == "ps"] <- "PS"
    names(SummData[[1]])[names(SummData[[1]]) == "flanker"] <- "Inhibition"
    
    # merge
    NewSummData[[1]][1:nrow(SummData[[1]]), 
                     which(names(NewSummDataNames) %in% names(SummData[[1]]))] <- SummData[[1]]
    NewSummData[[2]][1:nrow(SummData[[2]]), 
                     which(names(NewSummDataNames) %in% names(SummData[[2]]))] <- SummData[[2]]
    
    # unlist to data.frame
    NewSummData <- do.call(rbind, NewSummData)
    
    return(NewSummData)
    
}


# merge two data frames on the basis of EITHER PSY OR UPN, depending
# on which is present
mergeUPNorPSY <- function(df1, df2) {
    
    drops <- c("PSY.number.x", "UPN.x", "PSY.number.y", "UPN.y")
    
    df1.upn <- df1[!is.na(df1$UPN), ]
    df1.psy <- df1[!is.na(df1$PSY.number), ]
    df2.upn <- df2[!is.na(df2$UPN), ]
    df2.psy <- df2[!is.na(df2$PSY.number), ]   
    
    df3.upn <- merge(df1.upn, df2.upn, by = "UPN")
    df3.psy <- merge(df1.psy, df2.psy, by = "PSY.number")
    
    names(df3.upn)[names(df3.upn) == "PSY.number.x"] <- "PSY.number"
    names(df3.psy)[names(df3.psy) == "UPN.x"] <- "UPN"
    
    df3.upn <- df3.upn[ , !(names(df3.upn) %in% drops)]
    df3.psy <- df3.psy[ , !(names(df3.psy) %in% drops)]
    
    df3 <- rbind(df3.upn, df3.psy)
    
    return(df3)
    
}

# calculate age in years
calcAgeYears <- function(dob = as.POSIXlt(NA), date = as.POSIXlt(NA)) {

    # years from dob to date
    age <- date$year - dob$year
    
    # adjust for date being earlier in year than dob, or later
    age <- ifelse(date$mon < dob$mon |
                  (date$mon == dob$mon & date$mday < dob$mday),
                  age - 1, age)
    
    return(age)
    
}

# calculate age in months
calcAgeMonths <- function(dob = as.POSIXlt(NA), date = as.POSIXlt(NA)) {

    # months from dob to end of year
    from_months <- 12 - dob$mon
    
    # months from start of year to date
    to_months <- date$mon
    
    # months in full years after dob year
    age <- ((date$year - dob$year) - 1) * 12
    
    # sum months
    age <- from_months + age + to_months
    
    # adjust for day of month
    age <- ifelse(date$mday < dob$mday, age - 1, age)
    
    return(age)
    
}

# add child age at test to summary data and return data.frame
# pass summary data data.frame as input
addAgeToSummaryData <- function(SummData) {
    
    # We could do this date conversion on import, but currently sticking to the idea
    # of preserving the text of the data files so we can write them back out if necessary
    # So we convert here instead...
    #
    # age functions require date values as POSIXlt
    dob.posix <- strptime(SummData$Date.of.Birth, format = "%d/%m/%Y")
    testdate.posix <- strptime(SummData$Test.Date, format = "%d/%m/%Y %H:%M:%S")
    # alternative date format
    # searches for dates not yet matched and replaces these with a recalculation
    testdate.posix[which(is.na(testdate.posix))] <- 
        strptime(SummData$Test.Date[which(is.na(testdate.posix))], format = "%d-%b-%y %H:%M:%S")
    
    SummData$AgeInYears <- calcAgeYears(dob.posix, testdate.posix)
    SummData$AgeInMonths <- calcAgeMonths(dob.posix, testdate.posix)
    
    return(SummData)
    
}

# add ID to recorded data session info: ID is UPN and/or PSY, 
# depending on which is present
# this requires some transformation of the date and other key values in the summary
# and recorded data, followed by a merge with Summary Data
# returns merge as data.frame which will include the transformed values
addIDToRecData <- function(RecDataSessInfo, IDMatchData) {
    
    # transform recorded data session items for matching
    rd.match <- RecDataSessInfo
    rd.match$Test.Date <- as.Date(rd.match$Test.Date, format = "%d/%m/%Y")
    rd.match$Date.of.Birth <- as.Date(rd.match$Date.of.Birth, format = "%d/%m/%Y")
    # transform summary data session items for matching
    id.match <- IDMatchData[ , c("UPN", "PSY.number", "First.Name", "Last.Name", "Date.of.Birth")]
    id.match$Date.of.Birth <- as.Date(id.match$Date.of.Birth, format = "%d/%m/%Y")
    id.match$Full.Name <- paste0(id.match$First.Name, " ", id.match$Last.Name)
    # drop First.Name and Last.Name
    id.match <- id.match[ , !(colnames(id.match) %in% c("First.Name", "Last.Name"))]
    # remove duplicate children from ID match data
    id.match <- id.match[which((!duplicated(id.match$UPN) | is.na(id.match$UPN)) & 
                               (!duplicated(id.match$PSY.number) | is.na(id.match$PSY.number))),]
    
    # merge data
    # cannot merge on Test.Date as we may have tested the same child on different days
    result <- merge(rd.match, id.match, by = c("Full.Name", "Date.of.Birth"),
                    all.x = T)
    
    return(result)
    
}



# add reports count to summary data
# assumes summary data is merged across versions
# splits out v1 data with no PSY number then recombines
addFbReportToSummaryData <- function(SummData, rep.files) {
    
    # split according to whether PSY is present
    summ.psy <- SummData[!is.na(SummData$PSY.number), ]
    summ.nopsy <- SummData[is.na(SummData$PSY.number), ]
    
    # match nopsy data on full name, school name, tablet, daily dir
    # get full name into nopsy and school name in reports df
    summ.nopsy$Full.Name <- paste(summ.nopsy$First.Name, summ.nopsy$Last.Name)
    split.fb.path <- strsplit(rep.files$Path, "/")
    rep.files$School.Name <- sapply(split.fb.path, "[[", length(split.fb.path[[1]]))
    # do match
    summ.nopsy <- merge(summ.nopsy, rep.files, by = c("Full.Name", "School.Name", "TabletDir", "DailyDir"), all.x = T)
    
    # match psy data on psy
    summ.psy <- merge(summ.psy, rep.files, by = c("PSY.number"), all.x = T)
    
    # drop unwanted columns and some renaming
    drop.nopsy <- c("PSY.number.y", "Path.y", "Full.Name")
    drop.psy <- c("School.Name.y", "DailyDir.y", "TabletDir.y", "Path.y", "Full.Name")
    summ.nopsy <- summ.nopsy[ , !(names(summ.nopsy) %in% drop.nopsy)]
    summ.psy <- summ.psy[ , !(names(summ.psy) %in% drop.psy)]
    names(summ.nopsy)[names(summ.nopsy) == 'PSY.number.x'] <- 'PSY.number'
    names(summ.nopsy)[names(summ.nopsy) == 'Path.x'] <- 'Path'
    names(summ.psy)[names(summ.psy) == 'School.Name.x'] <- 'School.Name'
    names(summ.psy)[names(summ.psy) == 'DailyDir.x'] <- 'DailyDir'
    names(summ.psy)[names(summ.psy) == 'TabletDir.x'] <- 'TabletDir'
    names(summ.psy)[names(summ.psy) == 'Path.x'] <- 'Path'
    
    # recombine
    result <- rbind(summ.nopsy, summ.psy)

    return(result)
    
}


# add task accuracy to WM/EF data
addPSAccuracyToWMData <- function(wm.data, fullpath) {
    
    # PS relative file location
    filename <- "/results/ps.csv"
    
    PS.correct <- integer(0)
    PS.count <- integer(0)
    
    pb <- txtProgressBar(min = 0, max = length(fullpath), style = 3)
    
    f <- paste0(fullpath, filename)
    
    for(x in 1:length(f)) {
        
        setTxtProgressBar(pb, x)
        
        if(file.exists(f[x]) && length(readLines(f[x]))) {
            
            raw <- read.csv(f[x], 
                            colClasses = "character", 
                            stringsAsFactors = F,
                            row.names = NULL)
            
            # compute accuracy
            # suppress warnings about NAs
            # returns NA unless all expected 21 rows of data are present
            # skips first 3 practice trials
            PS.correct.x <- sum(suppressWarnings(as.numeric(raw$ans[4:21])))
            # give us count of number of trials, subtracting 3 practice trials
            PS.count.x <- max(0, nrow(raw) - 3)
            
        } else {
            
            # file is missing
            PS.correct.x <- NA
            PS.count.x <- NA
            
        }
        
        # add new item
        PS.correct <- c(PS.correct, PS.correct.x)
        PS.count <- c(PS.count, PS.count.x)
        
    }
    
    close(pb)
    
    # add to data frame
    wm.data$PS.correct <- PS.correct
    wm.data$PS.count <- PS.count
    
    return(wm.data)
    
    
}

# EF alias to addPSAccuracyToWMData
addPSAccuracyToEFData <- function(ef.data, fullpath) {
    
    return(addPSAccuracyToWMData(ef.data, fullpath))
       
}

# delete task session folder(s)
# if test == T then only copy the data to bin, otherwise, move it
# sesstype should be WM or CKAT
deleteSessionData <- function(kinelab.path, path = NULL, sesstype, test = T) {
    
    log = character(0)
    
    log <- paste0(log,
                  "Deleting \"", sesstype, "\" data from the following UPNs:\r\n\r\n")
    
    log <- paste0(log, paste0(path$UPN, collapse = "\r\n"), "\r\n\r\n----\r\n\r\n")
    
    log <- paste0(log, "Test mode is ", ifelse(test, "ON", "OFF"),
                  ", which means source data will ", ifelse(test, "NOT ", ""), "be deleted.", "\r\n\r\n\r\n"
                  )
    
    for(x in 1:nrow(path)) {
        
        binpath <- paste0(kinelab.path, "/bin/data/", path$DailyDir[x], "/", path$TabletDir[x],
                          "/BiB Kinelab Output/", sesstype)
        dir.create(binpath, recursive = T, showWarnings = F)
        
        log <- paste0(log,
                      x, ". Copying ", path$Path[x], "\r\n\r\n",
                      "   ... to ", binpath, "\r\n\r\n")
        
        file.copy(path$Path[x], binpath, overwrite = T, recursive = T,
                  copy.date = T)
    
        if(!test) {
            log <- paste0(log, "   Deleting ", path$Path[x], "\r\n\r\n")
            unlink(path$Path[x], recursive = T)
        }
        
    }
    
    log <- paste0(log, "====\r\n\r\n\r\n")
    
    return(log)
    
}

# delete WM data session folder(s)
# if test == T then only copy the data to bin, otherwise, move it
# provide either paths to delete OR UPNs
# if UPNs are provided, readDirSessInfo is called to get the UPN index
deleteWMData <- function(kinelab.path, path = NULL, UPN = NULL, test = T) {
    
    log = character(0)
    
    if(is.null(path)) {
        path <- readDirSessInfo(getWMDirs(kinelab.path))
        path <- path[which(path$UPN %in% UPN), ]
    } else {
        path <- makePathDataFrame(path, kinelab.path)
    }
    
    log = deleteSessionData(kinelab.path, path, "WM", test)
    
    return(log)
    
}

# delete CKAT data session folder(s)
# if test == T then only copy the data to bin, otherwise, move it
# provide either paths to delete OR UPNs
# if UPNs are provided, readDirSessInfo is called to get the UPN index
deleteCKATData <- function(kinelab.path, path = NULL, UPN = NULL, test = T) {
    
    log = character(0)
    
    if(is.null(path)) {
        path <- readDirSessInfo(getCKATDirs(kinelab.path))
        path <- path[which(path$UPN %in% UPN), ]
    } else {
        path <- makePathDataFrame(path, kinelab.path)
    }
    
    log = deleteSessionData(kinelab.path, path, "Processed data", test)
    
    return(log)

}

# delete recorded_data session folder(s)
# if test == T then only copy the data to bin, otherwise, move it
# provide either paths to delete OR UPNs
# if UPNs are provided, readRecDataSessInfo is called to get the UPN index
deleteRecData <- function(kinelab.path, path = NULL, UPN = NULL, test = T) {
    
    log = character(0)
    
    if(is.null(path)) {
        path <- readRecDataSessInfo(getRecDataDirs(kinelab.path))
        UPNMatch <- readSummaryData(getSummaryDataDirs(kinelab.path))
        path <- addUPNToRecData(path, UPNMatch)
        path <- path[which(path$UPN %in% UPN), ]
    } else {
        path <- makePathDataFrame(path, kinelab.path)
    }
    
    log = deleteSessionData(kinelab.path, path, "recorded_data", test)
    
    return(log)

}

# delete summary data and feedback reports: together called BiB Data
# if test == T then only copy the data to bin, otherwise, move it
# provide UPNs to be deleted
# readSummaryData is called to get the UPN index
# feedback reports are found by looking up names in the summary data
# hence these are deleted first
deleteBiBData <- function(kinelab.path, UPN, test = T) {
    
    log = character(0)
    
    log <- paste0(log,
                  "Deleting feedback reports and rows of summary data for the following UPNs:\r\n\r\n")
    
    log <- paste0(log, paste0(UPN, collapse = "\r\n"), "\r\n\r\n----\r\n\r\n")
    
    log <- paste0(log, "Test mode is ", ifelse(test, "ON", "OFF"),
                  ", which means the source file will ", ifelse(test, "NOT ", ""), "have these rows removed.", "\r\n\r\n\r\n"
    )
    
    # get path info and add (expected) feedback report files
    path <- readSummaryData(getSummaryDataDirs(kinelab.path))
    path <- path[which(path$UPN %in% UPN), ]
    if(nrow(path) == 0) {
        log <- paste0(log, "None of the supplied UPNs were found in the SUmmary Data files.\r\n\r\n")
        return(log)
    }
    path$pdfreport <- paste0(kinelab.path, "/",
                             path$DailyDir, "/",
                             path$TabletDir, "/BiB Kinelab Output/BiB data/Feedback reports/",
                             path$School.Name, "/",
                             path$First.Name, " ", 
                             path$Last.Name, ".pdf")
    # add summary data files
    path$SummDataFile <- paste0(path$Path, "/", "Database data.txt")
    # get unique summary data files that need changes, with tablet and daily dir
    summdatachange <- unique(path[ , (colnames(path) %in% c("SummDataFile", "DailyDir", "TabletDir"))])
    
    log <- paste0(log, "* Feedback Reports *\r\n\r\n")
    
    # do feedback reports
    for(x in 1:nrow(path)) {
        
        if(file.exists(path$pdfreport[x])) {
            
            binpath <- paste0(kinelab.path, "/bin/data/", path$DailyDir[x], "/", path$TabletDir[x],
                              "/BiB Kinelab Output/BiB data/Feedback reports/", path$School.Name[x])
            
            dir.create(binpath, recursive = T, showWarnings = F)
            
            log <- paste0(log,
                          x, ". Copying ", path$pdfreport[x], "\r\n\r\n",
                          "   ... to ", binpath, "\r\n\r\n")
            
            file.copy(path$pdfreport[x], binpath, overwrite = T, recursive = T,
                      copy.date = T)
            
            if(!test) {
                log <- paste0(log, "   Deleting ", path$pdfreport[x], "\r\n\r\n")
                unlink(path$pdfreport[x], recursive = T)
            }
            
        } else {
            
            log <- paste0(log,
                          x, ". Report not found: ", path$pdfreport[x], "\r\n\r\n",
                          "  Skipping...\r\n\r\n")
        }        
    }
    
    log <- paste0(log, "* Summary Data Files *\r\n\r\n")
    
    # do summary data
    for(x in 1:nrow(summdatachange)) {
        
        log <- paste0(log, x, ". Opening summary data file ", summdatachange$SummDataFile[x], "\r\n\r\n")
        
        d <- readLines(summdatachange$SummDataFile[x])
        
        # separate droppers and keepers based on search for UPN
        droplines <- c(d[1], grep(paste0(UPN,collapse="|"), d, value = T))
        keeplines <- grep(paste0(UPN,collapse="|"), d, value = T, invert = T)
        
        binpath <- paste0(kinelab.path, "/bin/data/", summdatachange$DailyDir[x], "/", summdatachange$TabletDir[x],
                          "/BiB Kinelab Output/BiB data/Database data")
        dir.create(binpath, recursive = T, showWarnings = F)
        
        log <- paste0(log, "   Splitting \"Database data.txt\" according to which UPNs are being kept, and which are being removed.\r\n\r\n ")
        
        log <- paste0(log, "   Saving changes to ", binpath, "\r\n\r\n")
        
        dropfile <- file(paste0(binpath, "/removed_Database data.txt"), "w")
        writeLines(droplines, dropfile)
        close(dropfile)
        
        keepfile <- file(paste0(binpath, "/kept_Database data.txt"), "w")
        writeLines(keeplines, keepfile)
        close(keepfile)
        
        log <- paste0(log, "      --> see \"removed_Database data.txt\" for the rows that have been dropped.\r\n")
        log <- paste0(log, "      --> see \"kept_Database data.txt\" for the rows that have been kept.\r\n\r\n")
        
        if(!test) {
            log <- paste0(log, "   Removing UPNs from source summary data file.\r\n\r\n")
            sourcefile <- file(summdatachange$SummDataFile[x], "w")
            writeLines(keeplines, sourcefile)
            close(sourcefile)
        }
    }
    
    log <- paste0(log, "====\r\n\r\n\r\n")
    
    return(log)
    
}

# full delete by UPN
deleteAllByUPN <- function(kinelab.path, UPN, test = T) {
    
    log <- paste0("kinelabr running at ", Sys.time(), "\r\n\r\n",
                  "Running Kinelab delete by UPN in ", ifelse(test, "test (copy only)", "full deletion"),
                  " mode.\r\n\r\n\r\n")
    
    log <- paste0(log, deleteWMData(kinelab.path = kinelab.path, UPN = UPN, test = test))
    log <- paste0(log, deleteCKATData(kinelab.path = kinelab.path, UPN = UPN, test = test))
    log <- paste0(log, deleteRecData(kinelab.path = kinelab.path, UPN = UPN, test = test))
    log <- paste0(log, deleteBiBData(kinelab.path = kinelab.path, UPN = UPN, test = test))
    
    logfile <- file(paste0(kinelab.path, "/bin/bin.log"), "w")
    writeLines(log, logfile)
    close(logfile)
    
}



# attempt to fix files by replacing the string <find> with <replace>
# renames original with a backup filename <backupname> first
# then reads it, replaces text and saves as original <filename>
util_fixFile <- function(path, filename, backupname, find, replace) {
    
    pb <- txtProgressBar(min = 0, max = length(path), style = 3)
    
    for(x in 1:length(path)) {
        
        setTxtProgressBar(pb, x)
        
        getfile <- paste0(path[x], "/", filename)
        bakfile <- paste0(path[x], "/", backupname)
        
        
        if(file.exists(getfile)) {
            
            file.rename(getfile, bakfile)
            
            original  <- readLines(bakfile)
            processed  <- gsub(pattern = find, replace = replace, x = original)
            writeLines(processed, con=getfile)
            
        } else {
            
            #file not found
            
        }
        
    }
    
    close(pb)

}
