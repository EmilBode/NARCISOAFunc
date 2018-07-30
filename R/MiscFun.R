#' read NARCIS classification scheme from pdf-file
#'
#' Read and convert NARCIS-classification-scheme
#'
#' @param FilePath Path to pdf-file containing classification. May be a URL
#' @return A data.frame containing codes, labels, code of overlaying level, and indication of level-depth
#' @export
readNARCIScla <- function(FilePath='https://www.narcis.nl/content/pdf/classification_en.pdf') {
  EmilMisc::libinstandload('pdftools')
  cl <- pdftools::pdf_text(FilePath)
  cl <- substr(cl, 1, nchar(cl)-4) # Removing pagenumbers
  cl <- paste(cl, collapse = ' ')
  cla <- data.frame(start=unlist(gregexpr('(D|E)[0-9]{5}',cl)))
  cla$stop <- c(cla$start[-1]-1,nchar(cl))
  cla <- apply(cla, 1, function(x) {substr(cl, x['start'], x['stop'])})
  cla <- data.frame(code=substr(cla,1,6), descr=gsub('[[:space:]\r\n]+',' ',substr(cla,7,500)),stringsAsFactors = FALSE)
  cla$descr <- gsub('(^ )|( $)','',cla$descr)
  cla$descr <- gsub('[^A-Za-z0-9, \\(\\)]+','-', cla$descr) # Non-ASCII causing trouble
  cla$PartOf <- gsub('(D|E)([0-9]*)[1-9](0*)$','\\1\\20\\3',cla$code)
  cla$PartOf[grepl('(D|E)0+$',cla$PartOf)] <- NA
  cla$lvl <- sapply(cla$code,function(x) {length(gregexpr('[1-9]',x)[[1]])+length(gregexpr('[1-9]0+[1-9]',x)[[1]])})
  return(cla)
}


#' Read the dataset/NARCIS-dump stored locally
#'
#' Assumed is that "OAI Harvest New Records.R" has been run, and results are stored on disk
#'
#' @param Summarize Should the entire dataset be loaded, or just the summary. Can be FALSE, TRUE or 'Only'. In the last case, the script looks if there is a summarizing file, and loads that if there is one. Otherwise, one is created.
#' If TRUE, the entire set is also loaded and stored (assigned to assignTo-environment), but nothing is done with any present summaryfile.
#' To update the summaryfile, simply remove it, and run this function again with Summarize=='Only'
#' @param FilePath Link to file, or 'Auto'. If Summarize=='Only', provide link to summary file.
#' @param set Which set to open. Can be 'dc', 'auto', or 'didlmods'. Auto opens the last stored set. Note that at current implementation, opening didlmods is of little use, the mongo-db-access is more useful.
#' Also note that as of june 2018, OAI Harvest New Records.R does not store a totalfile for didlmods, so 'auto' defaults to dc.
#' @param ForceReload Should the file always be reloaded? If FALSE, script first looks if a variable called 'Total' exists in the global environment, and reuses that if it does, which saves loading time.
#' @param verbose Emit status messages?
#' @param DropCols,KeepCols Columns to drop or keep form the totalSet. If both are defined, DropCols is ignored
#' @param KeepSets Select to use only certain sets.
#' @param KeepMulti Additional selection criteria, in the form of a named list with allowed values. E.g. list(Type==c('Article', 'Doctoral thesis'))
#' @param assignTo As a side-effect, the "total" data.frame and Paths variable are stored. Where should they be stored?
#' @return If Summarize==TRUE or Summarize=='Only' a data.frame with counts. If Summarize==FALSE, invisible 0. Side-effects: Assignment of 'Total' to the global environment, along with modification of 'Paths' global environment if it exists:
#' The slots loadedWerkset and loadedWerkSummary are adapted if they are used. Be careful that loadedWerkset is not updated if Summarize=='Only', and loadedWerkSummary is not updated of Summarize==FALSE.
#' @export
ReadForAnalysisfromTotal <- function(Summarize=FALSE, FilePath='Auto', set='auto', ForceReload=FALSE, verbose=TRUE, DropCols=NULL, KeepCols=NULL, KeepSets=NULL, KeepMulti=NULL, assignTo=.GlobalEnv) { # Padding
  EmilMisc::libinstandload('plyr','lubridate')
  `%!in%` <- EmilMisc::`%!in%`
  tz <- Sys.getenv('TZ', unset=NA)
  Sys.setenv(TZ='Europe/Amsterdam')
  if(!exists('Paths')) Paths <- getOption('Paths')
  if(Summarize=='Only') {
    if(FilePath=='Auto') {
      RDSfiles <- data.frame(name=list.files(path=paste0(Paths$Dumps,'/Werkset'),
                                             pattern='.*Summary.*dc.*\\.rds', full.names = TRUE, ignore.case=TRUE), stringsAsFactors = FALSE)
      if(nrow(RDSfiles)>0) {
        RDSfiles$time <- file.mtime(RDSfiles$name)
        RDSfiles <- RDSfiles[order(RDSfiles$time, decreasing=TRUE),]
        FilePath <- RDSfiles$name[grepl('.*Summary.*dc.*\\.rds',RDSfiles$name)][1]
      }
      # So if no files are found, FilePath is still 'Auto'
    } else {
      if(!grepl('Summary', FilePath, ignore.case = TRUE))
        warning('Summarize was given as "Only", but the provided FilePath-name does not contain "summary". Are you sure this is the right file?')
    }
    if(FilePath!='Auto') { # If filename could be found. Returns control
      Vals <- readRDS(FilePath)
      if(verbose) print('Loading completed')
      Paths$loadedWerkSummary <- FilePath
      temp <- regexpr('[0-9]{8,}', FilePath)
      temp <- substr(FilePath, temp, temp+7)
      Paths$WerksetDate <- as.Date(temp, format='%Y%m%d')
      attr(Vals, 'Date') <- Paths$WerksetDate
      if(exists('Paths', where=assignTo, inherits = FALSE))
        assign('Paths',Paths, pos = assignTo)
      return(Vals)
    } # If filename could be found. Returns control
    tempans <- ''
    while(tempans %!in% c('y','n')) {
      tempans <- readline('No summary files exists yet. Create one? (y/n) ')
    }
    if(tempans=='n')
      Summarize <- TRUE
  }
  if(FilePath=='Auto') {
    if(set=='auto') {
      RDSfiles <- data.frame(name=list.files(path=paste0(Paths$Dumps,'Werkset/'),
                                             pattern='.*Total.*\\.rds', full.names=TRUE, ignore.case=TRUE), stringsAsFactors = FALSE)
      RDSfiles$time <- file.mtime(RDSfiles$name)
      RDSfiles <- RDSfiles[order(RDSfiles$time, decreasing=TRUE),]
      FilePath <- RDSfiles$name[grepl('Total',RDSfiles$name)][1]
      if(is.na(FilePath) || is.null(FilePath)) {
        FilePath <- paste0(Paths$Dumps,'TotalSet.rds')
      }
    } else if(set=='dc') {
      RDSfiles <- data.frame(name=list.files(path=paste0(Paths$Dumps,'/Werkset'),
                                             pattern='.*Total.*dc.*\\.rds', full.names=TRUE, ignore.case=TRUE), stringsAsFactors = FALSE)
      RDSfiles$time <- file.mtime(RDSfiles$name)
      RDSfiles <- RDSfiles[order(RDSfiles$time, decreasing=TRUE),]
      FilePath <- RDSfiles$name[grepl('Total',RDSfiles$name)][1]
    } else if(set %in% c('didl', 'didlmods')) {
      RDSfiles <- data.frame(name=list.files(path=paste0(Paths$Dumps,'/Werkset'),
                                             pattern='.*Total.*didl.*\\.rds', full.names=TRUE, ignore.case=TRUE), stringsAsFactors = FALSE)
      RDSfiles$time <- file.mtime(RDSfiles$name)
      RDSfiles <- RDSfiles[order(RDSfiles$time, decreasing=TRUE),]
      FilePath <- RDSfiles$name[grepl('Total',RDSfiles$name)][1]
    } else stop('Unknown set parameter given to ReadForAnalysisfromTotal')
  }
  if(!exists('Total')||ForceReload||is.null(Paths$loadedWerkset)||is.na(Paths$loadedWerkset)||Paths$loadedWerkset!=FilePath) {
    if(verbose) print('Reading rds-file, which might take a while')
    Paths$loadedWerkset <- NA
    Paths$WerksetDate <- NA
    if(exists('Paths', where=assignTo, inherits=FALSE)) assign('Paths',Paths, pos=assignTo)
    Total <- readRDS(FilePath)
    if(verbose && !(is.null(DropCols) && is.null(KeepCols) && is.null(KeepSets) && is.null(KeepMulti))) print('Loading finished, now subsetting...')
  }

  # Now we have the Total-file read, time to subset
  if(is.null(KeepCols)) {
    if(!is.null(DropCols)) Total <- Total[-DropCols]
  } else {
    Total <- Total[KeepCols]
  }
  if (!all(unique(unlist(sapply(Total$setSpec, levels))) %in% names(Total$setSpec)[sapply(Total$setSpec,class)=='logical'])||
      any(is.na(Total$setSpec[sapply(Total$setSpec,class)!='factor']))) {
    m <- sum(sapply(Total$setSpec,class)!='logical')
    for(n in unique(unlist(sapply(Total$setSpec, levels)))) {
      Total$setSpec[n] <- apply(Total$setSpec[,1:m],1,function(x) {any(n==x,na.rm=TRUE)})
      print(paste(n,'tested'))
    }
  } # Nakijken: dit werkt nu alleen met SetSpec factor/dataframe
  if(!is.null(KeepSets)) {Total <- Total[apply(Total$setSpec[KeepSets],1,any),]}
  if(!is.null(KeepMulti)) {
    for (n in 1:(length(KeepMulti)-1)) {
      Total <- Total[Total[,names(KeepMulti)[[n]]] %in% KeepMulti[[n]],]
    }
  }
  if(!set %in% c('didl', 'didlmods')) {
    Total$year <- lubridate::year(Total$date.meta.date)
    Total$Type[Total$Bron=='UvA'&Total$date.header<as.POSIXct('2017-08-20')&Total$Type=='Other'] <- 'Doctoral Thesis'
    levels(Total$Type) <- c(levels(Total$Type),'Unknown')
    Total$Type[is.na(Total$Type)] <- 'Unknown'
  }
  if(!is.na(tz)) {
    Sys.setenv(TZ=tz)
  }
  if(Summarize==FALSE) {
    Paths$loadedWerkset <- FilePath
    temp <- regexpr('[0-9]{8,}', FilePath)
    temp <- substr(FilePath, temp, temp+7)
    Paths$WerksetDate <- as.Date(temp, format='%Y%m%d')
    attr(Total, 'Date') <- Paths$WerksetDate
    assign('Total', Total, pos=assignTo)
    if(verbose) {
      print('Reading Total completed')
    }
    if(exists('Paths', where=assignTo, inherits = FALSE))
      assign('Paths',Paths, pos = assignTo)
    return(0)
  } else {
    levels(Total$access) <- c(levels(Total$access),'Other')
    Total$access[!Total$access %in% c('Closed','Open','Restricted','Embargoed')] <- 'Other'
    Total$access <- droplevels(Total$access)
    Vals <- plyr::count(data.frame(Access=Total$access,
                                   Jaar=Total$year,
                                   Bron=Total$Bron,
                                   BronSoort=Total$BronCat,
                                   Type=Total$Type,
                                   lang=Total$language,
                                   set=Total$setSpec))
    Paths$loadedWerkset <- FilePath
    temp <- regexpr('[0-9]{8,}', FilePath)
    temp <- substr(FilePath, temp, temp+7)
    Paths$WerksetDate <- as.Date(temp, format='%Y%m%d')
    attr(Total, 'Date') <- Paths$WerksetDate
    attr(Vals, 'Date') <- Paths$WerksetDate
    if(Summarize=='Only') { # Meaning: Now calculated, we still have to store it
      Paths$loadedWerkSummary <- paste0(Paths$Dumps,'Werkset/Summary dc (', gsub('[^0-9]+','',as.character(Sys.time(), tz='UTC')),').rds')
      saveRDS(Vals, Paths$loadedWerkSummary)
    } else {
      Paths$loadedWerkSummary <- 'calculated'
    }
    if(exists('Paths', where=assignTo, inherits = FALSE))
      assign('Paths',Paths, pos = assignTo)
    if(Summarize!='Only') {
      assign('Total', Total, pos=assignTo)
    }
    if(verbose) {
      print('Reading Total completed')
    }
    return(Vals)
  }
  stop('This code should never be reached, ReadAnalysisfromTotal')
}

#' Read the numbers from the VSNU, based on pdf
#'
#' For private use, as the underlying document is not public
#'
#' @param pdfs Vector with paths to the pdfs
#' @return list with data.frames
#' @export
ReadVSNUpdf <- function(pdfs=paste0(getOption('Paths')$Input,c('/Narcis versus KUOZ 1.pdf','/Narcis versus KUOZ 2 uitgebreid.pdf'))) {
  EmilMisc::libinstandload('pdftools','data.table','zoo','tidyr')

  VSNU <- c(sapply(pdfs, pdftools::pdf_text))
  VSNU <- sapply(VSNU, strsplit, split='\\n')
  VSNU[[1]] <- unname(VSNU[[1]][-c(1,39)])
  VSNU[[2]] <- unname(VSNU[[2]][-c(1,21)])
  VSNU[[3]] <- unname(VSNU[[3]][-c(1,32)])
  VSNU[[4]] <- unname(VSNU[[4]][-c(31)])

  headers <- lapply(VSNU, function(x) {
    t <- strsplit(x[1],'\\s')[[1]]
    return(t[t!=''])
  })
  headers[[1]] <- c('Type',headers[[1]])
  headers[[2]] <- c('Type',headers[[2]])

  VSNU <- unname(lapply(VSNU, function(x) {unname(x[-1])}))
  names(VSNU) <- c(paste0('P',1:4))
  delimlines <- c(P1=VSNU[[1]][34], P2=VSNU[[2]][13], P3=VSNU[[3]][1], P4=VSNU[[4]][18])
  #delimlines <- gsub('[[:space:]]','_',delimlines)
  delimchars <- strsplit(delimlines, split='\\s',perl=T)
  delims <- lapply(delimchars, function(x) {list(start=unname(cumsum(sapply(x, nchar)+1)[c(x[-1],'')!='']+1),
                                                 stop=unname(cumsum(sapply(x, nchar)+1)[x!='']-1))})
  delims <- lapply(delims, function(x) {
    list(start=c(x$start[1:2], x$stop[3]-5, x$stop[3:(length(x$stop)-1)]+2),
         stop=c(x$start[2]-1, x$stop[3]-6, x$stop[3:length(x$stop)]+1))
  })
  delims$P2$start <- delims$P2$start[-16]
  delims$P2$stop <- delims$P2$stop[-15]

  dfs <- lapply(1:4, function(n) {
    df <- as.data.frame(do.call(rbind, lapply(VSNU[[n]], function(l) {
      gsub('([0-9]) ([0-9])','\\1\\2', gsub('^ *| *$|','',substring(l, delims[[n]]$start, delims[[n]]$stop)))
    })), stringsAsFactors = F)
    df[3:ncol(df)] <- sapply(df[3:ncol(df)], function(x) {ifelse(x=='',0,as.numeric(x))})
    names(df) <- headers[[n]]
    return(df)
  })
  doc1 <- rbind(dfs[[1]], dfs[[2]])
  doc1$Type[doc1$Type==''] <- NA
  doc1$Type <- zoo::na.locf(doc1$Type)
  NARCISTypes <- c('Annotation','Article','Book','Contribution to periodical','Doctoral Thesis','Report')
  doc1$Bron <- ifelse(doc1$Type %in% NARCISTypes, 'NARCIS','KUOZ')
  doc1$HoofdType <- zoo::na.locf(ifelse(doc1$Bron=='NARCIS', doc1$Type,NA))
  doc1 <- tidyr::gather(doc1, 'Uni','count', -'Type', -'jaar', -'Bron', -'HoofdType')
  doc1[-6] <- lapply(doc1[-6], as.factor)
  doc1 <- doc1[c(3,4,1,2,5,6)]

  doc2 <- rbind(dfs[[3]],dfs[[4]])
  doc2 <- doc2[doc2$Bron!='Bron' & doc2$Onderwerp!='Onderwerp',]
  names(doc2)[2] <- 'Type'
  doc2$HoofdType <- zoo::na.locf(ifelse(doc2$Bron=='NARCIS', doc2$Type, NA))
  doc2$HoofdType[substr(doc2$HoofdType,1,10)=='Conference'] <- 'Conference'
  doc2 <- tidyr::gather(doc2, 'jaar','count', -'Bron', -'Type', -'HoofdType')

  doc2[1:3] <- lapply(doc2[1:3], as.factor)
  return(list(doc1=doc1, doc2=doc2))
}

#' Read the numbers from the VSNU, based downloadable Excelsheet
#'
#' Mostly custom, will work on only specific sheet
#'
#' @param xls Paths to excelsheet
#' @return data.frame with data
#' @export
ReadVSNUxls <- function(xls='http://www.vsnu.nl/files/documenten/Feiten_en_Cijfers/Inzet_en_output_2016.xlsx') {
  EmilMisc::libinstandload('readxl','tidyr')
  year <- regexpr('[0-9]{4}\\.xlsx?', xls)
  year <- substring(xls, year, year+3)
  if(grepl('^https?://', xls)) {
    tfile <- tempfile(fileext = sub('^.*/[^\\./]+','', xls))
    utils::download.file(xls, tfile, mode="wb")
    xls <- tfile
    rm(tfile)
  }
  Thesis <- readxl::read_excel(xls, sheet=paste0('Dissertaties  2000-',year))
  Thesis <- Thesis[(which(Thesis[1]=='Dissertaties per universiteit')+1):
                     (which(Thesis[1]=='Dissertaties per universiteit, exclusief HOOP-gebied Gezondheid')),]
  Thesis[1,1] <- 'Universiteit'
  names(Thesis) <- Thesis[1,]
  Thesis <- Thesis[2:(which(Thesis[1]=='Totaal')-1),]
  Thesis <- tidyr::gather(Thesis, 'Jaar','Aantal', -'Universiteit')

  Vak <- readxl::read_excel(xls, sheet=paste0('Vakpublicaties 2000-',year))
  Vak <- Vak[(which(Vak[1]=='Vakpublicaties per universiteit')+1):
                     (which(Vak[1]=='Vakpublicaties per universiteit, exclusief HOOP-gebied Gezondheid')),]
  Vak[1,1] <- 'Universiteit'
  names(Vak) <- Vak[1,]
  Vak <- Vak[2:(which(Vak[1]=='Totaal')-1),]
  Vak <- tidyr::gather(Vak, 'Jaar','Aantal', -'Universiteit')

  Wetensch <- readxl::read_excel(xls, sheet=paste0('Wetensch. publ.  2000-',year))
  Wetensch <- Wetensch[(which(Wetensch[1]=='Wetenschappelijke publicaties per universiteit')+1):
                     (which(Wetensch[1]=='Wetenschappelijke publicaties per universiteit, excl. HOOP-gebied Gezondheid')),]
  Wetensch[1,1] <- 'Universiteit'
  names(Wetensch) <- Wetensch[1,]
  Wetensch <- Wetensch[2:(which(Wetensch[1]=='Totaal')-1),]
  Wetensch <- tidyr::gather(Wetensch, 'Jaar','Aantal', -'Universiteit')
  Thesis$Categorie <- 'Thesis'
  Vak$Categorie <- 'Vak'
  Wetensch$Categorie <- 'Wetensch'
  return(as.data.frame(lapply(rbind(Thesis, Wetensch, Vak)[c(4,1,2,3)], identity), stringsAsFactors=TRUE))
}

#' Apply NARCIS bussiness-rules for determining access-status
#'
#' To generate NARCIS-content, publications are harvested over OAI-PMH, with different schemas.
#' This function outputs access-status based on content.
#'
#' @param rec A list with record-information for one record
#' @param version Implemented for forward compatibilty: if the business-rules change this function will be updated, but only for higher version-numbers.
#' Currently only 1 is implemented
#' @return A nested list with information
#' @export
CalcAccess <- function(rec, version=1) {
  if(!version %in% c(1)) stop('Version-number not implemented (yet)')
  if(version==1) {
    if(!exists('Params')) Params <- list()
    if(is.null(Params$Accesslevels)) Params$AccessLevels <- factor(c('OpenAccess', 'RestrictedAccess', 'EmbargoedAccess','ClosedAccess'),
                                                                   levels=c('OpenAccess', 'RestrictedAccess', 'EmbargoedAccess','ClosedAccess'), ordered = T)
    ac <- rec$Item$Item$Component$Resource$mods
    ac <- ac[names(ac)=='accessCondition']
    if(length(ac)>0) {
      act <- unname(sapply(ac, function(ac1) {
        if(class(ac1)=='list' && 'text' %in% names(ac1)) ac1 <- ac1$text
        if(length(ac1)>1) {
          ac1 <- ac1[names(ac1)!='displayLabel']
          if(length(ac1)>1) stop('Access condition has multiple fields, unclear which to use (line ',utils::getSrcLocation(function(x) {x}),')')
        }
        return(which(sapply(Params$AccessLevels, grepl, x=ac1, ignore.case=T))[1])
      }))
      act <- act[!is.na(act)]
      if(length(act)>1) act <- act[1]
    } else act <- NULL
    if(length(act)>0) {
      objfiles <- rec$Item[names(rec$Item)=='Item']
      objfiles <- objfiles[sapply(objfiles, function(f) {
        f$Descriptor$Statement$type[['resource']]=='info:eu-repo/semantics/objectFile'
      })]
      if(length(objfiles)==0) {
        prod <- 4
        allrights <- c(prod, act)
      } else {
        allrights <- unlist(objfiles)
        allrights <- unname(allrights[names(allrights)=='Item.Descriptor.Statement.accessRights'])
        if(length(allrights)==0) {
          prod <- 1
          allrights <- c(prod, act)
        } else {
          allrights <- unname(sapply(allrights, function(ac1) {
            if(length(ac1)>1) {
              ac1 <- ac1[names(ac1)!='displayLabel']
              if(length(ac1)>1) stop('Access condition has multiple fields, unclear which to use (line ',utils::getSrcLocation(function(x) {x}),')')
            }
            which(sapply(Params$AccessLevels, grepl, x=ac1, ignore.case=T))
          }))
          if(length(allrights)>0) allrights <- allrights[!is.na(allrights)]
          if(length(allrights)<length(objfiles)) {
            prod <- 1
            allrights <- c(act, allrights, 1)
          } else {
            prod <- min(allrights)
            allrights <- c(act, allrights)
          }
        }
      }
    } else {
      objfiles <- rec$Item[names(rec$Item)=='Item']
      objfiles <- objfiles[sapply(objfiles, function(f) {
        !is.null(f$Descriptor$Statement$type[['resource']]) && f$Descriptor$Statement$type[['resource']]=='info:eu-repo/semantics/objectFile'
      })]
      if(length(objfiles)==0) {
        allrights <- act <- prod <- 4
      } else {
        allrights <- unlist(objfiles)
        allrights <- unname(allrights[names(allrights)=='Item.Descriptor.Statement.accessRights'])
        if(length(allrights)==0) {
          allrights <- act <- prod <- 1
        } else {
          allrights <- unname(sapply(allrights, function(ac1) {
            which(sapply(Params$AccessLevels, grepl, x=ac1, ignore.case=T))[1]
          }))
          allrights <- allrights[!is.na(allrights)]
          if(length(allrights)<length(objfiles)) {
            prod <- 1
            act <- min(allrights)
            allrights <- c(allrights, 1)
          } else {
            prod <- act <- min(allrights)
          }
        }
      }
    }
    return(list(Act=as.character(Params$AccessLevels[act]),
                Prod=as.character(Params$AccessLevels[prod]),
                All=as.character(Params$AccessLevels[allrights])))
  }
}

#' Does a string contain a DOI, and extract it
#'
#' Basically just regular expressions, wrapped in a function to simplify adaptations
#' DOIify also converts to lowercase to ease comparisons. If an ID that is not a DOI is passed to DOIify it is returned unaltered.
#' An ID is seen as a DOI when it satisfies the regular expression "10.\.[0-9]{4,5}/"
#'
#' @param ID Character vector of IDs to process
#' @return for is.DOI: a logical of length(ID)
#' for DOIify: a character vector of length(ID) with the bare DOIs (starting with 10.). If ID contains non-DOIs they are returned unaltered
#'
#' @export
is.DOI <- function(ID) {
  grepl('10\\.[0-9]{4,5}/', ID)
}
#' @rdname is.DOI
#' @export
DOIify <- function(ID) {
  tolower(substring(ID, regexpr('10\\.[0-9]{4,5}/', ID)))
}


#' Decide color based on Unpaywall-data
#'
#' Based on 4 criteria from Unpaywall:
#' \itemize{
#'   \item is_oa (logical)
#'   \item host_type (character or factor, must be 'publisher', 'repository' if is_oa is TRUE)
#'   \item journal_is_oa (logical)
#'   \item license (character or factor)
#' }
#' strings are case-insensitive
#' Applied rules are:
#' \enumerate{
#'   \item if is_oa is NA, 'unknown' is returned
#'   \item if is_oa is FALSE, 'closed' is returned
#'   \item if host_type is 'repository', 'green' is returned
#'   \item if host_type is 'publisher' and 'journal_is_oa' is TRUE, 'pure gold' is returned
#'   \item if host_type is 'publisher' and 'license' is NA or an empty string, 'bronze' is returned
#'   \item if host_type is 'publisher' and 'license' is not NA or an empty, 'hyrbid' is returned
#'   \item if we reach this step is_oa is TRUE, but host_type is unknown. SO we return 'unclear (error)'
#' }
#'
#' @param ..., df Either single vectors, or a data.frame with appropriate columns (or a list with equal-length elements)
#' Should either be named with the names "c('is_oa', 'host_type', 'journal_is_oa','license')", or be of length 4, in this order
#' @return A vector of colors (as a factor)
#'
#' @export

oa_color <- function(..., df=NULL) {
  expNames <- c('is_oa', 'host_type', 'journal_is_oa','license')
  expClasses <- list('logical',c('character','factor'), 'logical', c('character','factor'))
  if(missing(df) & missing(...)) {
    stop('Insufficient arguments')
  }
  if(!missing(df)) {
    if(!missing(...))
      warning('oa_color was provided with too many arguments, ignoring all but df')
    df <- as.list(df)
  } else {
    df <- list(...)
  }
  if(!all(expNames %in% names(df)) && (length(df)!=length(expNames) || !all(mapply(df, expClasses, FUN=function(col, cl) {class(col) %in% cl}))))
    stop('Not all expected columns (with right classes) present')
  if(!is.null(names(df)) && all(names(df) %in% expNames) && !all(names(df)==expNames))
    df <- df[match(expNames, names(df))]
  len <- length(df[[1]])
  for(i in 1:length(expNames)) {
    assign(expNames[i], df[[i]])
    if(length(df[[i]])!=len) stop('Unexpected length of argument ',expNames[i])
  }
  as.factor(ifelse(is.na(is_oa),
                   'unknown',
                   ifelse(is_oa,
                          ifelse(!is.na(host_type) & tolower(host_type)=="repository",
                                 "green",
                                 ifelse(!is.na(host_type) & tolower(host_type)=="publisher",
                                        ifelse(journal_is_oa,
                                               "pure gold",
                                               ifelse(!is.na(license) & license!='',
                                                      "hybrid",
                                                      "bronze")),
                                        ifelse('unclear (error)'))),
                          'closed')))
}



















