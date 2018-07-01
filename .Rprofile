print('.Rprofile run for project: NARCISOAfunc')
options(error=utils::recover)
options(warn=1)
options(stringsAsFactors=FALSE)

options(Paths=local({
  Paths <- getOption("Paths")
  Paths$RCode <- paste0(getwd(),"/R")
  # Extend path-list here
  return(Paths)
}))

invisible(sapply(c('EmilMisc','devtools','roxygen2',local({
  libs <- readLines('DESCRIPTION')
  if(any(grepl('Imports:',libs))) {
    imps <- libs[grep('Imports:',libs):length(libs)]
    imps <- imps[(2-(nchar(imps[1])>8)):(grep('^[^ ]', imps[-1]))[1]]
    imps <- gsub('(^ +)|(^Imports: *)|( *\\(.*\\)),?','',imps)
  } else imps <- NULL
  if(any(grepl('Depends:',libs))) {
    deps <- libs[grep('Depends:',libs):length(libs)]
    deps <- deps[(2-(nchar(deps[1])>8)):(grep('^[^ ]', deps[-1]))[1]]
    deps <- gsub('(^ +)|(^Depends: *)|( *\\(.*\\)),?','',deps)
    deps <- deps[deps!='R']
  } else deps <- NULL
  c(imps, deps)
})), function(p) {suppressPackageStartupMessages(library(p, character.only=TRUE))}))
