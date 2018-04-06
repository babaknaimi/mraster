#-------------------------------------
# Author: Babak Naimi, naimi.b@gmail.com
# Date (first version): October 2017
# Date (last update):  October 2017
# Version 0.1
# Licence GPL v3
#------------------------
.trim <- function (x) {
  x <- strsplit(x, "")[[1]]
  paste(x[x != " "], collapse = "")
}
#----
.LD <- function(s,t) {
  sl <- unlist(strsplit(s,''))
  tl <- unlist(strsplit(t,''))
  if (s == t) return(0)
  else if (length(sl) == 0) return(length(tl))
  else if (length(tl) == 0) return(length(sl))
  v0 <- 0:length(tl)
  v1 <- rep(NA,length(tl)+1)
  for (i in seq_along(sl)) {
    v1[1] <- i
    for (j in seq_along(tl)) {
      if (sl[i] == tl[j]) cost <- 0
      else cost <- 1
      v1[j+1] <- min(v1[j] + 1, v0[j + 1] + 1, v0[j] + cost)
    }
    for (j in seq_along(v0)) {
      v0[j] <- v1[j]
    }
  }
  return(v1[length(tl)+1])
}
#----------
.agrep <- function(n,choices, r=seq(0,0.3,0.05)) {
  # r is a range can be used for max distance
  for (i in r) {
    w <- agrep(n,choices,ignore.case = TRUE,max.distance = list(all=i))
    if (length(w) > 0) break
  }
  if (length(w) > 1) {
    d <- unlist(lapply(choices[w],function(x) .LD(n,x)))
    w2 <- which(d == min(d))
    if (length(w2) == 1) choices[w][w2]
    else NA
  } else if (length(w) == 1) choices[w]
  else NA
}
#-----------
.pmatch <- function(n,choices) {
  for (i in seq_along(n)) {
    if (n[i] != '') {
      if (!n[i] %in% choices) {
        u <- try(match.arg(tolower(n[i]),tolower(choices)),silent=TRUE)
        if (!inherits(u,"try-error")) {
          n[i] <- choices[which(tolower(choices) == u)]
        } else {
          n[i] <- .agrep(n[i],choices)
        }
      }
    } else n[i] <- NA
  }
  n
}
#--------

.getSeparator <- function(x) {
  if (length(strsplit(x,'/')[[1]]) != 1) '/'
  else '\\'
}
#---

.normalizePath <- function(x) {
  if (requireNamespace('raster')) x <- raster::trim(x)
  xx <- strsplit(x,'')[[1]]
  if (xx[length(xx)] %in% c('/','\\')) {
    xx <- xx[-length(xx)]
    x <- paste(xx,collapse='')
  }
  normalizePath(x,winslash = .getSeparator(x),mustWork = FALSE)
}
#---------
.fileBase <- function (x, overwrite=FALSE) {
  if (file.exists(x)) {
    if (!file.info(x)$isdir) stop("the new filename cannot be created because a file (which has not been created by the mraster package) with a name similar to the specified newfile exist!")
    if (!overwrite) stop(paste(x,"exists. You can use overwrite=TRUE to overwrite it!"))
    unlink(x, recursive = TRUE,force=TRUE)
  }
  dir.create(x,recursive = TRUE)
  file.exists(x)
}
#---------
.trim <- function (x) {
  x <- strsplit(x, "")[[1]]
  paste(x[x != " "], collapse = "")
}
