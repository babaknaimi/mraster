###################
#-----------------
# Author: Babak Naimi, naimi.b@gmail.com
# Date (first version): August 2017
# Date (last update):  March 2019
# Version 0.8
# Licence GPL v3

#-----------------
###################
.change_unit <- function(x,.from,.to) {
  # it is used for file size
  .u <- c('B','K','M','G','T','P')
  .w1 <- which(.u == .from)
  .w2 <- which(.u == .to)
  .w <- .w1-.w2
  x * (1024^.w)
}
#------------

.mem_used <- function (.u,.r=NULL) {
  # report the memory used by R objects in the session (based on the code of mem_used function in pryr)
  o <- .change_unit(sum(gc()[, 1] * c(8L * .Machine$sizeof.pointer, 8)),'B',.u)
  if (!is.null(.r)) round(o,.r)
  else o
}
#---------
.getPhysicalMemory_linux <- function(.unit='M') {
  if (!.unit %in% c('B','K','M','G','T')) {
    warning('.unit should be one of B, K, M, G, T, or P; it is changed to "M" (i.e., Mb)')
    .unit <- 'M'
  }

  .free <- .total <- NULL
  .a <- quote({
    .free <- as.numeric(system("free -l 1 | grep Mem | awk '{print $4}'",intern = TRUE))
    .total <- as.numeric(system("free -l 1 | grep Mem | awk '{print $2}'",intern=TRUE))
    .free <- .change_unit(.free,'K',.unit)
    .total <- .change_unit(.total,'K',.unit)
  })

  a <- try(eval(.a),silent=TRUE)
  if (inherits(a,'try-error')) {
    .a <- quote({
      .free <- as.numeric(system("grep MemTotal /proc/meminfo | awk '{print $2}'",intern = TRUE))
      .total <- as.numeric(system("grep MemFree /proc/meminfo | awk '{print $2}'",intern=TRUE))
      .free <- .change_unit(.free,'K',.unit)
      .total <- .change_unit(.total,'K',.unit)
    })
    a <- try(eval(.a),silent=TRUE)
  }
  c(total=.total,free=.free)
}

#--------------

.getPhysicalMemory_osx <- function(.unit='M') {
  if (!.unit %in% c('B','K','M','G','T','P')) {
    warning('.unit should be one of B, K, M, G, T or P; it is changed to "M" (i.e., Mb)')
    .unit <- 'M'
  }
  .free <- .total <- NULL

  .a <- quote({
    .total <- as.numeric(strsplit(system("sysctl hw.memsize",intern = TRUE),": ")[[1]][2])
    .free <- as.numeric(strsplit(system("sysctl vm.page_free_count",intern = TRUE),": ")[[1]][2])
    .w <- try(as.numeric(strsplit(system("sysctl hw.pagesize",intern = TRUE),": ")[[1]][2]),silent = TRUE)
    if (inherits(.w,'try-error')) .w <- 4096
    .free <- .free * .w
    .free <- .change_unit(.free,'B',.unit)
    .total <- .change_unit(.total,'B',.unit)
  })

  a <- try(eval(.a),silent=TRUE)

  if (inherits(a,'try-error')) {
    .a <- quote({
      a <- system("top -l 1 | grep PhysMem:", intern = TRUE)
      a <- strsplit(strsplit(a,': ')[[1]][[2]],' ')[[1]]
      #---- for total memory:
      .w <- strsplit(a[1],'')[[1]]
      .ww <- .w[length(.w)]
      .w <- as.numeric(paste(.w[1:(length(.w)-1)],collapse=''))
      if (.ww %in%  c('B','K','M','G','T')) {
        .total <- .change_unit(.w,.ww,.unit) # get the total memory in Mb
      } else if (!is.na(as.numeric(.ww))) {
        .w <- as.numeric(a[1])
        .total <- .change_unit(.w,'B',.unit) # get the total memory in Mb
        # we assumed it is in byte here!!! (but not likely to get here)
      }
      #---- for free memory:
      .w <- strsplit(a[5],'')[[1]]
      .ww <- .w[length(.w)]
      .w <- as.numeric(paste(.w[1:(length(.w)-1)],collapse=''))
      if (.ww %in%  c('B','K','M','G','T')) {
        .free <- .change_unit(.w,.ww,.unit) # get the total memory in Mb
      } else if (!is.na(as.numeric(.ww))) {
        .w <- as.numeric(a[5])
        .free <- .change_unit(.w,'B',.unit) # get the total memory in Mb
        # we assumed it is in byte here!!! (but not likely to get here)
      }
      .total <- .tatal + .free
    })
    a <- try(eval(.a),silent=TRUE)
  }

  c(total=.total,free=.free)
}
#-----------

.getPhysicalMemory_win <- function(.unit='M') {

  if (!.unit %in% c('B','K','M','G','T','P')) {
    warning('.unit should be one of B, K, M, G, T, or P; it is changed to "M" (i.e., Mb)')
    .unit <- 'M'
  }
  .free <- .total <- NULL

  .a <- quote({
    .free <- system('wmic OS get FreePhysicalMemory /Value',intern = TRUE)[3]
    .free <- as.numeric(strsplit(.free,'=')[[1]][2]) / (1024)
    .free <- .change_unit(.free,'K',.unit)

    .total <- system('wmic computersystem get TotalPhysicalMemory /Value',intern = TRUE)[3]
    .total <- as.numeric(strsplit(.total,'=')[[1]][2])
    .tatal <- .change_unit(.total,'B',.unit)
  })

  a <- try(eval(.a),silent=TRUE)
  c(total=.total,free=.free)
}
#-------------
.get_os <- function() {
  s <- Sys.info()[['sysname']]
  if (!is.null(s)) {
    if (s == 'Windows') os <- 'windows'
    else if (s == 'Darwin') os <- 'osx'
    else if (s == 'Linux') os <- 'linux'
    else {
      if (.Platform$OS.type == 'windows') os <- 'windows'
      else if (.Platform$OS.type == 'unix') {
        if (grepl("^darwin", R.version$os)) os <- "osx"
        else if (grepl("linux-gnu", R.version$os)) os <- "linux"
      } else os <- s
    }
  } else {
    if (.Platform$OS.type == 'windows') os <- 'windows'
    else if (.Platform$OS.type == 'unix') {
      if (grepl("^darwin", R.version$os)) os <- "osx"
      else if (grepl("linux-gnu", R.version$os)) os <- "linux"
    } else os <- 'unix'
  }
  os
}
#------------

if (!isGeneric("memory")) {
  setGeneric("memory", function(u,...)
    standardGeneric("memory"))
}



setMethod('memory', signature(u='characterORmissing'),
          function(u='M',session=FALSE, echo=TRUE, .r=5) {
            if (missing(echo)) echo <- TRUE
            if (missing(session)) session <- FALSE
            if (missing(.r)) .r <- 5
            if (missing(u)) u <- 'M'
            u <- toupper(u)
            if (!u %in% c('B','K','M','G','T','P')) {
              warning("size unit (u) should be either of 'B','K','M','G','T','P'...; 'M' (Mb) is considered!")
              u <- 'M'
            }
            gc(verbose=FALSE)

            .os <- .get_os()

            if (.os == 'windows') o <- .getPhysicalMemory_win(u)
            else if (.os == 'linux') o <- .getPhysicalMemory_linux(u)
            else if (.os == 'osx') o <- .getPhysicalMemory_osx(u)
            else {
              o <- .getPhysicalMemory_linux(u)
              if (length(o) == 0 || any(is.null(o))) o <- .getPhysicalMemory_osx(u)
            }
            if (session) {
              o <- c(o, used_by_this_session=.mem_used(u,.r))
            }
            if (is.numeric(o)) {
              if (!is.null(.r)) o <- round(o,.r)

              if (echo) {
                .n <- names(o)
                .o <- paste(o,paste0(u,'b'))
                #return(c(total=o[1],free=o[2]))
                names(.o) <- .n
                print(.o)
              }
            }


            invisible(o)
          }
)


