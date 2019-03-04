#-------------------------------------
# Author: Babak Naimi, naimi.b@gmail.com
# Date (first version): March 2018
# Date (last update):  March 2019
# Version 0.3
# Licence GPL v3
#------------------------

.getHeaderInfo <- function(filename) {
  # get info from raster.grd
  o <- list()
  sx <- readLines(filename)
  #sx <- scan(filename,'character',quiet = TRUE)
  ux <- unlist(lapply(sx,function(x) .trim(strsplit(x,'=')[[1]][1])))
  ux <- unlist(lapply(sx,function(x) strsplit(x,'=')[[1]][1]))
  w <- which(ux == "datatype")
  o[['datatype']] <- strsplit(sx[w],'=')[[1]][2]
  w <- which(ux == "bandorder")
  o[['bandorder']] <- strsplit(sx[w],'=')[[1]][2]
  w <- which(ux == "nbands")
  o[['nlayers']] <- as.numeric(strsplit(sx[w],'=')[[1]][2])
  w <- which(ux == "layername")
  o[['layerNames']] <- strsplit(strsplit(sx[w],'=')[[1]][2],':')[[1]]
  o[['ncells']] <- as.numeric(strsplit(sx[which(ux == "nrows")],'=')[[1]][2]) * as.numeric(strsplit(sx[which(ux == "ncols")],'=')[[1]][2])

  o[['nrows']] <- as.numeric(strsplit(sx[which(ux == "nrows")],'=')[[1]][2])

  o[['ncols']] <- as.numeric(strsplit(sx[which(ux == "ncols")],'=')[[1]][2])

  w <- which(ux == 'factors')
  if (length(w) > 0) {
    .v <- strsplit(sx[w],'=')[[1]][2]
    .levels <- list()
    .nf <- strsplit(.v,':')[[1]]
    o[['factors']] <- .nf
    for (.n in .nf) {
      w <- which(ux == paste0('\t',.n))
      .v <- strsplit(sx[w],'=')[[1]][2]
      o[['factor_lavels']][[.n]] <- strsplit(.v,':')[[1]]
    }
  } else o[['factors']] <- NULL

  o
}
#-----
.getValues <- function(m,nl,cells,n,bo='BSQ',.ncol=NULL) {
  .n <- length(m)/nl
  d <- data.frame(matrix(nrow=length(cells),ncol = nl))
  colnames(d) <- n

  if (bo == 'BSQ') {
    for (i in 1:nl) {
      d[,i] <- m[(.n*(i-1)) + cells]
    }
  } else if (bo == 'BIP') {
    for (i in 1:nl) {
      d[,i] <- m[(cells-1)*nl + i]
    }
  } else if (bo == 'BIL') {
    for (i in 1:nl) {
      d[,i] <- m[rep(cells + trunc((cells - 1)/.ncol) * .ncol * (nl-1) , each=1) + (i-1) * .ncol]
    }

  }
  d
}


.readRasterR6 <- function(filename) {
  filename <- .normalizePath(filename)
  if (!file.exists(filename)) stop('filename does not exist!')
  if (file.info(filename)$isdir) {
    if (!file.exists(paste0(filename,'/predictors.gri'))) stop('the specified sdm file object does not contain a raster object!')
    if (!file.exists(paste0(filename,'/',basename(filename),'.info'))) {
      if (!file.exists(paste0(filename,'/predictors.grd'))) stop('the raster object file is incomplete!')
      else .meta <- .getHeaderInfo(paste0(filename,'/predictors.grd'))
    } else .meta <- .getRasterInfo(paste0(filename,'/',basename(filename),'.info'))
    filename <- paste0(filename,'/predictors.gri')
  } else {
    nx <- strsplit(filename,'\\.')[[1]]
    if (length(nx) < 2 || !nx[length(nx)] %in% c('grd','gri')) stop('the raster format is not recognised!')

    nx <- nx[-length(nx)]
    if (length(nx) > 1) nx <- paste(nx,collapse='.')

    filename <- paste0(paste0(nx,'.gri'))

    if (!file.exists(paste0(nx,'.grd'))) stop(paste0(nx,'.grd (metadata of the raster file) does not exist!'))
    if (!file.exists(filename)) stop(paste0(nx,'.gri (the file of raster values) does not exist!'))

    .meta <- .getHeaderInfo(paste0(nx,'.grd'))
  }

  if (!is.null(.meta$factors)) .f <- .meta$factor_lavels
  else .f <- NULL
  .m <- .mmapRaster$new(filename=filename,nlayers=.meta$nlayers,bandorder=.meta$bandorder,
                       datatype=.meta$datatype,nrows=.meta$nrows,ncols=.meta$ncols,names=.meta$layerNames,factors=.f)
  .m
}
#-------------


if (!isGeneric("getRasterValues")) {
  setGeneric("getRasterValues", function(x,...)
    standardGeneric("getRasterValues"))
}


setMethod('getRasterValues', signature(x='mmapRaster'),
          function(x,cells,...) {
            if (!requireNamespace('mmap')) stop('package mmap is required, but is not installed on this machine!')
            #dtype <- switch(x@datatype,'FLT8S'=real64(),'FLT4S'=real32(),'INT1U'=uint8(),'LOG1S'=logi8(),'INT1S'=int8(),
            #                'INT2S'=int16(),'INT2U'=uint16(),'INT4S'=int32(),'INT4U'=uint32())
            #m <- mmap::mmap(x@filename,mode = dtype)

            #.x <- .getValue(m,nl=x@nlayers,cells = cells,n=x@names,bo =x@bandorder)
            #mmap::munmap(m)
            x$getValues(cells)

          }
)


if (!isGeneric("mraster")) {
  setGeneric("mraster", function(x,...)
    standardGeneric("mraster"))
}


setMethod('mraster', signature(x='character'),
          function(x,...) {
            .readRasterR6(normalizePath(x,'/'))

          }
)


