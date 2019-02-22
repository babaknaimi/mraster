#-------------------------------------
# Author: Babak Naimi, naimi.b@gmail.com
# Date (first version): April 2018
# Date (last update):  Feb. 2019
# Version 0.3
# Licence GPL v3
#------------------------


.createEmptyRaster <- function(filename,nrow,ncol,nlayers=1,extent,dtype='FLT8S',bandorder='BSQ',crs=NA,overwrite=FALSE,echo=FALSE) {
  require(mmap)
  .meta <- .createRasterMetadata(nrow,ncol,extent,nlayers,dtype,bandorder,crs)
  #a <- c('FLT8S','FLT4S','INT1U','LOG1S','INT1S','INT2S','INT2U','INT4S','INT4U')
  #datatype <- .pmatch(datatype[1],a)
  .na <- switch(dtype,'FLT8S'= NA,'FLT4S'=NA_real_,'INT1U'=NA_integer_,'LOG1S'=NA_integer_,'INT1S'=NA_integer_,
                  'INT2S'=NA_integer_,'INT2U'=NA_integer_,'INT4S'=NA_integer_,'INT8S'=NA)
  dtype <- switch(dtype,'FLT8S'=real64(),'FLT4S'=real32(),'INT1U'=uint8(),'LOG1S'=logi8(),'INT1S'=int8(),
                  'INT2S'=int16(),'INT2U'=uint16(),'INT4S'=int32(),'INT8S'=int64())


  .s <- strsplit(filename,'\\.')[[1]]
  if (length(.s) > 1 && .s[length(.s)] %in% c('grd','gri')) filename <- paste(.s[-length(.s)],collapse = '.')

  if (file.exists(paste0(filename,'.gri'))) {
    if (overwrite) unlink(paste0(filename,'.gri'))
    else stop('filename exists; use different name or use the argument overwrite=TRUE')
  }
  ncell <- nrow * ncol

  zzz <-file(paste0(filename,'.gri'),'ab')
  .x <- c(writeBin(.na, raw(), size=sizeof(dtype)))
  for (i in 1:nlayers) writeBin(rep(.x, ncell), zzz)
  close(zzz)

  ._writeHdrRasterForEmptyRaster(.meta,paste0(filename,'.grd'))
  if (echo) cat('The empty raster is successfully creater!')
}
#--------

# write a numeric vector of values (x) into a raster:
.writeInRasterV <- function(x,filename,band=NULL,cells=NULL) {
  .s <- strsplit(filename,'\\.')[[1]]

  if (length(.s) > 1 && .s[length(.s)] %in% c('grd','gri')) filename <- paste(.s[-length(.s)],collapse = '.')

  .meta <- .getHeaderInfo(paste0(filename,'.grd'))

  if (is.null(cells) || is.na(cells)) {
    cells <- 1:length(x)
  } else {
    if (length(cells) != length(x)) stop('length of cells should be the same as the length of x')
  }

  if (is.null(band) || is.na(band)) band <- 1
  else if (length(band) > 1) stop('the data are one dimension while you specified more than a single band!')

  if (band > .meta$nlayers) stop('the specified band does not exist!')


  dtype <- .meta$datatype
  dtype <- switch(dtype,'FLT8S'=real64(),'FLT4S'=real32(),'INT1U'=uint8(),'LOG1S'=logi8(),'INT1S'=int8(),
                  'INT2S'=int16(),'INT2U'=uint16(),'INT4S'=int32(),'INT4U'=uint32())

  m <- mmap::mmap(paste0(filename,'.gri'),dtype)

  if (.meta$bandorder == 'BSQ') {
    a <- ((band - 1) * .meta$ncells) + cells
    m[a] <- x
  } else if (.meta$bandorder == 'BIP') {
    m[seq(.meta$nlayers*(cells[1]-1) + band,.meta$ncells*.meta$nlayers,by=.meta$nlayers)] <- x
  } else {
    a <- rep(cells + trunc((cells - 1) / .meta$ncols) * .meta$ncols * (.meta$nlayers-1) , each=1) + (band-1) * .meta$ncols
    m[a] <- x
  }
  munmap(m)
}

#-----------
# x can be Matrix OR Data.frame:
.writeInRasterM <- function(x,filename,band=NULL,cells=NULL,start=NULL,end=NULL) {
  .s <- strsplit(filename,'\\.')[[1]]

  if (length(.s) > 1 && .s[length(.s)] %in% c('grd','gri')) filename <- paste(.s[-length(.s)],collapse = '.')

  .meta <- .getHeaderInfo(paste0(filename,'.grd'))

  .dx <- dim(x)

  if (is.null(cells) || is.na(cells)) {
    cells <- 1:dx[1]
  } else {
    if (length(cells) != nrow(x)) stop('length of cells should be the same as the number of rows in x')
  }


  if (is.null(band) || is.na(band)) band <- 1:.dx[2]
  else {
    if (length(band) != .dx[2]) stop('the dimension of the input values are not the same as the specified bands!')
    if (any(band > .meta$nlayers)) stop('at least one of the specified bands does/do not exist!')
  }

  dtype <- .meta$datatype
  dtype <- switch(dtype,'FLT8S'=real64(),'FLT4S'=real32(),'INT1U'=uint8(),'LOG1S'=logi8(),'INT1S'=int8(),
                  'INT2S'=int16(),'INT2U'=uint16(),'INT4S'=int32(),'INT4U'=uint32())

  m <- mmap::mmap(paste0(filename,'.gri'),dtype)

  if (.meta$bandorder == 'BSQ') {
    for (i in 1:.dx[2]) {
      a <- ((band[i] - 1) * .meta$ncells) + cells
      m[a] <- as.vector(x[,i])
    }
  } else if (.meta$bandorder == 'BIP') {
    for (i in 1:.dx[2]) {
      m[seq(.meta$nlayers*(cells[1]-1) + band[i],.meta$ncells*.meta$nlayers,by=.meta$nlayers)] <- as.vector(x[,i])
    }
  } else {
    for (i in 1:.dx[2]) {
      a <- rep(cells + trunc((cells - 1) / .meta$ncols) * .meta$ncols * (.meta$nlayers-1) , each=1) + (band[i]-1) * .meta$ncols
      m[a] <- as.vector(x[,i])
    }
  }
  munmap(m)
}

#-----------
# Create and write a header fro the created empty raster:
._writeHdrRasterForEmptyRaster <- function(.meta,filename) {
  # copied from the raster package (Robert Hijmans) and adjusted!
  thefile <- file(filename, "w")  # open an txt file connection
  cat("[general]", "\n", file = thefile, sep='')
  #cat("creator=R package 'sdm' (can be also read by package 'raster')", "\n", file = thefile, sep='')
  cat("creator=R package 'sdm' (can be opened in the 'raster' package as well)", "\n", file = thefile, sep='')
  cat("created=", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", file = thefile, sep='')

  cat("[georeference]", "\n", file = thefile, sep='')
  cat("nrows=",  .meta$nrows, "\n", file = thefile, sep='')
  cat("ncols=",  .meta$ncols, "\n", file = thefile, sep='')
  cat("xmin=", as.character(.meta$xmin), "\n", file = thefile, sep='')
  cat("ymin=", as.character(.meta$ymin), "\n", file = thefile, sep='')
  cat("xmax=", as.character(.meta$xmax), "\n", file = thefile, sep='')
  cat("ymax=", as.character(.meta$ymax), "\n", file = thefile, sep='')
  cat("projection=", .meta$projection, "\n", file = thefile, sep='')

  cat("[data]", "\n", file = thefile, sep='')
  cat("datatype=",  .meta$datanotation[1], "\n", file = thefile, sep='')
  cat("byteorder=", .meta$byteorder[1], "\n", file = thefile, sep='')
  cat("nbands=",  .meta$nlayers, "\n", file = thefile, sep='')
  cat("bandorder=",  .meta$bandorder[1], "\n", file = thefile, sep='')


  # currently only for single layer files!
  if (.meta$nlayers == 1) {
    fact <- .meta$is.factor[1]
    cat("categorical=", paste(fact, collapse=':'), "\n", file = thefile, sep='')
    if (any(fact)) {
      r <- .meta$attributes[[1]]
      cat("ratnames=", paste(colnames(r), collapse=':'), "\n", file = thefile, sep='')
      cat("rattypes=", paste(sapply(r, class), collapse=':'), "\n", file = thefile, sep='')
      cat("ratvalues=", paste(raster::trim(as.character(as.matrix(r))), collapse=':'), "\n", file = thefile, sep='')
    }
    if (length(x@legend@colortable) > 1) {
      cat("colortable=", paste(.meta$legend@colortable, collapse=':'), "\n", file = thefile, sep='')
    }
  }

  cat("minvalue=",  paste(.meta$minValue, collapse=':'), "\n", file = thefile, sep='')
  cat("maxvalue=",  paste(.meta$maxValue, collapse=':'), "\n", file = thefile, sep='')
  cat("nodatavalue=", paste(.meta$nodataValue, collapse=':'), "\n", file = thefile, sep='')

  cat("[legend]", "\n", file = thefile, sep='')
  #cat("legendtype=",  .meta$legend@type, "\n", file = thefile, sep='')
  #cat("values=",  paste(.meta$legend@values, collapse=':'), "\n", file = thefile, sep='')
  #cat("color=",  paste(.meta$legend@color, collapse=':'), "\n", file = thefile, sep='')

  # in .meta generated from RasterStack, these items are list (later should be updated!)
  cat("legendtype=", "\n", file = thefile, sep='')
  cat("values=",  "\n", file = thefile, sep='')
  cat("color=",  "\n", file = thefile, sep='')
  #----------------
  cat("[description]", "\n", file = thefile, sep='')
  ln <- gsub(":", ".", .meta$names)
  cat("layername=", paste(ln, collapse=':'), "\n", file = thefile, sep='')

  if (! is.null(.meta$z) && length(.meta$z) > 0) {
    zname <- names(.meta$z)[1]
    if (is.null(zname)) {
      zname <- 'z-value'
    }
    zclass <- class(z)
    # suggested by Michael Sumner
    if (inherits(.meta$z, "POSIXct")) {
      .meta$z <- format(.meta$z, "%Y-%m-%d %H:%M:%S", tz="UTC")
    } else {
      .meta$z <- as.character(.meta$z)
    }

    cat("zvalues=", paste(c(zname, .meta$z), collapse=':'), "\n", file = thefile, sep='')
    cat("zclass=", zclass, "\n", file = thefile, sep='')
  }

  a <- NULL
  try( a <- unlist(.meta$history), silent=TRUE )
  if (!is.null(a)) {
    cat("history=", a, "\n", file = thefile, sep='')
  }

  a <- NULL
  try( a <- rapply(.meta$history, function(x) paste(as.character(x), collapse='#,#')), silent=TRUE )
  if (!is.null(a)) {
    a <- gsub('\n', '#NL#', a)
    type <- rapply(.meta$history, class)
    type_value <- apply(cbind(type, a), 1, function(x) paste(x, collapse=':'))
    name_type_value <- apply(cbind(names(a), type_value), 1, function(x) paste(x, collapse='='))
    name_type_value <- paste(name_type_value, '\n', sep='')
    cat("[metadata]", "\n", file = thefile, sep='')
    cat(name_type_value, file = thefile, sep='')
  }

  if (any(.meta$isfactor) && length(.meta$attributes) == .meta$nlayers) {
    cat("[categorical]", "\n", file = thefile, sep='')
    cat("factors =",paste0(.meta$names[.meta$isfactor],collapse=':'),'\n',file=thefile,sep='')
    cat("\t[factor levels]\n",file=thefile,sep='')
    r <- .meta$attributes[.meta$isfactor]
    .nl <- .meta$names[.meta$isfactor]
    for (i in 1:length(r)) {
      if (is.data.frame(r[[i]][[1]]) && ncol(r[[i]][[1]]) > 1) {
        .l <- as.character(r[[i]][[1]][,2])
        cat("\t",.nl[i]," =",paste0(.l,collapse=':'),'\n',file=thefile,sep='')
      }
    }
  }
  close(thefile)
  return(TRUE)
}
#----------
# A metadata object for empty raster (used as input to ._writeHdrRasterForEmptyRaster, by .createEmptyRaster):
.createRasterMetadata <- function(nrow,ncol,extent,nlayers,dtype='FLT8S',bandorder='BSQ',crs=NULL,layerNames=NULL) {
  .meta <- list()
  .meta$nrows <- nrow
  .meta$ncols <- ncol
  .meta$xmin <- extent[1]
  .meta$ymin <- extent[3]
  .meta$xmax <- extent[2]
  .meta$ymax <- extent[4]
  .meta$projection <- crs
  .meta$minValue <- NA
  .meta$maxValue <- NA
  .meta$nodataValue <- -Inf
  .meta$z <- list()
  .meta$history <- list()
  .meta$nlayers <- nlayers

  if (nlayers > 1) {
    .meta$datanotation <- dtype
    .meta$byteorder <- 'little'
    .meta$bandorder <- bandorder
    .meta$isfactor <- FALSE
    .meta$attributes <- list()
    .meta$legend <- new( ".RasterLegend")
    if (is.null(layerNames) || length(layerNames) < nlayers()) .meta$names <- paste0('layer',1:nlayers)
    else .meta$names <- layerNames[1:nlayers]

  }
  .meta
}
#------------


if (!isGeneric("createRaster")) {
  setGeneric("createRaster", function(filename,nrow,ncol,extent,nlayers,dtype,bandorder,crs,overwrite,echo)
    standardGeneric("createRaster"))
}


setMethod('createRaster', signature(filename='character'),
          function(filename,nrow,ncol,extent,nlayers=1,dtype='FLT4S',bandorder='BSQ',crs=NA,overwrite=FALSE,echo=TRUE) {
            if (missing(nrow) | missing(ncol)) stop('nrow and ncol should be specified')
            if (missing(extent)) {
              extent <- c(0,ncol,0,nrow)
              warning('extent is not defined so an arbitrary extent is used!')
            }
            if (missing(nlayers)) nlayers <- 1
            if (missing(dtype)) dtype <- 'FLT4S'
            if (missing(bandorder)) bandorder <- 'BSQ'
            if (missing(crs)) crs <- NA
            if (missing(overwrite)) overwrite <- FALSE
            if (missing(echo)) echo <- TRUE

            .createEmptyRaster(filename=filename,nrow=nrow,ncol=ncol,nlayers=nlayers,extent=extent,dtype=dtype,bandorder=bandorder,crs=crs,overwrite=overwrite,echo=echo)
          }
)
###################################

if (!isGeneric("writeInRaster")) {
  setGeneric("writeInRaster", function(x,filename,band,cells,start,end)
    standardGeneric("writeInRaster"))
}


setMethod('writeInRaster', signature(x='numeric'),
          function(x,filename,band,cells,start,end) {

            if (missing(band)) band <- NULL
            if (missing(cells)) cells <- NULL

            .writeInRasterV(x=x,filename=filename,band=band,cells=cells)
          }
)

setMethod('writeInRaster', signature(x='matrixORdata.frame'),
          function(x,filename,band,cells) {

            if (missing(band)) band <- NULL
            if (missing(cells)) cells <- NULL

            .writeInRasterM(x=x,filename=filename,band=band,cells=cells)
          }
)
