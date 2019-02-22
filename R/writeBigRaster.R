#-------------------------------------
# Author: Babak Naimi, naimi.b@gmail.com
# Date (first version): August 2017
# Date (last update):  Feb. 2019
# Version 0.2
# Licence GPL v3
#------------------------


# if filename already exist, x will be appended, otherwise a new file will be created:
.writeBin <-  function(x,filename,dtype) {
  zzz <-file(filename,'ab')
  writeBin(x,zzz,size = sizeof(dtype))
  close(zzz)
}



.breakBigVector <- function(v,.len) {
  .w <- floor(length(v) / .len)
  o <- list()
  for (i in 1:.w) {
    o[[i]] <- v[(i-1)*.len + c(1:.len)]
  }
  if (length(v) > (i*.len)) {
    o[[i+1]] <- v[i*.len+1:length(v)]
  }
  o
}

.getValidLength <- function(dtype) {
  (2^31-100) / sizeof(dtype)
}


# we assume that the input filenames are either multiple files (all single layer raster, same format) or single file (can be multiple-bands)

##### Important: if scale = NULL, means the raster is float but it is not needed to rescale
######--------- if scale = 1, means the raster is integer and therefore is not needed to rescale
######--------- if scale is a numeric (0.0001,0.001) means the raster is float and should be rescaled!

.writeBigRaster.hasNA <- function(filename,newfile,overwrite=FALSE,scale=0.001,checkNA=FALSE) {
  if (!is.null(scale) && scale != 1) {
    if (scale < 1) scale <- 1 / scale
    if ((scale %% 1000) != 0) stop('scale is appropriate (should be NULL for integer rasters; 1 when scaling is not required, or a scale like: 0.001, 0.0001)')
  }

  newfile <- .trim(newfile)
  if (identical(basename(newfile), newfile)) newfile <- .normalizePath(newfile)

  if (.fileBase(newfile,overwrite=overwrite)) {
    .filemeta <- list()
    if (is.null(scale)) .filemeta$scale <- "NULL"
    else .filemeta$scale <- scale

    if (length(filename > 1)) {
      if (.isGDAL(filename[1])) {
        .meta <- .getGDALmeta(filename[1])
        #---------
        .v <- .getGDALband(filename[1])
        .c <- which(!is.na(.v))


        if (checkNA && length(filename) > 3) {
          # randomly check in 3 layers to see whether the NA cells in the first layer are the same in the randmoly selected layers
          .w <- c()
          for (.s in sample(filename,3)) {
            .w <- c(.w,all(is.na(.getGDALband(.s)[-.c])))
          }
          if (!all(.w)) stop('It seems that the NA cells in the first layer are not representative of all NAs in the entire dataset!')
        }

        .filemeta$activeCells <- length(.c)
        .filemeta$nlayers <- length(filename)
        #----------------
        .range <- range(.v[.c],na.rm=TRUE)

        if (!is.null(scale) && scale != 1) {
          if (as.integer(.range[2]*scale)  < 25000) .dtype <- int16()
          else if (as.integer(.range[1]*scale)  > 0 && as.integer(.range[2]*scale) < 50000) {
            .w <- c()
            for (.s in sample(filename,3)) c(.w,min(.getGDALband(.s)[.c],na.rm = TRUE))
            if (all(.w > 0)) .dtype <- uint16()
          } else if (as.integer(.range[2]*scale) < 2000000000) .dtype <- int32()
          else .dtype <- int64()
        } else if (is.null(scale)) {
          # scale=NULL means the rasters are already integer!
          # let's just check the validity:
          if (!all(c(.v[.c] - as.integer(.v[.c])) == 0)) stop('scale is NULL, means the raster data is expected to be integer, but it seems they are not!\n Use scale=1 if teh rasters are float but you are not willing to apply scale factor!')
          if (as.integer(.range[2]) < 25000) .dtype <- int16()
          else if (as.integer(.range[1]) > 0  && as.integer(.range[2]) < 50000) {
            .w <- c()
            for (.s in sample(filename,3)) c(.w,min(.getGDALband(.s)[.c],na.rm = TRUE))
            if (all(.w > 0)) .dtype <- uint16()
          } else if (as.integer(.range[2]) < 2000000000) .dtype <- int32()
          else .dtype <- int64()
        } else {
          .dtype <- real32()
        }
        #---------
        gc()


        if (length(.v) > 2147483647) {
          .ctype <- int64()
          .filemeta$cellDataType <- "INT8S"
        } else {
          .ctype <- int32()
          .filemeta$cellDataType <- "INT4S"
        }
        #-------
        .validLength <- .getValidLength(.ctype)

        if (length(.c) > .validLength) {
          .cb <- .breakBigVector(.c,.len=.validLength)
          for (i in length(.cb)) .writeBin(.cb[[i]],paste0(newfile,'/_activeCells.int'),.ctype)
          rm(.cb); gc()
        } else .writeBin(.c,paste0(newfile,'/_activeCells.int'),.ctype)

        #-----
        .v <- rep(as.integer(NA),length(.v))
        .v[.c] <- as.integer(1:length(.c))

        if (length(.v) > .validLength) {
          .v <- .breakBigVector(.v,.len=.validLength)
          for (i in length(.v)) .writeBin(.v[[i]],paste0(newfile,'/_newCells.int'),.ctype)
        } else .writeBin(.v,paste0(newfile,'/_newCells.int'),.ctype)

        rm(.v); gc()

        #-----------
        .validLength <- .getValidLength(.dtype)
        if (length(.c) > .validLength) {
          .c <- .breakBigVector(.c, .len=.validLength)
          if (is.null(scale)) {
            for (.f in filename) {
              .v <- as.integer(.getGDALband(.f))
              for (i in 1:length(.c)) .writeBin(.v[.c[[i]]],paste0(newfile,'/_rasterValues.int'),sizeof(.dtype))
            }
          } else if (scale == 1) {
            for (.f in filename) {
              .v <- .getGDALband(.f)
              for (i in 1:length(.c)) .writeBin(.v[.c[[i]]],paste0(newfile,'/_rasterValues.int'),sizeof(.dtype))
            }
          } else {
            for (.f in filename) {
              .v <- as.integer(.getGDALband(.f)*scale)
              for (i in 1:length(.c)) .writeBin(.v[.c[[i]]],paste0(newfile,'/_rasterValues.int'),sizeof(.dtype))
            }
          }
        } else {
          if (is.null(scale)) {
            for (.f in filename) {
              .v <- as.integer(.getGDALband(.f))[.c]
              .writeBin(.v,paste0(newfile,'/_rasterValues.int'),sizeof(.dtype))
            }
          } else if (scale == 1) {
            for (.f in filename) {
              .v <- .getGDALband(.f)[.c]
              .writeBin(.v,paste0(newfile,'/_rasterValues.int'),sizeof(.dtype))
            }
          } else {
            for (.f in filename) {
              .v <- as.integer(.getGDALband(.f)[.c]*scale)
              .writeBin(.v,paste0(newfile,'/_rasterValues.int'),sizeof(.dtype))
            }
          }
        }
      } else {


      }
      #---------------
      .filemeta$datatype <- switch(class(dtype)[2],"ushort"='INT2U','float'='FLT4S',"short"='INT2S','int'='INT4S','int64'='INT8S')
      # write metadata
      zz <- file(paste(newfile,'/_bigRaster','.info',sep=""),'w')
      cat("[General]\nCreator= R package 'mraster'\ncreated=",format(Sys.time(), "%Y-%m-%d %H:%M:%S"),'\n',file=zz,sep='')
      cat("\n[Raster info]\n",file=zz,sep='')
      cat("nrows =",.meta$nrows,'\n',file=zz,sep='')
      cat("ncols =",.meta$ncols,'\n',file=zz,sep='')
      cat("nActiveCells =",length(.c),'\n',file=zz,sep='')
      cat("nlayers =",.filemeta$nlayers,'\n',file=zz,sep='')
      cat("cell_dataType =",.filemeta$cellDataType,'\n',file=zz,sep='')
      cat("datatype =",.filemeta$datatype,'\n',file=zz,sep='')
      cat("scale =",.filemeta$scale,'\n',file=zz,sep='')
      close(zz)
      #-------------



    } else {

    }




  } else stop('file is not created!')


}





.getBigRasterInfo <- function(filename) {
  # get info from raster.grd
  o <- list()
  sx <- readLines(paste0(filename,'/_bigRaster.info'))
  #sx <- scan(filename,'character',quiet = TRUE)
  ux <- unlist(lapply(sx,function(x) .trim(strsplit(x,'=')[[1]][1])))
  ux <- unlist(lapply(ux,function(x) .trim(x)))
  ux <- unlist(lapply(ux,function(x) strsplit(x,'=')[[1]][1]))


  w <- which(ux == "datatype")
  o[['datatype']] <- strsplit(sx[w],'=')[[1]][2]
  w <- which(ux == "cell_dataType")
  o[['cellDataType']] <- strsplit(sx[w],'=')[[1]][2]
  w <- which(ux == "nlayers")
  o[['nlayers']] <- as.numeric(strsplit(sx[w],'=')[[1]][2])
  w <- which(ux == "nActiveCells")
  o[['nActiveCells']] <- as.numeric(strsplit(sx[w],'=')[[1]][2])

  o[['nrows']] <- as.numeric(strsplit(sx[which(ux == "nrows")],'=')[[1]][2])

  o[['ncols']] <- as.numeric(strsplit(sx[which(ux == "ncols")],'=')[[1]][2])


  o
}
#-----


.readBigRasterR6 <- function(filename) {
  filename <- .normalizePath(filename)
  if (!file.exists(filename)) stop('filename does not exist!')
  if (file.info(filename)$isdir) {
    if (!file.exists(paste0(filename,'/_bigRaster.info'))) stop('the specified file object is not big raster object!')

    .meta <- .getBigRasterInfo(filename)
  } else stop('the specified file object is not big raster object!')


  .m <- .bigRaster$new(filename=filename,nlayers=.meta$nlayers,bandorder='BSQ',
                        datatype=.meta$datatype,nrows=.meta$nrows,ncols=.meta$ncols,scale=.meta$scale,cellDataType=.meta$cellDataType)
  .m
}
#-------------


