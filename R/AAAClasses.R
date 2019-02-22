#-------------------------------------
# Author: Babak Naimi, naimi.b@gmail.com
# Date (first version): August 2017
# Date (last update):  Feb. 2019
# Version 0.3
# Licence GPL v3
#------------------------

setClassUnion("characterORmissing", c("character", "missing"))
setClassUnion("matrixORdata.frame", c("matrix", "data.frame"))

.mmapRaster <- R6Class('mmapRaster',
                      public = list(
                        initialize=function(filename,nlayers,bandorder,datatype,nrows,ncols,names,factors=NULL) {
                          if (!requireNamespace('mmap')) stop('mmap package is required but is not installed on this machine!')
                          private$filename <- filename
                          private$nlayers <- nlayers
                          private$bandorder <- bandorder
                          private$datatype <- datatype
                          private$nrows <- nrows
                          private$ncols <- ncols
                          private$ncells <- private$nrows * private$ncols
                          private$Names <- names
                          private$factors <- factors
                          private$con <- new.env(parent = emptyenv())
                          private$con$state <- FALSE
                          private$dtype <- switch(private$datatype,'FLT8S'=mmap::real64(),'FLT4S'=mmap::real32(),'INT1U'=mmap::uint8(),'LOG1S'=mmap::logi8(),'INT1S'=mmap::int8(),
                                                  'INT2S'=mmap::int16(),'INT2U'=mmap::uint16(),'INT4S'=mmap::int32(),'INT4U'=mmap::uint32())
                        },
                        getValues=function(cells) {
                          private$connect()
                          .v <- .getValues(private$obj,nl=private$nlayers,cells = cells,n=private$Names,bo=private$bandorder,.ncol = private$ncols)
                          private$disconnect()
                          return(.v)
                        },
                        writeValues=function(x,band=NULL,cells=NULL,echo=TRUE) {
                          .dx <- dim(x)

                          if (is.null(.dx)) {
                            if (is.null(band)) band <- 1
                            else if (length(band) > 1) stop('the data are one dimension while you specified more than a single band!')

                            if (band > private$nlayers) stop('the specified band does not exist!')

                            if (is.null(cells) || is.na(cells)) {
                              cells <- 1:length(x)
                            } else {
                              if (length(cells) != length(x)) stop('length of cells should be the same as the number of rows in x')
                            }
                          } else {
                            if (is.null(band)) band <- 1:.dx[2]
                            else {
                              if (length(band) != .dx[2]) stop('the dimension of the input values are not the same as the specified bands!')
                              if (any(band > private$nlayers)) stop('at least one of the specified bands does/do not exist!')
                            }
                            if (is.null(cells) || is.na(cells)) {
                              cells <- 1:dx[1]
                            } else {
                              if (length(cells) != .dx[1]) stop('length of cells should be the same as the number of rows in x')
                            }
                          }

                          private$connect()

                          if (private$bandorder == 'BSQ') {
                            if (is.null(.dx)) {
                              a <- ((band - 1) * private$ncells) + cells
                              private$obj[a] <- x
                            } else {
                              for (i in 1:.dx[2]) {
                                a <- ((band[i] - 1) * private$ncells) + cells
                                private$obj[a] <- as.vector(x[,i])
                              }
                            }
                            if (echo) print('values are successfully written to the file!')
                          } else if (private$bandorder == 'BIP') {
                            if (is.null(.dx)) {
                              private$obj[seq(private$nlayers*(cells[1]-1) + band,private$ncells*private$nlayers,by=private$nlayers)] <- x
                            } else {
                              for (i in 1:.dx[2]) {
                                private$obj[seq(private$nlayers*(cells[1]-1) + band[i],private$ncells*private$nlayers,by=private$nlayers)] <- as.vector(x[,i])
                              }
                            }
                            if (echo) print('values are successfully written to the file!')
                          } else {
                            if (is.null(.dx)) {
                              a <- rep(.cells + trunc((cells - 1) / private$ncols) * private$ncols * (private$nlayers-1) , each=1) + (band-1) * private$ncols
                              private$obj[a] <- x
                            } else {
                              for (i in 1:.dx[2]) {
                                a <- rep(.cells + trunc((cells - 1) / private$ncols) * private$ncols * (private$nlayers-1) , each=1) + (band[i]-1) * private$ncols
                                private$obj[a] <- as.vector(x[,i])
                              }
                            }
                            if (echo) print('values are successfully written to the file!')

                          }
                          private$disconnect()
                        },
                        format=function(...) {
                          c(
                            paste0('Class: ',class(self)[1]),
                            paste0('========================================='),
                            paste0('filename: ',private$filename),
                            paste0('number of cells: ',private$ncells),
                            paste0('nlayers: ',private$nlayers),
                            paste0('names: ',paste(private$Names,collapse=', '))
                          )
                        },
                        finalize = function() {
                          private$disconnect()
                        }
                      ),
                      active=list(
                        names=function(n) {
                          if (missing(n)) return(private$Names)
                          if (!is.character(n) || length(n) != length(private$Names)) stop('new names should be character with equal length as the number of layers!')
                          private$Names <- n
                        }
                      ),
                      private=list(
                        filename=NULL,
                        nlayers=NULL,
                        bandorder=NULL,
                        datatype=NULL,
                        nrows=NULL,
                        ncols=NULL,
                        ncells=NULL,
                        Names=NULL,
                        factors=NULL,
                        dtype=NULL,
                        con=NULL,
                        obj=NULL,
                        connect=function() {
                          if (!private$con$state) {
                            private$obj <- mmap::mmap(private$filename,private$dtype)
                            private$con$state <- TRUE
                            reg.finalizer(private$con,function (e) {
                              if (e$state) private$disconnect()
                            },onexit = TRUE)
                          }
                        },
                        disconnect=function(){
                          if (private$con$state) {
                            mmap::munmap(private$obj)
                            private$obj <- NULL
                            private$con$state <- FALSE
                          }
                        }
                      )
)
#---------------



.bigRaster <- R6Class('bigRaster',
                      public = list(
                        initialize=function(filename,nlayers,bandorder,datatype,nrows,ncols,cellDataType,scale) {
                          if (!requireNamespace('mmap')) stop('mmap package is required but is not installed on this machine!')
                          private$filename <- filename
                          private$nlayers <- nlayers
                          private$bandorder <- bandorder
                          private$datatype <- datatype
                          private$datatype <- cellDataType
                          private$scale <- scale
                          private$nrows <- nrows
                          private$ncols <- ncols
                          private$ncells <- private$nrows * private$ncols
                          #private$Names <- names
                          #private$factors <- factors
                          private$con <- new.env(parent = emptyenv())
                          private$con$state <- FALSE
                          private$dtype <- switch(private$datatype,'FLT8S'=mmap::real64(),'FLT4S'=mmap::real32(),'INT1U'=mmap::uint8(),'LOG1S'=mmap::logi8(),'INT1S'=mmap::int8(),
                                                  'INT2S'=mmap::int16(),'INT2U'=mmap::uint16(),'INT4S'=mmap::int32(),'INT8S'=mmap::int64())
                          private$ctype <- switch(private$cellDatatype,'FLT8S'=mmap::real64(),'FLT4S'=mmap::real32(),'INT1U'=mmap::uint8(),'LOG1S'=mmap::logi8(),'INT1S'=mmap::int8(),
                                                  'INT2S'=mmap::int16(),'INT2U'=mmap::uint16(),'INT4S'=mmap::int32(),'INT8S'=mmap::int64())
                        },
                        getValues=function(cells) {
                          private$connect()
                          .v <- .getValues(private$obj,nl=private$nlayers,cells = cells,n=private$Names,bo=private$bandorder,.ncol = private$ncols)
                          private$disconnect()
                          return(.v)
                        },
                        format=function(...) {
                          c(
                            paste0('Class: ',class(self)[1]),
                            paste0('========================================'),
                            paste0('filename: ',private$filename),
                            paste0('number of cells: ',private$ncells),
                            paste0('nlayers: ',private$nlayers),
                            paste0('names: ',paste(private$Names,collapse=', '))
                          )
                        },
                        finalize = function() {
                          private$disconnect()
                        }
                      ),
                      active=list(
                        names=function(n) {
                          if (missing(n)) return(private$Names)
                          if (!is.character(n) || length(n) != length(private$Names)) stop('new names should be character with equal length as the number of layers!')
                          private$Names <- n
                        }
                      ),
                      private=list(
                        filename=NULL,
                        nlayers=NULL,
                        bandorder=NULL,
                        datatype=NULL,
                        nrows=NULL,
                        ncols=NULL,
                        ncells=NULL,
                        Names=NULL,
                        factors=NULL,
                        dtype=NULL,
                        ctype=NULL,
                        con=NULL,
                        obj=NULL,
                        scale=NULL,
                        activeCells=NULL,
                        connect=function() {
                          if (!private$con$state) {
                            private$obj <- mmap::mmap(private$filename,private$dtype)
                            private$con$state <- TRUE
                            reg.finalizer(private$con,function (e) {
                              if (e$state) private$disconnect()
                            },onexit = TRUE)
                          }
                        },
                        disconnect=function(){
                          if (private$con$state) {
                            mmap::munmap(private$obj)
                            private$obj <- NULL
                            private$con$state <- FALSE
                          }
                        }
                      )
)
#---------------
setOldClass(c("mmapRaster", "R6"))
setOldClass(c("bigRaster", "R6"))


