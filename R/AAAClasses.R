#-------------------------------------
# Author: Babak Naimi, naimi.b@gmail.com
# Date (first version): August 2017
# Date (last update):  August 2017
# Version 0.2
# Licence GPL v3
#------------------------

setClassUnion("characterORmissing", c("character", "missing"))

mmapRaster <- R6Class('mmapRaster',
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
                          .getValues(private$obj,nl=private$nlayers,cells = cells,n=private$Names,bo=private$bandorder,.ncol = private$ncols)
                        },
                        format=function(...) {
                          c(
                            paste0('Class: ',class(self)[1]),
                            paste0('======================'),
                            paste0('filename: ',private$filename),
                            paste0('ncell: ',private$ncells),
                            paste0('nlayers: ',private$nlayers),
                            paste0('names: ',paste(private$Names,collapse=', '))
                          )
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
