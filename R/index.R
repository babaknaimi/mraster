#-------------------------------------
# Author: Babak Naimi, naimi.b@gmail.com
# Date (first version): March 2018
# Date (last update):  March 2018
# Version 0.1
# Licence GPL v3
#------------------------
setMethod("[", c(x="mmapRaster",i="numeric",j="missing"),
          function(x, i, j, drop=TRUE) {
            x$getValues(i)
          })

#
# setMethod("[[", c(x="mmapRaster"),
#           function(x, i, j, drop=TRUE) {
#             x$getValues(i)
#           })


# `[.mmapRaster` = function(x, i, ...) {
#   x$getValues(i)
# }
