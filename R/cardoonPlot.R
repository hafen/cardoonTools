#' cardoonPlot
#' 
#' Take a plot expression or object and turn it into a 
#' 
#' @param plotObj a lattice or ggplot object, or an expression of R base plotting commands
#' @param \ldots arguments passed to \code{\link{png}}
#' 
#' @return a base64 encoded string containing png
#' 
#' @examples
# R expressions that generate plots:
#' cardoonPlot(expression(plot(1:10)))
#' myPlotExpr <- expression({
#'    plot(1:10)
#'    abline(0, 1)
#' })
#' cardoonPlot(myPlotExpr)
#' 
#' # passing additional arguments to png
#' cardoonPlot(expression(plot(1:10)), 
#'    width = 800, height = 400, res = 150)
#' 
#' # using lattice:
#' cardoonPlot(xyplot(rnorm(10) ~ rnorm(10)))
#' # pass lattice object
#' p <- dotplot(variety ~ yield | year * site, data=barley)
#' cardoonPlot(p)
#' 
#' # using ggplot
#' cardoonPlot(qplot(mpg, wt, data = mtcars))
#' # pass ggplot object
#' p <- qplot(mpg, wt, data = mtcars)
#' cardoonPlot(p)
#' @export
cardoonPlot <- function(plotObj, ...) {
   # temporarily write to disk
   file <- tempfile(fileext = ".png")

   png(file, ...)
      res <- try(plotObject(plotObj), silent = TRUE)
   dev.off()

   # wait until after device is closed to throw error
   if(inherits(res, "try-error"))
      stop(geterrmessage())

   # read in and encode - final output can go in <img src=''>
   bytes <- file.info(file)$size
   b64 <- base64enc:::base64encode(readBin(file, "raw", n = bytes))
   file.remove(file)

   paste("data:image/png;base64,", b64, sep = "")   
}

plotObject <- function(x)
   UseMethod("plotObject", x)

plotObject.expression <- function(x)
   eval(x)

plotObject.trellis <- function(x)
   print(x)

plotObject.ggplot <- function(x)
   print(x)


