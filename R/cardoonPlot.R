#' cardoonPlot
#' 
#' Take a plot expression or object and turn it into a base64 encoded png string for embedding in a browser
#' 
#' @param plotObj a lattice or ggplot object, or an expression of R base plotting commands
#' @param type the type of plot to create (currently png or pdf)
#' @param width width of plot, in pixels
#' @param height width of plot, in pixels
#' @param res nominal resolution in ppi which will be recorded in the file, if a positive integer
#' @param \ldots arguments passed to \code{\link{png}} or \code{\link{pdf}}
#' 
#' @return a base64 encoded png string
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
cardoonPlot <- function(plotObj, type = c("png", "pdf"), width = 480, height = 480, res = 72, ...) {
   # temporarily write to disk
   if(!all(type %in% c("png", "pdf")))
      stop("Supported types are 'pdf' and 'png'")
   
   # store base64 encoded files here
   result <- list()
   
   for(plotType in type) {
      file <- tempfile(fileext = sprintf(".%s", plotType))
      
      if(plotType == "png") {
         png(file, width = width, height = height, res = res, ...)
      } else if(plotType == "pdf") {
         # pdf dimensions are in inches
         # dividing by res gives a commensurate result to png
         pdf(file, width = width / res, height = height / res, ...)
      }
      
      plotResult <- try(plotObject(plotObj), silent = TRUE)
      
      dev.off()
      
      # wait until after device is closed to throw error
      if(inherits(plotResult, "try-error"))
         stop(geterrmessage())
      
      # read in and encode - final output can go in <img src=''>
      bytes <- file.info(file)$size
      result[[plotType]] <- base64enc:::base64encode(readBin(file, "raw", n = bytes))
      file.remove(file)
   }
   # convert to json?
   result
}

plotObject <- function(x)
   UseMethod("plotObject", x)

plotObject.expression <- function(x)
   eval(x)

plotObject.trellis <- function(x)
   print(x)

plotObject.ggplot <- function(x)
   print(x)


