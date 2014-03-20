\name{cardoonPlot}
\alias{cardoonPlot}
\title{cardoonPlot}
\usage{
cardoonPlot(plotObj, type = c("png", "pdf"), width = 480, height = 480,
  res = 72, ...)
}
\arguments{
  \item{plotObj}{a lattice or ggplot object, or an
  expression of R base plotting commands}

  \item{type}{the type of plot to create (currently png or
  pdf)}

  \item{width}{width of plot, in pixels}

  \item{height}{width of plot, in pixels}

  \item{res}{nominal resolution in ppi which will be
  recorded in the file, if a positive integer}

  \item{\ldots}{arguments passed to \code{\link{png}} or
  \code{\link{pdf}}}
}
\value{
a base64 encoded png string
}
\description{
Take a plot expression or object and turn it into a base64
encoded png string for embedding in a browser
}
\examples{
cardoonPlot(expression(plot(1:10)))
myPlotExpr <- expression({
   plot(1:10)
   abline(0, 1)
})
cardoonPlot(myPlotExpr)

# passing additional arguments to png
cardoonPlot(expression(plot(1:10)),
   width = 800, height = 400, res = 150)

# using lattice:
cardoonPlot(xyplot(rnorm(10) ~ rnorm(10)))
# pass lattice object
p <- dotplot(variety ~ yield | year * site, data=barley)
cardoonPlot(p)

# using ggplot
cardoonPlot(qplot(mpg, wt, data = mtcars))
# pass ggplot object
p <- qplot(mpg, wt, data = mtcars)
cardoonPlot(p)
}

