\name{cardoonPlot}
\alias{cardoonPlot}
\title{cardoonPlot}
\usage{
cardoonPlot(plotObj, ...)
}
\arguments{
  \item{plotObj}{a lattice or ggplot object, or an
  expression of R base plotting commands}

  \item{\ldots}{arguments passed to \code{\link{png}}}
}
\value{
a base64 encoded string containing png
}
\description{
Take a plot expression or object and turn it into a
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

