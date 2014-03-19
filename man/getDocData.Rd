\name{getDocData}
\alias{getDocData}
\title{Get Documentation Information for a Function}
\usage{
getDocData(functionName)
}
\arguments{
  \item{functionName}{name of the function}
}
\value{
a named list of documentation components
}
\description{
Get documentation information for a function, including
package, title, description, examples, and argument names
and descriptions.
}
\examples{
a <- getDocData("glm")
toJSON(a)
}

