\name{windowList}
\alias{windowList}
\title{
  List all windows
}
\description{
  Create a data frame  
}
\usage{
windowList(pid = FALSE, geometry = FALSE, class = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pid}{
    Logical value indicating whether to include window process ids
    in the result.
  }
  \item{geometry}{
    Logical value indicating whether to include window geometry
    in the result.
  }
  \item{class}{
    Logical value indicating whether to include window class names
    in the result.
  }
}
\value{
  A data frame.  This will always have columns windowID, desktop,
  machine, and windowName.  If \code{pid} is \code{TRUE}, it will
  also contain a pid column.  If \code{geometry} is \code{TRUE},
  it will also contain columns x, y, w, and h.  If \code{class}
  is \code{TRUE}, it will also contain a windowClass column.
}
\references{
  \url{http://tripie.sweb.cz/utils/wmctrl/}
}
\author{
  Paul Murrell
}
\keyword{ utilities }
