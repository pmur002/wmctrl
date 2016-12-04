\name{focusWindow}
\alias{focusWindow}
\title{
  Give a window focus
}
\description{
  Give a window focus, either by bringing it to the current desktop,
  or switching to its desktop.
}
\usage{
focusWindow(windowID, here = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{windowID}{
    A unique window identifier (see \code{\link{getWindowID}}).
  }
  \item{here}{
    A boolean value.  If \code{TRUE}, bring the window to the
    current desktop.
  }
}
\references{
  \url{http://tripie.sweb.cz/utils/wmctrl/}
}
\author{
  Paul Murrell
}
\keyword{ utilities }
