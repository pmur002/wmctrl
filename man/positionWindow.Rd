\name{positionWindow}
\alias{positionWindow}
\alias{locateWindow}
\alias{sizeWindow}
\title{
  Position a window
}
\description{
  Change the location and size of a window.
}
\usage{
positionWindow(windowID, x, y, w, h, gravity = 0)
locateWindow(windowID, x, y, gravity = 0)
sizeWindow(windowID, w, h, gravity = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{windowID}{
    A unique window identifier (see \code{\link{getWindowID}}).
  }
  \item{x}{
    The position of the left side of the window.
  }
  \item{y}{
    The position of the top edge of the window.
  }
  \item{w}{
    The width of the window.
  }
  \item{h}{
    The height of the window.
  }
  \item{gravity}{
    The gravity of the window (which affects what happens to the content
    of the window on resize).
  }
}
\details{
  Location and size are all in pixels.  \code{locateWindow()}
  retains the current window size and \code{sizeWindow()} retains
  the current window left-top.
}
\references{
  \url{http://tripie.sweb.cz/utils/wmctrl/}
}
\author{
  Paul Murrell
}
\keyword{ utilities }
