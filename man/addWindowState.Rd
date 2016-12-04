\name{addWindowState}
\alias{addWindowState}
\alias{removeWindowState}
\alias{toggleWindowState}
\title{
  Modify window properties.
}
\description{
  Add, remove, or toggle a window property.
}
\usage{
addWindowState(windowID, state)
removeWindowState(windowID, state)
toggleWindowState(windowID, state)
}
\arguments{
  \item{windowID}{
    A unique window identifier (see \code{\link{getWindowID}}).
  }
  \item{state}{
    The name of the property to modify. One of:
    modal, sticky, maximized_vert, maximized_horz,
    shaded, skip_taskbar, skip_pager, hidden, fullscreen, above, and
    below.
  }
}
\references{
  \url{http://tripie.sweb.cz/utils/wmctrl/}
}
\author{
  Paul Murrell
}
\keyword{ utilities }
