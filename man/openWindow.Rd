\name{openWindow}
\alias{openWindow}
\title{
  Open a window
}
\description{
  Open a program window and return the windowID.
}
\usage{
openWindow(program, args = "", pid = FALSE, timeout = 10)
}
\arguments{
  \item{program}{
    The name of a program to execute at the command line.
  }
  \item{pid}{
    A logical indicating whether the program process ID should also
    be used for identifying the correct window.
  }
  \item{args}{
    Additional arguments used in the call to execute the program.
  }
  \item{timeout}{
    A number of seconds to wait for the program window to appear.
  }
}
\references{
  \url{http://tripie.sweb.cz/utils/wmctrl/}
}
\author{
  Paul Murrell
}
\keyword{ utilities }
