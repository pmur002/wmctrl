\name{wmctrl-package}
\alias{wmctrl-package}
\alias{wmctrl}
\docType{package}
\title{
\packageTitle{wmctrl}
}
\description{
\packageDescription{wmctrl}
}
\details{
  An R interface to the \code{wmctrl} command (on Linux).
}
\author{
\packageAuthor{wmctrl}

Maintainer: \packageMaintainer{wmctrl}
}
\references{
  \url{http://tripie.sweb.cz/utils/wmctrl/}
}
\examples{
\dontrun{
wid <- openWindow("gnome-terminal")
positionWindow(wid, 100, 200, 300, 400)
focusWindow(wid)
closeWindow(wid)
}
}
\keyword{ package }
