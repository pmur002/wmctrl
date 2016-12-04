\name{getWindowID}
\alias{getWindowID}
\title{
  Get a window identifier
}
\description{
  Get one or more window identifiers by matching on the name of
  the window, plus optionaly the window pid, plus optionally
  the window class.
}
\usage{
getWindowID(name, pid = NULL, class = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{
    A regular expression to match the window name.
  }
  \item{pid}{
    An integer to match the window pid.
  }
  \item{class}{
    A regular expression to match the window class.
  }
  \item{\dots}{
    Arguments passed to \code{grepl} to modify the match
    (for BOTH name and class).
  }
}
\value{
  An integer (unique identifier for the window).
}
\references{
  \url{http://tripie.sweb.cz/utils/wmctrl/}
}
\author{
  Paul Murrell
}
\keyword{ utilities }
