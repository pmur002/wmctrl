
showDesktop <- function() {
    system("wmctrl -k on")
}

## https://specifications.freedesktop.org/wm-spec/wm-spec-1.3.html#idm140130317598336
modifyWindowState <- function(windowID, action, state) {
    if (!state %in% c("modal", "sticky", "maximized_vert", "maximized_horz",
                      "shaded", "skip_taskbar", "skip_pager", "hidden",
                      "fullscreen", "above", "below"))
        stop("Invalid window state")
    system(paste0("wmctrl -ir ", windowID, " -b ", action, ",", state))
}
removeWindowState <- function(windowID, state) {
    modifyWindowState(windowID, "remove", state)
}
addWindowState <- function(windowID, state) {
    modifyWindowState(windowID, "add", state)
}
toggleWindowState <- function(windowID, state) {
    modifyWindowState(windowID, "toggle", state)
}

## Change x/y/w/h
positionWindow <- function(windowID, x, y, w, h, gravity=0) {
    system(paste0("wmctrl -ir ", windowID, " -e ",
                  paste(gravity, x, y, w, h, sep=",")))
}
## Retain current w/h
locateWindow <- function(windowID, x, y, gravity=0) {
    windows <- windowList(geometry=TRUE)
    wh <- windows[windows$windowID == windowID, c("w", "h")]
    positionWindow(windowID, x, y, wh[1], wh[2], gravity)
}
## Retain current x/y
sizeWindow <- function(windowID, w, h, gravity=0) {
    windows <- windowList(geometry=TRUE)
    xy <- windows[windows$windowID == windowID, c("x", "y")]
    positionWindow(windowID, xy[1], xy[2], w, h, gravity)
}

focusWindow <- function(windowID, here=TRUE) {
    if (here) {
        arg <- "R"
    } else {
        arg <- "a"
    }
    system(paste0("wmctrl -i", arg, " ", windowID))
}

closeWindow <- function(windowID) {
    system(paste0("wmctrl -ic ", windowID))
}

windowList <- function(pid=FALSE, geometry=FALSE, class=FALSE) {

    getWindows <- function(cmd) {
        wl <- system(cmd, intern=TRUE)
        if (!is.null(attr(wl, "status"))) {
            # Try again
            while (!is.null(attr(wl, "status"))) {
                wl <- system(cmd, intern=TRUE)
            }
        }
        wl
    }

    args <- "-l"
    ncol <- 4 # windowID, desktop, machine, windowName
    colnames <- c("windowID", "desktop")
    if (pid) {
        args <- paste0(args, "p")
        ncol <- ncol + 1
        colnames <- c(colnames, "pid")
    }
    if (geometry) {
        args <- paste0(args, "G")
        ncol <- ncol + 4
        colnames <- c(colnames, "x", "y", "w", "h")
    }
    if (class) {
        args <- paste0(args, "x")
        ncol <- ncol + 1
        colnames <- c(colnames, "windowClass")
    }
    colnames <- c(colnames, c("machine", "windowName"))
    listing <- getWindows(paste0("wmctrl ", args))
    if (length(listing)) {
        values <- strsplit(listing, " +")
        csv <- sapply(values,
                      function(x) {
                          paste0(paste(x[1:(ncol - 1)], collapse=","),
                                 ',"',
                                 paste(x[ncol:length(x)], collapse=" "),
                                 '"')
                      })
        textcon <- textConnection(csv)
        df <- read.csv(textcon, col.names=colnames, header=FALSE)
        close(textcon)
    } else {
        ## Zero windows
        df <- as.data.frame(matrix(nrow=0, ncol=length(colnames)))
        names(df) <- colnames
    }
    df
}

getWindowID <- function(name, pid=NULL, class=NULL, ...) {
    wl <- windowList(pid=!is.null(pid), class=!is.null(class))
    index <- grepl(name, wl$windowName, ...)
    if (!is.null(pid)) {
        index <- index & wl$pid == pid
    }
    if (!is.null(class)) {
        index <- index & grepl(class, wl$windowClass)
    }
    if (sum(index) > 0) {
        wl$windowID[index]
    } else {
        NULL
    }
}

