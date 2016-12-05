
## Run a program that opens a window and return the windowID of that window
## If timeout elapses without success, fail
openWindow <- function(program, args="", pid=FALSE, timeout=10) {
    w1 <- windowList(pid=pid)

    ## Run the program 
    system(paste(program, args))

    if (pid) {
        ## Get process id of program
        windowPid <- system(paste("pidof", program), intern=TRUE)
        ## May get multiple pids if run immediately after execution, but the
        ## last one is the common pid for things like gnome-terminal and firefox
        windowPid <- gsub(".+ ", "", windowPid)
    }
    
    ## Loop (waiting for program window to open)
    time <- proc.time()
    found <- FALSE
    while (!found && (proc.time() - time)[3] < timeout) {
        ## Check the window list again
        w2 <- windowList(pid=pid)
        if (nrow(w2) > nrow(w1)) {
            wnew <- ! w2$windowID %in% w1$windowID
            if (pid) {
                wnew <- wnew & w2$pid == windowPid
            }
            windowID <- w2$windowID[wnew]
            if (length(windowID) > 0) {
                if (length(windowID) > 1)
                    stop("More than one window matched")
                found <- TRUE
            }
        }
    }

    if (!found) stop("Failed to detect program window")

    windowID
}
