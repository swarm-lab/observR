#' @export
initCams <- function() {
  cams <- list()
  searching <- TRUE
  i <- 0
  while (searching) {
    cam <- tryCatch(Rvision::stream(i), error = function(e) NULL)

    if (is.null(cam)) {
      searching <- FALSE
    } else {
      cams[[i + 1]] <- cam
      i <- i + 1
    }
  }

  cams
}

#' @export
releaseCams <- function(cams) {
  lapply(cams, Rvision::release)
}

#' @export
grabPictures <- function(cams, n = 30) {
  lapply(cams, function(cam, n) {
    pics <- vector(mode = "list", length = n)
    for (i in seq_len(n)) {
      pics[[i]] <- Rvision::readNext(cam)
    }
    Rvision:::mean(pics)
  }, n = n)
}







