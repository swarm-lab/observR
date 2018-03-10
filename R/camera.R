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
      for (j in seq_len(10)) {
        pic <- Rvision::readNext(cam)
      }
      Rvision::display(pic)
      use <- svDialogs::dlgMessage("Do you want to use this camera?", "yesno")$res
      Rvision::destroyDisplay()

      if (use == "yes") {
        save_dir <- svDialogs::dlgDir(title = "Select folder to save images")$res
        cams[[i + 1]] <- list(cam = cam, save_dir = save_dir)
        setCam(cams[[i + 1]])
      }
      i <- i + 1
    }
  }

  cams
}

#' @export
setCam <- function(cam) {
  ini <- svDialogs::dlgOpen(title = "Select camera settings")$res
  settings <- read.csv(ini, header = FALSE, stringsAsFactors = FALSE)

  for (i in seq_len(nrow(settings))) {
    Rvision::setProp(cam$cam, settings[i, 1], settings[i, 2])
  }
}

#' @export
releaseCams <- function(cams) {
  for (i in seq_len(length(cams))) {
    Rvision::release(cams[[i]]$cam)
  }
}

#' @export
grabPictures <- function(cams, n = 30) {
  lapply(cams, function(cam, n) {
    pics <- vector(mode = "list", length = n)
    for (i in seq_len(n)) {
      pics[[i]] <- Rvision::readNext(cam$cam)
    }
    file_name <- paste0(cam$save_dir, sprintf("%06d", length(dir(cam$save_dir))), ".png")
    suppressMessages(Rvision::write.Image(Rvision:::mean(pics), file_name))
  }, n = n)
}







