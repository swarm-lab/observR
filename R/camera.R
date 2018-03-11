#' @export
camsID <- function() {
  sys <- Sys.info()[1]

  if (sys == "Linux") {
    as.numeric(gsub("video", "", list.files("/dev/", "video*")))
  } else {
    stop("This function was not implemented yet for your OS.")
  }
}

#' @export
initCams <- function() {
  id <- camsID()
  cams <- list()

  for (i in seq_len(length(id))) {
    cam <- Rvision::stream(id[i])
    for (j in seq_len(30)) {
      Rvision::display(Rvision::readNext(cam))
    }
    use <- svDialogs::dlgMessage("Do you want to use this camera?", "yesno")$res
    Rvision::destroyDisplay()

    if (use == "yes") {
      save_dir <- normalizePath(svDialogs::dlgDir(title = "Select folder to save images")$res)
      cams[[length(cams) + 1]] <- list(cam = cam, save_dir = save_dir)
      setCam(cams[[length(cams)]])
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
    file_name <- paste0(cam$save_dir, "/", sprintf("%06d", length(dir(cam$save_dir))), ".png")
    suppressMessages(Rvision::write.Image(Rvision:::mean(pics), file_name))
  }, n = n)
}







