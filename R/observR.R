#' @export
observR <- function(wemo) {
  initCam(cams)

  ui <- svDialogs::dlgInput(message = "Duration of the experiment in seconds")
  duration <- as.numeric(gsub("[^0-9]", "", ui$res))

  ui <- svDialogs::dlgInput(message = "Interval between 2 pictures in seconds")
  interval <- as.numeric(gsub("[^0-9]", "", ui$res))

  times <- now() + seq(0, duration, interval) * 1000

  pb <- progress::progress_bar$new(total = 100, format = "Progress [:bar] :percent")

  for (i in 1:length(times)) {
    lightON(wemo)
    Sys.sleep(2)

    grabPicture(cams)

    print(paste0("Last picture taken at: ", Sys.time()))

    lightOFF(wemo)

    if (i < length(times)) {
      Sys.sleep((times[i + 1] - now()) / 1000)
    }

    pb$tick()
  }

  NULL
}
