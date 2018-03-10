#' @export
observR <- function(wemo) {
  magicLamp::wemo_ON(wemo)
  cams <- initCams()
  magicLamp::wemo_OFF(wemo)

  ui <- svDialogs::dlgInput(message = "Duration of the experiment in seconds")
  duration <- as.numeric(gsub("[^0-9]", "", ui$res))

  ui <- svDialogs::dlgInput(message = "Interval between 2 pictures in seconds")
  interval <- as.numeric(gsub("[^0-9]", "", ui$res))

  times <- now() + seq(0, duration, interval) * 1000

  pb <- progress::progress_bar$new(total = length(times), format = "Progress [:bar] :percent")

  for (i in seq_len(length(times))) {
    magicLamp::wemo_ON(wemo)
    Sys.sleep(2)

    grabPictures(cams)

    pb$tick()

    magicLamp::wemo_OFF(wemo)

    if (i < length(times)) {
      Sys.sleep((times[i + 1] - now()) / 1000)
    }
  }

  releaseCams(cams)
  NULL
}
