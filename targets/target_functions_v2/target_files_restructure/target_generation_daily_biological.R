target_generation_daily_biological <- function(fluoro_files){

  ## FLUOROPROBE
  source('targets/target_functions/target_generation_FluoroProbe.R')

  fluoro_daily <- target_generation_FluoroProbe(current_file = fluoro_files[1], historic_file = fluoro_files[2])
  fluoro_daily$duration <- 'P1D'
  fluoro_daily$project_id <- 'vera4cast'

}
