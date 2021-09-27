#' @importFrom Rcpp evalCpp
#' @importFrom utils head
#' @importFrom utils tail
#' @useDynLib communication, .registration=TRUE
#' @title create audio_config
#' 
#' @description return config structure for extractFeatures
#' 
#' @param nThreads \code{integer}. defines the number of threads#' 
#' @param frameSize \code{double}. defines the frame size
#' @param frameStep \code{double}. defines the frame step
#' @return audio_config class
#' @export
createConfig <- function(nThreads = 1, frameSize = 0.025, frameStep = 0.0125 ) {
  config <- list(
    'componentInstances:cComponentManager' = list(
        'instance[dataMemory].type'='cDataMemory',
        'nThreads' = nThreads
      ),
    'componentInstances:cComponentManager' = list(
      'instance[waveIn].type'='cWaveSource',
      'instance[audspec_frame].type'='cFramer',
      'instance[lldrcppdatasink].type'='cRcppDataSink'
    ),
    'waveIn:cWaveSource' = list(
      'writer.dmLevel'='wave',
      'buffersize_sec'='5.0',
      'filename'='\\cm[inputfile(I){test.wav}:name of input file]',
      'start'='\\cm[start{0}:audio start position in seconds]',
      'end'='\\cm[end{-1}:audio end position in seconds, -1 for end of file]',
      'monoMixdown'='1',
      'outFieldName'='pcm'),
    'audspec_frame:cFramer' = list(
      'reader.dmLevel'='wave',
      'writer.dmLevel'='frames',
      'frameSize'=frameSize,
      'frameStep'=frameStep,
      'frameMode'='fixed',
      'frameCenterSpecial'='left'),

  # output config
    'lldrcppdatasink:cRcppDataSink' = list(
      'timestamp' = '\\cm[timestampcsv{1}:set to 0 to suppress timestamp column, default is 1, i.e. to show timestamp in second column]',
      'number' = 0,
      'frameIndex' = 0,
      'frameTime' = '\\cm[timestamparff{1}:set to 0 to suppress timestamp column, default is 1, i.e. to show timestamp in second column]',
      'errorOnNoOutput' = 1)
  )
  class(config) <- "audio_config"
  attr(config, "config_number_features") <- 0
  attr(config, "edges") <- NULL
  config
}



#' @title Method for obtaining the number of features to be extracted
#' @description Method for obtaining the number of features to be extracted
#' @param x audio object
#' @param ... other parameters
#' @return number of features
getNumberFeatures <- function(x, ...) {
  UseMethod("get_number_features", x)
}
#' @export
getNumberFeatures.audio_config <- function(x) {
  edges <- getEdges(x)
  sum(x$nFeatures[x$explicit_output])
}



#' @title Method for obtaining the edges of an audio_config object
#' @description Method for obtaining the edges of an audio_config object
#' @param x audio object
#' @param ... other parameters
#' @return number of features
#' @export
getEdges <- function(x, ...) {
  UseMethod("getEdges", x)
}
getEdges.audio_config <- function(x) {
  return(attr(x, "edges"))
}






#' @title Transform audio.config to string
#' @description Transform audio.config to string
#' to be utilized in extract.features function
#' @param audio_config An audio_config object
#' @importFrom magrittr "%>%"
#' @return config string for openSmile
generate_config_string <- function(audio_config) {
    purrr::imap(audio_config, ~
                  paste0('[',.y,']\n',
                      .x %>%
                          purrr::imap( ~ paste0(.y, "=", .x[[1]], '\n')) %>%
                          purrr::reduce(paste0)
                  )
    ) %>%
    purrr::reduce(paste0)
}






