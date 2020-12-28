#' @title Extract features from a file and return audio class with these data
#' @description Extract features from a file and return audio class with these data
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @param filename \code{character}. The path and file name. For example, "folder/1.wav"
#' @param config \code{audio_config}. An object of class 'audio_config' with parameters for extraction.
#' @return audio class
#' @export
#' 
extractFeatures <- function(filenames,  config = loudness(createConfig())) {
    purrr::map(filenames, function(x) {
        result <- extractFeature(x, config) 
        attr(result, "filename") <- x
        result
        })
}


extractFeature <- function(filename, config = config) {
    audio <- create_audio_object(filename, config)
    raw_data <- add_raw_data(filename, audio$timestamps)
    
    # For some reasons, length of audio$timestamps is not the same as raw_data (returning 
    # more data - I have to check it)
    audio$raw_data <- raw_data$cut_raw_data[1:length(audio$timestamps)]
    attr(audio, "header") <- raw_data$header
    colnames(audio) <- c(strsplit(attr(config, "columns"), ":")[[1]], "timestamps", "raw_data")
    audio
}


#' @title Label Utterances
#' @description Get a speech object and return it with labels
#' @param speech 
#' @param labels \code{character}. Vector of labels for the speech object.
#' @return speech object
#' @export
labelUtterances <- function(speeches, labels) {
    if ( length(speeches) != length(labels) )
        stop("Verify the number of labels and the number of speeches")
    
    purrr::map2(speeches, labels, function(x, y) {
        attr(x, "label") <- y
        x
    })
}


#' @title Play chunks of speech
#' @description Play chunks of a speech object
#' @param x  An object of class 'speech'
#' @param ...  other parameters
#' @return speech class
#' @export
play <- function(x, ...) {
    UseMethod("play", x)
}
play.speech <- function(x, n = NULL) {
    if ( is.null(n) ) n <- 1:nrow(x)
    rcpp_playWavFile(purrr::as_vector(x$raw_data[n]),
                attr(x, "header"))
}




#' @export
print.speech <- function(x, ...) {
    base::print(format(x[1:(ncol(x)-1)]))
#    invisible(x)
}


head.speech <- function(x, ...) {
    head(print(x))
}



tail.speech <- function(x, ...) {
    tail(print(x))
}

create_audio_object <- function(filename, config) {
    config_string <- generate_config_string(config)
    extracted_data <- rcpp_openSmileGetFeatures(filename, config_string_in = config_string)
    audio <- as.data.frame(extracted_data$audio_features_0)
    timestamps <- as.vector(extracted_data$audio_timestamps_0)
    audio$timestamps <- timestamps
    class(audio) <- c("speech", "data.frame")
    audio
    
}

add_raw_data <- function(filename, timestamps) {
    raw_data <- rcpp_parseWavFile(filename)
    # Timemust must be cut in time intervals of same size
    # We need to create a test for that
    timestamp_interval <- timestamps[2]
    
    # Split raw_data in intervals of sampleRate * timestamp_interval
    cut_raw_data <- split(raw_data[[2]], 
                          ceiling(seq_along(raw_data[[2]])/(raw_data[[1]]$sampleRate*timestamp_interval)))
    list(cut_raw_data = cut_raw_data, header = raw_data[[1]])
}


