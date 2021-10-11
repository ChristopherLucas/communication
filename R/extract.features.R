#' @title Extract features from a file and return audio class with these data
#' @description Extract features from a file and return audio class with these data
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @param files \code{character}. Vector of path and file name. For example, c("folder/1.wav", "folder/2.wav")
#' @param config \code{audio_config}. An object of class 'audio_config' with parameters for extraction.
#' @param use.full.names \code{boolean}. Whether to use full file path as names of returned list elements
#' @param use.exts \code{boolean}. Whether to use file extensions in names of returned list elements.
#'   Only applicable if use.full.names is FALSE
#' @return audio class
#' @export
#' 
extractFeatures <- function(files,  config = loudness(createConfig()), use.full.names=T, use.exts=T) {
    features <- purrr::map(files, function(x) {
        result <- extractFeature(x, config)
        attr(result, "filename") <- x
        result
        })

    list_names <- files
    if (!use.full.names) list_names <- basename(list_names)
    if (!use.exts) list_names <- tools::file_path_sans_ext(list_names)
    
    purrr::set_names(features, list_names)
}


extractFeature <- function(filename, config = config) {
    audio_nfeatures <- create_audio_object(filename, config)
    audio <- audio_nfeatures$audio
    n_features <- audio_nfeatures$n_features
    raw_data <- add_raw_data(filename, audio$timestamps)
    
    # For some reasons, length of audio$timestamps is not the same as raw_data (returning 
    # more data - I have to check it)
    audio$raw_data <- raw_data$cut_raw_data[1:length(audio$timestamps)]
    attr(audio, "header") <- raw_data$header
    
    # If the no. of columns supplied by the config object is not the same as the no. of features
    # we got, then most likely the config does not know how many features are created
    # In this case, we name the feature columns based on the output name of the components
    # But we can do so reliably only if there is a single component that connects to the sink.
    column_names <- strsplit(attr(config, "columns"), ":")[[1]]
    if (length(column_names) < n_features) {
        edges <- attr(config, 'edges')
        outputs <- subset(edges, 'explicit_output')$output
        if (length(outputs) > 1) {
            warning('Unable to determine column names for features. Please correctly specify the no. of features of each component.')
        } else {
            column_names <- paste0(outputs[1], '_', seq(n_features))
        }
    }
    colnames(audio) <- c(column_names, "timestamps", "raw_data")
    audio
}


#' @title Label Utterances
#' @description Get a speech object and return it with labels
#' @param features Features object
#' @param labels \code{character}. Vector of labels for the speech object.
#' @return speech object
#' @export
labelUtterances <- function(features, labels) {
    if ( length(features) != length(labels) )
        stop("Verify the number of labels and different speech units")
    
    purrr::map2(features, labels, function(x, y) {
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


play.speech <- function(x, start = NULL, end = NULL) {
    n <- 1:nrow(x)
    if ( is.null(start) || is.null(end)) {
        rcpp_playWavFile(purrr::as_vector(x$raw_data[n]),
                         attr(x, "header"))
    }
    else {
        rcpp_playWavFileSubset(purrr::as_vector(x$raw_data[n]),
                            attr(x, "header"), start, end)
    }
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
    n_features <- ncol(audio)
    timestamps <- as.vector(extracted_data$audio_timestamps_0)
    audio$timestamps <- timestamps
    class(audio) <- c("speech", "data.frame")
    
    list(audio=audio, n_features=n_features)
}

add_raw_data <- function(filename, timestamps) {
    raw_data <- rcpp_parseAudioFile(filename)
    # Timemust must be cut in time intervals of same size
    # We need to create a test for that
    timestamp_interval <- timestamps[2]
    
    # Split raw_data in intervals of sampleRate * timestamp_interval
    cut_raw_data <- split(raw_data[[2]], 
                          ceiling(seq_along(raw_data[[2]])/(raw_data[[1]]$sampleRate*timestamp_interval)))
    list(cut_raw_data = cut_raw_data, header = raw_data[[1]])
}


