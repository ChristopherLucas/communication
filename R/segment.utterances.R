segment.utterances.pipeline <- function() {
  pipeline(
    list(
      'cVectorPreemphasis',
      copyInputName = 0,
      processArrayFields = 1,
      k = 0.970000,
      de = 0
    ),
    
    list(
      'cWindower',
      copyInputName = 1,
      processArrayFields = 1,
      offset = 0,
      winFunc = 'ham'
    ),
    
    list(
      'cTransformFFT'
    ),
    
    list(
      'cFFTmagphase'
    ),
    
    list(
      'cMelspec',
      htkcompatible = 0,
      nBands = 26,
      usePower = 1,
      lofreq = 0,
      hifreq = 8000,
      specScale = 'mel'
    ),
    
    list(
      'cPlp',
      .output = 'plp_VAD',
      buffersize = 100,
      firstCC = 1,
      lpOrder = 18,
      cepLifter = 22,
      compression = 0.33,
      htkcompatible = 0,
      newRASTA = 1,
      RASTA = 0,
      rastaUpperCutoff = 29.0,
      rastaLowerCutoff = 0.9,
      doIDFT = 1,
      doLpToCeps = 1,
      doLP = 1,
      doInvLog = 1,
      doAud = 1,
      doLog = 1
    ),
    
    list(
      'cDeltaRegression',
      .output = 'plpde_VAD',
      deltawin = 2,
      blocksize = 1
    ),
    
    list(
      'cVectorMVN',
      .input = 'plp_VAD;plpde_VAD',
      copyInputName = 1,
      processArrayFields = 0,
      mode = 'transform',
      initFile = system.file('extdata', 'vad', 'rplp18d_norm.dat', package='speech'),
      htkcompatible = 0,
      meanEnable = 1,
      stdEnable = 1,
      normEnable = 0
    ),
    
    list(
      'cRnnProcessor',
      netfile = system.file('extdata', 'vad', 'lstmvad_rplp18d_12.net', package='speech')
    ),
    
    list(
      'cDataSelector',
      nameAppend = 'vadBin',
      copyInputName = 1,
      selectedRange = 0,
      elementMode = 1
    ),
    
    list(
      'cTurnDetector',
      .output = 'turn',
      readVad = 1,
      threshold = 0,
      threshold2 = 0,
      writer.levelconf.noHang = 1,
      msgInterval = 0,
      messageRecp = 'waveSinkCut',
      eventRecp = 'waveSinkCut',
      statusRecp = 'waveSinkCut',
      debug = 4,
      maxTurnLength = 0,
      maxTurnLengthGrace = 1,
      nPre = 0,
      nPost = 0
    )
  )
}

#' Segment utterances by using voice activity detection
#'
#' @param folder Folder name of wav files to process
#' @param files Either a single wav file or a vector of wav files to process.
#'   If specified, takes precedence over the folder argument
#' @param config A configuration object created through createConfig() or pipeline()
#' @param tsv.folder If specified, segment information of each input file is saved
#'   to this folder in tsv format, with the same base name as the input file, but
#'   with the .txt extension
#' @param use.full.names If TRUE, the complete path of each input file is used as
#'   the name of the returned list components. If FALSE, only the base names are
#'   used.
#' @param use.exts If TRUE, the extensions of the input files are used in the names
#'   of the returned list components. If FALSE, the extensions are not used.    
#' @return A list of segment information, with names as the names of the input files
#'   encountered (but see use.full.names and use.exts arguments), and the values as
#'   a list of 2 vectors with name start and end, with the start and end times of
#'   each detected segment in seconds.
#' @export
segment.utterances <- function(folder=NULL, files=NULL, config=NULL, tsv.folder=NULL, use.full.names=F, use.exts=F) {
  if (is.null(files)) {
    stopifnot(!is.null(folder))
    files <- tools::list_files_with_exts(folder, exts='wav')
  }
  if (is.null(config)) config <- segment.utterances.pipeline()
  features <- extractFeatures(files, config, use.full.names=use.full.names, use.exts=use.exts)
  
  all_segments = list()
  for (filename in names(features)) {
    data <- features[[filename]]
    segments <- find_segments(data[, 'turn'], data[, 'timestamps'])
    all_segments[[filename]] <- segments
    
    if (!is.null(tsv.folder)) {
      # Add a column with the static string value 'segment' and save to tsv so we can open it up in audacity
      tbl <- cbind('start' = segments$start, 'end' = segments$end, 'segment' = paste0('segment_', 1:length(segments$start)))
      tsv_path = file.path(tsv.folder, paste0(tools::file_path_sans_ext(basename(filename)), ".txt"))
      utils::write.table(tbl, tsv_path, sep="\t", quote=F, col.names=F, row.names=F)
    }
  }
  
  all_segments
}

find_segments <- function(values, timestamps, threshold=0, min.segment.length=0) {
  stopifnot(length(values) == length(timestamps))
  segments <- list(start=NULL, end=NULL)
  last_start <- NULL
  in_segment <- F
  
  j <- 1
  last_timestamp <- -1
  while (j <= length(values)) {
    this_timestamp <- timestamps[j]
    # The cTurnDetector component seems to sometimes return ~0 timestamps, especially towards
    # the end of the stream - here we insist that we deal with monotonically increasing timestamps
    if (!this_timestamp > last_timestamp) break
    if (values[j] > threshold) {
      if (!in_segment) {
        in_segment <- T
        last_start <- this_timestamp
      }
    } else {
      if (in_segment && last_timestamp - last_start >= min.segment.length) {
        segments$start <- append(segments$start, last_start)
        segments$end <- append(segments$end, last_timestamp)
      }
      in_segment <- F
    }
    last_timestamp <- this_timestamp
    j <- j + 1
  }
  
  if (in_segment && last_timestamp - last_start >= min.segment.length) {
    segments$start <- append(segments$start, last_start)
    segments$end <- append(segments$end, last_timestamp)
  }
  segments
}
