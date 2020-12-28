#' @name components
#' @title Components
#' 
#' @description Describes components to be added to audio config 
#' 
#' In order to extract features from your data file, 
#' you can add components to your config
#' 
#' @param config audio_config 
#' @param input  input
#' @param output output
#' @param func component function
#'
#' @section Component types:
#' 
#' Currently our package supports the following types of components.
#' 
#' 
#' \strong{Sample quantities} - Enable  quantities from sample value
#' 
#' \describe{
#' \item{`a_max`}{maximum absolute sample value}
#' \item{`zcr`}{zero-crossing rate}
#' \item{`mcr`}{mean-crossing rate}
#' \item{`maxmin`}{maximum and minimum sample value}
#' \item{`dc`}{dc-offset - arithmetic mean}
#' \item{`loudness`}{loudness}
#' \item{`intensity`}{intensity}
#' }
#' 
#' 
#' 
NULL


mzcr <- function(config, input = input, output = output, zcr = 0,amax = 0,mcr = 0,maxmin = 0,dc = 0) {
        create_component(config, 
                         input = input,
                         output = output,
                 component_type = 'cMZcr',
                 component_data = list(
                                       
                                       'zcr' = zcr,
                                       'amax' = amax,
                                       'mcr' = mcr,
                                       'maxmin' = maxmin,
                                       'dc' = dc))
}

# cMzr functions
#' @rdname components
#' @export
a_max <- function(config, input = "frames", output = "amax") mzcr(config, input = input, output = output, amax = 1)

#' @rdname components
#' @export
zcr <- function(config, input = "frames", output = "zcr") mzcr(config, input = input, output = output, zcr = 1)


#' @rdname components
#' @export
mcr <- function(config, input = "frames", output = "mcr") mzcr(config, input = input, output = output, mcr = 1)

#' @rdname components
#' @export
max_min <- function(config, input = "frames", output = "maxmin") mzcr(config,input = input, output = output, maxmin = 1)

#' @rdname components
#' @export
dc <- function(config, input = "frames", output = "dc") mzcr(config, input = input, output = output, dc = 1)

# cIntensity functions
loud_intensity <- function(config, input = input, output = output, loudness = 0, intensity = 0) {
  create_component(config, 
                   input = input,
                   output = output,
                   component_type = 'cIntensity',
                   component_data = list(
                                        
                                         'intensity' = intensity,
                                         'loudness' = loudness))
}
  
 

#' @rdname components
#' @export
intensity <- function(config, input = "frames", output = "intensity") loud_intensity(config, input = input, output = output, intensity = 1)


#' @rdname components
#' @export
loudness <- function(config, input = "frames", output = "loudness") loud_intensity(config, input = input, output = output, loudness = 1)


#' @rdname components
#' @export
fastFourierTransform <- function(config, input = 'frames',output = 'fft') {
    create_component(config, 
                     input = input,
                      output = output,
                      component_type = 'cTransformFFT',
                      component_data = list(
                                            'inverse'= 0,'zeroPadSymmetric' =0),
                      output_define = F
  )
}


#' @rdname components
#' @export
magPhase <- function(config, input = 'fft', output = 'magphase') {
    create_component(config, input = input,
      output = output,
      component_type = 'cFFTmagphase',
      component_data = list(),
      output_define = F
    )

}

#' @rdname components
#' @param usePower \code{binary}
#' @param cepstrum \code{binary}
#' @export
cacf <- function(config, input = "magphase", output = 'acf', usePower = 1, cepstrum = 0) {
    create_component(config, input = input,
                     output = output,
                     component_type = 'cMelspec',
                     component_data = list(
                                           'usePower' = usePower
                                           ),
                     n_features = 27,
                     output_define = T
    )
}

#' @rdname components
#' @param usePower \code{integer}
#' @param lofreq \code{integer}
#' @param hifreq \code{integer}
#' @param nBands \code{integer}
#' @export
melSpec <- function(config, input = "magphase", output = 'melspec', usePower = 1, lofreq = 20, hifreq = 8000, nBands = 26) {
    create_component(config, input = input,
                     output = output,
                     component_type = 'cMelspec',
                     component_data = list(
                                           'htkcompatible' = 0,
                                           'usePower' = usePower,
                                           'lofreq' = lofreq,
                                           'hifreq' = hifreq,
                                           'nBands' = nBands),
                     output_define = F
    )
}


#' @rdname components
#' @param firstMfcc \code{integer}
#' @param lastMfcc \code{integer}
#' @param cepLifter \code{double}
#' @param htkcompatible \code{integer}
#' @export
mfcc <- function(config, input = 'melspec', output = 'mfcc', 
                 firstMfcc = 1, lastMfcc = 12, cepLifter = 22.0, htkcompatible = 1) {
  create_component(config, input = input,
                   output = output,
                   component_type = 'cMfcc',
                   component_data = list(
                                         'firstMfcc' = firstMfcc,
                                         'lastMfcc'  = lastMfcc,
                                         'cepLifter' = cepLifter,
                                         'htkcompatible' = htkcompatible),
                   n_features = lastMfcc - firstMfcc + 1,
                   output_define = T
  )
}


#' @rdname components
#' @param nFormants \code{integer}
#' @export
formant <- function(config, input = 'lpc', output = 'formants', nFormants = 3 ) {
  create_component(config, input = input,
                   output = output,
                   component_type = 'cFormantLpc',
                   component_data = list(#'copyInputName' = 1,'processArrayFields' = 1,
                                         'nFormants' = nFormants),
                   n_features = nFormants,
                   output_define = T
  )
}


#' @rdname components
#' @param k \code{numeric} Coefficient for preemphasis
#' @export
vectorPreemphasis <- function(config, input = 'frames', output = 'pe', k = 0.97) {
      create_component(config,
                       input = input,
                       output = output,
                       component_type = 'cVectorPreemphasis',
                       component_data = list(
                         'k' = k
   
                       ),
                       output_define = F
                       
      )
}


#' @rdname components
#' @param method \code{string} Methods 'act', 'burg'
#' @export
lpc <- function(config, input = 'pe', output = 'lpc', method = 'acf') {
    create_component(config,
                     input = input,
                     output = output,
                     component_type = 'cLpc',
                     component_data = list(
                       'method' = method,
                       'p'= 8,
                       'saveLPCoeff' = 1,
                       'lpGain' = 0,
                       'saveRefCoeff' = 0,
                       'residual' = 0,
                       'forwardFilter' = 0,
                       'lpSpectrum' = 0
                       ),
                     output_define = F
                     )
}



#' @rdname components
#' @export
delta <- function(config, input = '', output = NULL) {
    if ( is.null(output) ) {
      output <- paste0("de_", input)
    }
    create_component(config, input = input,
                     output = output,
                     component_type = 'cDeltaRegression',
                     component_data = list(
                       'nameAppend' = 'de',
                       'copyInputName' = 1,
                       'noPostEOIprocessing' = 0,
                       'deltawin'=2,
                       'blocksize'=1
                     ),
                     output_define = T
                     
    )

}


#' @rdname components
#' @export
delta2 <- function(config, input = '', output = 'dede') {
  if( input == '' ) { 
    input <- config$`lldrcppdatasink:cRcppDataSink`$reader.dmLevel
  } else {
    input <- paste0(input, collapse = ';')
  }
  create_component(config, input,
                   output = output,
                   component_type = 'cDeltaRegression',
                   component_data = list(
                     'nameAppend' = 'de',
                     'copyInputName' = 1,
                     'noPostEOIprocessing' = 0,
                     'deltawin'=2,
                     'blocksize'=1
                   ),
                   output_define = T
                   
  )
  
}

#' @rdname components
#' @param winFunc \code{character}
#' @param gain \code{numeric}
#' @param offset \code{integer}
#' @export
winFrame <- function(config, input = "frames", output = 'winframes', 
                   winFunc = "ham", gain = 1.0, offset = 0) {
  create_component(config, input = input,
                   output = output,
                   component_type = 'cWindower',
                   component_data = list(
                                         winFunc = winFunc,                                       
                                         gain = 1.0,                                                      
                                         offset = 0  ),
                   output_define = F
  )
}

#' @rdname components
#' @param rms \code{integer}
#' @param log_ \code{integer}
#' @export
energy <- function(config, input = "winframes", output = 'energy', 
                         rms = 1, log_ = 0) {
  create_component(config, input = input,
                   output = output,
                   component_type = 'cEnergy',
                   component_data = list(
                                         rms = rms, log = log_),
                   output_define = T
  )
}



#' @rdname components
#' @param maxTurnLength \code{integer}
#' @param maxTurnLengthGrace \code{integer}
#' @param threshold \code{numeric}
#' @export
turnDetector <- function(config, input = "energy", output = 'turn', 
                         maxTurnLength = 12, maxTurnLengthGrace = 3, threshold = 0.0015) {
  create_component(config, input = input,
                   output = output,
                   component_type = 'cTurnDetector',
                   component_data = list(
                                         'writer.levelconf.noHang' = 1,
                                         'messageRecp' = "functL1",
                                         'eventRecp' = "turnDump",
                                         'maxTurnLength' = maxTurnLength,
                                         'maxTurnLengthGrace' = maxTurnLengthGrace,
                                         'idx' = 0,
                                         'nPost' = 30,
                                         'nPre' = 10,
                                         'useRMS' = 1,
                                         'autoThreshold' = 0,              
                                         'threshold' = threshold),
                   output_define = T
  )
}



#' @rdname components
#' @export
addComponents <- function(config, func, input, output = NULL) {
  funcs <- purrr::map(input, ~ purrr::partial(delta, input = .x))
  do.call(purrr::compose, funcs)(config)
}
