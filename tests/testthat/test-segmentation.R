context("Segment wav file using voice activity detection")

setup({
  wavfile <- system.file('testdata', 'opensmile.wav', package='communication')
  assign("wavfile", wavfile, envir = .GlobalEnv)
})

teardown({
  rm(wavfile, envir = .GlobalEnv)
})

test_that("segmentation finds one segment at the correct timestamps", {
  segments <- communication::segment.utterances(files=wavfile, use.full.names=F, use.exts=F)
  # The (single) attribute of segments is called 'opensmile' simply because that's the basename of our wav file
  segment = segments$opensmile
  expect_true(length(segment$start) == 1)
  expect_true(length(segment$end) == 1)
  expect_equal(segment$start, 1.0)
  expect_equal(segment$end, 2.0125)
})
