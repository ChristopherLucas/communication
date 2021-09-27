context("Parse WAV File")

setup({
  wavfile <- system.file('testdata', 'opensmile.wav', package='communication')
  wav <- communication:::rcpp_parseAudioFile(wavfile)
  assign("wav", wav, envir = .GlobalEnv)
})

teardown({
  rm(wav, envir = .GlobalEnv)
})

test_that("wav has expected attributes", {
  expect_true("header" %in% names(wav))
  expect_true("data" %in% names(wav))
})

test_that("wav header has expected attributes and values", {
  h <- wav$header
  expect_equal(h$sampleRate, 44100)
  expect_equal(h$nChan, 1)
  expect_equal(h$blockSize, 2)
  expect_equal(h$nBPS, 2)
  expect_equal(h$nBits, 16)
  expect_equal(h$nBlocks, 90112)
})
