context("Test a linear pipeline of communication processing")

setup({
  config <- pipeline(
    list(
      'cIntensity',
      .output = 'loudness',
      intensity = 0,
      loudness = 1
    )
  )
  config_string <- communication:::generate_config_string(config)
  
  assign("config", config, envir = .GlobalEnv)
  assign("config_string", config_string, envir = .GlobalEnv)
})

teardown({
  rm(config, config_string, envir = .GlobalEnv)
})

test_that("Pipeline configuration object produces the correct config_string", {
  conf_file <- system.file('testdata', 'loudness.conf', package='communication')
  conf_file_string <- readChar(conf_file, file.info(conf_file)$size)
  # normalize eol for different platforms
  conf_file_string <- gsub("\r\n", "\n", conf_file_string)
  expect_equal(config_string, conf_file_string)
})

test_that("Pipeline configuration object produces the same config_string as a named component instantiation", {
  config_check <- communication::loudness(communication::createConfig())
  config_string_check <- communication:::generate_config_string(config_check)
  expect_equal(config_string, config_string_check)
})
