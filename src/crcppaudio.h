#ifndef CRcppAudio_H
#define CRcppAudio_H

#include "crcppwav.h"
#include "utils_global.h"

// opensmile wrapper to wav and mp3 files
// contain functions to convert between different formats
class CRcppAudio: public CRcppWave
{
public:
  enum Format {wav, mp3};
  CRcppAudio() = default;

  //if stereo rawData interleaved rawData
  //if mono simple raw data
  static void wavToMp3(const sWaveParameters & header, 
                       const std::vector<int32_t> & rawData ,
                       speech::filepath mp3_file_out);
  static void mp3ToWav(speech::filepath mp3_file_in, speech::filepath wav_file_out);
  static void mp3ToWav(speech::filepath mp3_file_in, sWaveParameters & header, 
                       std::vector<int32_t> & rawDataL , std::vector<int32_t> & rawDataR);
  
  static bool isWavFile ( speech::filepath file_in );
  static bool isMp3File ( speech::filepath file_in );

  static CRcppWave::Errors parseAudioFile(const speech::filepath & strFile, sWaveParameters & header, 
                                         std::vector<int32_t> & rawDataL , std::vector<int32_t> & rawDataR);
  //to stereo and mono
  //in mono case rawData - simple data
  //in stereo case rawData - interleaved data
  static void saveToAudioFile ( const sWaveParameters & header,                              
                                const std::vector<int32_t> & rawData,
                                const speech::filepath & filePath);
  //only to stereo: rawDataL - left channel, rawDataR - right channel  
  static void saveToAudioFileStereo ( const sWaveParameters & header,                              
                                      const std::vector<int32_t> & rawDataL,
                                      const std::vector<int32_t> & rawDataR,
                                      const speech::filepath & filePath);  
  
  bool setInputData (speech::filepath_vector audio_files_in, 
                     std::string config_string_in);
  void work();
protected:
  //if wav_file_out is empty fill in header, rawDataL, rawDataR
  //else write to wav_file_out
  static void mp3ToWav(speech::filepath mp3_file_in, speech::filepath wav_file_out, 
                       sWaveParameters & header, 
                       std::vector<int32_t> & rawDataL , std::vector<int32_t> & rawDataR);  
  //input data
  std::vector <Format> formatAudioFiles; //mp3 files have converted to tmp wav files, that must be removed after end of processing
};

#endif // CRcppAudio_H