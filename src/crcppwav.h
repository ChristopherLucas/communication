#ifndef CRcppWave_H
#define CRcppWave_H

#include "portaudio.h"
#include "crcppdatabase.h"

#include <vector>
#include <string>
#include <stdint.h>
#include <unistd.h>

class CRcppWave: public CRcppDataBase
{
public:
  enum Errors {NoError, StereoError, PcmError, FileNotOpenError, HeaderParseError};
  CRcppWave();
  ~CRcppWave();
  bool setInputData (std::vector<std::string> audio_files_in, 
                     std::string config_string_in);
  void getOutputData (std::vector <arma::mat> & rcpp_audio_features_out,
                      std::vector <arma::rowvec> & rcpp_audio_timestamps_out,
                      std::vector <sWaveParameters> & rcpp_wave_header_out);
  void getBorderFrames(std::vector <arma::rowvec> & rcpp_border_frame_starts_out,
                       std::vector <arma::rowvec> & rcpp_border_frame_ends_out);
  
  void work();
  static CRcppWave::Errors parseWavFile(const std::string & strWavfile, sWaveParameters & header, std::vector<int32_t> & error);
  static CRcppWave::Errors parseWavFile_sh_int(const std::string & strWavfile, sWaveParameters & pcmParams, std::vector<short int> & rawData_16);
  static const float int8_max;
  static const float int16_max;
  static const float int24_max;
  static const float int32_max;
  
  bool playWaveFile(std::vector<int32_t> rawData, sWaveParameters header);
  static void saveToWaveFile (const std::string & filePath,
                              const std::vector<int32_t> & rawData, 
                              const sWaveParameters & header);
  static void saveToWaveFile_sh_int (const std::string & filePath,
                                    const std::vector<short int> & rawData, 
                                    const sWaveParameters & header);  
  static int static_paStreamCallback(
                const void *input, void *output,
                unsigned long frameCount,
                const PaStreamCallbackTimeInfo* timeInfo,
                PaStreamCallbackFlags statusFlags,
                void *ptrInstance)
  {
    return reinterpret_cast<CRcppWave*>(ptrInstance)->paStreamCallback(input, output,
                                        frameCount, timeInfo,
                                        statusFlags);
  }
  int paStreamCallback(
      const void *input, void *output,
      unsigned long frameCount,
      const PaStreamCallbackTimeInfo* timeInfo,
      PaStreamCallbackFlags statusFlags);  
protected:
  enum class Endianness
  {
    LittleEndian,
    BigEndian
  };
  
  virtual void getData1file();
  
  //input data
  std::vector<std::string> audio_files; 
  std::string config_file;
  
  //output data
  std::vector <arma::mat> rcpp_audio_features;
  std::vector <arma::rowvec> rcpp_audio_timestamps;
  std::vector <sWaveParameters> rcpp_wave_header; 
  
  //output - turn testing
  std::vector <arma::rowvec> rcpp_border_frame_starts;
  std::vector <arma::rowvec> rcpp_border_frame_ends;  
    
  //wav playing
  bool portAudioOpen();
  std::vector<int32_t> rawDataPlayFile;
  sWaveParameters headerPlayFile;
  int indent_Audio_Raw_PlayFile {0};
  PaStream* streamPlayFile {nullptr};
  
  //wav writing
  static void writeDataToFile (std::vector<uint8_t>& fileData, std::string filePath);
  static void addStringToFileData (std::vector<uint8_t>& fileData, std::string s);
  static void addInt32ToFileData (std::vector<uint8_t>& fileData, int32_t i, Endianness endianness = Endianness::LittleEndian);
  static void addInt16ToFileData (std::vector<uint8_t>& fileData, int16_t i, Endianness endianness = Endianness::LittleEndian);
  
};

#endif // CRcppWave_H
