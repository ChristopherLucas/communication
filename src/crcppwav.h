#ifndef CRcppWave_H
#define CRcppWave_H

#include "portaudio.h"
#include "crcppdatabase.h"

#include <vector>
#include <string>
#include <stdint.h>
#include <unistd.h>

#include "utils_global.h"

class CRcppWave: public CRcppDataBase
{
public:
  enum Errors {NoError, PcmError, FileNotOpenError, HeaderParseError, UnknownFormatError, IncorrectData};
  CRcppWave();
  ~CRcppWave();
  bool setInputData (speech::filepath_vector audio_files_in, 
                     std::string config_string_in);
  void getOutputData (std::vector <arma::mat> & rcpp_audio_features_out,
                      std::vector <arma::rowvec> & rcpp_audio_timestamps_out,
                      std::vector <sWaveParameters> & rcpp_wave_header_out);
  void getBorderFrames(std::vector <arma::rowvec> & rcpp_border_frame_starts_out,
                       std::vector <arma::rowvec> & rcpp_border_frame_ends_out);
  
  void work();
  //if stereo return rawDataL - left channel, rawDataR - right channel
  //if mono rawDataL stores simple data
  static CRcppWave::Errors parseWavFile(const speech::filepath & strWavfile, sWaveParameters & header, 
                                        std::vector<int32_t> & rawDataL , std::vector<int32_t> & rawDataR);
  //if stereo return rawDataL - left channel, rawDataR - right channel
  //if mono rawDataL stores simple data  
  static CRcppWave::Errors parseWavFile(const speech::filepath & strWavfile, sWaveParameters & pcmParams, 
                                        std::vector<short int> & rawData_16L, std::vector<short int> & rawData_16R);
  //if stereo return Interleaved rawData
  //if mono return simple data
  static CRcppWave::Errors parseWavFile(const speech::filepath & strWavfile, sWaveParameters & header, 
                                        std::vector<int32_t> & rawData);
  //if stereo return Interleaved rawData
  //if mono return simple data  
  static CRcppWave::Errors parseWavFile( const speech::filepath & strWavfile, sWaveParameters & pcmParams, 
                                          std::vector<short int> & rawData_16);
  
  static CRcppWave::Errors subsetWavFile(const speech::filepath & strWavfile, 
                                         double  startSubWav,               //seconds 
                                         double endSubWav,                  //seconds
                                         const speech::filepath & filePathOut);
  
  
  static CRcppWave::Errors subsetWavFile(const speech::filepath & strWavfile, 
                                         double  startSubWav,                     //seconds 
                                         double endSubWav,                        //seconds
                                         sWaveParameters & headerSubset,
                                         std::vector<int32_t> & rawDataSubset);     //if stereo Interleaved, if mono simple data     
  
 
  static CRcppWave::Errors subsetWavFile(const sWaveParameters & header,             //the same to origin and subset
                                        const std::vector<int32_t> & rawDataOrigin, //if stereo Interleaved, if mono simple data
                                        double  startSubWav,                      //seconds 
                                        double endSubWav,                         //seconds
                                        std::vector<int32_t> & rawDataSubset);      //if stereo Interleaved, if mono simple data 
                                                 
  static const float int8_max;
  static const float int16_max;
  static const float int24_max;
  static const float int32_max;
  
  bool playWaveFile(std::vector<int32_t> rawData, sWaveParameters header);
  //to mono and stereo
  //in stereo case: rawData is interleaved data
  static void saveToWaveFile (const sWaveParameters & header,                              
                              const std::vector<int32_t> & rawData,
                              const speech::filepath & filePathOut);
  //only to stereo
  static void saveToWaveFileStereo (const sWaveParameters & header,                              
                                    const std::vector<int32_t> & rawDataL,
                                    const std::vector<int32_t> & rawDataR,
                                    const speech::filepath & filePathOut);
  //to mono and stereo
  //in stereo case: rawData is interleaved data  
  static void saveToWaveFile (const sWaveParameters & header,                              
                              const std::vector<short int> & rawData,
                              const speech::filepath & filePathOut);
  
  //to mono and stereo
  //in stereo case: rawData is interleaved data  
  static void saveToWaveFile (const sWaveParameters & header,                              
                              const std::vector<int32_t> & rawData,
                              std::vector<uint8_t> & fileDataOut);    
protected:
  enum class Endianness
  {
    LittleEndian,
    BigEndian
  };
  
  virtual void getData1file();
  
  //input data
  speech::filepath_vector audio_files; 
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
  
  //wav writing
  static void writeDataToFile (std::vector<uint8_t>& fileData, std::string filePath);
  static void addStringToFileData (std::vector<uint8_t>& fileData, std::string s);
  static void addInt32ToFileData (std::vector<uint8_t>& fileData, int32_t i, Endianness endianness = Endianness::LittleEndian);
  static void addInt16ToFileData (std::vector<uint8_t>& fileData, int16_t i, Endianness endianness = Endianness::LittleEndian);
  
  //if bInterleaved==true rawDataL stores data (if stereo interleaved raw data, if mono simple data), 
  //if bInterleaved==false if stereo: in rawDataL data of left channel
  // in rawDataR data of left channel
  //if mono: simple data
  static CRcppWave::Errors parseWavFile(const speech::filepath & strWavfile, sWaveParameters & header,
                                        bool bInterleaved,
                                        std::vector<int32_t> & rawDataL , std::vector<int32_t> & rawDataR);  
  
};

#endif // CRcppWave_H
