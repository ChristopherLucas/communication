#include <RcppArmadillo.h> 
// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppCommon.h>

#include <core/smileCommon.hpp>

#include <core/configManager.hpp>
#include <core/commandlineParser.hpp>
#include <core/componentManager.hpp>

#include "crcppdatabase.h"
#include "crcppwav.h"
#include "crcppaudio.h"

#include <fstream>
#include <cstdio>
#include <cstdlib>
#include <algorithm>

#include "lame.h"
#include "id3.h"
#include "utils_global.h"

#define MODULE "RcppOpenSmileMain"

namespace Rcpp {
template <>
SEXP wrap(const sWaveParameters& x);
}

namespace Rcpp {
template <>
SEXP wrap(const sWaveParameters& x) {
  return Rcpp::wrap(Rcpp::List::create(Rcpp::Named("sampleRate") = Rcpp::wrap(x.sampleRate),
                                       Rcpp::Named("sampleType") = Rcpp::wrap(x.sampleType),
                                       Rcpp::Named("nChan") = Rcpp::wrap(x.nChan),
                                       Rcpp::Named("blockSize") = Rcpp::wrap(x.blockSize),                                       
                                       Rcpp::Named("nBPS") = Rcpp::wrap(x.nBPS),  
                                       Rcpp::Named("nBits") = Rcpp::wrap(x.nBits),
                                       Rcpp::Named("byteOrder") = Rcpp::wrap(x.byteOrder),
                                       Rcpp::Named("memOrga") = Rcpp::wrap(x.memOrga),
                                       Rcpp::Named("nBlocks") = Rcpp::wrap(x.nBlocks),
                                       Rcpp::Named("headerOffset") = Rcpp::wrap(x.headerOffset)));
};
}

using namespace Rcpp;

sWaveParameters waveHeader_cPP(Rcpp::List header)
{
  sWaveParameters wp;
  wp.sampleRate = Rcpp::as<long>(header["sampleRate"]);
  wp.sampleType = Rcpp::as<int>(header["sampleType"]);
  wp.nChan = Rcpp::as<int>(header["nChan"]);
  wp.blockSize = Rcpp::as<int>(header["blockSize"]);
  wp.nBPS = Rcpp::as<int>(header["nBPS"]);
  wp.nBits = Rcpp::as<int>(header["nBits"]);
  wp.byteOrder = Rcpp::as<int>(header["byteOrder"]);
  wp.memOrga = Rcpp::as<int>(header["memOrga"]);
  wp.nBlocks = Rcpp::as<long>(header["nBlocks"]);
  wp.headerOffset =Rcpp::as<int>(header["headerOffset"]);
  wp.audioFormat =Rcpp::as<uint16_t>(header["headerOffset"]);
  return wp;
}

// [[Rcpp::export]]
SEXP rcpp_parseAudioFile(std::string strWavfile)
{
  std::vector<int32_t> rawDataL;
  std::vector<int32_t> rawDataR;  
  sWaveParameters header;
  CRcppWave::Errors error = CRcppAudio::parseAudioFile(strWavfile, header, rawDataL, rawDataR);  
  if(CRcppWave::NoError == error)
  {
    if(1 == header.nChan)
      return Rcpp::List::create(_["header"]=header, _["data"]=rawDataL);
    else
      return Rcpp::List::create(_["header"]=header, _["dataL"]=rawDataL, _["dataR"]=rawDataR);      
  }
  else if(CRcppWave::HeaderParseError == error)
    Rcpp::stop("Error parsing file header");  
  else if(CRcppWave::PcmError == error)
    Rcpp::stop("Error parsing file. Unsupported file format: float data");  
  else if(CRcppWave::FileNotOpenError == error)
    Rcpp::stop("Error parsing file. Can not open file - " + strWavfile);      
  else
    Rcpp::stop("Error parsing file. Unknown error! Please say devs that needing rewrite rcpp_parseAudioFile)");    
}
 
   
// [[Rcpp::export]]   
bool rcpp_playWavFile(Rcpp::List header, std::vector<int32_t> rawData)
{
  CRcppWave rcpp_wave;
  return rcpp_wave.playWaveFile(rawData, waveHeader_cPP(header));
}

// [[Rcpp::export]]
SEXP test_rcpp_playWavFile(std::string strWavfile)
{
  std::vector<int32_t> rawData;
  sWaveParameters header;
  Rcout << "test_rcpp_playWavFile" << std::endl;
  CRcppWave::Errors error = CRcppWave::parseWavFile(strWavfile, header, rawData);  
  if(CRcppWave::PcmError == error)
    Rcpp::stop("Error parsing file. Unsupported file format: float data");  
  else if(CRcppWave::FileNotOpenError == error)
    Rcpp::stop("Error parsing file. Can not open file - " + strWavfile);      
  else if(CRcppWave::HeaderParseError == error)
    Rcpp::stop("Error parsing file header");  
  
  CRcppWave rcpp_wave;
  bool res = rcpp_wave.playWaveFile(rawData, header);
  return Rcpp::List::create(res, header, rawData);  
}

//to mono and stereo
//in stereo case: rawData is interleaved data of all channels
//in mono case: rawData is simple data
// [[Rcpp::export]]   
void rcpp_writeAudioFile( Rcpp::List header, std::vector<int32_t> rawData, std::string  filePath)
{
  return  CRcppAudio::saveToAudioFile(waveHeader_cPP(header), rawData, filePath); 
}

//only to stereo
//rawDataL - data of left channel, rawDataR - data of right channel
// [[Rcpp::export]]   
void rcpp_writeAudioFileStereo(Rcpp::List header, std::vector<int32_t> rawDataL,  std::vector<int32_t> rawDataR,std::string  filePath)
{
  return  CRcppAudio::saveToAudioFileStereo(waveHeader_cPP(header), rawDataL, rawDataR,filePath); 
}

// [[Rcpp::export]]  
void test_rcpp_writeWavFile(std::string  filePathIn, std::string  filePathOut)
{
  std::vector<int32_t> rawData;
  sWaveParameters header;
  CRcppWave::Errors error = CRcppWave::parseWavFile(filePathIn, header, rawData);
  header.nBPS = 4;
  header.nBits = 32;
  header.blockSize = header.nChan * header.nBPS;
  if(CRcppWave::PcmError == error)
    Rcpp::stop("Error parsing file. Unsupported file format: float data");  
  else if(CRcppWave::FileNotOpenError == error)
    Rcpp::stop("Error parsing file. Can not open file - " + filePathIn);      
  else if(CRcppWave::HeaderParseError == error)
    Rcpp::stop("Error parsing file header");  
  
  //return   CRcppWave::saveToWaveFile(header, rawData, filePathOut);
  return   CRcppAudio::saveToAudioFile(header, rawData, filePathOut);
}

// [[Rcpp::export]]
SEXP rcpp_openSmileGetFeatures(std::vector<std::string> audio_files_in, 
                          std::string config_string_in)
{
  Rcpp::List result;
  try { 
    CRcppAudio rcppAudio;      
    if(rcppAudio.setInputData(audio_files_in, config_string_in));
    {
      rcppAudio.work();
      std::vector <arma::mat> rcpp_audio_features;
      std::vector <arma::rowvec> rcpp_audio_timestamps;
      std::vector <sWaveParameters> rcpp_wave_header;     
      rcppAudio.getOutputData( rcpp_audio_features,
                              rcpp_audio_timestamps, 
                              rcpp_wave_header);
      for(int i=0; i<rcpp_audio_features.size(); i++)
      {
        {
          std::string name = "audio_features_" + std::to_string(i);
          result[name.c_str()] =  rcpp_audio_features[i];
        }
        {
          std::string name = "audio_timestamps_" + std::to_string(i);
          result[name.c_str()] =  rcpp_audio_timestamps[i];
        }
        {
          std::string name = "wave_header_" + std::to_string(i);
          result[name.c_str()] =  rcpp_wave_header[i];
        }         
      }
    }
  }
  catch (const std::bad_alloc& e) 
  {
    Rcpp::stop("Allocation failed: " + std::string(e.what()));
  }
  return result;
}

// [[Rcpp::export]]
SEXP test_rcpp_openSmileGetFeatures(std::vector<std::string> audio_files_in, 
                                  std::string config_file_in)
{
  Rcpp::List result;  
  std::string config_string_in;
  
  std::ifstream stream;
  stream.open(config_file_in, std::ifstream::binary);
  if(stream.is_open())
  {
    stream.seekg (0, stream.end);
    config_string_in.reserve(stream.tellg());
    stream.seekg (0, stream.beg);
    
    config_string_in.assign((std::istreambuf_iterator<char>(stream)),
                  std::istreambuf_iterator<char>());
    stream.close();  
    result = rcpp_openSmileGetFeatures(audio_files_in, config_string_in);    
  }
  
  return result;
}


// [[Rcpp::export]]
SEXP  rcpp_openSmileGetBorderFrames(std::vector<std::string> audio_files_in, 
                              std::string config_string_in)
{
  Rcpp::List result;
  try { 
    CRcppAudio rcppAudio;      
    if(rcppAudio.setInputData(audio_files_in, config_string_in));
    {
      rcppAudio.work();
      std::vector <arma::rowvec> rcpp_border_frame_starts;
      std::vector <arma::rowvec> rcpp_border_frame_ends;     
      rcppAudio.getBorderFrames( rcpp_border_frame_starts,
                                rcpp_border_frame_ends);
      for(int i=0; i<rcpp_border_frame_starts.size(); i++)
      {
        {
          std::string name = "border_frames_starts_" + std::to_string(i);
          result[name.c_str()] =  rcpp_border_frame_starts[i];
        }
        {
          std::string name = "border_frames_ends_" + std::to_string(i);
          result[name.c_str()] =  rcpp_border_frame_ends[i];
        }
      }
    }
  }
  catch (const std::bad_alloc& e) 
  {
    Rcpp::stop("Allocation failed: " + std::string(e.what()));
  }
  return result;
}

// [[Rcpp::export]]
SEXP test_rcpp_openSmileGetBorderFrames(std::vector<std::string> audio_files_in, 
                               std::string config_file_in)
{
  Rcpp::List result;  
  std::string config_string_in;
    
  std::ifstream stream;
  stream.open(config_file_in, std::ifstream::binary);
  if(stream.is_open())
  {
    stream.seekg (0, stream.end);
    config_string_in.reserve(stream.tellg());
    stream.seekg (0, stream.beg);
    
    config_string_in.assign((std::istreambuf_iterator<char>(stream)),
                            std::istreambuf_iterator<char>());
    stream.close();  
    result =  rcpp_openSmileGetBorderFrames(audio_files_in, config_string_in);    
  }
  
 
  return result;
}


// [[Rcpp::export]]
SEXP  rcpp_openSmileMain(std::vector<std::string> arguments)
{
  CRcppDataBase rccpDataBase;
  return Rcpp::wrap(rccpDataBase.work1file(arguments));
}

// [[Rcpp::export]]
SEXP rcpp_openSmileGetFeatures_Turns(std::vector<std::string> audio_files_in, 
                                     std::string config_string_in)
{
  Rcpp::List features = rcpp_openSmileGetFeatures(audio_files_in, config_string_in);
  Rcpp::List turns = rcpp_openSmileGetBorderFrames(audio_files_in, config_string_in);
  return Rcpp::List::create(features, turns);
}

// [[Rcpp::export]]
SEXP test_rcpp_openSmileGetFeatures_Turns(std::vector<std::string> audio_files_in, 
                                        std::string config_file_in)
{
  Rcpp::List features = test_rcpp_openSmileGetFeatures(audio_files_in, config_file_in);
  Rcpp::List turns = test_rcpp_openSmileGetBorderFrames(audio_files_in, config_file_in);
  return Rcpp::List::create(features, turns);  
}