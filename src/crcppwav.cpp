#include <fstream>
#include <cstdio>
#include <cstdlib>
#include <limits.h>


#include "crcppwav.h"
#include <smileutil/smileUtil.h>
#include <smileutil/smileUtil_cpp.h>

#include "utils_global.h"

#include <Rcpp.h>
using namespace Rcpp;

#define MODULE "CRcppWave"

union sample24bit {
  char b[4];
  int32_t result;
};

const float CRcppWave::int8_max = 127.;
const float CRcppWave::int16_max = 32767.;
const float CRcppWave::int24_max = 8388607.;
const float CRcppWave::int32_max = 2147483647.;

CRcppWave::CRcppWave() : CRcppDataBase()
{
  modeWork = cComponentManager::RccpWavFiles;
}

CRcppWave::~CRcppWave()
{
  std::remove(config_file.c_str());  
}

CRcppWave::Errors CRcppWave::parseWavFile(const std::string & strWavfile, sWaveParameters & pcmParams, std::vector<int32_t> & rawData)
{
  FILE * filehandle = fopen_speech(strWavfile.c_str(), "rb");  

  if (nullptr == filehandle)
    return CRcppWave::FileNotOpenError;
  //handle header
  int resHeader = smilePcm_readWaveHeader(filehandle, &pcmParams, nullptr);
  if(0 == resHeader)
  {
    fclose(filehandle);    
    return CRcppWave::HeaderParseError;   
  }
  if(1 != pcmParams.nChan)
  {
    fclose(filehandle);    
    return CRcppWave::StereoError;     
  }
  if(1 != pcmParams.audioFormat)
  {
    fclose(filehandle);    
    return CRcppWave::PcmError;     
  }
  //handle samples
  long startRaw = ftell(filehandle);
  fseek(filehandle, 0, SEEK_END);   
  long size = ftell(filehandle);
  fseek(filehandle, startRaw, SEEK_SET); 
  uint8_t *start_buf = new uint8_t[size - startRaw];
  int nRead = (int)fread(start_buf, 1, size-startRaw, filehandle);
  int nSamples = nRead/(pcmParams.nChan * pcmParams.nBPS);
  uint8_t * buf = start_buf;
  
  switch(pcmParams.nBPS)
  {
  case 1: // 8-bit int
    for(int iSample=0; iSample<nSamples; iSample++)
    {
      int32_t value_32;
      value_32 = (*buf) * (int32_max/int8_max);
      rawData.push_back(value_32);
      buf += pcmParams.nChan * pcmParams.nBPS;  
    }
    break;
  case 2: // 16-bit int
    for(int iSample=0; iSample<nSamples; iSample++)
    {
      int32_t value_32;
      value_32 = (*reinterpret_cast<const int16_t*>(buf)) * (int32_max/int16_max);
      rawData.push_back(value_32);
      buf += pcmParams.nChan * pcmParams.nBPS;  
    }    
    break;
  case 3: // 24-bit int
    for(int iSample=0; iSample<nSamples; iSample++)
    {
      int32_t value_32;
      sample24bit union_ret;
      union_ret.b[0] = buf[0];
      union_ret.b[1] = buf[1];
      union_ret.b[2] = buf[2];
      union_ret.b[3] = (buf[2] & 0x80 ? 0xFF : 0);
      value_32= union_ret.result * (int32_max/int24_max);
      rawData.push_back(value_32);
      buf += pcmParams.nChan * pcmParams.nBPS;    
    }     
    break;
  case 4: // 32-bit int or 24-bit packed int
    if (24 == pcmParams.nBits) 
    {
      for(int iSample=0; iSample<nSamples; iSample++) 
      {
        int32_t value_32;        
        value_32 = (*(reinterpret_cast<int32_t*>(buf)))&0xFFFFFF;
        rawData.push_back(value_32);
        buf += pcmParams.nChan * pcmParams.nBPS; 
      }
    } 
    else if (pcmParams.nBits == 32) 
    {
      for(int iSample=0; iSample<nSamples; iSample++)
      {
        int32_t value_32;
        value_32 = *(reinterpret_cast<int32_t*>(buf));
        rawData.push_back(value_32);
        buf += pcmParams.nChan * pcmParams.nBPS;  
      }
    }    
    break;    
  }
  
  delete[] start_buf;
  fclose(filehandle);
  return CRcppWave::NoError;  
}

CRcppWave::Errors CRcppWave::parseWavFile_sh_int(const std::string & strWavfile, sWaveParameters & pcmParams, std::vector<short int> & rawData_16)
{
  std::vector<int32_t> rawData;
  CRcppWave::Errors res = parseWavFile(strWavfile, pcmParams, rawData);
  for(int i=0; i<rawData.size(); i++)
  {
    rawData_16.push_back(rawData[i] * (SHRT_MAX/int32_max));  
  }
  return res;
}

bool CRcppWave::playWaveFile(std::vector<int32_t> rawData_, sWaveParameters header_)
{
  indent_Audio_Raw_PlayFile = 0;
  rawDataPlayFile = rawData_;
  headerPlayFile = header_;
  Rprintf("start playing...\n");
  if(!portAudioOpen())
  {
    Rcout << "portAudioOpen false";
    return false;
  }
  // wait until streamPlayFile has finished playing
  while(Pa_IsStreamActive(streamPlayFile) > 0)
    Pa_Sleep(1000);
  
  Rprintf("finished\n");
  Pa_CloseStream(streamPlayFile);
  Pa_Terminate();  
  return true;
}
  
int CRcppWave::paStreamCallback(
    const void *input, void *output,
    unsigned long frameCount,
    const PaStreamCallbackTimeInfo* timeInfo,
    PaStreamCallbackFlags statusFlags)
{
  int numRead = frameCount;
  if(frameCount > rawDataPlayFile.size() - indent_Audio_Raw_PlayFile)
    numRead = rawDataPlayFile.size() - indent_Audio_Raw_PlayFile;
  std::memcpy(output, rawDataPlayFile.data() + indent_Audio_Raw_PlayFile, numRead*sizeof(int32_t));
  output = reinterpret_cast<uint8_t *>(output) + numRead *sizeof(int32_t);
  frameCount -= numRead;
  indent_Audio_Raw_PlayFile += numRead;
  if(frameCount > 0) 
  {
    memset(output, 0, frameCount * headerPlayFile.nChan *sizeof(int32_t));
    return paComplete;
  }
  return paContinue;
}

bool CRcppWave::portAudioOpen() 
{
  if(paNoError != Pa_Initialize())
  {
    Rcout << "PortAudio: Pa_Initialize failed";
    return false;
  }
  PaStreamParameters outputParameters;
  
  
  PaError ret =Pa_OpenDefaultStream(&streamPlayFile,
                                    0,                                  /* no input */
                                    headerPlayFile.nChan,                          /* stereo out */
                                    paInt32,                               /* floating point */
                                    headerPlayFile.sampleRate,
                                    paFramesPerBufferUnspecified,
                                    &CRcppWave::static_paStreamCallback,
                                    reinterpret_cast<void*>(this));        /* our sndfile data struct */
  
  if(paNoError != ret) 
  {
    Rcout << "PortAudio: Pa_OpenDefaultStream failed: err - " << ret << " err text - " << Pa_GetErrorText(ret) << std::endl;
    if(streamPlayFile)
      Pa_CloseStream(streamPlayFile);
    return false;
  }
  
  if (paNoError != Pa_StartStream(streamPlayFile))
  {
    Rcout << "PortAudio: Pa_StartStream failed";
    return false;
  }
  else
  {
    return true;
  }
}


bool CRcppWave::setInputData (std::vector<std::string> audio_files_in, 
                                     std::string config_string_in)
{
  std::remove(config_file.c_str());
  config_file = std::tmpnam(nullptr);
  std::ofstream stream;
  stream.open(config_file, std::ofstream::out | std::ofstream::trunc);
  if(!stream.is_open())
    return false;
  stream << config_string_in << std::endl;
  stream.close();
  audio_files = audio_files_in;
  
  //clear output data
  rcpp_audio_features.clear();
  rcpp_audio_timestamps.clear();
  rcpp_wave_header.clear();  
  
  return true;
}

void CRcppWave::getOutputData (std::vector <arma::mat> & rcpp_audio_features_out,
                    std::vector <arma::rowvec> & rcpp_audio_timestamps_out,
                    std::vector <sWaveParameters> & rcpp_wave_header_out)
{
  rcpp_audio_features_out = rcpp_audio_features;
  rcpp_audio_timestamps_out = rcpp_audio_timestamps;
  rcpp_wave_header_out = rcpp_wave_header;
}

void CRcppWave::getBorderFrames(std::vector <arma::rowvec> & rcpp_border_frame_starts_out,
                     std::vector <arma::rowvec> & rcpp_border_frame_ends_out)
{
  rcpp_border_frame_starts_out = rcpp_border_frame_starts;
  rcpp_border_frame_ends_out = rcpp_border_frame_ends;
}

void CRcppWave::work()
{
  for(int iFile = 0; iFile < audio_files.size(); iFile++)
  {
    std::vector<std::string> arguments;
    arguments.push_back(std::string("-I"));
    arguments.push_back(audio_files[iFile]);
    arguments.push_back(std::string("-C")); 
    arguments.push_back(config_file);
    work1file(arguments);
  }
}
  
void CRcppWave::getData1file()
{
  arma::rowvec rcpp_audio_start_frames_1;
  arma::rowvec rcpp_audio_end_frames_1;  
  cmanGlob->getWaveFrameBorders(rcpp_audio_start_frames_1,
                                rcpp_audio_end_frames_1);
  rcpp_border_frame_starts.push_back(rcpp_audio_start_frames_1);
  rcpp_border_frame_ends.push_back(rcpp_audio_end_frames_1);

  arma::mat rcpp_audio_features_1;
  arma::rowvec rcpp_audio_timestamps_1;
  sWaveParameters rcpp_wave_header_1;
    
  cmanGlob->getFeatures(rcpp_audio_features_1,
                        rcpp_audio_timestamps_1, rcpp_wave_header_1);
    
  rcpp_audio_features.push_back(rcpp_audio_features_1);
  rcpp_audio_timestamps.push_back(rcpp_audio_timestamps_1);
  rcpp_wave_header.push_back(rcpp_wave_header_1);    
}


void CRcppWave::saveToWaveFile_sh_int ( const std::string & filePath,
                                        const std::vector<short int> & rawData, 
                                        const sWaveParameters & header)
{
  std::vector<int32_t> rawData_32;
  //fill in rawData_32
  for(int i=0; i<rawData.size(); i++)
  {
    rawData_32.push_back(rawData[i] * (int32_max/SHRT_MAX)); 
  }
  saveToWaveFile(filePath, rawData_32, header);
}

void CRcppWave::saveToWaveFile (const std::string & filePath,
                                const std::vector<int32_t> & rawData, 
                                const sWaveParameters & header)
{
  std::vector<uint8_t> fileData;
  
  int32_t dataChunkSize = header.nBlocks * header.blockSize;
  int32_t samplesInChannel =  header.nBlocks/ header.nChan;
  
  // -----------------------------------------------------------
  // HEADER CHUNK
  addStringToFileData (fileData, "RIFF");
  
  // The file size in bytes is the header chunk size (4, not counting RIFF and WAVE) + the format
  // chunk size (24) + the metadata part of the data chunk plus the actual data chunk size
  int32_t fileSizeInBytes = 4 + 24 + 8 + dataChunkSize;
  addInt32ToFileData (fileData, fileSizeInBytes);
  
  addStringToFileData (fileData, "WAVE");
  
  // -----------------------------------------------------------
  // FORMAT CHUNK
  addStringToFileData (fileData, "fmt ");
  addInt32ToFileData (fileData, 16); // format chunk size (16 for PCM)
  addInt16ToFileData (fileData, 1); // audio format = 1
  addInt16ToFileData (fileData, (int16_t)header.nChan); // num channels
  addInt32ToFileData (fileData, (int32_t)header.sampleRate); // sample rate
  
  int32_t numBytesPerSecond = (int32_t) ((header.nChan * header.sampleRate * header.nBits) / 8);
  addInt32ToFileData (fileData, numBytesPerSecond);
  
  int16_t numBytesPerBlock = header.nChan * (header.nBits / 8);
  addInt16ToFileData (fileData, numBytesPerBlock);
  
  addInt16ToFileData (fileData, (int16_t)header.nBits);
  
  // -----------------------------------------------------------
  // DATA CHUNK
  addStringToFileData (fileData, "data");
  addInt32ToFileData (fileData, dataChunkSize);
  
  //now allways 1 channel
  for (int i = 0; i < samplesInChannel; i++)
  {
    switch(header.nBits)
    {
      case 8:
      {
        uint8_t value = (rawData[i] * int8_max) / int32_max;
        fileData.push_back (value);
      }
      break;
      case 16:
      {
        uint16_t value = (rawData[i] * int16_max) / int32_max;
        addInt16ToFileData (fileData,value ); 
      }
      break;
      case 32:
      {
        addInt32ToFileData (fileData, rawData[i]);           
      }
      break;
      default:
        Rcpp::stop("Trying to write a file with unsupported bit depth. Currently supported bit depth: 8,16,32");  
    }
  }

  // check that the various sizes we put in the metadata are correct
  if (fileSizeInBytes != (fileData.size() - 8) || dataChunkSize != (header.nBlocks * (header.nBits / 8)))
    Rcpp::stop("ERROR: couldn't save file to "  + filePath);
  
  // try to write the file
  return writeDataToFile (fileData, filePath);
}


void CRcppWave::writeDataToFile (std::vector<uint8_t>& fileData, std::string filePath)
{
  FILE * wav = fopen_speech( filePath.c_str(), "wb");
  if( nullptr == wav )
    Rcpp::stop("can not open file - " + filePath);
  
  for (int i = 0; i < fileData.size(); i++)
  {
    char value = (char) fileData[i];
    fwrite(& value, sizeof (char), 1, wav);
  }
  
  fclose(wav); 
}



void CRcppWave::addStringToFileData (std::vector<uint8_t>& fileData, std::string s)
{
  for (int i = 0; i < s.length();i++)
    fileData.push_back ((uint8_t) s[i]);
}



void CRcppWave::addInt32ToFileData (std::vector<uint8_t>& fileData, int32_t i, Endianness endianness)
{
  uint8_t bytes[4];
  
  if (endianness == Endianness::LittleEndian)
  {
    bytes[3] = (i >> 24) & 0xFF;
    bytes[2] = (i >> 16) & 0xFF;
    bytes[1] = (i >> 8) & 0xFF;
    bytes[0] = i & 0xFF;
  }
  else
  {
    bytes[0] = (i >> 24) & 0xFF;
    bytes[1] = (i >> 16) & 0xFF;
    bytes[2] = (i >> 8) & 0xFF;
    bytes[3] = i & 0xFF;
  }
  
  for (int i = 0; i < 4; i++)
    fileData.push_back (bytes[i]);
}

void CRcppWave::addInt16ToFileData (std::vector<uint8_t>& fileData, int16_t i, Endianness endianness)
{
  uint8_t bytes[2];
  
  if (endianness == Endianness::LittleEndian)
  {
    bytes[1] = (i >> 8) & 0xFF;
    bytes[0] = i & 0xFF;
  }
  else
  {
    bytes[0] = (i >> 8) & 0xFF;
    bytes[1] = i & 0xFF;
  }
  
  fileData.push_back (bytes[0]);
  fileData.push_back (bytes[1]);
}


