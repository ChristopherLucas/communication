#include <RcppCommon.h>

#include <core/smileCommon.hpp>

#include <core/configManager.hpp>
#include <core/commandlineParser.hpp>
#include <core/componentManager.hpp>

#include "crcppdatabase.h"
#include "crcppwav.h"

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

#include <RcppArmadillo.h> 
// [[Rcpp::depends(RcppArmadillo)]]


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


// [[Rcpp::export]]
void wavToMp3(std::string wav_file_in, std::string mp3_file_out)
{
  setlocale(LC_ALL, " ");
  
  //tilda handling
  mp3_file_out = tildaString(mp3_file_out);   
  wav_file_in = tildaString(wav_file_in); 
  
  FILE *pcm = nullptr;
  FILE *mp3 = nullptr;
  lame_t lame = nullptr;
  try
  {
    pcm = fopen_speech(wav_file_in.c_str(), "rb");
    if( nullptr == pcm )
      throw std::string("can not open input wave file");
    
    mp3 = fopen_speech(mp3_file_out.c_str(), "wb");
    if( nullptr == mp3 )
      throw std::string("can not open output mp3 file"); 
    
    lame = lame_init();
    if (nullptr == lame) 
      throw std::string("fatal error during initialization lame");  
    
    int read, write;  
    
    const std::vector<short int>::size_type PCM_SIZE = 8192;
    const int MP3_SIZE = 8192;
    
    unsigned char mp3_buffer[MP3_SIZE];
    
    //take wav header
    std::vector<short int> rawData;
    sWaveParameters header;
    {
      CRcppWave::Errors error = CRcppWave::parseWavFile_sh_int(wav_file_in, header, rawData);  
    
      if(CRcppWave::StereoError == error)
        throw std::string("Error parsing file. Unsupported file format: stereo channels");
      else if(CRcppWave::HeaderParseError == error)
        throw std::string("Error parsing file header");  
      else if(CRcppWave::PcmError == error)
        throw std::string("Error parsing file. Unsupported file format: float data");  
      else if(CRcppWave::FileNotOpenError == error)
        throw std::string("Error parsing file. Can not open file - " + wav_file_in);      
      else if(CRcppWave::NoError != error)
        throw std::string("Error parsing file. Unknown error! Please say devs that needing rewrite encode_lame)");
    }
    if (-1 == lame_set_in_samplerate(lame, header.sampleRate) )
      throw std::string("fatal error during set samplerate to lame: unsupported samplerate");  
    if(-1 == lame_set_num_channels( lame, header.nChan ) )
      throw std::string("fatal error during set number channels to lame: unsupported number of channels");     
    if( 1 == header.nChan )
      lame_set_mode( lame, MONO );
  
    if(-1 == lame_set_VBR(lame, vbr_default) )
      throw std::string("fatal error during set VBR to lame: unsupported VBR");     
    if( 0 > lame_init_params(lame) )
      throw std::string("fatal error during initialization parameters of lame");  
    short int * pcm_buffer = rawData.data();
    std::vector<short int>::size_type handled_elems = 0;
    do {
      
      auto sz = 2*sizeof(short int);
      std::vector<short int>::size_type numElems = std::min(PCM_SIZE, (rawData.size() - handled_elems)/2);
      if( 1 == header.nChan )
      {
        sz = sizeof(short int);
        numElems = std::min(PCM_SIZE, rawData.size() - handled_elems);
      }
      read = fread(pcm_buffer, sz, numElems, pcm);
      if (0 == read)
        write = lame_encode_flush(lame, mp3_buffer, MP3_SIZE);
      else
      {
        if( 1 == header.nChan )
        {
          write = lame_encode_buffer(lame, pcm_buffer, nullptr, read, mp3_buffer, MP3_SIZE);
        }
        else
        {
          write = lame_encode_buffer_interleaved(lame, pcm_buffer, read, mp3_buffer, MP3_SIZE);
        }
      }
      if (write < 0) 
      {
        if (write == -1)
          throw std::string("mp3 buffer is not big enough...");
        else
          throw std::string("mp3 internal error:  error code = " + std::to_string(write) );
      }      
      if( 1 == header.nChan )
      {
        if(handled_elems + read <= rawData.size())
        {
          handled_elems += read;
          pcm_buffer += read;
        }
        else
          throw std::string("Error handled_elems > rawData.size()");         
      }
      else
      {
        if(handled_elems + 2*read <= rawData.size())
        {
          handled_elems += 2*read;
          pcm_buffer += 2*read;
        }
        else
          throw std::string("Error handled_elems > rawData.size()");        
      }
      fwrite(mp3_buffer, write, 1, mp3);
    } while (0 != read);
  }
  catch (std::string s)
  {
    if(nullptr != lame)
      lame_close(lame);
    if(nullptr != mp3)    
      fclose(mp3);
    if(nullptr != pcm)    
      fclose(pcm);
    Rcpp::stop(s);
  }
  
  lame_close(lame);
  fclose(mp3);
  fclose(pcm);
}

// [[Rcpp::export]]
void mp3ToWav(std::string mp3_file_in, std::string wav_file_out)
{
  setlocale(LC_ALL, " ");
  
  //tilda handling
  mp3_file_in = tildaString(mp3_file_in);   
  wav_file_out = tildaString(wav_file_out); 
    
  lame_t lame = nullptr;
  hip_t hip = nullptr;  
  FILE *mp3 = nullptr;
  try
  {
    int id3_tag_size=0;
    
    //calc ID3 tags size
    {
      ID3 tag(mp3_file_in.c_str());
      id3_tag_size=tag.size();
    }    
    mp3 = fopen_speech(mp3_file_in.c_str(), "rb");
    if( nullptr == mp3 )
      throw std::string("Decode: can not open input mp3 file");     
    
    const int PCM_SIZE = 4096;
    const int MP3_SIZE = 4096;
    
    short int pcm_buffer_l[ PCM_SIZE];
    short int pcm_buffer_r[ PCM_SIZE];
    unsigned char mp3_buffer[MP3_SIZE];
    
    lame = lame_init();
    if (nullptr == lame) 
      throw std::string("fatal error during initialization lame");
    
    lame_set_decode_only( lame, 1 );
    
    if( 0 > lame_init_params( lame ) )
      throw std::string("fatal error during initialization parameters of lame");      
    
    hip = hip_decode_init();
    
    bool got_header = false;
    std::vector<short> rawData_sh; 
    sWaveParameters  header; 

    //skip id3 tag
    if(id3_tag_size) 
    {
      unsigned char * id3_buffer = new unsigned char[id3_tag_size];
      int id3_read = 0;
      id3_read =fread(&id3_buffer[0],sizeof(unsigned char), id3_tag_size, mp3);
      delete [] id3_buffer;
    }
    while( true )
    {
      int read = fread(mp3_buffer, sizeof(unsigned char), MP3_SIZE, mp3);    
      if( read == 0 )
        break;
      int num_samples = 0;
      mp3data_struct mp3data;
      while( true )
      {
        if( got_header )
        {
          num_samples = hip_decode1_headers ( hip, mp3_buffer, read,
                                      & pcm_buffer_l[0], & pcm_buffer_r[0], &mp3data );
        }
        else
        {
          num_samples = hip_decode1_headers ( hip, mp3_buffer, read,
                                              & pcm_buffer_l[0], & pcm_buffer_r[0], &mp3data );
          if( 1 == mp3data.header_parsed )
          {
            got_header = true;
            
            header.nChan = mp3data.stereo;
            header.sampleRate = mp3data.samplerate;
            header.audioFormat = 1;
            header.byteOrder = BYTEORDER_LE;
            header.memOrga = MEMORGA_INTERLV;            
            header.nBits = 16;
            header.nBPS = 2;            
            header.blockSize = header.nChan * header.nBPS;
            header.nBlocks = 0;
            
            //now only stereo// [[Rcpp::export]]
            if( 2 == header.nChan )
              throw std::string("Error parsing mp3 file. Unsupported file format: stereo channels");
          }
        }

        if( num_samples == 0 )
        {
          if( read > 0 )  // need more data to continue decoding
          {
            read = 0;
            continue;
          }
          break;
        }           
        
        if( num_samples == -1 )
          throw std::string("Codec::decode(): decoding error");
        
        if( got_header == false )
          throw std::string("Codec::decode(): got samples without header");

        read    = 0;
        
        if(num_samples > 0)
        {
          if( 2 == header.nChan )
          {
            for( int i = 0; i < num_samples; i++)
            {
              rawData_sh.push_back ( pcm_buffer_l[i] ); 
              rawData_sh.push_back ( pcm_buffer_r[i] ); 
            }
            header.nBlocks += num_samples;
          }
          else
          {
            std::vector<short int> pcm_vector (pcm_buffer_l, pcm_buffer_l+num_samples );
            rawData_sh.insert( rawData_sh.end(), pcm_vector.begin(), pcm_vector.end() );
            header.nBlocks += num_samples;
          }
        }
        
      }
      if( ferror(mp3))
        throw std::string("Decode: error during reading mp3 file.");
    }
    
    if( false == got_header )
      throw std::string("Codec::decode(): got samples without header");
  
    CRcppWave::saveToWaveFile_sh_int(wav_file_out, rawData_sh, header);
    
  }
  catch (std::string s)
  {
    if(nullptr != lame)
      lame_close(lame);
    if(nullptr != hip)
      hip_decode_exit(hip);
    if(nullptr != mp3)    
      fclose(mp3);    
    Rcpp::stop(s);
  }
  lame_close(lame);  
  hip_decode_exit(hip);
  fclose(mp3);
}

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
SEXP rcpp_parseWavFile(std::string strWavfile)
{
  setlocale(LC_ALL, " ");
  
  //tilda handling
  strWavfile = tildaString(strWavfile); 
  
  std::vector<int32_t> rawData;
  sWaveParameters header;
  CRcppWave::Errors error = CRcppWave::parseWavFile(strWavfile, header, rawData);  
  if(CRcppWave::NoError == error)
    return Rcpp::List::create(header, rawData);  
  else if(CRcppWave::StereoError == error)
    Rcpp::stop("Error parsing file. Unsupported file format: stereo channels");
  else if(CRcppWave::HeaderParseError == error)
    Rcpp::stop("Error parsing file header");  
  else if(CRcppWave::PcmError == error)
    Rcpp::stop("Error parsing file. Unsupported file format: float data");  
  else if(CRcppWave::FileNotOpenError == error)
    Rcpp::stop("Error parsing file. Can not open file - " + strWavfile);      
  else
    Rcpp::stop("Error parsing file. Unknown error! Please say devs that needing rewrite rcpp_parseWavFile)");    
}
 
   
// [[Rcpp::export]]   
bool rcpp_playWavFile(std::vector<int32_t> rawData, Rcpp::List header)
{
  setlocale(LC_ALL, " ");  
  CRcppWave rcpp_wave;
  return rcpp_wave.playWaveFile(rawData, waveHeader_cPP(header));
}

// [[Rcpp::export]]
SEXP test_rcpp_playWavFile(std::string strWavfile)
{
  setlocale(LC_ALL, " ");
  
  //tilda handling  
  strWavfile = tildaString(strWavfile);
  
  std::vector<int32_t> rawData;
  sWaveParameters header;
  CRcppWave::Errors error = CRcppWave::parseWavFile(strWavfile, header, rawData);  
  if(CRcppWave::StereoError == error)
    Rcpp::stop("Error parsing file. Unsupported file format: stereo channels");
  else if(CRcppWave::PcmError == error)
    Rcpp::stop("Error parsing file. Unsupported file format: float data");  
  else if(CRcppWave::FileNotOpenError == error)
    Rcpp::stop("Error parsing file. Can not open file - " + strWavfile);      
  else if(CRcppWave::HeaderParseError == error)
    Rcpp::stop("Error parsing file header");  
  
  CRcppWave rcpp_wave;
  bool res = rcpp_wave.playWaveFile(rawData, header);
  return Rcpp::List::create(res, header, rawData);  
}

// [[Rcpp::export]]   
void rcpp_writeWavFile(std::string  filePath, std::vector<int32_t> rawData, Rcpp::List header)
{
  setlocale(LC_ALL, " ");
  
  //tilda handling
  filePath = tildaString(filePath);  
  
  return  CRcppWave::saveToWaveFile(filePath, rawData, waveHeader_cPP(header)); 
}

// [[Rcpp::export]]  
void test_rcpp_writeWavFile(std::string  filePathIn, std::string  filePathOut)
{
  setlocale(LC_ALL, " ");
  
  //tilda handling
  filePathIn = tildaString(filePathIn);
  filePathOut = tildaString(filePathOut);    
  
  std::vector<int32_t> rawData;
  sWaveParameters header;
  CRcppWave::Errors error = CRcppWave::parseWavFile(filePathIn, header, rawData);
  header.nChan = 1;
  header.nBPS = 4;
  header.nBits = 32;
  header.sampleRate = 44100;
  header.blockSize = header.nChan * header.nBPS;
  if(CRcppWave::StereoError == error)
    Rcpp::stop("Error parsing file. Unsupported file format: stereo channels");
  else if(CRcppWave::PcmError == error)
    Rcpp::stop("Error parsing file. Unsupported file format: float data");  
  else if(CRcppWave::FileNotOpenError == error)
    Rcpp::stop("Error parsing file. Can not open file - " + filePathIn);      
  else if(CRcppWave::HeaderParseError == error)
    Rcpp::stop("Error parsing file header");  
  
  return   CRcppWave::saveToWaveFile(filePathOut, rawData, header);
}

// [[Rcpp::export]]
SEXP rcpp_openSmileGetFeatures(std::vector<std::string> audio_files_in, 
                          std::string config_string_in)
{
  setlocale(LC_ALL, " ");
  Rcout << "rcpp_openSmileGetFeatures start" << std::endl;
  Rcout << "size = " << audio_files_in.size() << std::endl;
  //tilda handling  
  for(int i=0; i<audio_files_in.size(); i++)
  {
    audio_files_in[i] = tildaString(audio_files_in[i]);  
  }
  
  Rcpp::List result;
  try { 
    CRcppWave rcppWave;      
    if(rcppWave.setInputData(audio_files_in, config_string_in));
    {
      Rcout << "rcpp_openSmileGetFeatures if" << std::endl;      
      rcppWave.work();
      std::vector <arma::mat> rcpp_audio_features;
      std::vector <arma::rowvec> rcpp_audio_timestamps;
      std::vector <sWaveParameters> rcpp_wave_header;     
      rcppWave.getOutputData( rcpp_audio_features,
                              rcpp_audio_timestamps, 
                              rcpp_wave_header);
      Rcout << "rcpp_openSmileGetFeatures size = " << rcpp_audio_features.size() << std::endl;
      Rcout << "rcpp_audio_timestamps size = " << rcpp_audio_timestamps.size() << std::endl;
      Rcout << "rcpp_wave_header size = " << rcpp_wave_header.size() << std::endl;        
      for(int i=0; i<rcpp_audio_features.size(); i++)
      {
        Rcout << "rcpp_openSmileGetFeatures audio_timestamps = " << std::endl;          
        {
          std::string name = "audio_timestamps_" + std::to_string(i);
          result[name.c_str()] =  rcpp_audio_timestamps[i];
        }
        Rcout << "rcpp_openSmileGetFeatures wave_header = " << std::endl;          
        {
          std::string name = "wave_header_" + std::to_string(i);
          result[name.c_str()] =  rcpp_wave_header[i];
        }
        Rcout << "rcpp_openSmileGetFeatures audio_features rows = " << rcpp_audio_features[i].n_rows 
              << " columns = " << rcpp_audio_features[i].n_cols << std::endl;          
        {
                std::string name = "audio_features_" + std::to_string(i);
                result[name.c_str()] =  rcpp_audio_features[i];
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
  setlocale(LC_ALL, " ");  

  //tilda handling  
  config_file_in = tildaString(config_file_in); 
  for(int i=0; i<audio_files_in.size(); i++)
  {
    audio_files_in[i] = tildaString(audio_files_in[i]);  
  }
  
  
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
  setlocale(LC_ALL, " ");
  
  //tilda handling    
  for(int i=0; i<audio_files_in.size(); i++)
  {
    audio_files_in[i] = tildaString(audio_files_in[i]);  
  }  
  
  Rcpp::List result;
  try { 
    CRcppWave rcppWave;
    if(rcppWave.setInputData(audio_files_in, config_string_in));
    {
      rcppWave.work();
      std::vector <arma::rowvec> rcpp_border_frame_starts;
      std::vector <arma::rowvec> rcpp_border_frame_ends;     
      rcppWave.getBorderFrames( rcpp_border_frame_starts,
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
  setlocale(LC_ALL, " ");
  
  //tilda handling    
  config_file_in = tildaString(config_file_in); 
  for(int i=0; i<audio_files_in.size(); i++)
  {
    audio_files_in[i] = tildaString(audio_files_in[i]);  
  }  
  
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
  setlocale(LC_ALL, " ");  
  CRcppDataBase rccpDataBase;
  return Rcpp::wrap(rccpDataBase.work1file(arguments));
}

// [[Rcpp::export]]
SEXP rcpp_openSmileGetFeatures_Turns(std::vector<std::string> audio_files_in, 
                                     std::string config_string_in)
{
  setlocale(LC_ALL, " ");
  
  //tilda handling    
  for(int i=0; i<audio_files_in.size(); i++)
  {
    audio_files_in[i] = tildaString(audio_files_in[i]);  
  }  
  
  Rcpp::List features = rcpp_openSmileGetFeatures(audio_files_in, config_string_in);
  Rcpp::List turns = rcpp_openSmileGetBorderFrames(audio_files_in, config_string_in);
  return Rcpp::List::create(features, turns);
}

// [[Rcpp::export]]
SEXP test_rcpp_openSmileGetFeatures_Turns(std::vector<std::string> audio_files_in, 
                                        std::string config_file_in)
{
  setlocale(LC_ALL, " ");  
  
  //tilda handling    
  config_file_in = tildaString(config_file_in); 
  for(int i=0; i<audio_files_in.size(); i++)
  {
    audio_files_in[i] = tildaString(audio_files_in[i]);  
  }  
  
  Rcpp::List features = test_rcpp_openSmileGetFeatures(audio_files_in, config_file_in);
  Rcpp::List turns = test_rcpp_openSmileGetBorderFrames(audio_files_in, config_file_in);
  return Rcpp::List::create(features, turns);  
}