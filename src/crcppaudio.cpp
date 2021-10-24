#include "crcppaudio.h"

#include <smileutil/smileUtil_cpp.h>

#include "lame.h"
#include "id3.h"
#include "utils_global.h"

#include <cstdio>
#include <algorithm>

#include <Rcpp.h>
using namespace Rcpp;

bool CRcppAudio::setInputData ( speech::filepath_vector audio_files_in, 
                                std::string config_string_in)
{
  formatAudioFiles.clear();
  speech::filepath_vector wav_files;
  
  for(int i=0; i<audio_files_in.size(); i++)
  {
    if( isWavFile(audio_files_in[i]) )
    {
      formatAudioFiles.push_back(CRcppAudio::wav);
      wav_files.push_back(audio_files_in[i]);
    }
    if( isMp3File(audio_files_in[i]) )
    {
      formatAudioFiles.push_back(CRcppAudio::mp3);
      wav_files.push_back(audio_files_in[i]);
    }
  }
  return CRcppWave::setInputData(wav_files, config_string_in);
}

CRcppWave::Errors CRcppAudio::parseAudioFile( const speech::filepath & strFile, sWaveParameters & header, 
                                              std::vector<int32_t> & rawDataL , std::vector<int32_t> & rawDataR)
{
  if( isMp3File(strFile) )
  {
    mp3ToWav(strFile, header, rawDataL, rawDataR);
    return CRcppWave::NoError;
  }
  
  if( isWavFile(strFile) )
    return parseWavFile(strFile, header, rawDataL, rawDataR); 

  return CRcppWave::UnknownFormatError;   
}

void CRcppAudio::work()
{
  for(int iFile = 0; iFile < audio_files.size(); iFile++)
  {
    std::string fileName;
    if(isWavFile(audio_files[iFile]))
      fileName = audio_files[iFile];
    if (isMp3File(audio_files[iFile]))
    {
#ifdef __APPLE__
      char filename_[] = "/tmp/fileXXXXXX";
      int fd = mkstemp(filename_);
      if (fd != -1)
        close(fd);
      fileName = filename_; 
#else      
      fileName = std::tmpnam(nullptr);
      if (!fileName.empty ()) {
        if (fileName.at (0) == '\\')
          fileName.erase (0,1);
      }
#endif      
      mp3ToWav(audio_files[iFile], fileName);
    }
    std::vector<std::string> arguments;
    arguments.push_back(std::string("-I"));
    arguments.push_back(fileName);
    arguments.push_back(std::string("-C")); 
    arguments.push_back(config_file);
    work1file(arguments);
    
    if (isMp3File(audio_files[iFile]))
      std::remove(fileName.c_str());
  }  
}


//if stereo rawData interleaved rawData
//if mono simple raw data
void CRcppAudio::wavToMp3(const sWaveParameters & header, 
                          const std::vector<int32_t> & rawData,
                          speech::filepath mp3_file_out)
{
  FILE *pcm = nullptr;
  FILE *mp3 = nullptr;
  lame_t lame = nullptr;
  try
  {
    //will work only in case int == int32_t
    if(sizeof(int) != sizeof(int32_t))
      throw std::string("wavToMp3 not support your platform: sizeof(int) != sizeof(int32_t)");
    
    lame = lame_init();
    if (nullptr == lame) 
      throw std::string("fatal error during initialization lame");  
    
    //int read, write;  
    int write = 0;  
    
    const std::vector<int>::size_type PCM_SIZE = 8192;
    const int MP3_SIZE = 8192;
    
    unsigned char mp3_buffer[MP3_SIZE];
    
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
    
    std::vector<int>::size_type handled_elems = 0;
    std::vector<int>::size_type numElems = 0;
    
    mp3 = speech::fopen(mp3_file_out.c_str(), "wb");
    if( nullptr == mp3 )
      throw std::string("can not open output mp3 file");     
    
    int * pcm_buffer = const_cast<int *>(rawData.data());
    
    do {
      auto sz = 2*sizeof(int);
      numElems = std::min(PCM_SIZE, (rawData.size() - handled_elems)/2);
      if( 1 == header.nChan )
      {
        sz = sizeof(int);
        numElems = std::min(PCM_SIZE, rawData.size() - handled_elems);
      }
      if (0 == numElems)
        write = lame_encode_flush(lame, mp3_buffer, MP3_SIZE);
      else
      {
        if( 1 == header.nChan )
        {
          write = lame_encode_buffer_int(lame, pcm_buffer, nullptr, numElems, mp3_buffer, MP3_SIZE);
        }
        else
        {
          write = lame_encode_buffer_interleaved_int(lame, pcm_buffer, numElems, mp3_buffer, MP3_SIZE);
        }
      }
      if ( 0 > write) 
      {
        if (-1 == write)
          throw std::string("mp3 buffer is not big enough...");
        else
          throw std::string("mp3 internal error:  error code = " + std::to_string(write) ); 
      }      
      if( 1 == header.nChan )
      {
        if(handled_elems + numElems <= rawData.size())
        {
          handled_elems += numElems;
          pcm_buffer += numElems;
        }
        else
          throw std::string("Error handled_elems > rawData.size()");         
      }
      else
      {
        if(handled_elems + 2*numElems <= rawData.size())
        {
          handled_elems += 2*numElems;
          pcm_buffer += 2*numElems;
        }
        else
          throw std::string("Error handled_elems > rawData.size()");        
      }
      fwrite(mp3_buffer, write, 1, mp3);
      
    } while (0 != numElems);
  }
  catch (std::string s)
  {
    if(nullptr != lame)
      lame_close(lame);
    if(nullptr != mp3)    
      fclose(mp3);
    std::remove(mp3_file_out.c_str());    
    Rcpp::stop(s);
  }
  
  if(nullptr != lame)
    lame_close(lame);
  if(nullptr != mp3)    
    fclose(mp3);
}

void CRcppAudio::mp3ToWav(speech::filepath mp3_file_in, speech::filepath wav_file_out)
{
  if(!mp3_file_in.empty())
  {
    sWaveParameters header;
    std::vector<int32_t> rawDataL; 
    std::vector<int32_t> rawDataR;
    return mp3ToWav(mp3_file_in, wav_file_out, 
                    header,
                    rawDataL, rawDataR);
  }
  else
    throw std::string("mp3ToWav: mp3 file is empty");  
}

void CRcppAudio::mp3ToWav(speech::filepath mp3_file_in, sWaveParameters & header, 
                          std::vector<int32_t> & rawDataL , std::vector<int32_t> & rawDataR)
{
  Rcout << "CRcppAudio::mp3ToWav 1" << std::endl;  
  return mp3ToWav(mp3_file_in, std::string(), 
                  header,
                  rawDataL, rawDataR);  
}

//if wav_file_out empty fill in header, rawDataL, rawDataR
//else write to wav_file_out
void CRcppAudio::mp3ToWav(speech::filepath mp3_file_in, speech::filepath wav_file_out, 
                          sWaveParameters & header, 
                          std::vector<int32_t> & rawDataL , std::vector<int32_t> & rawDataR)
{
  Rcout << "CRcppAudio::mp3ToWav 2" << std::endl;    
  lame_t lame = nullptr;
  hip_t hip = nullptr;  
  FILE *mp3 = nullptr;
  bool bWavFile = true;
  if(wav_file_out.empty())
    bWavFile = false;  
  try
  {
    int id3_tag_size=0;
    
    //calc ID3 tags size
    {
      ID3 tag(mp3_file_in.c_str());
      id3_tag_size=tag.size();
    }    
    mp3 = speech::fopen(mp3_file_in.c_str(), "rb");
    if( nullptr == mp3 )
      throw std::string("Decode: can not open input mp3 file");     
    
    const int PCM_SIZE = 4096;
    const int MP3_SIZE = 4096;
    
    short int pcm_buffer_l[ PCM_SIZE];
    short int pcm_buffer_r[ PCM_SIZE];
    unsigned char mp3_buffer[MP3_SIZE];
    
    lame = lame_init();
    if (nullptr == lame) 
      throw std::string("Decode: fatal error during initialization lame");
    
    lame_set_decode_only( lame, 1 );
    
    if( 0 > lame_init_params( lame ) )
      throw std::string("Decode: fatal error during initialization parameters of lame");      
    
    hip = hip_decode_init();
    
    bool got_header = false;
    std::vector<short> rawData_sh; 
    
    //skip id3 tag
    if(id3_tag_size) 
    {
      unsigned char id3_buffer[id3_tag_size];
      int id3_read = 0;
      id3_read =fread(&id3_buffer[0],sizeof(unsigned char), id3_tag_size, mp3);
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
            
            if( 2 == header.nChan && !bWavFile)
              rawDataR = rawDataL;  
            
          }
        }
        
        if( 0 == num_samples)
        {
          if( read > 0 )  // need more data to continue decoding
          {
            read = 0;
            continue;
          }
          break;
        }             
        
        if( -1 == num_samples )
          throw std::string("Decode: decoding error");
        
        if( false == got_header )
          throw std::string("Decode: got samples without header");
        
        read    = 0;
        
        if(num_samples > 0)
        {
          if( 2 == header.nChan )
          {
            if(bWavFile)
            {
              for( int i = 0; i < num_samples; i++)
              {
                rawData_sh.push_back ( pcm_buffer_l[i] ); 
                rawData_sh.push_back ( pcm_buffer_r[i] ); 
              }
            }
            else
            {
              for( int i = 0; i < num_samples; i++)
              {
                rawDataL.push_back ( pcm_buffer_l[i] * (int32_max/SHRT_MAX)); 
                rawDataR.push_back ( pcm_buffer_r[i] * (int32_max/SHRT_MAX)); 
              }              
            }
            header.nBlocks += num_samples;
          }
          else
          {
            if(bWavFile)
            {
              std::vector<short int> pcm_vector (pcm_buffer_l, pcm_buffer_l+num_samples );              
              rawData_sh.insert( rawData_sh.end(), pcm_vector.begin(), pcm_vector.end() );
            }
            else
            {
              for( int i = 0; i < num_samples; i++)
              {
                rawDataL.push_back ( pcm_buffer_l[i] * (int32_max/SHRT_MAX)); 
              }                
            }
            header.nBlocks += num_samples;
          }
        }
        
      }
      if( ferror(mp3))
        throw std::string("Decode: error during reading mp3 file.");
    }
    
    if( false == got_header )
      throw std::string("Decode: got samples without header");
    
    if(bWavFile)
      CRcppWave::saveToWaveFile(header, rawData_sh, wav_file_out);
    
  }
  catch (std::string s)
  {
    if(nullptr != lame)
      lame_close(lame);
    if(nullptr != hip)
      hip_decode_exit(hip);
    if(nullptr != mp3)    
      fclose(mp3);
    std::remove(wav_file_out.c_str());
    Rcpp::stop(s);
  }
  if(nullptr != lame)
    lame_close(lame);
  if(nullptr != hip)
    hip_decode_exit(hip);
  if(nullptr != mp3)    
    fclose(mp3); 
}

bool CRcppAudio::isWavFile ( speech::filepath file_in )
{
  bool res = false;
  if( speech::str_ends_with(speech::str_tolower(file_in), "wav")
       ||
      speech::str_ends_with(speech::str_tolower(file_in), "wave"))
    res = true;
  else
    res = false;
  return res;
}

bool CRcppAudio::isMp3File ( speech::filepath file_in )
{
  if(speech::str_ends_with(speech::str_tolower(file_in), "mp3") )
    return true;
  else
    return false;
}

//to stereo and mono
//in mono case rawData - simple data
//in stereo case rawData - interleaved data
void CRcppAudio::saveToAudioFile (const sWaveParameters & header,                              
                                  const std::vector<int32_t> & rawData,
                                  const speech::filepath & filePath)
{
  if( isMp3File(filePath) )
    return wavToMp3(header, rawData, filePath);
  
  if( isWavFile(filePath) )
    return saveToWaveFile(header, rawData, filePath); 
  
  throw std::string("saveToAudioFile: unknown format");     
}

//only to stereo: rawDataL - left channel, rawDataR - right channel
void CRcppAudio::saveToAudioFileStereo (  const sWaveParameters & header,                              
                                          const std::vector<int32_t> & rawDataL,
                                          const std::vector<int32_t> & rawDataR,
                                          const speech::filepath & filePath)
{
  if( 1 == header.nChan )
    throw std::string("saveToWaveFileStereo: try call function with mono file");
  std::vector<int32_t> rawData (static_cast<std::vector<int32_t>::size_type>(rawDataL.size() * 2));
  for(int i=0; i<rawDataL.size(); i++)
  {
    rawData[i] = rawDataL[i];
    rawData[i] = rawDataR[i];    
  }
  return saveToAudioFile(header, rawData, filePath);  
}