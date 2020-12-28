#include <R.h>
#include <Rdefines.h>

#include <smileutil/smileUtil_cpp.h>
#include <string.h>

//need refactoring!!!
struct chunk
{
  char        id[4];
  uint32_t     size;
};

struct RIFFHeader
{
  chunk       descriptor;
  char        type[4];
};

struct WAVEHeader
{
  uint16_t     audioFormat;
  uint16_t     numChannels;
  uint32_t     sampleRate;
  uint32_t     byteRate;
  uint16_t     blockAlign;
  uint16_t     bitsPerSample;
};

template <typename T>
T swap_endian(T u)
{
  union
{
  T u;
  unsigned char u8[sizeof(T)];
} source, dest;
  
  source.u = u;
  
  for (size_t k = 0; k < sizeof(T); k++)
    dest.u8[k] = source.u8[sizeof(T) - k - 1];
  
  return dest.u;
}


// filename is optional and can be NULL ! It is used only for log messages.
int smilePcm_readWaveHeader(FILE *filehandle, sWaveParameters *pcmParam, const char *filename)
{
  if (nullptr == filename) 
    filename = "unknown";
  
  if ( (nullptr != filehandle) && (nullptr != pcmParam) ) 
  {
    RIFFHeader riff;
    WAVEHeader wave;
    bool bigEndian = false;
    int nRead;
    fseek(filehandle, 0, SEEK_SET);
    nRead = (int)fread(&riff, 1, sizeof(RIFFHeader), filehandle);
    if (nRead != sizeof(RIFFHeader)) 
    {
      Rprintf("smilePcm: Error reading %i bytes (header) from beginning of wave file '%s'! File too short??",sizeof(riff),filename);
      return 0;
    }
    if (((0 != strncmp(riff.descriptor.id, "RIFF", 4)) && (0 != strncmp(riff.descriptor.id, "RIFX", 4)))
          || (0 != strncmp(riff.type, "WAVE", 4)))  
    {
      Rprintf("smilePcm: bogus wave/riff header or file in wrong format ('%s')!",filename);
      return 0;
    } else 
    {
      if (0 == strncmp(riff.descriptor.id, "RIFX", 4))
        bigEndian = true;
      else
        bigEndian = false;
    }    
    bool fmtHandled = false;
    while(1)
    {
      chunk sub_chunk;
      nRead = (int)fread(&sub_chunk, 1, sizeof(chunk), filehandle);
      if (nRead != sizeof(chunk)) 
      {
        Rprintf("smilePcm: Error reading subchunk in wave file '%s'! File too short??", filename);
        return 0;
      }
      if ( 0 == strncmp(sub_chunk.id, "fmt ", 4) )
      {
        nRead = (int)fread(reinterpret_cast<char *>(&wave), 1, sizeof(WAVEHeader), filehandle);
        if (nRead != sizeof(WAVEHeader)) 
        {
          Rprintf("smilePcm: Error reading wave header in wave file '%s'! File too short??",filename);
          return 0;
        }
        if (bigEndian) 
        {
          wave.audioFormat = swap_endian<uint16_t>(wave.audioFormat);
        }
        if (0 != wave.audioFormat && 
            1 != wave.audioFormat) 
        {
          Rprintf("smilePcm: Error reading wave header, not supported audio format = %d. Filename - '%s'!",wave.audioFormat, filename);
          return 0;
        } 
        else 
        {
          if(0 == wave.numChannels)
          {
            Rprintf("smilePcm: Error reading wave header, number of channels = 0. Filename - '%s'!",filename);
            return 0;            
          }
          pcmParam->memOrga = MEMORGA_INTERLV;
          if (bigEndian) 
          {
            pcmParam->sampleRate = swap_endian<uint32_t>(wave.sampleRate);
            pcmParam->nChan = swap_endian<uint16_t>(wave.numChannels);
            pcmParam->nBPS = swap_endian<uint16_t>(wave.blockAlign)/swap_endian<uint16_t>(wave.numChannels);
            pcmParam->nBits = swap_endian<uint16_t>(wave.bitsPerSample);
            pcmParam->audioFormat = swap_endian<uint16_t>(wave.audioFormat);
            pcmParam->blockSize = swap_endian<uint16_t>(wave.blockAlign);
            pcmParam->byteOrder = BYTEORDER_BE;          
          }
          else
          {
            pcmParam->sampleRate = wave.sampleRate;
            pcmParam->nChan = wave.numChannels;
            pcmParam->nBPS = wave.blockAlign/wave.numChannels;
            pcmParam->nBits = wave.bitsPerSample;
            pcmParam->audioFormat = wave.audioFormat;
            pcmParam->blockSize = wave.blockAlign;
            pcmParam->byteOrder = BYTEORDER_LE;               
          }
        }
        fmtHandled = true;
      }
      else if ( 0 == strncmp(sub_chunk.id, "data", 4) )
      {
        if(!fmtHandled)
        {
          Rprintf("smilePcm: Error reading wave header, subchunk \"data\" before subchunk \"fmt\". Filename - '%s'!",filename);
          return 0;          
        }
        if(bigEndian) 
        {
          if(0 == swap_endian<uint16_t>(wave.blockAlign))
          {
            Rprintf("smilePcm: Error reading wave header, block align = 0. Filename - '%s'!",filename);
            return 0;              
          }
          if (sub_chunk.size < 99999) {
            char * tmp = (char*)malloc(sub_chunk.size);
            nRead = (int)fread(tmp, 1, sub_chunk.size, filehandle);
            free(tmp);            
            if (nRead != sub_chunk.size) {
              Rprintf("smilePcm: less bytes read (%i) from wave file '%s' than indicated by Subchunk2Size (%i)! File seems broken!\n", nRead, filename, sub_chunk.size);
              return 0;
            }
          } else {
            Rprintf("smilePcm: Subchunk2Size > 99999. This seems to be a bogus file!\n");
            return 0;
          }          
          pcmParam->nBlocks = swap_endian<uint16_t>(sub_chunk.size) / swap_endian<uint16_t>(wave.blockAlign);
        }
        else
        {
          if(0 == wave.blockAlign)
          {
            Rprintf("smilePcm: Error reading wave header, block align = 0. Filename - '%s'!",filename);
            return 0;              
          }
          pcmParam->nBlocks = sub_chunk.size / wave.blockAlign;          
        }
        pcmParam->headerOffset = ftell(filehandle);        
        break;
      }
      else
      {
        char * tmp = (char*)malloc(sub_chunk.size);
        nRead = (int)fread(tmp, 1, sub_chunk.size, filehandle);
        free(tmp);        
        if (nRead != sub_chunk.size) {
          Rprintf("smilePcm: less bytes read (%i) from wave file '%s' than indicated by Subchunk Size (%i)! File seems broken!\n", nRead, filename, sub_chunk.size);
          return 0;
        }       
      }
    }
    
    return 1;
#if 0    
    int nRead;
    sRiffPcmWaveHeader head;
    sRiffChunkHeader chunkhead;
    int safetytimeout = 20;  // max <safetytimeout> chunks of 99kb size before 'data' chunk
    
    fseek(filehandle, 0, SEEK_SET);
    nRead = (int)fread(&head, 1, sizeof(head), filehandle);
    if (nRead != sizeof(head)) {
      Rprintf("smilePcm: Error reading %i bytes (header) from beginning of wave file '%s'! File too short??",sizeof(head),filename);
      return 0;
    }
    
    /* Check for valid header , TODO: support other endianness */
    if ((head.Riff != 0x46464952) ||
    (head.Format != 0x45564157) ||
    (head.Subchunk1ID != 0x20746D66) ||
    //      (head.Subchunk2ID != 0x61746164) ||
    (head.AudioFormat != 1) ||   // 32-bit: 0xfffe
    (head.Subchunk1Size != 16)) {  // 32-bit: 28
      Rprintf("smilePcm:  Riff: %x\n  Format: %x\n  Subchunk1ID: %x\n  Subchunk2ID: %x\n  AudioFormat: %x\n  Subchunk1Size: %x",
              head.Riff, head.Format, head.Subchunk1ID, head.Subchunk2ID, head.AudioFormat, head.Subchunk1Size);
      Rprintf("smilePcm: bogus wave/riff header or file in wrong format ('%s')! (maybe you are trying to read a 32-bit wave file which is not yet supported (new header type...)?)",filename);
      return 0;
    }
    
    while ((head.Subchunk2ID != 0x61746164)&&(safetytimeout>0)) { // 0x61746164 = 'data'
      // keep searching for 'data' chunk:
      if (head.Subchunk2Size < 99999) {
        char * tmp = (char*)malloc(head.Subchunk2Size);
        nRead = (int)fread(tmp, 1, head.Subchunk2Size, filehandle);
        if (nRead != head.Subchunk2Size) {
          Rprintf("smilePcm: less bytes read (%i) from wave file '%s' than indicated by Subchunk2Size (%i)! File seems broken!\n",nRead,filename,head.Subchunk2Size);
          return 0;
        }
        free(tmp);
      } else {
        Rprintf("smilePcm: Subchunk2Size > 99999. This seems to be a bogus file!\n");
        return 0;
      }
      nRead = (int)fread(&chunkhead, 1, sizeof(chunkhead), filehandle);
      if (nRead != sizeof(chunkhead)) {
        Rprintf("smilePcm: less bytes read (%i) from wave file '%s' than there should be (%i) while reading sub-chunk header! File seems broken!\n",nRead,filename,sizeof(chunkhead));
        return 0;
      }
      head.Subchunk2ID = chunkhead.SubchunkID;
      head.Subchunk2Size = chunkhead.SubchunkSize;
      safetytimeout--;
    }
    if (safetytimeout <= 0) {
      Rprintf("smilePcm: No 'data' subchunk found in wave-file among the first %i chunks! corrupt file?\n",safetytimeout);
      return 0;
    }
    
    pcmParam->sampleRate = head.SampleRate;
    pcmParam->nChan = head.NumChannels;
    pcmParam->nBPS = head.BlockAlign/head.NumChannels;
    pcmParam->nBits = head.BitsPerSample;
    pcmParam->audioFormat = head.AudioFormat;
    //    p->bits = head.BitsPerSample;
    //    SMILE_DBG(5,"bits per sample = %i",head.BitsPerSample);
    //    pcmParam.sampleType = pcmBitsToSampleType( head.BitsPerSample, BYTEORDER_LE, 0 );
    /*
     if (head.NumChannels * head.BitsPerSample / 8 != head.BlockAlign) {
     FEATUM_ERR_FATAL(0,"Error reading wave file header: head.BlockAlign != head.NumChannels * head.BitsPerSample / 8");
     return 0;
     }
     */
    pcmParam->nBlocks = head.Subchunk2Size / head.BlockAlign;
    pcmParam->blockSize = head.BlockAlign;
    pcmParam->byteOrder = BYTEORDER_LE;
    pcmParam->memOrga = MEMORGA_INTERLV;
    pcmParam->headerOffset = ftell(filehandle);
    return 1;
#endif  
  }
  Rprintf("filehandle or pcmParam nullptr");
  return 0;
}
