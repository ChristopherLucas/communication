/*F***************************************************************************
 * 
 * openSMILE - the Munich open source Multimedia Interpretation by 
 * Large-scale Extraction toolkit
 * 
 * This file is part of openSMILE.
 * 
 * openSMILE is copyright (c) by audEERING GmbH. All rights reserved.
 * 
 * See file "COPYING" for details on usage rights and licensing terms.
 * By using, copying, editing, compiling, modifying, reading, etc. this
 * file, you agree to the licensing terms in the file COPYING.
 * If you do not agree to the licensing terms,
 * you must immediately destroy all copies of this file.
 * 
 * THIS SOFTWARE COMES "AS IS", WITH NO WARRANTIES. THIS MEANS NO EXPRESS,
 * IMPLIED OR STATUTORY WARRANTY, INCLUDING WITHOUT LIMITATION, WARRANTIES OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE, ANY WARRANTY AGAINST
 * INTERFERENCE WITH YOUR ENJOYMENT OF THE SOFTWARE OR ANY WARRANTY OF TITLE
 * OR NON-INFRINGEMENT. THERE IS NO WARRANTY THAT THIS SOFTWARE WILL FULFILL
 * ANY OF YOUR PARTICULAR PURPOSES OR NEEDS. ALSO, YOU MUST PASS THIS
 * DISCLAIMER ON WHENEVER YOU DISTRIBUTE THE SOFTWARE OR DERIVATIVE WORKS.
 * NEITHER TUM NOR ANY CONTRIBUTOR TO THE SOFTWARE WILL BE LIABLE FOR ANY
 * DAMAGES RELATED TO THE SOFTWARE OR THIS LICENSE AGREEMENT, INCLUDING
 * DIRECT, INDIRECT, SPECIAL, CONSEQUENTIAL OR INCIDENTAL DAMAGES, TO THE
 * MAXIMUM EXTENT THE LAW PERMITS, NO MATTER WHAT LEGAL THEORY IT IS BASED ON.
 * ALSO, YOU MUST PASS THIS LIMITATION OF LIABILITY ON WHENEVER YOU DISTRIBUTE
 * THE SOFTWARE OR DERIVATIVE WORKS.
 * 
 * Main authors: Florian Eyben, Felix Weninger, 
 * 	      Martin Woellmer, Bjoern Schuller
 * 
 * Copyright (c) 2008-2013, 
 *   Institute for Human-Machine Communication,
 *   Technische Universitaet Muenchen, Germany
 * 
 * Copyright (c) 2013-2015, 
 *   audEERING UG (haftungsbeschraenkt),
 *   Gilching, Germany
 * 
 * Copyright (c) 2016,	 
 *   audEERING GmbH,
 *   Gilching Germany
 ***************************************************************************E*/


/*  openSMILE component:

rnnVad

Although this component is called rnnVad, it does not contain nor use an RNN.
It reads the RNN output activation, and applies some thresholds and logic to smooth it to a
stable VAD decision. For the SEMAINE system it supports VAD supression, when
then agent is talking.

*/


#include <R.h>
#include <Rdefines.h>
#include <rnn/rnnVad2.hpp>

#define MODULE "cRnnVad2"


SMILECOMPONENT_STATICS(cRnnVad2)

SMILECOMPONENT_REGCOMP(cRnnVad2)
{
  SMILECOMPONENT_REGCOMP_INIT
  scname = COMPONENT_NAME_CRNNVAD2;
  sdescription = COMPONENT_DESCRIPTION_CRNNVAD2;

  // we inherit cVectorProcessor configType and extend it:
  SMILECOMPONENT_INHERIT_CONFIGTYPE("cDataProcessor")

  // if the inherited config type was found, we register our configuration variables
  SMILECOMPONENT_IFNOTREGAGAIN( {} // <- this is only to avoid compiler warnings...
    // name append has a special role: it is defined in cDataProcessor, and can be overwritten here:
    //ct->setField("nameAppend",nullptr,"processed");
  ct->setField("voiceIdx","The index of the field which contains the 'voice' class output activation. (0 is the first field)",0);
  ct->setField("agentIdx","The index of the field which contains the 'agent/alien' class output activation. (0 is the first field)",1);
    ct->setField("voiceThresh","The threshold to apply to the 'voice' output activation.",0.4);
    ct->setField("agentThresh","The threshold to apply to the 'agent' output activation.",0.3);
    ct->setField("energyIdx","The index of the field which contains the energy/loudness/intensity/etc. value (set to -1 to disable)",2);
    ct->setField("f0Idx","Index of F0 input field (set to -1 to disable)",3);
//    ct->setField("nPost","Number of silence frames after speech before silence is detected.",10);
    // agent block by energy...
    //ct->setField("agentBlockTime","initial user speech time during which to block agent in frames (this must be high enough in order to have robust enough models)",1000);
    ct->setField("agentTurnPastBlock","time the VAD will be blocked after receiving an agent speech end message (in frames, usually 100fps) (use 20 for the SEMAINE speech2speech system, and 60 for the speech2face system).",20);
  	ct->setField("alwaysRejectAgent", "1 = never detect a speaker turn while the agent is speaking",0);
    ct->setField("smartRejectAgent", "1 = apply different VAD strategy while agent is speaking",1);

    ct->setField("userEavgHold","Hold time for user energy envelope and average computation (10ms frames as unit).",500);  // 600
    ct->setField("userEavgDecay","Decay (linear) time for user energy envelope and average computation (10ms frames as unit).",500);  // 300

    ct->setField("agentEavgHold","Hold time for user energy envelope and average computation (10ms frames as unit).",200);
    ct->setField("agentEavgDecay","Decay (linear) time for user energy envelope and average computation (10ms frames as unit).",200); // 100

    ct->setField("vadDebug","1 = output energy and VAD statistics for debugging (set to 2 to always force vad output value to 0 while debugging).",0);
    ct->setField("allowEoverride","1 = allow VAD output even if LSTM does not detect voice when the energy is in the range of the user's current energy envelope (NOTE: this reduces noise robustness, e.g. when moving a headset etc.)",1);

    /*
agent reject based on mean energy:
learning:
  mean energy of user & agent
productive:
  user speech turn only detected when energy >> mean agent energy ??
*/
  )

  // The configType gets automatically registered with the config manger by the SMILECOMPONENT_IFNOTREGAGAIN macro
  
  // we now create out sComponentInfo, including name, description, success status, etc. and return that
  SMILECOMPONENT_MAKEINFO(cRnnVad2);
}

SMILECOMPONENT_CREATE(cRnnVad2)


//-----


cRnnVad2::cRnnVad2(const char *_name) :
  cDataProcessor(_name), voiceThresh(0.0), frameO(nullptr),
  doReset(0), agentTurn(0), userPresence(0), agentTurnCntdn(0),
  eUser(nullptr), eCurrent(nullptr), eAgent(nullptr), eBg(nullptr), cnt(0)
{

}

void cRnnVad2::fetchConfig()
{
  cDataProcessor::fetchConfig();
 
  voiceIdx = getInt("voiceIdx");
  SMILE_IDBG(2,"voiceIdx = %i",voiceIdx);

  agentIdx = getInt("agentIdx");
  SMILE_IDBG(2,"agentIdx = %i",agentIdx);

  energyIdx = getInt("energyIdx");
  SMILE_IDBG(2,"energyIdx = %i",energyIdx);

  f0Idx = getInt("f0Idx");
  SMILE_IDBG(2,"f0Idx = %i",f0Idx);

//  nPost = getInt("nPost");

  voiceThresh = (FLOAT_DMEM)getDouble("voiceThresh");
  agentThresh = (FLOAT_DMEM)getDouble("agentThresh");

  //agentBlockTime = getInt("agentBlockTime");
  agentTurnPastBlock = getInt("agentTurnPastBlock");
  smartRejectAgent = getInt("smartRejectAgent");
  alwaysRejectAgent = getInt("alwaysRejectAgent");

  allowEoverride = getInt("allowEoverride");
  vadDebug = getInt("vadDebug");

  int userEavgHold = getInt("userEavgHold");
  int userEavgDecay = getInt("userEavgDecay");
  int agentEavgHold = getInt("agentEavgHold");
  int agentEavgDecay = getInt("agentEavgDecay");

  

  eCurrent = new cEavgHold(20, 10); /* short-term envelope and average */
  eUser = new cEavgHold(userEavgHold, userEavgDecay); /* user energy level, long term */
  eAgent = new cEavgHold(agentEavgHold, agentEavgDecay); /* user energy level, long term */
  eBg = new cEavgHold(1000, 1000);
}



int cRnnVad2::setupNewNames(long nEl)
{
  writer_->addField("voiceAct");
	namesAreSet_=1;
  return 1;
}



int cRnnVad2::myFinaliseInstance()
{
  int ret = cDataProcessor::myFinaliseInstance();

  if (ret) {
    frameO = new cVector(1);
  }

  return ret;
}


int cRnnVad2::processComponentMessage( cComponentMessage *_msg )
{
	if (isMessageType(_msg,"semaineCallback")) {
		// determine origin by message's user-defined name, which can be set in the config file
		SMILE_IDBG(3,"received 'semaineCallback' message '%s'",_msg->msgname);
    if (!strncmp(_msg->msgname,"start",5)) { agentTurn = 1; agentTurnCntdn = 0; }
    else if (!strncmp(_msg->msgname,"end",3)) {
      agentTurn = 0; agentTurnCntdn = agentTurnPastBlock;
    }
    else if (!strncmp(_msg->msgname,"present",7)) { if (userPresence != 1) { userPresence = 1; doReset=1; } }
    else if (!strncmp(_msg->msgname,"absent",6)) { if (userPresence != 0) { userPresence = 0; doReset=1; } }
		return 1;  // message was processed
	}

	return 0; // if message was not processed
}


// a derived class should override this method, in order to implement the actual processing
int cRnnVad2::myTick(long long t)
{
  cVector * frame = reader_->getNextFrame();
  if (frame == nullptr) return 0;

  cnt++;

  int vad = 0;
  FLOAT_DMEM E = 0.0;
  FLOAT_DMEM vact = frame->dataF[voiceIdx];
  FLOAT_DMEM aact = 0.0;
  
  if (agentIdx >= 0) aact = frame->dataF[agentIdx];

  /* get frame energy */
  if (energyIdx >= 0) {
    E = frame->dataF[energyIdx];
    eCurrent->nextE(E); 
  }

  /*
    get the "agent's talking state" ..
  */
  int _agentTurn = 0;
  int noV = 0;
  lockMessageMemory();
  if (doReset == 1) {

    doReset=0;
  }
  _agentTurn = agentTurn;
  if ((agentTurn)||(agentTurnCntdn>0)) {
    if (smartRejectAgent || alwaysRejectAgent) { noV = 1; }
    //if (cnt < agentBlockTime) { noV=1; }
  }
  if (agentTurnCntdn > 0) agentTurnCntdn--;
  unlockMessageMemory();


  /* case A: we know it's the agent's turn, and we apply a different strategy for getting the user's voice */
  if (noV) {
    if (energyIdx >= 0 && !alwaysRejectAgent) {

      if (vact > voiceThresh) {
        /* energy based user voice detection with a high threshold */
        /*if (eCurrent->getEnv() > eUser->getAvg()*1.2 && eCurrent->getEnv() > eAgent->getEnv()*0.9 && aact < agentThresh && eUser->getAvg() > 0.0) {
          vad = 8;
        } else */
          if (eCurrent->getEnv() > eUser->getEnv()*0.9 && eCurrent->getEnv() > eAgent->getEnv()*0.9 && aact < agentThresh-0.1 && eUser->getEnv() > 0.0 && eAgent->getEnv() > 0.0) {
          vad = 9;
        } else if (eCurrent->getEnv() > eAgent->getEnv()*1.1 && aact < agentThresh+0.1 && eAgent->getEnv() > 0.0) {
          vad = 10;
        }
      }

      /* update agent energy stats */
      if (vact > voiceThresh && aact > 0.1 && vad == 0 && eCurrent->getEnv() < eUser->getEnv()*0.9) {
        eAgent->nextE(E);
      }

    }
  } else {

  /* case B: no agent turn, we look for user voice primarily */
  if (vact > voiceThresh) {
    if (energyIdx >= 0) {
      if (aact <= agentThresh+0.05 && eCurrent->getEnv() > eBg->getAvg()*1.1 && eBg->getAvg() > 0.0) { /* if no agent voice is detected, update user energy stats */
        eUser->nextE(E);
      } /*else {
        //eAgent->nextE(E);
      }*/
      if ((eCurrent->getEnv() > eUser->getEnv()*0.75 || vact > 0.95 || eUser->getEnv() == 0.0 || eCurrent->getEnv() == 0.0) && aact < agentThresh) { /* Energy verification, low threshold */
        vad = 1;
      } else {
        /* energy based user voice detection with a high threshold */
        if (eCurrent->getEnv() > eUser->getEnv()*0.9 && eUser->getEnv() > 0.0) {
          vad = 3;
        } /* else if (eCurrent->getEnv() > eUser->getEnv()*0.6 && eUser->getEnv() > 0.0) {
          vad = 4;
        }*/
      }
    } else {
      vad = 5;
    }
  } else {
    if (energyIdx >= 0) {
      /* energy based user voice detection with a high threshold */
      if (eCurrent->getEnv() > eUser->getEnv()*0.8  && vact > 0.3 && eUser->getEnv() > 0.0) {
        vad = 6; /* higher thresh */
      } else if (eCurrent->getEnv() > eUser->getEnv()*0.9 && eCurrent->getEnv() < eUser->getEnv()*1.1 && eUser->getEnv() > 0.0 && vact > 0.1) {
        vad = 7;
      } else {
        /* update background energy model */
        eBg->nextE(E);
      }
    }
  }

  }

  if (vadDebug) {
    printf("noV=%i vact=%.3f aact=%.3f eU=%.3f eCur=%.3f eBg=%.3f eAg=%.3f v=%i\n",noV,vact,aact,eUser->getEnv(),eCurrent->getEnv(),eBg->getEnv(),eAgent->getEnv(),vad);
  }

  /*
    generate output frame with voicing info
  */
  if (cnt < 100) vad=0; /* block vad output at the beginning, it may confuse the turnDetector otherwise */
  if (vad > 0) vad = 1; /* map the internal VAD states to binary 0/1 */
  
  if (vadDebug==2) { vad=0; } /* Disable VAD output for debugging ONLY vad functionality without other interfering components */

  frameO->dataF[0] = (FLOAT_DMEM)(vad);
  writer_->setNextFrame(frameO);

  return 1;
}

cRnnVad2::~cRnnVad2()
{
  if (frameO != nullptr) delete frameO;
  if (eCurrent != nullptr) delete eCurrent;
  if (eUser != nullptr) delete eUser;
  if (eAgent != nullptr) delete eAgent;
  if (eBg != nullptr) delete eBg;
}

