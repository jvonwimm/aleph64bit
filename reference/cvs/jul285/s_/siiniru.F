      SUBROUTINE SIINIRU
C----------------------------------------------------------------------
C!  - SICAL Run Initialisation
C!
C!   Author   : B. Bloch-Devaux 911029
C!              Modified for Bunch train mode October 1994
C!              Modified for Bunch train mode in SCAN 95 October 1995
C!   Input    : none
C!   Output   :
C!              SREC bank (constants used in cluster reconstruction)
C!              SCOR bank (constants for cluster position corrections)
C!              SDCO bank (constants for dead pad energy correction)
C!              SBHA bank (Cuts for Bhabha event selection)
C!              SECA bank (Scal energy calibration)
C!              SMPD bank (Scal missing pad list)
C!              SCAX bank ]
C!              SCGA bank ] Scal Calibration constants
C!              SCPH bank ]
C!              SCFD bank ]
C!              SIBX bank (constants to define in-time events in SCAN95)
C!
C!   Description :
C!   ===========
C?   Read geometry and constant banks from DAF
C?   Reset lumi statistics bank SLUM
C?
C!======================================================================
#ifndef DOC
#include "sinamc.h"
#include "rcurnt.h"
#include "rparac.h"
#include "bcs.h"
#include "rlunit.h"
#include "rflags.h"
      INTEGER ALGTDB,ALGTWA,GTSTUP
      PARAMETER ( LSSTA=9 )
      EXTERNAL ALGTDB,NDROP,ALGTWA,GTSTUP
#include "slumjj.h"
#include "bmacro.h"
C----------------------------------------------------------------------
C Get geometry
      CALL SIRDAF(LRGEOM,IRUNRC,IOK)
      IF(IOK.NE.0) THEN
        CALL RERROR ('SIINIRU', -1, 'Missing SCAL banks on DAF')
      ENDIF
C Get reconstruction constants
      NR = ALGTDB(LRGEOM,'SBHASRECSCORSDCOSECA',IRUNRC)
C Get constants to define in-time events in SCAN 1995 and Bunch calibration
      NS = 1
      IF ( GTSTUP('SI',IRUNRC).GE.4) THEN
         NS=ALGTDB(LRGEOM,'SIBXSECBSHLD',IRUNRC)
      ENDIF
      IF(NR*NS.EQ.0) THEN
        CALL RERROR ('SIINIRU', 1, 'Missing SCAL recons banks on DAF')
      ENDIF
C Get dead pad list
C  Build SMPD ( mising pad list) from SDPD and SKPD if any
      CALL SIGTDP
C Get calibration constants if required  ( only Data )
        IF ( .NOT.FMCRUN ) THEN
          NR = ALGTDB(LRGEOM,'SCGASCAXSCPHSCFD',IRUNRC)
          IF(NR.EQ.0) THEN
            CALL RERROR ('SIINIRU', 1,
     &        'Missing calib SCAL banks on DAF')
          ENDIF
        ENDIF
C Print out geometry if needed
      IF(JDBDRF(JULSI).GE.1) CALL SIPRGO
C  Reset reconstruction statistics
      IF ( IW(NASSTA).GT.0) GO TO 997
      NWAG = ALGTWA(IRUNRC)
      LEN = NWAG*LSSTA+LMHLEN
      CALL AUBOS('SSTA',0,LEN,KSSTA,IGARB)
      IF ( IGARB.EQ.2) GO TO 998
      IW(KSSTA+1) = LSSTA
      IW(KSSTA+2) = NWAG
  997 CONTINUE
C Reset Lumi statistics bank
        KSBHA = IW(NASBHA)
        IF ( KSBHA.GT.0) THEN
          CALL BDROP (IW,'SLUM')
          NWAG = ALGTWA(IRUNRC)
C The first rows are the total for method 1 and 2 ,then idem for each wg
          NWAG = NWAG+1
          LEN = NWAG*LSLUMA*LROWS(KSBHA)+2
          CALL AUBOS('SLUM',IRUNRC,LEN,KSLUM,IGARB)
          IW(KSLUM+1) = LSLUMA
          IW(KSLUM+2) = NWAG*LROWS(KSBHA)
        ENDIF
C
      GOTO 999
C
  998 CALL RERROR ('SIINIRU',-1,'Unable to create SSTA')
  999 CONTINUE
      END
#endif
