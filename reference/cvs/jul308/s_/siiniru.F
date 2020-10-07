      SUBROUTINE SIINIRU
C----------------------------------------------------------------------
C!  - SICAL Run Initialisation
C!
C!   Author   : B. Bloch-Devaux 911029
C!              Modified for Bunch train mode October 1994
C!              Modified for Bunch train mode in SCAN 95 October 1995
C!              Move booking of SSTA to siinijo. M.Cattaneo September 1997
C!              Modified to load SECT bank . B.Bloch November 1997
C!              Modified to load SNOI bank . B.Bloch June 1998
C!              Suppress error message for SECT when setup<7 M.Cattaneo 12/97
C!   Input    : none
C!   Output   :
C!              SREC bank (constants used in cluster reconstruction)
C!              SCOR bank (constants for cluster position corrections)
C!              SDCO bank (constants for dead pad energy correction)
C!              SBHA bank (Cuts for Bhabha event selection)
C!              SECA bank (Scal energy calibration)
C!              SECT bank (Scal bunch dependent gain corrections ) 
C!              SMPD bank (Scal missing pad list)
C!              SNOI bank (definition of electronic noise cluster )
C!              SCAX bank ]
C!              SCGA bank ] Scal Calibration constants
C!              SCPH bank ]
C!              SCFD bank ]
C!              SIBX bank (constants to define in-time events in SCAN95)
C!              SECB bank (bunch energy calibration)
C!              SHLD bank (Hold timing curve parameters)
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
      NR = ALGTDB(LRGEOM,'SBHASRECSCORSDCOSECASNOI',IRUNRC)

C Get constants to define in-time events in SCAN 1995 and Bunch calibration
      IF ( GTSTUP('SI',IRUNRC).GE.4) THEN
         NS=ALGTDB(LRGEOM,'SIBXSECBSHLD',IRUNRC)
      ELSE
         NS = 1
      ENDIF

      IF ( GTSTUP('SI',IRUNRC).GE.7) THEN
         NT=ALGTDB(LRGEOM,'SECT',IRUNRC)
      ELSE
         NT = 1
      ENDIF

      IF(NR*NS*NT.EQ.0) THEN
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
  999 CONTINUE
      END
#endif
