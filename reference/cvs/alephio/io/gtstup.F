      INTEGER FUNCTION GTSTUP (DET,IRUN)
C -----------------------------------------------------------
C - F.Ranjard - 901018
CKEY ALEF ADBR DETECTOR SETUP
C! get setup code for a given detector
C
C - Input  : DET      / A = CHARACTER*2 variable to define the
C                           detector: 'BP VD IT TP EC LC SA HC MU TR GE'
C            IRUN     / I = run number
C - Output : GTSTUP   / I = setup code
C                           =-1 in case of error
C - Description :
C      IF run < 2001 THEN
C       montecarlo : get period number from ASIM
C       IF ASIM does not exist THEN get period number from run date
C      ELSE run>= 2001 THEN
C       raw data : get period number from run number
C      ENDIF
C
C -------------------------------------------------------------
#ifndef DOC
      CHARACTER*2 DET
      INTEGER ADBRPN,ADBRUN
#include "bcs.h"
#include "asimjj.h"
#include "runhjj.h"
#include "adbrlist.h"
      DATA NASIM / 0/ , NRUNH/0/
#include "bmacro.h"
C ------------------------------------------------------------
C - 1st entry
C
      IF (NASIM.EQ.0) THEN
         NASIM = NAMIND ('ASIM')
         NRUNH = NAMIND ('RUNH')
      ENDIF
C
C - next entry
C
      IPERIOD = 0
      IF (IRUN.LT.2001) THEN
C      monte carlo input file
         JASIM = IW(NASIM)
         IF (JASIM .GT. 0) THEN
            IPERIOD = ITABL(JASIM,1,JASIYM)
         ELSE
            JRUNH = IW(NRUNH)
            IF (JRUNH.GT.0) IPERIOD = IW(JRUNH+JRUNSD)/100
         ENDIF
         IF(IPERIOD.LE.12) THEN
           IPERIOD = 200000 +IPERIOD
         ELSEIF(IPERIOD.LT.10000)THEN
           IPERIOD = 190000+IPERIOD
         ENDIF
         NPERIOD = ADBRPN (DET,IROW,IPERIOD,NRUN,ISETUP)
         IF (NPERIOD.EQ.0) ISETUP = -1
      ELSE
C      raw data
         NRUN = ADBRUN (DET,IROW,IPERIOD,IRUN,ISETUP)
         IF (NRUN.EQ.0) ISETUP = -1
      ENDIF
C
      GTSTUP  = ISETUP
#if defined(ARDEB)
      WRITE(IW(6),*) ' +++GTSTUP+++ ',DET,IRUN
      WRITE(IW(6),*) ' IPERIOD= ',IPERIOD,' NPERIOD= ',NPERIOD
     & ,' NRUN= ',NRUN,' ISETUP= ',ISETUP
      WRITE(IW(6),*) ' =========================='
#endif 
C
      END
#endif
