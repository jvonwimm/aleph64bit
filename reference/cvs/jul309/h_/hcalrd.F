      SUBROUTINE HCALRD
C
C***********************************************************************
C! Reading of HCAL calibration constants from DAF                      *
C!                                                                     *
C!  AUTHORS  :  R. Tenchini             890126                         *
C!                                                                     *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : H1EC,H2EC,H3EC,H4EC                                    *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C!                                                                     *
C!                                                                     *
C***********************************************************************
#ifndef DOC
#include "alcons.h"
#include "rparac.h"
#include "rlunit.h"
#include "rflags.h"
#include "rcurnt.h"
#include "bcs.h"
      CHARACTER*30 WHERE(3)
      INTEGER GTSTUP, ALGTDB
      DATA WHERE /
     &  ' ===> READ FROM DATA CARDS '  ,
     &  ' ===> READ FROM DATABASE   '  ,
     &  ' ===> MISSING              '  /
#include "bmacro.h"
      KPERIO=GTSTUP('HC',IRUNRC)
C
C    ******   H1EC   ******
C
C
C First we check if there are cards that overwrite the Database
C
                                   IFLAG=1
      NH1EC=NAMIND('H1EC')
      KH1EC=IW(NH1EC)
C
C  Database reading
C
      IF(KH1EC.EQ.0)THEN
                                   IFLAG=2
         IGT = ALGTDB(LRCONS,'H1EC',-KPERIO)
         KH1EC = IW(NH1EC)
      ENDIF
C
C  If also the Database is missing we set default values
C
      IF(KH1EC.EQ.0) THEN
                                   IFLAG=3
      ENDIF
      WRITE(LDEBRL,*) '     HCAL TOWERS CALIBRATIONS (H1EC) WERE ',
     &  WHERE(IFLAG)
C
C    ******   H2EC   ******
C
C
C First we check if there are cards that overwrite the Database
C
                                   IFLAG=1
      NH2EC=NAMIND('H2EC')
      KH2EC=IW(NH2EC)
C
C  Database reading
C
      IF(KH2EC.EQ.0)THEN
                                   IFLAG=2
         IGT = ALGTDB(LRCONS,'H2EC',-KPERIO)
         KH2EC = IW(NH2EC)
      ENDIF
C
C  If also the Database is missing we set default values
C
      IF(KH2EC.EQ.0) THEN
                                   IFLAG=3
      ENDIF
      WRITE(LDEBRL,*) '     HCAL TOWERS CALIBRATIONS (H2EC) WERE ',
     &  WHERE(IFLAG)
C
C    ******   H3EC   ******
C
C
C First we check if there are cards that overwrite the Database
C
                                   IFLAG=1
      NH3EC=NAMIND('H3EC')
      KH3EC=IW(NH3EC)
C
C  Database reading
C
      IF(KH3EC.EQ.0)THEN
                                   IFLAG=2
         IGT = ALGTDB(LRCONS,'H3EC',-KPERIO)
         KH3EC = IW(NH3EC)
      ENDIF
C
C  If also the Database is missing we set default values
C
      IF(KH3EC.EQ.0) THEN
                                   IFLAG=3
      ENDIF
      WRITE(LDEBRL,*) '     HCAL TOWERS CALIBRATIONS (H3EC) WERE ',
     &  WHERE(IFLAG)
C
C    ******   H4EC   ******
C
C
C First we check if there are cards that overwrite the Database
C
                                   IFLAG=1
      NH4EC=NAMIND('H4EC')
      KH4EC=IW(NH4EC)
C
C  Database reading
C
      IF(KH4EC.EQ.0)THEN
                                   IFLAG=2
         IGT = ALGTDB(LRCONS,'H4EC',-KPERIO)
         KH4EC = IW(NH4EC)
      ENDIF
C
C  If also the Database is missing we set default values
C
      IF(KH4EC.EQ.0) THEN
                                   IFLAG=3
      ENDIF
      WRITE(LDEBRL,*) '     HCAL TOWERS CALIBRATIONS (H4EC) WERE ',
     &  WHERE(IFLAG)
      IF(JDBDRF(JULHC).GE.1) THEN
         CALL PRTABL('H1EC',0)
         CALL PRTABL('H2EC',0)
         CALL PRTABL('H3EC',0)
         CALL PRTABL('H4EC',0)
      ENDIF
      RETURN
      END
#endif
