      SUBROUTINE ECBLDA
C----------------------------------------------------------------------
C!  - GIVE VALUE TO ECAL CONSTANTS
C!                                                                     *
C!  AUTHOR   : A. BONISSENT  860615                                    *
C!  MODIFIED : A. BONISSENT  861108                                    *
C!                                                                     *
C!  BANKS :                                                            *
C!    INPUT   : NONE                                                   *
C!    OUTPUT  : NONE                                                   *
C!    CREATED : NONE                                                   *
C?
C!======================================================================
#ifndef DOC
#include "alcons.h"
#include "bcs.h"
#include "rlunit.h"
#include "rcurnt.h"
#include "ecnsjj.h"
#include "eclcom.h"
#include "ectcom.h"
#include "egnrec.h"
#include "eminic.h"
#include "eratio.h"
#include "eenbac.h"
      INTEGER  ALGTDB
      EXTERNAL ALGTDB
#include "bmacro.h"
C
C? Access data base to initialize data
      KECNS = ALGTDB(LRCONS,'ECNS',IRUNRC)
      IF (KECNS .EQ. 0) GOTO 999
      KECNS = IW(NAMIND('ECNS'))
      ENHIGH=RTABL(KECNS,1,JECNEH)
      ENELOW=RTABL(KECNS,1,JECNEL)
      ECGTHR=RTABL(KECNS,1,JECNCT)
      STPKEC=RTABL(KECNS,1,JECNSS)
      STMNEC=RTABL(KECNS,1,JECNSM)
      PMINEC = RTABL(KECNS,1,JECNPM)
C? Get speed of light in adequate units
      AAAKEC = CLGHT * 1.E-5
C? Initialize cluster array for start
      CALL VZERO(IARRAY,6144)
C- COMPUTE REGION NUMBER
      DO 10 J=1,8
      NUMREG(J)     = 1
      NUMREG(229-J) = 1
 10   CONTINUE
      DO 20 J=9,24
      NUMREG(J)     = 2
      NUMREG(229-J) = 2
 20   CONTINUE
      DO 30 J=25,40
      NUMREG(J)     = 3
      NUMREG(229-J) = 3
 30   CONTINUE
      DO 40 J=41,50
      NUMREG(J)     = 4
      NUMREG(229-J) = 4
 40   CONTINUE
      DO 50 J=51,114
      NUMREG(J)     = 5
      NUMREG(229-J) = 5
 50   CONTINUE
C? SET DATA FOR MINIMUM IONIZING PARTICLES
C? BASED ON ANALYSIS OF TEST BEAM MUONS
C? ENERGY LOSS IN STACK (GEV)
      ENFMIP(1) = RTABL(KECNS,1,JECNEP)
      ENFMIP(2) = RTABL(KECNS,1,JECNEP+1)
      ENFMIP(3) = RTABL(KECNS,1,JECNEP+2)
C? ERROR SQUARE IN GEV**2
      ER2ST1 = RTABL(KECNS,1,JECNER)
      ER2ST2 = RTABL(KECNS,1,JECNER+1)
      ER2ST3 = RTABL(KECNS,1,JECNER+2)
C? PROBABILITY FOR MINION IDENTIFICATION
      ECHIMN = RTABL(KECNS,1,JECNEI)
      EPRMIP = RTABL(KECNS,1,JECNPR)
C? ENERGY ABOVE WICH A CLUSTER IS NOT A MINION CANDIDATE(500MEV!)
      ETOMIN = RTABL(KECNS,1,JECNEM)
C? SET DATA FOR ENERGY/MOMENTUM BALANCE
C? FOR PARTICLES BETWEEN ECAL AND TPC
      ERENB2 = RTABL(KECNS,1,JECNEB)
      EPREBA = RTABL(KECNS,1,JECNEB+1)
C? fill electron/mip and electron/pion ratios
      RAPEMU = RTABL(KECNS,1,JECNME)
      RAPEPI = RTABL(KECNS,1,JECNRG)
      CFNTER(1) = RTABL(KECNS,1,JECNR1)
      CFNTER(2) = RTABL(KECNS,1,JECNR2)
      CFNTER(3) = RTABL(KECNS,1,JECNR3)
      RETURN
 999  CONTINUE
      CALL RERROR('ECBLDA',1,'ECNS bank missing :no ecal data')
      RETURN
      END
#endif
