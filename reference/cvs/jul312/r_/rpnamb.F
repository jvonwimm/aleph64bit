      SUBROUTINE RPNAMB
C-------------------------------------------------------
C! Print size of named banks created or deleted since
C! last call
C!
C!  AUTHOR   : J.Knobloch  22-March-1989
C!  MODIFIED :
C!
C!    INPUT   : MODUL = Number of module
C!                      Special values:
C!                       MODUL=0  close previous module
C!                       MODUL=-1 Print summary
C!    OUTPUT  : RMOD    LOGICAL flag = .TRUE. if module to
C!                                     be executed
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rlunit.h"
      COMMON /SYSBOS/NSYST,NAMES,NPRIM,IDNAM,IDPTR,
     1               IDFMT,NDUMM,NRESR,NLPLM, NARR,
     2               IARR(10),
     3               IEFMT,TLEFT,
     4               LEPIO,NAMI,INDI,INDJ,IBC,DUMMI(73),
     5               INTA(200), NPTR,NRUN,NEVT,
     6               LUNDAT,LUNSEL,LUNSE2,LUTDAT,MASKR,LMASK,
     7               NRE,NAMERE(3),NUMMRE(3),IRUNRE(3),IEVTRE(3)
      DIMENSION LAST(2000)
      CHARACTER*4 NAME,CHAINT
      DATA LAST/2000*0/
      NNAM=IW(IDNAM)
      DO 50 I=1,NNAM
        ID=IW(NSYST+I)
        NWRD=0
        NAME=CHAINT(IW(IDNAM+I))
        NB=0
        NP=0
   10   CONTINUE
        IF(ID.EQ.0)GOTO 20
        NB=NB+1
        NWRD=NWRD+IW(ID)
        NLW=IW(ID)
        NCO=IW(ID+1)
        IF(NCO.LT.1.OR.NCO.GT.200)NCO=0
        NRO=IW(ID+2)
        IF(NRO.LT.1.OR.NRO.GT.10000)NRO=0
        ID=IW(ID-1)
        GOTO 10
   20   CONTINUE
        IDIFF=NWRD-LAST(I)
        IF(IDIFF.NE.0) THEN
          LAST(I)=NWRD
          IF(NLW.GT.0.AND.NB.EQ.1 )THEN
            WRITE(IW(6),'(2A,I3,4I7)')' Bank ',NAME,NB,IDIFF,NLW,NCO,
     &          NRO
          ELSE
            WRITE(IW(6),'(2A,I3,I7)')' Bank ',NAME,NB,IDIFF
          ENDIF
        ENDIF
   50 CONTINUE
      RETURN
      END
#endif
