      SUBROUTINE RREVHE
C-------------------------------------------------------------------
C!  Create reconstruction event header
C
C  Author: J. Knobloch 13-SEP-1989
C!   Modified :- E. Lancon             12-MAY-1993
C!            create and fill JEDS bank
C
C Bank created:  REVH
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "revhjj.h"
#include "rflags.h"
#include "rtimes.h"
#include "rcurnt.h"
#include "xtcnjj.h"
#include "istajj.h"
#include "lolejj.h"
#include "jedsjj.h"
#include "rparac.h"
      PARAMETER( NDET = 16 )
      INTEGER ALTRIG
      EXTERNAL ALTRIG
      LOGICAL FIRST
      LOGICAL LREVH , SIOK, SICAOK, BTEST
      INTEGER NADET(NDET)
      DATA FIRST/.TRUE./
#include "bmacro.h"
      IF(FIRST)THEN
        FIRST=.FALSE.
        NADET(1)=NAMIND('VPLH')
        NADET(2)=NAMIND('IDIG')
        NADET(3)=NAMIND('TPDI')
        NADET(4)=NAMIND('TWDI')
        NADET(5)=NAMIND('ETDI')
        NADET(6)=NAMIND('EWDI')
        NADET(7)=NAMIND('HTDI')
        NADET(8)=NAMIND('HWDI')
        NADET(9)=NAMIND('MUDG')
        NADET(10)=NAMIND('SRTD')
        NADET(11)=NAMIND('LTDI')
        NADET(12)=NAMIND('LWDI')
        NADET(13)=NAMIND('BHIT')
        NADET(14)=ALTRIG(IT1,IT2,IL2)
        NADET(15)=NAMIND('BOMR')
        NADET(16)=NAMIND('SIDI')
      ENDIF
C
C?   Get all previous REVH bank from previous processings
C?   coy them into new REVH banks with nr = nr + 1
C?   Save in local variables detector status and readout-bits
C?   (this may not be redermined in case of reprocessing from a DST)
C?   Do it only in case of POT-POT reprocessing....

C
      KREVH = IW(NAMIND('REVH'))
      LREVH = KREVH.NE.0 .AND. FREPRO
      IF ( LREVH ) THEN
        ISDS  = ITABL(KREVH,1,JREVDS)
        ISSB  = ITABL(KREVH,1,JREVSB)
        ISRB1 = ITABL(KREVH,1,JREVRB)
        ISRB2 = ITABL(KREVH,1,JREVRB+1)
C-  look for the highest REVH bank number
   10   NHI = IW(KREVH-2)
        IF (IW(KREVH-1).NE.0) THEN
          KREVH = IW(KREVH-1)
          GOTO 10
        ENDIF
C-  swap old REVH banks
        DO IB =  NHI,0,-1
          IND = MSWAP (IW,'REVH',IB,'REVH',IB+1)
        ENDDO
      ENDIF
C
      NBK=0
      CALL AUBOS('REVH',NBK,LREVHA+LMHLEN,KREVH,IGARB)
      IF(IGARB.EQ.2) GOTO 888
      IW(KREVH+LMHCOL) = LREVHA
      IW(KREVH+LMHROW) = 1
      IW(KREVH+LMHLEN+JREVDS) = 0
      KXTCN=IW(NAMIND('XTCN'))
      IF(KXTCN.NE.0)THEN
        IW(KREVH+LMHLEN+JREVDS) =IW(KXTCN+LMHLEN+JXTCTR)
      ENDIF
      KISTA=IW(NAMIND('ISTA'))
      IF(KISTA.NE.0)THEN
        IF(IW(KISTA+LMHLEN+JISTHT).NE.0)THEN
          IW(KREVH+LMHLEN+JREVDS) = IBSET(IW(KREVH+LMHLEN+JREVDS),5)
        ENDIF
      ENDIF
      IW(KREVH+LMHLEN+JREVFE  ) = JFBIRF(1)
      IW(KREVH+LMHLEN+JREVFE+1) = JFBIRF(2)
      IW(KREVH+LMHLEN+JREVNE  ) = JEBIRF(1)
      IW(KREVH+LMHLEN+JREVNE+1) = JEBIRF(2)
      IW(KREVH+LMHLEN+JREVSB) = 0
      IW(KREVH+LMHLEN+JREVRB) = 0
      DO 20 I=1,NDET
        IF(IW(NADET(I)).NE.0)THEN
          IW(KREVH+LMHLEN+JREVRB)=IBSET(IW(KREVH+LMHLEN+JREVRB),I-1)
        ENDIF
   20 CONTINUE
C
C?   If REVH exists before save some previous quantities
C
      IF ( LREVH ) THEN
        IW(KREVH+LMHLEN+JREVDS)     = ISDS
        IW(KREVH+LMHLEN+JREVSB)     = ISSB
        IW(KREVH+LMHLEN+JREVRB)     = ISRB1
        IW(KREVH+LMHLEN+JREVRB+1)   = ISRB2
      ENDIF
C
C Compute EDIR Class Word and accumulate statistics
C
      CALL RNXMOD(MODEDI,RMOD)
C   In case of garbage collection in RNXMOD
      KREVH=IW(NAMIND('REVH'))
      CALL ALCLASW (MSKED)
      CALL ALSUMCL (MSKED)
      IW(KREVH+LMHLEN+JREVEC) = MSKED
      CALL ALTIME(TIME)
      RW(KREVH+LMHLEN+JREVTI) = TIME-AEVSRT
C
C?   Now Fill JEDS
C
      KJEDS = IW(NAMIND('JEDS'))
      IF ( KJEDS.LE.0 ) THEN
        CALL AUBOS ('JEDS',IRUNRC,2*LJEDSA+LMHLEN,KJEDS,IGARB)
        IF(IGARB.EQ.2) GOTO 777
        IW(KJEDS+LMHCOL) = LJEDSA
        IW(KJEDS+LMHROW) = 2
      ENDIF
      SIOK = SICAOK (IRUNRC)
      IF ( KJEDS.GT.0 ) THEN
        DO ICL =  1, 30
          IF ( BTEST(MSKED,ICL-1) ) THEN
            IW(KJEDS+LMHLEN+JJEDCL+ICL-1) =
     &        IW(KJEDS+LMHLEN+JJEDCL+ICL-1) + 1
            IF ( SIOK ) THEN
              IW(KJEDS+LMHLEN+LJEDSA+JJEDCL+ICL-1) =
     &        IW(KJEDS+LMHLEN+LJEDSA+JJEDCL+ICL-1) + 1
            ELSE
            ENDIF
          ELSE
          ENDIF
        ENDDO
      ELSE
      ENDIF
C
      GOTO 999
C
  888 CALL RERROR('RRHEVE',-1,'No space to create REVH bank')
      GOTO 999
C
  777 CALL RERROR('RRHEVE',-2,'No space to create JEDS bank')
      GOTO 999
C
  999 RETURN
      END
#endif
