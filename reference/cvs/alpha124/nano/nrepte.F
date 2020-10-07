      SUBROUTINE NREPTE(IRET)
CKEY NANO IN ALPHA /INTERNAL
C----------------------------------------------------------------------
C! NANO unpacking : create PTEX bank from NDDE bank
C!
C!   Author   :- Gerrit Graefe         15-FEB-1994
C!
C!   Inputs:
C!        - none
C!
C!   Outputs:
C!        - IRET / I        return code  0=created PTEX from NDDE
C!                                       1=can't find NDDE
C!                                       2=no space to book PTEX
C!                                       3=unknown error
C!
C!   Libraries required: ALPHA,BOS77
C!
C!   Description
C!   ===========
C!
C?   This subroutine creates the bank PTEX from the NDDE bank.
C?   Comments:
C?              See NREFRF subroutine descriptions !!
C?
C?   Remind that there are some differences between the original PTEX
C?   bank and that one created here. Every track has just one entry, as
C?   no sector information is avaliable any more. Therefore the bank
C?   should never be accesed directly. Use QDEDX or QDEDXM instead
C?   to get a correct result.
C?
C?
C!======================================================================
#ifndef DOC
      IMPLICIT NONE
#include "qdecl.h"
#include "qcde.h"
#include "nbpdcl.h"
#include "nbnkpo.h"
#include "nddejj.h"
#include "ndtkjj.h"
#include "ptexjj.h"
      INTEGER NLINK,NVERS,NAMIND
      INTEGER I,J,INDNDE,INDPTE,IGARB,IROWN,IROWF,KRNDDE,KOPTEX,IRET
      INTEGER INDNDT,KRNDTK,IROWT
      INTEGER MASK8,MASK9,MASK12,MASK20,MASK23
      DATA MASK8,MASK9,MASK12,MASK20,MASK23/255,511,4095,1048575,
     &                                      8388607/
#include "qmacro.h"
C----------------------------------------------------------------------
C
C!..LINK TO BANK NDDE
C
      IRET=3
      INDNDE=IW(NAMIND('NDDE'))
      IF(INDNDE.EQ.0)THEN
        IRET=1
        RETURN
      ENDIF
      KONDDE=INDNDE
      KCNDDE=IW(KONDDE+1)
      KRNDDE=IW(KONDDE+2)
C
C!..LINK TO BANK NDTK
C
      INDNDT=IW(NAMIND('NDTK'))
      IF(INDNDT.EQ.0)THEN
        IRET=1
        RETURN
      ENDIF
      KONDTK=INDNDT
      KCNDTK=IW(KONDTK+1)
      KRNDTK=IW(KONDTK+2)
C
C!..CREATE BANK PTEX
C
      CALL AUBOS('PTEX',0,LMHLEN+LPTEXA*KRNDDE,INDPTE,IGARB)
      IF(IGARB.EQ.2)THEN
        CALL QWMESE('### NREPTE ### No space to book PTEX')
        IRET=2
        RETURN
      ENDIF
      CALL BKFMT('PTEX','2I,(7I)')
      NAPTEX=NAMIND('PTEX')
      KOPTEX=INDPTE
      IW(KOPTEX+1)=LPTEXA
      IW(KOPTEX+2)=KRNDDE
C
C!..FILL BANK PTEX
C
      IROWF=KOPTEX+LMHLEN
      DO 100 I=1,KRNDTK
C!..ROW IN PTEX BANK
         IROWT=KONDTK+LMHLEN+LNDTKA*(I-1)
         IROWN=IAND(ISHFT(IW(IROWT+JNDTTA),-19),MASK8 )
         IF (IROWN.EQ.0) GOTO 100
C!..SEGMENT ID (ALWAYS 37. DUMMY SECTOR  FOR NO CALIBRATION)
        IW(IROWF+JPTESL)=37
C!..DE/DX (TRUNCATED MEAN)
        IW(IROWF+JPTETM)=IAND(ISHFT(IW(IROWN+JNDDLI),-12),MASK20)
     &                  /100000
C!..TRACK LENGTH
        IW(IROWF+JPTEUL)=IAND(IW(IROWN+JNDDLI),MASK12)/5
C!..NUMBER OF SAMPLES
        IW(IROWF+JPTENS)=IAND(IW(IROWN+JNDDSE),MASK9 )
C!..AVERAGE DRIFT LENGTH
        IW(IROWF+JPTEAD)=0
C!..TRACK NUMBER
        IW(IROWF+JPTETN)=I
C!..SATURATION FLAG
        IW(IROWF+JPTESF)=0
        IROWF=IROWF+LPTEXA
  100 CONTINUE
      CALL BLIST(IW,'S+','PTEX')
      IRET=0
  999 RETURN
      END
#endif
