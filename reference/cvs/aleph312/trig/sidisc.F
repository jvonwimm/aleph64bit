      SUBROUTINE SIDISC(IEND,IPTY,IPATT)
C***********************************************************************
C                                                                      *
C   Author   :- Joe Boudreau          30-OCT-1991                      *
C                                                                      *
C!  Discriminate the SIXA bank for endcap IEND odd or IEND even.       *
C!  PARITY=1  means odd , PARITY=2 means even                          *
C                                                                      *
C   Inputs        : INTEGER IEND   = 1 if endcap A, = 2 if endcap B    *
C                   INTEGER IPTY   = 1 if odd, = 2 if even             *
C   Outputs       : INTEGER IPATT  bit 0-3 pattern after discrimination*
C                                  against threshholds 1-4             *
C                                                                      *
C                                                                      *
C***********************************************************************
#ifndef DOC
C
#include "bcs.h"
C
#include "x1namc.h"
C
      LOGICAL BTEST
      INTEGER IEND,IPTY,IPATT
      INTEGER IROAD,ISECT,IBT,IADC,JPNTR,KSIXA,I
#include "sitrco.h"
#include "bmacro.h"
C
C  Loop over every sector and every road within this endcap and parity,
C  reading the energy deposit from the SIXA bank . If any sector has
C  an  energy deposit above threshold, then set a a bit in IPATT
C  corresponding to that threshold.
C
      IPATT = 0
      KSIXA = IW(NASIXA)
      IF (KSIXA.GT.0) THEN
        DO 200 IROAD = 1,2
          DO 300 ISECT= 1,8
            IBT = 16*(IROAD-1) + 8*(IEND-1) + ISECT -1
            IF (.NOT.BTEST(ISIDIS(IPTY),IBT)) THEN
              JPNTR=KROW(KSIXA,(IEND-1)*8+ISECT)+IPTY
              IADC = IBITS(IW(JPNTR),16*(IROAD-1),16)
              DO 400 I = 1,4
                IF (IADC.GT.ISITHR(I)) IPATT=IBSET(IPATT,I-1)
 400          CONTINUE
            ENDIF
 300      CONTINUE
 200    CONTINUE
      ENDIF
      RETURN
      END
#endif
