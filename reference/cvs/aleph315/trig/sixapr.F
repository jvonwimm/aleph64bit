      SUBROUTINE SIXAPR
C***********************************************************************
C                                                                      *
C   Author   :- B. Bloch-Devaux      30-OCT-1991                       *
C                                                                      *
C! Print the content of SIXA trigger ADC's in readable format          *
C                                                                      *
C   Inputs        :  SIXA bank                                         *
C                                                                      *
C***********************************************************************
#ifndef DOC
C
#include "bcs.h"
      DIMENSION SIADC(4)
      CHARACTER*1 NAM(2)
#include "sitrco.h"
      DATA NAM/'A','B'/
#include "bmacro.h"
      NASIXA = NAMIND('SIXA')
      KSIXA = IW(NASIXA)
      IF (KSIXA.GT.0) THEN
        WRITE(IW(6),1000)
        DO 100 IEND= 1,2
           WRITE(IW(6),1001) NAM(IEND)
           DO 200 ISECT= 1,8
             DO 300 IPTY= 1,2
               DO 400 IROAD = 1,2
              IBT = 16*(IROAD-1) + 8*(IEND-1) + ISECT
              JPNTR=KROW(KSIXA,(IEND-1)*8+ISECT)+IPTY
              IADC = IBITS(IW(JPNTR),16*(IROAD-1),16)
              SIADC(2*(IPTY-1)+IROAD) = IADC/(SIFOXA*SIDIFO)
 400    CONTINUE
 300    CONTINUE
        IF ( MAX(SIADC(1),SIADC(2),SIADC(3),SIADC(4)).GT.0)
     $  WRITE(IW(6),1002) ISECT,SIADC
 200    CONTINUE
 100    CONTINUE
      ENDIF
 1000 FORMAT(//,' +++SIXAPR+++ Sical  TriggerSignals  (MeV)',//,
     * '                    odd  layers        even  layers    ',/,
     * '                   road1  road2      road1    road2   ',/)
 1001 FORMAT(' Endcap ',A,'    : ')
 1002 FORMAT(' Sector ',I5,4F10.1,/)
      RETURN
      END
#endif
