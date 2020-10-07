      SUBROUTINE LHVSTA(ITYPE)
C-----------------------------------------------------------------------
C! Check the high voltage status bits
C!
C!   Author   : P. H. Hansen  891017
C!   Modified : J. Wear       891119
C!              J. Wear       900322  ! Added physics trigger check
C!   Modified : P. H. Hansen  960501  ! Use X1RG bank
C!                            Output:
C!                                      ITYPE=0 Nothing turned on
C!                                      ITYPE=1   ECAL+HCAL+LCAL HV on
C!                                                and no LOLE error
C!                                      ITYPE=2   also ITC+TPC HV on
C!                                                and basic triggers on
C!
C-----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "rcurnt.h"
#include "rflags.h"
#include "xtcnjj.h"
#include "lolejj.h"
C
      DIMENSION IHV(16)
      LOGICAL BTEST,HVSET,CASET,LOLST,XTGOK,XPTENB
      CHARACTER*4 CHAINT
      EXTERNAL XPTENB
      EXTERNAL NAMIND
      DATA LRUN /-9999/
C
#include "bmacro.h"
C---------------------------------------------------------------------
      ITYPE=0
      IF (FMCRUN) THEN
        ITYPE = 2
        GOTO 999
      ENDIF
      IHVBT=0
      HVSET=.FALSE.
      CASET=.FALSE.
      LOLST=.FALSE.
      KLOLE = IW(NAMIND('LOLE'))
      KXTCN = IW(NAMIND('XTCN'))
      KX1RG = IW(NAMIND('X1RG'))
      IF(KX1RG.NE.0) THEN
        DO 2 IROW=1,LROWS(KX1RG)
          IF(CHAINT(ITABL(KX1RG,IROW,1)).EQ.'TSR ') THEN
            IHVBT=ITABL(KX1RG,IROW,4)
            GOTO 3
          ENDIF
    2   CONTINUE
    3   CONTINUE
      ELSEIF(KXTCN.NE.0) THEN
        IHVBT=ITABL(KXTCN,1,JXTCTR)
      ELSE
        GOTO 999
      ENDIF
C
C Get the high voltage status bits
C The bits 1,2,3 are ECAL HV bits. 4 is LCAL.
C 5 is TPC dE/dx. 6 is ITC. 8,9,10 are HCAL.
      DO 33 I=1,16
        IHV(I)=0
        IF(BTEST(IHVBT,I-1)) IHV(I)=1
   33 CONTINUE
C
      IF(IHV(1).EQ.1 .AND. IHV(2).EQ.1 .AND. IHV(3).EQ.1
     &   .AND. IHV(4).EQ.1 .AND. IHV(8).EQ.1
     &   .AND. IHV(9).EQ.1 .AND. IHV(10).EQ.1 ) THEN
        CASET=.TRUE.
        IF(IHV(5).EQ.1.AND.IHV(6).EQ.1) HVSET=.TRUE.
      ENDIF
C
      IF(KLOLE.NE.0) THEN
        IF(ITABL(KLOLE,1,JLOLER).EQ.0) THEN
           LOLST=.TRUE.
        ENDIF
      ENDIF
      IF(CASET.AND.LOLST) ITYPE=1
      IF(HVSET.AND.LOLST.AND.XPTENB(IRUNRC)) ITYPE=2
      IF(FDEBRF.AND.JDBDRF(5).GE.2) THEN
          WRITE(IW(6),'(/,2X,A,/,2X,I5,5X,16I3,/)')
     &     '+++LHVSTA+++ High Voltage Check',
     &     ITYPE,IHV
        ENDIF
C
 999  CONTINUE
      END
#endif
