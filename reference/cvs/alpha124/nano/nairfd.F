      SUBROUTINE NAIRFD(I1,I2,DXY,DZ0,DZ2,DTH,RMA,ZMA,XMA,NC1,
     &                  DIN1,NC2,DIN2,P,IER)
CKEY NANO IN ALPHA /INTERNAL
C----------------------------------------------------------------------
C! Fast pair-finder from the NANO : look into bank NDGC
C!
C!   Author   :- Gerrit Graefe         25-APR-1994
C!   Modified :- Gerrit Graefe         23-JUN-1994 removed old/new track
C!                                                 number assignment
C!
C! Inputs: I1 = Alpha track number (in CHT section)
C!         I2 = Another ALPHA track (in CHT section)
C! Output: DXY = the distance(cm) in the xy plane at the closest approach
C!         DZ2 = the distance(cm) between the tracks in z at the xy point
C!         DZ0 = the distance(cm) between the tracks in z at the origin
C!         DTH = 0.0
C!         RMA = the rho value at the materializtion point
C!         ZMA = the z value at the materialization point
C!         XMA = the invariant mass of the tracks at the materializtion
C!               point assuming they are both electrons
C!         NC1(2) = 0
C!         DIN1(2) = 0
C!         P(3) = Summed momentum of the two tracks at the materialization
C!                point
C!         IER = 0 if all OK
C!               1 if error in calculation
C!
C!   Libraries required: ALPHA,BOS77
C!
C!   Description
C!   ===========
C!
C?   This subroutine looks in the NDGC bank if the both tracks given form a
C?   converted gamma. The routine is called inside ALPHA instead of PAIRFD
C?   if input comes from a NanoDst.
C?
C!======================================================================
#ifndef DOC
      IMPLICIT NONE
#include "qdecl.h"
#include "qcde.h"
#include "nbpdcl.h"
#include "nbnkpo.h"
#include "ndgcjj.h"
      INTEGER NLINK,NVERS,NAMIND
      INTEGER I,J,INDNGC,IROWN,IROWK,KRNDGC
      INTEGER IRET,IT1,IT2,IIT1,IIT2
      INTEGER I1,I2,IER,NC1,NC2,ITYP
      REAL    XCO,YCO,ZCO,XDUM,YDUM,ZDUM
      REAL    DXY,DZ0,DZ2,DTH,RMA,ZMA,XMA,DIN1,DIN2,P(3)
      INTEGER MASK2,MASK8
      DATA MASK2/3/,MASK8/255/
#include "qmacro.h"
C----------------------------------------------------------------------
C
C!..LINK TO BANK NDGC
C
      IER=0
      INDNGC=IW(NAMIND('NDGC'))
      IF(INDNGC.EQ.0)THEN
        IER=0
        RETURN
      ENDIF
      KONDGC=INDNGC
      KCNDGC=IW(KONDGC+1)
      KRNDGC=IW(KONDGC+2)
C
C!..SET EVERYTHING TO ZERO
C
      DXY=0.0
      DZ0=0.0
      DZ2=0.0
      XMA=0.0
      DTH=0.0
      RMA=0.0
      ZMA=0.0
      NC1=0
      NC2=0
      DIN1=0.0
      DIN2=0.0
      P(1)=0.0
      P(2)=0.0
      P(3)=0.0
      XCO=0.0
      YCO=0.0
      ZCO=0.0
C
C!..GET JULIA TRACK NUMBERS
C
      IROWK=0
      IT1=KTN(I1)
      IT2=KTN(I2)
      IROWN=KONDGC+LMHLEN
      DO 100 I=1,KRNDGC
C
C!..IF THIS PHOTON WASN'T FOUND BY QPAIRF, RETURN
C
        IF(IAND(ISHFT(IW(IROWN+JNDGDA),-16),MASK2).NE.0)GOTO 100
        IIT1=IAND(IW(IROWN+JNDGDA),MASK8)
        IIT2=IAND(ISHFT(IW(IROWN+JNDGDA),-8),MASK8)
        IF((IT1.EQ.IIT1.AND.IT2.EQ.IIT2).OR.(IT2.EQ.IIT1.AND.IT1.EQ.IIT2
     &    ))IROWK=IROWN
        IROWN=IROWN+KCNDGC
  100 CONTINUE
      IF(IROWK.EQ.0)GOTO 999
C
C!..FILL VARIABLES
C
      DXY=FLOAT(IW(IROWK+JNDGDX)) / 10000.0
      DZ2=FLOAT(IW(IROWK+JNDGDZ)) / 10000.0
      XMA=FLOAT(IW(IROWK+JNDGXM)) / 1000000.0
      RMA=SQRT((FLOAT(IW(IROWK+JNDGCX))/10000.0)**2+
     &         (FLOAT(IW(IROWK+JNDGCY))/10000.0)**2)
      ZMA=FLOAT(IW(IROWK+JNDGCZ))/10000.0
      P(1)=FLOAT(IW(IROWK+JNDGPX)) / 10000000.0
      P(2)=FLOAT(IW(IROWK+JNDGPY)) / 10000000.0
      P(3)=FLOAT(IW(IROWK+JNDGPZ)) / 10000000.0
      XCO=FLOAT(IW(IROWK+JNDGCX)) / 10000.0
      YCO=FLOAT(IW(IROWK+JNDGCY)) / 10000.0
      ZCO=FLOAT(IW(IROWK+JNDGCZ)) / 10000.0
      IER=0
  999 RETURN
C
C!..entry point to get coordinates of the conversion point
C
      ENTRY NAIRCP(XDUM,YDUM,ZDUM)
      XDUM=XCO
      YDUM=YCO
      ZDUM=ZCO
      RETURN
      END
#endif
