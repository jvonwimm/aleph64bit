      SUBROUTINE TSIRDP(IUNIT,ISLOT)
C
C------------------------------------------------------------
C! Dump the TSIR, TSLE, and TSDI banks (TPC wire raw data) to
C! unit IUNIT for sector slot number ISLOT
C
C  Author:   R. Johnson  13-8-92
C
C  PLEASE do not delete this routine from the library even
C  though it is not called by any other Julia routine.  It is
C  a useful debugging and testing tool.
C------------------------------------------------------------
#ifndef DOC
C
#include "tpgpar.h"
#include "tpgeom.h"
#include "bcs.h"
#include "tpccon.h"
C
      PARAMETER (MXPRNT=20)
      DIMENSION ISMPL(MXPRNT)
      DIMENSION IBTOR(0:7)
      DATA IBTOR/24,28,16,20,8,12,0,4/
C
      KTSIR=NLINK('TSIR',ISLOT)
      KTSLE=NLINK('TSLE',ISLOT)
      KTSDI=NLINK('TSDI',ISLOT)
      IF (KTSIR.EQ.0.OR.KTSDI.EQ.0) RETURN
      WRITE(IUNIT,1) ISLOT
 1    FORMAT(/' Dump of TPC TPP-reduced wire raw data for sector',
     &        I2/'     wire F   Q     time     T0     ',
     &        4X,'Z1',8X,'Z2',4X,' N      samples')
      NWHIT=IW(KTSIR)
      ISOF=0
      DO 201 IWH=1,NWHIT
        IHTWD=IW(KTSIR+IWH)
        IRDFG=IBITS(IHTWD,13,1)
        IWRNM=IBITS(IHTWD,24,8)
        IF (IRDFG.EQ.1) THEN
          IT0=0
          ICHRG=IBITS(IHTWD,14,10)*2
          TIM=FLOAT(IBITS(IHTWD,0,13))/16.
          IF (KTSLE.NE.0) THEN
            IWORD=KTSLE+1+(IWH-1)/8
            IBIT0=IBTOR(MOD(IWH-1,8))
            NWSMP=IBITS(IW(IWORD),IBIT0,4)
          ELSE
            NWSMP=0
          ENDIF
          NPRNT=0
          LENP=NWSMP
        ELSE
          IT0=IBITS(IHTWD,0,9)
          NWSMP=IBITS(IHTWD,16,8)
          TIM=FLOAT(2*IT0+NWSMP)/2.
          IFG14=IBITS(IHTWD,14,1)
          IFG15=IBITS(IHTWD,15,1)
          ICHRG=0
          IF (IFG14.EQ.0.AND.IFG15.EQ.0) THEN
            NPRNT=MIN(NWSMP,MXPRNT)
            LENP=0
            DO 14 I=1,NWSMP
              IWORD=KTSDI+1+(ISOF+I-1)/4
              IBIT0=24-8*MOD(ISOF+I-1,4)
              IPH=IBITS(IW(IWORD),IBIT0,8)
              IF (IPH.GT.2) THEN
                ICHRG=ICHRG+IPH
                LENP=LENP+1
              ENDIF
              IF (I.LE.MXPRNT) ISMPL(I)=IPH
 14         CONTINUE
            ISOF=ISOF+NWSMP
          ELSE
            NPRNT=0
            LENP=NWSMP-2
          ENDIF
        ENDIF
        ZPUL=(TIM-TVOFFS(ISLOT))*TVDRFT(IENDTP(ISLOT))
        ZP1=ZPUL - 0.5*FLOAT(LENP+4)*TVDRFT(IENDTP(ISLOT))
        ZP2=ZPUL + 0.5*FLOAT(LENP+4)*TVDRFT(IENDTP(ISLOT))
        WRITE(IUNIT,2) IWH,IWRNM,IRDFG,ICHRG,TIM,IT0,ZP1,ZP2,NWSMP,
     &                 (ISMPL(I),I=1,NPRNT)
  201 CONTINUE
C
 2    FORMAT(1X,I4,1X,I3,1X,I1,1X,I5,1X,F9.3,1X,I3,1X,F9.3,1X,F9.3,
     &       3X,I3,20(1X,I3))
C
      WRITE(IUNIT,3) ISLOT
 3    FORMAT(' End of TSIR dump for sector ',I2,2X,100('*'))
C
      END
#endif
