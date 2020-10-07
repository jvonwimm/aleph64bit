      SUBROUTINE TWRRDP(SLOT,UNIT)
C-----------------------------------------------------------------------
C!Dump TPC raw and reduced wire data for sector SLOT
C Please do not delete this routine from the library, even though
C it is not referenced by any other routine---it is essential for
C debugging purposes.
C
C Author:  R. Johnson    9-4-93
C
C-----------------------------------------------------------------------
#ifndef DOC
C
      IMPLICIT NONE
C
#include "bcs.h"
C
      INTEGER MXPRNT
      PARAMETER (MXPRNT=20)
      INTEGER PH(MXPRNT),IHIT,KTSIR,KTSDI,KTWLE,KTWRR,I,T0,NBUCK,IHTWD
      INTEGER WIRNB,OFFSET,WORD,BIT,IT,IQ,PLEN,NLINK,FLG,SLOT,IRED
      INTEGER IWIR,NPRNT,UNIT,IRAW,KTWRF,IBITS
      LOGICAL SMT
      REAL T
C
C TSIR and TSDI are the raw data produced in the TPP (or in TWRRED)
C TWRR and TWLE are produced in the Julia routine TWIRES
C
      KTWRF=NLINK('TWRF',SLOT)
      IF (KTWRF.EQ.0) THEN
        WRITE(UNIT,1)
    1   FORMAT(/' TWRRDP:  cannot find TWRF.  Did you remember to ',
     &          'include the TOPT ''TWPU'' card?'/)
        RETURN
      ENDIF
      KTSDI=NLINK('TSDI',SLOT)
      KTWLE=NLINK('TWLE',SLOT)
      KTWRR=NLINK('TWRR',SLOT)
      KTSIR=NLINK('TSIR',SLOT)
      IF (KTSIR.EQ.0.OR.KTWLE.EQ.0.OR.KTWRR.EQ.0.OR.KTSDI.EQ.0)
     &  GOTO 999
C
      WRITE(UNIT,7) SLOT
 7    FORMAT(/' Dump of TSIR/TSDI/TWRR/TWLE wire hits in sector ',I2/
     &       ' Pulses appearing twice have been split by TWIRES.',/
     &       ' TWRR  TSIR  Wire Red T0  T     Plen  Q  FLG ',
     &       ' N                Samples')
      DO 100 IHIT=1,IW(KTWRR+LMHROW)
        WORD=KTWRF+LMHLEN+IHIT
        IRAW=IBITS(IW(WORD),0,16)
        OFFSET=IBITS(IW(WORD),16,16)
        IHTWD=IW(KTSIR+IRAW)
        WIRNB=IBITS(IHTWD,24,8)
        IRED=IBITS(IHTWD,13,1)
        NPRNT=0
        IF (IRED.EQ.0) THEN
          SMT=IBITS(IHTWD,15,1).EQ.1.OR.IBITS(IHTWD,14,1).EQ.1
          T0=IBITS(IHTWD,0,9)
          NBUCK=IBITS(IHTWD,16,8)
          IF (.NOT.SMT) THEN
            NPRNT=MIN(NBUCK,MXPRNT)
            DO 56 I=1,NPRNT
              WORD=KTSDI + (OFFSET+I-1)/4 + 1
              BIT=24-8*MOD(OFFSET+I-1,4)
              PH(I)=IBITS(IW(WORD),BIT,8)
 56         CONTINUE
          ENDIF
        ELSE
          T0=0
          NBUCK=0
        ENDIF
        IT=IBITS(IW(KTWRR+2+IHIT),0,13)
        T=FLOAT(IT)/16.
        IQ=IBITS(IW(KTWRR+2+IHIT),14,10)*2
        FLG=IBITS(IW(KTWRR+2+IHIT),13,1)
        IWIR=IBITS(IW(KTWRR+2+IHIT),24,8)
        IF (IWIR.NE.WIRNB) THEN
          WRITE(UNIT,9)
    9     FORMAT(' WARNING: TWRR and TSIR are out of sequence!')
        ENDIF
        WORD=KTWLE+2+(IHIT-1)/4+1
        BIT=24-8*MOD(IHIT-1,4)
        PLEN=IBITS(IW(WORD),BIT,8)
        WRITE(UNIT,8) IHIT,IRAW,WIRNB,IRED,T0,T,PLEN,IQ,FLG,NBUCK,
     &                              (PH(I),I=1,NPRNT)
 8      FORMAT(1X,2(1X,I4),3(1X,I3),2X,F6.2,1X,I3,1X,I4,1X,
     &                               I2,1X,I3,3X,20(I3,1X))
 100  CONTINUE
      WRITE(UNIT,101) SLOT
 101  FORMAT(' ****** End of dump of TPC wires in sector ',I2,
     &       ' **************************************************',/)
C
  999 RETURN
      END
#endif
