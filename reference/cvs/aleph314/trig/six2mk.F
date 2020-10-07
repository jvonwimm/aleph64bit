      SUBROUTINE SIX2MK
C***********************************************************************
C                                                                      *
C   Author   :- Joe Boudreau          30-OCT-1991                      *
C                                                                      *
C! Build the SIX2 bank from SIXA and SITC                              *
C                                                                      *
C   Inputs        :  SIXA bank,SITC bank                               *
C   Outputs       :  SIX2 bank                                         *
C                                                                      *
C***********************************************************************
#ifndef DOC
C
#include "bcs.h"
#include "six2jj.h"
C
C
      LOGICAL BTEST
      INTEGER VLOW,LOW,HIGH,VHIGH
      PARAMETER ( VLOW=0,LOW=1,HIGH=2,VHIGH=3)
      INTEGER IEND,IPTY,SXBT(2,2),KSIX2,JPNT
      INTEGER ECABT,ECBBT
C
C  Discriminate the sector and return a "Thermometer code" for each end
C  cap odd and each endcap even.
C
      DO 100 IEND = 1,2
          DO 200 IPTY = 1,2
              CALL SIDISC(IEND,IPTY,SXBT(IEND,IPTY))
 200      CONTINUE
 100  CONTINUE
C
C     Create the SIX2 bank with fixed length
C
      LEN = LMHLEN + LSIX2A
      CALL AUBOS('SIX2',0,LEN,KSIX2,IGARB)
      IF (KSIX2.LE.0) RETURN
      IW(KSIX2 + LMHCOL) = LSIX2A
      IW(KSIX2 + LMHROW) = 1
      JPNT  = KSIX2 + LMHLEN +1
C
C  Make the six basic SICAL triggers and store them in the bank
C
      ECABT = IOR(SXBT(1,1),SXBT(1,2))
      ECBBT = IOR(SXBT(2,1),SXBT(2,2))
      IF (BTEST(ECABT,HIGH).AND.BTEST(ECBBT,LOW))
     .                              IW(JPNT)=IBSET(IW(JPNT),0)
      IF (BTEST(ECABT,LOW).AND.BTEST(ECBBT,HIGH))
     .                              IW(JPNT)=IBSET(IW(JPNT),1)
      IF (BTEST(ECABT,VHIGH))  IW(JPNT)=IBSET(IW(JPNT),2)
      IF (BTEST(ECBBT,VHIGH))  IW(JPNT)=IBSET(IW(JPNT),3)
      IF (BTEST(ECABT,VLOW))   IW(JPNT)=IBSET(IW(JPNT),4)
      IF (BTEST(ECBBT,VLOW))   IW(JPNT)=IBSET(IW(JPNT),5)
C
C     Store the thermometer codes
C
      CALL MVBITS(SXBT(1,1),0,4,IW(JPNT), 6)
      CALL MVBITS(SXBT(1,2),0,4,IW(JPNT),10)
      CALL MVBITS(SXBT(2,1),0,4,IW(JPNT),14)
      CALL MVBITS(SXBT(2,2),0,4,IW(JPNT),18)
C
      RETURN
      END
#endif
