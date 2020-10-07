      SUBROUTINE FITWRI
C-----------------------------------------------------------------------
C! Creation of pot banks PFRF PFRT PFXT from Julia banks FRFT FRTL FXTR
C
C   Author         L. Garrido    10-may-1988
C   Modified       R. Johnson    25-oct-1990
C   Modified       R. Johnson    21-jan-1991 to handle more than one
C                                FRFT bank (with different NR)
C   Modified       R. Johnson    06-aug-1991 to call FPKCM
C   Modified       D. Casper     01-dec-1995 pack FXTR -> PFXT
C
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "zpfrjj.h"
#include "pfrfjj.h"
#include "pfrtjj.h"
#include "frftjj.h"
#include "frtljj.h"
#include "fxtrjj.h"
#include "pfxtjj.h"
C
      LOGICAL FIRST
      DIMENSION CMPK(15)
      DATA FIRST/.TRUE./
C
#include "bmacro.h"
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NFRFT=NAMIND('FRFT')
        NPFRF=NAMIND('PFRF')
        NPFRT=NAMIND('PFRT')
        NZPFR=NAMIND('ZPFR')
        NFRTL=NAMIND('FRTL')
        NFXTR=NAMIND('FXTR')
        NPFXT=NAMIND('PFXT')
      ENDIF
C
      IF (IW(NPFRF).NE.0) CALL BDROP(IW,'PFRF')
      IF (IW(NPFRT).NE.0) CALL BDROP(IW,'PFRT')
      IF (IW(NPFXT).NE.0) CALL BDROP(IW,'PFXT')
C
      KFRFT = IW(NFRFT)
      KFRTL = IW(NFRTL)
      KFXTR = IW(NFXTR)
      IF (KFRFT.EQ.0 .OR. KFRTL.EQ.0) RETURN
      IF (KFXTR.EQ.0) CALL RERROR('FITWRI',4,'FXTR bank missing')
C
      NR = LROWS(KFRTL)
      IF (NR.EQ.0) RETURN
C
      KZPFR = IW(NZPFR)
      IF (KZPFR.EQ.0) THEN
        CC1=100.
        CC2=100.
        CALL RERROR('FITWRI',1,'ZPFR bank not found.  Defaults taken.')
      ELSE
        IZPFR=KROW(KZPFR,1)
        CC1=RW(IZPFR+JZPFC1)
        CC2=RW(IZPFR+JZPFC2)
      ENDIF
C
C     First do the PFRT pointer bank
C
      CALL AUBOS('PFRT',0,LMHLEN+ LPFRTA*NR,KPFRT,IGAR2)
      IF (IGAR2.EQ.2) GO TO 999
      KFRTL = IW(NFRTL)
C
C     If bank FRTL NR=2 exists, chose it to fill PFRT
C
      IF(NLINK('FRTL',2).NE.0) KFRTL = NLINK('FRTL',2)
C
      IW(KPFRT+LMHCOL)= LPFRTA
      IW(KPFRT+LMHROW)= NR
      DO 32 J=1,NR
        IPFRT=KROW(KPFRT,J)
        IFRTL=KROW(KFRTL,J)
        IW(IPFRT+JPFRNV)=IW(IFRTL+JFRTNV)
        IW(IPFRT+JPFRNI)=IW(IFRTL+JFRTNI)
        IW(IPFRT+JPFRNE)=IW(IFRTL+JFRTNE)
        IW(IPFRT+JPFRNT)=IW(IFRTL+JFRTNT)
        IW(IPFRT+JPFRNR)=IW(IFRTL+JFRTNR)
   32 CONTINUE
C
C     Now do all of the FRFT banks (NRs 0 and 1)
C
      KFRFT=IW(NFRFT)
   55 IF (KFRFT.EQ.0) GO TO 56
        NRIN=IW(KFRFT-2)
        IF (NR.NE.LROWS(KFRFT)) THEN
          CALL RERROR('FITWRI',3,'Mismatch in length between the '//
     &                'FRFT and FRTL banks')
          NR=LROWS(KFRFT)
        ENDIF
        CALL AUBOS('PFRF',NRIN,LMHLEN+LPFRFA*NR,KPFRF,IGAR1)
        IF (IGAR1.EQ.2) GO TO 999
        IF (IGAR1.NE.0) THEN
          KFRFT=NLINK('FRFT',NRIN)
        ENDIF
        IW(KPFRF+LMHCOL)= LPFRFA
        IW(KPFRF+LMHROW)= NR
        DO 1 J=1,NR
          IFRFT=KROW(KFRFT,J)
          IPFRF=KROW(KPFRF,J)
          CALL UCOPY(RW(IFRFT+JFRFIR),RW(IPFRF+JPFRIR),6)
          CALL FPKCM(RW(IFRFT+JFRFEM),CMPK)
          RW(IPFRF+JPFREO)=CMPK(1)
          RW(IPFRF+JPFREO+1)=CMPK(3)
          RW(IPFRF+JPFREO+2)=CMPK(6)
          RW(IPFRF+JPFREO+3)=CMPK(10)
          RW(IPFRF+JPFREO+4)=CMPK(15)
          RW(IPFRF+JPFREO+5)=RW(IFRFT+JFRFEM+20)
          IW(IPFRF+JPFREM)=INT(CMPK(2)*CC1+CC2)
          IW(IPFRF+JPFREM+1)=INT(CMPK(4)*CC1+CC2)
          IW(IPFRF+JPFREM+2)=INT(CMPK(5)*CC1+CC2)
          IW(IPFRF+JPFREM+3)=INT(CMPK(7)*CC1+CC2)
          IW(IPFRF+JPFREM+4)=INT(CMPK(8)*CC1+CC2)
          IW(IPFRF+JPFREM+5)=INT(CMPK(9)*CC1+CC2)
          IW(IPFRF+JPFREM+6)=INT(CMPK(11)*CC1+CC2)
          IW(IPFRF+JPFREM+7)=INT(CMPK(12)*CC1+CC2)
          IW(IPFRF+JPFREM+8)=INT(CMPK(13)*CC1+CC2)
          IW(IPFRF+JPFREM+9)=INT(CMPK(14)*CC1+CC2)
          DO 21 I=10,14
            IW(IPFRF+JPFREM+I)=0
   21     CONTINUE
          CMAX=CC1+CC2
          IF (RW(IFRFT+JFRFC2).GT.CMAX) THEN
            IW(IPFRF+JPFRC2)=INT(CMAX)
          ELSE
            IW(IPFRF+JPFRC2)=INT(RW(IFRFT+JFRFC2))
          ENDIF
          IF (IW(IPFRF+JPFRC2).LT.0) IW(IPFRF+JPFRC2)=0
C
C         Add number of degrees of freedom to an anyhow packed word
C         NDF can no longer be simply derived from the number of coordin
C         Also add a flag indicating that a new packing method is used.
C
          IW(IPFRF+JPFRNO)=100000+IW(IFRFT+JFRFNO)+1000*IW(IFRFT+JFRFDF)
    1   CONTINUE
        KFRFT=IW(KFRFT-1)
        GO TO 55
   56 CONTINUE
C
C   Pack the FXTR bank, suppressing rows with all zeroes in the interesting
C   places
C
      KFXTR = IW(NFXTR)
      IF (KFXTR.GT.0) THEN
        NR = LROWS(KFXTR)
        CALL AUBOS('PFXT',0,LMHLEN+LPFXTA*NR,KPFXT,IGAR1)
        IF (IGAR1.EQ.2) GO TO 999
        IF (IGAR1.EQ.1) KFXTR = IW(NFXTR)
        CALL BLIST(IW,'E+','PFXT')
        IW(KPFXT+LMHCOL) = LPFXTA
        IW(KPFXT+LMHROW) = 0
        DO IR = 1, NR
            IFXTR = KROW(KFXTR,IR)
            IF (IW(IFXTR+JFXTV0).NE.0 .OR. IW(IFXTR+JFXTNC).NE.0 .OR.
     &          IW(IFXTR+JFXTKN).NE.0)THEN
                IW(KPFXT+LMHROW) = IW(KPFXT+LMHROW) + 1
                IPFXT = KROW(KPFXT,IW(KPFXT+LMHROW))
                IW(IPFXT + JPFXTN) = IR
                IW(IPFXT + JPFXV0) = IW(IFXTR+JFXTV0)
                IW(IPFXT + JPFXNC) = IW(IFXTR+JFXTNC)
                IW(IPFXT + JPFXKN) = IW(IFXTR+JFXTKN)
                IW(IPFXT + JPFXVD) = IW(IFXTR+JFXTVD)
            ENDIF
        ENDDO
        CALL AUBPRS('PFXT')
      ENDIF
      RETURN
  999 CONTINUE
      CALL RERROR('FITWRI',2,'No room to create POT track banks.')
      CALL BDROP(IW,'PFRFPFRT')
      END
#endif
