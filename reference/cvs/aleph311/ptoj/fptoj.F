      SUBROUTINE FPTOJ (LIST,IER)
C------------------------------------------------------
C!    unpack track fit banks from POT
CKEY FPTOJ UNPACK FIT
C!  Author:   L. Garrido
C!  Modified: J.Sedgbeer 90/05/23. No. deg. of freedom unpacked from
C!                       PFRF bank (available from JUL240.21 onwards).
C!                       If not available use NDF calculated from
C!                       number of coords on track as before.
C             F.Ranjard  91/04/16 to unpack PFRF,NR=2 into FRFT,NR=2
C             R. Johnson 91/08/07 call FUPKCM to unpack covariance matrix
C             S. Haywood 92/03/04 Skip error matrix if not required.
C
C   Input:   banks PFRF,PFRT
C            LIST  BOS event list
C                  if LIST(2:2).eq.'-' drop POT banks
C
C            If a NOEM card is supplied, error matrix is not filled.
C
C   Output:  banks FRFT,FRTL
C            banks FTCL,FICL,FVCL
C                  meaningful only if coordinate banks exist
C     Output:    IER       = 0  successful
C                          = 1  input bank does not exist or is empty
C                          = 2  not enough space
C                          =-1  OK but garbage collection
C------------------------------------------------------------
#ifndef DOC
      SAVE
      CHARACTER*(*) LIST
#include "bcs.h"
#include "frftjj.h"
#include "frtljj.h"
#include "ftcljj.h"
#include "ficljj.h"
#include "vdcojj.h"
#include "fvcljj.h"
#include "pfrfjj.h"
#include "pfrtjj.h"
#include "zpfrjj.h"
      CHARACTER PLIST*8, JLIST*20, PRGNM*8, CHAINT*4
      DIMENSION CMPK(15)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
#include "bmacro.h"

      IF (FIRST) THEN
         NFRFT=NAMIND('FRFT')
         NFRTL=NAMIND('FRTL')
         NFTCL=NAMIND('FTCL')
         NFICL=NAMIND('FICL')
         NFVCL=NAMIND('FVCL')
         NPFRF=NAMIND('PFRF')
         NPFRT=NAMIND('PFRT')
         NTPCO=NAMIND('TPCO')
         NITCO=NAMIND('ITCO')
         NVDCO=NAMIND('VDCO')
         NZPFR=NAMIND('ZPFR')
         NNOEM=NAMIND('NOEM')
         CALL BKFMT('FRFT',' 2I,(28F,2I)')
         CALL BKFMT('FRTL','I')
         CALL BKFMT('FTCL','I')
         CALL BKFMT('FICL','I')
         CALL BKFMT('FVCL','I')
         NPRT=0
         FIRST=.FALSE.
      ENDIF
C
      IER = 1
      KPFRF = IW(NPFRF)
      IF (KPFRF.EQ.0) GOTO 999
      NR = LROWS(KPFRF)
      IF (NR.EQ.0) GOTO 999
      KZPFR = 0
      IF(NZPFR.NE.0)  KZPFR = IW(NZPFR)
      IF(KZPFR.EQ.0) THEN
        CC1=100.
        CC2=100.
      ELSE
        IZPFR=KROW(KZPFR,1)
        CC1=RW(IZPFR+JZPFC1)
        CC2=RW(IZPFR+JZPFC2)
      ENDIF
C
C     Creating banks
C
      KPFRF = NPFRF+1
 2    KPFRF = IW(KPFRF-1)
      IF (KPFRF.EQ.0) GOTO 10
      NBR = IW(KPFRF-2)
      CALL AUBOS('FRFT',NBR,LMHLEN+LFRFTA*NR,KFRFT,IER)
      IF (IER.EQ.2) GOTO 999
      JLIST = 'FRFT'
      IER1 = IER
      IF (IER .EQ. 1) KPFRF = NLINK ('PFRF',NBR)
      IW(KFRFT+LMHCOL)= LFRFTA
      IW(KFRFT+LMHROW)= NR
C
      DO 1 J=1,NR
        IFRFT=KROW(KFRFT,J)
        IPFRF=KROW(KPFRF,J)
        CALL UCOPY(RW(IPFRF+1),RW(IFRFT+1),6)
        RW(IFRFT+JFRFEM+ 0)=RW(IPFRF+JPFREO+0)**2
        RW(IFRFT+JFRFEM+ 2)=RW(IPFRF+JPFREO+1)**2
        RW(IFRFT+JFRFEM+ 5)=RW(IPFRF+JPFREO+2)**2
        RW(IFRFT+JFRFEM+ 9)=RW(IPFRF+JPFREO+3)**2
        RW(IFRFT+JFRFEM+14)=RW(IPFRF+JPFREO+4)**2
        RW(IFRFT+JFRFEM+20)=RW(IPFRF+JPFREO+5)**2
        RW(IFRFT+JFRFC2)=IW(IPFRF+JPFRC2)
        IPKWD=IW(IPFRF+JPFRNO)
        IPKFG=IPKWD/100000
        IPKWD = (IPKWD-IPKFG*100000)
        NDOF=IPKWD/1000
        IW(IFRFT+JFRFDF) = NDOF
        IW(IFRFT+JFRFNO) = IPKWD-1000*NDOF
C
C       If error matrix not required, skip to end - saves lots of time.
C
        IF (IW(NNOEM).GT.0) GOTO 1
        IF (IPKFG.EQ.0) THEN
C
C         Old method of packing the covariance matrix
C
          N1=0
          N2=0
          DO 21 I1=1,6
            DO 22 I2=1,I1
              N1=N1+1
              IF(I1.EQ.I2) GOTO 21
              N2=N2+1
              RW(IFRFT+JFRFEM+N1-1)=(FLOAT(IW(IPFRF+JPFREM+N2-1))-CC2)
     &        /CC1
              IF(RW(IFRFT+JFRFEM+N1-1).GT.1.)
     &                         RW(IFRFT+JFRFEM+N1-1)=.9999
              IF(RW(IFRFT+JFRFEM+N1-1).LT.-1.)
     &                         RW(IFRFT+JFRFEM+N1-1)=-.9999
              RW(IFRFT+JFRFEM+N1-1)=RW(IFRFT+JFRFEM+N1-1)
     &           *RW(IPFRF+JPFREO+I1-1)*RW(IPFRF+JPFREO+I2-1)
   22       CONTINUE
   21     CONTINUE
        ELSE
C
C         New, improved method of packing the covariance matrix
C
          CMPK(1)=RW(IPFRF+JPFREO)
          CMPK(3)=RW(IPFRF+JPFREO+1)
          CMPK(6)=RW(IPFRF+JPFREO+2)
          CMPK(10)=RW(IPFRF+JPFREO+3)
          CMPK(15)=RW(IPFRF+JPFREO+4)
          RW(IFRFT+JFRFEM+20)=RW(IPFRF+JPFREO+5)
          CMPK(2)=(FLOAT(IW(IPFRF+JPFREM))-CC2)/CC1
          CMPK(4)=(FLOAT(IW(IPFRF+JPFREM+1))-CC2)/CC1
          CMPK(5)=(FLOAT(IW(IPFRF+JPFREM+2))-CC2)/CC1
          CMPK(7)=(FLOAT(IW(IPFRF+JPFREM+3))-CC2)/CC1
          CMPK(8)=(FLOAT(IW(IPFRF+JPFREM+4))-CC2)/CC1
          CMPK(9)=(FLOAT(IW(IPFRF+JPFREM+5))-CC2)/CC1
          CMPK(11)=(FLOAT(IW(IPFRF+JPFREM+6))-CC2)/CC1
          CMPK(12)=(FLOAT(IW(IPFRF+JPFREM+7))-CC2)/CC1
          CMPK(13)=(FLOAT(IW(IPFRF+JPFREM+8))-CC2)/CC1
          CMPK(14)=(FLOAT(IW(IPFRF+JPFREM+9))-CC2)/CC1
          CALL FUPKCM(CMPK,RW(IFRFT+JFRFEM))
C
C         Covariance terms for the scattering angle are not used and are
C         just set to zero.
C
          DO 134 I=15,19
            RW(IFRFT+JFRFEM+I)=0.
  134     CONTINUE
        ENDIF
    1 CONTINUE
      GOTO 2
   10 CONTINUE
C
      CALL AUBOS('FRTL',0,LMHLEN+LFRTLA*NR,KFRTL,IER)
      IF (IER.EQ.2) GOTO 998
      JLIST = JLIST(1:LNBLNK(JLIST)) // 'FRTL'
      IER2 = IER
      KFRTL = IW(NFRTL)
      KPFRT = IW(NPFRT)
      KFRFT = IW(NFRFT)
      KVDCO = IW(NVDCO)
      IW(KFRTL+LMHCOL)= LFRTLA
      IW(KFRTL+LMHROW)= NR
      NPV=0
      NPI=0
      NPT=0
      DO 11 J=1,NR
        IFRTL=KROW(KFRTL,J)
        IPFRT=KROW(KPFRT,J)
        IFRFT=KROW(KFRFT,J)
        IOFFV = 0
        IF(KVDCO.NE.0) THEN
          DO 12 I=1,LROWS(KVDCO)
            IF(ITABL(KVDCO,I,JVDCTN) .EQ. J) THEN
              IOFFV = I-1
              GO TO 13
            ENDIF
  12      CONTINUE
  13      CONTINUE
        ENDIF
        IW(IFRTL+JFRTIV)=IOFFV
        IW(IFRTL+JFRTNV)=IW(IPFRT+JPFRNV)
        IW(IFRTL+JFRTII)=NPI
        IW(IFRTL+JFRTNI)=IW(IPFRT+JPFRNI)
        IW(IFRTL+JFRTNE)=IW(IPFRT+JPFRNE)
        IW(IFRTL+JFRTIT)=NPT
        IW(IFRTL+JFRTNT)=IW(IPFRT+JPFRNT)
        IW(IFRTL+JFRTNR)=IW(IPFRT+JPFRNR)
        NPV=NPV+IW(IPFRT+JPFRNV)
        NPI=NPI+IW(IPFRT+JPFRNI)+IW(IPFRT+JPFRNE)
        NPT=NPT+IW(IPFRT+JPFRNT)+IW(IPFRT+JPFRNR)
C for POTs which have not the NDF packed in PFRF get it from PFRT
        IF (IW(IFRFT+JFRFDF).EQ.0) THEN
           IW(IFRFT+JFRFDF) =
     +     2*(IW(IPFRT+JPFRNV)+IW(IPFRT+JPFRNI)+IW(IPFRT+JPFRNT))-5
         ENDIF
   11 CONTINUE
C
C       coordinate list only if unpacking for coordinates requested
C           TPC  list
         CALL AUBOS('FTCL',0,LMHLEN+LFTCLA*NPT,KFTCL,IER)
         IF(IER.EQ.2) GOTO 998
         JLIST = JLIST(1:LNBLNK(JLIST)) // 'FTCL'
         IER3 = IER
         IW(KFTCL+LMHCOL)= LFTCLA
         IW(KFTCL+LMHROW)= NPT
         DO 33 I=1,NPT
           IW(KFTCL+LMHLEN+I)=I
   33    CONTINUE
C              ITC list
        CALL AUBOS('FICL',0,LMHLEN+LFICLA*NPI,KFICL,IER)
        IF(IER.EQ.2) GOTO 998
        JLIST = JLIST(1:LNBLNK(JLIST)) // 'FICL'
        IER4 = IER
        IW(KFICL+LMHCOL)= LFICLA
        IW(KFICL+LMHROW)= NPI
        DO 32 I=1,NPI
          IW(KFICL+LMHLEN+I)=I
   32   CONTINUE
C              VERTEX list
        CALL AUBOS('FVCL',0,LMHLEN+LFVCLA*NPV,KFVCL,IER)
        IF(IER.EQ.2) GOTO 998
        JLIST = JLIST(1:LNBLNK(JLIST)) // 'FVCL'
        IER5 = IER
        IW(KFVCL+LMHCOL)= LFVCLA
        IW(KFVCL+LMHROW)= NPV
        DO 31 I=1,NPV
          IW(KFVCL+LMHLEN+I)=I
   31   CONTINUE
C
  998 CONTINUE
C - get the drop flag if any, then drop POT banks if required,
C   add JUL banks to S-list
C   POT banks are on PLIST, JUL banks on JLIST
      PLIST = 'PFRTPFRF'
#include "jplist.h"
C
C - set IER=-1 in case of garbage collection
      IF (IER1+IER2+IER3+IER4+IER5 .GT. 0) IER = -1
C
 999  RETURN
      END
#endif
