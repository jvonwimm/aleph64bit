      SUBROUTINE PHSTOJ (LIST,IER )
C----------------------------------------------------------------------
C!   Change HCAL POT banks into JULIA banks
C
C   Author   :D. SCHLATTER              5-NOV-1988
C   modified by: G.Capon , F.Ranjard    1-DEC-1988
C   Inputs:    banks:  PHST
C              LIST  /C         BOS event list
C                               if LIST(2:2).eq.'-' drop POT banks
C
C   Outputs:   IER              = 0 successful
C                               = 1 input bank does not exist or empty
C                               = 2 not enough space
C                               =-1 OK but garbage collection
C              banks:  HSDA, HSTO
C                      IF the number of storeys .lt. MXSTO(=1000) THEN
C                         fill HSDA word(JHSDNS)
C                         in this case  JDWORK is used
C                      ENDIF
C        -
C   Calls:     HFNDMD,BDROP,BKFMT,AUBOS
C              SORTZV from KERNLIB
C======================================================================
#ifndef DOC
      SAVE
#include "bcs.h"
#include "hsdajj.h"
#include "hstojj.h"
#include "phstjj.h"
      COMMON /LOCAL/ JDWORK
      CHARACTER*5 SYSTM
      PARAMETER (LHPOI=8, MXSTO=1000)
      REAL CORNR(3,11)
      INTEGER HNREG
      LOGICAL FIRST
      CHARACTER*(*) LIST, PLIST*4, JLIST*8
      DATA FIRST/.TRUE./
#include "bmacro.h"
C
      IF(FIRST) THEN
        FIRST=.FALSE.
        NAPHST=NAMIND('PHST')
        NAHSDA=NAMIND('HSDA')
        NAHSTO=NAMIND('HSTO')
        JDWORK = 0
        CALL BKFMT('HSDA','2I,(3I,F,5I)')
        CALL BKFMT('HSTO','2I,(33F,3I)')
      ENDIF
C
      IER = 1
      IF(IW(NAPHST).LE.0) GOTO 999
      NHSTO=LROWS (IW(NAPHST))
      IF(NHSTO.LE.0) GOTO 999
C
C           create HSDA bank
      LNHSD=LMHLEN+NHSTO*LHSDAA
      CALL AUBOS('HSDA',0,LNHSD,KHSDA,IER)
      IF (IER.EQ.2) GOTO 999
      JLIST = 'HSDA'
      IER1 = IER
      IW(KHSDA+LMHCOL)=LHSDAA
      IW(KHSDA+LMHROW)=NHSTO
C
      KPHST=IW(NAPHST)
      DO 100 IST=1,NHSTO
C?           get module #, subcomp #
        ITETHS=ITABL(KPHST,IST,JPHSTI)
        ISTKHS=1
        IF(ITETHS.GT.128) THEN
          ITETHS=ITETHS-128
          ISTKHS=2
        ENDIF
        IPHIHS=ITABL(KPHST,IST,JPHSPI)
C
        CALL HFNDMD(ITETHS,IPHIHS,ISTKHS,ISUBHS,IMODHS,IOVRHS)
C
        IW(KROW(KHSDA,IST)+JHSDTI)=ITETHS
        IW(KROW(KHSDA,IST)+JHSDPI)=IPHIHS
        IW(KROW(KHSDA,IST)+JHSDSN)=ISTKHS
        RW(KROW(KHSDA,IST)+JHSDDE)=RTABL(KPHST,IST,JPHSCE)
        IW(KROW(KHSDA,IST)+JHSDMN)=IMODHS
        IW(KROW(KHSDA,IST)+JHSDSC)=ISUBHS
        IW(KROW(KHSDA,IST)+JHSDRN)=HNREG(ITETHS)
        IW(KROW(KHSDA,IST)+JHSDCN)=ITABL(KPHST,IST,JPHSPH)
  100 CONTINUE
C
C -  get next storeys of a cluster/calobject when the number of storeys
C    is reasonable (max. number of storeys MXSTO=1000)
C
      IF (NHSTO .GT. MXSTO) GOTO 190
C
C      create a work bank JDWORK to sort HSDA
C      If not enough space skip this part: next storey is not filled
      CALL WBANK (IW,JDWORK,NHSTO,*190)
      DO 102 IST = 1,NHSTO
         IW(JDWORK+IST)=(IST-1)*LHSDAA+1
 102  CONTINUE
C
      JJHSDA=KHSDA+LMHLEN+JHSDCN-1
      IIHSDA=KHSDA+LMHLEN+JHSDNS-1
      CALL SORTZV(IW(JJHSDA+1),IW(JDWORK+1),NHSTO,-1,0,1)
C
      IW(IIHSDA+1)=0
      IF(NHSTO.GT.1) THEN
        DO 101 IST=2,NHSTO
          IW(IIHSDA+IW(JDWORK+IST))=0
          IF(IW(JJHSDA+IW(JDWORK+IST)).EQ.IW(JJHSDA+IW(JDWORK+IST-1)) )
     2     IW(IIHSDA+IW(JDWORK+IST-1))=IW(JDWORK+IST)/LHSDAA+1
  101   CONTINUE
      ENDIF
C
C          drop work bank JDWORK
      CALL WDROP (IW,JDWORK)
C
 190  CONTINUE
C
C           create HSTO bank
      LNHST=LMHLEN+NHSTO*LHSTOA
      CALL AUBOS('HSTO',0,LNHST,JHSTO,IER)
      IF (IER.EQ.2) GOTO 998
      IER2 = IER
      JLIST = JLIST(1:LNBLNK(JLIST)) // 'HSTO'
      JPHST=IW(NAPHST)
      IW(JHSTO+LMHCOL) = LHSTOA
      IW(JHSTO+LMHROW) = NHSTO
      KHSTO=JHSTO+LMHLEN
C
      DO 200 IST=1,NHSTO
C
        ITETHS=ITABL(JPHST,IST,JPHSTI)
        ISTKHS=1
        IF(ITETHS.GT.128) THEN
          ITETHS=ITETHS-128
          ISTKHS=2
        ENDIF
        IPHIHS=ITABL(JPHST,IST,JPHSPI)
C
C?           get module #, subcomp #
C
        CALL HFNDMD(ITETHS,IPHIHS,ISTKHS,ISUBHS,IMODHS,IOVRHS)
C
C?           compute coordinates of storey corners
C
        CALL VZERO(CORNR,33)
        SYSTM='ALEPH'
        CALL HSRCR(SYSTM,ITETHS,IPHIHS,ISTKHS,LHPOI,CORNR)
C
C?            compute coordinates of center of inner face,outer face,
C
        DO 11 K=1,3
           DO 12 N=1,4
              CORNR(K,9)=CORNR(K,9)+CORNR(K,N)/4.
              CORNR(K,10)=CORNR(K,10)+CORNR(K,N+4)/4.
   12      CONTINUE
           CORNR(K,11)=(CORNR(K,9)+CORNR(K,10))/2.
   11   CONTINUE
C
C?            fill HSTO bank
C
        I=0
        DO 20 K=1,3
           DO 20 N=1,11
              I=I+1
              RW(KHSTO+I)=CORNR(K,N)
   20   CONTINUE
C
        IW(KHSTO+JHSTSN)=ISUBHS
        IW(KHSTO+JHSTMN)=IMODHS
        IW(KHSTO+JHSTOF)=IOVRHS
        KHSTO=KHSTO+LHSTOA
 200  CONTINUE
C
 998  CONTINUE
C - get the drop flag if any, then drop POT banks if required,
C   add JUL banks to S-list
C   POT banks are on PLIST, JUL banks on JLIST
      PLIST = 'PHST'
#include "jplist.h"
C
      IF (IER1+IER2 .GT. 0) IER = -1
C
  999 CONTINUE
      END
#endif
