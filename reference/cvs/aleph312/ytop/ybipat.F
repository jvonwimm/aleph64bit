      SUBROUTINE YBIPAT(ITR,IBVDET,IBITPC,IBMVD,IBMIT,IBMTP,IFLAG,LDMP)
C----------------------------------------------------------*
C!    Find bit pattern of hits in VDET ITC and TPC
CKEY YTOP
C!    Author :     W. Manner 25/01/93
C!
C!    Description
C!    ===========
C!     For track ITR the bit pattern is found and stored in
C!     INTEGER arrays IBMVD, IBMIT, IBMTP 1 word/bit and packed
C!     in words IBVDET IBITPC
C!     If bank DTRA exists (MINI) it is used and unpacked.
C!     Otherwise the banks VDCO ITCO TPCO are used
C!     IBVDET gives a somewhat more detailed VDET bit pattern
C!     than DTRA. It is:
C!     bit 1:  1st r-phi hit in layer 1
C!     bit 2:  1st   z   hit in layer 1
C!     bit 3:  2nd r-phi hit in layer 1 (overlap)
C!     bit 4:  2nd   z   hit in layer 1 (overlap)
C!     bit 5:  1st r-phi hit in layer 2
C!     bit 6:  1st   z   hit in layer 2
C!     bit 7:  2nd r-phi hit in layer 2 (overlap)
C!     bit 8:  2nd   z   hit in layer 2 (overlap)
C!     If this pattern is filled from DTRA
C!     it is fudged.
C!     IBITPC is identical to DTRA.JDTRHO
C----------------------------------------------------------*
#ifndef DOC
      SAVE
      INTEGER IBMVD(8),IBMIT(8),IBMTP(21)
      REAL XYZ(3,4),RVDC(4),FVDC(4),ZVDC(4)
      LOGICAL LDMP
#include "itcojj.h"
#include "tpcojj.h"
#include "dtrajj.h"
#include "frtljj.h"
#include "ficljj.h"
#include "bcs.h"
#include "bmacro.h"
C ----------------------------------------------------------
      IFLAG=1
      IBVDET=0
      IBITPC=0
      CALL UZERO(IBMVD,1,8)
      CALL UZERO(IBMIT,1,8)
      CALL UZERO(IBMTP,1,21)
C if bank DTRA exists (MINI) fill bit pattern from DTRHO
      KDTRA=IW(NAMIND('DTRA'))
      IF(KDTRA.EQ.0) GO TO 100
      IWD=ITABL(KDTRA,ITR,JDTRHO)
      IBITPC=IWD
C Fudge VDET bit pattern
      IB=1
      DO IL=1,2
        IF(IAND(IWD,IB).NE.0) THEN
          IBMVD(1+(IL-1)*4) = 1
          IBVDET=IBVDET+2**((IL-1)*4)
          IBMVD(2+(IL-1)*4) = 1
          IBVDET=IBVDET+2**((IL-1)*4+1)
        ENDIF
        IB=IB*2
      ENDDO
C Fill ITC bit pattern
      DO IL=1,8
        IF(IAND(IWD,IB).NE.0) THEN
          IBMIT(IL)=1
        ENDIF
        IB=IB*2
      ENDDO

C Fill TPC bit pattern
      DO IL=1,21
        IF(IAND(IWD,IB).NE.0) THEN
          IBMTP(IL)=1
        ENDIF
        IB=IB*2
      ENDDO
#if defined(YDEBUG)
      IF(LDMP)
     +  WRITE(6,'('' DTRA'',I3,'' BITS'',4I2,2X,4I2,4X,8I2,4X,21I2)')
     +  ITR,IBMVD,IBMIT,IBMTP
#endif
      RETURN

  100 CONTINUE
      CALL YDEFRF(KFRFT0,KFRFT,KFRTL,IFAIL)
C Find VDET bit pattern
      CALL YVDCOF(ITR,NUVD,XYZ,RVDC,FVDC,ZVDC,NCOM,.FALSE.)
      KBI=0
      DO IV=1,NUVD
        KBI=KBI+1
        IF(KBI.LE.2.AND.RVDC(IV).GT.8.) KBI=3
        IF(XYZ(1,IV).LT.900.) THEN
          IBMVD(1+(KBI-1)*2) = 1
          IBVDET=IBVDET+2**((KBI-1)*2)
        ENDIF
        IF(XYZ(3,IV).LT.900.) THEN
          IBMVD(2+(KBI-1)*2) = 1
          IBVDET=IBVDET+2**(1+(KBI-1)*2)
        ENDIF
      ENDDO
      IF(IBMVD(1).EQ.1.AND.IBMVD(2).EQ.1) IBITPC=IBITPC+1
      IF(IBMVD(5).EQ.1.AND.IBMVD(6).EQ.1) IBITPC=IBITPC+2

C Find ITC bit pattern
      IOIT=ITABL(KFRTL,ITR,JFRTII)
      NNIT=ITABL(KFRTL,ITR,JFRTNI)
      KFICL=IW(NAMIND('FICL'))
      KITCO=IW(NAMIND('ITCO'))
      IF(KFRTL.EQ.0.OR.KFICL.EQ.0.OR.KITCO.EQ.0) RETURN
      DO IT=1,NNIT
        KOR=ABS(ITABL(KFICL,IOIT+IT,1))
        IWI=ITABL(KITCO,KOR,JITCWN)
        IB=IWI/1000
        IBMIT(IB)=1
        IBITPC=IBITPC+2**(IB-1) * 4
      ENDDO
C Find TPC bit pattern
      IOTP=ITABL(KFRTL,ITR,JFRTIT)
      NNTP=ITABL(KFRTL,ITR,JFRTNT)
      KFTCL=IW(NAMIND('FTCL'))
      KTPCO=IW(NAMIND('TPCO'))
      IF(KFTCL.EQ.0.OR.KTPCO.EQ.0) RETURN
      DO IT=1,NNTP
        KOR=ITABL(KFTCL,IOTP+IT,1)
        IWI=ITABL(KTPCO,KOR,JTPCIN)
        IB=IWI/100000
        IBMTP(IB)=1
        IBITPC=IBITPC+2**(IB-1) * 2**10
      ENDDO
#if defined(YDEBUG)
      IF(LDMP) THEN
        WRITE(6,'('' TR'',I3,'' BITS'',4I2,2X,4I2,4X,8I2,4X,21I2)')
     +    ITR,IBMVD,IBMIT,IBMTP
        WRITE(6,'('' TR'',I3,'' PATTERN VDET'',Z10,'' VIT '',Z10)')
     +    ITR,IBVDET,IBITPC
      ENDIF
#endif
      IFLAG=0
      END
#endif
