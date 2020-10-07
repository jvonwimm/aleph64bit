      SUBROUTINE MGINIT(LRGEO,IRUNR,IERR,NEWNR)
C
C----------------------------------------------------------*
C! Initialize MUON reconstruction
C!
C!    Author:   Weimin Wu/Giorgio Capon     10-06-1987
C!    modified :F.Ranjard                   26-02-1988
C!    Description
C!    ===========
C!    This routine is called once per run for
C!    initialization of the MUON reconstruction program.
C!
C!    called by READRN
C!
C!---------------------------------------------------------*
#ifndef DOC
      SAVE
#include "bcs.h"
#include "mjjpar.h"
#include "mupar1.h"
#include "mrddat.h"
#include "mbg1da.h"
#include "mmg1da.h"
#include "mcg1da.h"
#include "msg1da.h"
      INTEGER ALGTDB
      CHARACTER LISTM*44
      DATA LISTM/'MUONMBACMENCMMMCMSELMSLMMSBLMTYMMTBYMTEYMSSL'/
#include "bmacro.h"
C
      IBOS = ALGTDB (LRGEO,LISTM,IRUNR)
      IF (IBOS .EQ. 0) THEN
C     at least one bank is missing ==> IERR=1 and RETURN
         IERR = 1
         NEWNR = 0
      ELSEIF (IBOS .GT. 0) THEN
C     all banks are there and have not been reloaded ==> RETURN
         IERR = 0
         NEWNR = 0
      ELSE
C     all banks are there but at least 1 bank has been reloaded
C     ===> set the variables in common blocks
         IERR = 0
         NEWNR = 1
C
C    fill the constants charecterizing the Muon Chambers globally
C
         JMUON = IW(NAMIND('MUON'))
         WDEIMU=RTABL(JMUON,1,JMUOWE)
         OFTBMU=RTABL(JMUON,1,JMUOOT)
         XPITCH=RTABL(JMUON,1,JMUODX)
         YPITCH=RTABL(JMUON,1,JMUODY)
C
C        Same for all SLOTS
C
         DO 111 NSUB=1,3
            ZZOFFS(NSUB,1)=RTABL(JMUON,1,JMUOZI)
            ZZOFFS(NSUB,2)=RTABL(JMUON,1,JMUOZI+1)
 111     CONTINUE
C
C       For barrel  : NSUB=3
C
         JMBAC = IW(NAMIND('MBAC'))
         XXOFFS(3,1)=RTABL(JMBAC,1,JMBAXS)
         XXOFFS(3,2)=RTABL(JMBAC,1,JMBAXS+1)
         YYOFFS(3,1)=RTABL(JMBAC,1,JMBAYS)
         YYOFFS(3,2)=RTABL(JMBAC,1,JMBAYS+1)
C
C    WD16MU depends on WDEIMU and WDGP16 which comes from MBAC
C
         WD16MU(3)=2.*WDEIMU +RTABL(JMBAC,1,JMBAWG)
C
C       For middle angle : NSUB=2
C
         JMMMC = IW(NAMIND('MMMC'))
         XXOFFS(2,1)=RTABL(JMMMC,1,JMMMXS)
         XXOFFS(2,2)=RTABL(JMMMC,1,JMMMXS+1)
         YYOFFS(2,1)=RTABL(JMMMC,1,JMMMYS)
         YYOFFS(2,2)=RTABL(JMMMC,1,JMMMYS+1)
C
C    WD16MU depends on WDEIMU and WDGP16 which comes from MMMC
C
         WD16MU(2)=2.*WDEIMU +RTABL(JMMMC,1,JMMMWG)
C
C       For endcap : NSUB=1
C
         JMENC = IW(NAMIND('MENC'))
         XXOFFS(1,1)=RTABL(JMENC,1,JMENXS)
         XXOFFS(1,2)=RTABL(JMENC,1,JMENXS+1)
         YYOFFS(1,1)=RTABL(JMENC,1,JMENYS)
         YYOFFS(1,2)=RTABL(JMENC,1,JMENYS+1)
C
C    WD16MU depends on WDEIMU and WDGP16 which comes from MENC
C
         WD16MU(1)=2.*WDEIMU +RTABL(JMENC,1,JMENWG)
C
C      fill constants characterizing the subdetectors,namely
C      Middle Angle,Barrel,Endcap
C
C     Fill common block for Middle angle
C
         THICKM =RTABL(JMMMC,1,JMMMTZ)
         JMSLM = IW(NAMIND('MSLM'))
         DO 12 NSL=1,NSLMDA
            RINTEM(NSL)=RTABL(JMSLM,NSL,JMSLRM)
            INPHIM(NSL)=ITABL(JMSLM,NSL,JMSLIM)
            CENDVM(NSL)=RTABL(JMSLM,NSL,JMSLCM)
            WIDTHM(NSL)=RTABL(JMSLM,NSL,JMSLWZ)
            ZWIDTM(NSL)=RTABL(JMSLM,NSL,JMSLZW)
C
            ZMODUL(NSL)=RTABL(JMSLM,NSL,JMSLUM)
            IF(NSL.GT.10) ZMODUL(NSL)=RTABL(JMSLM,NSL,JMSLUM+1)
            IF(NSL.GT.19) ZMODUL(NSL)=RTABL(JMSLM,NSL,JMSLUM)
            IF(NSL.GT.29) ZMODUL(NSL)=RTABL(JMSLM,NSL,JMSLUM+1)
 12      CONTINUE
C
C        Fill common block for Barrel
C
         THICKB=RTABL(JMBAC,1,JMBATZ)
         JMSBL = IW(NAMIND('MSBL'))
         DO 15 NSL=1,NSLBAR
            RINTEB(NSL)=RTABL(JMSBL,NSL,JMSBRB)
            INPHIB(NSL)=1+MOD(NSL-1,12)
            CENDVB(NSL)=RTABL(JMSBL,NSL,JMSBCB)
            WIDTHB(NSL)=RTABL(JMSBL,NSL,JMSBWB)
            ZWIDTB(NSL)=RTABL(JMSBL,NSL,JMSBZB)
 15      CONTINUE
C
C       fill common block for Endcap
C
         THICKC=RTABL(JMENC,1,JMENTZ)
         JMSEL = IW(NAMIND('MSEL'))
         DO 16 NSL=1,NSLEND
            XWIDTH(NSL)=RTABL(JMSEL,NSL,JMSEXW)
            YWIDTH(NSL)=RTABL(JMSEL,NSL,JMSEYW)
            XCENTR(NSL)=RTABL(JMSEL,NSL,JMSEXC)
            IF (NSL.GT.8) XCENTR(NSL)=-XCENTR(NSL)
            YCENTR(NSL)=RTABL(JMSEL,NSL,JMSEYC)
            ZENCAP(NSL)=RTABL(JMSEL,NSL,JMSEZE)
 16      CONTINUE
C
C        fill common block for Special barrel
C
         JMSSL = IW(NAMIND('MSSL'))
         DO 17 NSL=1,NSLBSM
            WTPLBN(NSL)=RTABL(JMSSL,NSL,JMSSWL)
            XTPYBT(NSL)=RTABL(JMSSL,NSL,JMSSXY)
 17      CONTINUE
C
      ENDIF
C
      END
#endif
