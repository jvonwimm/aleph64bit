      SUBROUTINE TKSGDP(KTGFT,ITK,IUNIT,ICTRL)
C----------------------------------------------------------------------
C! Dump single track
C!
C!   Author:    Z. Feng     10-03-87
C!   Modified:  R. Johnson   6-05-87
C!   Modified:  D. Cinabro  29-05-91
C!   Modified:  R. Johnson  24-08-92   Use pulse length banks
C!              R. Johnson   5-09-93   use TWRF instead of TWPU
C!              D. Casper    4-11-95   add pad dE/dx info
C!
C!   Called by TRKDMP
C!
C!   Input:
C!         - IUNIT   /I    Fortran output logical unit number
C!         - ITK     /I    Track number
C!         - KTGFT   /I    Offset to the BOS bank TGFT
C!         - ICTRL   /I    Control parameter
C!                         0:  print only track information
C!                         1:  include coordinates
C!                         2:  include wire hits
C!                         3:  include coordinates and wire hits
C!---------------------------------------------------------------------
#ifndef DOC
C
#include "tparac.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "tgtljj.h"
#include "tclujj.h"
#include "tcrljj.h"
#include "tlrljj.h"
#include "tpuljj.h"
#include "tgftjj.h"
#include "texsjj.h"
#include "tpxsjj.h"
#include "tpcojj.h"
#include "twtbjj.h"
#include "tpdxjj.h"
#include "twitjj.h"
#include "twirct.h"
#include "tpccon.h"
#include "bcs.h"
C
      PARAMETER (MWPHP=11)
      DIMENSION JWPH(MWPHP)
      DIMENSION A(5,5)
      LOGICAL L1415
C
C-----------------------------------------------------------------------
C
#include "bmacro.h"
C
C-----------------------------------------------------------------------
C
      WRITE(IUNIT,100) ITK,ITABL(KTGFT,ITK,JTGFTC)
  100 FORMAT(/' TKSGDP:  Dump of TPC track number ',I3,
     &          ', FRFT number ',I3)
C
C++   Dump TGFT Bank
C
      WRITE(IUNIT,102) (RW(KROW(KTGFT,ITK)+J),J=1,5)
      CHI = RW(KROW(KTGFT,ITK)+JTGFCD)
      ND =  IW(KROW(KTGFT,ITK)+JTGFDF)
      PRB = PROB(CHI,ND)
      CHID = CHI/FLOAT(ND)
      WRITE(IUNIT,103) CHID,PRB
  102 FORMAT(1X,' 1/Radius = ',E11.4,2X,' TanLambda= ',F10.4,2X,
     &            ' Phi0= ',F10.4,2X,' D0= ',F10.4,2X,' Z0= ',F10.4)
  103 FORMAT(1X,' Chisqr/dof= ',E11.4,2X,', Prob =',E11.4)
C
C++   Print the error matrix
C
      WRITE (IUNIT,2000)
 2000 FORMAT(4X,' The covariance matrix :')
      N0=0
      J0=0
      DO 11 I=1,5
        DO 10 J=I,5
          J0=J0+J-1
          A(I,J)=RW(KROW(KTGFT,ITK)+N0+J0+JTGFEM)
          A(J,I)=A(I,J)
   10   CONTINUE
        N0=N0+I
        J0=0
   11 CONTINUE
      WRITE(IUNIT,104) ((A(I,J),J=1,5),I=1,5)
  104 FORMAT(5(1X,E11.4))
C
C++   Print dE/dx information, if available
C
      KTEXS=IW(NAMIND('TEXS'))
      IF (KTEXS.NE.0) THEN
        WRITE(IUNIT,680) ITK
        WRITE(IUNIT,685)
        DO 674 ISG=1,LROWS(KTEXS)
          IF (ITABL(KTEXS,ISG,JTEXTN).EQ.ITK) THEN
            WRITE(IUNIT,683) ITABL(KTEXS,ISG,JTEXSI),
     &                           RTABL(KTEXS,ISG,JTEXTM),
     &                           RTABL(KTEXS,ISG,JTEXTL),
     &                           ITABL(KTEXS,ISG,JTEXNS),
     &                           RTABL(KTEXS,ISG,JTEXAD),
     &                           ITABL(KTEXS,ISG,JTEXSF)
          ENDIF
  674   CONTINUE
  683   FORMAT(6X,I2,4X,2(F12.5,2X),I3,5X,F12.5,3X,I1)
  685   FORMAT(3X,'Sector',5X,'Trunc. Mean',7X,'Length',4X,
     &         'N',9X,'drift','     SatFlg')
  680   FORMAT(' dE/dx results for track number ',I3,':')
      ENDIF
C
C++   Print pad dE/dx information, if available
C
      KTPXS=IW(NAMIND('TPXS'))
      IF (KTPXS.NE.0) THEN
        WRITE(IUNIT,1680) ITK
        WRITE(IUNIT,1685)
        DO 1674 ISG=1,LROWS(KTPXS)
          IF (ITABL(KTPXS,ISG,JTPXTN).EQ.ITK) THEN
            WRITE(IUNIT,1683) ITABL(KTPXS,ISG,JTPXSI),
     &                           RTABL(KTPXS,ISG,JTPXTM),
     &                           RTABL(KTPXS,ISG,JTPXTL),
     &                           ITABL(KTPXS,ISG,JTPXNS),
     &                           RTABL(KTPXS,ISG,JTPXAD)
          ENDIF
 1674   CONTINUE
 1683   FORMAT(6X,I2,4X,2(F12.5,2X),I3,5X,F12.5)
 1685   FORMAT(3X,'Sector',5X,'Trunc. Mean',7X,'Length',4X,
     &         'N',9X,'drift')
 1680   FORMAT(' Pad dE/dx results for track number ',I3,':')
      ENDIF
C
      IF (ICTRL.LT.1) GO TO 999
C
C++   Dump all coords and info in TPCO Bank associated with this track
C
      IF (ICTRL.NE.2) THEN
        KTPCO=IW(NAMIND('TPCO'))
        IF (KTPCO.EQ.0) GO TO 999
        KTGTL=IW(NAMIND('TGTL'))
        KTGCL=IW(NAMIND('TGCL'))
        IF (KTGTL.EQ.0 .OR. KTGCL.EQ.0) GO TO 999
        NPC1=ITABL(KTGTL,ITK,JTGTN1)
        NPC=NPC1+ITABL(KTGTL,ITK,JTGTNR)
        WRITE(IUNIT,105) NPC,ITK,NPC1
  105   FORMAT(/1X,I3,' TPC pad coordinates for track number ',I3/
     &         ' of which the first ',I2,' are on the first arc:')
        WRITE(IUNIT,106)
  106   FORMAT(2X,'IC',2X,'sec.',2X,' row',2X,' pad',5X,
     &         'radius',5X,' phi',6X,
     &         '   z',8X,'SigRPhi',5X,' SigZ ',3X,' Origin',
     &         2X,'track',1X,'clus',1X,'twin',2X,'sect R*Phi',4X,
     &         'sect Z')
C
        IOFF=ITABL(KTGTL,ITK,JTGTIO)
        DO 21 II=1,NPC
          IC=IW(KTGCL+LMHLEN+IOFF+II)
          IROW=ITABL(KTPCO,IC,JTPCIN)/100000
          IPAD=MOD(ITABL(KTPCO,IC,JTPCIN),1000)
          ISEC=MOD(ITABL(KTPCO,IC,JTPCIN),100000)/1000
          WRITE(IUNIT,107) IC,ISEC,IROW,IPAD,
     &                   (RTABL(KTPCO,IC,J),J=2,6),
     &                   (ITABL(KTPCO,IC,J),J=7,10),
     &                   (RTABL(KTPCO,IC,J),J=11,12)
  107     FORMAT(1X,I3,2X,I4,2X,I4,2X,I4,5(1X,F10.4),
     &           4X,I2,5X,3(2X,I3),2(1X,F10.4))
   21   CONTINUE
      ENDIF
C
      IF (ICTRL.LT.2) GO TO 999
C
C++   Dump all wire hits in TWTB Bank associated with this track
C
      KTWTB=IW(NAMIND('TWTB'))
      IF (KTWTB.EQ.0) GO TO 999
      KTWAT=IW(NAMIND('TWAT'))
      KTWIT=IW(NAMIND('TWIT'))
      IF (KTWAT.EQ.0 .OR. KTWIT.EQ.0) GO TO 999
      IF (IW(NAMIND('TWRR')).EQ.0) GO TO 999
C
      NWCG=ITABL(KTWIT,ITK,JTWING)
      NWCB=ITABL(KTWIT,ITK,JTWINB)
      WRITE(IUNIT,108) ITK,NWCG
  108 FORMAT(/' TPC wire pulses associated with track number ',I3/,
     &        1X,I3,' good pulses:')
      WRITE(IUNIT,109)
  109 FORMAT(1X,' IC',2X,'sector',2X,'wire',2X,'charge',
     &            2X,'path length',1X,' Z residual',1X,
     &            ' R position Z position',1X,'Len',
     &            ' NWSMP',2X,'                Samples')
C
      IOFF=ITABL(KTWIT,ITK,JTWIOS)
      DO 31 II=1,NWCG+NWCB
        IF (II.EQ.NWCG+1) THEN
          WRITE(IUNIT,142) NWCB,ITK
  142     FORMAT(/1X,I3,' bad pulses for track ',I2,':')
        ENDIF
        IC= IW(KTWAT+LMHLEN+IOFF+II)
        ID=ITABL(KTWTB,IC,JTWTWI)
        ISLOT=ID/65536
        KTWRR=NLINK('TWRR',ISLOT)
        IF (KTWRR.EQ.0) GO TO 31
        IPT=(ID-ISLOT*65536)
        ZFIT=RTABL(KTWTB,IC,JTWTPN)
        CHRG=RTABL(KTWTB,IC,JTWTCE)
        IF (CHRG.EQ.0.) THEN
          ZHIT=ZFIT
          IWIR=IPT
        ELSE
          IWHIT=IW(KTWRR+LMHLEN+IPT)
          TIME=FLOAT(IBITS(IWHIT,0,13))/TPACKW
          ZHIT=(TIME-TVOFFS(ISLOT))*TVDRFT(IENDTP(ISLOT))
          IWIR=IBITS(IWHIT,24,8)
        ENDIF
        ZRES=ZHIT-ZFIT
C
        KTWRF=NLINK('TWRF',ISLOT)
        KTWLE=NLINK('TWLE',ISLOT)
        KTWDI=NLINK('TSDI',ISLOT)
        KTSIR=NLINK('TSIR',ISLOT)
        IF (CHRG.GT.0. .AND. KTWLE.NE.0 .AND.
     &        KTSIR.NE.0 .AND. KTWRF.NE.0 .AND. KTWDI.NE.0) THEN
          IHT=IBITS(IW(KTWRF+LMHLEN+IPT),0,16)
          ISOFS=IBITS(IW(KTWRF+LMHLEN+IPT),16,16)
          IWRAW=IW(KTSIR+IHT)
          IF (IBITS(IWRAW,13,1).EQ.0) THEN
            NWSMP=IBITS(IWRAW,16,8)
            IT0=IBITS(IWRAW,0,9)
            L1415=IBITS(IWRAW,14,1).EQ.1.OR.IBITS(IWRAW,15,1).EQ.1
            IF (L1415) THEN
              NPRNT=0
            ELSE
              NPRNT=MIN(MWPHP,NWSMP)
            ENDIF
          ELSE
            IT0=-1
            NPRNT=0
            NWSMP=0
          ENDIF
C
C++       Get the sample pulse heights
C
          DO 202 IS=1,NPRNT
            IWORD=(ISOFS+IS-1)*LTPDNB/32+1
            IBYTE=3-MOD(ISOFS+IS-1,32/LTPDNB)
            ISMPL=IBITS(IW(KTWDI+IWORD),IBYTE*LTPDNB,LTPDNB)
            JWPH(IS)=ISMPL
  202     CONTINUE
        ELSE
          NPRNT=0
          NWSMP=0
        ENDIF
        LNPL=0
        IF (CHRG.GT.0.) THEN
          KTWLE=NLINK('TWLE',ISLOT)
          IF (KTWLE.NE.0) THEN
            IWORD=KTWLE+LMHLEN+1+(IPT-1)/4
            IBIT0=24-8*MOD(IPT-1,4)
            LNPL=IBITS(IW(IWORD),IBIT0,8)
          ENDIF
        ENDIF
        PLEN=RTABL(KTWTB,IC,JTWTSL)
        RPOS=RTABL(KTWTB,IC,JTWTRP)
        WRITE(IUNIT,110) IC,ISLOT,IWIR,CHRG,
     &          PLEN,ZRES,RPOS,ZFIT,LNPL,
     &          NWSMP,(JWPH(K),K=1,NPRNT)
  110   FORMAT(1X,I4,2X,I4,2X,I4,5(1X,F10.4),2X,I3,
     &                  1X,I3,4X,11(I3,1X))
   31 CONTINUE
C
C++   Dump all pad hits in TPDX Bank associated with this track
C
      KTPDX=NLINK('TPDX',ITK)
      IF (KTPDX.EQ.0) GO TO 33
C
      WRITE(IUNIT,1108) ITK,LROWS(KTPDX)
 1108 FORMAT(/' TPC pad samples associated with track number ',I3/,
     &        1X,I2,' good pulses:')
      WRITE(IUNIT,1109)
 1109 FORMAT(1X,' IC',2X,'sector',2X,'row',2X,' pad',2X,
     &            'Npads',2X,'Nwire',2X,'charge',2X,
     &            'path length',1X,'Z position',1X,'cos(alpha)',
     &            1X,'delta(rphi)')
C
      DO ISMP = 1, LROWS(KTPDX)
        INDX = ITABL(KTPDX,ISMP,JTPDIN)
        IROW = INDX/100000
        ISLOT = (INDX-IROW*100000)/1000
        IPAD = MOD(INDX,1000)
        WRITE(IUNIT,1110)ISMP,ISLOT,IROW,IPAD,
     &        ITABL(KTPDX,ISMP,JTPDNP),ITABL(KTPDX,ISMP,JTPDNW),
     &        RTABL(KTPDX,ISMP,JTPDTC),RTABL(KTPDX,ISMP,JTPDSL),
     &        RTABL(KTPDX,ISMP,JTPDRZ),RTABL(KTPDX,ISMP,JTPDCA),
     &        RTABL(KTPDX,ISMP,JTPDDR)
 1110 FORMAT(1X,I3,2X,I6,2X,I3,2X,I4,2X,I5,2X,I5,2X,F5.2,2X,
     &            F11.2,1X,F10.2,1X,F9.5,1X,F11.4)
      ENDDO
   33 CONTINUE
C
      WRITE(IUNIT,101) ITK
  101 FORMAT('------ End of TKSGDP dump of track Number ',I3,' ------')
  999 RETURN
      END

#endif
