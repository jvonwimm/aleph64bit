      SUBROUTINE FRFTDP(ITK,IUNIT,ICTRL)
C----------------------------------------------------------------------
C! Dump single track from the FRFT bank along with coordinates and
C! associated wire hits.
CKEY PRINT TRACKS TPC ITC VDET
C!
C!   Author:      R. Johnson  29-06-88
C!   modified by  F. Ranjard  24-02-92
C!                call ABRUEV to get the run number
C!                call TPDVEL to get drift velocity
C!                R. Johnson   5-09-93  use TWRF instead of TWPU
C!                F.Ranjard   27-11-95  call TPDHYP instead of TIDHYP
C!                W.Wiedenmann 12.05.99  Read clock frequency in Hz 
C!                                       from TSOR bank
C!
C!   Called by FTKDMP
C!
C!   Input:
C!         - ITK     /I    Track num
C!         - IUNIT   /I    Fortran output logical unit number
C!         - ICTRL   /I    Control parameter
C!                         0:  print only track information
C!                         1:  include coordinates
C!                         2:  include wire hits
C!                         3:  include coordinates and wire hits
C!                         NOTE that wire hits generally are not
C!                              available from the POT
C!---------------------------------------------------------------------
#ifndef DOC
      SAVE
C
#include "tsimjj.h"
#include "alcons.h"
#include "frftjj.h"
#include "frtljj.h"
#include "texsjj.h"
#include "tpcojj.h"
#include "twtbjj.h"
#include "twitjj.h"
#include "twrcjj.h"
#include "tpgpar.h"
#include "tpgeom.h"
#include "tsorjj.h"
#include "t0gljj.h"
#include "t0rljj.h"
#include "bcs.h"
C
      REAL DVA(3),DVB(3)
      PARAMETER (MWPHP=11,NHYP=4,LTPDNB=8)
      DIMENSION JWPH(MWPHP),RMHYP(NHYP),TVOFS(LTSECT),DV(3)
      INTEGER AGETDB
      LOGICAL L1415
      DATA RMHYP/.000511,.13957,.49367,.93828/
      DATA NAFRFT/ 0/
C
C-----------------------------------------------------------------------
C
#include "bmacro.h"
C
C-----------------------------------------------------------------------
C
      IF (NAFRFT.EQ.0) THEN
         NAFRFT = NAMIND('FRFT')
         NAFVCL = NAMIND('FVCL')
         NAVDCO = NAMIND('VDCO')
         NATEXS = NAMIND('TEXS')
         NAFRTL = NAMIND('FRTL')
         NAFTCL = NAMIND('FTCL')
         NATPCO = NAMIND('TPCO')
         NAFICL = NAMIND('FICL')
         NAITCO = NAMIND('ITCO')
         NATWTB = NAMIND('TWTB')
         NATWAT = NAMIND('TWAT')
         NATWIT = NAMIND('TWIT')
         NATWRR = NAMIND('TWRR')
         NATWTT = NAMIND('TWTT')
         NATWRC = NAMIND('TWRC')
         NATSIM = NAMIND('TSIM')
         NATSOR = NAMIND('TSOR')
         NAT0GL = NAMIND('T0GL')
         NAT0RL = NAMIND('T0RL')
         IROLD = 0
      ENDIF
      KFRFT=IW(NAFRFT)
      IF (KFRFT.EQ.0) GO TO 999
      IF (ITK.LT.1 .OR. ITK.GT.LROWS(KFRFT)) GO TO 999
C
      CALL ABRUEV (IRUN,IEVT)
C
      WRITE(IUNIT,100) ITK,IRUN,IEVT
  100 FORMAT(/' FRFTDP:  Dump of ALEPH track number ',I3,
     &        ' for run ',I5,', event ',I6)
C
C++   Dump FRFT Bank
C
      WRITE(IUNIT,102) (RTABL(KFRFT,ITK,J),J=1,6)
      CHI = RTABL(KFRFT,ITK,JFRFC2)
      ND =  ITABL(KFRFT,ITK,JFRFDF)
      IF (ND.GT.0) THEN
        PRB = PROB(CHI,ND)
      ELSE
        PRB = -1.
      ENDIF
      WRITE(IUNIT,103) CHI,ND,PRB
  102 FORMAT(1X,'1/R=',E11.4,'  TanL=',F9.4,
     &          '  Phi0=',F8.5,'  D0=',F10.5,'  Z0=',F10.4,
     &          '  Theta=',F8.5)
  103 FORMAT(1X,' Chisqr/dof= ',E8.2,'/',I2,',  Prob =',E10.3)
C
      RI=RTABL(KFRFT,ITK,JFRFIR)
      IF (RI.NE.0.) THEN
        RAD=1./RI
      ELSE
        RAD=1.0E20
      ENDIF
      FIELD=ALFIEL(DUM)
      PT=RAD*CLGHT*FIELD/100000.
      TANL=RTABL(KFRFT,ITK,JFRFTL)
      SECL=SQRT(1.0+TANL**2)
      SINL=TANL/SECL
      P=ABS(PT)*SECL
      DPDRI= -P*RAD
      DPDTL= PT*SINL
      SRI=RTABL(KFRFT,ITK,JFRFEM)
      STL=RTABL(KFRFT,ITK,JFRFEM+2)
      SCOR=RTABL(KFRFT,ITK,JFRFEM+1)
      SGP=SQRT((DPDRI**2)*SRI + (DPDTL**2)*STL
     &                + 2.0*(DPDRI*DPDTL)*SCOR)
      WRITE(IUNIT,921) P,SGP,PT
  921 FORMAT('  Momentum=',E12.5,'+-',E12.5,' GeV,   pt=',E12.5,' GeV')
C
C++   Print the error matrix
C
      WRITE (IUNIT,2000)
 2000 FORMAT(4X,'The covariance matrix:')
      WRITE(IUNIT,104) (RTABL(KFRFT,ITK,JFRFEM-1+J),J=1,21)
  104 FORMAT(4X,E10.3/
     &       4X,E10.3,2X,E10.3/
     &       4X,E10.3,2X,E10.3,2X,E10.3/
     &       4X,E10.3,2X,E10.3,2X,E10.3,2X,E10.3/
     &       4X,E10.3,2X,E10.3,2X,E10.3,2X,E10.3,2X,E10.3/
     &       4X,E10.3,2X,E10.3,2X,E10.3,2X,E10.3,2X,E10.3,2X,E10.3)
C
C++   Print dE/dx information, if available
C
      KTEXS=IW(NATEXS)
      IF (KTEXS.NE.0) THEN
        WRITE(IUNIT,680) ITK
        WRITE(IUNIT,685)
        DO 674 ISG=1,LROWS(KTEXS)
          IF (ITABL(KTEXS,ISG,JTEXTN).EQ.ITK) THEN
            WRITE(IUNIT,683) ITABL(KTEXS,ISG,JTEXSI),
     &                       RTABL(KTEXS,ISG,JTEXTM),
     &                       RTABL(KTEXS,ISG,JTEXTL),
     &                       ITABL(KTEXS,ISG,JTEXNS),
     &                       RTABL(KTEXS,ISG,JTEXAD)
          ENDIF
  674   CONTINUE
  683   FORMAT(6X,I2,4X,2(F12.5,2X),I3,5X,F12.5)
  685   FORMAT(3X,'  Sector',3X,'Trunc. Mean',7X,'Length',4X,
     &         'N',7X,'drift')
  680   FORMAT(/4X,'dE/dx results for track number ',I3,':')
C
C++     Mass hypotheses:
C
        WRITE(IUNIT,432)
        DO 431 IHYP=1,NHYP
          CALL TPDHYP('WIRE',ITK,FIELD,1,RMHYP(IHYP),1.,DEDX,XNS,
     &                 S,DEDXP,SDEDXP,IER)
          IF (IER.GT.0) GO TO 431
          CHI2= ((DEDX-DEDXP)/SDEDXP)**2
          WRITE(IUNIT,433) RMHYP(IHYP),DEDX,XNS,S,DEDXP,SDEDXP,CHI2
  433     FORMAT(4X,F8.6,2X,F8.3,2X,F5.0,2X,F8.3,2X,F8.3,2X,F8.3,
     &           2X,F8.2)
  432     FORMAT(4X,'Mass hypotheses for dE/dx:'/
     &           4X,'  mass  ',2X,'  de/dx ',2X,'  N ',2X,
     &           ' length ',2X,'expected',2X,' sigma  ',2X,' chi**2')
  431   CONTINUE
      ENDIF
C
      IF (ICTRL.LT.1) GO TO 999
C
C++   Dump all coords and info in TPCO Bank associated with this track
C
      KFRTL=IW(NAFRTL)
      KFTCL=IW(NAFTCL)
      KTPCO=IW(NATPCO)
      IF (ICTRL.NE.2 .AND. KTPCO.NE.0 .AND. KFRTL.NE.0
     &                                     .AND. KFTCL.NE.0) THEN
        WRITE(IUNIT,105) ITK
  105   FORMAT(/,4X,'TPC pad coordinates for track number ',I3,':')
        WRITE(IUNIT,106)
  106   FORMAT(6X,'IC',2X,'sec.',2X,' row',2X,' pad',5X,
     &         'radius',5X,' phi',6X,
     &         '   z',8X,'SigRPhi',5X,' SigZ ',3X,' Origin',
     &         2X,' loop',1X,'clus',1X,'twin')
C
        NTOT=ITABL(KFRTL,ITK,JFRTNT)+ITABL(KFRTL,ITK,JFRTNR)
        DO 21 II=1,NTOT
          IC=IW(KFTCL+LMHLEN+ITABL(KFRTL,ITK,JFRTIT)+II)
          IROW=IW(KROW(KTPCO,IC)+JTPCIN)/100000
          IPAD=MOD(IW(KROW(KTPCO,IC)+JTPCIN),1000)
          ISEC=MOD(IW(KROW(KTPCO,IC)+JTPCIN),100000)/1000
          IF (II.LE.ITABL(KFRTL,ITK,JFRTNT)) THEN
            ILOOP=0
          ELSE
            ILOOP=1
          ENDIF
          WRITE(IUNIT,107) IC,ISEC,IROW,IPAD,
     &                   (RTABL(KTPCO,IC,J),J=2,6),
     &                   ITABL(KTPCO,IC,JTPCOF),ILOOP,
     &                   (ITABL(KTPCO,IC,J),J=9,10)
  107     FORMAT(5X,I3,2X,I4,2X,I4,2X,I4,5(1X,F10.4),
     &           4X,I2,5X,3(2X,I3))
   21   CONTINUE
      ENDIF
C
C++   Dump all coords and info in ITCO Bank associated with this track
C
      KFICL=IW(NAFICL)
      KITCO=IW(NAITCO)
      IF (ICTRL.NE.2 .AND. KITCO.NE.0 .AND. KFRTL.NE.0
     &                                     .AND. KFICL.NE.0) THEN
        WRITE(IUNIT,108) ITK
        DO 721 II=1,ITABL(KFRTL,ITK,JFRTNI)
          IC=IABS(IW(KFICL+LMHLEN+ITABL(KFRTL,ITK,JFRTII)+II))
          WRITE(IUNIT,835) ITABL(KITCO,IC,1),
     &                     (RTABL(KITCO,IC,J),J=2,8)
  835     FORMAT(4X,I6,2X,F6.2,2X,F8.5,2X,F8.5,2X,F6.1,2X,
     &           F8.6,2X,E10.3,2X,F6.1)
  108     FORMAT(/,4X,'ITC coordinates for track number ',I3,':'/
     &           4X,' wire ',2X,'radius',2X,'  phi1  ',2X,'  phi2  ',
     &           2X,' zhit ',2X,'sig(rphi)',2X,' sig(z) ',3X,'drift')
  721   CONTINUE
      ENDIF
C
C++   Dump all coords and info in VDCO Bank associated with this track
C
      KFVCL=IW(NAFVCL)
      KVDCO=IW(NAVDCO)
      IF (ICTRL.NE.2 .AND. KVDCO.NE.0 .AND. KFRTL.NE.0
     &                                     .AND. KFVCL.NE.0) THEN
        WRITE(IUNIT,769) ITK
  769   FORMAT(/,4X,'VDET coordinates for track number ',I3,':'/
     &         4X,'wafer ',2X,' radius  ',2X,'  phi     ',2X,
     &         '    z   ',2X,' sig(rphi)',2X,' sig(z)   ',2X,
     &         'quality')
        DO 770 II=1,ITABL(KFRTL,ITK,JFRTNV)
          IC=IW(KFVCL+LMHLEN+ITABL(KFRTL,ITK,JFRTIV)+II)
          WRITE(IUNIT,771) ITABL(KVDCO,IC,1),
     &                     (RTABL(KVDCO,IC,J),J=2,6),
     &                     ITABL(KVDCO,IC,8)
  771     FORMAT(4X,I6,2X,F9.5,2X,F10.7,2X,F8.4,2X,E10.3,2X,
     &           E10.3,2X,I6)
  770   CONTINUE
      ENDIF
C
      IF (ICTRL.LT.2) GO TO 999
C
C++   Dump all wire hits in TWTB Bank associated with this track
C
      KTWTB=IW(NATWTB)
      IF (KTWTB.EQ.0) GO TO 999
      KTWAT=IW(NATWAT)
      KTWIT=IW(NATWIT)
      IF (KTWAT.EQ.0 .OR. KTWIT.EQ.0) GO TO 999
      IF (IW(NATWRR).EQ.0) GO TO 999
C
      IF (LROWS(KTWIT).LT.ITK) GO TO 999
      NWCG=ITABL(KTWIT,ITK,JTWING)
      NWCB=ITABL(KTWIT,ITK,JTWINB)
      WRITE(IUNIT,2108) ITK,NWCG
 2108 FORMAT(/' TPC wire pulses associated with track number ',I3/,
     &        1X,I3,' good pulses:')
      WRITE(IUNIT,109)
  109 FORMAT(1X,' I ',2X,'sector',2X,'wire',2X,'charge',
     &            2X,'path length',1X,' Z residual',1X,
     &            ' R position Z position',1X,'Len',
     &            ' NWSMP',2X,'                Samples')
C
C++   Get database constants necessary for unpacking wire pulses
C
      IF (IRUN.NE.IROLD) THEN
        IROLD = IRUN
        TVOF = GTT0GL(IRUN)
        IRET=AGETDB('TWRCT0RL',IRUN)
        IF (IRET.EQ.0) GOTO 999
        CALL TPDVEL ('POT',DVA,DVB,IRVEL)
        IF (IRVEL.NE.0) GOTO 999
        DV(1) = DVA(3)
        DV(2) = DVB(3)
      ENDIF
C
      KTWRC=IW(NATWRC)
      IF (KTWRC.EQ.0) GOTO 999
      TPACK=RTABL(KTWRC,1,JTWRTP)
      KTSIM=IW(NATSIM)
      IF (KTSIM.GT.0) THEN
C      MC data
         DO 837 II=1,LTSECT
           TVOFS(II)=0.
  837    CONTINUE
         SL=RTABL(KTSIM,1,JTSITD)
      ELSE
C      Real data
         KTSOR=IW(NATSOR)
         IF (KTSOR.EQ.0) GOTO 999
         IFREQ=IW(KTSOR+JTSOCL)
         CLFREQ=FLOAT(IFREQ) 
         IF (IW(KTSOR+JTSOCH).NE.0) THEN
            IFREQH=IW(KTSOR+JTSOCH)
            IF (IABS(IFREQ-(IFREQH/1000)).LE.1) 
     +         CLFREQ=FLOAT(IFREQH)/1000. 
         ENDIF
C
         SL=1.0E6/CLFREQ
C
         KT0RL=IW(NAT0RL)
         IF (KT0RL.EQ.0) GOTO 999
         IF (TVOF.EQ.0.) GOTO 999
         SLM=SL/1000.
         DO 4683 II=1,LROWS(KT0RL)
            TVOFS(II)= (TVOF + RTABL(KT0RL,II,JT0RTP))/SLM
 4683    CONTINUE
      ENDIF
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
          TIME=FLOAT(IBITS(IWHIT,0,13))/TPACK
          ZHIT=(TIME-TVOFS(ISLOT))*DV(IENDTP(ISLOT))*SL/1000.
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
        WRITE(IUNIT,110) II,ISLOT,IWIR,CHRG,
     &          PLEN,ZRES,RPOS,ZFIT,LNPL,
     &          NWSMP,(JWPH(K),K=1,NPRNT)
  110   FORMAT(1X,I4,2X,I4,2X,I4,5(1X,F10.4),2X,I3,
     &                  1X,I3,4X,11(I3,1X))
   31 CONTINUE
C
  999 CONTINUE
      WRITE(IUNIT,101) ITK
  101 FORMAT('------ End of FRFTDP dump of track Number ',I3,' ------'/)
      RETURN
      END
#endif
