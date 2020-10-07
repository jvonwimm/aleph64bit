      SUBROUTINE QLTERM
CKEY QSELEP/INTERNAL
C-------------------------------------------------------------------------------
C! QSELEP summary
C  Author ( for original LEPTAG )    Mark Parsons  01-Jun-1992
C  Adapted from LEPTAG to QSELEP       I. Ten Have 27-Jul-1994
C  Called from QMTERM
C  This routine gives the final run  statistics and an overview
C  of occurrance of the various errors.
C-------------------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
#include "qdecl.h"
#include "qcde.h"
      INTEGER SLOUT,NAMIND
      REAL LTSCFA
      PARAMETER(LTSCFA = 1000000.)
      INTEGER NCNT
      PARAMETER(NCNT = 25)
      INTEGER LEPCNT(NCNT)
C
C     QSELEP counters
C
      INTEGER LTNCHR,LTQLPR,LTECHR,LTACHR,LTBOSC,LTBOSS,LTNJET,LTJTAS,
     @        LTPTFL,LTHVDQ,LTHAD ,LTNLEP,LTYLEP,LTCONV,LTELMU,LTNEMI,
     @        LTNEPL,LTNMMI,LTNMPL,LTNCNV,LTCALL,LTNEFO,LTGTYF,LTCPDF,
     @        LTBRFL
C
      PARAMETER(LTNCHR= 1,LTQLPR= 2,LTECHR= 3,LTACHR= 4,LTBOSC= 5,
     @          LTBOSS= 6,LTNJET= 7,LTJTAS= 8,LTPTFL= 9,LTHVDQ=10,
     @          LTHAD =11,LTNLEP=12,LTYLEP=13,LTCONV=14,LTELMU=15,
     @          LTNEMI=16,LTNEPL=17,LTNMMI=18,LTNMPL=19,LTNCNV=20,
     @          LTCALL=21,LTNEFO=22,LTGTYF=23,LTCPDF=24,LTBRFL=25)
C
      COMMON /LPTPUB/ LEPCNT
C-------------------------------------------------------------------------------
      IF (IW(NAMIND('QLID')).EQ.0) GO TO 999
      SLOUT = KUPRNT
      WRITE (SLOUT,*)
      WRITE (SLOUT,1000)
      WRITE (SLOUT,1001) LEPCNT(LTCALL)
      WRITE (SLOUT,1002) LEPCNT(LTBOSC)
      WRITE (SLOUT,1003) LEPCNT(LTBOSS)
      WRITE (SLOUT,1004) LEPCNT(LTHAD)
      WRITE (SLOUT,1005) LEPCNT(LTHVDQ)
      WRITE (SLOUT,1006) LEPCNT(LTQLPR)
      WRITE (SLOUT,1007) LEPCNT(LTNEFO)
      WRITE (SLOUT,1008) LEPCNT(LTNCHR)
      WRITE (SLOUT,1009) LEPCNT(LTECHR)
      WRITE (SLOUT,1010) LEPCNT(LTNJET)
      WRITE (SLOUT,1011) LEPCNT(LTNLEP)
      WRITE (SLOUT,1012) LEPCNT(LTYLEP)
      WRITE (SLOUT,*)
      WRITE (SLOUT,1100) LEPCNT(LTNEMI),LEPCNT(LTNEPL),
     @                   LEPCNT(LTNMMI),LEPCNT(LTNMPL)
      WRITE (SLOUT,*)
      WRITE (SLOUT,1013) LEPCNT(LTNCNV)
      WRITE (SLOUT,1014) LEPCNT(LTCONV)
      WRITE (SLOUT,1015) LEPCNT(LTELMU)
      WRITE (SLOUT,1016) LEPCNT(LTACHR)
      WRITE (SLOUT,1017) LEPCNT(LTJTAS)
      WRITE (SLOUT,1018) LEPCNT(LTPTFL)
      WRITE (SLOUT,1019) LEPCNT(LTGTYF)
      WRITE (SLOUT,1020) LEPCNT(LTCPDF)
      WRITE (SLOUT,1021) LEPCNT(LTBRFL)
      WRITE (SLOUT,1000)
      WRITE (SLOUT,*)
C
C Here are the format statements for the above output.
C
 1000 FORMAT(1X,'------- QSELEP summary table ----------------------')
 1001 FORMAT(1X,'Total number of calls to QSELEP',19X,I6)
 1002 FORMAT(1X,'BOS space corruption',30X,I6)
 1003 FORMAT(1X,'BOS more space requested than available',11X,I6)
 1004 FORMAT(1X,'No. events passing the hadronic event selection',3X,I6)
 1005 FORMAT(1X,'No. events failing the HV and DAQ status test',5X,I6)
 1006 FORMAT(1X,'No. events with an incorrect QELEP',16X,I6)
 1007 FORMAT(1X,'No. events with no EFT objects or no tracks',7X,I6)
 1008 FORMAT(1X,'No. events failing the no. charged tracks cut',5X,I6)
 1009 FORMAT(1X,'No. events with less than 10% * QELEP',13X,I6)
 1010 FORMAT(1X,'No. events with less than 2 jets',18X,I6)
 1011 FORMAT(1X,'No. events with no lepton found',19X,I6)
 1012 FORMAT(1X,'No. events with a good lepton found',15X,I6)
 1013 FORMAT(1X,'No. of electron/positron conversions found',8X,I6)
 1014 FORMAT(1X,'No. conversions found to be muons',17X,I6)
 1015 FORMAT(1X,'No. ambiguous electron/muon candidates',12X,I6)
 1016 FORMAT(1X,'No. lepton candidates with incorrect charge',7X,I6)
 1017 FORMAT(1X,'No. times the jet-lepton association failed',7X,I6)
 1018 FORMAT(1X,'No. times an error in the pt calculation occurred',1X,
     @        I6)
 1019 FORMAT(1X,'No. of times GTYEAR returned zero',17X,I6)
 1020 FORMAT(1X,'No. of times the CALPOIDS calls failed',12X,I6)
 1021 FORMAT(1X,'No. of times QTRUTH returned a bad value',10X,I6)
 1100 FORMAT(1X,'No. e-',1X,I6,' No. e+',1X,I6,' No. mu-',1X,I6,
     @          ' No. mu+',1X,I6)
 999  RETURN
      END
#endif
