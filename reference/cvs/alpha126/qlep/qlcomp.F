      SUBROUTINE QLCOMP
CKEY QSELEP/INIT
C-------------------------------------------------------------------------------
C! Compares present QSELEP parameters to PLSC ones
C  Author                           M.N Minard  28-Dec-1994
C  Called from QLINIT
C-------------------------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
#include "plscjj.h"
      COMMON / QLREDO / XREDO(15),XRSEL
#include "qmacro.h"
C-------------------------------------------------------------------------------
      ISLOUT = KUPRNT
      CALL VZERO(XREDO,15)
      XRSEL = 0.
      NAPLSC = NAMIND('PLSC')
      KPLSC = IW(NAPLSC)
      ILINK = 0
100   IF (KPLSC.NE.0) THEN
        KPLSC = IW(KPLSC-1)
        ILINK = ILINK + 1
        GO TO 100
      ENDIF
      IF (ILINK.LT.2) GO TO 900
C
C-    compare section to reselect
C
      KPNEW = NLINK('PLSC',ILINK-1)+LMHLEN
      KPOLD = NLINK('PLSC',ILINK-2)+LMHLEN
      IF (IW(KPNEW+JPLSDT).NE.IW(KPOLD+JPLSDT))             XREDO(1)=-1.
      IF (ABS(RW(KPNEW+JPLSIS)-RW(KPOLD+JPLSIS)).GT.0.0001) XREDO(1)=-1.
      IF (IW(KPNEW+JPLSIT).NE.IW(KPOLD+JPLSIT))             XREDO(1)=-1.
      IF (ABS(RW(KPNEW+JPLSD0)-RW(KPOLD+JPLSD0)).GT.0.0001) XREDO(1)=-1.
      IF (ABS(RW(KPNEW+JPLSZ0)-RW(KPOLD+JPLSZ0)).GT.0.0001) XREDO(1)=-1.
      IF (ABS(RW(KPNEW+JPLSCO)-RW(KPOLD+JPLSCO)).GT.0.0001) XREDO(1)=-1.
      IF (XREDO(1) .LT. 0) THEN
         WRITE(ISLOUT,1000)
         WRITE(ISLOUT, 1001)
         WRITE(ISLOUT,1002)
         WRITE(ISLOUT, 1004)
         WRITE(ISLOUT,1003)
         WRITE(ISLOUT,1000)
         XRSEL = 1.
         GO TO 900
 1000 FORMAT(1X,'+--------------------------------------------+')
 1001 FORMAT(1X,'| QSELEP to be redone                        |')
 1002 FORMAT(1X,'| General event variable cuts would change   |')
 1004 FORMAT(1X,'| Jet definition                             |')
 1003 FORMAT(1X,'| LSHD card parameters changed               |')
      ENDIF
      IF (IW(KPNEW+JPLSNE).LT.IW(KPOLD+JPLSNE))           XREDO(2)=-1.
      IF (IW(KPNEW+JPLSNE).GT.IW(KPOLD+JPLSNE))           XREDO(3)= 1.
      IF ((RW(KPNEW+JPLSDE)-RW(KPOLD+JPLSDE)).GT.0.0001)  XREDO(2)=-2.
      IF ((RW(KPNEW+JPLSDE)-RW(KPOLD+JPLSDE)).LT.-0.0001) XREDO(3)= 2.
      IF ((RW(KPNEW+JPLSZE)-RW(KPOLD+JPLSZE)).GT.0.0001)  XREDO(2)=-3.
      IF ((RW(KPNEW+JPLSZE)-RW(KPOLD+JPLSZE)).LT.-0.0001) XREDO(3)= 3.
      IF ((RW(KPNEW+JPLSCE)-RW(KPOLD+JPLSCE)).GT.0.0001)  XREDO(2)=-4.
      IF ((RW(KPNEW+JPLSCE)-RW(KPOLD+JPLSCE)).LT.-0.0001) XREDO(3)= 4.
      IF ((RW(KPNEW+JPLSPE)-RW(KPOLD+JPLSPE)).LT.-0.0001) XREDO(2)=-5.
      IF ((RW(KPNEW+JPLSPE)-RW(KPOLD+JPLSPE)).GT.0.0001)  XREDO(3)= 5.
      IF ((RW(KPNEW+JPLSTE)-RW(KPOLD+JPLSTE)).LT.-0.0001) XREDO(2)=-6.
      IF ((RW(KPNEW+JPLSTE)-RW(KPOLD+JPLSTE)).GT.0.0001)  XREDO(3)= 6.
      IF (XREDO(2).LT.0) THEN
        WRITE(ISLOUT,1000)
        WRITE(ISLOUT,1001)
        WRITE(ISLOUT,1005)
        WRITE(ISLOUT,1000)
        XRSEL = 2
        GO TO 900
      ENDIF
      IF (XREDO(3).GT.0) THEN
        WRITE(ISLOUT,1000)
        WRITE(ISLOUT,1006)
        WRITE(ISLOUT,1007)
        WRITE(ISLOUT,1000)
      ENDIF
 1005 FORMAT(1X,'| LSET electron cuts definition has changed  |')
 1006 FORMAT(1X,'| LSET change in electron cuts               |')
 1007 FORMAT(1X,'| Tighter cuts on electron selection applied |')
       IF ((RW(KPNEW+JPLS2N)-RW(KPOLD+JPLS2N)).LT.-0.0001) XREDO(4)=-1.
       IF ((RW(KPNEW+JPLS2N)-RW(KPOLD+JPLS2N)).GT.0.0001)  XREDO(5)= 1.
       IF ((RW(KPNEW+JPLS2X)-RW(KPOLD+JPLS2X)).GT.0.0001)  XREDO(4)=-1.
       IF ((RW(KPNEW+JPLS2X)-RW(KPOLD+JPLS2X)).LT.-0.0001) XREDO(5)= 2.
       IF ((RW(KPNEW+JPLS3N)-RW(KPOLD+JPLS3N)).LT.-0.0001) XREDO(4)=-1.
       IF ((RW(KPNEW+JPLS3X)-RW(KPOLD+JPLS3X)).GT.0.0001)  XREDO(5)= 3.
       IF (IW(KPNEW+JPLSIM).LT.IW(KPOLD+JPLSIM))           XREDO(4)=-1.
       IF (IW(KPNEW+JPLSIM).GT.IW(KPOLD+JPLSIM))           XREDO(5)= 4.
       IF ((RW(KPNEW+JPLS5N)-RW(KPOLD+JPLS5N)).LT.-0.0001) XREDO(4)=-1.
       IF ((RW(KPNEW+JPLS5N)-RW(KPOLD+JPLS5N)).GT.0.0001)  XREDO(5)= 5.
       IF ((RW(KPNEW+JPLS5X)-RW(KPOLD+JPLS5X)).GT.0.0001)  XREDO(4)=-1.
       IF ((RW(KPNEW+JPLS5X)-RW(KPOLD+JPLS5X)).LT.0.0001)  XREDO(5)= 6.
       IF (XREDO(4).LT.0) THEN
        WRITE(ISLOUT,1000)
        WRITE(ISLOUT,1001)
        WRITE(ISLOUT,1008)
        WRITE(ISLOUT,1000)
        XRSEL = 3
        GO TO 900
      ENDIF
      IF (XREDO(5).GT.0) THEN
        WRITE(ISLOUT,1000)
        WRITE(ISLOUT,1009)
        WRITE(ISLOUT,1010)
        WRITE(ISLOUT,1000)
      ENDIF
 1008 FORMAT(1X,'| LSEC electron estimator cuts  has changed  |')
 1009 FORMAT(1X,'| LSEC change in electron estimator cuts     |')
 1010 FORMAT(1X,'| Tighter cuts on estimator will be  applied |')
      IF ((RW(KPNEW+JPLSXY)-RW(KPOLD+JPLSXY)).GT.0.0001)  XREDO(6)=-1.
      IF ((RW(KPNEW+JPLSXY)-RW(KPOLD+JPLSXY)).LT.-0.0001) XREDO(7)= 1.
      IF ((RW(KPNEW+JPLSZC)-RW(KPOLD+JPLSZC)).GT.0.0001)  XREDO(6)=-1.
      IF ((RW(KPNEW+JPLSZC)-RW(KPOLD+JPLSZC)).LT.-0.0001) XREDO(7)= 2.
      IF ((RW(KPNEW+JPLSMI)-RW(KPOLD+JPLSMI)).GT.0.0001)  XREDO(6)=-1.
      IF ((RW(KPNEW+JPLSMI)-RW(KPOLD+JPLSMI)).LT.-0.0001) XREDO(7)= 3.
      IF (IW(KPNEW+JPLSSF).GT.IW(KPOLD+JPLSSF))           XREDO(6)=-1.
      IF (IW(KPNEW+JPLSSF).LT.IW(KPOLD+JPLSSF))           XREDO(8)= 4.
      IF (XREDO(6).LT.0) THEN
        WRITE(ISLOUT,1000)
        WRITE(ISLOUT,1001)
        WRITE(ISLOUT,1011)
        WRITE(ISLOUT,1000)
        XRSEL = 4
        GO TO 900
      ENDIF
      IF (XREDO(7).GT.0) THEN
        WRITE(ISLOUT,1000)
        WRITE(ISLOUT,1012)
        WRITE(ISLOUT,1000)
      ENDIF
      IF(XREDO(8).EQ.4) WRITE(ISLOUT,1013)
 1011 FORMAT(1X,'| LSCV conversion identification cuts changed|')
 1012 FORMAT(1X,'| LSCV change in conversion cuts             |')
 1013 FORMAT(1X,'|      Conversion are removed                |')
      IF (IW(KPNEW+JPLSNM).LT.IW(KPOLD+JPLSNM))           XREDO(9)=-1.
      IF (IW(KPNEW+JPLSNM).GT.IW(KPOLD+JPLSNM))           XREDO(10)=1.
      IF ((RW(KPNEW+JPLSDM)-RW(KPOLD+JPLSDM)).GT.0.0001)  XREDO(9)=-1.
      IF ((RW(KPNEW+JPLSDM)-RW(KPOLD+JPLSDM)).LT.-0.0001) XREDO(10)=2.
      IF ((RW(KPNEW+JPLSZM)-RW(KPOLD+JPLSZM)).GT.0.0001)  XREDO(9)=-1.
      IF ((RW(KPNEW+JPLSZM)-RW(KPOLD+JPLSZM)).LT.-0.0001) XREDO(10)=3.
      IF ((RW(KPNEW+JPLSCM)-RW(KPOLD+JPLSCM)).GT.0.0001)  XREDO(9)=-1.
      IF ((RW(KPNEW+JPLSCM)-RW(KPOLD+JPLSCM)).LT.-0.0001) XREDO(10)=4.
      IF ((RW(KPNEW+JPLSPM)-RW(KPOLD+JPLSPM)).LT.-0.0001) XREDO(9)=-1.
      IF ((RW(KPNEW+JPLSPM)-RW(KPOLD+JPLSPM)).GT.0.0001)  XREDO(10)=3.
      IF ((RW(KPNEW+JPLSTM)-RW(KPOLD+JPLSTM)).LT.-0.0001) XREDO(9)=-1.
      IF ((RW(KPNEW+JPLSTM)-RW(KPOLD+JPLSTM)).GT.0.0001)  XREDO(10)=4.
      IF (XREDO(9).LT.0) THEN
        WRITE(ISLOUT,1000)
        WRITE(ISLOUT,1001)
        WRITE(ISLOUT,1014)
        WRITE(ISLOUT,1000)
        XRSEL = 5
        GO TO 900
      ENDIF
      IF (XREDO(10).GT.0) THEN
        WRITE(ISLOUT,1000)
        WRITE(ISLOUT,1015)
        WRITE(ISLOUT,1016)
        WRITE(ISLOUT,1000)
      ENDIF
 1014 FORMAT(1X,'| LSMU Muon selection cuts enlarged          |')
 1015 FORMAT(1X,'| LSMU change in muon selection cuts         |')
 1016 FORMAT(1X,'|      Muon selection tightened              |')
      IF (IW(KPNEW+JPLSQM).NE.IW(KPOLD+JPLSQM))    XREDO(11)=-1.
      IF ((IW(KPNEW+JPLSQ1).NE.IW(KPOLD+JPLSQ1)).AND.
     +    (IW(KPNEW+JPLSQ1).NE.IW(KPOLD+JPLSQ2)))  XREDO(11)=-1
      IF ((IW(KPNEW+JPLSQ2).NE.IW(KPOLD+JPLSQ1)).AND.
     +    (IW(KPNEW+JPLSQ2).NE.IW(KPOLD+JPLSQ2)))  XREDO(11)=-1
      IF (XREDO(11).LT.0) THEN
        WRITE(ISLOUT,1000)
        WRITE(ISLOUT,1001)
        WRITE(ISLOUT,1017)
        WRITE(ISLOUT,1000)
        XRSEL = 6
        GO TO 900
      ENDIF
 1017 FORMAT(1X,'| Muon selection flag different              |')
      IF (IW(KPNEW+JPLSNJ).NE.IW(KPOLD+JPLSNJ))            XREDO(12)= 1.
      IF (ABS(RW(KPNEW+JPLSEJ)-RW(KPOLD+JPLSEJ)).GT.0.0001)XREDO(13)= 2.
      IF (IW(KPNEW+JPLSJN).NE.IW(KPOLD+JPLSJN))            XREDO(13)= 3.
      IF (ABS(RW(KPNEW+JPLSYJ)-RW(KPOLD+JPLSYJ)).GT.0.0001)XREDO(14)=-1.
      IF (XREDO(12).GT.0.AND.IW(KPOLD+JPLSKJ).EQ.0)        XREDO(14)=-2.
      IF (XREDO(13).GT.0.AND.IW(KPOLD+JPLSKJ).EQ.0)        XREDO(14)=-3.
      IF (IW(KPNEW+JPLSKJ).GT.IW(KPOLD+JPLSKJ))            XREDO(14)=-4.
      IF (IW(KPNEW+JPLSKJ).LT.IW(KPOLD+JPLSKJ))            XREDO(15)= 4.
      IF (XREDO(14).LT.0) THEN
        WRITE(ISLOUT,1000)
        WRITE(ISLOUT,1001)
        WRITE(ISLOUT,1018)
        WRITE(ISLOUT,1000)
        XRSEL = 6
        GO TO 900
      ENDIF
      IF (XREDO(12).GT.0) WRITE(ISLOUT,1021)
      IF (XREDO(13).GT.0) WRITE(ISLOUT,1019)
      IF (XREDO(15).EQ.4) WRITE(ISLOUT,1020)
        WRITE(ISLOUT,1000)
 1018 FORMAT(1X,'| LSJT PT definition parameter changed       |')
 1021 FORMAT(1X,'| Minimum numbet of object in a jet changed  |')
 1020 FORMAT(1X,'| LSJT Remove not well defined Pt lepton     |')
 1019 FORMAT(1X,'| LSJT Jet requirements changed              |')
  900 CONTINUE
      RETURN
      END
#endif
