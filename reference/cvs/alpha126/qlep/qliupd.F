      SUBROUTINE QLIUPD
CKEY QSELEP/INIT
C-------------------------------------------------------------------------------
C! For MINI10 processing correct for missinng PLSC bank
C  Author                           M.N Minard  28-Dec-1994
C  Called from QLINIT
C  This routine is called for the first event only.  It creates the
C  PLSC bank which should have been created at MINI level.
C-------------------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
#include "qdecl.h"
#include "qcde.h"
      EXTERNAL CHAINT
      CHARACTER*8 PRNAM
      CHARACTER*4 CHAINT
      CHARACTER*4 CTEMP
      INTEGER KRHAH
#include "rhahjj.h"
#include "plscjj.h"
      INTEGER IFIR , NLENG, NAPLSC , KPLSC,ILINK,IGARB,JPLSC
      INTEGER ILVMX , ILVER,IMINI,IRHAH,NRHAH,IDVER,IDVMX
      INTEGER INTCHA  ,ALVERN ,ALVVRS ,ALVER0
      REAL SYCUT,ALFAVR

C
      DATA IFIR / -1 /
      DATA ILVMX , IDVMX / 204 , 100 /
      DATA ALVER0 / 12002 /
C--
      INTEGER IQM,ICONT,SLOUT
      INTEGER NAMIND,NLINK,NDROP
#include "qmacro.h"
C-------------------------------------------------------------------------------
C-    Only called at 1st event
C
      SLOUT = KUPRNT
      KRHAH = IW(NARHAH)
      NRHAH=LROWS(KRHAH)
      IMINI = 0
      DO 10 IRHAH = 1, NRHAH
        PRNAM(1:4) = CHAINT(ITABL(KRHAH,IRHAH,JRHAPN))
        PRNAM(5:8) = CHAINT(ITABL(KRHAH,IRHAH,JRHAPN+1))
        IF (PRNAM.EQ.'MINI ') THEN
          IMINI = 1
          IDVER = ITABL(KRHAH,IRHAH,JRHAPV)
          ILVER = ITABL(KRHAH,IRHAH,JRHAAV)
        ENDIF
 10   CONTINUE
      IF (IMINI.EQ.0) THEN
          IDVER = ITABL(KRHAH,NRHAH,JRHADV)
          ILVER = ITABL(KRHAH,NRHAH,JRHAAV)
      ENDIF
      IF (IW(NAMIND('QLID')).EQ.0) GO TO 990
      IF (IMINI.EQ.0) GO TO 990
      IF (IDVER.GT.IDVMX) GO TO 990
      NAPLSC = NDROP('PLSC',0)
C
C--
C--   Welcome and summary of user cards files
C--
      WRITE(SLOUT,1000)
      WRITE(SLOUT,1001)
 1000 FORMAT(1X,'+--------------------------------------------+')
 1001 FORMAT(1X,'| Missing PLSC on MINI10 being rebuilt       |')
 1002 FORMAT(1X,'| -----------------------------              |')
 1003 FORMAT(1X,'|                                            |')
 1004 FORMAT(1X,'| PLSC bank attached  in link ',i3,'      |')
C-------------------------------------------------------------------------------
C-    Build header bank for lepton selection PLSC
C
      CALL BKFMT('PLSC','2I,(I,F,I,3F,I,9F,I,5F,2I,5F,I,2F,I,F,I,F,I)')
      NAPLSC = NAMIND('PLSC')
      KPLSC = IW(NAPLSC)
C
C-    If former selection add  PLSC  bank
      ILINK = 0
 800  IF ( KPLSC.NE.0) THEN
        KPLSC = IW(KPLSC-1)
        ILINK = ILINK +1
        GO TO 800
      ENDIF
      NLENG = LPLSCA + LMHLEN
      CALL AUBOS('PLSC',ILINK,NLENG,KPLSC,IGARB)
      IF ( IGARB.EQ.2 ) GO TO 801
      IW(KPLSC+LMHCOL) = LPLSCA
      IW(KPLSC+LMHROW) = 1
C
C-    Fill track selection
C
      JPLSC = KPLSC +LMHLEN
      IW( JPLSC +JPLSDT) = 5
      RW( JPLSC + JPLSIS ) = 0.1
      IW( JPLSC + JPLSIT ) = 4
      RW( JPLSC + JPLSD0 ) = 2.0
      RW( JPLSC + JPLSZ0 ) = 10.0
      RW( JPLSC + JPLSCO ) = 0.95
C
C     Electron track parameter cuts
C
      IW( JPLSC + JPLSNE ) = 5
      RW( JPLSC + JPLSDE ) = 2.0
      RW( JPLSC + JPLSZE ) = 10.0
      RW( JPLSC + JPLSCE ) = 0.95
      RW( JPLSC + JPLSPE ) = 2.
      RW( JPLSC + JPLSTE ) = 0.
C
C-    Electron - id selection
C
      RW( JPLSC + JPLS2N ) = -3.
      RW( JPLSC + JPLS2X ) =  999.
      RW( JPLSC + JPLS3N ) = -3.
      RW( JPLSC + JPLS3X ) =  3.
      IW( JPLSC + JPLSIM ) =  0.
      RW( JPLSC + JPLS5N ) =  -998.
      RW( JPLSC + JPLS5X ) =   999.
C
C-    Conversion selection
C
      RW( JPLSC + JPLSXY ) = 1.
      RW( JPLSC + JPLSZC ) = 1.
      RW( JPLSC + JPLSMI ) = 0.02
      IW( JPLSC + JPLSSF ) = 1
C
C     Muon  track parameter cuts
C
      IW( JPLSC + JPLSNM ) = 5
      RW( JPLSC + JPLSDM ) = 2.0
      RW( JPLSC + JPLSZM ) = 10.
      RW( JPLSC + JPLSCM ) = 0.95
      RW( JPLSC + JPLSPM ) = 3.
      RW( JPLSC + JPLSTM ) = 0.
C
C-    Selection criteria for muon
C
      IW( JPLSC + JPLSQM ) = 2
      RW( JPLSC + JPLSQ1 ) = 13.
      RW( JPLSC + JPLSQ2 ) = 14.
C
C-    Jet selection criteria
C
      IW( JPLSC + JPLSNJ ) = 5
      RW( JPLSC + JPLSEJ ) = 0.9
      IW( JPLSC + JPLSJN ) = 2
      RW( JPLSC + JPLSYJ ) = 0.0043
      IW( JPLSC + JPLSKJ ) = 1
      CALL BLIST(IW,'C+','PLSC')
      CALL BLIST(IW,'R+','PLSC')
      WRITE (SLOUT,1004)
      WRITE(SLOUT,1002)
      WRITE(SLOUT,1003)
      CONTINUE
 801  IF(IGARB.EQ.2) THEN
        WRITE(SLOUT,1060)
      ELSE
        WRITE(SLOUT,1061)ILINK+1
      ENDIF
 1061 FORMAT(1X,'| Bank PLSC added as a selection # ', I3)
 1060 FORMAT(1X,'| Bank PLSC for cut summary not added  ******|')
  990 CONTINUE
      RETURN
      END
#endif
