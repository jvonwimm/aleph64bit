      INTEGER FUNCTION KNMCSI(ITK)
C***********************************************************************
C Find number of SICAL EF objects that go with input MC track.
C Author: Paul Bright-Thomas
C Mods:   03/03/97 PBT Created.
C         04/03/97 PBT Required to only deal with SICAL clusters
C         07/03/97 PBT Crude angular matching used, no real match info exists
C***********************************************************************
#ifndef DOC
#include "qdecl.h"
#include "qcde.h"
#include "qhac.h"
#include "mcef.h"
      INTEGER ITK
      INTEGER II,JJ,KK,NUM,ITMC,NMCUSE
      INTEGER KFVER,NFVER,IVEND,JVEND,IMMX
      INTEGER ITFK,ITMFK,ITEF,ITPH,ITFKPH
     +       ,ITEFPE,ITEFPC,ITEFPT,ITEFPH,ITEFTY
      CHARACTER*4 VNNAM,CHAINT
      REAL    AMCSI,CTMCSI,CTMX
      INTEGER IEFMC,DUMMY
#include "qmacro.h"
C***********************************************************************
      KNMCSI = 0
C-- Were we given an MC track number ?
      IF( ITK.LT.KFMCT .OR. ITK.GT.KLMCT ) GOTO 999
C-- Does input vertex track meet the criteria for a match ?
      IF( KSTABC(ITK).LT.0 ) GOTO 999
C-- Put in some clumsy angular matching
C-- Match if:
C--           stable and not a gamma
C--           end vertex in ECAL or HCAL
C--           within 100 mrad of the input track
C--
C-- Link the vertex bank
      KFVER  = 0
      NAFVER = NAMIND('FVER')
      IF( NAFVER.GT.0 ) KFVER = IW(NAFVER)
      IF( KFVER.GT.0 )  NFVER = LROWS(KFVER)
      IF( KFVER.EQ.0 ) GOTO 999
C-- Test enters SICAL
      IVEND = KENDV(ITK)
      IF( IVEND.LE.0 ) GOTO 999
      JVEND = KVN(IVEND)
      VNNAM  = CHAINT(ITABL(KFVER,JVEND,JFVEVN))
      IF( VNNAM(1:1).NE.'S' ) GOTO 999
C-- Test angular match over all SICAL EFLW objects.
      CTMX = 0.
      IEMX = 0
      DO 500 ITEF = KFEFT,KLEFT
C-- Must have a Sical component.
        IF( KEFOTY(ITEF).NE.8 ) GOTO 500
C-- Test angle with ITEF
        CTMCSI = QCOSA(ITK,ITEF)
        IF( CTMCSI.GT.CTMX ) THEN
          CTMX = CTMCSI
          IEMX = ITEF
        ENDIF
        IF( CTMCSI.LT.CTAMH  ) GOTO 500
C-- Bingo ... we have match; Check for repetition in matching to this input
        NEFUSE = 0
        DO 580 KK = 1,MIN(NMCEF(NMCIN),MXMTCH)
          IF( ITMCEF(NMCIN,KK).EQ.ITEF ) NEFUSE = NEFUSE + 1
 580    END DO
        IF( NEFUSE.GT.0 ) GOTO 500
C-- Increment count of matched EF objects,
        KNMCSI       = KNMCSI + 1
        NMCEF(NMCIN) = NMCEF(NMCIN) + 1
C-- Save ALPHA number of matching MC track
        ITMCEF(NMCIN,NMCEF(NMCIN)) = ITEF
C-- Save matching angle  ...
        AMCSI = -ACOS(MIN(CTMCSI,0.999999))
        SMCEF(NMCIN,NMCEF(NMCIN))  = AMCSI
C-- PBT debug
        IF( FCDEBU ) WRITE(6,1012) ITK,NEFIN,NEFMC(NEFIN),KNMCSI
     +    ,ITMC,AMCSI,QE(ITMC),QE(ITK),KEFOTY(ITK),CQTPN(ITMC)
 1012   FORMAT(' KNMCSI I: ',5(1X,I3),1X,F6.3,2(1X,F6.2),1X,I2,1x,A6)
C
 500  END DO
C
      IF( FCDEBU .AND. KNMCSI.EQ.0 .AND. NEFUSE.EQ.0 ) THEN
        WRITE(6,1013) ITK,IEMX,CTMX
 1013   FORMAT(' KNMCSI W: No angular match made - closest was '
     +   ,2(I3,1X),F8.4)
      ENDIF
C
 999  RETURN
      END
#endif
