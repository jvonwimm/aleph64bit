      INTEGER FUNCTION KNSIMC(ITK)
C***********************************************************************
C Find number of MC track going with input SICAL EFLW object
C Author: Paul Bright-Thomas
C Mods:   03/03/97 PBT Created.
C         04/03/97 PBT Required to only deal with SICAL clusters
C         05/03/97 PBT SICAL clusters appear in PHCO, but not PEMH
C       Crude angular matching used, since no real match info exists
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
      REAL    ASIMC,CTSIMC,CTMX
      INTEGER IEFMC,DUMMY
#include "qmacro.h"
C***********************************************************************
      KNSIMC = 0
C-- Were we given an EF track number ?
      IF( ITK.LT.KFEFT .OR. ITK.GT.KLEFT ) GOTO 999
C-- Get EFLW type for ALPHA EF particle ... quit if not SICAL object
      ITEFTY = KEFOTY(ITK)
      IF( ITEFTY.NE.8 ) GOTO 999
      ITEFPE = KEFOLE(ITK)
      ITEFPC = KEFOLC(ITK)
C-- PBT debug
      IF( FCDEBU ) THEN
        ITEFPT = KEFOLT(ITK)
        ITEFPH = KEFOLH(ITK)
        IF( ITEFTY.GE.8 ) THEN
C          WRITE(6,*) ' KNSIMC I: Got a SICAL object'
          DUMMY = 1
        ENDIF
      ENDIF
C
C****************************
C A N G U L A R
C****************************
C-- Put in some clumsy angular matching
C-- Match if:
C--           stable
C--           end vertex in SICAL
C--           within 100 mrad of the input track
C--
C-- Link the vertex bank
 499  KFVER  = 0
      NAFVER = NAMIND('FVER')
      IF( NAFVER.GT.0 ) KFVER = IW(NAFVER)
      IF( KFVER.GT.0 )  NFVER = LROWS(KFVER)
      IF( KFVER.EQ.0 ) GOTO 999
C-- Loop over all MC tracks and keep any which satisfy these criteria.
      CTMX = 0.
      IMMX = 0
      DO 500 ITMC = KFMCT,KLMCT
C-- Test stability
        IF( KSTABC(ITMC).LT.0 ) GOTO 500
C-- Test enters HCAL (or ECAL, since most FKIN hadrons appear to end in ECAL)
        IVEND = KENDV(ITMC)
        IF( IVEND.LE.0 ) GOTO 500
        JVEND = KVN(IVEND)
        VNNAM  = CHAINT(ITABL(KFVER,JVEND,JFVEVN))
        IF( VNNAM(1:1).NE.'S' ) GOTO 500
C-- Test angular match
        CTSIMC = QCOSA(ITMC,ITK)
        IF( CTSIMC.GT.CTMX ) THEN
          CTMX = CTSIMC
          IMMX = ITMC
        ENDIF
        IF( CTSIMC.LT.CTAMS  ) GOTO 500
C-- Bingo ... we have match; Check for repetition in matching to this input
        NMCUSE = 0
        DO 580 KK = 1,MIN(NEFMC(NEFIN),MXMTCH)
          IF( ITEFMC(NEFIN,KK).EQ.ITMC ) NMCUSE = NMCUSE + 1
 580    END DO
        IF( NMCUSE.GT.0 ) GOTO 500
C-- Increment count of matched EF objects,
        KNSIMC       = KNSIMC + 1
        NEFMC(NEFIN) = NEFMC(NEFIN) + 1
C-- Save ALPHA number of matching MC track
        ITEFMC(NEFIN,NEFMC(NEFIN)) = ITMC
C-- Save matching angle  ...
        ASIMC = -ACOS(MIN(CTSIMC,0.999999))
        SEFMC(NEFIN,NEFMC(NEFIN))  = ASIMC
C-- PBT debug
        IF( FCDEBU ) WRITE(6,1012) ITK,NEFIN,NEFMC(NEFIN),KNSIMC
     +    ,ITMC,ASIMC,QE(ITMC),QE(ITK),KEFOTY(ITK),CQTPN(ITMC)
 1012   FORMAT(' KNSIMC I: ',5(1X,I3),1X,F6.3,2(1X,F6.2),1X,I2,1X,A6)
C
 500  END DO
      IF( FCDEBU .AND. KNSIMC.EQ.0 .AND. NMCUSE.EQ.0 ) THEN
        WRITE(6,1003) ITK,IMMX,CTMX
 1003   FORMAT(' KNSIMC W: No angular match made - closest was '
     +   ,2(I3,1X),F8.4)
      ENDIF
C
 999  RETURN
      END
#endif
