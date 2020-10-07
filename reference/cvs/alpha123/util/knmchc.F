      INTEGER FUNCTION KNMCHC(ITK)
C***********************************************************************
C Find the alpha EFLW objects with PHCO part matching input MC track.
C The matching is circuitous -- using PHMH bank, FKIN number of MC input, and
C PHCO parts of EFLW objects
C Author: Paul Bright-Thomas
C Mods:   03/03/97 PBT Created.
C         04/03/97 PBT Absence of PHMH in most MC means a dirty angle fix.
C         05/03/97 PBT Yet to be updated for PCOB objects (KEFOLC)
C***********************************************************************
#ifndef DOC
#include "qdecl.h"
#include "qcde.h"
#include "qhac.h"
#include "mcef.h"
      INTEGER ITK
      INTEGER KPHMH,NPHMH,IPHMH,NEFUSE
      INTEGER KPHCO,NPHCO,IPHCO,JPHCO
      INTEGER KFVER,NFVER,IVEND,JVEND
      INTEGER ITFK,ITMFK,ITEF,ITFKPH,ITFKEF
     +       ,ITEFPH,ITEFPC,ITEFPT,ITEFTY,JTPHCO(MXMTCH),IEMX
      CHARACTER*4 VNNAM,CHAINT
      REAL    EFKIN,AMCHC,CTMCHC,CTMX
      INTEGER IMCEF
#include "qmacro.h"
C***********************************************************************
      KNMCHC = 0
C-- Were we given an MC track number ?
      IF( ITK.LT.KFMCT .OR. ITK.GT.KLMCT ) GOTO 999
C-- Get FKIN number for ALPHA MC particle.
      ITMFK = KTN(ITK)
      IF( ITMFK.LE.0 ) GOTO 999
C-- Link the PHMH bank
      KPHMH  = 0
      NAPHMH = NAMIND('PHMH')
      IF( NAPHMH.GT.0 ) KPHMH = IW(NAPHMH)
      IF( KPHMH.GT.0 )  NPHMH = LROWS(KPHMH)
      IF( KPHMH.EQ.0 ) GOTO 499
C-- Link the PHCO bank
      KPHCO  = 0
      NAPHCO = NAMIND('PHCO')
      IF( NAPHCO.GT.0 ) KPHCO = IW(NAPHCO)
      IF( KPHCO.GT.0  ) NPHCO = LROWS(KPHCO)
C-- EFLW ECAL: loop over PHMH to find EFLW obj w matching PHCO cluster
 201  DO 200 IPHMH = 1,NPHMH
C-- Get FKIN number for this cluster-FKIN match
        ITFK   = ITABL(KPHMH,IPHMH,JPHMTN)
        IF( ITFK.NE.ITMFK ) GOTO 200
C-- Get PHCO number for this cluster-FKIN match
        ITFKPH = ITABL(KPHMH,IPHMH,JPHMCO)
        IF( ITFKPH.LE.0 ) GOTO 200
C-- Search all EFLW objects for same PHCO component ... or PCOB->PHCO
        ITEF   = KPDIR('EFLW',KRECO)
 210    IF( ITEF.LE.0 ) GOTO 200
C-- Get PHCO/PCOB part of EFLW object ... test against PHMH match.
        ITEFPH = KEFOLH(ITEF)
        ITEFPC = KEFOLC(ITEF)
        IF( ITEFPH.LE.0 .AND. ITEFPC.LE.0 ) GOTO 230
C-- Store relevant PHCO row numbers, from ITEFPH or ITEFPC, in JTPHCO
        JPHCO  = 0
        ITFKEF = 0
        IF( ITEFPH.GT.0 ) THEN
          JPHCO         = JPHCO + 1
          JTPHCO(JPHCO) = ITEFPH
        ENDIF
C-- Search PHCO for rows matching PCOB
        IF( ITEFPC.EQ.0  .OR. KPHCO.EQ.0 ) GOTO 199
        DO 100 IPHCO = 1,NPHCO
          IPC = ITABL(KPHCO,IPHCO,JPHCPC)
          IF( IPC.EQ.ITEFPC .AND. JPHCO.LT.MXMTCH ) THEN
            JPHCO         = JPHCO + 1
            JTPHCO(JPHCO) = IPHCO
          ENDIF
 100    END DO
C-- Are there any PHCO objects to match ?
 199    IF( JPHCO.LE.0 ) GOTO 230
C-- Search through JTPHCO list of PHCO objects for any belonging to ITFK
        DO 240 JJ = 1,JPHCO
          IF( ITFKPH.EQ.JTPHCO(JJ) ) ITFKEF = ITFKPH
 240    END DO
        IF( ITFKEF.LE.0 .OR. ITFKEF.NE.ITFKPH ) GOTO 230
C-- Check for repetition in matching to this input
        NEFUSE = 0
        DO 250 KK = 1,MIN(NMCEF(NMCIN),MXMTCH)
          IF( ITMCEF(NMCIN,KK).EQ.ITEF ) NEFUSE = NEFUSE + 1
 250    END DO
        IF( NEFUSE.GT.0 ) GOTO 230
C-- We have a match ... check array bounds
        IF( NMCEF(NMCIN).GE.MXMTCH ) THEN
          CALL QWMESE(' KNMCHC - increase MXMTCH limit.')
          GOTO 999
        ENDIF
C-- Increment count of matched EF objects,
        KNMCHC       = KNMCHC + 1
        NMCEF(NMCIN) = NMCEF(NMCIN) + 1
C-- Save ALPHA number of matching EFLW object.
        ITMCEF(NMCIN,NMCEF(NMCIN)) = ITEF
C-- Save matching energy
        EFKIN  = FLOAT(ITABL(KPHMH,IPHMH,JPHMSE)) / 1000.
        SMCEF(NMCIN,NMCEF(NMCIN))  = EFKIN
C-- PBT debug
        IF( FCDEBU ) WRITE(6,1001) ITK,NMCIN,NMCEF(NMCIN),KNMCHC
     +    ,ITMFK,ITFK,ITFKPH,ITFKEF,ITMC,EFKIN,QE(ITMC),QE(ITEF)
     +    ,KEFOTY(ITEF)
 1001   FORMAT(' KNMCHC I: ',9(1X,I3),3(1X,F6.2),1X,I2)
C-- Next EFLW object.
 230    ITEF   = KFOLLO(ITEF)
        GOTO 210
C
 200  END DO
 299  GOTO 999
C
C****************************
C A N G U L A R
C****************************
 499  IF( FCWARN .AND. KNEVT.EQ.1 .AND. NMCIN.EQ.1 ) WRITE(6,*)
     +  ' KNMCHC W: No PHMH matching information - no HCAL-MC matches.'
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
C-- Test enters HCAL (or ECAL, since most FKIN hadrons appear to end in ECAL)
      IVEND = KENDV(ITK)
      IF( IVEND.LE.0 ) GOTO 999
      JVEND = KVN(IVEND)
      VNNAM  = CHAINT(ITABL(KFVER,JVEND,JFVEVN))
      IF( VNNAM(1:1).NE.'H' .AND. VNNAM(1:1).NE.'E' ) GOTO 999
C-- No gammas can have made it through .... anything else is fair game.
      IF( XPEQOR(ITK,'gamma') ) GOTO 999
C-- Test angular match over all HCAL objects.
C-- Loop over all EFLW HCAL objects and keep any which match well on angle
      CTMX = 0.
      IEMX = 0
      DO 500 ITEF = KFEFT,KLEFT
C-- Get PHCO/PCOB part of EFLW object ... test against PHMH match.
        ITEFPH = KEFOLH(ITEF)
        ITEFPC = KEFOLC(ITEF)
        IF( ITEFPH.LE.0 .AND. ITEFPC.LE.0 ) GOTO 500
C-- Store relevant PHCO row numbers, from ITEFPH or ITEFPC, in JTPHCO
        JPHCO  = 0
        ITFKEF = 0
        IF( ITEFPH.GT.0 ) THEN
          JPHCO         = JPHCO + 1
          JTPHCO(JPHCO) = ITEFPH
        ENDIF
C-- Search PHCO for rows matching PCOB
        IF( ITEFPC.EQ.0  .OR. KPHCO.EQ.0 ) GOTO 529
        DO 520 IPHCO = 1,NPHCO
          IPC = ITABL(KPHCO,IPHCO,JPHCPC)
          IF( IPC.EQ.ITEFPC .AND. JPHCO.LT.MXMTCH ) THEN
            JPHCO         = JPHCO + 1
            JTPHCO(JPHCO) = IPHCO
          ENDIF
 520    END DO
C-- Require a PHCO component, from ITEFPH or ITEFPC
 529    IF( JPHCO.LE.0 ) GOTO 500
C-- Test angle with ITH
        CTMCHC = QCOSA(ITK,ITEF)
        IF( CTMCHC.GT.CTMX ) THEN
          CTMX = CTMCHC
          IEMX = ITEF
        ENDIF
        IF( CTMCHC.LT.CTAMH  ) GOTO 500
C-- Bingo ... we have match; Check for repetition in matching to this input
        NEFUSE = 0
        DO 580 KK = 1,MIN(NMCEF(NMCIN),MXMTCH)
          IF( ITMCEF(NMCIN,KK).EQ.ITEF ) NEFUSE = NEFUSE + 1
 580    END DO
        IF( NEFUSE.GT.0 ) GOTO 500
C-- Increment count of matched EF objects,
        KNMCHC       = KNMCHC + 1
        NMCEF(NMCIN) = NMCEF(NMCIN) + 1
C-- Save ALPHA number of matching MC track
        ITMCEF(NMCIN,NMCEF(NMCIN)) = ITEF
C-- Save matching angle  ...
        AMCHC = -ACOS(MIN(CTMCHC,0.999999))
        SMCEF(NMCIN,NMCEF(NMCIN))  = AMCHC
C-- PBT debug
        IF( FCDEBU ) WRITE(6,1012) ITK,NMCIN,NMCEF(NMCIN),KNMCHC
     +    ,ITFK,ITFKPH,ITEFPH,ITEF,AMCHC,QE(ITEF),QE(ITK),KEFOTY(ITK)
     +    ,CQTPN(ITK)
 1012   FORMAT(' KNMCHC I: ',8(1X,I3),1X,F6.3,2(1X,F6.2),1X,I2,1X,A6)
C
 500  END DO
C
      IF( FCDEBU .AND. KNMCHC.EQ.0 .AND. NEFUSE.EQ.0 ) THEN
        WRITE(6,1013) ITK,IEMX,CTMX
 1013   FORMAT(' KNMCHC W: No angular match made - closest was '
     +   ,2(I3,1X),F8.4)
      ENDIF
C
 999  RETURN
      END
#endif
