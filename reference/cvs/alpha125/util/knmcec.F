      INTEGER FUNCTION KNMCEC(ITK)
C***********************************************************************
C Find the alpha EFLW objects with PECO part matching input MC track.
C The matching is circuitous -- using PEMH bank, FKIN number of MC input, and
C PECO parts of EFLW objects
C Author: Paul Bright-Thomas
C Mods:   03/03/97 PBT Created.
C         04/03/97 PBT Note that LCAL clusters are also in PECO/PEMH
C         05/03/97 PBT Yet to be updated for PCOB objects (KEFOLC)
C***********************************************************************
#ifndef DOC
#include "qdecl.h"
#include "qcde.h"
#include "qhac.h"
#include "mcef.h"
      INTEGER ITK
      INTEGER KPEMH,NPEMH,IPEMH,NEFUSE
      INTEGER KPECO,NPECO,IPECO,JPECO
      INTEGER ITFK,ITMFK,ITEF,ITFKPE,ITFKEF
     +       ,ITEFPE,ITEFPC,ITEFPT,ITEFPH,ITEFTY,JTPECO(MXMTCH)
      REAL    EFKIN
      INTEGER IMCEF
#include "qmacro.h"
C***********************************************************************
      KNMCEC = 0
C-- Were we given an MC track number ?
      IF( ITK.LT.KFMCT .OR. ITK.GT.KLMCT ) GOTO 999
C-- Get FKIN number for ALPHA MC particle.
      ITMFK = KTN(ITK)
      IF( ITMFK.LE.0 ) GOTO 999
C-- Link the PEMH bank
      KPEMH  = 0
      NAPEMH = NAMIND('PEMH')
      IF( NAPEMH.GT.0 ) KPEMH = IW(NAPEMH)
      IF( KPEMH.GT.0 )  NPEMH = LROWS(KPEMH)
      IF( KPEMH.EQ.0 ) GOTO 499
C-- Link the PECO bank
      KPECO  = 0
      NAPECO = NAMIND('PECO')
      IF( NAPECO.GT.0 ) KPECO = IW(NAPECO)
      IF( KPECO.GT.0  ) NPECO = LROWS(KPECO)
C-- EFLW ECAL: loop over PEMH to find EFLW obj w matching PECO cluster
 201  DO 200 IPEMH = 1,NPEMH
C-- Get FKIN number for this cluster-FKIN match
        ITFK   = ITABL(KPEMH,IPEMH,JPEMTN)
        IF( ITFK.NE.ITMFK ) GOTO 200
C-- Get PECO number for this cluster-FKIN match
        ITFKPE = ITABL(KPEMH,IPEMH,JPEMCO)
        IF( ITFKPE.LE.0 ) GOTO 200
C-- Search all EFLW objects for same PECO component ... or PCOB->PECO
        ITEF   = KPDIR('EFLW',KRECO)
 210    IF( ITEF.LE.0 ) GOTO 200
C-- Get PECO/PCOB part of EFLW object ... test against PEMH match.
        ITEFPE = KEFOLE(ITEF)
        ITEFPC = KEFOLC(ITEF)
        IF( ITEFPE.LE.0 .AND. ITEFPC.LE.0 ) GOTO 230
C-- Store relevant PECO row numbers, from ITEFPE or ITEFPC, in JTPECO
        JPECO  = 0
        ITFKEF = 0
        IF( ITEFPE.GT.0 ) THEN
          JPECO         = JPECO + 1
          JTPECO(JPECO) = ITEFPE
        ENDIF
C-- Search PECO for rows matching PCOB
        IF( ITEFPC.EQ.0  .OR. KPECO.EQ.0 ) GOTO 199
        DO 100 IPECO = 1,NPECO
          IPC = ITABL(KPECO,IPECO,JPECPC)
          IF( IPC.EQ.ITEFPC .AND. JPECO.LT.MXMTCH ) THEN
            JPECO         = JPECO + 1
            JTPECO(JPECO) = IPECO
          ENDIF
 100    END DO
C-- Are there any PECO objects to match ?
 199    IF( JPECO.LE.0 ) GOTO 230
C-- Search through JTPECO list of PECO objects for any belonging to ITFK
        DO 240 JJ = 1,JPECO
          IF( ITFKPE.EQ.JTPECO(JJ) ) ITFKEF = ITFKPE
 240    END DO
        IF( ITFKEF.LE.0 .OR. ITFKEF.NE.ITFKPE ) GOTO 230
C-- Check for repetition in matching to this input
        NEFUSE = 0
        DO 250 KK = 1,MIN(NMCEF(NMCIN),MXMTCH)
          IF( ITMCEF(NMCIN,KK).EQ.ITEF ) NEFUSE = NEFUSE + 1
 250    END DO
        IF( NEFUSE.GT.0 ) GOTO 230
C-- We have a match ... check array bounds
        IF( NMCEF(NMCIN).GE.MXMTCH ) THEN
          CALL QWMESE(' KNMCEC - increase MXMTCH limit.')
          GOTO 999
        ENDIF
C-- Increment count of matched EF objects,
        KNMCEC       = KNMCEC + 1
        NMCEF(NMCIN) = NMCEF(NMCIN) + 1
C-- Save ALPHA number of matching EFLW object.
        ITMCEF(NMCIN,NMCEF(NMCIN)) = ITEF
C-- Save matching energy
        EFKIN  = FLOAT(ITABL(KPEMH,IPEMH,JPEMSE)) / 1000.
        SMCEF(NMCIN,NMCEF(NMCIN))  = EFKIN
C-- PBT debug
        IF( FCDEBU ) WRITE(6,1001) ITK,NMCIN,NMCEF(NMCIN),KNMCEC
     +    ,ITMFK,ITFK,ITFKPE,ITFKEF,ITMC,EFKIN,QE(ITMC),QE(ITEF)
     +    ,KEFOTY(ITEF)
 1001   FORMAT(' KNMCEC I: ',9(1X,I3),3(1X,F6.2),1X,I2)
C-- Next EFLW object.
 230    ITEF   = KFOLLO(ITEF)
        GOTO 210
C
 200  END DO
C
 499  IF( FCWARN .AND. KNEVT.EQ.1 .AND. NMCIN.EQ.1 ) WRITE(6,*)
     +  ' KNMCEC W: No PEMH matching information - no MC-ECAL matches.'
C
 999  RETURN
      END
#endif
