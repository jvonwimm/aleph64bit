      SUBROUTINE TPADS(IER)
C
C--------------------------------------------------------------------
C! Reconstruct pad clusters
C!
C!    Author:    D. Schlatter
C!    Modified:  R. Johnson    20-07-89
C!
C!    Output:
C!        - IER      /I    Error code = 1 if we run out of space
C!                                    = 2 if input banks are missing
C!                                    = 0 if all is OK
C!
C!    Input banks:
C!          TPAD:          TPC pad hits
C!          TPDI:          TPC pad digitizations
C!          TISL:          Optional; it will be created if not input
C!    Output banks:
C!          TPRL           Row list pointers to raw data
C!          TPUL,TCLU      TPC pad pulses and clusters
C!          TSPU,TSCL      TPC pad subpulses and subclusters
C!          TLRL,TSRL      Row pointers for clusters and subclusters
C!
C!    Called by TPREDA
C!
C!    Description
C!    ===========
C?    For each sector:
C?      Build a pad row list
C?      Find pad islands
C?      Set up cluster and pulse banks
C?      Find subpulses and subclusters
C!
C!    NOTE:  This routine plus those subroutines which it calls
C!           represent a MODULE within JULIA.
C!----------------------------------------------------------------------
#ifndef DOC
C
#include "rparac.h"
#include "tparac.h"
#include "tpgpar.h"
#include "tmonit.h"
#include "rcurnt.h"
#include "rlunit.h"
#include "bcs.h"
#include "tpgeom.h"
#include "ptstjj.h"
C
C--------------------- Local variables ---------------------------------
C
      CHARACTER TEXT*70
      LOGICAL LGARB,FIRST
      DATA FIRST/.TRUE./
C
C-----------------------------------------------------------------------
C
#include "bmacro.h"
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTPAD=NAMIND('TPAD')
        NTPDI=NAMIND('TPDI')
        NTISL=NAMIND('TISL')
        NTCLU=NAMIND('TCLU')
        NTLRL=NAMIND('TLRL')
        NTPRL=NAMIND('TPRL')
        NTPUL=NAMIND('TPUL')
        NTSCL=NAMIND('TSCL')
        NTSPU=NAMIND('TSPU')
        NTSRL=NAMIND('TSRL')
        NPTST=NAMIND('PTST')
      ENDIF
C
C++   Check that we have all necessary input banks
C
      IF (IW(NTPAD).EQ.0 .OR. IW(NTPDI).EQ.0) THEN
        IER=2
        GO TO 999
      ENDIF
C
C++   If any of the output banks already exist, drop them.
C
      IF (IW(NTCLU).NE.0) CALL BDROP(IW,'TCLU')
      IF (IW(NTLRL).NE.0) CALL BDROP(IW,'TLRL')
      IF (IW(NTPRL).NE.0) CALL BDROP(IW,'TPRL')
      IF (IW(NTPUL).NE.0) CALL BDROP(IW,'TPUL')
      IF (IW(NTSCL).NE.0) CALL BDROP(IW,'TSCL')
      IF (IW(NTSPU).NE.0) CALL BDROP(IW,'TSPU')
      IF (IW(NTSRL).NE.0) CALL BDROP(IW,'TSRL')
C
C++   Loop over all sectors with data
C
      KTPAD=IW(NTPAD)
      KTPDI=IW(NTPDI)
      KTISL=IW(NTISL)
  310 IF (KTPAD.EQ.0 .OR. KTPDI.EQ.0) GO TO 410
        ISLOT=IW(KTPAD-2)
        IF (ISLOT.LE.0 .OR. ISLOT.GT.NTSECT) THEN
          WRITE(TEXT,832) ISLOT
  832     FORMAT('Sector number ',I2,' out of range.')
          CALL RERROR('TPADS ',2,TEXT(1:30))
          KTPAD=IW(KTPAD-1)
          KTPDI=IW(KTPDI-1)
          IF (KTISL.GT.0) KTISL=IW(KTISL-1)
          IND=NDROP('TPAD',ISLOT)
          IND=NDROP('TPDI',ISLOT)
          IND=NDROP('TISL',ISLOT)
          GO TO 310
        ENDIF
        IF (IW(KTPDI-2).NE.ISLOT) THEN
          IND=NLINK('TPDI',ISLOT)
          IF (IND.EQ.0) THEN
            WRITE(TEXT,833) ISLOT
  833       FORMAT('Digitization bank missing for sector ',I2,'.')
            CALL RERROR('TPADS ',3,TEXT(1:40))
            KTPAD=IW(KTPAD-1)
            IF (KTISL.GT.0) KTISL=IW(KTISL-1)
            IND=NDROP('TPAD',ISLOT)
            IND=NDROP('TISL',ISLOT)
            GO TO 310
          ELSE
            KTPDI=IND
          ENDIF
        ENDIF
        IF (KTISL.NE.0) THEN
          IF (IW(KTISL-2).NE.ISLOT) THEN
            KTISL=NLINK('TISL',ISLOT)
          ENDIF
        ENDIF
        ISTYP=ITPTYP(ISLOT)
C
C++     Establish the ROWLIST bank for this sector
C
        CALL TPRLST(ISLOT,KTPAD,KTPDI,KTPRL,IGARB,IER)
        IF (IER.EQ.1) GO TO 999
        IF (IER.GT.1) THEN
          KPTST=IW(NPTST)
          IF (KPTST.NE.0) THEN
            IW(KROW(KPTST,ISLOT)+JPTSTP)=0
            IW(KROW(KPTST,ISLOT)+JPTSNT)=4*(IW(KTPAD)+IW(KTPDI))
          ENDIF
C
C++       Data format error:  drop data for this sector
C++       and go on to the next sector.
C
          IER=0
          WRITE(TEXT,473) ISLOT
  473     FORMAT('Data format error in sector ',I2,
     &           ' Dropping raw data for this sector.')
          CALL RERROR('TPADS',1,TEXT(1:67))
          NDRPTM(ISLOT)=NDRPTM(ISLOT)+1
          KTPAD=IW(KTPAD-1)
          KTPDI=IW(KTPDI-1)
          IF (KTISL.GT.0) KTISL=IW(KTISL-1)
          IND=NDROP('TPAD',ISLOT)
          IND=NDROP('TPDI',ISLOT)
          IND=NDROP('TISL',ISLOT)
          GO TO 310
        ENDIF
        IF (IGARB.NE.0) THEN
          KTISL=NLINK('TISL',ISLOT)
        ENDIF
C
C++     Fill the TISL bank for this sector (or just complete it
C++     if the TPP has already done part or all of it)
C
        CALL TISLND(ISLOT,KTPAD,KTISL,NSECL,IGARB,IER)
        IF (IER.NE.0) GO TO 999
        IF (IGARB.NE.0) THEN
          KTPDI=NLINK('TPDI',ISLOT)
          KTPRL=NLINK('TPRL',ISLOT)
        ENDIF
C
C++     Call routine to store clusters and pulses
C
        CALL TCLUST(ISLOT,NSECL,KTPAD,KTPDI,KTPRL,KTLRL,KTISL,
     &                               KTCLU,KTPUL,IGARB,IER)
        IF (IER.NE.0) GO TO 999
C
C++     Call routine to analyze clusters and pulses and fill banks
C++     of subclusters and subpulses
C
        CALL TCLCOR(KTPAD,KTPDI,KTPUL,KTCLU,KTLRL,ISLOT,IGARB,IER)
        IF (IER.NE.0) GO TO 999
        IF (IGARB.NE.0) THEN
          KTISL=NLINK('TISL',ISLOT)
        ENDIF
C
C++     End of the sector loop.  Find BOS indices for the next sector.
C
        KTPAD=IW(KTPAD-1)
        KTPDI=IW(KTPDI-1)
        IF (KTISL.NE.0) KTISL=IW(KTISL-1)
        GO TO 310
  410 CONTINUE
      IER=0
  999 CONTINUE
C
C++   Drop all the TISL banks
C
      CALL BDROP(IW,'TISL')
      RETURN
      END
#endif
