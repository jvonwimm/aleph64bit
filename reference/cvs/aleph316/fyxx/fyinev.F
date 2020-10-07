      SUBROUTINE FYINEV(IBFUL)
C ----------------------------------------------------------
CKEY FYXX  /INTERNAL
C J.Hilgart - 871307          mods 310588
C! Fxxx DST format event initialization.
C If this routine has already been called in this event, do not
C repeat certain steps dealing w/ MC info.
C OUTPUT : IBFUL = -1 if not enough space to book a bank
C
C - Called from    USER
C - Calls          BLIST,BWIND,BDROP,WBANK,WDROP    from BOS.OLB
C                  FYKILL                           from ALEPHLIB
#ifndef DOC
      SAVE
#include "bcs.h"
#include "fyrela.h"
#include "kgjjpar.h"
      DATA IONC /0/
C
#include "bmacro.h"
C ====================================================================
C Name indices
      IF (IONC.EQ.0) THEN
         NKINE = NAMIND('KINE')
         NVERT = NAMIND('VERT')
         NCALO = NAMIND('CALO')
         NKEVH = NAMIND('KEVH')
         NKHIS = NAMIND('KHIS')
         IONC = 1
      ENDIF
      IBFUL = 0
C
C Drop any negative KINE banks to avoid any trouble
      KK = NKINE + 1
 2    KK = IW(KK-1)
      IF (KK .GT. 0) THEN
         NR = IW(KK-2)
         IF (NR .GT. 0) GOTO 3
         IDRP = NDROP('KINE',NR)
         GOTO 2
      ENDIF
 3    CONTINUE
C
C Get no. of tracks and vertices which came from event generator
      JKEVH = IW(NKEVH)
      IF (JKEVH .NE. 0) THEN
         KINTRK = ITABL(JKEVH,1,JKEVNT)
         KINVER = ITABL(JKEVH,1,JKEVNV)
      ELSE
         KINTRK = 0
         KINVER = 0
      ENDIF
C
C Drop previous Fxxx banks
      LEN = LNBLNK (ELISAD)
      CALL BDROP(IW,ELISAD(1:LEN))
C
C Add the Fxxx banks to E list.
      CALL BLIST(IW,'E+',ELISAD(1:LEN))
C
C - get all KINE and VERT indices
      CALL BWIND (IW,'KINE',MAXMCX,INDKIN)
      IF (INDKIN.EQ.0) GOTO 990
      LAST = IW(INDKIN)
      KLAST = IW(INDKIN+LAST)
      IF (IW(KLAST-1).NE.0) GOTO 989
      CALL BWIND (IW,'VERT',MAXMCX,INDVER)
      IF (INDVER.EQ.0) GOTO 990
      LAST = IW(INDVER)
      KLAST = IW(INDVER+LAST)
      IF (IW(KLAST-1).NE.0) GOTO 989
C
C Set work banks JDKEKS and JDVNFO with new track #s and new
C     vertex #s or 0 if the track or the vertex has to be dropped.
       CALL FYKILL (IBFUL)
       IF (IBFUL .EQ. -1) GOTO 990
C
C normal, healthy return
      RETURN
C sick return: not enough space to book bank...
 989  CONTINUE
      WRITE (IW(6),*) ' FYINEV MAXMCX in /FYRELA/ is too small '
 990  CONTINUE
      IBFUL = -1
      IW(1) = LFXWBK
      CALL WDROP (IW,JDFXWB)
      END
#endif
