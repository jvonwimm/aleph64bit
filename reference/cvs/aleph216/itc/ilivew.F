      SUBROUTINE ILIVEW(LIST,IER)
C-----------------------------------------------------------------------
CKEY ITC WIRE ILIV
C! Create 'Live' wire bank ILIV
C!
C!    Author     :- W. Atwood
C!    Modified   :- J. Sedgbeer  89/12/08
C!    Modified   :- J. Sedgbeer  91/01/07 Ensure correct IWST bank used.
C!
C!    Input:
C!      LIST   /C    : BOS list to which ILIV is to be added e.g. 'T+'
C!      commons:     /BCS/     dbase bank IWST
C!      params:      IWSTJJ
C!
C!    Output:
C!      IER    /I    : = 0 successful
C!                     = 1 input bank is empty or does not exist
C!                     = 2 not enough space (garbage collection may
C!                                  have been done).
C!                     =-1 O.K. but garbage collection
C!      ILIV bank
C!
C!   calls     : none
C!
C! ILIVEW:
C! Create bank ILIV of 'live' ITC wires for this event. ILIV gives
C! the status of a wire (channel) - see DDL.
C!
C?  If first then Set name indices and bank formats
C?  Create ILIV bank
C?  Get IWST bank
C?  Loop over wires in IWST
C?    fill ILIV
C?  End Loop
C?  Add ILIV to BOS list
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C I/O commons and parameters
#include "bcs.h"
#include "iwstjj.h"
      CHARACTER*(*) LIST*(*)
      EXTERNAL NAMIND
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      DATA NWMAX/960/
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
      IF(FIRST) THEN
        FIRST = .FALSE.
        NIWST = NAMIND('IWST')
        NILIV = NAMIND('ILIV')
        CALL BKFMT('ILIV','I')
      ENDIF
      IER = 1
C
C Create ILIV bank. Use IGARB to note if garbage collection done.
C
      IGARB = 0
      CALL AUBOS('ILIV',0,LMHLEN+NWMAX,JILIV,IER)
      IF(IER.EQ.2) GOTO 999
      IF(IER.EQ.1) IGARB = 1
C
      IW(JILIV+LMHCOL) = 1
      IW(JILIV+LMHROW) = NWMAX
C
C-----------------------------------------------------------------------
C Check for validity of IWST bank.
C If valid IWST bank then loop over wires in IWST - fill ILIV
C
      JIWST = IW(NIWST)
      IF(JIWST.GT.0) THEN
        NWIR = LROWS(JIWST)
        DO 10 I=1,NWIR
          KK = KROW(JIWST,I)
          IWIR = IW(KK+JIWSIW)
          IFLG = IW(KK+JIWSFL)
          IW(JILIV+LMHLEN+IWIR) = IFLG
   10   CONTINUE
      ENDIF
C
C-----------------------------------------------------------------------
C Add ILIV to list
C
      CALL BLIST (IW,LIST,'ILIV')
C
C If garbage collection then set error flag to -1
C
      IER = 0
      IF(IGARB.EQ.1) IER = -1
C
  999 CONTINUE
      END
#endif
