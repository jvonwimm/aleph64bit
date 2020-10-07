      SUBROUTINE MINDST(ELIST,RLIST)
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Steering routine to create Mini-DST banks not already on POT.
C
C     Author: Stephen Haywood      22-Jan-90
C     Modify: Stephen Haywood      06-Jan-93 ! Break up into parts
C
C     Input  : POT banks
C     Output : MDST banks
C              ELIST = event bank-list (character*800)
C              RLIST = run   bank-list (character*800)
C
C     A list of banks which are formed for the event are stored in the
C     string MLISTE in / MINCOM /.
C     The event and run lists are also passed as local variables.
C-----------------------------------------------------------------------
#ifndef DOC
C
#include "bcs.h"
#include "mincom.h"
      CHARACTER*800 ELIST,RLIST,WLIST
      CHARACTER*4 NAME
      LOGICAL FIRST,CLASS
      DIMENSION CLASS(32)
      SAVE FIRST,NWANT,WLIST
      DATA FIRST / .TRUE. /
C
#include "bmacro.h"
C-----------------------------------------------------------------------
C++   Initialisation.
C
      IF(FIRST) THEN
         CALL MININI(NWANT,WLIST)
         FIRST = .FALSE.
      ENDIF
C
C++  Preparation for this event.
C
      CALL MINPRE(WLIST, CLASS,IMINI,KEEP)
C
C++   If we are reading Mini with wish-list, may want to include all
C++   MC banks.
C
      IF (IMINI.EQ.1) GOTO 1000
C
C++   If we are reading Mini without wish-list, E-list is copied in its
C++   entirety - nothing further to do, unless user wishes to add banks.
C
      IF (IMINI.EQ.2) GOTO 2000
C
C++   If event is not destined to be kept, user may still wish to add
C++   his own banks.
C
      IF (KEEP.EQ.0) GOTO 2000
C
C++   Build Mini banks for data.
C
      CALL MINBLD(NWANT,WLIST,KEEP,CLASS)
C
C++   If MC data, then add MC banks, unless NOMC card is used.
C++   The nature of the data is determined in MINRUN.
C
 1000 CONTINUE
      IF(MINIMC) CALL MINBLM
C
C++   Add requested banks to Mini.
C
 2000 CONTINUE
      CALL MINADD
C
C++   Finally, all banks are copied to the local lists.
C
      ELIST = MLISTE
      RLIST = MLISTR
C
      RETURN
      END
#endif
