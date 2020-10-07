      SUBROUTINE ABWSEL (LIST)
C --------------------------------------------------------------
CKEY ALPHARD WRITE
C! set write-event flag
C - F.Ranjard - 930720      from H.Albrecht
C - Input : LIST / A1 = one character output list
C           Recommended : 'E' , 'R', 'C'. Forbidden : ' ', 'Z'.
C           This parameter is ignored if the output is an event
C           directory which refers to an input file.
C
C   ABWSEL sets an output flag. Actual output is done during the next
C   call to ABRREC. Thus, there is no danger to call it more than once
C   for one event. For the very last event, ABWEND (see below) has
C   to be called.
C
C ----------------------------------------------------------------
#ifndef DOC
      CHARACTER*1 LIST
#include "bcs.h"
#include "abrcom.h"
#include "bitconst.h"
C --------------------------------------------------------------
#ifndef DOC
      IF (WLIST.NE.' ' .AND. WLIST.NE.LIST) CALL ABWEVE
      WLIST = LIST
C
      END
#endif
#endif
