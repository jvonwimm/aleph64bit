*DK aswrtp
      SUBROUTINE ASWRTP (LIST)
C ----------------------------------------------------------------------
C - F.Ranjard - 850903
C! Write output file
C - Input   : LIST   / A  = 'C' or 'E'  list
C - Called by    ASCEVE                                  from this .HLB
C
C ----------------------------------------------------------
#ifndef DOC
      SAVE
      CHARACTER*1 LIST
#include "jqcom.h"
#include "iocom.h"
#include "jobcom.h"
      INTEGER ALRUNR, ALFIND
      CHARACTER*4 NLISTF, NAME
C ----------------------------------------------------------------------
C
      IF (LIST.EQ.'E') THEN
C - Event record
         NAME = 'EVEH'
C
      ELSE
C
C - parameters/ calibration/ Geant3 geometry ....
         NAME = 'RUNR'
C
C - build the RUNR bank if it does not exist
         IF (IW(NARUNR) .EQ. 0) THEN
            JRUNR = ALRUNR (IEXPJO,IRUNJO)
            IF (JRUNR .EQ. 0)
     &      CALL ALTELL ('ASWRTP: not enough space for RUNR ',1,'STOP')
         ENDIF
      ENDIF
C
C   check that the bank RUNR is 1st in the C-list , or
C   EVEH on E-list. If not put it first.
C
      IF (NLISTF(IW,1,LIST) .NE. NAME) THEN
         CALL BDROP (IW,'T')
         CALL BLIST (IW,'T=',LIST)
         CALL BLIST (IW,LIST//'=',NAME)
         CALL BLIST (IW,LIST//'+','T')
         CALL BLIST (IW,'T=','0')
      ENDIF
C
C - suppress empty banks from the list
C
      I=0
 10   I=I+1
      NAME = NLISTF (IW,I,LIST)
      IF (NAME .NE. '    ') THEN
         JNAME = IW(NAMIND(NAME))
         IF (JNAME.EQ.0) CALL BLIST (IW,LIST//'-',NAME)
         GOTO 10
      ENDIF
C
C - write the record
C
      IF (MGETJO.EQ.0) THEN
C     no input tape so cannot use ABRSEL package
         CALL BWRITE (IW,LSAVIO,LIST)
      ELSE
         CALL ABWSEL (LIST)
      ENDIF
C
      RETURN
      END
#endif
