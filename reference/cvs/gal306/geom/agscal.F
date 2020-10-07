*DK agscal
      SUBROUTINE AGSCAL
C ----------------------------------------------------------
C - B.Bloch-Devaux and J.P.Schuller 910930
C! build SCAL geometry
CKEY SCAL GEOM
C - called by AGEOME
C - calls     AGGEAN,AGSI92,AGDIMP              from this Lib
C - calls     ALTELL                            from ALEPHLib
C ---------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "iocom.h"
#include "agcons.h"
      EXTERNAL JHOCHA
C --------------------------------------------------------
C - get setup code and build SCAL from data base banks
C
      CALL AGGEAN ('SI',ISIST)
C
      IF (ISIST.LT.1) THEN
         CALL ALTELL ('AGSCAL: SCAL<92 is not implemented ',0,'STOP')
      ELSEIF (ISIST.GE.1) THEN
C
C     Define sensitive volume flag as defined if SET 'SCAL' is selected
C
         IF(IAGSLV+1.GT.LSENV) GO TO 992
         IAGSLV=IAGSLV+1
         IAGSEN(IAGSLV,1)=JHOCHA('SSIL')
C     store number (1-NMOD) of SMOD which is level 8
         IAGSEN(IAGSLV,2)=    8
         CALL AGSI92
      ENDIF
C
C    Store volume name and level in the geometry tree which define
C    entrance in detector
C
      CALL AGDIMP('SACT',7,'SCAL')
      GOTO 999
C - error
C
C   not enough space to SAVE sensitive module
  992 CONTINUE
      WRITE (LOUTIO,*) ' AGSCAL : too many sensitive volumes, IAGSLV= '
     &   ,  IAGSLV, ' LSENV= ',LSENV
      GOTO 991
C
 991  CALL ALTELL ('AGSCAL: cannot build SCAL geometry ',0,'STOP')
C - end
C
  999 CONTINUE
      RETURN
      END
#endif
