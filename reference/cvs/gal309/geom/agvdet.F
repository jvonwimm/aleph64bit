*DK agvdet
      SUBROUTINE AGVDET
C ----------------------------------------------------------
CKEY VDET GEOM
C! build VDET geometry
C - W.Manner - 901100
C - Modified : A. Bonissent 940215 : use geom. package
C  VDET91-95 is the same object built with AGVF92
C  a new description of the wafers was introduced in 94.
C  new VDET was introduced in 9510, it is built with AGVF95
C - Modified : FLR 961113 - new description of the faces
C  to avoid VFAC volumes which were overlapping each other.
C  it is built with AGVF96 and ADBSCONS no. > 218
C - Modified: S. Wasserbaech 971216 - use the same
C  description for 1991-1995
C - called by AGEOME
C ---------------------------------------------------------
#ifndef DOC
#include "jobcom.h"
#include "iocom.h"
#include "agcons.h"
#include "bcs.h"
C
C --------------------------------------------------------
C - get setup code and build VDET from data base banks
C
      CALL AGGEAN ('VD',IVDST)
C
C - position faces depending on the setup code
C
      IF (IVDST .LT. 3) THEN
C      VDET89-90 is not implemented 
         CALL ALTELL ('AGVDET: VDET 89-90 is not implemented ',0,'STOP')
      ELSEIF (IVDST .LT. 8) THEN
C      VDET91-95 old VDET 
         CALL AGVF92
         IF(IAGSLV+2.GT.LSENV) GO TO 992
         IAGSLV = IAGSLV + 1
         IAGSEN(IAGSLV,1) = JHOCHA('VDWS')
         IAGSEN(IAGSLV,2) = 4
      ELSE
         IF (IDAFJO .GT. 218) THEN
C         VDET9611 new VDET description
            CALL AGVF96
         ELSE
C         VDET9510 new VDET 
            CALL AGVF95
         ENDIF
         IF(IAGSLV+2.GT.LSENV) GO TO 992
         IAGSLV=IAGSLV+1
         IAGSEN(IAGSLV,1)=JHOCHA('VWSS')
         IAGSEN(IAGSLV,2)=    4
      ENDIF
C
C    Store volume name and level in the geometry tree which define
C    entrance in detector
C
      CALL AGDIMP('VDET',3,'VDET')
      RETURN
C
C - error ---------------------------------------------------------
C
C   not enough space to SAVE sensitive module
  992 CONTINUE
      WRITE (LOUTIO,*) ' AGVDET : too many sensitive volumes, IAGSLV= '
     &   ,  IAGSLV, ' LSENV= ',LSENV
C
      CALL ALTELL ('AGVDET: cannot build VDET geometry ',0,'STOP')
C - end
C
      END
#endif
