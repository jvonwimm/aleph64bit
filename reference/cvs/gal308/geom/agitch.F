*DK agitch
      SUBROUTINE AGITCH
C---------------------------------------------
C!   Build ITC chamber and related cables and supports geometry
C    Modified : B.Bloch-Devaux May 1993 rewritten to get ITC GEANT
C               Geometry from Data BAse banks
C.  -Called by AGEOME                        from this .HLB
C.  -Calls AGGEAN,AGDIMP,ALTELL              from this .HLB
C. -Builds geometry levels below 'CDET' level for ITC  part
C. -Initialises some search optimisation
C------------------------------------------------
#ifndef DOC
#include "agcons.h"
C ----------------------------------------------------------------------
C - get setup code and build ITC  from data base banks
C
      CALL AGGEAN ('IT',ITCST)
C
C   Define where to find slot number
C
      IF (IAGSLV.GE.LSENV) GOTO 998
      IAGSLV=IAGSLV+1
      IAGSEN(IAGSLV,1)=JHOCHA('ITC ')
      IAGSEN(IAGSLV,2)=4
C
C    Store volume name and level in the geometry tree which define
C    entrance in detector
C
      CALL AGDIMP('ITC ',4,'ITC ')
      GOTO 999
C
C - not enough space to save sensitive module
C
 998  CONTINUE
      WRITE (LOUTIO,*) ' AGITCH : too many sensitive volumes, IAGSLV= '
     &   ,  IAGSLV, ' LSENV= ',LSENV
 991  CALL ALTELL ('AGITCH: cannot build ITC  geometry ',0,'STOP')
C
 999  CONTINUE
      RETURN
      END
#endif
