      SUBROUTINE AGSMBA
C ----------------------------------------------------------
C - B.Bloch-Devaux                  911210
C! build SAMBA geometry for materials in GEANT
CKEY SAMBA GEOM
C - called by AGSATR
C - calls     AGGEAN                            from this Lib
C - calls     ALTELL                            from ALEPHLib
C ---------------------------------------------------------
C --------------------------------------------------------
C - get setup code and build SAMBA from data base banks
C
      CALL AGGEAN ('PM',ISAST)
C
      IF (ISAST.LE.0) THEN
         CALL ALTELL ('AGSMBA: SAMBA<92 is not implemented ',0,'STOP')
      ENDIF
      RETURN
      END
