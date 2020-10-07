      LOGICAL FUNCTION ARISTO(ITHETA,JPHI)
C-------------------------------------------------------------------
C     M. Verderi                                             2-10-94
C! True if the tower (itheta,jphi) has a face on a crack
C-------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
      INTEGER ITHETA, JPHI
      INTEGER SUBCOM, MODNUM, REGCOD
      INTEGER JPHMOD, COLUMN

      ARISTO = .FALSE.
      IF (ITHETA.EQ.0.OR.JPHI.EQ.0) RETURN
      CALL EMDTOW(ITHETA,JPHI,SUBCOM,MODNUM,REGCOD)

C Column = nombre divisions phi ( voir Ecal Geo Pack pour def regcod )
      COLUMN = REGCOD*8
C Barrel :
      IF (SUBCOM.EQ.2) THEN
         JPHMOD = JPHI  - (MODNUM-1)*COLUMN
C EndCap :
      ELSE
         JPHMOD = JPHI  - (MODNUM-1)*COLUMN + COLUMN/2
         IF (JPHMOD.GT.COLUMN) JPHMOD = JPHMOD - COLUMN*12
      ENDIF

      IF (JPHMOD.EQ.1.OR.JPHMOD.EQ.COLUMN) ARISTO = .TRUE.
      RETURN
      END
#endif
