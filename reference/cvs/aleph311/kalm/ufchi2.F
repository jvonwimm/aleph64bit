      FUNCTION UFCHI2(CHI2,NDEG)
C! Return probability for a given Chi^2 and #DOF
#ifndef DOC
      UFCHI2 = 1.
      IF(CHI2.LE.0. .OR. NDEG.LT.1 .OR. NDEG.GT.2) RETURN
      IF(NDEG.EQ.1)THEN
        UFCHI2 = PROB(CHI2,1)
      ELSE
        UFCHI2 = EXP(-CHI2/2.)
      ENDIF

      RETURN
      END
#endif
