C location of data files
      CALL GETENV('ALDOC',ALDOCC)
      IF (ALDOCC .EQ. ' ') THEN
        CALL GETENV('ALEPH',ALDOCC)
        ILDOCC = LNBLNK(ALDOCC)
        ALDOCC(ILDOCC+1:) = '/doc'
      END IF
      ILDOCC = LNBLNK(ALDOCC)
