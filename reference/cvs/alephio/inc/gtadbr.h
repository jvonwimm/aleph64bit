C! get ADBR bank from data base
      IF (NADBR.EQ.0) THEN
         NADBR = NAMIND('ADBR')
         LDBAS = JUNIDB(0)
         JADBR = MDARD (IW,LDBAS,'ADBR',0)
         IF (JADBR.EQ.0) GOTO 998
      ENDIF
C
C - next entry
      JADBR = IW(NADBR)
      IF (JADBR .EQ. 0) GOTO 998
      LADBR = LROWS(JADBR)
      IDET = INDEX (DLIST,DET)
      IF (IDET.EQ.0) THEN
         JDETJJ = 0
      ELSE
         JDETJJ = JADBFR+IDET/2+1
      ENDIF
C
