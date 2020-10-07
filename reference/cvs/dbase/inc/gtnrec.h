C! decode DAFB or OUTB data card to get NBREC, NBDIR and NREC
      JDAFB = NLINK ('DAFB',0)
      IF (JDAFB.EQ.0) THEN
         JDAFB = NLINK ('OUTB',0)
         IF (JDAFB.EQ.0) THEN
            WRITE(IW(6),*) SUBR, ' : no DAFB/OUTB card - STOP'
            STOP
         ENDIF
      ENDIF
      NBREC = IW(JDAFB+1)
      NBDIR = 0
      IF (IW(JDAFB).GT.1) NBDIR = IW(JDAFB+2)
      NREC = NBREC + NBDIR*10000
