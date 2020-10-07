C! get logical unit number in characters
#if defined(UNIX)
      CHUNIT = '  '
      IF (LUN.LT.10) THEN
         CALL CSETDI (LUN,CHUNIT,1,1)
      ELSE
         CALL CSETDI (LUN,CHUNIT,1,2)
      ENDIF
      DSNAME = 'fort.'//CHUNIT
#else
      CHUNIT = '00'
      CALL CSETDI (LUN,CHUNIT,1,2)
#if defined(ALEPH_DEC)
      DSNAME = 'FOR0'//CHUNIT
#else
      DSNAME = 'IOFILE'//CHUNIT
#endif
#endif
#if defined(ARDEB)
      WRITE (IW(6),*) ' GTCHUNIT ',SUBR,' LUN= ',LUN,' CHUNIT= ',CHUNIT
     &               ,' DSNAME= ',DSNAME
#endif
C
