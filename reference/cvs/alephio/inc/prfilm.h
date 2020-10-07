C! print fname,ftype,fdevi from a FILM card
#if defined(ARDEB)
      WRITE (IW(6),*) SUBR,' AFILMI IRET= ',IRET
      IF (IRET.NE.-1) THEN
         LN = LNBLNK(NAMDAT)+1
         LT = LNBLNK(TYPDAT)+1
         WRITE (IW(6),*) ' NAMDAT= ',NAMDAT(1:LN),' TYPDAT= ',
     &                   TYPDAT(1:LT), ' DEVDAT= ',DEVDAT
      ENDIF
#endif
