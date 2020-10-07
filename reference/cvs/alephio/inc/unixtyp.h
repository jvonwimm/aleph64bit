C! Define UNIX type: bin, ascii
C
      IF (FTYP3.EQ.'EDI' .OR. FTYP3.EQ.'CAR') THEN
         UXTYP = 'ascii'
      ELSEIF (FTYP3.EQ.'NAT'.OR.FTYP3.EQ.'DAF'.OR.FTYP3.EQ.'EPI') THEN
         UXTYP = 'bin  '
      ELSEIF (FTYP3.EQ.'HIS' .OR. FTYP3.EQ.'EXC') THEN
         UXTYP = 'bin  '
      ELSE
         WRITE (IW(6),*) SUBR, ' UNIXTYP unkown type ', FTYP3
         IST = 5
         GOTO 98
      ENDIF
#if defined(ARDEB)
      WRITE(IW(6),*) SUBR,' UNIXTYP : ',UXTYP(1:5)
#endif
