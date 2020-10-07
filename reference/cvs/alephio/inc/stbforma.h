C! set BOS file format
C  then pass format and record length to BOS
      IF (FTYP3.EQ.'EPI') THEN
         BFORMA = 'EPIO'
C       record length in 16bit words
         LREC = KRECL/2
      ELSEIF (FTYP3.EQ.'EDI' .OR. FTYP3.EQ.'CAR') THEN
         BFORMA = 'TEXT'
         LREC = KRECL
      ELSEIF (FTYP3.EQ.'NAT') THEN
         BFORMA = 'FORT'
         LREC = LWRDS
      ENDIF
      BFORMA = BFORMA(1:4)//BFORM2
      IF (FTYP3.NE.'DAF') CALL BUNIT (LUN,BFORMA,LREC)
#if defined(ARDEB)
      IF (FTYP3.NE.'DAF') WRITE(IW(6),*) ' STBFORMA ',SUBR,' BFORMA= ',
     &                    BFORMA,' LUN= ',LUN,' LREC= ',LREC
#endif
C
