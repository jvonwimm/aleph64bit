#if defined(IBM)
C     create FILEDEF statment
         MSGB = 'FI     DISK '//TFIL
         LB = LENOCC(MSGB)
         MSGE = ' (PERM XTENT      LRECL 1024 RECFM F'
         IF (LUN.LT.10) THEN
            WRITE(MSGB(5:5),'(I1)') LUN
            MSGB(4:4) = '0'
         ELSE
            WRITE(MSGB(4:5),'(I2)') LUN
         ENDIF
         WRITE(MSGE(14:16),'(I3)') NBREC
         MSG = MSGB(1:LB)//MSGE
         LG = LENOCC(MSG)
         CALL VMCMS(MSG(1:LG),IER)
C     open the DAF in write mode
         CALL INITDA(LUN,IRECL,NREC,'ADAM')
         OPEN(LUN,STATUS='OLD',ACCESS='DIRECT',RECL=IRECL*4,ERR=90,
     &          IOSTAT =IOP)
         IW(1) = 2
         CALL BDABF(LUN,IRECL,'   ','ADAM')
#else
#if defined(ALEPH_SGI) || defined(ALEPH_DEC)
C     open the DAF in write mode
       OPEN(LUN,FILE=TFIL,STATUS='NEW',ACCESS='DIRECT',
     &      ORGANIZATION='RELATIVE',RECL=IRECL,FORM='UNFORMATTED',
     &      ERR=90)
       IW(1) = 2
       CALL INITDU (LUN,IRECL,NREC,'ADAM')
       IW(1) = 0
       CALL BDABF (LUN,IRECL,TFIL(1:LNBLNK(TFIL)),'ADAM')
#else
#if defined(ALEPH_LINUX)
C     open the DAF in write mode
       OPEN(LUN,FILE=TFIL,STATUS='NEW',ACCESS='DIRECT',
     &      RECL=IRECL*4,FORM='UNFORMATTED',
     &      ERR=90)
       IW(1) = 2
       CALL INITDU (LUN,IRECL,NREC,'ADAM')
       IW(1) = 0
       CALL BDABF (LUN,IRECL,TFIL(1:LNBLNK(TFIL)),'ADAM')
#else
#if defined(CRAY)
      OPEN (LUN,FILE=TFIL,STATUS='NEW',
     &      ACCESS='DIRECT',
     &      RECL=IRECL*8,FORM='UNFORMATTED',ERR=90,IOSTAT=IOP)
      CALL INITDU (LUN,IRECL,NREC,'ADAM')
      IW(1) = 0
      CALL BDABF(LUN,IRECL,TFIL,'ADAM')
      IF (INDEX(TDEVI,'DISP').NE.0) THEN
         CALL ADSPOS (LUN,'    ',TFIL, TATYP(1:3),IER)
         IF (IER .NE.0 ) WRITE (IW(6),*) '  error in DISPOSE ',
     &          LUN,TFIL,TATYP(1:3),IER
      ENDIF
#else
#if defined(APOLLO) || defined(ALEPH_HP) || defined(RS6K)
      OPEN (LUN,FILE=TFIL,STATUS='NEW',
     &      ACCESS='DIRECT',
     &      RECL=IRECL*4,FORM='UNFORMATTED',ERR=90,IOSTAT=IOP)
      CALL INITDU (LUN,IRECL,NREC,'ADAM')
      IW(1) = 0
      CALL BDABF(LUN,IRECL,TFIL(1:LNBLNK(TFIL)),'ADAM')
#endif
#endif
#endif
#endif
#endif




