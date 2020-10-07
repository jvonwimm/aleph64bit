C!  get file type and record length
C   close previous file on the same unit
C   pass file name to EPIO if required
      IF (ATYPE(1:1).EQ.' ') THEN
        IF (FNAME(1:1).NE.' ') THEN
          LE  = LNBLNK(FNAME)
C        pass VAX directory name or UNIX path name if any
          I1 = INDEX(FNAME,']')
          IF (I1.EQ.0) THEN
            I1 = ICFILA('/',FNAME,1,LE)
            IF (I1.GT.LE) I1=0
          ENDIF         
C        look for '.' in VAX or UNIX name
C        if it does not exist look for ' ' in IBM name
          IBG = INDEX(FNAME(I1+1:LE),'.')
          IF (IBG.EQ.0) IBG = INDEX(FNAME(I1+1:LE),' ')
          IBG = ICNEXT (FNAME,I1+IBG+1,LE)
          FTYP3 = FNAME(IBG:IBG+2)
        ELSE
          FTYP3 = 'NAT'
        ENDIF
      ELSE
        FTYP3=ATYPE(1:3)
      ENDIF
      CALL CLTOU (FTYP3(1:3))
C
      IF (FTYP3.EQ.'DAF') DIRECT=.TRUE.
C
C - KRECL is the block length in bytes
      KRECL = KBYTREC (FTYP3,FDEVI)
C - LWRDS is the block length in words
      LWRDS = KRECL/NBYTW
C - MRECL is the block length in the unit required by the machine
#if defined(ALEPH_DEC) || defined(ALEPH_SGI)
      MRECL = LWRDS
#else
      MRECL = KRECL
#endif
C
C - existing files have to be closed before reopened
      CALL ACLOSE(LUN,IRCL)
C
C - initialize EPIO if necessary and send file name
      IF (FTYP3.EQ.'EPI') THEN
         CALL EPINIT
         LE = LNBLNK (FNAME)
         CALL EPDEFU (LUN,FNAME(1:LE),IST)
         IF (IST.NE.0) GOTO 98
      ENDIF
#if defined(ARDEB)
      WRITE (IW(6),*) ' GTFTYP ',SUBR,' FTYP3= ',FTYP3,' KRECL= ',KRECL,
     &                ' LWRDS= ',LWRDS,' MRECL= ',MRECL
#endif
C
