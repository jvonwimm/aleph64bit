      SUBROUTINE ASREAD
C ----------------------------------------------------------------------
C. - F.RANJARD - 850329
C! Read asll input files
C. - Read input tape on unit# LGETIO
C. - Read data base
C. - called by    ASIJOB                           from this .HLB
C -  calls        ABRSEL, ABUNIT, BKINJO           from ALEPHLIB
C.                BLIST, MDARD                     from BOS77
C.                ASIRUN                           from this .HLB
C.
C -----------------------------------------------------
      SAVE
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
      PARAMETER (LOFFMC = 1000)
      PARAMETER (LHIS=20, LPRI=20, LTIM=6, LPRO=6, LRND=3)
      PARAMETER (LBIN=20, LST1=LBIN+3, LST2=3)
      PARAMETER (LSET=15, LTCUT=5, LKINP=20)
      PARAMETER (LDET=9,  LGEO=LDET+4, LBGE=LGEO+5)
      PARAMETER (LCVD=10, LCIT=10, LCTP=10, LCEC=15, LCHC=10, LCMU=10)
      PARAMETER (LCLC=10, LCSA=10, LCSI=10)
      COMMON /JOBCOM/   JDATJO,JTIMJO,VERSJO
     &                 ,NEVTJO,NRNDJO(LRND),FDEBJO,FDISJO
     &                 ,FBEGJO(LDET),TIMEJO(LTIM),NSTAJO(LST1,LST2)
     &                 ,IDB1JO,IDB2JO,IDB3JO,IDS1JO,IDS2JO
     &                 ,MBINJO(LST2),MHISJO,FHISJO(LHIS)
     &                 ,IRNDJO(LRND,LPRO)
     &                 ,IPRIJO(LPRI),MSETJO,IRUNJO,IEXPJO,AVERJO
     3                 ,MPROJO,IPROJO(LPRO),MGETJO,MSAVJO,TIMLJO,IDATJO
     5                 ,TCUTJO(LTCUT),IBREJO,NKINJO,BKINJO(LKINP),IPACJO
     6                 ,IDETJO(LDET),IGEOJO(LGEO),LVELJO(LGEO)
     7                 ,ICVDJO(LCVD),ICITJO(LCIT),ICTPJO(LCTP)
     8                 ,ICECJO(LCEC),ICHCJO(LCHC),ICLCJO(LCLC)
     9                 ,ICSAJO(LCSA),ICMUJO(LCMU),ICSIJO(LCSI)
     &                 ,FGALJO,FPARJO,FXXXJO,FWRDJO,FXTKJO,FXSHJO,CUTFJO
     &                 ,IDAFJO,IDCHJO,TVERJO
      LOGICAL FDEBJO,FDISJO,FHISJO,FBEGJO,FGALJO,FPARJO,FXXXJO,FWRDJO
     &       ,FXTKJO,FXSHJO
      COMMON /JOBKAR/   TITLJO,TSETJO(LSET),TPROJO(LPRO)
     1                 ,TKINJO,TGEOJO(LBGE),TRUNJO
      CHARACTER TRUNJO*60
      CHARACTER*4 TKINJO,TPROJO,TSETJO,TITLJO*40
      CHARACTER*2 TGEOJO
C
      PARAMETER (LERR=20)
      COMMON /JOBERR/   ITELJO,KERRJO,NERRJO(LERR)
      COMMON /JOBCAR/   TACTJO
      CHARACTER*6 TACTJO
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON /NAMCOM/   NARUNH, NAPART, NAEVEH, NAVERT, NAKINE, NAKRUN
     &                 ,NAKEVH, NAIMPA, NAASEV, NARUNE, NAKLIN, NARUNR
     &                 ,NAKVOL, NAVOLU
      EQUIVALENCE (NAPAR,NAPART)
C
      PARAMETER (JASIYM=1,LASIMA=1)
      COMMON /KINCOM/   IPROKI,ECMSKI,IDEVKI,ISTAKI,WEITKI
     &                 ,NOTRKI,NITRKI,NIVXKI
C
      INTEGER ALRUNH
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
C ----------------------------------------------------------------------
C
C - get the RUN record from the input tape if any
C
      IF (MGETJO.GT.0) THEN
  100   CALL ABRSEL ('E','   ' ,JRET)
        IF (JRET.GT.3) THEN
          IF (JRET.EQ.9) THEN
            CALL ALTELL ('ASREAD:  Time limit reached',0,'STOP')
          ELSEIF (JRET.EQ.11) THEN
            CALL ALTELL ('ASREAD:  Error in data cards',0,'STOP')
          ELSEIF (JRET.LE.16) THEN
            CALL ALTELL ('ASREAD:  Cannot open input or output file'
     &                              ,0,'STOP')
          ELSEIF (JRET.EQ.19) THEN
            CALL ALTELL ('ASREAD:  Not enough space for unpacking',0
     &                              ,'STOP')
          ELSEIF (JRET.EQ.17) THEN
            CALL ALTELL ('ASREAD:  Read error-try again',0,'RETURN')
            GOTO 100
          ELSEIF (JRET.EQ.18) THEN
            CALL ALTELL ('ASREAD: Error in decompressing-go on',0
     &                              ,'RETURN')
          ENDIF
        ENDIF
C
C      get logical unit numbers
        CALL ABUNIT (LGETIO,KUNSEL,KUNSE2,LSAVIO,KUTSEL)
C
C      run record
        IF (JRET.EQ.2) THEN
          IF (IPRIJO(15).EQ.1) CALL AUBLIS ('C')
        ELSE
          WRITE(LOUTIO,*) ' No RUNH bank '
          IF (IPRIJO(15).EQ.1) CALL AUBLIS('E')
        ENDIF
C
      ELSE
C       no input file
         CALL BLIST (IW,'C=','RUNRRUNHKRUN')
         IF (MSAVJO.EQ.1) THEN
C          open output file
            CALL AOPEWR (LGETIO,KUTSEL,LSAVIO,IERW)
            IF (IERW.NE.0) THEN
              CALL ALTELL ('ASREAD: Cannot open FILO file',0,'STOP')
            ENDIF
         ENDIF
      ENDIF
C
C   Get PART bank from data base if not read from cards or tape
C
      JPART = IW(NAPART)
      IF (JPART.EQ.0) THEN
        JPART = MDARD(IW,LRDBIO,'PART',0)
        CALL BKFMT ('PART','2I,(I,3A,I,4F,I)')
        WRITE (LOUTIO,'(/1X,''+++ASREAD+++ PART bank read from '',
     &                       ''d.b.'')')
      ENDIF
      IF (JPART.LE.0) THEN
        CALL ALTELL ('ASREAD: NO PART bank ',0,'STOP')
      ENDIF
C
C   Get GDEC decay modes bank from data base if not there
C
      IF (IW(NAMIND('GDEC')).EQ.0) THEN
        IND=MDARD(IW,LRDBIO,'GDEC',0)
        IF (IND.LE.0) CALL ALTELL ('ASREAD: NO GDEC bank ',0,'STOP')
        CALL BKFMT('GDEC','2I,(I,6F,6I)')
      ENDIF
C
C -  build run type and initialize event generator, build 'RUNH'
C    ASRTYP needs the PART bank
C
      IF (IW(NARUNH) .EQ. 0) THEN
         CALL ASRTYP
         IEXPJO = LOFFMC+1
         JRUNH = ALRUNH (IRUNJO,IEXPJO,IPROKI)
         IF (JRUNH.EQ.0) THEN
            CALL ALTELL ('ASIRUN: not enough space to book RUNH ',1,
     &                   'FATAL')
         ENDIF
      ENDIF
C
      RETURN
      END
