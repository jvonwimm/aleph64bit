      SUBROUTINE AGGEAN (DET,ISTUP)
C------------------------------------------------------
C!  Implement detector  geometry
C - F.Ranjard - 901212
C - Input :   - DET     / CHA*2  : detector initials
C - Output:   - ISTUP   / I      : setup code
C.  -Called by AGEOME                  from this .HLB
C.  -Calls GSTMED,GSVOLU,GSPOS,GSROTM  from  GEANT3
C.
C. -Stores extra Tracking Media needed
C. -Builds geometry levels below  'CDET'levels for the VDET parts
C. -Initialises some search optimisation
C.
C------------------------------------------------
      CHARACTER*(*) DET
      CHARACTER*4 CHAINT,TYPE
      CHARACTER*16 NAME,NAMX
      CHARACTER*20 NAMG
      INTEGER INAME(5)
      PARAMETER (LNAM=200, LCHNAM=16, LALL=LNAM*LCHNAM, NWRDS=4)
      CHARACTER*3200 ALLNAM
      INTEGER GTSTUP, ALGTDB
      PARAMETER (LCOMP=10)
      REAL AA(LCOMP),ZZ(LCOMP),WW(LCOMP),EPS(LCOMP)
      INTEGER LIS(LCOMP)
      CHARACTER*44 BKLIST
      PARAMETER (KGWBK=69000,KGWRK=5200)
      COMMON /GCBANK/   NGZEBR,GVERSN,GZVERS,IGXSTO,IGXDIV,IGXCON,
     &                  GFENDQ(16),LGMAIN,LGR1,GWS(KGWBK)
      DIMENSION IGB(1),GB(1),LGB(8000),IGWS(1)
      EQUIVALENCE (GB(1),IGB(1),LGB(9)),(LGB(1),LGMAIN),(IGWS(1),GWS(1))
C
      COMMON/GCLINK/JGDIGI,JGDRAW,JGHEAD,JGHITS,JGKINE,JGMATE,JGPART
     +        ,JGROTM,JGRUN,JGSET,JGSTAK,JGGSTA,JGTMED,JGTRAC,JGVERT
     +        ,JGVOLU,JGXYZ,JGPAR,JGPAR2,JGSKLT
C
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
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      INTEGER NBITW, NBYTW, LCHAR
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
      PARAMETER (NBITW = 32 , NBYTW = NBITW/8 , LCHAR = 4)
C
      PARAMETER  (D360=TWOPI*RADEG   ,D180=PI*RADEG   ,D90=0.5*D180)
      PARAMETER  (D45=0.5*D90   ,D15=D45/3.   ,D210=7.*D90/3.)
      PARAMETER  (D225=D180+D45   ,D270=D180+D90  ,D7P5=0.5*D15)
      PARAMETER(LSENV=30)
      PARAMETER (LIMVOL=17)
C
      COMMON/AGCONS/ IAGROT,IAGMAT,IAGMED,IAGFHB,IAGSLV,IAGSEN(LSENV,2)
     2      , NAGIMP,LAGIMP(3,LIMVOL)
C
       COMMON /WRKSPC/ WSPACE(88320)
      PARAMETER (LPTAB=50)
      DIMENSION PTAB(LPTAB),JTAB(LPTAB)
      EQUIVALENCE (PTAB(1),JTAB(1),WSPACE(1))
C
      PARAMETER ( NLIMR = 9 , NLIMZ = 6)
      COMMON / AGECOM / AGLIMR(NLIMR),AGLIMZ(NLIMZ),IAGFLD,IAGFLI,IAGFLO
      COMMON/ALFGEO/ALRMAX,ALZMAX,ALFIEL,ALECMS
C
      PARAMETER(JMATMN=1,JMATAW=5,JMATAN=6,JMATDE=7,JMATRL=8,JMATAL=9,
     +          LMATEA=9)
      PARAMETER(JMIXMN=1,JMIXMT=5,JMIXDE=6,LMIXTA=6)
      PARAMETER(JMELVM=1,JMELEL=2,JMELNU=6,LMELEA=6)
      PARAMETER(JMMAVM=1,JMMAMA=2,JMMATH=6,LMMATA=6)
      PARAMETER(JGPAVT=1,JGPAPA=2,JGPAPV=4,LGPARA=4)
      PARAMETER(JPCOVV=1,JPCOZB=2,JPCORN=3,JPCORX=4,LPCONA=4)
      PARAMETER(JPOSDA=1,JPOSMO=2,JPOSFL=3,JPOSVR=4,JPOSCN=5,JPOSPO=6,
     +          LPOSIA=8)
      PARAMETER(JROTA1=1,JROTP1=2,JROTA2=3,JROTP2=4,JROTA3=5,JROTP3=6,
     +          LROTAA=6)
      PARAMETER(JTMETN=1,JTMEMA=5,JTMEFD=9,JTMEMS=10,JTMEDE=11,
     +          JTMEEP=12,JTMEST=13,JTMEVO=14,JTMEGP=15,JTMEFL=16,
     +          LTMEDA=16)
      PARAMETER(JVOLVO=1,JVOLSH=2,JVOLVT=3,JVOLNP=4,JVOLPA=5,LVOLUA=9)
      PARAMETER(JATTVO=1,JATTAN=2,JATTAV=3,LATTRA=3)
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
C
C ----------------------------------------------------------------------
C
C - get the setup code
C
      ISTUP = 0
      ISTUP = GTSTUP (DET,1)
      IF (ISTUP.LE.0) THEN
         ALLNAM = ' '
         ALLNAM = 'AGGEAN: '//DET//' does not exist '
         CALL ALTELL (ALLNAM(1:LENOCC(ALLNAM)),0,'STOP')
      ENDIF

      BKLIST = ' MAT MIX MEL MMA TME GPA VOL PCO POS ROT ATT'
      DO I=1,LEN(BKLIST),4
         BKLIST(I:I) = DET(1:1)
      ENDDO
      IRET = ALGTDB (LRDBIO,BKLIST,ISTUP)
      JMAT = IW(NAMIND(DET(1:1)//'MAT'))
      JMIX = IW(NAMIND(DET(1:1)//'MIX'))
      JMEL = IW(NAMIND(DET(1:1)//'MEL'))
      JMMA = IW(NAMIND(DET(1:1)//'MMA'))
      JTME = IW(NAMIND(DET(1:1)//'TME'))
      JGPA = IW(NAMIND(DET(1:1)//'GPA'))
      JVOL = IW(NAMIND(DET(1:1)//'VOL'))
      JPCO = IW(NAMIND(DET(1:1)//'PCO'))
      JPOS = IW(NAMIND(DET(1:1)//'POS'))
      JROT = IW(NAMIND(DET(1:1)//'ROT'))
      JATT = IW(NAMIND(DET(1:1)//'ATT'))
C
      IF (DET.EQ.'GE') IGEST = ISTUP
      JGROT = NLINK('GROT',IGEST)
      JGTME = NLINK('GTME',IGEST)
      DO 10 L=1,LDET
        IF (DET .NE.TGEOJO(L)) GOTO 10
        JDET = L
        GOTO 20
  10  CONTINUE
      JDET = LDET+1
 20   CONTINUE
C
C - store additional materials needed by DET ========================
C
      INDMAT = IAGMAT
      IF (JMAT.NE.0) THEN
C
        DO 100 I=1,LROWS(JMAT)
          IAGMAT=IAGMAT+1
          KMAT = KROW(JMAT,I)
          CALL ALSTIN (IW(KMAT+JMATMN),NWRDS,NAME)
          CALL GSMATE (IAGMAT,NAME,RW(KMAT+JMATAW),RW(KMAT+JMATAN),
     &       RW(KMAT+JMATDE),RW(KMAT+JMATRL),RW(KMAT+JMATAL),0,0)
  100   CONTINUE
      ENDIF
C
C - get all existing materials, store them into ALLNAM
C
      NMAT = IAGMAT
      IF ( JMIX.GT.0) NMAT = IAGMAT+LROWS( JMIX)
      IF (NMAT*LCHNAM.GT.LALL) GOTO 998
      ALLNAM = ' '
      DO 200 I=1,IAGMAT
        IF (GVERSN.LT.3.15) THEN
           CALL GFMATE (I,INAME,AD,ZD,DD,RD,AL,DUM,ND)
           CALL ALSTHO (INAME,5,NAMG)
        ELSE
           CALL GFMATE (I,NAMG,AD,ZD,DD,RD,AL,DUM,ND)
        ENDIF
        L = LNBLNK (NAMG)
        IND = INDEX (NAMG,'$')
        IF (IND.NE.0) L=IND-1
        L = MIN (L,LCHNAM)
        IC = (I-1)*LCHNAM
        ALLNAM(IC+1:IC+L)  = NAMG(1:L)
  200 CONTINUE
      LAS = IAGMAT*LCHNAM
C
C - define mixtures needed by DET =====================================
C
      IF ( JMIX.NE.0) THEN
C
C
C       loop over MIXT rows
        DO 201 I=1,LROWS( JMIX)
          NCOMP = 0
          KMIX = KROW( JMIX,I)
          CALL ALSTIN (IW(KMIX+JMIXMN),NWRDS,NAMX)
          TYPE = CHAINT(IW(KMIX+JMIXMT))
C
          IF (TYPE .EQ.'ELEM') THEN
C            it is a mixture of ELEMents
            IF (JMEL.EQ.0) GOTO 997
            DENS = RW(KMIX+JMIXDE)
            IS = -1
C             loop over MELE rows : take those which refer to
C                                   the MIXT row # I
            DO 202 J=1,LROWS(JMEL)
              IF (ITABL(JMEL,J,JMELVM).EQ.I) THEN
                NCOMP = NCOMP+1
                KMEL = KROW(JMEL,J)
                CALL ALSTIN (IW(KMEL+JMELEL),NWRDS,NAME)
                IND = INDEX(ALLNAM,NAME)
                IF (IND.EQ.0) GOTO 996
                IMAT = IND/LCHNAM + 1
                IF (GVERSN.LT.3.15) THEN
                   CALL GFMATE (IMAT,INAME,AA(NCOMP),ZZ(NCOMP),
     &                          DD,RD,AL,DUM,ND)
                ELSE
                   CALL GFMATE (IMAT,NAMG,AA(NCOMP),ZZ(NCOMP),
     &                          DD,RD,AL,DUM,ND)
                ENDIF
                WW(NCOMP) = ITABL(JMEL,J,JMELNU)
              ENDIF
  202       CONTINUE
C
          ELSEIF (TYPE.EQ.'MATE' .OR. TYPE.EQ.'COMP') THEN
C            it is a mixture of materials or a compound
            IF (JMMA.EQ.0) GOTO 997
            IS  = 1
C             loop over MMAT rows: take those which refer to
C                                  the MIXT row # I
            DO 203 J=1,LROWS(JMMA)
              IF (ITABL(JMMA,J,JMMAVM).EQ.I) THEN
                NCOMP = NCOMP+1
                KMMA = KROW(JMMA,J)
                CALL ALSTIN (IW(KMMA+JMMAMA),NWRDS,NAME)
                IND = INDEX (ALLNAM,NAME)
                IF (IND.EQ.0) GOTO 996
                IMAT = IND/LCHNAM + 1
                LIS(NCOMP) = IMAT
                EPS(NCOMP)  = RTABL(JMMA,J,JMMATH)
              ENDIF
  203       CONTINUE
            CALL AGMIX (NCOMP,LIS,EPS,AA,ZZ,WW,DENS)
            IF (TYPE.EQ.'MATE') THEN
C                the density given by AGMIX must be scaled
              DENS = DENS * RW(KMIX+JMIXDE)
            ELSEIF(TYPE.EQ.'COMP') THEN
C                the density and weights are read from the bank
              DENS = RW(KMIX+JMIXDE)
              DO 204 N=1,NCOMP
                WW(N) = EPS(N)
  204         CONTINUE
            ENDIF
C
          ELSE
C            unknown type of mixture
            GOTO 995
          ENDIF
C
          IF (NCOMP.EQ.0) GOTO 994
C          store the new material in GEANT tables and in ALLNAM
          IAGMAT = IAGMAT+1
          CALL GSMIXT (IAGMAT,NAMX,AA,ZZ,DENS,IS*NCOMP,WW)
          ALLNAM(LAS+1:LAS+LCHNAM) = NAMX
          LAS = LAS + LCHNAM
  201   CONTINUE
      ENDIF
C
C - define tracking media used in DET      =====================
C
      INDMED = IAGMED
      IF (JTME.NE.0) THEN
        DO 301 I=1,LROWS(JTME)
          KTME = KROW(JTME,I)
          CALL ALSTIN (IW(KTME+JTMEMA),NWRDS,NAME)
          IND = INDEX (ALLNAM,NAME)
          IF (IND.EQ.0) GOTO 996
          IMAT = IND/LCHNAM + 1
          CALL ALSTIN (IW(KTME+JTMETN),NWRDS,NAMX)
          IAGMED = IAGMED+1
          ISVOL = 0
          IF (IW(KTME+JTMEVO).EQ.1) THEN
             IF (JDET.GT.LDET) GOTO 990
             ISVOL = IDETJO(JDET)
          ENDIF
          IFLD = IW(KTME+JTMEFL)
          IF (ALFIEL.EQ.0.) IFLD = 0
          CALL GSTMED (IAGMED,NAMX,IMAT,ISVOL,IFLD,ALFIEL,
     &        RW(KTME+JTMEFD),RW(KTME+JTMEMS),RW(KTME+JTMEDE),
     &        RW(KTME+JTMEEP),RW(KTME+JTMEST),0.,0.)
          IF (IW(KTME+JTMEGP).EQ.1) THEN
            IF (JGPA.EQ.0) GOTO 993
            DO 302 J=1,LROWS(JGPA)
              IF (ITABL(JGPA,J,JGPAVT).EQ. I) THEN
                KGPA = KROW(JGPA,J)
                CALL ALSTIN (IW(KGPA+JGPAPA),2,NAME)
                L = LENOCC(NAME)
                CALL GSTPAR (IAGMED,NAME(1:L),RW(KGPA+JGPAPV))
              ENDIF
  302       CONTINUE
          ENDIF
  301   CONTINUE
      ENDIF
C
C - define volumes used in DET  =====================================
C
      IF (JVOL.NE.0) THEN
        DO 401 I=1,LROWS(JVOL)
          KVOL = KROW(JVOL,I)
          NAME(1:4) = CHAINT(IW(KVOL+JVOLVO))
          NAMX(1:4) = CHAINT(IW(KVOL+JVOLSH))
          NPAR      = IW(KVOL+JVOLNP)
          DO 403 N=1,NPAR
            PTAB(N) = RW(KVOL+JVOLPA-1+N)
  403     CONTINUE
          IF (NAMX(1:4).EQ.'PCON') THEN
            IF (JPCO.EQ.0) GOTO 992
            DO 402 J=1,LROWS(JPCO)
              IF (ITABL(JPCO,J,JPCOVV).EQ.I) THEN
                KGPC = KROW(JPCO,J)
                PTAB(NPAR+1) = RTABL(JPCO,J,JPCOZB)
                PTAB(NPAR+2) = RTABL(JPCO,J,JPCORN)
                PTAB(NPAR+3) = RTABL(JPCO,J,JPCORX)
                NPAR = NPAR+3
              ENDIF
  402       CONTINUE
          ENDIF
          ITME = IW(KVOL+JVOLVT)
          IF (ITME.LT.0) THEN
C             the tracking medium number is the absolute track.med. #
            ITME = -ITME
          ELSEIF (ITME.GT.0) THEN
C             the tracking medium # is relative to the 1st one entered
C             for this detector
            ITME = INDMED + ITME
          ENDIF
          CALL GSVOLU (NAME(1:4),NAMX(1:4),ITME,PTAB,NPAR,IVOL)
          IF (I.EQ.1) INDVOL = IVOL-1
  401   CONTINUE
      ENDIF
C
C - store rotations matrices used in DET  ============================
C
      IF (JROT.NE.0) THEN
        INDROT = IAGROT
        DO 501 I=1,LROWS(JROT)
          KROT = KROW(JROT,I)
          IAGROT = IAGROT+1
          CALL GSROTM (IAGROT,RW(KROT+JROTA1),RW(KROT+JROTP1),
     &                          RW(KROT+JROTA2),RW(KROT+JROTP2),
     &                          RW(KROT+JROTA3),RW(KROT+JROTP3))
  501   CONTINUE
      ENDIF
C
C - position daughter volumes into mother volumes ======================
C
      IF (JPOS.NE.0) THEN
        DO 601 I=1,LROWS(JPOS)
          KPOS = KROW(JPOS,I)
          NAME = CHAINT (IW(KPOS+JPOSDA))
          NAMX = CHAINT (IW(KPOS+JPOSMO))
          TYPE = CHAINT (IW(KPOS+JPOSFL))
          IROT = IW(KPOS+JPOSVR)
          IF (IROT.LT.0) THEN
C             the rotation number is the absolute rotation #
            IROT = -IROT
          ELSEIF (IROT.GT.0) THEN
C             the rotation number is relative to the 1st one entered
C             for this detector
            IROT = INDROT + IROT
          ENDIF
          CALL GSPOS (NAME(1:4),IW(KPOS+JPOSCN),NAMX(1:4),RW(KPOS+
     &        JPOSPO),RW(KPOS+JPOSPO+1),RW(KPOS+JPOSPO+2),IROT,TYPE)
  601   CONTINUE
      ENDIF
C
C - set volumes drawing attributes =====================================
C
      IF (JATT.NE.0) THEN
        DO 701 I=1,LROWS(JATT)
           KATT = KROW(JATT,I)
           NAME = CHAINT (IW(KATT+JATTVO))
           NAMX = CHAINT (IW(KATT+JATTAN))
           CALL GSATT (NAME(1:4),NAMX(1:4),IW(KATT+JATTAV))
 701    CONTINUE
      ENDIF
C
C - end ===========================================================
C
      GOTO 999
C
C - errors ============================================================
C
C   this detector is not a sensitive one: error in data base
  990 CONTINUE
      CALL PRTABL (DET(1:1)//'TME',ISTUP)
      ALLNAM = ' '
      ALLNAM = 'AGGEAN: '//DET//' cannot be sensitive '
      CALL ALTELL (ALLNAM(1:LENOCC(ALLNAM)),0,'STOP')
C   GPCO is missing
  992 CONTINUE
      WRITE (LOUTIO,*) ' AGGEAN : PCON is missing but required IROW= ',I
      CALL PRTABL (DET(1:1)//'PCO',ISTUP)
      GOTO 991
C   GPAR is missing
  993 CONTINUE
      WRITE (LOUTIO,*) ' AGGEAN : GPAR is missing but required IROW= ',I
      CALL PRTABL (DET(1:1)//'TME',ISTUP)
      GOTO 991
C    no mixture : NCOMP=0
  994 CONTINUE
      WRITE (LOUTIO,*) ' AGGEAN : no mixture NCOMP=0 , IROW= ',I
      CALL PRTABL (DET(1:1)//'MIX',ISTUP)
      CALL PRTABL (DET(1:1)//'MEL',ISTUP)
      CALL PRTABL (DET(1:1)//'MMA',ISTUP)
      GOTO 991
C    type of mixture is unknown
  995 CONTINUE
      WRITE (LOUTIO,*) ' AGGEAN : unknown type of mixture , IROW= ',I
      CALL PRTABL (DET(1:1)//'MIX',ISTUP)
      GOTO 991
C    MATE or ELEM or TMED name is unknown
  996 CONTINUE
      WRITE (LOUTIO,*) ' AGGEAN : unknown name , I= ',I,' J= ',J
     &               , ' NAME= ',NAME
      WRITE (LOUTIO,*) ' ALLNAM= ',ALLNAM(1:LAS)
      CALL PRTABL (DET(1:1)//'MEL',ISTUP)
      CALL PRTABL (DET(1:1)//'MMA',ISTUP)
      CALL PRTABL (DET(1:1)//'TME',ISTUP)
      GOTO 991
C    MELE or MMAT is missing but MIXT is there
  997 CONTINUE
      WRITE (LOUTIO,*) ' AGGEAN : MMAT or MELE is missing , IROW= ',I
      CALL PRTABL (DET(1:1)//'MIX',ISTUP)
      GOTO 991
C    too many materials: increase ALLNAM length
  998 CONTINUE
      WRITE (LOUTIO,*)
     &  ' AGGEAN : too many materials, increase ALLNAM length , NMAT= ',
     &  NMAT
      GOTO 991
C
  991 CONTINUE
      ALLNAM = ' '
      ALLNAM = 'AGGEAN: cannot build '//DET//' geometry '
      CALL ALTELL (ALLNAM(1:LENOCC(ALLNAM)),0,'STOP')
C - end
C
  999 CONTINUE
      RETURN
      END
