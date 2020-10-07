      INTEGER FUNCTION  ALGTYP(IGEAN,ISIMD,IFAST,ITPCS)
C ----------------------------------------------------------------------
C! Finds the Galeph tracking level for a file of simulated events
CKEY  ALEF GALEPH TYPE / USER
C Author     J. Boucrot     26-AUG-1988
C Input banks : AJOB ,  TSIM
C   Returns  ALGTYP = 1  if the file WAS     processed through GALEPH
C                   = 0        #     WAS NOT            #
C Output arguments :
C   IGEAN = 1   Geant tracking was used ;  = 0  other tracking
C   ISIMD = 1   SIMDST smearing and DST writing  ; = 0   no SIMDST
C   IFAST = 1   FAST tracking was used  ;  = 0   other tracking
C   ITPCS = 1   TPCSIM digitisation was done  ;   = 0   not done
C ----------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JACUMN=1,JACUGC=2,JACUEC=3,JACUHC=4,JACUNC=5,JACUMC=6,
     +          LACUTA=6)
      PARAMETER(JAFIAR=1,JAFIAZ=2,JAFIMF=3,JAFIBE=4,LAFIDA=4)
      PARAMETER(JAJOBM=1,JAJORM=2,JAJOGI=3,JAJOSO=4,JAJOSD=5,JAJOGC=6,
     +          JAJOJD=7,JAJOJT=8,JAJOGV=9,JAJOAV=10,JAJOFT=11,
     +          JAJOFS=12,JAJOFC=13,JAJODV=14,JAJODD=15,JAJOTV=16,
     +          JAJOCV=17,LAJOBA=17)
      PARAMETER(JAKIKT=1,JAKIKP=2,LAKINA=9)
      PARAMETER(JAPRPF=1,JAPRRG=2,LAPROA=4)
      PARAMETER(JARURC=1,LARUNA=10)
      PARAMETER(JASERG=1,LASEVA=3)
      PARAMETER(JATIRT=1,LATITA=1)
       PARAMETER ( IFUL = 1 , ISIM = 101 , IFAS = 11 )
       PARAMETER ( IDST = 100 )
C!    set of intrinsic functions to handle BOS banks
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
        ALGTYP=0
        IGEAN=0
        ISIMD=0
        IFAST=0
        ITPCS=0
C
        JAJOB=NLINK('AJOB',0)
        IF (JAJOB.EQ.0) GO TO 999
        ITEST=ITABL(JAJOB,1,JAJORM)
        IF (MOD(ITEST,IDST).LT.IFAS) IGEAN=IFUL
        IF (ITEST.EQ.ISIM) ISIMD=1
        IF (MOD(ITEST,IDST).EQ.IFAS) IFAST=1
        IF (NLINK('TSIM',0).GT.0) ITPCS=1
        ALGTYP=1
C
 999    RETURN
        END
