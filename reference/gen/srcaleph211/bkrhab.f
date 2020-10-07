      SUBROUTINE BKRHAB(JRHAH)
C-----------------------------------------------------------------------
C! Books and fills the RHAH bank for MC or OnLine steps
CKEY BOOK RUN HEADER / INTERNAL
C Author     J. Boucrot     25-Sep-1988    Modified  02-Feb-1989
C Auxiliary to BKRHAW                           from ALEPHLIB
C NOT User-callable
C Calls AUBOS,ALGTYP                            from ALEPHLIB
C Output argument : bank index JRHAH if all is OK
C                  = 0 if no space to book the bank
C Input banks needed  :  RUNH  ; AJOB,KJOB ( for Monte-Carlo data)
C ----------------------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JRHAPN=1,JRHAPD=3,JRHAPH=4,JRHAPV=5,JRHAAV=6,JRHADV=7,
     +          JRHADD=8,JRHANI=9,JRHANO=10,JRHACV=11,JRHANU=12,
     +          LRHAHA=12)
      PARAMETER(JACUMN=1,JACUGC=2,JACUEC=3,JACUHC=4,JACUNC=5,JACUMC=6,
     +          LACUTA=6)
      PARAMETER(JAJOBM=1,JAJORM=2,JAJOGI=3,JAJOSO=4,JAJOSD=5,JAJOGC=6,
     +          JAJOJD=7,JAJOJT=8,JAJOGV=9,JAJOAV=10,JAJOFT=11,
     +          JAJOFS=12,JAJOFC=13,JAJODV=14,JAJODD=15,JAJOTV=16,
     +          JAJOCV=17,LAJOBA=17)
      PARAMETER(JAKIKT=1,JAKIKP=2,LAKINA=9)
      PARAMETER(JAPRPF=1,JAPRRG=2,LAPROA=4)
      PARAMETER(JARURC=1,LARUNA=10)
      PARAMETER(JASERG=1,LASEVA=3)
      PARAMETER(JATIRT=1,LATITA=1)
      PARAMETER(JRUNEN=1,JRUNRN=2,JRUNRT=3,JRUNSD=4,JRUNST=5,LRUNHA=5)
CKEY KINE KINGAL HAC
      PARAMETER(JKEVRN=1,JKEVNT=2,JKEVNV=3,JKEVPI=4,JKEVWT=5,JKEVSR=6,
     +          JKEVTR=7,LKEVHA=7)
      PARAMETER(JKHIHC=1,LKHISA=1)
      PARAMETER(JKJOJD=1,JKJOJT=2,JKJOAV=3,JKJODV=4,JKJODC=5,LKJOBA=5)
      PARAMETER(JKLIGN=1,LKLINA=1)
      PARAMETER(JKPOKI=1,JKPOHX=2,JKPOHY=3,JKPOHZ=4,LKPOLA=4)
      PARAMETER(JKRUGI=1,JKRUNO=2,JKRURT=3,JKRUFS=15,JKRUSS=16,
     +          LKRUNA=16)
      PARAMETER(JKVOVN=1,JKVOVM=2,LKVOLA=2)
      EXTERNAL NAMIND,INTCHA
      COMMON / NAMRHA / NARHAH,NARHOH,NARUNH,NAAJOB,
     +                  NAKJOB,NAKRUN
      PARAMETER (IKIN = 1 ,IRAW = 2 ,IPOT = 3 ,IDST = 4 ,IMINI = 5 )
      PARAMETER ( ISEL = 10  ,  IMCRL = 1001 )
      CHARACTER*8 PRNAM
      INTEGER ALGTYP
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
C-----------------------------------------------------------------------
        JRHAH=IW(NARHAH)
        IF (JRHAH.NE.0) GO TO 999
        NROWS=1
C Check which kind of input is currently being read
C Monte-Carlo data . They may come either from a KINGAL file , or not .
C
        JKJOB=IW(NAKJOB)
        NKING=0
        IF (JKJOB.NE.0) THEN
           JKRUN=IW(NAKRUN)
           NVKIN=ITABL(JKRUN,1,JKRUGI)
           IF (NVKIN.NE.0) NKING=1
           NROWS=2
        ENDIF
 20     LENRH=LMHLEN+NROWS*LRHAHA
C
C Book bank 'RHAH' with 1 row ( if real data , or no KINGAL step)
C or with 2 rows ( if Monte-Carlo with KINGAL step)
C
        JRHAH=0
        CALL AUBOS('RHAH',0,LENRH,JRHAH,IGARB)
        IF (IGARB.EQ.2) GO TO 999
        IF (IGARB.EQ.1) JKJOB=IW(NAKJOB)
        IW(JRHAH+LMHCOL)=LRHAHA
        IW(JRHAH+LMHROW)=NROWS
C
C Kingal step
C
        IF(JKJOB.NE.0) THEN
           KRHAH=KROW(JRHAH,1)
           PRNAM='KINGAL  '
           IW(KRHAH+JRHAPN)=INTCHA(PRNAM(1:4))
           IW(KRHAH+JRHAPN+1)=INTCHA(PRNAM(5:8))
           IW(KRHAH+JRHANO)=NKING
           IW(KRHAH+JRHAPV)=NVKIN
           KKJOB=KROW(JKJOB,1)
           CALL UCOPY(IW(KKJOB+JKJOJD),IW(KRHAH+JRHAPD),2)
           CALL UCOPY(IW(KKJOB+JKJOAV),IW(KRHAH+JRHAAV),3)
        ENDIF
C
C Galeph step
C
        JAJOB=IW(NAAJOB)
        IF (JAJOB.NE.0) THEN
           KRHAH=KROW(JRHAH,NROWS)
           PRNAM='GALEPH  '
           IW(KRHAH+JRHAPN)=INTCHA(PRNAM(1:4))
           IW(KRHAH+JRHAPN+1)=INTCHA(PRNAM(5:8))
           KAJOB=KROW(JAJOB,1)
           CALL UCOPY (IW(KAJOB+JAJOJD),IW(KRHAH+JRHAPD),4)
           IW(KRHAH+JRHADV)=IW(KAJOB+JAJODV)
           IW(KRHAH+JRHADD)=IW(KAJOB+JAJODD)
           INDAT=0
           IF (NROWS.EQ.2.AND.NKING.NE.0) INDAT=IKIN
           IW(KRHAH+JRHANI)=INDAT
           IOUTD=IRAW
           IGTS=ALGTYP(IGEAN,ISIMD,IFAST,ITPCS)
           IF (IGTS.GT.0.AND.ISIMD.GT.0) IOUTD=IDST
           IW(KRHAH+JRHANO)=IOUTD
C Correction version # ( may not be available in old Galeph files)
           ICVER=0
           LRJO=LCOLS(JAJOB)
           IF (LRJO.LT.JAJOCV) GO TO 55
           ICVER=IW(KAJOB+JAJOCV)
 55        IW(KRHAH+JRHACV)=ICVER
           GO TO 999
        ENDIF
C
C Real data in input
C
        IF (JAJOB.EQ.0.AND.JKJOB.EQ.0) THEN
           JRUNH=IW(NARUNH)
C Use 'RUNR' if 'RUNH' absent :
           IF (JRUNH.EQ.0) THEN
              IEXPN=IW(JRUNH+JRUNEN)
           ELSE
              JRUNR=IW(NAMIND('RUNR'))
              IF (JRUNR.EQ.0) GO TO 999
              IEXPN=IW(JRUNR+1)
           ENDIF
           IF (IEXPN.EQ.IMCRL) GO TO 999
           KRHAH=KROW(JRHAH,1)
           PRNAM='ON LINE '
           IW(KRHAH+JRHAPN)=INTCHA(PRNAM(1:4))
           IW(KRHAH+JRHAPN+1)=INTCHA(PRNAM(5:8))
           IF (JRUNH.GT.0) THEN
              IW(KRHAH+JRHAPD)=IW(JRUNH+JRUNSD)
              IW(KRHAH+JRHAPH)=IW(JRUNH+JRUNST)
           ENDIF
           IW(KRHAH+JRHANO)=IRAW
        ENDIF
C
 999   RETURN
       END
