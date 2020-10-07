      SUBROUTINE PTGMAJ(LIST,IER)
C
C-----------------------------------------------------------------------
C! Unpack banks TGMA and TMTL from the POT
C
C     Author:   R. Johnson   10-1-89
C
C     Input :    LIST      BOS event list
C                          if LIST(2:2).eq.'-' drop POT banks
C     Output:    IER       = 0  successful
C                          = 1  input bank does not exist or is empty
C                          = 2  not enough space
C                          =-1  OK but garbage collection
C
C    Called by TPTOJ
C
C-----------------------------------------------------------------------
      SAVE
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JFRTIV=1,JFRTNV=2,JFRTII=3,JFRTNI=4,JFRTNE=5,JFRTIT=6,
     +          JFRTNT=7,JFRTNR=8,LFRTLA=8)
      PARAMETER(JFRFIR=1,JFRFTL=2,JFRFP0=3,JFRFD0=4,JFRFZ0=5,JFRFAL=6,
     +          JFRFEM=7,JFRFC2=28,JFRFDF=29,JFRFNO=30,LFRFTA=30)
      PARAMETER(JTGMNM=1,JTGMOM=2,JTGMNC=3,JTGMNA=4,LTGMAA=4)
      PARAMETER(JTMTMT=1,JTMTNH=2,JTMTCS=3,LTMTLA=3)
      PARAMETER(JPTMNM=1,JPTMNA=2,LPTMAA=2)
      PARAMETER(JPTMMT=1,JPTMNH=2,LPTMLA=2)
      PARAMETER(JFKIPX=1,JFKIPY=2,JFKIPZ=3,JFKIMA=4,JFKIPA=5,JFKIOV=6,
     +          JFKIEV=7,JFKIHC=8,LFKINA=8)
      PARAMETER(JFVEVX=1,JFVEVY=2,JFVEVZ=3,JFVETO=4,JFVEIP=5,JFVEIS=6,
     +          JFVENS=7,JFVEVN=8,JFVEVM=9,LFVERA=9)
      PARAMETER(JFPOIP=1,JFPOIS=2,LFPOIA=2)
      PARAMETER(JFPOKI=1,JFPOHX=2,JFPOHY=3,JFPOHZ=4,LFPOLA=4)
CD FLCOJJ
      PARAMETER(JFLCIL=1,LFLCOA=1)
CD FLTRJJ
      PARAMETER(JFLTIL=1,LFLTRA=1)
CD FSCOJJ
      PARAMETER(JFSCEN=1,JFSCIH=2,JFSCE1=3,JFSCE2=4,JFSCE3=5,JFSCH1=6,
     +          JFSCH2=7,JFSCTH=8,JFSCPH=9,JFSCIP=10,LFSCOA=10)
CD FSTRJJ
      PARAMETER(JFSTPX=1,JFSTPY=2,JFSTPZ=3,JFSTIQ=4,JFSTIH=5,JFSTCH=6,
     +          JFSTNH=7,JFSTD0=8,JFSTPH=9,JFSTZ0=10,JFSTAV=11,
     +          JFSTS2=12,JFSTNW=13,JFSTM2=14,JFSTNM=15,JFSTNC=16,
     +          JFSTMC=17,JFSTIC=18,LFSTRA=18)
CD FTCMJJ
      PARAMETER(JFTCIP=1,LFTCMA=1)
CD FTOCJJ
      PARAMETER(JFTOIP=1,LFTOCA=1)
CD FTTMJJ
      PARAMETER(JFTTIP=1,LFTTMA=1)
      PARAMETER(JPARGN=1,JPARNA=2,JPARCO=5,JPARMA=6,JPARCH=7,JPARLT=8,
     +          JPARMW=9,JPARAN=10,LPARTA=10)
C
      CHARACTER LIST*(*),PLIST*8,JLIST*8
      DIMENSION A(5,5),HELIX(5),P(3),X(3),XC(2)
      LOGICAL FIRST
      DATA FIRST/.TRUE./, FIELD/15.0/
C
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
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTGMA=NAMIND('TGMA')
        NTMTL=NAMIND('TMTL')
        NPTMA=NAMIND('PTMA')
        NPTML=NAMIND('PTML')
        NFRTL=NAMIND('FRTL')
        NFRFT=NAMIND('FRFT')
        NFKIN=NAMIND('FKIN')
        NFVER=NAMIND('FVER')
        NPART=NAMIND('PART')
      ENDIF
C
      IER=1
      KPTMA=IW(NPTMA)
      IF (KPTMA.EQ.0) GO TO 888
      KPTML=IW(NPTML)
      IF (KPTML.EQ.0) GO TO 888
C
      IER=0
      IF (IW(NTGMA).NE.0) CALL BDROP(IW,'TGMA')
      IF (IW(NTMTL).NE.0) CALL BDROP(IW,'TMTL')
C
      LEN=LROWS(KPTMA)*LTGMAA+LMHLEN
      IW(1)=1
      CALL AUBOS('TGMA',0,LEN,KTGMA,IER)
      IF (IER.EQ.2) GO TO 999
      IW(KTGMA+LMHCOL)=LTGMAA
      IW(KTGMA+LMHROW)=LROWS(KPTMA)
C
      KPTML=IW(NPTML)
      LEN=LROWS(KPTML)*LTMTLA+LMHLEN
      IW(1)=1
      CALL AUBOS('TMTL',0,LEN,KTMTL,IER)
      IF (IER.EQ.2) GO TO 999
      IW(KTMTL+LMHCOL)=LTMTLA
      IW(KTMTL+LMHROW)=LROWS(KPTML)
C
C++   Copy information from the POT banks
C
      KPTML=IW(NPTML)
      KPTMA=IW(NPTMA)
      KTGMA=IW(NTGMA)
      IOFF=0
      DO 500 ITK=1,LROWS(KTGMA)
        IW(KROW(KTGMA,ITK)+JTGMNM)=ITABL(KPTMA,ITK,JPTMNM)
        IW(KROW(KTGMA,ITK)+JTGMOM)=IOFF
        IW(KROW(KTGMA,ITK)+JTGMNC)=-1
        IW(KROW(KTGMA,ITK)+JTGMNA)=ITABL(KPTMA,ITK,JPTMNA)
        NA=0
        DO 300 II=1,ITABL(KPTMA,ITK,JPTMNM)
          IPT=IOFF+II
          IW(KROW(KTMTL,IPT)+JTMTMT)=ITABL(KPTML,IPT,JPTMMT)
          IW(KROW(KTMTL,IPT)+JTMTNH)=ITABL(KPTML,IPT,JPTMNH)
          RW(KROW(KTMTL,IPT)+JTMTCS)=9999.
  300   CONTINUE
        IOFF=IOFF+ITABL(KPTMA,ITK,JPTMNM)
  500 CONTINUE
C
C++   Calculate the remaining information
C
      KFRTL=IW(NFRTL)
      IF (KFRTL.NE.0) THEN
        DO 600 ITK=1,LROWS(KTGMA)
          IW(KROW(KTGMA,ITK)+JTGMNC)=ITABL(KFRTL,ITK,JFRTNT)
  600   CONTINUE
      ENDIF
C
      KFRFT=IW(NFRFT)
      KFKIN=IW(NFKIN)
      KFVER=IW(NFVER)
      KPART=IW(NPART)
      IF (KFRTL.NE.0 .AND. KFRFT.NE.0 .AND. KFKIN.NE.0
     &                 .AND. KPART.NE.0 .AND. KFVER.NE.0) THEN
        DO 800 ITK=1,LROWS(KTGMA)
          IDX=-1
          DO 180 I=1,6
            DO 178 J=1,I
              IDX=IDX+1
              IF (I.LE.5) THEN
                A(I,J)= RTABL(KFRFT,ITK,JFRFEM+IDX)
              ENDIF
  178       CONTINUE
  180     CONTINUE
C
          DO 700 II=1,ITABL(KTGMA,ITK,JTGMNM)
            IMT= ITABL(KTGMA,ITK,JTGMOM) + II
            ITMC=ITABL(KTMTL,IMT,JTMTMT)
            ITYP=ITABL(KFKIN,ITMC,JFKIPA)
            Q=RTABL(KPART,ITYP,JPARCH)
            IF (Q.EQ.0.) THEN
              CHISQ=9999.
            ELSE
              P(1)=RTABL(KFKIN,ITMC,JFKIPX)
              P(2)=RTABL(KFKIN,ITMC,JFKIPY)
              P(3)=RTABL(KFKIN,ITMC,JFKIPZ)
              IF (P(1).NE.0. .OR. P(2).NE.0.) THEN
                IVT=ITABL(KFKIN,ITMC,JFKIOV)
                X(1)=RTABL(KFVER,IVT,JFVEVX)
                X(2)=RTABL(KFVER,IVT,JFVEVY)
                X(3)=RTABL(KFVER,IVT,JFVEVZ)
                CALL TNRHPA(P,X,Q,FIELD,HELIX,XC,S)
                CHISQ=0.
                DO 185 K=1,5
                  IF (A(K,K).EQ.0.) THEN
                    CHISQ=9999.
                    GO TO 186
                  ENDIF
                  DIF= HELIX(K)-RTABL(KFRFT,ITK,JFRFIR-1+K)
                  IF (ABS(DIF).LT.1.0E10
     &                         .AND. A(K,K).GT.1.0E-10) THEN
                    CHISQ=CHISQ + (DIF)**2/A(K,K)
                  ELSE
                    CHISQ=9999.
                    GO TO 186
                  ENDIF
  185           CONTINUE
  186           CONTINUE
              ELSE
                CHISQ=9999.
              ENDIF
            ENDIF
            RW(KROW(KTMTL,IMT)+JTMTCS)=CHISQ
  700     CONTINUE
  800   CONTINUE
      ENDIF
C
C++   Get the drop flag if any, then drop POT banks if required;
C     add JUL banks to S-list
C     POT banks are on PLIST, JUL banks on JLIST
C
      PLIST = 'PTMAPTML'
      JLIST = 'TGMATMTL'
C! add JLIST to S-list, drop PLIST if required
      IF (LNBLNK(LIST).EQ.2) THEN
         IF (LIST(2:2).EQ.'-' .AND. LNBLNK(PLIST).GE.4) THEN
            CALL BDROP (IW,PLIST)
            CALL BLIST (IW,LIST,PLIST(1:LNBLNK(PLIST)))
         ENDIF
      ENDIF
      CALL BLIST (IW,'S+',JLIST(1:LNBLNK(JLIST)))
C
C
  888 CONTINUE
      RETURN
C
  999 CONTINUE
      CALL BDROP(IW,'TGMATMTL')
      RETURN
      END
