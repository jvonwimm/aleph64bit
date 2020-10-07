      SUBROUTINE MUDSO(NPLIS,IPLIS,KPPDS,NPPDS,KPPOB,NHPLT)
C***********************************************************
C! Looking for pattern in  barrel-endcap overlap           *
CKEY MUCAL MUON CALOBJ / INTERNAL
C                                                          *
C  Authors: U. Bottigli, A.Messineo  - 890310
C                                                          *
C  Input : NPLIS = Number of element of IPLIS              *
C          IPLIS = Vector of patterns index                *
C          KPPDS = Bank index of PPDS                      *
C          NPPDS = Rows of PPDS                            *
C          KPPOB = Bank index of PPOB                      *
C  Output:                                                 *
C          NHPLT = Vector of hits for each hcal plane      *
C                                                          *
C                                                          *
************************************************************
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JPPODI=1,JPPODE=2,JPPOC1=3,JPPOC2=4,JPPOIP=5,JPPOFP=6,
     +          JPPOLP=7,JPPOMD=8,JPPOPD=9,LPPOBA=9)
      PARAMETER(JPPDNL=1,JPPDFL=2,JPPDMD=3,JPPDPP=4,LPPDSA=4)
      PARAMETER (LENVEC=1000,LASPLN=23,MUFLAG=LASPLN+1)
      INTEGER IPLIS(*),NHPLT(*)
      INTEGER NHPLS(MUFLAG),NHPL(MUFLAG)
      PARAMETER(ALB=1.025)
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
      DO 2000 LL=1,MUFLAG
       NHPLS(LL)=0
       NHPL(LL)=0
 2000 CONTINUE
      DO 10 I=1,NPLIS
       IND=IPLIS(I)
       DO 20 LL=1,NPPDS
        NPDSPT=ITABL(KPPDS,LL,JPPDPP)
        IF(NPDSPT.EQ.IND) THEN
         NLAYE=ITABL(KPPDS,LL,JPPDNL)
         IF(NLAYE.GT.LASPLN) GOTO 997
         FIREL=RTABL(KPPDS,LL,JPPDFL)
         NXXX=IFIX(FIREL/ALB+0.5)
         IZONE=ITABL(KPPOB,IND,JPPODI)
         IF(IZONE.EQ.1) THEN
C
C Here we fill for the barrel's layers
C
          NHPL(NLAYE)=NHPL(NLAYE)+NXXX
          IULT=LASPLN
          IF(NHPL(IULT).GE.1.AND.NHPL(IULT-1).GE.1) NHPL(MUFLAG)=1
         ELSE
C
C Here we fill for the overlap's layers
C
          NHPLS(NLAYE)=NHPLS(NLAYE)+NXXX
          IULT=LASPLN-1
          IF(NHPLS(IULT).GE.1.AND.NHPLS(IULT-1).GE.1) NHPLS(MUFLAG)=1
         ENDIF
        ENDIF
 20    CONTINUE
 10   CONTINUE
C
C Here we correct for the endcap's layers
C
      LAST=-1
      DO 30 I=1,LASPLN
       IF(NHPL(I).NE.0) LAST=I
 30   CONTINUE
      INDPL=0
      DO 40 LL=1,LASPLN-1
       IF(LL.LE.7) THEN
        NHPL(LL)=NHPL(LL)+NHPLS(LL)
       ELSE
        IF(LAST.NE.0) THEN
         INDPL=INDPL+1
        ELSE
         INDPL=LL
        ENDIF
        IPLSV=INDPL+LAST
        IF(IPLSV.GE.LASPLN-1) IPLSV=LASPLN-1
        NHPL(IPLSV)=NHPL(IPLSV)+NHPLS(LL)
       ENDIF
  40  CONTINUE
      IF(NHPL(MUFLAG).EQ.1.OR.NHPLS(MUFLAG).EQ.1)NHPLT(MUFLAG)=1
      DO 50 I=1,LASPLN
       NHPLT(I)=NHPLT(I)+NHPL(I)
 50   CONTINUE
 997  CONTINUE
      RETURN
      END
