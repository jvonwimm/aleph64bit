      SUBROUTINE MINPCQ
C
CKEY MDST /USER
C-----------------------------------------------------------------------
C! Fill PCQA bank from DNEU and DRES.
C
C     Author: J.Carr    14 May 1991
c              ( modified from MINPCP of Stephen Haywood )
C-----------------------------------------------------------------------
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JDNENA=1,JDNEE0=2,JDNETH=3,JDNEPH=4,JDNER1=5,JDNER2=6,
     +          JDNEPC=7,JDNEDE=8,LDNEUA=8)
      PARAMETER(JDRENA=1,JDREE0=2,JDRETH=3,JDREPH=4,JDREP0=5,JDREPS=6,
     +          JDREPC=7,LDRESA=7)
      PARAMETER (AFACTM=10000.,DFACTM=10000.,EFACTM=1000.)
*     PARAMETER (AFACTM=100000.,DFACTM=100000.,EFACTM=10000.)
C
      LOGICAL HADRO,GAMMA,GAMEX,GARBA,LCALO
      PARAMETER ( MXNEUT=200 )
      DIMENSION PARLOC(MXNEUT,5)
      DIMENSION IPEPC(MXNEUT),IPCOB(MXNEUT),ITYPE(MXNEUT)
      LOGICAL PARGON(MXNEUT)
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
      HADRO(I) = ITYPE(I).GE.13.AND.ITYPE(I).LE.20
      GAMMA(I) = ITYPE(I).LT.13.AND.ITYPE(I).NE.5
      GAMEX(I) = ITYPE(I).EQ.5
      GARBA(I) = ITYPE(I).EQ.21
      LCALO(I) = ITYPE(I).EQ.22
C
      KDNEU = NLINK('DNEU',0)
      IF(KDNEU.GT.0) THEN
         NDNEU = LROWS(KDNEU)
      ELSE
         NDNEU = 0
      ENDIF
C
      KDRES = NLINK('DRES',0)
      IF(KDRES.GT.0) THEN
         NDRES = LROWS(KDRES)
      ELSE
         NDRES = 0
      ENDIF
C
      IF (NDNEU.LE.0 .AND. NDRES.LE.0) RETURN
      MVER = MINGTV(DUM)
C
C++   Fill temporary PARLOC array from DNEU bank.
C
      J = 0
      DO 100 I=1,NDNEU
         IF(J.LT.MXNEUT)J = J + 1
         IY = ITABL(KDNEU,I,JDNENA)
         EN = FLOAT(ITABL(KDNEU,I,JDNEE0))/EFACTM
         TH = FLOAT(ITABL(KDNEU,I,JDNETH))/AFACTM
         PH = FLOAT(ITABL(KDNEU,I,JDNEPH))/AFACTM
         PSUM=0.
         PN = EN
         CP = COS (PH)
         SP = SIN (PH)
         CT = COS (TH)
         ST = SIN (TH)
         PT = PN * ST
         PARLOC(J,1) = PT * CP
         PARLOC(J,2) = PT * SP
         PARLOC(J,3) = PN * CT
         PARLOC(J,4) = EN
         PARLOC(J,5) = PSUM
         ITYPE(J) = ITABL(KDNEU,I,JDNENA)
         IF (MVER.GE.54) THEN
            IPEPC(J) = ITABL(KDNEU,I,JDNEDE)
            IPCOB(J) = ITABL(KDNEU,I,JDNEPC)
         ELSE
            IPEPC(J) = ITABL(KDNEU,I,7)
            IPCOB(J) = 0
         ENDIF
         PARGON(J) = .FALSE.
         IF(GARBA(I)) PARGON(J) = .TRUE.
  100 CONTINUE
      NMXNEU=J
C
C++   Fill temporary array from DRES.
C
      DO 200 I=1,NDRES
         IF(J.LT.MXNEUT) J = J + 1
         IY = ITABL(KDRES,I,JDRENA)
         EN = FLOAT(ITABL(KDRES,I,JDREE0))/EFACTM
         TH = FLOAT(ITABL(KDRES,I,JDRETH))/AFACTM
         PH = FLOAT(ITABL(KDRES,I,JDREPH))/AFACTM
         PSUM = FLOAT(ITABL(KDRES,I,JDREPS))/EFACTM
         PN   = FLOAT(ITABL(KDRES,I,JDREP0))/EFACTM
         CP = COS (PH)
         SP = SIN (PH)
         CT = COS (TH)
         ST = SIN (TH)
         PT = PN * ST
         PARLOC(J,1) = PT * CP
         PARLOC(J,2) = PT * SP
         PARLOC(J,3) = PN * CT
         PARLOC(J,4) = EN
         PARLOC(J,5) = PSUM
         ITYPE(J) = ITABL(KDRES,I,JDRENA)
         IPEPC(J) = 0
         IF (MVER.GE.54) THEN
            IPCOB(J) = ITABL(KDRES,I,JDREPC)
         ELSE
            IPCOB(J) = 0
         ENDIF
         PARGON(J) = .FALSE.
  200 CONTINUE
C
      NMXALL = J
C
C++   Maybe merge and cut particles
C
      CALL PCMECU( NMXALL,MXNEUT,PARLOC,IPEPC,IPCOB,ITYPE,PARGON)
C
      RETURN
      END
