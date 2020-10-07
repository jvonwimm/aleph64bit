      SUBROUTINE PCMECU(NMXALL,MXNEUT,PARLOC,IPEPC,IPCOB,ITYPE,PARGON)
C----------------------------------------------------------------------
CKEY PTOJ PCPA PCQA / USER
C----------------------------------------------------------------------
C!  - Fill PCQA bank from PARLOC array
C!         if NEMO card not presents merge some particles
C!         make some cuts on partioles with low energy
C!
C!       NMXALL  total number of PCPA particles in this event
C!       MXNEUT  dimension of arrays
C!       PARLOC  array containing 4-vector + PSUM for each particle
C!       IPEPC   PECO number for each particle
C!       IPCOB   PCOB number for each particle
C!       ITYPE   nature for each particle ( see PCPA DDL )
C!       PARGON  logical flag .TRUE. to ignore/cut particle
C!
C!   Author   :-J. Carr    20 June 1991
C!======================================================================
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JPCQNA=1,JPCQPX=2,JPCQPY=3,JPCQPZ=4,JPCQEN=5,LPCQAA=5)
      PARAMETER(JPCPNA=1,JPCPEN=2,JPCPTE=3,JPCPFI=4,JPCPR1=5,JPCPR2=6,
     +          JPCPPC=7,LPCPAA=7)
C
      DIMENSION PARLOC(MXNEUT,5)
      DIMENSION IPEPC(MXNEUT),IPCOB(MXNEUT),ITYPE(MXNEUT)
      LOGICAL PARGON(MXNEUT)
      LOGICAL HADRO,GAMMA,GAMEX,GARBA,LCALO
      DATA MNEMO /0/
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
      HADRO(I) = ITYPE(I).GE.13.AND.ITYPE(I).LE.20
      GAMMA(I) = ITYPE(I).LT.13.AND.ITYPE(I).NE.5
      GAMEX(I) = ITYPE(I).EQ.5
      GARBA(I) = ITYPE(I).EQ.21
      LCALO(I) = ITYPE(I).EQ.22
C ------------------------------------------------------------------
      IF (MNEMO.EQ.0) MNEMO = NAMIND ('NEMO')
C
C     Merge PCPA particles from same CAL object
C      ( no merge if CARD NEMO included)
C
      IF(IW(MNEMO).NE.0) GO TO 40
C
C++   Merge from same PECO OR PCOB object
C     Merge gammas which come from the same peco
C     Merge hadrons which come from the same pcob
C     Merge extracted gammas with the hadrons from the same pcob
C
      DO 300 I=1,NMXALL-1
         IF(IPEPC(I).EQ.0) GO TO 300
         DO 301 J=I+1,NMXALL
            IF( IPEPC(I).EQ.IPEPC(J) .AND. GAMMA(I).AND.GAMMA(J) )THEN
               DO 302 K=1,5
                   PARLOC(I,K)=PARLOC(I,K)+PARLOC(J,K)
 302           CONTINUE
C   make particle type  gamma from multi gamma neutral cluster.
               ITYPE(I)=2
               PARGON(J)=.TRUE.
            ENDIF
 301     CONTINUE
 300  CONTINUE
      DO 303 I=1,NMXALL-1
         IF(IPCOB(I).EQ.0) GO TO 303
         DO 304 J=I+1,NMXALL
            IF(     ( IPCOB(I).EQ.IPCOB(J) .AND. HADRO(I).AND.HADRO(J) )
     ,         .OR. ( IPCOB(I).EQ.IPCOB(J) .AND. GAMEX(I).AND.HADRO(J) )
     ,         .OR. ( IPCOB(I).EQ.IPCOB(J) .AND. HADRO(I).AND.GAMEX(J) )
     ,         ) THEN
               DO 305 K=1,5
                   PARLOC(I,K)=PARLOC(I,K)+PARLOC(J,K)
 305           CONTINUE
C   make particle nature hadron+gamma = hadron
C                        gamma+gamma  = gamma
C                        residual+X   = residual
               ITYPE(I)=MAX0(ITYPE(I),ITYPE(J))
               PARGON(J)=.TRUE.
            ENDIF
 304     CONTINUE
 303  CONTINUE
C
C
  40  CONTINUE
C
C++   Make some cuts.
C
      NMAXQ=0
      DO 500 I=1,NMXALL
         IF(PARGON(I)) GO TO 500
C
C++      Cut if energy less than 200 Mev for photons.
C++      Cut if energy less than 400 Mev for hadrons.
C
         IF(GAMMA(I).OR.GAMEX(I))THEN
            ECUT=0.2
         ELSE
            ECUT=0.4
         ENDIF
         IF(PARLOC(I,4).LT.ECUT) THEN
             PARGON(I)=.TRUE.
C++      Cuts to avoid fluctuations for residual energy particles.
         ELSEIF(ITYPE(I).EQ.19) THEN
            PSUM=AMAX1(PARLOC(I,5),0.)
            IF(PARLOC(I,4).LT.0.5*SQRT(PSUM)) PARGON(I)=.TRUE.
         ELSEIF(ITYPE(I).EQ.20) THEN
            PSUM=AMAX1(PARLOC(I,5),0.)
            IF(PARLOC(I,4).LT.1.0*SQRT(PSUM)) PARGON(I)=.TRUE.
         ENDIF
         IF(.NOT.PARGON(I)) NMAXQ=NMAXQ+1
 500  CONTINUE
C
C++   Now put remaining particles in PCQA bank.
C
      LEN  = NMAXQ*LPCQAA + LMHLEN
      CALL AUBOS('PCQA',0,LEN,KPCQA,IGARB)
      IF(IGARB.GE.2) RETURN
      IW(KPCQA+LMHROW) = NMAXQ
      IW(KPCQA+LMHCOL) = LPCQAA
      CALL BLIST(IW,'S+','PCQA')
C
C++   So finally store the good ones.
C
      IPP=0
      DO 600 I=1,NMXALL
         IF(.NOT.PARGON(I)) THEN
            IPP=IPP+1
            KK=KROW(KPCQA,IPP)
            IW(KK+JPCQNA) = ITYPE(I)
            RW(KK+JPCQPX) = PARLOC(I,1)
            RW(KK+JPCQPY) = PARLOC(I,2)
            RW(KK+JPCQPZ) = PARLOC(I,3)
C
C++         avoid E < P.
C
            PN = SQRT(PARLOC(I,1)**2+PARLOC(I,2)**2+PARLOC(I,3)**2)
            EN = PARLOC(I,4)
            IF(EN.LT.PN) EN=PN
            RW(KK+JPCQEN) = EN
         ENDIF
 600  CONTINUE
C
      RETURN
      END
