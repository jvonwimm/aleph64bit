      SUBROUTINE TRUSLU(ITRQUA,KNCHS,THRUST,AXTR)
C-----------------------------------------------------------------------
CKEY EDIR THRUST
C! Gives the correct format for thrust calculation and call the
C! thrust routine.
C-
C   Input  : ITRQUA  = Charged tracks quality array.
C                      Track quality defined in LEPTO.
C            KNCHS   = Number of good charged tracks.
C                      Good tracks defined in LEPTO.
C   Output : THRUT   = Thrust.
C            AXTR    = Thrust axes.
C-
C   Called by : LEPTO
C   Calls     : ULTHRU,TRPFRF
C   Input banks : PFRF
C-
C                                Author: J.C.Brient      date : 29/6/89
C-----------------------------------------------------------------------
      SAVE
C --
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C --
      DIMENSION PRAX(4,242),IFX(242),PP1(4),AXTR(3)
      DIMENSION ITRQUA(*)
      LOGICAL DEBUG
      DATA DEBUG/.TRUE./
C --
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
C --
       NPFRF = 0
       JPFRF = IW(NAMIND('PFRF'))
       IF(JPFRF.GT.0) NPFRF = LROWS(JPFRF)
C --
       NPAT = KNCHS +42
       IF(NPAT.GT.242) THEN
          IF(IW(6).GT.0) WRITE(IW(6),*) 'TRUSLU_ too many charged',
     &                   ' tracks to compute the trust'
          STOP
       ENDIF
C --
        N42 = 42
        IERR2  = 0
        THRUST = -2.
C --
        JTRU = 0
        NP = NPAT - 42
        DO 10 I = 1 , NPFRF
          IF(ITRQUA(I).NE.1)    GO TO 10
          CALL TRPFRF(I,PP1,IRF)
          PP1(4) = PP1(4)**2 + (0.139567)**2
          IF(PP1(4).GT.0.000001) PP1(4) = SQRT(PP1(4))
          JTRU =  JTRU + 1
          IFX (JTRU) = 0
          DO 5 JX = 1 , 4
            PRAX(JX,JTRU) = PP1(JX)
 5        CONTINUE
 10     CONTINUE
C --
        CALL ULTHRU(N42,NP,PRAX,IFX,THRUST,AXTR,OBL)
C --
        RETURN
        END
