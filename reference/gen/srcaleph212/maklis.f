      SUBROUTINE MAKLIS(IRLBN,JHACPA,JHACPB,INDOBJ,N,NVEC,IER)
C****************************************************************
C! Make list of objects for tabular relation banks              *
CKEY MUCAL MUON CALOBJ / INTERNAL
C  Author:    R.Tenchini     890310                             *
C                                                               *
C  Input : IRLBN = Relation bank index (the one from NLINK)     *
C          JHACPA= HAC parameter for object A                   *
C          JHACPB= HAC parameter for object B                   *
C          INDOBJ= Element of object A for which  you want      *
C                  the list of elements of objects B that       *
C                  are in relation                              *
C  Output:                                                      *
C          N     = Number of elements B related to INDOBJ       *
C          NVEC  = List of B elements related to INDOBJ         *
C          IER   = 0 -> OK                                      *
C                = 1 -> ERROR More than LENVEC relations        *
C                                                               *
C****************************************************************
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (LENVEC=1000,LASPLN=23,MUFLAG=LASPLN+1)
      INTEGER NVEC(*),NSER(LENVEC)
      LOGICAL TROVAT
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
      IER=0
      NRLBN=LROWS(IRLBN)
      N1=0
      N=0
      DO 10 I=1,NRLBN
         ISERV=ITABL(IRLBN,I,JHACPA)
         IF(ISERV.EQ.INDOBJ) THEN
            INOBJB=ITABL(IRLBN,I,JHACPB)
            IF(INOBJB.NE.0) THEN
               N1=N1+1
               IF(N1.GT.LENVEC) THEN
                  IER=1
                  GO TO 999
               ENDIF
               NSER(N1)=INOBJB
            ENDIF
         ENDIF
 10   CONTINUE
C
C   Here we suppress double counting
C
      DO 20 I=1,N1
         TROVAT=.FALSE.
         DO 30 J=1,N
            IF(NSER(I).EQ.NVEC(J)) THEN
               TROVAT=.TRUE.
            ENDIF
 30      CONTINUE
         IF(.NOT.TROVAT) THEN
            N=N+1
            NVEC(N)=NSER(I)
         ENDIF
 20   CONTINUE
 999  RETURN
      END
