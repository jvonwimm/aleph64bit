      SUBROUTINE MINQMU(ITRK, IBE,IBT,IM1,IM2,NEXP,NFIR,N10,N03,XMULT,
     &  ISHAD,IER)
C
CKEY MDST /INTERNAL
C-----------------------------------------------------------------------
C! Mini-DST interface for QMUIDO.
C
C     Author: Stephen Haywood      09-Jan-91
C
C     Input  : DMUO bank
C              ITRK  = Julia track number
C     Output : variables for QMUIDO - see QMUIDO for description
C              IER   = error code: 0 if ok, +10 if no information
C
C     Called by QMUIDO
C
C     Note:
C     Because some variables are stored in DMUO with limited precision,
C     the results may not be identical to those obtained from the POT.
C     However, the differences should not be important.
C     XMULT is stored to 2 DP in range [0.00,5.11].
C     If a charged track points to a shadowed muon track, then if the
C     charged track is the best matching one, ISHAD is set to that track
C     number; else it is set to -1.
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
      PARAMETER(JDMUHO=1,JDMUHE=2,JDMUSH=3,JDMUDT=4,LDMUOA=4)
C
      DIMENSION HITO(23),HITE(23)
      LOGICAL FIRST
      DATA FIRST,LDMUO / .TRUE.,0 /
      SAVE FIRST,LDMUO
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
      IF(FIRST) THEN
         LDMUO = NAMIND('DMUO')
         FIRST = .FALSE.
      ENDIF
C
C++   Initialise for each track.
C
      IER = 0
      IBE = 0
      IBT = 0
      IM1 = 0
      IM2 = 0
      NEXP = 0
      NFIR = 0
      N10 = 0
      N03 = 0
      XMULT = 0.
      ISHAD = 0
C
C++   Pick up DMUO bank.
C
      KDMUO = IW(LDMUO)
      IF(KDMUO.LE.0) RETURN
      NDMUO = LROWS(KDMUO)
      IF(NDMUO.LE.0) RETURN
C
C++   Loop over DMUO bank and see if there is any information for this
C++   track.
C
      DO I=1,NDMUO
         IDTRA = ITABL(KDMUO,I,JDMUDT)
         IF(IDTRA.EQ.ITRK) GOTO 100
      ENDDO
C
      RETURN
C
C++   Obtain muon information for this track.
C
  100 IER = 0
      IWORD = ITABL(KDMUO,I,JDMUHO)
      CALL MVBITS(IWORD,0,23,IBT,0)
      CALL MVBITS(IWORD,23,3,IM1,0)
      CALL MVBITS(IWORD,26,3,IM2,0)
      JWORD = ITABL(KDMUO,I,JDMUHE)
      CALL MVBITS(JWORD,0,23,IBE,0)
C
C++   Copy the bit patterns into conveniant arrays.
C
      DO J=1,23
         HITO(J) = FLOAT(JBIT(IWORD,J))
         HITE(J) = FLOAT(JBIT(JWORD,J))
      ENDDO
C
      NFIR = NINT(VSUM(HITO,23))
      NEXP = NINT(VSUM(HITE,23))
C
C++   Count through the expected fired planes to identify the last 10
C++   and last 3 planes.
C++   (Actually, upto but excluding 11th and 4th planes.)
C
      NE = 0
      K03 = 1
      K10 = 1
      DO J=23,1,-1
         NE = NE + NINT(HITE(J))
         IF(NE.GT. 3 .AND. K03.EQ.1) K03 = J + 1
         IF(NE.GT.10 .AND. K10.EQ.1) THEN
            K10 = J + 1
            GOTO 200
         ENDIF
      ENDDO
  200 N10 = NINT( VSUM(HITO(K10),23+1-K10) )
      N03 = NINT( VSUM(HITO(K03),23+1-K03) )
C
      IWORD = ITABL(KDMUO,I,JDMUSH)
      IMULT = 0
      CALL MVBITS(IWORD,0,9,IMULT,0)
      XMULT = FLOAT(IMULT) / 100.
C
C++   Consider whether the muon track is shadowed.
C
      IF(JBIT(IWORD,10).EQ.1) THEN
         IF(JBIT(IWORD,11).EQ.1) THEN
            ISHAD = ITRK
         ELSE
            ISHAD = -1
         ENDIF
      ELSE
         ISHAD = 0
      ENDIF
C
      RETURN
      END
