       SUBROUTINE VGCRMT (IWAFA,CORMAT,IER)
C ----------------------------------------------------------------------
CKEY VDETDES ALIGN JULIA / USER
C!  Retrieves local wafer alignment covariance matrix from VALC
C - Alain Bonissent, March 1995
C
C   This routine returns the error correlation matrix from the
C   VALC bank (local alignment).  By definition, the error on the
C   global alignment is zero.
C
C   This operation was previously performed in VDHTER for VDET91.
C   The correspondence between the wafers and the VALC row numbers
C   was given by the routine VDINDX.  Accordingly, the VALC banks
C   for VDET91 contained 108 rows (=96 for the wafers + 12 dummies).
C
C   VGCRMT checks the wafer identifier in each row of VALC, so the
C   order of the rows is arbitrary.  The dummy rows are not needed.
C
C   The row and column indices of CORMAT are interchangeable, being
C   markers for the alignment quantities in the order:
C    V displacement,
C    U displacement,
C    W displacement,
C    V rotation,
C    U rotation,
C    W rotation.
C   The returned quantities are SQUARED errors (in the case of
C   diagonal elements).
C
C - Input:
C   IWAFA       / I  Decimal wafer address
C
C - Output:
C   CORMAT(6,6) / R  Covariance matrix
C   IER         / I  = 0 if successful
C                    = 1 if error occurred
C ----------------------------------------------------------------------
C     IMPLICIT NONE
      SAVE FIRST, NAVALC, IORD
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      INTEGER JVALWI, JVALTR, JVALRO, JVALEM, LVALCA
      PARAMETER (JVALWI=1, JVALTR=2, JVALRO=5, JVALEM=8, LVALCA=28)
C
C     Arguments:
C
      INTEGER IWAFA, IER
      REAL CORMAT(6,6)
C
C     Local variables
C
      INTEGER NAVALC, KVALC, IVALC, NVALC, IWL, I, J, IORD(6,6)
      INTEGER NAMIND
      LOGICAL FIRST
      INTEGER LCOLS, LROWS, KROW, KNEXT, ITABL,LFRWRD, LFRROW
      REAL RTABL
      INTEGER ID, NRBOS, L
C
      DATA IORD / 1, 2, 4, 7,11,16,
     >            2, 3, 5, 8,12,17,
     >            4, 5, 6, 9,13,18,
     >            7, 8, 9,10,14,19,
     >           11,12,13,14,15,20,
     >           16,17,18,19,20,21 /
      DATA FIRST / .TRUE. /
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
C ----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        NAVALC = NAMIND('VALC')
      ENDIF
C
      KVALC = IW(NAVALC)
      IER = 1
      IF (KVALC .EQ. 0) GO TO 999
      NVALC = LROWS(KVALC)
      DO 100 IVALC=1,NVALC
        IWL = ITABL(KVALC,IVALC,JVALWI)
        IF (IWAFA .EQ. IWL) THEN
          DO I=1,6
            DO J=1,6
              CORMAT(I,J) = RTABL(KVALC,IVALC,JVALEM-1+IORD(I,J))
            ENDDO
          ENDDO
          IER = 0
          GO TO 999
        ENDIF
  100 CONTINUE
C
  999 CONTINUE
      RETURN
      END
