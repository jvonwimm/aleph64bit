      SUBROUTINE VDAMB(IAMB,MAXTRK)
C ----------------------------------------------------------------
C!  Simple subroutine to count the ambiguity bits by track
CKEY VDETDES
C
C  Input:  MAXTRK       = Dimension of IAMB array, should
C                         be large enough to contain the largest event
C                         (IE around 200)
C          FRFT and VDCO must be in the bank list
C
C  Output: IAMB, an integer array numbered by FRFT track number,
C          bits:          meaning
C          8             r-phi ambiguity
C          9             z ambiguity
C  14-9-92 Dave Brown
C
      SAVE FIRST,NVDCO,NFRFT
      INTEGER IAMB(*),MAXTRK
      INTEGER NAMIND
      INTEGER NVDCO,IVDCO,NFRFT,IFRFT,NHIT,IHIT,NTRK,ITRK,QF
      INTEGER IMOD,ILAY,IWAF,IPHI,IVIEW,JAMB
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      INTEGER LCOLS, LROWS, KROW, KNEXT, ITABL,LFRWRD, LFRROW
      REAL RTABL
      INTEGER ID, NRBOS, L
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
C  Initialize the name indices the first time through
C
      IF(FIRST)THEN
        FIRST = .FALSE.
        NVDCO = NAMIND('VDCO')
        NFRFT = NAMIND('FRFT')
      END IF
C
C  Find the VDCO and FRFT banks
C
      IVDCO = IW(NVDCO)
      IFRFT = IW(NFRFT)
C
C  If no FRFT bank, return
C
      IF(IFRFT .LE. 0)RETURN
C
C  Zero the upper bits
C
      NTRK = MIN(LROWS(IFRFT),MAXTRK)
      DO 600 ITRK=1,NTRK
        IAMB(ITRK) = IAND(IAMB(ITRK),255)
 600  CONTINUE
C
C  If no VDCO bank, return
C
      IF(IVDCO .LE. 0)RETURN
C
C  Loop over the hits
C
      NHIT = LROWS(IVDCO)
      DO 700 IHIT=1,NHIT
C
C  Get the quality flag, track number
C
        ITRK = ITABL(IVDCO,IHIT,8)
        QF = ITABL(IVDCO,IHIT,7)
        JAMB = IAND(ISHFT(QF,-2),3)
        IF(ITRK .LE. MAXTRK)THEN
          IAMB(ITRK) = IOR(IAMB(ITRK),ISHFT(JAMB,8))
        END IF
 700  CONTINUE
      RETURN
      END
