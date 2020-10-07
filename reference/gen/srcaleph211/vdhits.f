      SUBROUTINE VDHITS(IHITPAT,MAXTRK)
C ---------------------------------------------------------------------
C!  count the number of VDET hits by layer and view.
CKEY VDET HITS
C
C  Input:  MAXTRK       = Dimension of IHITPAT array, should
C                         be large enough to contain the largest event
C                         (IE around 200)
C          FRFT and VDCO must be in the bank list
C
C  Output: IHITPAT, an integer array numbered by FRFT track number,
C          of which only the first 8 bits are used, as described below
C          bits:     meaning
C          0-1       number of inner layer U hits on track (0,1,or2)
C          2-3       number of outer layer U hits on track     "
C          4-5       number of inner layer W hits on track     "
C          6-7       number of outer layer W hits on track     "
C
C  10-3-92 Dave Brown
C ----------------------------------------------------------------------
      INTEGER IHITPAT(*),MAXTRK
      INTEGER NAMIND
      INTEGER NVDCO,IVDCO,NFRFT,IFRFT,NHIT,IHIT,NTRK,ITRK,QF
      INTEGER IMOD,ILAY,IWAF,IPHI,IVIEW
C
      INTEGER LCOLS, LROWS, KROW, KNEXT, ITABL,LFRWRD, LFRROW
      REAL RTABL
      INTEGER ID, NRBOS, L
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      DATA NVDCO,NFRFT / 0, 0/
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
      IF(NVDCO.EQ.0) THEN
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
C  Zero the entries
C
      NTRK = MIN(LROWS(IFRFT),MAXTRK)
      DO 600 ITRK=1,NTRK
        IHITPAT(ITRK) = 0
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
C  Get the quality flag, track number, and row number from the hit
C
        ITRK = ITABL(IVDCO,IHIT,8)
        QF = ITABL(IVDCO,IHIT,7)
        IMOD = ITABL(IVDCO,IHIT,1)
        CALL VADEWA(IMOD,ILAY,IWAF,IPHI,IVIEW)
C
C   Separate the U and W hits and layers
C
        IF(ITRK .LE. MAXTRK)THEN
          IF(ILAY.EQ.1)THEN
            IF(IAND(QF,1).EQ.1)IHITPAT(ITRK) = IHITPAT(ITRK) + 1
            IF(IAND(QF,2).EQ.2)IHITPAT(ITRK) = IHITPAT(ITRK) + 16
          ELSE
            IF(IAND(QF,1).EQ.1)IHITPAT(ITRK) = IHITPAT(ITRK) + 4
            IF(IAND(QF,2).EQ.2)IHITPAT(ITRK) = IHITPAT(ITRK) + 64
          END IF
        END IF
 700  CONTINUE
      RETURN
      END
