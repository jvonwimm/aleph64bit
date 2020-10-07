      SUBROUTINE VTRUH(IHITPAT,IFK,MAXTRK)
C ---------------------------------------------------------------------
C!  count the number of properly associated
C!  VDET hits by layer and view.
CKEY VDET HITS
C
C  Input:  MAXTRK       = Dimension of IHITPAT array, should
C                         be large enough to contain the largest event
C                         (IE around 200)
C           IFK          = best match monte carlo track for each FRFT tr
C          FRFT and VDCO must be in the bank list
C
C  Output: IHITPAT, an integer array numbered by FRFT track number,
C          of which only the first 8 bits are used, as described below
C          bits:     meaning
C          10-11 number of inner layer good U hits on track (0,1,or2)
C          12-13 number of outer layer good U hits on track     "
C          14-15 number of inner layer good W hits on track     "
C          16-17 number of outer layer good W hits on track     "
C
C  30-05-96 Alain Bonissent
C ----------------------------------------------------------------------
      INTEGER IHITPAT(*),IFK(*),MAXTRK
      INTEGER NAMIND
      INTEGER NAVDCO,KVDCO,NFRFT,IFRFT,NVDCO,IVDCO,ITRK,QF
      INTEGER IMOD,ILAY,IWAF,IPHI,IVIEW
      INTEGER NAVDFK,IFKIN
      SAVE NAVDCO,NFRFT,NAVDFK
C
      INTEGER LCOLS, LROWS, KROW, KNEXT, ITABL,LFRWRD, LFRROW
      REAL RTABL
      INTEGER ID, NRBOS, L
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
      INTEGER IW
      REAL RW(10000)
      COMMON /BCS/ IW(10000)
      EQUIVALENCE (RW(1),IW(1))
      PARAMETER(JVDFPC=1,JVDFSC=3,JVDFFK=5,JVDFVD=6,LVDFKA=6)
      DATA NAVDCO,NFRFT / 0, 0/
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+LMHCOL)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+LMHROW)
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
C  Initialize the name indices the first time through
C
      IF(NAVDCO.EQ.0) THEN
        NAVDCO = NAMIND('VDCO')
        NFRFT = NAMIND('FRFT')
        NAVDFK = NAMIND('VDFK')
      END IF
C
C  Find the VDCO and FRFT banks
C
      KVDCO = IW(NAVDCO)
      IFRFT = IW(NFRFT)
C
C Get VDFK bank
C
      KVDFK = IW(NAVDFK)
C
C  If no FRFT bank, return
C
      IF(IFRFT .LE. 0)RETURN
C
C  If no VDCO bank, return
C
      IF(KVDCO .LE. 0)RETURN
C
C  If no VDFK bank, return
C
      IF(KVDFK .LE. 0)RETURN
      NVDFK=LROWS(KVDFK)
C
C  Loop over the hits
C
      NVDCO = LROWS(KVDCO)
      DO 700 IVDCO=1,NVDCO
C
C  Get the quality flag, track number, and row number from the hit
C
        ITRK = ITABL(KVDCO,IVDCO,8)
        IFKIN=IFK(ITRK)
C
C Check quality of association
C
        IOKP=0
        IOKZ=0
        DO 30 IVDFK=1,NVDFK
          ITL = ITABL(KVDFK,IVDFK,JVDFFK)
          IF(ITL.NE.IFKIN)GO TO 30
          NMATCZ=ITABL(KVDFK,IVDFK,JVDFSC)
          NMATCP=ITABL(KVDFK,IVDFK,JVDFSC+1)
          IMATCZ=0
          IMATCP=0
          IF(NMATCZ.GT.0)IMATCZ=1
          IF(NMATCP.GT.0)IMATCP=1
          IF(IMATCZ.EQ.0.AND.IMATCP.EQ.0)GO TO 30
          IVDCL=ITABL(KVDFK,IVDFK,JVDFVD)
          IF(IMATCZ.EQ.1.AND.IVDCL.EQ.IVDCO)THEN
             IOKZ=1
          ENDIF
          IF(IMATCP.EQ.1.AND.IVDCL.EQ.IVDCO)THEN
             IOKP=1
          ENDIF
   30   CONTINUE
        QF=0
        IF(IOKP.EQ.1)QF=IOR(QF,1)
        IF(IOKZ.EQ.1)QF=IOR(QF,2)
        IMOD = ITABL(KVDCO,IVDCO,1)
        CALL VADEWA(IMOD,ILAY,IWAF,IPHI,IVIEW)
C
C   Separate the U and W hits and layers
C
        IF(ITRK .LE. MAXTRK)THEN
          IF(ILAY.EQ.1)THEN
            IF(IAND(QF,1).EQ.1)IHITPAT(ITRK) = IHITPAT(ITRK) + 1024
            IF(IAND(QF,2).EQ.2)IHITPAT(ITRK) = IHITPAT(ITRK) + 16384
          ELSE
            IF(IAND(QF,1).EQ.1)IHITPAT(ITRK) = IHITPAT(ITRK) + 4096
            IF(IAND(QF,2).EQ.2)IHITPAT(ITRK) = IHITPAT(ITRK) + 65536
          END IF
        END IF
 700  CONTINUE
      RETURN
      END
