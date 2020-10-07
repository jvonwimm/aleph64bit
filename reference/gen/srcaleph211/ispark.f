      INTEGER FUNCTION ISPARK(MODUL,NEWPLA)
C.----------------------------------------------------------------------
CKEY GAMPACK SPARK/ INTERNAL
C   J.C.Brient      Creation  1/10/91
C! search for spark in ECAL
C  Looks the energy wires in the 45 planes to flag spark in module
C  number MODUL (input)
C   Input :
C           MODUL   ECAL module number           INTEGER
C           NEWPLA  # OF WIRES PLANES IN ALEPH   INTEGER
C   Output:
C           Function = 1 if The module have a "spark" wires distribution
C   Calls: VASUM,VMAXA,LVMAXA
C   Called by GAMPEK
C.----------------------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (IPLNEC=45)
C array used in routine
      DIMENSION  EEMM(IPLNEC)
      DIMENSION  EODEV(2)
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
      ISPARK = 0
      NPEWI  = 0
      JPEWI  = IW(NAMIND('PEWI'))
      IF ( JPEWI.EQ.0) JPEWI = IW(NAMIND('PWEI'))
      IF(JPEWI .GT. 0 ) NPEWI = LROWS(JPEWI)
      IF(NPEWI .LE. 0 ) THEN
        ISPARK = 1
        RETURN
      ENDIF
C
C read wires energy for module number MODUL
C -----------------------------------------
      DO 1   I = 1 , NPEWI
        IMODO = ITABL(JPEWI,I,1)
        IF(IMODO .NE. MODUL ) GO TO 1
        DO K = 2 , NEWPLA + 1
          J=K-1
          EEMM(J) = 0.
          IEXX = ITABL(JPEWI,I,K)
          EPL = 0.
          IF(IEXX .GT. 0 ) EPL = FLOAT(IEXX)/1000.
          IF(EPL  .GT. 0.5) EEMM(J) = EPL/1000.
        ENDDO
    1 CONTINUE
      EMOTO = VASUM(EEMM,NEWPLA)
      IF( EMOTO .LT. 0.05) THEN
        ISPARK = -1
        RETURN
      ENDIF
      EMMX  = VMAXA(EEMM,NEWPLA)
      LLMM  = LVMAXA(EEMM,NEWPLA)
      EODEV(1) = 0.
      EODEV(2) = 0.
      EMOYN = 0.
      NMOYN = 0.
      DO K = 1 , NEWPLA
        IF(EEMM(K) .GT. 0.001) THEN
          KIMP = MOD(K,2) + 1
          NMOYN = NMOYN + 1
          IF(K .NE. LLMM) EMOYN = EMOYN + EEMM(K)
          EODEV(KIMP) = EODEV(KIMP) + EEMM(K)
        ENDIF
      ENDDO
C
C less than 3 planes, it is a spark
C ---------------------------------
      IF(NMOYN .LT. 3 ) THEN
        ISPARK = 1
        RETURN
      ENDIF
      RC = 999.
      IF(EODEV(2) .GT. 0.00001) RC = EODEV(1)/EODEV(2)
      IF(RC .GT. 2.0) THEN
        ISPARK = 1
        RETURN
      ENDIF

      RETURN
      END
