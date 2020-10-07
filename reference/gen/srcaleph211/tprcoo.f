      SUBROUTINE TPRCOO
C -------------------------------------------------------------------
C - M.MERMIKIDES - 860410                     F.Ranjard - 860418
C!  print TPC coodinates from TPCO and TPCH banks
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
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
      DATA NACO/0/
C --------------------------------------------------------------------
      LOUT = IW(6)
C
C  NACO  is the name-index of the coordinate bank 'TPCO'
C  NACH  is the name-index of the coord-hit ref. bank 'TPCH'
C  NAHT  is the name-index of the hit bank 'TPHT'
C
      IF (NACO .EQ. 0) THEN
         NACO = NAMIND ('TPCO')
         NACH = NAMIND ('TPCH')
         NAHT = NAMIND ('TPHT')
      ENDIF
C
C  Print out TPCO bank
C
      CALL BPRNT (IW,'TPCHTPCO')
      JTPCO = IW(NACO)
      IF (JTPCO.EQ.0) GO TO 999
          KTPCO = JTPCO + LMHLEN
          NWPCO = LCOLS(JTPCO)
          NCO   = LROWS(JTPCO)
          KTPCH = IW(NACH) + LMHLEN
          JTPHT = IW(NAHT)
C
          IREF = 0
          KREF = 0
          WRITE (LOUT,1000) NCO
1000      FORMAT(/1X,'+++TPRCOO+++ TPCO No of coords = ', I6,/
     &            2X,'IC',2X,'ROW',2X,'SLOT',2X,'PAD',4X,'R',5X,
     &          'Phi',5X,'Z',5X,'D(Phi)/DR',1X,'D(Z)/DR',1X,'Stat',1X,
     &          'HitRef',1X, 'KinRef')
          DO 20 IC = 1,NCO
             IREF = IW(KTPCH+IC)
             KREF = ITABL(JTPHT,IREF,1)
             IROW = IW(KTPCO+1)/100000
             ISLOT= MOD (IW(KTPCO+1)/1000,100)
             IPAD = MOD (IW(KTPCO+1),1000)
             WRITE(LOUT,1001) IC,IROW,ISLOT,IPAD,
     &          (RW(KTPCO+M),M= 2,6),IW(KTPCO+7),IREF,KREF
 1001        FORMAT(1X,I4,')',I3,I6,I5,F8.3,F7.4,3F9.3,I5,2I7)
             KTPCO= KTPCO + NWPCO
 20       CONTINUE
C
 999  CONTINUE
      RETURN
      END
