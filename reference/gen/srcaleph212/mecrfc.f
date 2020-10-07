      INTEGER FUNCTION MECRFC(LST33,KSTAG)
C.----------------------------------------------------------------------
CKEY GAMPACK CRACK / INTERNAL
C   J.C.Brient      Creation  1/10/91
C!  Test ECAL crack for photon
C   Input :
C           LST33   central matrix of the photon INTEGER
C           KSTAG   stack number                 INTEGER
C   Output:
C           Function = 1 if the photon is near a ECAL crack
C   Calls: ESRBC,EBCDRG
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
C
C--for INPUT DIMENSION
      DIMENSION LST33(3,6,3)
C
C--for EBCDRG
      DIMENSION IKOD(4) , NREG(3)
      DIMENSION IDEST(3) , ICLST(3) , POINT(3)
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
      MECRFC = 0
C
      DO 150 I = 1 , 3
        DO 100 J = 1, 6
          IT = LST33(I,J,1)
          JF = LST33(I,J,2)
          KS = LST33(I,J,3)
          IF(KS .NE. KSTAG ) GO TO 100
          IF(IT .LE. 0 .OR. JF .LE. 0 ) GO TO 100
          CALL ESRBC('ALEPH',IT,JF,KS,POINT)
          PM= VMOD ( POINT , 3 )
          TE = ACOS ( POINT(3) / PM )
          FE = ATAN2( POINT(2) , POINT(1)  )
          CALL EBCDRG(TE,FE,ITT,JFF,IKOD,NREG,IER)
          IF(IKOD(3) .NE. 0 ) THEN
            MECRFC = 1
            RETURN
          ENDIF
          IF(IKOD(2) .EQ. 1 ) THEN
            MECRFC = 1
            RETURN
          ENDIF
          IF(IER .EQ. 1 ) THEN
            MECRFC = 1
            RETURN
          ENDIF
  100   CONTINUE
  150 CONTINUE
C
      RETURN
      END
