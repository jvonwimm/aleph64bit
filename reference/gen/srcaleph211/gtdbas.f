      INTEGER FUNCTION GTDBAS(LBASE,KRUN)
C --------------------------------------------------------------------
C! Get name of the correct data base and open it
CKEY ALEF DBASE
C - F.Ranjard - 910308
C
C  Input  :   LBASE = data base logical unit
C             KRUN  = current run number
C
C  Ouput  :  GTDBAS = return code
C                   = 0  a correct data base has been opened
C
C     get ADBR,NR=0  which contains the setup code.
C         ADBS,NR=0  which contains the version number
C         ADBN,NR=0  which contains the name of the data base
C     Then check whether the run number has changed.
C     IF yes THEN
C       Check that the opened data base is the right one.
C     ELSE
C       call AOPDBS again which will use the setup code to open
C       the right data base
C     ENDIF
C
C--------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JADBFV=1,JADBLV=2,JADBFI=3,LADBNA=4)
      PARAMETER(JADBPN=1,JADBFR=2,JADBBP=3,JADBVD=4,JADBIT=5,JADBTP=6,
     +          JADBEC=7,JADBLC=8,JADBSA=9,JADBHC=10,JADBMU=11,
     +          JADBTR=12,JADBGE=13,JADBDB=14,JADBSI=15,JADBBE=16,
     +          LADBRA=16)
      PARAMETER(JADBVN=1,JADBDC=2,LADBSA=2)
      CHARACTER*8  FNAM
      INTEGER GTSTUP
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
C ----------------------------------------------------------------------
      IRUN = IABS (KRUN)
      IRET = 0
C
C - 1st entry
      JADBR = MDARD (IW,LBASE,'ADBR',0)
      IF (JADBR.EQ.0) THEN
          CALL ALTELL (
     &     'ALGTDB: ADBR is missing, current d.b. will be used',
     &      0,'RETURN')
      ELSE
         IDBSTP = GTSTUP ('DB',IRUN)
         JADBN  = MDARD (IW,LBASE,'ADBN',0)
         IF (JADBN.EQ.0) THEN
            CALL ALTELL (
     &        'ALGTDB: ADBN is missing, curent d.b. will be used',
     &         0,'RETURN')
         ELSE
            IVMIN = ITABL(JADBN,IDBSTP,JADBFV)
            IVMAX = ITABL(JADBN,IDBSTP,JADBLV)
            JADBS = MDARD (IW,LBASE,'ADBS',0)
            IF (JADBS.EQ.0) THEN
               CALL ALTELL (
     &         'ALGTDB: ADBS is missing, curent d.b. will be used',
     &          0,'RETURN')
            ELSE
               IVCUR = ITABL(JADBS,1,JADBVN)
               IF (IVCUR.LT.IVMIN .OR. IVCUR.GT.IVMAX) THEN
                  KADBN = KROW (JADBN,IDBSTP)
                  CALL ALSTIN (IW(KADBN+JADBFI),LADBNA-JADBFI+1,FNAM)
                  LN = LNBLNK (FNAM)
                  CALL AOPDBS (FNAM(1:LN),IRET)
                  CALL DAFRST (LBASE)
                  CALL BDROP (IW,'ADBSADBRADBN')
                  CALL BDROP (IW,'+DIS')
                  CALL ADBVER(IVERS,IDATE)
                  JADBR = MDARD (IW,LBASE,'ADBR',0)
               ENDIF
            ENDIF
         ENDIF
      ENDIF
C
      GTDBAS = IRET
C
      END
