      SUBROUTINE BGETDS
C.----------------------------------------------------------------------
CKEY GAMPACK DEADST / USER
C   AUTHOR   : A.Bonissent et A.Rosowsky
C!  Store the dead storeys in array IMORT
C   Bank EDDB and EKLS are used too load the dead storeys list
C   in array IMORT(3,imaxd)
C   Input : None
C   Output: None
C   Calls: None
C   Called by GAMPEX or USER
C.----------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER ( IMAXD=20000 )
      COMMON/DEADST/IMORT(3,IMAXD),NSTMOR
      PARAMETER(JEDDAD=1,LEDDBA=1)
      PARAMETER(JEKLAD=1,LEKLSA=1)
      INTEGER AGETDB, GTSTUP
      EXTERNAL AGETDB, GTSTUP
      DATA NAEVEH,NAEKLS,NAEDDB,NAAJOB,LRCONS / 5*0/
      DATA NGPRUN /0/
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
C        -------------------------------
      CALL ABRUEV (NRUN,NEVT)
      IF (NRUN .EQ. NGPRUN) RETURN
      NGPRUN = NRUN
      IF (NAEKLS.EQ.0) THEN
         NAEKLS = NAMIND ('EKLS')
         NAEDDB = NAMIND ('EDDB')
         NAAJOB = NAMIND ('AJOB')
         LRCONS = JUNIDB (0)
      ENDIF
C
      IDEAD = 0
C
      DO 1 I=1,IMAXD
        DO J = 1 , 3
          IMORT(J,I) = 0
        ENDDO
    1 CONTINUE
C
      ENDEAD=0.0001
      NTOTW=LMHLEN
      NUMBAD=0
      NKILL=0
C
C   read dead storey banks  , if banks are present (from run header or
C   already loaded from daf) do nothing, otherwise take them from daf.
C
      ISTP = GTSTUP('EC',NGPRUN)
      KAJOB = IW(NAAJOB)
C
C?   EKLS : ECAL killed storeys in the ROC
C
      KEKLS = IW( NAEKLS )
      IF ( KEKLS.EQ.0 ) THEN
        IF ( KAJOB.EQ.0 ) THEN
          IREADB = AGETDB( 'EKLS',NGPRUN )
        ENDIF
      ENDIF
C
C?   EDDB : ECAL Dead storeys
C
      KEDDB = IW( NAEDDB )
      IF ( KEDDB.EQ.0 ) THEN
        IF ( KAJOB.EQ.0 ) THEN
          IREADB = AGETDB( 'EDDB',NGPRUN )
        ELSE
          KEDDB = MDARD (IW,LRCONS,'EDDB',ISTP)
        ENDIF
      ENDIF
C
C
      JEDDB=IW(NAEDDB)
      NEDDB = 0
      IF(JEDDB .GT. 0) NEDDB=LROWS(JEDDB)
      IF(NEDDB .EQ.0) GO TO 15
      DO 10 IB=1,NEDDB
        IADDS=ITABL(JEDDB,IB,1)
        JVAL=IBITS(IADDS,16,8)
        IVAL=IBITS(IADDS,2,9)
        KVAL=IBITS(IADDS,26,2)
        IF(KVAL .EQ. 0 ) GO TO 10
        IF(IVAL .EQ. 0 ) GO TO 10
        IF(JVAL .EQ. 0 ) GO TO 10
        IF (IDEAD.EQ.IMAXD) GOTO 999
        IDEAD = IDEAD + 1
        IMORT(1,IDEAD) = JVAL
        IMORT(2,IDEAD) = IVAL
        IMORT(3,IDEAD) = KVAL
   10 CONTINUE
C
   15 CONTINUE


C! READ DEAD CHANNELS FROM ON LINE EKLS (ROC)
      JEKLS=IW(NAEKLS)
      NEKLS = 0
      IF(JEKLS .GT. 0) NEKLS = LROWS(JEKLS)
      IF (NEKLS .EQ. 0) GOTO 12
      DO 11 IB=1 , NEKLS
        IADDS=ITABL(JEKLS,IB,1)
        JVAL=IBITS(IADDS,16,8)
        IVAL=IBITS(IADDS,2,9)
        KVAL=IBITS(IADDS,26,2)
        IF(KVAL .EQ. 0 ) GO TO 11
        IF(IVAL .EQ. 0 ) GO TO 11
        IF(JVAL .EQ. 0 ) GO TO 11
        IF (IDEAD.EQ.IMAXD) GOTO 999
        IDEAD = IDEAD + 1
        IMORT(1,IDEAD) = JVAL
        IMORT(2,IDEAD) = IVAL
        IMORT(3,IDEAD) = KVAL
   11 CONTINUE
   12 CONTINUE
C
   14 CONTINUE
      NSTMOR = IDEAD
      RETURN
C
  999 WRITE (IW(6),*) ' GAMPEK-BGETDS array IMPORT is too small ', IDEAD
      NSTMOR = IDEAD
      RETURN
      END
