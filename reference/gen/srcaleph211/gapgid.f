      SUBROUTINE GAPGID
C----------------------------------------------------------------------
C!  - Build PGID bank (result from EBNEUT )
CKEY PHOTONS PGID
C!   Author   :- MN Minard             27-JAN-1993
C!======================================================================
      DIMENSION IOPT(6),IFOT(19),PHOT(23),CORE(6)
      DIMENSION CMINFO(16)
      DIMENSION XGER(3),XVXR(3),DX(3)
C! define universal constants
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (JPGIIF=1,JPGIDE=2,JPGICM=3,JPGIM1=4,JPGIM2=5
     &          ,JPGIM3=6,JPGICE=7,JPGITH=8,JPGIPH=9,JPGIPE=10
     &          ,LPGIDA=10)
      PARAMETER(JPECER=1,JPECE1=2,JPECE2=3,JPECTH=4,JPECPH=5,JPECEC=6,
     +          JPECKD=7,JPECCC=8,JPECRB=9,JPECPC=10,LPECOA=10)
      PARAMETER(JEGTET=1,JEGTTY=2,LEGTEA=2)
      PARAMETER(JPCRPC=1,JPCRPE=2,JPCRPF=3,JPCRPH=4,JPCRPP=5,LPCRLA=5)
      INTEGER ALGTDB,GTSTUP
      CHARACTER DET*2, LIST*4
      DATA NAEGTE/0/, IROLD/0/, DET/'EC'/,LIST/'EGTE'/
      DATA NAPECO,NAPCRL /2*0/
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
C-   Initialise pointers
C
      IF(NAEGTE.EQ.0) THEN
        NAPECO = NAMIND('PECO')
        NAEGTE = NAMIND('EGTE')
        NAPCRL = NAMIND('PCRL')
      ENDIF
C
C-   Initialise threshold
C
C! Get banks from DB depending on run and setup code
C
      CALL ABRUEV (IRUN,IEVT)
      IRET = 0
      IF (IRUN.NE.IROLD) THEN
        IROLD = IRUN
        IF (IRUN.LE.2000) THEN
           ITP = GTSTUP (DET,IRUN)
        ELSE
           ITP = IRUN
        ENDIF
        IRET= ALGTDB(JUNIDB(0),LIST,-ITP)
      ENDIF
C
      ETHGAM = 0.
      KEGTE = IW(NAEGTE)
      NEGTE = 0
      IF ( KEGTE.NE.0) NEGTE = LROWS(KEGTE)
      DO IEGTE = 1,NEGTE
        IF(ITABL(KEGTE,IEGTE,2).EQ.1) THEN
          ETHGAM = RTABL(KEGTE,IEGTE,JEGTET)
        ENDIF
      ENDDO
C
C-   Define vertex
C
      CALL VZERO(XVXR(1),3)
      CALL EVTVER (X,Y,Z)
      XVXR(1) = X
      XVXR(2) = Y
      XVXR(3) = Z
C
C    Built PGID bank
C
      KPECO = IW(NAPECO)
      KPCRL = IW(NAPCRL)
C
C    Cluster bank present ?
C
      NCLU = 0
      IF(KPECO.LE.0) GO TO 999
      NCLU = LROWS(KPECO)
      K = NDROP('PGID',0)
      KMAX = LMHLEN + LPGIDA * NCLU
      CALL AUBOS('PGID',0,KMAX,KPGID,IGARB)
      IF ( IGARB.EQ.2 ) GO TO 999
      IW(KPGID+LMHROW) = 0
      IW(KPGID+LMHCOL) = LPGIDA
      IF ( IGARB.NE.0 ) THEN
        KPECO = IW(NAPECO)
        KPRCL = IW(NAPCRL)
      ENDIF
C
C-  Loop over cluster
C
      IGAM = 0
      DO 1 ICLN = 1, NCLU
         EC = RTABL(KPECO,ICLN,JPECEC)
         KCODE = ITABL(KPECO,ICLN,JPECKD)
         IF ( KCODE.LT.192) THEN
         ICH = ITABL(KPECO,ICLN,JPECRB)
C
C-   Remove charged clusters
C
         IF( ICH.NE.0.AND.ICH.NE.2) GO TO 1
C
C-   Take cluster above threshold
C
         IF ( EC.LT.ETHGAM) GO TO 1
C
C-    Cluster treated by EBNEUT
C
         CALL GBNEUT(ICLN,IOPT,IFOT,PHOT,CORE,IER)
C-
         KFLG = 0
         IF(IER.GT.10) KFLG = 10
         IF ( IFOT(14).EQ.11 ) KFLG = 1
         IF ( IFOT(14).EQ.12 ) KFLG = 2
         IF ( IFOT(9).EQ.1.OR.IFOT(9).EQ.6) KFLG = 3
         IF ( IFOT(9).GE.2.AND.IFOT(9).LE.5) KFLG = 4
         IF ( IER.LT.-2.AND.IER.GT.-6) KFLG = 4
         IGAM = IGAM + 1
         JPGID = KROW(KPGID,IGAM)
         IW(JPGID+JPGIIF) = KFLG
         CALL ENOL12(PHOT(7),PHOT(8),EC,PHOT(5),ESL1N,ESL2N)
         RW(JPGID+JPGIDE) = PHOT(7)
         CALL ENOF4(PHOT(12),EC,PHOT(5),ESF4N)
         RW(JPGID+JPGICM) = ESF4N
C
C-    Perform moment analysis
C
         CALL CLMOMS(ICLN,NIMP,CMINFO,IWARN,IERROR,EC)
         CALL ENOW12(CMINFO(1),CMINFO(2),EC,PHOT(5),ESW1N,ESW2N)
         IF( IERROR.EQ.0) THEN
           RW(JPGID+JPGIM1) = ESW1N
           RW(JPGID+JPGIM2) = ESW2N
           RW(JPGID+JPGIM3) = CMINFO(5)
         ENDIF
         CALL GVERCR(ICLN,RAD,IER)
         RW(JPGID+JPGIPH) = PHOT(6)
         RW(JPGID+JPGITH) = PHOT(5)
         RW(JPGID+JPGICE) = PHOT(14)
         IF ( IER.EQ.0) THEN
         DR = 0
         XGER(1) = RAD * COS(PHOT(6))* SIN(PHOT(5))
         XGER(2) = RAD * SIN(PHOT(6))* SIN(PHOT(5))
         XGER(3) = RAD * COS(PHOT(5))
         DO IV =1,3
           DX (IV) = XGER(IV)-XVXR(IV)
         ENDDO
         DXER = SQRT(DX(1)**2+DX(2)**2)
         IF ( DXER .GT. 0 ) THEN
         XPHIX = ATAN2(DX(2),DX(1))
         IF (XPHIX.LT.0.) XPHIX = XPHIX + TWOPI
         RW(JPGID+JPGIPH) = XPHIX
         RW(JPGID+JPGITH) = ATAN2(DXER,DX(3))
         ENDIF
         ENDIF
         IW(JPGID+JPGIPE) = ICLN
        ENDIF
 1    CONTINUE
      IW(KPGID+LMHROW) = IGAM
      CALL AUBPRS('PGID')
 999  CONTINUE
      RETURN
      END
