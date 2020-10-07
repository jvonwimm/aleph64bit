      SUBROUTINE SIHIT
C-------------------------------------------------------------
C! Controls tracking in SCal
C. - B.Bloch-Devaux 910115
C. - Called by   GUSTEP                          from this .HLB
C. - Calls       BDROP,BLIST                     from BOS lib
C. - Calls       ALBOS                           from ALEPHLIB
C. - Calls       SISHOW,SITRAK                   from this LIB
C.
C --------------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (LETRK=27, LITRK=11, LNTRK=10)
      COMMON /TRKCOM/   ITRKEL(LITRK), TRKELE(LETRK), TRKNXT(LNTRK)
     &                , FTRHAD
      LOGICAL FTRHAD
      COMMON /TRKKAR/   TRKVOL
      CHARACTER*4 TRKVOL
C
      PARAMETER (LOFFMC = 1000)
      PARAMETER (LHIS=20, LPRI=20, LTIM=6, LPRO=6, LRND=3)
      PARAMETER (LBIN=20, LST1=LBIN+3, LST2=3)
      PARAMETER (LSET=15, LTCUT=5, LKINP=20)
      PARAMETER (LDET=9,  LGEO=LDET+4, LBGE=LGEO+5)
      PARAMETER (LCVD=10, LCIT=10, LCTP=10, LCEC=15, LCHC=10, LCMU=10)
      PARAMETER (LCLC=10, LCSA=10, LCSI=10)
      COMMON /JOBCOM/   JDATJO,JTIMJO,VERSJO
     &                 ,NEVTJO,NRNDJO(LRND),FDEBJO,FDISJO
     &                 ,FBEGJO(LDET),TIMEJO(LTIM),NSTAJO(LST1,LST2)
     &                 ,IDB1JO,IDB2JO,IDB3JO,IDS1JO,IDS2JO
     &                 ,MBINJO(LST2),MHISJO,FHISJO(LHIS)
     &                 ,IRNDJO(LRND,LPRO)
     &                 ,IPRIJO(LPRI),MSETJO,IRUNJO,IEXPJO,AVERJO
     3                 ,MPROJO,IPROJO(LPRO),MGETJO,MSAVJO,TIMLJO,IDATJO
     5                 ,TCUTJO(LTCUT),IBREJO,NKINJO,BKINJO(LKINP),IPACJO
     6                 ,IDETJO(LDET),IGEOJO(LGEO),LVELJO(LGEO)
     7                 ,ICVDJO(LCVD),ICITJO(LCIT),ICTPJO(LCTP)
     8                 ,ICECJO(LCEC),ICHCJO(LCHC),ICLCJO(LCLC)
     9                 ,ICSAJO(LCSA),ICMUJO(LCMU),ICSIJO(LCSI)
     &                 ,FGALJO,FPARJO,FXXXJO,FWRDJO,FXTKJO,FXSHJO,CUTFJO
     &                 ,IDAFJO,IDCHJO,TVERJO
      LOGICAL FDEBJO,FDISJO,FHISJO,FBEGJO,FGALJO,FPARJO,FXXXJO,FWRDJO
     &       ,FXTKJO,FXSHJO
      COMMON /JOBKAR/   TITLJO,TSETJO(LSET),TPROJO(LPRO)
     1                 ,TKINJO,TGEOJO(LBGE),TRUNJO
      CHARACTER TRUNJO*60
      CHARACTER*4 TKINJO,TPROJO,TSETJO,TITLJO*40
      CHARACTER*2 TGEOJO
C
      PARAMETER (LERR=20)
      COMMON /JOBERR/   ITELJO,KERRJO,NERRJO(LERR)
      COMMON /JOBCAR/   TACTJO
      CHARACTER*6 TACTJO
C
      PARAMETER ( NSIST = 15)
      DOUBLE PRECISION ESICOU
      COMMON/SISTAT/NSIPRT,NSICOU(NSIST),ESICOU(NSIST)
      LOGICAL SIPARF
      COMMON /SIPARM/   SINORM,SIALPH(2),
     &                  SIGMAA(12),SIGMAB(12),SIGMAC(12),
     &                  SIRAAB(12),SIRABC(12),
     &                  SIFLUC,SIPERG,SIPARF
      PARAMETER ( LSROW = 100)
      PARAMETER ( NOMOR = 3)
      PARAMETER(JSIHPT=1,JSIHTA=2,JSIHDE=3,LSIHIA=5)
      LOGICAL FSIAC ,FA,FB
C   definition of the SICAL active area (crystal plane)
      FSIAC = TRKVOL.EQ.'SSIL'
C -------------------------------------------------------------
      IF(FBEGJO(9)) THEN
C - Drop existing banks
         CALL BDROP (IW,'SIHI')
C
C - Book hit banks at the beginning of events
C
         LEN = LSROW *LSIHIA+LMHLEN
         CALL ALBOS('SIHI',0,LEN,JSIHI,IGARB)
         IW(JSIHI+LMHCOL) = LSIHIA
C - Add to the 'E' list
         CALL BLIST(IW,'E+','SIHI')
C
C - Set SI print flag
         IF(FDEBJO) THEN
            IF (IPRIJO(9) .NE. 0) NSIPRT = 1+ICSIJO(1)
         ELSE
            NSIPRT = 0
         ENDIF
C
C - Count events with tracks hitting SCAL
         NSICOU(1) = NSICOU(1) +1
         FA = .TRUE.
         FB = .TRUE.
C - Set begin of event flag to false
         FBEGJO(9) = .FALSE.
      ENDIF
C - Count events with tracks hitting SCAL in end A and B
      IF ( TRKELE(3).GT.0.) THEN
         IF ( FA) THEN
            NSICOU(2) = NSICOU(2) +1
            FA = .FALSE.
         ENDIF
      ELSE
         IF ( FB) THEN
            NSICOU(3) = NSICOU(3) +1
            FB = .FALSE.
         ENDIF
      ENDIF
C
C - Parametrize electron/positron if parametrization has been selected
C   otherwise track particle in active region
      IF ( SIPARF .AND. ITRKEL(11).EQ.2) THEN
        CALL SISHOW
        ITRKEL(9) = NOMOR
      ELSE IF (FSIAC) THEN
        CALL SITRAK
      ENDIF
C
  999 CONTINUE
      RETURN
      END
