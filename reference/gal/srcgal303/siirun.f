      SUBROUTINE SIIRUN
C.----------------------------------------------------------------
C  B.Bloch-Devaux  October 91
C! SCAL run initialisation
C     - Called by ASIMOD
C     - Calls     SIRDAF,SIDFGO                      from this HLB
C                 ALTELL, GTSTUP                     from ALEPHLIB
C.----------------------------------------------------------------
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
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
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON/SINAMC/NASIHT,NASIHI,NASIDI,NASIX2,NASIXA,NASIFO
      PARAMETER ( NSIST = 15)
      DOUBLE PRECISION ESICOU
      COMMON/SISTAT/NSIPRT,NSICOU(NSIST),ESICOU(NSIST)
      LOGICAL SIPARF
      COMMON /SIPARM/   SINORM,SIALPH(2),
     &                  SIGMAA(12),SIGMAB(12),SIGMAC(12),
     &                  SIRAAB(12),SIRABC(12),
     &                  SIFLUC,SIPERG,SIPARF
      INTEGER GTSTUP,AGETDB
      EXTERNAL GTSTUP,AGETDB
C ----------------------------------------------------------------
      ISITP = GTSTUP ('SI',1)
C
C Define formats and name-indices of SI BOS banks
C
C---BANKS created by GALEPH or on line
      NASIHT = NAMIND('SIHT')
      CALL BKFMT('SIHT','I')
      NASIDI = NAMIND('SIDI')
      CALL BKFMT('SIDI','I')
      NASIX2 = NAMIND('SIX2')
      CALL BKFMT('SIX2','I')
      NASIXA = NAMIND('SIXA')
      CALL BKFMT('SIXA','I')
      NASIFO = NAMIND('SIFO')
      CALL BKFMT('SIFO','I')
      NASIHI = NAMIND('SIHI')
      CALL BKFMT('SIHI','I')
C
C Initialize Statistics for SCAL
C
      DO 10 I = 1,NSIST
         NSICOU(I) = 0
         ESICOU(I) = 0.
 10   CONTINUE
C
C No need for SICAL Geometry if not there
      IF ( ISITP.LE.0) GO TO 900
C---BANKS taken from data base
      JND = AGETDB('SZTHSFTHSRCO',ISITP)
      IF (JND.EQ.0) WRITE(IW(6),990)
C
C get Zero suppression scheme
C
      CALL SIZINI
C
C Define Readout Geometry for SCAL
C
      CALL SIRDAF (LRDBIO,IRUNJO,IERR)
      IF (IERR .NE. 0) THEN
         CALL ALTELL ('SIIRUN: missing data base bank(s)',0,'STOP')
      ENDIF
      IF (IPRIJO(9).GT.0) CALL SIPRGO
C
C Fill some geometry quantities
C
      CALL SIDFGO
C
C set parametrization flag defined by ICSIJO(2)
C
      SIPARF = ICSIJO(2).EQ.1
C
 900  CONTINUE
 990  FORMAT(////,'  Warning No SICAL SRCO found for this run  ',
     $          /,'  Defaults will be applied')
      END
