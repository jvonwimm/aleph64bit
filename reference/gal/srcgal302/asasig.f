      SUBROUTINE ASASIG
C ------------------------------------------------------------------
C - F.Ranjard - 860211
C! Analog signal steering routine
C - Compute analog signals with saturation effects in the calorimeters
C - Fill HIT banks for various detectors if required
C
C - modified by : F.Ranjard - 911002
C                 suppress reference to FBEGJO(9)
C                 call SIASIG if SICAl is there
C - modified by : F.Ranjard - 920131
C                 suppress reference to FASGAL and RDST
C
C - Called by   ASPEVE                          from this .HLB
C - Calls       ECASIG, LCASIG, HCASIG          from this .HLB
C               RDMIN , RDMOUT                  from CERNLIB
C -------------------------------------------------------------------
      SAVE
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
      DATA IFI/0/
C --------------------------------------------------------------------
C
C - Initialize random number if required
C
      IF (IFI.EQ.0) THEN
        IF (IRNDJO(1,3).NE.0) CALL RDMIN (IRNDJO(1,3))
        IFI =1
      ENDIF
      CALL RDMOUT (IRNDJO(1,3))
C
C - loop over some detectors
C
      IF (IDETJO(1).GT.0) CALL VDASIG
      IF (IDETJO(2).GT.0) CALL ITASIG
      IF (IDETJO(3).GT.0) CALL TPASIG
      IF (IDETJO(6).GT.0) CALL SAASIG
      IF (IDETJO(9).GT.0) CALL SIASIG
      IF (IDETJO(4).GT.0) CALL ECASIG
      IF (IDETJO(5).GT.0) CALL LCASIG
      IF (IDETJO(7).GT.0) CALL HCASIG
      IF (IDETJO(8).GT.0) CALL MUASIG
C
      RETURN
      END
