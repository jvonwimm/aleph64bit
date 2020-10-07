      SUBROUTINE SISHOW
C.----------------------------------------------------------------
C  J.Rander & B.Bloch-Devaux  January 92
C! SCAL : Shower parametrisation
C     - Called by SIHIT
C     - Calls     SIDFPA,SILONG,SITRAN,SITRA3,CAHIST from this lib
C                 SIZFRI,SIXTOA                      from Alephlib
C                 RANNOR,RNDM                        fron Cern libs
C.----------------------------------------------------------------
      SAVE
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      INTEGER NBITW, NBYTW, LCHAR
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
      PARAMETER (NBITW = 32 , NBYTW = NBITW/8 , LCHAR = 4)
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
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
      PARAMETER (LETRK=27, LITRK=11, LNTRK=10)
      COMMON /TRKCOM/   ITRKEL(LITRK), TRKELE(LETRK), TRKNXT(LNTRK)
     &                , FTRHAD
      LOGICAL FTRHAD
      COMMON /TRKKAR/   TRKVOL
      CHARACTER*4 TRKVOL
C
      PARAMETER ( NSIST = 15)
      DOUBLE PRECISION ESICOU
      COMMON/SISTAT/NSIPRT,NSICOU(NSIST),ESICOU(NSIST)
      LOGICAL SIPARF
      COMMON /SIPARM/   SINORM,SIALPH(2),
     &                  SIGMAA(12),SIGMAB(12),SIGMAC(12),
     &                  SIRAAB(12),SIRABC(12),
     &                  SIFLUC,SIPERG,SIPARF
      COMMON/SIGACO/NMODSI,NRBNSI,NPBNSI,NZBNSI,Z0SNSI(2),
     $              ZWIDSI,ZWFRSI,ZWFLSI,ZWLASI,ZWRFSI,ZWRLSI,OVLPSI
      COMMON/SINAMC/NASIHT,NASIHI,NASIDI,NASIX2,NASIXA,NASIFO
      DATA EMNCT / 0./
C
C--- calculation of parameters which depends upon energy
C
      CALL SIDFPA (TRKELE(8))
C
C--- track enters active area on front face or from inner side
      Z = ABS(TRKELE(3))
      COSTH = ABS(TRKELE(6))
      NUMOD =1
      IF ( TRKELE(3).LT.0.) NUMOD = 2
C
C--- compute which active plane will be first hit
C    ZWIDSI   width between two successive z planes (CM)
C    ZWFLSI   width in front of first Si layer      (CM)
C    ZWRFSI   Number of radiation length before first Si layer
C    ZWRLSI   Number of radiation length per layer
      IF ( Z .LE.Z0SNSI(NUMOD) ) THEN
         IFIR = 1
         S0 =(Z0SNSI(NUMOD)-Z)*ZWRFSI/(COSTH*ZWFLSI)
         ZL0=(Z0SNSI(NUMOD)-Z)/COSTH
      ELSE
         IFIR =(Z - Z0SNSI(NUMOD))/ZWIDSI +2
         S0 =(Z0SNSI(NUMOD)+(IFIR-1)*ZWIDSI-Z)*ZWRLSI/(COSTH*ZWIDSI)
         ZL0=(Z0SNSI(NUMOD)+(IFIR-1)*ZWIDSI-Z)/COSTH
      ENDIF
C
C--- Loop on layers
      DO 20 ISTK = IFIR,NZBNSI
C
C--- Track length in radiation length(S),  CM(ZLON)
        S = S0+ (ISTK-1)*ZWRLSI/COSTH
        ZLON = ZL0 +(ISTK-1)* ZWIDSI/COSTH
        SIGNL = SILONG(S) * SINORM
        CALL RANNOR(ALEA,ALEB)
        SIGNL = SIGNL + SIFLUC*ALEA*SQRT(SIGNL)
           SIGNL = SIGNL * ZWRLSI
        IF(SIGNL.LT.EMNCT) SIGNL=0.0
C
C==> SIMULATE RADIAL DEPOSITION AND FILL HITS INTO SICAL PADS
        NHITS=IFIX(SIGNL*SIPERG)
        IF (NHITS.GT.0) THEN
           CALL SIZFRI(ZHIT,ISTK,NUMOD)
           XHIT0=TRKELE(1)+(ZHIT-TRKELE(3))*TRKELE(4)/TRKELE(6)
           YHIT0=TRKELE(2)+(ZHIT-TRKELE(3))*TRKELE(5)/TRKELE(6)
C
           DO 11 NH =  1,NHITS
              IF ( ICSIJO(4).EQ.0) CALL SITRA3(ISTK,RADIS)
              IF ( ICSIJO(4).EQ.1) CALL SITRAN(ISTK,RADIS)
C------generate shower hit in stack layer
              APHI=TWOPI*RNDM(DUMMY)
              XHIT=XHIT0+RADIS*COS(APHI)
              YHIT=YHIT0+RADIS*SIN(APHI)
C------FIND OUT WHICH PAD HIT
              ZPOS = ZHIT +SIGN ( 0.1,TRKELE(3))
              CALL SIXTOA(IAD,IOR,XHIT,YHIT,ZPOS   ,IOK)
              IF (IOK.NE.0 ) THEN
C-    Count energy elements Lost outside SICAL
                 ESICOU(7) = ESICOU(7) +1000./SIPERG
                 ESICOU(7+NUMOD) = ESICOU(7+NUMOD) +1000./SIPERG
                 IF (NSIPRT.GT.1) WRITE(LOUTIO,1000) XHIT,YHIT,ZHIT,
     $                            SQRT(XHIT*XHIT+YHIT*YHIT),IOK
              ELSE
C-    Deposit energy on the relevant address (MEV)
                 IDE = IFIX(1000./SIPERG)
                 CALL CAHIST(NASIHI,IAD,IOR+1,IDE)
C-    Count energy elements entering SICAL
                 ESICOU(4) = ESICOU(4) +1000./SIPERG
                 ESICOU(4+NUMOD) = ESICOU(4+NUMOD) +1000./SIPERG
                 IF (NSIPRT.GT.2) WRITE(LOUTIO,1100) IAD,IOR+1,IDE
              ENDIF
   11      CONTINUE
C
        ENDIF
 20   CONTINUE
      RETURN
 1000 FORMAT (' ++++ SISHOW : the following space point is outside the'
     #      ,' sensitive area of crystals:X,Y,Z,R', 4F9.4,I4)
 1100 FORMAT (' ++++ SISHOW : Triplet address ',I8,' position ',I5,
     #        ' Energy deposit ',I10,' Mev ')
      END
