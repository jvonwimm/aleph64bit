      SUBROUTINE ASKINE
C ----------------------------------------------------------------------
C. - F.RANJARD - 850328                           modified - 871029
C! Kinematics steering routine
C. - TKINJO = LUND     LUND generator     (default)
C.            SJET     LUND generator , single jet
C.            PART     single particle
C.            other    user generator
C. - Called from    ASPEVE                       from  this .HLB
C. - Calls          ASKLUN , ASKSIN              from  this .HLB
C.                  ASKUSE                       from user library
C.                  there is a dummy subroutine ASKUSE in this .HLB
C.                  to avoid unsatisfied external.
C.
C -----------------------------------------------------
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
      COMMON /KINCOM/   IPROKI,ECMSKI,IDEVKI,ISTAKI,WEITKI
     &                 ,NOTRKI,NITRKI,NIVXKI
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON/GCNUM/NGMATE,NGVOLU,NGROTM,NGTMED,NGTMUL,NGTRAC,NGPART
     +     ,NGSTMA,NGVERT,NGHEAD,NGBIT
      COMMON/GCNUMX/ NGALIV,NGTMST
C
      DATA IFI /0/
C ----------------------------------------------------------------------
C
C - Drop existing banks
C
      CALL BDROP (IW,'VERTKINE')
C - Add kinematics banks to the 'E' list
      CALL BLIST (IW,'E+','VERTKINE')
C
C - keep 1st random number
C
      IF (IFI .EQ. 0) THEN
         IF (IRNDJO(1,1) .NE. 0) CALL RDMIN (IRNDJO(1,1))
         IFI = 1
      ENDIF
      CALL RDMOUT (IRNDJO(1,1))
C
C - Call the event generator according to the process chosen
C
      ISTAKI = 0
      WEITKI = 1
C
      IF (IPROKI .LT. LOFFMC*4) THEN
C -      TKINJO = PART            Single particle generation
         CALL ASKSIN (IDEVKI,ISTAKI,NITRKI,NIVXKI,ECMSKI,WEITKI)
      ELSE
C -      TKINJO = anything else   User generator
         CALL ASKUSE (IDEVKI,ISTAKI,NITRKI,NIVXKI,ECMSKI,WEITKI)
      ENDIF
C
C - skip the event if the status word is NOT 0
C
      IF (ISTAKI .NE. 0) CALL ALTELL (' ',10,'NEXT')
C
C - end
C
 999  CONTINUE
      END
