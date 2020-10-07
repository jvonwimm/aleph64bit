      SUBROUTINE ASIJOB
C ----------------------------------------------------------------------
C. - F.RANJARD - 850325
C! Job initialization
C. - called by   GALEPH
C. - calls       ASINIT, ASREDC, ASREAD, ASIMOD          from this .HLB
C.               ASIGEA, AGEOME, ASIPAC, USIJOB          from this . HLB
C.               AOPDBS, ADBVER                          from ALEPHLIB
C                GINIT, GPMATE, GPTMED, GPROTM, GPVOLU   from GEANT
C.               TIMAD                                   from KERNLIB
C.
C---------------------------------------------------------
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
      COMMON/ALFGEO/ALRMAX,ALZMAX,ALFIEL,ALECMS
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (JASIYM=1,LASIMA=1)
C ----------------------------------------------------------------------
C - Keep initial time
C
      TLIM = 3600.
      CALL TIMAST (TLIM)
      CALL TIMAD(TIMEJO(1))
C
C - Initialize flags and constants
C
      CALL ASINIT
C
C - Read STEERING data cards to set necessary flags for this job
C
      CALL ASREDC
C
C - Build ASIM with date of the geometry
C
      CALL AUBOS ('ASIM',0,LASIMA+LMHLEN,JASIM,IGARB)
      IF (JASIM.EQ.0) CALL ALTELL ('ASIJOB: no space for ASIM bank',0,
     &                             'STOP')
      IW(JASIM+LMHCOL) = LASIMA
      IW(JASIM+LMHROW) = 1
      IW(JASIM+LMHLEN+JASIYM) = IDATJO
      CALL BKFMT ('ASIM','I')
C
C - Do not compress output file
C
      JCOMP = IW(NAMIND('COMP'))
      IF (JCOMP.EQ.0) THEN
         CALL AUBOS ('COMP',0,1,JCOMP,IGARB)
         IF (JCOMP.EQ.0) CALL ALTELL ('ASIJOB: no space for COMP bank',
     &                                 0,'STOP')
         IW(JCOMP+1) = INTCHA ('NONE')
      ENDIF
C
C - check coherence of data cards
C     you cannot read the KINEmatics and PROCess KINEmatics
      IF (MGETJO.NE.0 .AND. IPROJO(1).NE.0) THEN
        CALL ALTELL ('ASIJOB: GET and PROC KINE are incompatible',0,
     &                'STOP')
      ENDIF
C     check the mag.field level
      IF (ABS(ALFIEL) .GT. 20.) THEN
        CALL ALTELL ('ASIJOB: the mag.field is greater than 20KGauss.
     &Is that normal ?',0,'RETURN')
      ENDIF
C
C     set parametrization flag and bank format
      FPARJO = ICECJO(5).EQ.2
      IF (FPARJO) CALL BKFMT ('CAPA','4I,F')
C
C - print geometry date
C
      WRITE (LOUTIO,'(/1X,''+++ASREAD+++ geometry date '',I4)') IDATJO
C
C - Open DAF file
C
      LRDBIO = JUNIDB(0)
      TFILIO(4) = ' '
      CALL AOPDBS (TFILIO(4),IRETD)
      IF (IRETD.NE.0) THEN
        CALL AWERRC (LOUTIO,'AOPDBS',TFILIO(4),IRETD)
        CALL ALTELL('ASIJOB :DB not found reported by AOPDBS ',
     &        0,'STOP')
      ENDIF
      CALL ADBVER(IDAFJO,IDCHJO)
C
C - read run record and get  the run #, PART, GDEC, ASIM banks
C
      CALL ASREAD
C
C - Initialize GEANT3 and get the GEANT version #
C
      CALL GINIT
C
C - Initialize various modules (detectors, trigger) depending on
C   the run # and the GEANT version #.
C
      CALL ASIMOD
C
C - Interface to GEANT3 : set GEANT3 flags
C
      CALL ASIGEA
C
C - Build the geometry in GEANT3
C
      CALL AGEOME
C
C - Initialize various packages used during this run
C
      CALL ASIPAC
C
C - print out materials , media , rotation matrices ,volumes
C
      IF (IPRIJO(19).EQ.1) THEN
        CALL GPMATE(0)
        CALL GPTMED(0)
        CALL GPROTM(0)
        CALL GPVOLU(0)
      ENDIF
C
C - write run record if required
C
      CALL ASIRUN
C
C - Call USER routine
      CALL USIJOB
C
      RETURN
      END
