      SUBROUTINE ITIRUN
C.
C...ITIRUN  1.18  930721  18:08                        R.Beuselinck
C.
C!  Initialise ITC routines.
C.
C.  Define formats for all banks used and set /ITNAMC/.
C.  Initialise ITC readout geometry and calibration from database.
C.
C.  Called by: ASIPAC                                   from this .HLB
C.      Calls: ITWINI, ITDCNS, ITRGPA                   from this .HLB
C.             NAMIND, NLIST, BKFMT, BLIST              from BOS77
C.             ALTELL, ALGTDB                           from ALEPHLIB
C.
C-----------------------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
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
      PARAMETER (LIHIT=8, MIHIT=500, LITWP=3)
      PARAMETER (LITFN=6, LITFP=2)
      COMMON /ITNAMC/NAIHIT,NAIDIG,NAITTR,NAIDHR,NAIXBW,
     + JDITWP,JDITFN,JDITFP,JDITNW,JDITAB,JDITDC,JDIZSC
C
      PARAMETER (LITWBK = 7)
      INTEGER JDITWB(LITWBK)
      EQUIVALENCE (JDITWB(1),JDITWP)
C
      COMMON/ITSUMC/NEVHIT,NEVDIT,NTCUIT,NHCUIT,NDCUIT,DIGHIT,
     +      NTSMIT(10),NHSMIT(10),NDSMIT(10),NDHSIT(10)
      INTEGER NEVHIT,NEVDIT,NTCUIT,NHCUIT,NDCUIT,
     +      NTSMIT,NHSMIT,NDSMIT,NDHSIT
      REAL DIGHIT
C
C
      CHARACTER*(*) LIST, NAME*4
      PARAMETER (LIST =
     +  'IGEOITCCILYRIALIIRFEIZFEIZNLIZRSIDRPIRRFIXBWIXCRIEFF')
C
      INTEGER  ALGTDB, GTSTUP
      CHARACTER*4 NLIST
      EXTERNAL NLIST, ALGTDB, GTSTUP
C
C--  Check ITC run condition parameter for legal values
C--
      IF (ICITJO(1).LT.0 .OR. ICITJO(1).GT.2) THEN
         CALL ALTELL('ITIRUN: bad values for run conditions ',0,'STOP')
      ENDIF
C
      NAIHIT = NAMIND('IHIT')
      NAITTR = NAMIND('ITTR')
      NAIDIG = NAMIND('IDIG')
      NAIDHR = NAMIND('IDHR')
      NAIXBW = NAMIND('IXBW')
C
      CALL BKFMT('IHIT','2I,(3I,5F)')
      CALL BKFMT('IDIG','2I,(I)')
      CALL BKFMT('ITTR','2I,(I)')
      CALL BKFMT('IXRP','2I,(I)')
      CALL BKFMT('IDHR','2I,(I)')
C
      JDITWP = 0
      JDITFN = 0
      JDITFP = 0
      JDITNW = 0
      JDITAB = 0
      JDITDC = 0
      JDIZSC = 0
C
      IRUN = GTSTUP('IT',IRUNJO)
      IF (IRUN.EQ.-1) THEN
C       ... no setup number found. ==> Fatal error.
        CALL ALTELL('ITIRUN: no setup found from GTSTUP',0,'STOP')
      ENDIF
C
C--   Load the banks needed from the Data Base one at a time to check
C--   each one for updating or error.  If you don't care then a single
C--   call to ALGTDB with LIST instead of NAME will do.
C--
      N = 1
   10 NAME = NLIST(IW,N,LIST)
      IF (NAME.NE.'    ') THEN
        IND = ALGTDB(LRDBIO, NAME, IRUN)
        IF (IND.EQ.0) THEN
          CALL ALTELL('ITIRUN: Missing Data Base bank '//NAME//
     +  ' for ITC',0,'STOP')
        ENDIF
        N = N + 1
        GO TO 10
      ENDIF
C
C--  Initialise ITC readout geometry.
C--
      CALL ITWINI
C
C--  Setup drift time constants.
C--
      CALL ITDCNS
C
C--  Initialise trigger constants.
C--
      CALL ITRGPA
C
C--  Initialise alignment constants.
C--
      CALL ITROTI
C
C--  Print geometry if required
C
      IF (IPRIJO(2) .NE. 0) CALL ITPGEO
C
C--  Initialise summary counters.
C--
      NEVHIT = 0
      NEVDIT = 0
      DO 20 I=1,10
        NTSMIT(I) = 0
        NHSMIT(I) = 0
        NDSMIT(I) = 0
        NDHSIT(I) = 0
   20 CONTINUE
      END
