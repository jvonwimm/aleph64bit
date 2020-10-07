      SUBROUTINE TPIRUN
C -------------------------------------------------------------------
C. - Clear name indices
C. - Initialise DetectorDescription and SimulationConst
C.   for TPC simulation
C. - Get user geometry
C. - Called from         ASIMOD                     from GALEPH.HLB
C. - Calls                NAMIND, BKFMT              from BOS77.hlb
C -------------------------------------------------------
      SAVE
      PARAMETER (LTPTE=12, LBTE=1000*LTPTE, LBTEX=LBTE/2)
      PARAMETER (NWPHT=6 , LBHT=1000*NWPHT, LBHTX=LBHT/2)
      PARAMETER (NWPHE=1 , LBHE=1000*NWPHE, LBHEX=LBHE/2)
      COMMON /TPNAMC/ NATPTE,NATPHT,NATPHE,NATTHT,NATTHE,NATPCO,NATPCH,
     &                 NATCRL
     &               ,JDTWRK,JDSORT,JDWORK
      PARAMETER (LTPST=44)
      COMMON /TPSTAT/   JTPSTA (LTPST)
      REAL RTPSTA(LTPST)
      EQUIVALENCE(RTPSTA(1),JTPSTA(1))
C
      COMMON/ALFGEO/ALRMAX,ALZMAX,ALFIEL,ALECMS
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
      EXTERNAL NAMIND
C ----------------------------------------------------------------------
C
C  Initialize statistics array filled in TPASIG
C
      DO 10 I = 1,LTPST
         JTPSTA(I) = 0
   10 CONTINUE
C  Name indices for track TPC elements, pad hits, trigger pad hits
      NATPHT = NAMIND ('TPHT')
      NATTHT = NAMIND ('TTHT')
      NATPTE = NAMIND ('TPTE')
      NATPHE = NAMIND ('TPHE')
      NATTHE = NAMIND ('TTHE')
      NATPCO = NAMIND ('TPCO')
      NATPCH = NAMIND ('TPCH')
      NATCRL = NAMIND ('TCRL')
C
      CALL BKFMT ('TPHE','2I,(I)')
      CALL BKFMT ('TTHE','2I,(I)')
      CALL BKFMT ('TPTE','2I,(I,11F)')
      CALL BKFMT ('TTHT','2I,(2I,4F)')
      CALL BKFMT ('TPHT','2I,(2I,4F)')
      CALL BKFMT ('TPCO','2I,(I,5F,4I,2F)')
      CALL BKFMT ('TPCH','(I)')
      CALL BKFMT ('TCRL','(I)')
C
C  Get geometry banks needed from the data base
      CALL TRDDAF (LRDBIO,IRUNJO,IRET)
      IF (IRET .EQ. 0)
     &  CALL ALTELL('TPIRUN - Error accessing banks from DA file',
     &              0,'STOP')
C
C  Debug geometry and useful quantities
C
      IF (IPRIJO(3) .NE. 0) CALL TGEPRI (LOUTIO)
C
C  Full TPCSIM initialisation.
C
      IF( ICTPJO(7) .GT. 0) THEN
         CALL TGINIT (ALFIEL,ICTPJO)
         CALL TPCVER (TVERJO)
      ELSE
C        TVERJO = 0.
      ENDIF
C
      RETURN
      END
