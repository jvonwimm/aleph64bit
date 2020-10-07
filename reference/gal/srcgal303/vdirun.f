      SUBROUTINE VDIRUN
C-----------------------------------------------------------------------
C! Initialize run for VDET
CKEY VDET
C!
C!
C!  Author         F.Forti        10/6/86
C!  Modified       G.Triggiani    17/02/88
C!  Modified       A. Bonissent   10/05/94
C!
C!  Description
C!  ===========
C!  Read Data Base constants for VDET, Format hits and digits banks,
C!
C!  Called by :    ASIMOD                        from this .HLB
C!  Calls     :    BKFMT                         from BOS77
C!                 VRDDAF,VDGDMP,ALTELL          from  ALEPHLIB
C!
C---------------------------------------------------------------------
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
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
C
      DIMENSION IBID(2)
      INTEGER VDGDMP
      INTEGER GTSTUP,ALGTDB
      EXTERNAL NAMIND
C
C Initialize the geometry package
C
      CALL VRDDAF(LRDBIO,IRUNJO,IFLAG)
C
      IF (IFLAG.NE.1) CALL ALTELL
     &   ('VDIRUN: Return code from VRDDAF.NE.1. Stop',0,'STOP')
C
C
C Make sure that all banks necessary for digitizing are present
C
      IRUN = 1
      LDBAS = JUNIDB(0)
      IVSTP = GTSTUP ('VD',IRUN)
      IRET    = ALGTDB (LDBAS,'VDEPVPHOVDCC',IVSTP)
      KVDEP = IW(NAMIND('VDEP'))
      KVPHO = IW(NAMIND('VPHO'))
      KVDCC = IW(NAMIND('VDCC'))
      IF(KVDEP.EQ.0.OR.KVPHO.EQ.0.OR.KVDCC.EQ.0)THEN
         CALL ALTELL('VDIRUN : data base bank missing ',0,'STOP')
      ENDIF
C
C Initialize statistics variables
C
      CALL VDFILL('INIT',IBID)
C
      CALL BKFMT('VDHT','2I,(3I,7F)')
      CALL BKFMT('VDSS','2I,(3I,7F,2I)')
      CALL BKFMT('VHLS','I')
      CALL BKFMT('VPLH','I')
      CALL BKFMT('VDLH','2I,(6F,3I,F,2I)')
      CALL BKFMT('VTRS','2I,(I,F,2I)')
      CALL BKFMT('VDTE','2I,(2F)')
      CALL BKFMT('VWS1','2I,(F,I)')
      CALL BKFMT('VWS2','2I,(F,I)')
      CALL BKFMT('VWC1','2I,(2F,I)')
      CALL BKFMT('VWC2','2I,(2F,I)')
      CALL BKFMT('VCLU','I')
      CALL BKFMT('VTSK','2I,(F,2I)')
C
C    PRINT minivertex geometrical constants
C
      IF (IPRIJO(1).NE.0) IRET=VDGDMP()
      RETURN
      END
