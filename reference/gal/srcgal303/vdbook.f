      SUBROUTINE VDBOOK
C-----------------------------------------------------------------------
C! Book histograms for VDET
CKEY VDET HISTO
C!
C!  Author         F.Forti         7/08/86
C!  Modified       G.Triggiani     7/06/88
C!  Modified       P.Cattaneo      17/10/88
C!  Modified       A. Bonissent    10/05/94
C!     Bug fix : two histos with index 110 were booked. Use 111
C!
C!  Description
C!  ===========
C!  If Mini-vertex is active book HBOOK4 histograms for it and
C!  specify options
C!
C!  Called by :    ASBOOK                        from this .HLB
C!  Calls     :    HBOOK1,HIDOPT                 from HBOOK4
C!
C-----------------------------------------------------------------------
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
C
      IF (FHISJO(1)) THEN
C
        CALL HBOOK1(101,'VDET-Number of hits per event$',
     &               50 ,0.,50.    ,0.)
        CALL HBOOK1(102,'VDET-Number of hits per wafer$',
     &               50 ,0.,50.    ,0.)
        CALL HBOOK1(103,'VDET-Number of strips per event$',
     &               100,0.,500.   ,0.)
        CALL HBOOK1(104,'VDET-Number of zed strips per wafer$',
     &               100,0.,100.   ,0.)
        CALL HBOOK1(110,'VDET-Number of r-phi strips per wafer$',
     &               100,0.,100.   ,0.)
        CALL HBOOK1(105,'VDET-Minimum hits distance in a wafer (cm)$',
     &               50 ,0.,5.     ,0.)
        CALL HBOOK1(106,'VDET-Strips'' signals (GeV)$',
     &               100,0.,500.E-6,0.)
        CALL HBOOK1(107,'VDET-Released energy (GeV)$',
     &               100,0.,500.E-6,0.)
        CALL HBOOK1(108,'VDET-Number of zed strips/hit$',
     &               10 ,0.,10.    ,0.)
        CALL HBOOK1(109,'VDET-Number of r-phi strips/hit$',
     &               10 ,0.,10.    ,0.)
        CALL HBOOK1(111,'VDET-Fraction of energy collected$',
     &               100,0.,1.E-0,0.)
C
        DO 100 ID=101,110
          CALL HIDOPT(ID,'1EVL')
  100   CONTINUE
C
      ENDIF
  999 RETURN
      END
