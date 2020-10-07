      SUBROUTINE TPASIG
C -------------------------------------------------------------------
C - TPC user operations after event tracking
C! Compress and sort TCP banks
C. - Called from          ASASIG                     from GALEPH.HLB
C -------------------------------------------------------------------
      SAVE
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
      COMMON /NAMCOM/   NARUNH, NAPART, NAEVEH, NAVERT, NAKINE, NAKRUN
     &                 ,NAKEVH, NAIMPA, NAASEV, NARUNE, NAKLIN, NARUNR
     &                 ,NAKVOL, NAVOLU
      EQUIVALENCE (NAPAR,NAPART)
C
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
      PARAMETER (LTPDRO=21,LTTROW=19,LTSROW=12,LTWIRE=200,LTSTYP=3,
     +           LTSLOT=12,LTCORN=6,LTSECT=LTSLOT*LTSTYP,LTTPAD=4,
     +           LMXPDR=150,LTTSRW=11)
      PARAMETER (LTPST=44)
      COMMON /TPSTAT/   JTPSTA (LTPST)
      REAL RTPSTA(LTPST)
      EQUIVALENCE(RTPSTA(1),JTPSTA(1))
C
      DIMENSION NPHR(LTPDRO),NTHR(LTTROW)
      PARAMETER (LTPTE=12, LBTE=1000*LTPTE, LBTEX=LBTE/2)
      PARAMETER (NWPHT=6 , LBHT=1000*NWPHT, LBHTX=LBHT/2)
      PARAMETER (NWPHE=1 , LBHE=1000*NWPHE, LBHEX=LBHE/2)
      COMMON /TPNAMC/ NATPTE,NATPHT,NATPHE,NATTHT,NATTHE,NATPCO,NATPCH,
     &                 NATCRL
     &               ,JDTWRK,JDSORT,JDWORK
C -------------------------------------------------------------------
C
C   Reduce size of banks if possible
C
      CALL AUBPRS ('TPTETPHTTPHETTHTTTHE')
C
C  Accumulate some statistics for the run summary
C  Count hits on each padrow and track entries
C  (at this stage, hits are stored in chronological
C  order for each track)
C
      KHTP = IW(NATPHT)
      IF (KHTP.GT.0) THEN
         IBIN = IUCHAN(FLOAT(IW(KHTP+2)),0.,100.,10)
         IF (IBIN.GT.0) JTPSTA (IBIN) = JTPSTA (IBIN) + 1
         ITLAST = 0
         NTRK = 0
      DO 10 K=1,LTPDRO
  10     NPHR(K)=0
         DO 5 J= 1,IW(KHTP + 2)
            KH = KHTP + 2 + (J-1)*IW(KHTP+1)
            IT =   IW(KH + 1)
            IROW = IW(KH + 2)/100000
            NPHR(IROW) = NPHR(IROW) + 1
            IF (IT.NE.ITLAST) NTRK = NTRK + 1
            ITLAST = IT
5        CONTINUE
         IBIN = IUCHAN(FLOAT(NTRK),0.,10.,10)
         IF (IBIN.GT.0) JTPSTA (22+IBIN) = JTPSTA (22+IBIN) + 1
      ENDIF
C  Trigger pad hits
      KHTT = IW(NATTHT)
      IF (KHTT.GT.0) THEN
         IBIN = IUCHAN(FLOAT(IW(KHTT+2)),0.,100.,10)
         IF (IBIN.GT.0) JTPSTA (11+IBIN) = JTPSTA (11+IBIN) + 1
         ITLAST = 0
         NTRK = 0
         DO 20 K=1,LTTROW
  20     NTHR(K)=0
         DO 6 J= 1,IW(KHTT + 2)
            KH = KHTT + 2 + (J-1)*IW(KHTT+1)
            IT =   IW(KH + 1)
            IROW = IW(KH + 2)/100000
            NTHR(IROW) = NTHR(IROW) + 1
            IF (IT.NE.ITLAST) NTRK = NTRK + 1
            ITLAST = IT
 6       CONTINUE
C Get track statistics from trigger pad hits if pad hits not available
         IF (KHTP.EQ.0) THEN
            IBIN = IUCHAN(FLOAT(NTRK),0.,10.,10)
            IF (IBIN.GT.0) JTPSTA (22+IBIN) = JTPSTA (22+IBIN) + 1
         ENDIF
      ENDIF
C
C  Sort hit banks and their TE references
C
      CALL TPHSRT(NATPHT,NATPHE)
      CALL TPHSRT(NATTHT,NATTHE)
C
C Get statistics on BOS size of banks created by TPC
C
      IF(KHTP.GT.0)
     &     JTPSTA(34) = MAX (JTPSTA(34),IW(KHTP+LMHROW))
      IF(KHTT.GT.0)
     &     JTPSTA(35) = MAX (JTPSTA(35),IW(KHTT+LMHROW))
      KTPTE = IW(NATPTE)
      IF(KTPTE.GT.0)
     &     JTPSTA (36) = MAX (JTPSTA(36),IW(KTPTE+LMHROW))
      KTPHE = IW(NATPHE)
      IF(KTPHE.GT.0)
     &     JTPSTA(37) = MAX (JTPSTA(37),IW(KTPHE+LMHROW))
      KTTHE = IW(NATTHE)
      IF(KTTHE.GT.0)
     &     JTPSTA(38) = MAX (JTPSTA(38),IW(KTTHE+LMHROW))
C Debug printout
C
      IF (FDEBJO .AND. IPRIJO(3).EQ.1) THEN
         IF(ICTPJO(2).GT.0) THEN
            WRITE(LOUTIO,810) NPHR
            CALL TPRHIT('TPHT')
         ENDIF
         IF(ICTPJO(3).GT.0) THEN
            WRITE(LOUTIO,811) NTHR
            CALL TPRHIT('TTHT')
         ENDIF
      ENDIF
 810  FORMAT (/1X,'+++TPASIG+++ TPC pad hits on each row         ',21I4)
 811  FORMAT (/1X,'+++TPASIG+++ TPC trigger pad hits on each row ',19I4)
C
      RETURN
      END
