      SUBROUTINE TPWSUM
C -------------------------------------------------------------------
C. - M. MERMIKIDES   860415
C! Brief run summary for TPC
C. - Called by      ASCRUN                          from GALEPH.HLB
C  - Modifications:
C       1. D.Cowen/P.Janot 30-6-88  --  Add TPCSIM closings.
C       2. P.Janot         30-9-88  --  Add time statistics.
C -------------------------------------------------------------------
      SAVE
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
      PARAMETER (LTPST=44)
      COMMON /TPSTAT/   JTPSTA (LTPST)
      REAL RTPSTA(LTPST)
      EQUIVALENCE(RTPSTA(1),JTPSTA(1))
C
      DIMENSION IBIN(11)
C
      WRITE(LOUTIO,801)
C
C If TPCSIM was used, call its closing routine, and
C let 'em know it was used!
C
      IF ( ICTPJO(7).GT.0 ) THEN
         CALL TGFINI
         WRITE(LOUTIO,800) TVERJO
C Time statistics
         TIME = 0.
         NEV = NEVTJO
         IF (NEV.GT.0) TIME = RTPSTA(44)/FLOAT(NEV)
         WRITE(LOUTIO,799) TIME
      ENDIF
C
      WRITE (LOUTIO,802) (ICTPJO(K),K=1,3)
      IF (ICTPJO(4).EQ.0) THEN
         WRITE(LOUTIO,803)
      ELSE
         WRITE(LOUTIO,804)
      ENDIF
      IF (LVELJO(3).LT.2) THEN
         WRITE(LOUTIO,805)
      ELSEIF (LVELJO(3).EQ.2) THEN
         WRITE(LOUTIO,806)
      ENDIF
C
C Statistics on hits
C
      WRITE(LOUTIO,807)
      IF (ICTPJO(2).GT.0) WRITE(LOUTIO,808) (JTPSTA (M),M=1,11)
      IF (ICTPJO(3).GT.0) WRITE(LOUTIO,809) (JTPSTA (M),M=12,22)
C
C Statistics on tracks
C
      WRITE(LOUTIO,810)( JTPSTA (22+M),M=1,11)
C
C Statistics on BOS size
C
      WRITE(LOUTIO,811)( JTPSTA (33+M),M=1,5)
C
 800  FORMAT(//,17x,'*******************************************',/
     &          17x,'*                                         *',/
     &          17x,'*   TPCSIM Version ',F4.2,' ran successfully  *',/
     &          17x,'*                                         *',/
     &          17x,'*******************************************',//)
 799  FORMAT(1X,'Time spent per event = ',F9.3,
     .            ' secs during TPC digitization ' )
 801  FORMAT(1H1,//,10X,'RUN SUMMARY FOR TPC',//)
  802   FORMAT(1X,'simulation level for wires, pads, trigger pads ',3I2)
 803  FORMAT(/,' Track elements have not been saved')
 804  FORMAT(/,' Track elements have been saved')
 805  FORMAT(/,' Simple endplate geometry requested')
 806  FORMAT(/,' Detailed endplate geometry requested')
  807   FORMAT(//,'  Hit multiplicity  ',
     & '  100  200  300  400  500  600  700  800  900 1000  >1000',/)
 808  FORMAT(2X,'PADS' ,14X,11I5)
 809  FORMAT(2X,'TRIGGER PADS',6X,11I5)
 810  FORMAT(//,'  Track entries     ',
     & '    10   20   30   40   50   60   70   80   90  100  >100',/,
     &      20X,11I5)
 811  FORMAT(//,'  maximum # of entries in     TPHT     TTHT     TPTE'
     &,'     TPHE     TTHE' /25X,5I9 )
C
      RETURN
      END
