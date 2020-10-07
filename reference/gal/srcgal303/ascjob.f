      SUBROUTINE ASCJOB
C ----------------------------------------------------------------------
C. - F.RANJARD - 850328
C! Close the job
C.   print job summary
C.   print histograms
C.   close files
C. - Called by    : QNEXT                            from this .HLB
C. - Calls        : ACLOSE                           from ALEPHLIB
C.                  ASWSUM, xxWSUM, USCJOB           from this .HLB
C.                  HPHST                            from GENLIB
C.
C ----------------------------------------------------------
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
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
C ----------------------------------------------------------------------
C
C - write the last record
      IF(MSAVJO.GT.0) THEN
        IF (MGETJO.GT.0) THEN
           CALL ABWEND
        ELSE
           CALL BWRITE (IW,LSAVIO,'0')
        ENDIF
      ENDIF
C
C - Print run summary
      CALL ASWSUM
C
C - Print detector summaries
      IF (IDETJO(1).GT.0) CALL VDWSUM
      IF (IDETJO(2).GT.0) CALL ITWSUM
      IF (IDETJO(3).GT.0) CALL TPWSUM
      IF (IDETJO(4).GT.0) CALL ECWSUM
      IF (IDETJO(7).GT.0) CALL HCWSUM
      IF (IDETJO(5).GT.0) CALL LCWSUM
      IF (IDETJO(6).GT.0) CALL SAWSUM
      IF (IDETJO(8).GT.0) CALL MUWSUM
      IF (IDETJO(9).GT.0) CALL SIWSUM
C
C - Print trigger summary
      IF (IPROJO(5).NE.0) THEN
         CALL X1WSUM
         CALL X2WSUM
      ENDIF
C
C - Call USER routine
      CALL USCJOB
C
C - Print histograms
C
      CALL HPHST(0)
C
C - Close all files
C
      CALL ACLOSE (0,IER)
C
      RETURN
      END
