      SUBROUTINE SITRAK
C--------------------------------------------------------------
C! - controls tracking in SCAL
C. - B.Bloch-Devaux     910115
C. - Called by  SIHIT                            from this .HLB
C. - Calls  SIXTOA,CAHIST                        from this .HLB
C -----------------------------------------------
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
      COMMON/SICONST/ISIFCT,ISIZCT,SIMPCT,SIPGEV
C Following parameters are filled from Data Base banks SFTH,SZTH,SRCO
C ISIFCT  : Zero suppression threshold on SIFO data   ( ADC count )
C ISIZCT  : Zero suppression threshold on SIDI data   ( Mev       )
C SIMPCT  : Conversion factor from signal to SIFO ADC ( Mev/ADC count )
C SIPGEV  : enegy deposit per GEV for tracking in Silicium( Gev-1)
      COMMON/SINAMC/NASIHT,NASIHI,NASIDI,NASIX2,NASIXA,NASIFO
      DIMENSION POS(3)
C   This is the value to be deposited per Gev for 20Kev tracking cut
C--------------------------------------------------------------
C-    not 1st step in sensitive medium
      IF(ITRKEL(8).NE.1) THEN
C-    Get subcomponents from track elemnt position
         POS(1) = TRKELE(1)
         POS(2) = TRKELE(2)
         POS(3) = TRKELE(3)
         ZPOS = POS(3) +SIGN ( 0.1,TRKELE(3))
         CALL SIXTOA(IAD,IOR,POS(1),POS(2),ZPOS  ,IOK)
         IF (IOK.NE.0 ) THEN
            IF (NSIPRT.GT.1) THEN
                WRITE(LOUTIO,*) ' +++ SITRAK : the following space ',
     &          'point is outside the sensitive area of crystals: ',
     &          'X,Y,Z,R ',
     &          POS ,SQRT(POS(1)*POS(1)+POS(2)*POS(2)),IOK
            ENDIF
         ELSE
C-    Deposit energy on the relevant address (MEV)
            IDE = IFIX(TRKELE(12)*1000./SIPGEV)
            IF (IDE.GT.0 ) THEN
               CALL CAHIST(NASIHI,IAD,IOR+1,IDE)
C-    Count energy elements entering SICAL
               ESICOU(1) = ESICOU(1) +TRKELE(12)*1000./SIPGEV
               IF ( TRKELE(3).GT.0.) THEN
                  ESICOU(2) = ESICOU(2) +TRKELE(12)*1000./SIPGEV
               ELSE
                  ESICOU(3) = ESICOU(3) +TRKELE(12)*1000./SIPGEV
               ENDIF
               IF (NSIPRT.GT.2) WRITE(LOUTIO,1100)IAD,IOR+1,IDE
            ENDIF
         ENDIF
      ENDIF
      RETURN
 1100 FORMAT (' ++++ SITRAK : Triplet address ',I8,' position ',I5,
     #        ' Energy deposit ',I10,' Mev ')
      END
