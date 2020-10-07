      SUBROUTINE SIASIG
C--------------------------------------------------------------
C! Controls the analog signal processing in SCal
C. - B.Bloch-Devaux October 91
C. - Called by  ASASIG                             from this Lib
C. - Calls      BDROP                              from BOS  Lib
C. - Calls      AUBPRS,PRTABL,SIDCOD,CAFIHT,ALTELL from ALEPHLIB
C --------------------------------------------------------------
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
      COMMON/SINAMC/NASIHT,NASIHI,NASIDI,NASIX2,NASIXA,NASIFO
      PARAMETER ( NSIST = 15)
      DOUBLE PRECISION ESICOU
      COMMON/SISTAT/NSIPRT,NSICOU(NSIST),ESICOU(NSIST)
      INTEGER CAFIHT
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
C -------------------------------------------------------------
C
C - If no SIHI exists nothing to be done
      IF (IW(NASIHI) .LE. 0) GO TO 999
C - Reduce bank size to real one
      CALL AUBPRS('SIHI')
C - protect against zero length
      IF (LROWS(IW(NASIHI)).LE.0) GO TO 999
      IF (NSIPRT.GT.0)  CALL PRTABL('SIHI',0)
C - Fill hit bank
      JSIHT = CAFIHT(NASIHI,'SIHT')
      IF (JSIHT.LE.0) GO TO 998
      CALL BLIST (IW,'T+','SIHT')
C - count number of pads hit per event
      RET = 0.
      DO 120 I=1 , LROWS(JSIHT)
      DO 120 J = 1,3
         IF ( ITABL(JSIHT,I,J+1).GT.0) NSICOU(4)=NSICOU(4)+1
         RET =RET+ 0.001*FLOAT(ITABL(JSIHT,I,J+1))
 120  CONTINUE
      IF (NSIPRT.GT.0) THEN
         WRITE (LOUTIO,'(1X,''Total pad energy (Mev)  '',F10.3)') RET
         IF (NSIPRT.GE.2) THEN
            WRITE (LOUTIO,'(/1X,''+++SIASIG+++'')')
            WRITE (LOUTIO,'(1X,'' SIHT bank : nrows/ncols '',2I5)')
     &             LROWS(JSIHT),LCOLS(JSIHT)
            WRITE (LOUTIO,'(1X,''Pad hits bank SIHT''/3X,''Module'',4X,
     &              ''phi '',''radius '',5X,''Planes'',19X,''Hits'')')
            DO 130 J =1 , LROWS(JSIHT)
               IAD = IW(KROW(JSIHT,J)+1)
               CALL SIDCOD(IAD,0,IMD,IST,IPH,IRD)
               WRITE(LOUTIO,'(9I8)') IMD,IPH,IRD,IST,IST+1,IST+2,
     $              (IW(KROW(JSIHT,J)+K),K=2,4)
  130       CONTINUE
          ENDIF
       ENDIF
C
      ESICOU(10)= ESICOU(10)+DBLE(RET**2)
      IF (FHISJO(9)) CALL SIFILH(1)
C
 999  RETURN
C
C - not enough space
C
 998  CONTINUE
      CALL ALTELL ('SIASIG: not enough space for HIT banks ',1,'NEXT')
      END
