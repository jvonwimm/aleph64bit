      SUBROUTINE HCDIGI
C-------------------------------------------------------
C
C! Implements the module CREATE HC DIGITISINGS
C!
C!     Author     : G.Zito     86/05/21
C!     Modified   : G.Catanesi 89/02/06
C!     Modified   : L.Silvestris 18/3/93
C!
C!     Input bank : HTHT  McHcStoreys
C!                  HWHT  McHcTubeSignal with induction
C!                  HPHT  McHcPlaneSignal
C!     Output bank :
C!                  HWDI McHcTubeDigitising
C!                  HTDI  McHcStackDigitising
C!                  HPDI  McHcPlaneDigitising
C!                  HWTR McHcPlaneTriggerSignal
C!                  HTTR McHcTowerTriggerSignal
C!
C!   -Called by : ASDIGI
C!   -Calls : HCTRGA, HCTRGD, HCFORA, HCFOPL, HCFORD from this .HLB
C!            BDROP,BLIST from BOS77.HLB
C!            ALBOS  from ALEPHLIB
C -------------------------------------------
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
      PARAMETER(NHTTR=192,NHWTR=36)
      PARAMETER (MHCSE=200)
      PARAMETER(MHCTH=300)
      COMMON /HCNAMC/NAHTHT,NAHWHT,NAHTTR,NAHWTR,NAHTDI,NAHWDI, NAHTDT,
     &               NAHTTD,NAHWDT,NAHWTD,NAHPDI,NAHPHT,NAHLWD,
     &               JDHCSE,JDHCHI,JDHCTH
      PARAMETER(JHWDCA=1,LHWDIA=1)
      PARAMETER(JHWTNO=1,LHWTRA=1)
      PARAMETER(JHTDTA=1,JHTDED=2,LHTDIA=2)
      PARAMETER(JHTTED=1,LHTTRA=1)
      PARAMETER(JHPDPA=1,JHPDED=2,LHPDIA=2)
      COMMON /HCCOUN/NHCC01,NHCC02,NHCC03,NHCC04,NHCC05,HCEAVE ,HCANST,
     +               HCEPOR(3) ,FHCDEB,FHCDB1,FHCDB2
      LOGICAL FHCDEB,FHCDB1,FHCDB2
C
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
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
C
C          Check for old digitisings banks and drop them
C
      IDRP = IW(NAHWDI)
      IF (IDRP.NE.0) CALL BDROP (IW,'HTDIHWDIHTTRHWTRHPDI')
C
C       Book HxTR and HxDI banks and add them to 'E' list
C
      JHWHT = IW(NAHWHT)
      IF (JHWHT.EQ.0) RETURN
      NHWHT = LROWS (JHWHT)
      CALL ALBOS ('HWDI',0,NHWHT*LHWDIA+LMHLEN,JHWDI,IGARB)
      IW(JHWDI+1) = LHWDIA
      IW(JHWDI+2) = 0
      CALL ALBOS ('HWTR',0,NHWTR*LHWTRA+LMHLEN,JHWTR,IGARB)
      IW(JHWTR+1) = LHWTRA
      IW(JHWTR+2) = NHWTR
      CALL BLIST(IW,'E+','HWDIHWTR')
C
      JHTHT = IW(NAHTHT)
      IF (JHTHT.EQ.0) RETURN
      NHTHT = LROWS (JHTHT)
      CALL ALBOS ('HTDI',0,NHTHT*LHTDIA+LMHLEN,JHTDI,IGARB)
      IW(JHTDI+1) = LHTDIA
      IW(JHTDI+2) = 0
      CALL ALBOS ('HTTR',0,NHTTR*LHTTRA+LMHLEN,JHTTR,IGARB)
      IW(JHTTR+1) = LHTTRA
      IW(JHTTR+2) = NHTTR
      CALL BLIST(IW,'E+','HTDIHTTR')
C
C     Issue HC trigger signal
C
C     Form tower trigger signal: HTHT -> HTTR 'E' list
C
      CALL HCTRGA
C
C     Form tube trigger signal: HWHT -> HWTR 'E' list
C
      CALL HCTRGD
C
C
      JHPHT = IW(NAHPHT)
      IF (JHTHT.EQ.0) RETURN
      NHPHT = LROWS (JHPHT)
      CALL ALBOS ('HPDI',0,NHPHT*LHPDIA+LMHLEN,JHPDI,IGARB)
      IW(JHPDI+1) = LHPDIA
      IW(JHPDI+2) = 0
      CALL BLIST(IW,'E+','HPDI')
C
C       Format tower signal
C
      CALL HCFORA
C
C
C       Format plane signal
C
      CALL HCFOPL
C       Format tube signal
C
      CALL HCFORD
C
C - Compress output banks
C
      CALL AUBPRS ('HTDIHWDIHPDI')
C
      END
