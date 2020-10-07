      SUBROUTINE HCWSUM
C--------------------------------------------
C
C!      Print statistics
C!
C!      Authors     :G.Catanesi,G.Zito  86/12/03
C!      Modified by :G.Catanesi  87/07/10,87/11/19
C!
C!       -Called by : ASCRUN      from this .HLB
C!   -Calls     : none
C -------------------------------------------
      SAVE
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
      COMMON /HCCOUN/NHCC01,NHCC02,NHCC03,NHCC04,NHCC05,HCEAVE ,HCANST,
     +               HCEPOR(3) ,FHCDEB,FHCDB1,FHCDB2
      LOGICAL FHCDEB,FHCDB1,FHCDB2
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
      CHARACTER*3 TONOF(2)
      DATA TONOF / 'ON ', 'OFF' /
C ---------------------------------------------------------
      WRITE (LOUTIO,'(//3X,''+++HCWSUM+++ HCAL Run Summary '')')
      IF (NHCC05.EQ.0) THEN
         WRITE (LOUTIO,'(//3X,''NO track in HCAL'')')
      ELSE
         HCC01 = FLOAT(NHCC01)/FLOAT(NHCC05)
         HCC02 = FLOAT(NHCC02)/FLOAT(NHCC05)
         HCC03 = FLOAT(NHCC03)/FLOAT(NHCC05)
         HCC04 = FLOAT(NHCC04)/FLOAT(NHCC05)
         HCEAVE = HCEAVE/FLOAT(NHCC05)
         HCANST = HCANST / FLOAT(NHCC05)
C
         WRITE (LOUTIO,'(/10X,''HCAL Run Conditions'')')
         IF (FPARJO) THEN
            WRITE (LOUTIO,'(3X,''GEANTINO parametrisation is ON'')')
         ELSE
            I = ICHCJO(2) + 1
            WRITE (LOUTIO,'(3X,''Parametrisation is '',A)') TONOF(I)
         ENDIF
         I = ICHCJO(3) + 1
         WRITE (LOUTIO,'(3X,''Dead zones are '',A)') TONOF(I)
         I = ICHCJO(4) + 1
         WRITE (LOUTIO,'(3X,''Induction effect is '',A)') TONOF(I)
         I = ICHCJO(5) + 1
         WRITE (LOUTIO,'(3X,''Tube inefficiency is '',A)') TONOF(I)
          I = ICHCJO(6) + 1
         WRITE(LOUTIO,'(3X,''Dead tubes Killing is '',A)')TONOF(I)
C
         WRITE(LOUTIO,500)NHCC05,HCC01,HCC02,HCC03,HCC04,HCANST,HCEAVE
      ENDIF
      RETURN
  500 FORMAT(/3X,'HCAL run summary',I5,' events went through HCAL'/3X,
     +'Average number of track elements / event ',T50,F10.2/ 3X,
     +'Average number of tube segments  / event ',T50,F10.2/ 3X,
     +'Average number of fired tubes    / event ',T50,F10.2/ 3X,
     +'Average number of fired storeys  / event ',T50,F10.2/ 3X,
     +'Average number of streamers  / event ',T50,F10.2/3X,
     +'Average deposited energy in HCAL / event ',T50,F10.2/)
      END
