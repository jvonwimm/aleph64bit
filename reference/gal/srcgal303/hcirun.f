      SUBROUTINE HCIRUN
C-----------------------------------------------------
C
C!     Initialisation Routine for HCAL
C!
C!                      Author : G.Catanesi 87/09/15
C!                     modified: F.Ranjard  88/02/05
C!   Called by : ASIMOD                             from this .HLB
C!   Calls     : HRDGAL,HRDTRI,HCNAMI,HCINIT,HCINPA from this .HLB
C!               HRDDAF                             from ALEPHLIB
C!
C ----------------------------------------------------
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
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
C -----------------------------------------------------------------
C  Reads constants from Data Base
C
      CALL HRDDAF (LRDBIO,IRUNJO,IFLAG)
      IF (IFLAG .EQ. 0) THEN
         CALL ALTELL ('HRDDAF: error when getting geo.banks ',0,'STOP')
      ENDIF
      IF (IPRIJO(7) .NE. 0) CALL HCPRGE
C
C  Reads the Data Base sections used only in Galeph
C
      CALL HRDGAL
C
C  Reads the trigger section of the data base
C
      CALL HRDTRI (LRDBIO,IRUNJO,IFLAG)
      IF (IFLAG .EQ. 0) THEN
         CALL ALTELL ('HRDTRI: error when getting trig.banks ',0,'STOP')
      ENDIF
C
C  If the HCAL set is active  .....
C
      IF (IDETJO(7).NE.0) THEN
C     defines and formattes  read-out bank names
         CALL HCNAMI
C     Initialises variables in work commons
         CALL HCINIT
      ENDIF
C
      END
