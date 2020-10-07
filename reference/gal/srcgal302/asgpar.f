      SUBROUTINE ASGPAR
C ----------------------------------------------------------------------
C. - F.Ranjard - 850828
C! Fill 'PTYP' Geant3 bank with 'PART' BOS bank
C. - Define branching ratios and decay modes for standard GEANT particle
C.   (copy of GPART from GEANT pam)
C. - called by    ASIGEA                                from this .HLB
C. - calls        GSDK                                  from GEANT pam
C ----------------------------------------------------------------------
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
      PARAMETER (LFIL=6)
      COMMON /IOCOM/   LGETIO,LSAVIO,LGRAIO,LRDBIO,LINPIO,LOUTIO
      DIMENSION LUNIIO(LFIL)
      EQUIVALENCE (LUNIIO(1),LGETIO)
      COMMON /IOKAR/   TFILIO(LFIL),TFORIO(LFIL)
      CHARACTER TFILIO*60, TFORIO*4
C
      COMMON /KINCOM/   IPROKI,ECMSKI,IDEVKI,ISTAKI,WEITKI
     &                 ,NOTRKI,NITRKI,NIVXKI
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
      CHARACTER*13 NAME
      CHARACTER*4 CHAINT
      PARAMETER (KGWBK=69000,KGWRK=5200)
      COMMON /GCBANK/   NGZEBR,GVERSN,GZVERS,IGXSTO,IGXDIV,IGXCON,
     &                  GFENDQ(16),LGMAIN,LGR1,GWS(KGWBK)
      DIMENSION IGB(1),GB(1),LGB(8000),IGWS(1)
      EQUIVALENCE (GB(1),IGB(1),LGB(9)),(LGB(1),LGMAIN),(IGWS(1),GWS(1))
C
      COMMON/GCLINK/JGDIGI,JGDRAW,JGHEAD,JGHITS,JGKINE,JGMATE,JGPART
     +        ,JGROTM,JGRUN,JGSET,JGSTAK,JGGSTA,JGTMED,JGTRAC,JGVERT
     +        ,JGVOLU,JGXYZ,JGPAR,JGPAR2,JGSKLT
C
      COMMON/GCPHYS/ IGPAIR,GSPAIR,GSLPAI,GZINPA,GSTPPA
     &              ,IGCOMP,GSCOMP,GSLCOM,GZINCO,GSTPCO
     &              ,IGPHOT,GSPHOT,GSLPHO,GZINPH,GSTPPH
     &              ,IGPFIS,GSPFIS,GSLPFI,GZINPF,GSTPPF
     &              ,IGDRAY,GSDRAY,GSLDRA,GZINDR,GSTPDR
     &              ,IGANNI,GSANNI,GSLANN,GZINAN,GSTPAN
     &              ,IGBREM,GSBREM,GSLBRE,GZINBR,GSTPBR
     &              ,IGHADR,GSHADR,GSLHAD,GZINHA,GSTPHA
     &              ,IGMUNU,GSMUNU,GSLMUN,GZINMU,GSTPMU
     &              ,IGDCAY,GSDCAY,GSLIFE,GSUMLI,GDPHY1
     &              ,IGLOSS,GSLOSS,GSOLOS,GSTLOS,GDPHY2
     &              ,IGMULS,GSMULS,GSOMUL,GSTMUL,GDPHY3
     &              ,IGRAYL,GSRAYL,GSLRAY,GZINRA,GSTPRA
C
      COMMON/GCNUM/NGMATE,NGVOLU,NGROTM,NGTMED,NGTMUL,NGTRAC,NGPART
     +     ,NGSTMA,NGVERT,NGHEAD,NGBIT
      COMMON/GCNUMX/ NGALIV,NGTMST
C
      DATA IFI /0/
C.
C ----------------------------------------------------------------------
      IF (NAPART.EQ.0) THEN
         CALL ALTELL ('ASGPAR : NO PARTicle data bank ',0,'STOP')
      ENDIF
C
C - 'PART' : # of words/particle, # of particles and then
C            GEANT# (I), name(3words), tracking type (I), mass, charge,
C            time of life
      KPART = IW(NAPART)
      LP1   = IW(KPART+1)
      LP2   = IW(KPART+2)
      KP    = KPART + LMHLEN
      LROW = MIN (LP2,NGPART)
      DO 1 I=1,LROW
         IF (IFI.GT.0 .AND. IW(KP+5).EQ.NOTRKI) GOTO 1
         IF (IW(KP+5).EQ.NOTRKI) IFI=1
         NAME(1:4) = CHAINT(IW(KP+2))
         NAME(5:8) = CHAINT(IW(KP+3))
         NAME(9:13)= CHAINT(IW(KP+4))//'$'
         CALL GSPART (IW(KP+1),NAME,IW(KP+5),RW(KP+6),RW(KP+7)
     &               ,RW(KP+8),0,0)
 1       KP = KP + LP1
C
C - Define decay modes
C
      IF (IGDCAY.GT.0 .AND. IPROJO(2).GT.0) THEN
         KGDEC=NLINK('GDEC',0)
         IF (KGDEC.LE.0) CALL ALTELL ('AGSPAR: NO GDEC bank ',0,'STOP')
         LP1=IW(KGDEC+1)
         LP2=IW(KGDEC+2)
         KP=KGDEC+LMHLEN
         DO 10 I=1,LP2
            CALL GSDK(IW(KP+1),RW(KP+2),IW(KP+8))
            KP=KP+LP1
  10     CONTINUE
      ENDIF
C - Debug
      IF(IPRIJO(20).EQ.1) THEN
         CALL GPPART(0)
         IF (IPRIJO(19).EQ.1)  CALL PRGDEC
      ENDIF
C
      RETURN
      END
