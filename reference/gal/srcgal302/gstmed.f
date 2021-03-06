      SUBROUTINE GSTMED(KTMED,NATMED,NMAT,ISVOL,IFIELD,FIELDM,TMAXFD,
     +        DMAXMS,DEEMAX,EPSIL,STMIN,UBUF,NWBUF)
C --------------------------------------------------------------------
C - F.Ranjard - 911213      from GSTMED / GEANT314
C  put DMAXMS to a big value in case of GEANT version > or = 3.15
*                                                                     *
C!      Store tracking media parameters
*                                                                     *
*    Stores the  parameters of the tracking  medium ITMED in  the data*
*  structure JTMED.                                                   *
*  ITMED     tracking medium number 0<ITMED<100                       *
*  NATMED    tracking medium name (up to 20 characters ended by $)    *
*  NMAT      material number corresponding to ITMED                   *
*  ISVOL     =0 if not a sensitive volume                             *
*  IFIELD    = 0  if no magnetic field                                *
*            = -1  reserved for user decision in GUSWIM               *
*            = 1  tracking performed with GRKUTA                      *
*            = 2  tracking performed with GHELIX                      *
*            = 3  tracking performed with GHELX3                      *
*  FIELDM    maximum field value (in Kilogauss)                       *
*  TMAXFD    maximum  angle due  to field  permitted in  one step  (in*
*            degrees)                                                 *
*  DMAXMS    maximum displacement for multiple  scattering in one step*
*            (in cm)                                                  *
*  DEEMAX    maximum fractional energy loss in one step               *
*  EPSIL     tracking precision (in cm)                               *
*  STMIN     minimum step  due to energy  loss or  multiple scattering*
*            (in cm)                                                  *
*  UBUF      array of NWBUF additional parameters                     *
*  NWBUF                                                              *
*                                                                     *
*                                                                     *
*          The Tracking Medium data structure JTMED                   *
*          ----------------------------------------                   *
*                                                                     *
*                                         | JTMED                     *
*    NTMED           ITMED                v                           *
*     ..........................................................      *
*     |               | |                | | Standard Trac.media      *
*     ..........................................................      *
*                      | JT                                           *
*                      v                                              *
*                    ..........................                       *
*                    | 1 |                    |                       *
*                    .....                    |                       *
*                    | 2 |  Tracking medium   |                       *
*                    |...|                    |                       *
*                    | 3 |   Name             |                       *
*                    |...|                    |                       *
*                    | 4 |                    |                       *
*                    |...|                    |                       *
*                    | 5 |                    |                       *
*                    ..........................                       *
*                    | 6 |   NMAT             |                       *
*                    |...|....................|                       *
*                    | 7 |   ISVOL            |                       *
*                    |...|....................|                       *
*                    | 8 |   IFIELD           |                       *
*                    |...|....................|                       *
*                    | 9 |   FIELDM           |                       *
*                    |...|....................|                       *
*                    | 10|   TMAXFD           |                       *
*                    |...|....................|                       *
*                    | 11|   DMAXMS           |                       *
*                    |...|....................|                       *
*                    | 12|   DEEMAX           |                       *
*                    |...|....................|                       *
*                    | 13|   EPSIL            |                       *
*                    |...|....................|                       *
*                    | 14|   STMIN            |                       *
*                    |...|....................|                       *
*                    | 15|   User words ....  |                       *
*                    ..........................                       *
*   JT = LQ(JTMED-ITMED) pointer to tracking medium ITMED             *
*                                                                     *
*    ==>Called by : <USER>, UGEOM    ,<GXINT> GINC3                   *
*       Author    R.Brun  *********                                   *
*                                                                     *
***********************************************************************
      PARAMETER (KWBANK=69000,KWWORK=5200)
      COMMON/GCBANK/NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,FENDQ(16)
     +             ,LMAIN,LR1,WS(KWBANK)
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1))
      EQUIVALENCE (JCG,JGSTAT)
      COMMON/GCLINK/JDIGI ,JDRAW ,JHEAD ,JHITS ,JKINE ,JMATE ,JPART
     +      ,JROTM ,JRUNG ,JSET  ,JSTAK ,JGSTAT,JTMED ,JTRACK,JVERTX
     +      ,JVOLUM,JXYZ  ,JGPAR ,JGPAR2,JSKLT
C
      COMMON/GCCUTS/CUTGAM,CUTELE,CUTNEU,CUTHAD,CUTMUO,BCUTE,BCUTM
     +             ,DCUTE ,DCUTM ,PPCUTM,TOFMAX,GCUTS(5)
C
      COMMON/GCPHYS/IPAIR,SPAIR,SLPAIR,ZINTPA,STEPPA
     +             ,ICOMP,SCOMP,SLCOMP,ZINTCO,STEPCO
     +             ,IPHOT,SPHOT,SLPHOT,ZINTPH,STEPPH
     +             ,IPFIS,SPFIS,SLPFIS,ZINTPF,STEPPF
     +             ,IDRAY,SDRAY,SLDRAY,ZINTDR,STEPDR
     +             ,IANNI,SANNI,SLANNI,ZINTAN,STEPAN
     +             ,IBREM,SBREM,SLBREM,ZINTBR,STEPBR
     +             ,IHADR,SHADR,SLHADR,ZINTHA,STEPHA
     +             ,IMUNU,SMUNU,SLMUNU,ZINTMU,STEPMU
     +             ,IDCAY,SDCAY,SLIFE ,SUMLIF,DPHYS1
     +             ,ILOSS,SLOSS,SOLOSS,STLOSS,DPHYS2
     +             ,IMULS,SMULS,SOMULS,STMULS,DPHYS3
     +             ,IRAYL,SRAYL,SLRAYL,ZINTRA,STEPRA
*
      COMMON/GCUNIT/LIN,LOUT,NUNITS,LUNITS(5)
      INTEGER LIN,LOUT,NUNITS,LUNITS
      COMMON/GCMAIL/CHMAIL
      CHARACTER*132 CHMAIL
C
      COMMON/GCNUM/NMATE ,NVOLUM,NROTM,NTMED,NTMULT,NTRACK,NPART
     +            ,NSTMAX,NVERTX,NHEAD,NBIT
      COMMON /GCNUMX/ NALIVE,NTMSTO
C
      COMMON/GCMZFO/IOMATE,IOPART,IOTMED,IOSEJD,IOSJDD,IOSJDH,IOSTAK
     +             ,IOMZFO(13)
C
      INTEGER       IOMATE,IOPART,IOTMED,IOSEJD,IOSJDD,IOSJDH,IOSTAK
     +             ,IOMZFO
C
      PARAMETER (MAXMEC=30)
      COMMON/GCTRAK/VECT(7),GETOT,GEKIN,VOUT(7),NMEC,LMEC(MAXMEC)
     + ,NAMEC(MAXMEC),NSTEP ,MAXNST,DESTEP,DESTEL,SAFETY,SLENG
     + ,STEP  ,SNEXT ,SFIELD,TOFG  ,GEKRAT,UPWGHT,IGNEXT,INWVOL
     + ,ISTOP ,IGAUTO,IEKBIN, ILOSL, IMULL,INGOTO,NLDOWN,NLEVIN
     + ,NLVSAV,ISTORY
C
      DIMENSION MECA(5,13)
      EQUIVALENCE (MECA(1,1),IPAIR)
      DIMENSION UBUF(1)
      CHARACTER*(*) NATMED
      CHARACTER*20 NAME
C.
C.    ------------------------------------------------------------------
C.
      ITMED=ABS(KTMED)
      IF(JTMED.LE.0)THEN
         CALL MZBOOK(IXCONS,JTMED,JTMED,1,'TMED',NTMED,NTMED,25,3,0)
         CALL UCOPY(CUTGAM,Q(JTMED+1),10)
         IQ(JTMED-5)=0
         DO 10 I=1,13
            Q(JTMED+10+I)=MECA(1,I)
   10    CONTINUE
      ENDIF
      IF(ITMED.GT.NTMED)THEN
         CALL MZPUSH(IXCONS,JTMED,ITMED-NTMED,0,'I')
         NTMED=ITMED
         JTM1=0
      ELSE
         JTM1=LQ(JTMED-ITMED)
         IF(JTM1.GT.0) THEN
            WRITE(CHMAIL,10100)
            CALL GMAIL(1,0)
            CALL GPTMED(ITMED)
            CALL MZDROP(IXCONS,LQ(JTMED-ITMED),' ')
         ENDIF
      ENDIF
      NW=NWBUF+14
      CALL MZBOOK(IXCONS,JTM,JTMED,-ITMED,'TMED',10,10,NW,IOTMED,0)
C
      NAME=NATMED
      NCH=LENOCC(NAME)
      IF(NCH.GT.0)THEN
         IF(NAME(NCH:NCH).EQ.'$')NAME(NCH:NCH)=' '
      ENDIF
      CALL UCTOH(NAME,IQ(JTM+1),4,20)
C
      EPS=EPSIL
      IF(EPSIL.LE.0.0) THEN
               WRITE(CHMAIL,10000) EPSIL
         CALL GMAIL(0,0)
         EPS=1.E-4
      END IF
C ============================================================
CFLR - IGAUTO is set for GEANT 3.14 onwards
C ============================================================
      IF(GVERSN.GE.3.14 .AND.IGAUTO.NE.0.AND.ITMED.GT.0)THEN
         DE=-1.
         ST=-1.
      ELSE
         DE=DEEMAX
         ST=STMIN
      ENDIF
C ===============================================================
CFLR - Add protection for GEANT 315 version: the definition of DMAXMS
C      has changed to be STEMAX (maximum step size)
C      so if GEANT version > or = to 3.15 put DMAXMS to BIG
      IF (GVERSN .GE. 3.15) DMAXMS = 1.E9
C ==================================================================
      Q(JTM + 6) = NMAT
      Q(JTM + 7) = ISVOL
      Q(JTM + 8) = IFIELD
      Q(JTM + 9) = FIELDM
      Q(JTM + 10) = TMAXFD
      Q(JTM + 11) = DMAXMS
      Q(JTM + 12) = DE
      Q(JTM + 13) = EPS
      Q(JTM + 14) = ST
      IF(NWBUF.GT.0)CALL UCOPY(UBUF,Q(JTM+15),NWBUF)
C
      IF(JTM1.GT.0) THEN
         CALL GPTMED(-ITMED)
      ENDIF
C
10000 FORMAT('0*** GSTMED *** Warning, input value of EPSIL=',
     +       E10.3,' reset to 1 micron')
10100 FORMAT(' *** GSTMED ***: Warning, tracking medium redefinition:')
  999 CONTINUE
      END
