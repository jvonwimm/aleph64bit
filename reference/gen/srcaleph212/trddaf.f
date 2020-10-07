      SUBROUTINE TRDDAF (LBASE,IRUN,IRET)
C ----------------------------------------------------------------------
C. - M.Mermikides - 860921             modified by F.Ranjard - 880420
C!  Read DAF and Fill commom blocks TPGEOM,TPGEOP,TPGEOW,TPGEOT
C.   with useful TPC geometric quantities derived from data base
C
C - Input arguments:  LBASE   = DAF file logical unit #
C                     IRUN    = current run #
C
C - Output argument:  IRET    = return code ( = ALGTDB return value)
C                               = 0 means at least one bank is missing
C                               < 0 means at least one bank is new
C                               > 0 means no bank is new
      SAVE
C! define universal constants
      REAL PI, TWOPI, PIBY2, PIBY3, PIBY4, PIBY6, PIBY8, PIBY12
      REAL RADEG, DEGRA
      REAL CLGHT, ALDEDX
      PARAMETER (PI=3.141592653589)
      PARAMETER (RADEG=180./PI, DEGRA=PI/180.)
      PARAMETER (TWOPI = 2.*PI , PIBY2 = PI/2., PIBY4 = PI/4.)
      PARAMETER (PIBY6 = PI/6. , PIBY8 = PI/8.)
      PARAMETER (PIBY12= PI/12., PIBY3 = PI/3.)
      PARAMETER (CLGHT = 29.9792458, ALDEDX = 0.000307)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JTCGID=1,JTCGVR=2,JTCGCN=4,JTCGNS=5,JTCGNC=6,JTCGNE=7,
     +          JTCGAS=8,JTCGRS=9,JTCGPH=10,JTCGPS=11,JTCGPW=12,
     +          JTCGGW=13,JTCGEW=14,JTCGES=15,JTCGBD=16,JTCGTS=17,
     +          JTCGTH=18,JTCGWP=19,JTCGWD=20,JTCGTO=21,JTCGTT=24,
     +          JTCGWT=27,JTCGWE=28,JTCGWW=29,JTCGWK=30,JTCGFT=33,
     +          LTCGDA=33)
      PARAMETER(JTCROC=1,JTCRNC=2,JTCRN1=3,LTCRLA=3)
      PARAMETER(JTMTID=1,JTMTVR=2,JTMTMO=4,JTMTPP=5,JTMTRF=6,JTMTNP=7,
     +          JTMTPR=8,JTMTRT=20,JTMTNT=21,JTMTTR=22,JTMTAT=33,
     +          JTMTTC=34,JTMTPW=38,JTMTNW=39,JTMTEF=40,JTMTEL=41,
     +          JTMTWF=42,LTMTYA=45)
      PARAMETER(JTPCIN=1,JTPCRV=2,JTPCPH=3,JTPCZV=4,JTPCSR=5,JTPCSZ=6,
     +          JTPCOF=7,JTPCTN=8,JTPCCN=9,JTPCIT=10,JTPCRR=11,
     +          JTPCRZ=12,LTPCOA=12)
      PARAMETER(JTPHIT=1,LTPHEA=1)
      PARAMETER(JTPHKT=1,JTPHCI=2,JTPHPH=3,JTPHZV=4,JTPHDD=5,JTPHDZ=6,
     +          LTPHTA=6)
      PARAMETER(JTPTKT=1,JTPTXA=2,JTPTYA=3,JTPTZA=4,JTPTDX=5,JTPTDY=6,
     +          JTPTDZ=7,JTPTPV=8,JTPTLN=9,JTPTTF=10,JTPTPM=11,
     +          JTPTCH=12,LTPTEA=12)
      PARAMETER(JTSGID=1,JTSGVR=2,JTSGNC=4,JTSGXC=5,JTSGYC=10,JTSGTM=15,
     +          LTSGMA=15)
      PARAMETER(JTSLID=1,JTSLVR=2,JTSLSN=4,JTSLSB=5,JTSLSS=6,JTSLDS=7,
     +          JTSLAS=8,JTSLRS=9,JTSLTM=10,JTSLTS=11,LTSLOA=11)
      PARAMETER(JTTHIT=1,LTTHEA=1)
      PARAMETER(JTTHKT=1,JTTHCI=2,JTTHPH=3,JTTHZV=4,JTTHDD=5,JTTHDZ=6,
     +          LTTHTA=6)
      PARAMETER(JTSCID=1,JTSCVR=2,JTSCSN=4,JTSCNS=5,JTSCRP=6,JTSCAZ=9,
     +          JTSCAX=10,JTSCAY=11,JTSCSG=12,JTSCTC=13,LTSCOA=13)
      PARAMETER(JT1FID=1,JT1FVR=2,JT1FLL=4,JT1FUL=5,JT1FSS=6,JT1FNP=7,
     +          LT1FCA=7)
      PARAMETER(JT2FID=1,JT2FVR=2,JT2FR1=4,JT2FR2=916,JT2FP1=1828,
     +          JT2FP2=2740)
      PARAMETER (LTPDRO=21,LTTROW=19,LTSROW=12,LTWIRE=200,LTSTYP=3,
     +           LTSLOT=12,LTCORN=6,LTSECT=LTSLOT*LTSTYP,LTTPAD=4,
     +           LMXPDR=150,LTTSRW=11)
      COMMON /TPGEOM/RTPCMN,RTPCMX,ZTPCMX,DRTPMN,DRTPMX,DZTPMX,
     &               TPFRDZ,TPFRDW,TPAVDZ,TPFOF1,TPFOF2,TPFOF3,
     &               TPPROW(LTPDRO),TPTROW(LTTROW),NTSECT,NTPROW,
     &               NTPCRN(LTSTYP),TPCORN(2,LTCORN,LTSTYP),
     &               TPPHI0(LTSECT),TPCPH0(LTSECT),TPSPH0(LTSECT),
     &               ITPTYP(LTSECT),ITPSEC(LTSECT),IENDTP(LTSECT)
C
C
      COMMON /TPGEOP/ NTPDRW(LTSTYP),NTPDPR(LTSROW,LTSTYP),
     &                TPDRBG(LTSTYP),TPDRST(LTSTYP),TPDHGT(LTSTYP),
     &                TPDSEP(LTSTYP),TPDWID(LTSTYP),TPDHWD(LTSTYP),
     &                TPDPHF(LTSROW,LTSTYP),TPDPHW(LTSROW,LTSTYP),
     &                TPDPHS(LTSROW,LTSTYP)
C
C
      COMMON /TPGEOW/ TWSTEP(LTSTYP),TWIRE1(LTSTYP),NTWIRE(LTSTYP),
     &                TWIRMN(LTWIRE,LTSTYP),TWIRMX(LTWIRE,LTSTYP),
     &                TWIRLE(LTWIRE,LTSTYP),ITLWIF(LTSTYP),
     &                ITLWIL(LTSTYP),NTREG1(4,LTSTYP),TFRATH
C
      COMMON /TPGEOT/ NTPTRW(LTSTYP),NTPTPR(LTTSRW,LTSTYP),
     &                TPTRBG(LTSTYP),TPTRST(LTSTYP),TPTRHG(LTSTYP),
     &                TPTPHC(LTTPAD,LTTSRW,LTSTYP),
     &                TPTPHW(LTTPAD,LTTSRW,LTSTYP),
     &                ITPADG(LTTPAD,LTTSRW,LTSTYP)
C
C
      LOGICAL FIRST
      EXTERNAL NAMIND, ALGTDB
      INTEGER ALGTDB
      DIMENSION ANFIR(LTSTYP),ANTPA(LTSTYP)
      DIMENSION ITPD1 (LTTPAD,LTSTYP)
      DATA FIRST /.TRUE./
C
C!    set of intrinsic functions to handle BOS banks
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
C ---------------------------------------------------------------------
      NTPROW = LTPDRO
      NTSECT = LTSECT
C
      IRET = ALGTDB (LBASE,'TPOSTCGDTSLOTMTYTSGMTSCO',IRUN)
C - IF banks are missing (IRET=0) OR banks are still valid
C      and it is not the 1st entry (IRET>0 and .NOT.FIRST) RETURN
      IF ((IRET.EQ.0) .OR. (IRET.GT.0 .AND. .NOT.FIRST)) RETURN
      FIRST = .FALSE.
C
C  +------------------------------------------------------------------+
C  I                                                                  I
C  I           'TSGM':             <<<  Sector geometry >>>           I
C  I           -------             ------------------------           I
C  I                                                                  I
C  I  Generate the constants describing                               I
C  I               the corners of the 3 sector - types                I
C  +------------------------------------------------------------------+
C
      KTSGM = IW(NAMIND('TSGM'))
C
      IF (KTSGM .GT. 0) THEN
C
         DO 40 IS = 1, LTSTYP
            NTPCRN (IS) = ITABL (KTSGM,IS,JTSGNC)
            DO 30 ICRN = 1,5
               TPCORN (1,ICRN,IS) = RTABL (KTSGM,IS,JTSGYC-1+ICRN)
               TPCORN (2,ICRN,IS) = RTABL (KTSGM,IS,JTSGXC-1+ICRN)
   30       CONTINUE
   40    CONTINUE
C
      ENDIF
C
C  +------------------------------------------------------------------+
C  I                                                                  I
C  I           'TCGD':             <<<  General sector constants >>>  I
C  I           -------             ---------------------------------  I
C  I                                                                  I
C  I  Generate the constants describing                               I
C  I           the geometrical constants valid for all sector types   I
C  I                                                                  I
C  +------------------------------------------------------------------+
C
      KTCGD = IW(NAMIND('TCGD'))
C
      IF (KTCGD .GT. 0) THEN
C
C Angular spacing between sectors of the same type (nominally = pi/3)
         ANGSP = RTABL (KTCGD,1,JTCGAS)
C  Distance of edgde of first pad from sector border
         BORDS = RTABL (KTCGD,1,JTCGBD)
C
C++     Fill general constants for pads into   /TPGEOP/
C
         DO 220 IS = 1,LTSTYP
            TPDRST (IS) = RTABL (KTCGD,1,JTCGRS)
            TPDHGT (IS) = RTABL (KTCGD,1,JTCGPH)
            TPDSEP (IS) = RTABL (KTCGD,1,JTCGPS)
            TPDWID (IS) = RTABL (KTCGD,1,JTCGPW)
            TPDHWD (IS) = RTABL (KTCGD,1,JTCGEW)
C
C         General constants for trigger - pads into   /TPGEOT/
C
            TPTRST (IS) = RTABL (KTCGD,1,JTCGTS)
            TPTRHG (IS) = RTABL (KTCGD,1,JTCGTH)
C
C         General constants for wires into   /TPGEOW/
C
            TWSTEP (IS) = RTABL (KTCGD,1,JTCGWP)
C
  220    CONTINUE
C
C     Wall thickness of TPC
C
          DRTPMN      = RTABL (KTCGD,1,JTCGTT)
          DRTPMX      = RTABL (KTCGD,1,JTCGTT+1)
          DZTPMX      = RTABL (KTCGD,1,JTCGTT+2)
C
C     Compute inner dimensions (sensitive volume)
C
          RTPCMN = RTABL (KTCGD,1,JTCGTO) + DRTPMN
          RTPCMX = RTABL (KTCGD,1,JTCGTO+1) - DRTPMX
          ZTPCMX = RTABL (KTCGD,1,JTCGTO+2) - DZTPMX
C
C    Wheel rib - thickness, rib - width and equivalent thickness
C
          TPFRDZ      = RTABL (KTCGD,1,JTCGWT)
          TPFRDW      = RTABL (KTCGD,1,JTCGWW)
          TPAVDZ      = RTABL (KTCGD,1,JTCGWE)
C
C    Wheel rib offsets (describing the kinks in the ribs)
C
          TPFOF1      = RTABL (KTCGD,1,JTCGWK)
          TPFOF2      = RTABL (KTCGD,1,JTCGWK+1)
          TPFOF3      = RTABL (KTCGD,1,JTCGWK+2)
C
C    Thickness of frame on which wires are mounted
C
          TFRATH      = RTABL (KTCGD,1,JTCGFT)
       ENDIF
C
C  +------------------------------------------------------------------+
C  I                                                                  I
C  I           'TMTY':             <<<  Sector - types >>>            I
C  I           -------             -----------------------            I
C  I                                                                  I
C  I  Generate the constants describing                               I
C  I       the geometry of the sensitive elements (pads, wires, tpads)I
C  I                                                                  I
C  +------------------------------------------------------------------+
C
      KTMTY = IW(NAMIND('TMTY'))
C
      IF (KTMTY .GT. 0) THEN
         DO 80 IS = 1, LTSTYP
            ANFIR (IS)      = RTABL (KTMTY,IS,JTMTPP)
C      radius of 1. pad row:
            TPDRBG (IS)     = RTABL (KTMTY,IS,JTMTRF)
C      number of padrows
            NTPDRW (IS)     = ITABL (KTMTY,IS,JTMTNP)
C      number of pads in each padrow
            DO  50 IROW = 1,12
               NTPDPR (IROW,IS) = ITABL (KTMTY,IS,JTMTPR-1+IROW)
   50       CONTINUE
C
C      radius of 1. tpad row:
            TPTRBG (IS)      = RTABL (KTMTY,IS,JTMTRT)
C      number of tpadrows
            NTPTRW (IS)      = ITABL (KTMTY,IS,JTMTNT)
C
C      number of tpads in each tpadrow
            DO 150 IROW = 1,11
               NTPTPR (IROW,IS) = ITABL (KTMTY,IS,JTMTTR-1+IROW)
  150       CONTINUE
C
            ANTPA(IS) = RTABL (KTMTY,IS,JTMTAT)
C  Trigger pad numbering
            ITPD1(1,IS) = ITABL (KTMTY,IS,JTMTTC)
            ITPD1(2,IS) = ITABL (KTMTY,IS,JTMTTC+1)
            ITPD1(3,IS) = ITABL (KTMTY,IS,JTMTTC+2)
            ITPD1(4,IS) = ITABL (KTMTY,IS,JTMTTC+3)
C      radius of center of 1. wire, # of wires:
            TWIRE1 (IS) = RTABL (KTMTY,IS,JTMTPW)
            NTWIRE (IS) = ITABL (KTMTY,IS,JTMTNW)
C      wire pointers to first and last el. channel
            ITLWIF (IS) = ITABL (KTMTY,IS,JTMTEF)
            ITLWIL (IS) = ITABL (KTMTY,IS,JTMTEL)
            DO 78 IREG = 1,4
   78       NTREG1 (IREG,IS) = ITABL (KTMTY,IS,JTMTWF+IREG-1)
C
   80    CONTINUE
C
C++     compute radii of padrows
C
         DO  70 IS = 1,9
            TPPROW (IS) = TPDRBG (1) + (IS- 1) * TPDRST (1)
   70    CONTINUE
         DO  75 IS = 10,21
            TPPROW (IS) = TPDRBG (2) + (IS-10) * TPDRST (2)
   75    CONTINUE
C
C++     Compute angular pad-width, PHI of sector-frame
C++     and leading pad edge
C
         DO 88 I=1,LTSROW*LTSTYP
            TPDPHS(I,1) = 0.
            TPDPHW(I,1) = 0.
            TPDPHF(I,1) = 0.
   88    CONTINUE
C
         DO 91 IS = 1,LTSTYP
            DO  90 IROW = 1, NTPDRW (IS)
               RAD = TPDRBG (IS) + (IROW-1) * TPDRST (IS)
               PHI = TPDSEP (IS) / RAD
               FPADS = FLOAT (NTPDPR (IROW,IS))
               TPDPHS (IROW,IS) = PHI
               TPDPHW (IROW,IS) = 0.5 * PHI * FPADS
               TPDPHF (IROW,IS) = 0.5 * PHI
     +                * (FPADS + 2.*(BORDS/TPDSEP(IS)))
   90       CONTINUE
   91    CONTINUE
C
C++     Compute PHI of edges, half-width and PHI
C++     of centers of Triggerpads
C
         DO 112 IS   = 1, LTSTYP
            DO 111 IROW = 1, NTPTRW(IS)
               P1 = TPDPHW(IROW,IS)
               IF (NTPTPR(IROW,IS).EQ.2) THEN
                  TPTPHC(1,IROW,IS) = -P1/2.
                  TPTPHC(2,IROW,IS) =  P1/2.
                  TPTPHW(1,IROW,IS) =  P1/2.
                  TPTPHW(2,IROW,IS) =  P1/2.
               ELSE
                  P2 = ANTPA (IS)
                  TPTPHC(1,IROW,IS) = - (P1+P2)/2.
                  TPTPHC(2,IROW,IS) = - P2/2.
                  TPTPHC(3,IROW,IS) = - TPTPHC(2,IROW,IS)
                  TPTPHC(4,IROW,IS) = - TPTPHC(1,IROW,IS)
                  TPTPHW(1,IROW,IS) =   (P1-P2)/2.
                  TPTPHW(2,IROW,IS) =   P2/2.
                  TPTPHW(3,IROW,IS) =   TPTPHW(2,IROW,IS)
                  TPTPHW(4,IROW,IS) =   TPTPHW(1,IROW,IS)
               ENDIF
C
               DO 110 IT   = 1,LTTPAD
C
C++        Create numbering scheme of the triggerpads
C
                  ITPADG(IT,IROW,IS) = IROW + ITPD1(IT,IS) - ITPD1(1,IS)
                  IF (IS.EQ.1.OR.IT.EQ.1) GO TO 110
                  IF (IROW .GT. 2*ITPD1(1,IS) - ITPD1(2,IS) ) GO TO 110
C  Situation of rows with 2 tpads
                  IF (IT.EQ.2) THEN
                     ITPADG(IT,IROW,IS) =IROW+ ITPD1(4,IS) - ITPD1(1,IS)
                  ELSE
                     ITPADG(IT,IROW,IS) = 0
                  ENDIF
  110          CONTINUE
  111       CONTINUE
  112    CONTINUE
C
C++     compute radii of tpadrows
C
         DO 170 IS = 1, 8
            TPTROW (IS) = TPTRBG (1) + (IS- 1) * TPTRST (1)
  170    CONTINUE
         DO 175 IS = 9,19
            TPTROW (IS) = TPTRBG (2) + (IS- 9) * TPTRST (2)
  175    CONTINUE
      ENDIF
C
C  +------------------------------------------------------------------+
C  I                                                                  I
C  I            'TSLO':             <<<  Slot positions >>>           I
C  I            -------             -----------------------           I
C  I                                                                  I
C  I  Generate the constants describing the slots in both endplates.  I
C  I                   (A "slot" is a sector position in the wheel.)  I
C  I                                                                  I
C  +------------------------------------------------------------------+
C
      KTSLO = IW(NAMIND('TSLO'))
      IF (KTSLO .GT. 0) THEN
C
         DO  20 IS = 1, LTSTYP*LTSLOT
            ICHT           = ITABL (KTSLO,IS,JTSLSN)
            ISLOT          = ITABL (KTSLO,IS,JTSLSB)
            ITPSEC (ISLOT) = ITABL (KTSLO,IS,JTSLSS)
            ITPTYP (ISLOT) = ITABL (KTSLO,IS,JTSLTM)
            IENDTP (ISLOT) = ITABL (KTSLO,IS,JTSLTS)
C
            TPPHI0 (ISLOT) = ANFIR (ITPTYP (ISLOT)) +
     +                     ANGSP * MOD (ITPSEC (ISLOT) -1, 6)
            TPCPH0 (ISLOT) = COS (TPPHI0(ISLOT))
            TPSPH0 (ISLOT) = SIN (TPPHI0(ISLOT))
   20    CONTINUE
      ENDIF
C
C++  Calculate wire limits
C
      DO 89 I=1,LTWIRE*LTSTYP
         TWIRMN(I,1) = 0.
         TWIRMX(I,1) = 0.
         TWIRLE(I,1) = 0.
   89 CONTINUE
C
      CALL TGEWIR
C
C - Establish TPC alignment matrices in /TALIGN/ using 'TSCO'
C
       IF (NAMIND('TSCO') .GT. 0) THEN
         CALL TALINI
       ENDIF
C
      RETURN
      END
