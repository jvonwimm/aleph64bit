      SUBROUTINE TWRRED(IER)
C
C----------------------------------------------------------------------
C! Apply the TPP wire reduction algorithm to the data
C  This routine is only used in case NO wire reduction has been
C  done in the TPP (or in TPCSIM for MC).  JULIA common blocks are
C  not used, so this routine can be called from GALEPH as well as from
C  JULIA.
CKEY TPC WIRE REDUCTION
C
C  R. Johnson   4-12-90
C  modified by : F.Ranjard - 910514 - get the run number from RUNH
C                                     or EVEH
C                          - 910516 - do not put output banks
C                                     on T-list, give a format
C  modified by : W.Wiedenmann - 920306 - write new wire banks TSDI,TSIR
C                                        in replacement of TRDI, TRIR
C  modified by : R.Johnson - 930202 - fix bug in production of TSDI bank
C
C  Output:   IER     /I       Error code
C                             1= no run number found
C                             2= cannot find bank database banks
C                             3= TWDI digitization bank missing
C                             4= sector number out of range
C                             5= BOS ran out of space
C                             6= No pedestal info found
C                             7= cannot find bank TSWP on database
C
C  Input banks:      TWIR,TWDI         Raw data
C                    TSWP,TWRC         Parameters for algorithm
C  Output banks:     TSIR,TSDI         Reduced raw data
C----------------------------------------------------------------------
C
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JTWRID=1,JTWRVR=2,JTWRMV=4,JTWRTP=5,JTWRMS=6,JTWRRM=7,
     +          JTWRAL=8,JTWRSD=9,JTWRSZ=10,LTWRCA=10)
      PARAMETER(JTSWML=1,JTSWMX=2,JTSWPS=3,JTSWPO=4,JTSWPD=5,JTSWTH=6,
     +          JTSWMA=7,JTSWMS=8,JTSWCN=9,JTSWR1=10,JTSWR2=11,
     +          JTSWR3=12,JTSWR4=13,JTSWR5=14,JTSWR6=15,JTSWR7=16,
     +          LTSWPA=16)
      PARAMETER(JRUNEN=1,JRUNRN=2,JRUNRT=3,JRUNSD=4,JRUNST=5,LRUNHA=5)
      PARAMETER(JEVEEN=1,JEVERN=2,JEVERT=3,JEVEDA=4,JEVETI=5,JEVEEV=6,
     +          JEVEM1=7,JEVEM2=8,JEVEM3=9,JEVEM4=10,JEVETY=11,
     +          JEVEES=12,JEVETE=13,LEVEHA=13)
      PARAMETER (LTPDRO=21,LTTROW=19,LTSROW=12,LTWIRE=200,LTSTYP=3,
     +           LTSLOT=12,LTCORN=6,LTSECT=LTSLOT*LTSTYP,LTTPAD=4,
     +           LMXPDR=150,LTTSRW=11)
      PARAMETER(JTCLCD=1,JTCLCT=2,JTCLNT=3,JTCLPD=4,JTCLPT=5,JTCLWT=6,
     +          JTCLPL=7,JTCLWL=8,LTCLBA=8)
      PARAMETER(JTSITV=1,JTSIMX=2,JTSIIT=3,JTSINS=4,JTSINF=5,JTSINP=6,
     +          JTSINC=7,JTSILT=8,JTSINR=9,JTSINO=10,JTSIMI=11,
     +          JTSILH=12,JTSINE=13,JTSINT=14,JTSIMN=15,JTSINW=16,
     +          JTSIDV=17,JTSISA=18,JTSISR=19,JTSITA=20,JTSITD=21,
     +          JTSIAM=22,JTSICU=23,JTSIFF=24,JTSISW=25,JTSISH=26,
     +          JTSIHX=27,JTSIEF=28,JTSISI=29,JTSISC=30,JTSIRX=31,
     +          JTSITS=32,JTSIPE=33,JTSISP=34,JTSISG=35,JTSISD=36,
     +          JTSIWN=37,JTSIPN=38,JTSITN=39,JTSICF=40,JTSIWS=41,
     +          JTSITZ=42,JTSIIL=43,JTSIIG=44,LTSIMA=44)
      COMMON /TWRPRM/ NPREWP,NPSTWP,IPEDWP,MXSTWP,RTHRWP,JPCKWP,
     &                JTHRWP,MNLNWP,MXLNWP,IALGWP,TPCKWP,MXABWP
C
      SAVE
      PARAMETER (MXSMP=50,MNSMP=5,MXEPR=5)
      DIMENSION IPHSV(MXSMP),JPEDS(LTSECT)
      INTEGER ALGTDB
      LOGICAL FIRST,FIRPD
      DATA FIRST/.TRUE./,NERPR/0/,FIRPD/.TRUE./
      DIMENSION IBTOR(0:7)
      DATA IBTOR/24,28,16,20,8,12,0,4/
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
      IF (FIRST) THEN
        FIRST=.FALSE.
        NRUNH=NAMIND('RUNH')
        NEVEH=NAMIND('EVEH')
        NTWIR=NAMIND('TWIR')
        NTWDI=NAMIND('TWDI')
        NTSIR=NAMIND('TSIR')
        CALL BKFMT ('TSIR','I')
        NTSDI=NAMIND('TSDI')
        CALL BKFMT ('TSDI','I')
        NTSIM=NAMIND('TSIM')
        NTCLB=NAMIND('TCLB')
        NTWPD=NAMIND('TWPD')
        NTSWP=NAMIND('TSWP')
        NTWRC=NAMIND('TWRC')
        NTSLE=NAMIND('TSLE')
        CALL BKFMT('TSLE','I')
        IRLST=-1
      ENDIF
      IER=0
C
C++   Get the run number
C
      KRUNH=IW(NRUNH)
      IF (KRUNH.NE.0) THEN
         IRUN = IW(KRUNH+JRUNRN)
      ELSE
         KEVEH=IW(NEVEH)
         IF (KEVEH.EQ.0) THEN
            IER=1
            RETURN
         ELSE
            IRUN=IW(KEVEH+JEVERN)
         ENDIF
      ENDIF
C
C++   Look for the constants banks (read from dbase if necessary)
C
      KTWRC=IW(NTWRC)
      IF (KTWRC.EQ.0) THEN
        IRET=ALGTDB(JUNIDB(0),'TWRC',IRUN)
        IF (IRET.EQ.0) THEN
          IER=2
          RETURN
        ENDIF
        KTWRC=IW(NTWRC)
      ELSE
        IF (IRUN.LT.ITABL(KTWRC,1,JTWRVR)
     &         .OR. IRUN.GT.ITABL(KTWRC,1,JTWRVR+1)) THEN
          IRET=ALGTDB(JUNIDB(0),'TWRC',IRUN)
          IF (IRET.EQ.0) THEN
            IER=2
            RETURN
          ENDIF
          KTWRC=IW(NTWRC)
        ENDIF
      ENDIF
C
      KTSWP=IW(NTSWP)
      IF (KTSWP.EQ.0) THEN
        NR=NDANR(JUNIDB(0),'TSWP','LE',IRUN)
        IF (NR.EQ.0) THEN
          IER=2
          RETURN
        ENDIF
        IRLST=IRUN
        KTSWP=MDARD(IW,JUNIDB(0),'TSWP',NR)
      ELSEIF (IRUN.NE.IRLST) THEN
        IF (IW(KTSWP-2).NE.IRUN .AND. IW(KTSWP-2).GE.0) THEN
          NR=NDANR(JUNIDB(0),'TSWP','LE',IRUN)
          IF (NR.NE.0 .AND. NR.NE.IW(KTSWP-2)) THEN
            KTSWP=MDARD(IW,JUNIDB(0),'TSWP',NR)
          ENDIF
        ENDIF
        IRLST=IRUN
      ENDIF
C
C++   Get the pedestals
C
      KTWPD=IW(NTWPD)
      IF (KTWPD.NE.0) THEN
C
C++     Set by user's data card TWPD
C
        DO 5 I=1,LTSECT
          JPEDS(I)=IW(KTWPD+1)
    5   CONTINUE
        IF (FIRPD) THEN
          FIRPD=.FALSE.
          IF (IW(6).NE.0) WRITE(IW(6),371) JPEDS(1)
  371     FORMAT(' TWRRED: TPC wire pedestal set to ',I4,
     &           ' ADC counts from user''s TWPD card.')
        ENDIF
      ELSE
        KTSIM=IW(NTSIM)
        IF (KTSIM.NE.0) THEN
C
C++       Monte Carlo pedestals
C
          DO 6 I=1,LTSECT
            PED = RTABL(KTSIM,1,JTSIPE)
            JPEDS(I) = NINT(255.*PED/2000.)
    6     CONTINUE
        ELSE
C
C++       For real data, look for the TCLB bank
C
          KTCLB=IW(NTCLB)
          IF (KTCLB.NE.0) THEN
            DO 7 I=1,LROWS(KTCLB)
              JPEDS(I)=ITABL(KTCLB,I,JTCLPD)
    7       CONTINUE
          ELSE
            IER=6
            RETURN
          ENDIF
        ENDIF
      ENDIF
C
C++   Fill the TWRPRM common block with cuts and parameters
C
      MXSTWP=ITABL(KTSWP,1,JTSWMS)
      JPCKWP=ITABL(KTSWP,1,JTSWCN)
      JTHRWP=ITABL(KTSWP,1,JTSWTH)
      MNLNWP=ITABL(KTSWP,1,JTSWML)
      MXLNWP=MIN(MXSMP,ITABL(KTSWP,1,JTSWMX))
      MXABWP=ITABL(KTSWP,1,JTSWMA)
      NPREWP=ITABL(KTSWP,1,JTSWPS)
      NPSTWP=ITABL(KTSWP,1,JTSWPO)
      TPCKWP=RTABL(KTWRC,1,JTWRTP)
      IALGWP=ITABL(KTSWP,1,JTSWR1)
      RTHRWP=RTABL(KTSWP,1,JTSWR2)
C
      LPROWP=ITABL(KTSWP,1,JTSWR4)
C
C++   Are there any unreduced pulses?
C
      KTWIR=IW(NTWIR)
      IF (KTWIR.EQ.0) RETURN
      KTWDI=IW(NTWDI)
      IF (KTWDI.EQ.0) THEN
        IER=3
        RETURN
      ENDIF
C
C++   Drop the output banks if they already exist
C
      KTSIR=IW(NTSIR)
      IF (KTSIR.NE.0) CALL BDROP(IW,'TSIR')
      KTSDI=IW(NTSDI)
      IF (KTSDI.NE.0) CALL BDROP(IW,'TSDI')
      KTSLE=IW(NTSLE)
      IF (KTSLE.NE.0) CALL BDROP(IW,'TSLE')
C
C++   Loop over the input banks to be reduced
C
      KTWIR=IW(NTWIR)
  100 IF (KTWIR.EQ.0) GO TO 800
        ISLOT=IW(KTWIR-2)
        IF (ISLOT.LE.0 .OR. ISLOT.GT.LTSECT) THEN
          IER=4
          KTWIR=IW(KTWIR-1)
          GO TO 100
        ENDIF
        NWHIT=IW(KTWIR)
        IF (NWHIT.LE.0) THEN
          KTWIR=IW(KTWIR-1)
          GO TO 100
        ENDIF
C
C++     Store the pedestal for this sector
C
        IPEDWP=JPEDS(ISLOT)
C
C++     Create output banks for this sector
C
        IW(1)=1
        CALL AUBOS('TSIR',ISLOT,NWHIT,KTSIR,IGARB)
        IF (IGARB.EQ.2) THEN
          IER=5
          RETURN
        ENDIF
        IF (IGARB.NE.0) THEN
          KTWIR=NLINK('TWIR',ISLOT)
        ENDIF
        KTWDI=NLINK('TWDI',ISLOT)
        IF (KTWDI.EQ.0) THEN
          IF (NERPR.LT.MXEPR) THEN
            NERPR=NERPR+1
            CALL ALTELL('TWRRED: TWDI bank missing.  Skip sector.',
     &                      0,'RETURN')
            KTSIR=NDROP('TSIR',ISLOT)
          ENDIF
          KTWIR=IW(KTWIR-1)
          GO TO 100
        ENDIF
        IW(1)=1
        CALL AUBOS('TSDI',ISLOT,IW(KTWDI),KTSDI,IGARB)
        IF (IGARB.EQ.2) THEN
          IER=5
          RETURN
        ENDIF
        IF (IGARB.NE.0) THEN
          KTWIR=NLINK('TWIR',ISLOT)
          KTWDI=NLINK('TWDI',ISLOT)
          KTSIR=NLINK('TSIR',ISLOT)
        ENDIF
        IW(1)=1
        LEN=(IW(KTWIR)-1)/8 + 1
        IF (MOD(IW(KTWIR),8).EQ.0) LEN=LEN+1
        CALL AUBOS('TSLE',ISLOT,LEN,KTSLE,IGARB)
        IF (IGARB.EQ.2) THEN
          IER=5
          RETURN
        ENDIF
        IF (IGARB.NE.0) THEN
          KTWIR=NLINK('TWIR',ISLOT)
          KTWDI=NLINK('TWDI',ISLOT)
          KTSIR=NLINK('TSIR',ISLOT)
          KTSDI=NLINK('TSDI',ISLOT)
        ENDIF
C
C++     Initialize pointers into the digitization banks
C
        IWORD=KTWDI+1
        IBIT0=24
C
        IWRD2=KTSDI+1
        IBT02=24
C
C++     Loop over all pulses in this sector
C
        DO 500 IH=1,NWHIT
          IWHTL=IW(KTWIR+IH)
          NWSMP=IBITS(IWHTL,16,8)
          IF (NWSMP.GT.MXLNWP) GO TO 400
          IF (NWSMP.LT.MNLNWP) GO TO 400
C
          IT0=IBITS(IWHTL,0,9)
          IWIR=IBITS(IWHTL,24,8)
C
C++       Unpack all of the digitizations
C
          DO 250 IS=1,NWSMP
            IPHSV(IS)=IBITS(IW(IWORD),IBIT0,8)
            IBIT0=IBIT0-8
            IF (IBIT0.LT.0) THEN
              IWORD=IWORD+1
              IBIT0=24
            ENDIF
  250     CONTINUE
C
C++       Call routine to analyze the pulse
C
          IF (IALGWP.EQ.1) THEN
            CALL TWPANA(IT0,NWSMP,IPHSV,ICHG,TIM,IERP)
            NSOT=3
          ELSE
            CALL TWPOLD(IT0,NWSMP,IPHSV,ICHG,TIM,NSOT,IERP)
          ENDIF
          IF (IERP.NE.0) THEN
C
C++         Pulse rejected by TWPANA; do not reduce it
C
            IFLG=0
            CALL MVBITS(IFLG,0,1,IWHTL,13)
            IW(KTSIR+IH)=IWHTL
C
C++         Pack all of the digitizations in the new bank
C
            DO 252 IS=1,NWSMP
              CALL MVBITS(IPHSV(IS),0,8,IW(IWRD2),IBT02)
              IBT02=IBT02-8
              IF (IBT02.LT.0) THEN
                IWRD2=IWRD2+1
                IBT02=24
              ENDIF
  252       CONTINUE
            GO TO 500
          ENDIF
C
C++       The charge is divided by JPCKWP to pack into 10 bits
C
          ICHG=ICHG/JPCKWP
C
C++       The z position is multiplied by 8 before packing
C
          IBUCK= MIN(8191,INT(TIM*TPCKWP))
C
C++       Bit-pack the results and store in the new bank
C
          CALL MVBITS(IBUCK,0,13,IWHTL,0)
          IFLG=1
          CALL MVBITS(IFLG,0,1,IWHTL,13)
          CALL MVBITS(ICHG,0,10,IWHTL,14)
          CALL MVBITS(IWIR,0,8,IWHTL,24)
          IW(KTSIR+IH)=IWHTL
C
C++       Save the pulse length (above threshold) in TSLE.  This is
C++       done ONLY for reduced pulses.
C
          IWRDS=KTSLE+1+(IH-1)/8
          IBTST=IBTOR(MOD(IH-1,8))
          CALL MVBITS(NSOT,0,4,IW(IWRDS),IBTST)
C
          GO TO 500
C
C++       Go here if the pulse is not to be reduced
C
  400     CONTINUE
C
C++       Set the reduce flag to zero and store the hit word
C
          IFLG=0
          CALL MVBITS(IFLG,0,1,IWHTL,13)
C
C++       Set the digitization flags in TSIR for TSDI if pulse
C++       is too long
C
          IF (NWSMP.GT.LPROWP) THEN
            IDIG1=1
            IDIG2=0
C
C++         Skip over the digitizations for this pulse
C
            DO 272 IS=1,NWSMP
              IBIT0=IBIT0-8
              IF (IBIT0.LT.0) THEN
                IWORD=IWORD+1
                IBIT0=24
              ENDIF
  272       CONTINUE
          ELSE
            IDIG1=0
            IDIG2=0
C
C++         Transfer the digitizations to the new bank
C
            DO 271 IS=1,NWSMP
              CALL MVBITS(IW(IWORD),IBIT0,8,IW(IWRD2),IBT02)
              IBIT0=IBIT0-8
              IF (IBIT0.LT.0) THEN
                IWORD=IWORD+1
                IBIT0=24
              ENDIF
              IBT02=IBT02-8
              IF (IBT02.LT.0) THEN
                IWRD2=IWRD2+1
                IBT02=24
              ENDIF
  271       CONTINUE
          ENDIF
          CALL MVBITS(IDIG1,0,1,IWHTL,14)
          CALL MVBITS(IDIG2,0,1,IWHTL,15)
          IW(KTSIR+IH)=IWHTL
  500   CONTINUE
C
C++     Set the length of the digitization bank
C
        IF (IBT02.EQ.24) THEN
          LEN=IWRD2-KTSDI-1
        ELSE
          LEN=IWRD2-KTSDI
        ENDIF
        CALL AUBOS('TSDI',ISLOT,LEN,KTSDI,IGARB)
        IF (IGARB.EQ.2) THEN
          IER=5
          RETURN
        ENDIF
        IF (IGARB.NE.0) THEN
          KTWIR=NLINK('TWIR',ISLOT)
        ENDIF
C
        KTWIR=IW(KTWIR-1)
        GO TO 100
  800 CONTINUE
C
      END
