      SUBROUTINE IXHITS(IRUN,NT,IV,IER)
C-----------------------------------------------------------------------
C! See if a track should have had a coord. in each ITC layer.
C!
C!    Author  :- I. Tomalin  89/06/24
C!    Modified:- J.Sedgbeer  89/10/12 Remove obsolete IHTD bank
C!    Modified:- J.Sedgbeer  91/01/07 Ensure correct IWST bank. Test
C!                                    r-phi coords only.
C!   Input:
C!     IRUN   /I4 : Run number.
C!     NT     /I4 : FRFT number of track.
C!     need common /BCS/ for FRFT bank.
C!     need common /ITWICC/ for ITC geom.
C!
C! IMPORTANT: If you wish to use this subroutine, then at the beginning
C!            of each run, you must call the ALEPHLIB routine, IRDDAF,
C!            to fill commons with the ITC geometry etc.
C!
C!   Output:
C!     IVi    /I4:  =1 if coordinate was expected in ITC layer i
C!                  =0 if it was not.
C!     IER    /I4:  =1 if FRFT bank was missing (and IV is not filled).
C!                  =0 if all O.K.
C!
C!   calls     : none
C!
C!   Libraries required: BOS
C!
C? This subroutine determines in which layers of the ITC, the track NT
C? should have had coordinates. This is done on the basis of whether the
C? track actually intersected each layer, and upon whether the wire at
C? the intersection point was "alive".
C!
C-----------------------------------------------------------------------
      SAVE
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER (JWIRIT=8,MWIRIT=960)
      COMMON/ITWICC/RWIRIT(JWIRIT),NWIRIT(JWIRIT),IWIRIT(JWIRIT),
     +              PHWRIT(JWIRIT),CELWIT(JWIRIT),WZMXIT,SGMXIT
      PARAMETER(JFRFIR=1,JFRFTL=2,JFRFP0=3,JFRFD0=4,JFRFZ0=5,JFRFAL=6,
     +          JFRFEM=7,JFRFC2=28,JFRFDF=29,JFRFNO=30,LFRFTA=30)
      PARAMETER(JIWSID=1,JIWSVR=2,JIWSIW=4,JIWSFL=5,JIWSIP=6,LIWSTA=6)
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
C-----------------------------------------------------------------------
      DIMENSION IV(*)
C-----------------------------------------------------------------------
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
C-----------------------------------------------------------------------
      IER=1
      KFRFT = NLINK('FRFT',0)
      IF (KFRFT.EQ.0) GOTO 999
      IER = 0
C Get the parameters of the track
      IFRFT = KROW(KFRFT,NT)
      R0 = 1.0/RW(IFRFT+JFRFIR)
      TANL = RW(IFRFT+JFRFTL)
      PHI0 = RW(IFRFT+JFRFP0)
      D0 = RW(IFRFT+JFRFD0)
      D0Q   = D0*D0
      Z0 = RW(IFRFT+JFRFZ0)
C
C Get the IWST bank
      KIWST = IW(NAMIND('IWST'))
C
C Loop over the layers of the ITC and find their intersection points wit
C the track.
      DO 50 LAY = 1,8
        IV(LAY) = 0
        RAD   = RWIRIT(LAY)
C Check that there is an intersection.
        IF (ABS(D0).GT.RAD.OR.ABS(2.0*R0-D0).LT.RAD) GOTO 50
        RADQ  = RAD*RAD
        FACT1 = RADQ - D0Q
        FACT2 = 0.5*FACT1/(R0-D0)
C Get the phi coordinate of the intercept.
        SINA  = (FACT2-D0)/RAD
        IF(ABS(SINA).GT.1.0) GOTO 50
        PHIT = PHI0 + ASIN(SINA)
        PHIT = MOD(PHIT+TWOPI,TWOPI)
C Get the z coordinate of the intersept.
        SPSI2 = 0.5*FACT2/R0
        IF (SPSI2.LT.0.0.OR.SPSI2.GT.1.0) GOTO 50
        PSI = ASIN(SQRT(SPSI2))
        ZT  = Z0 + 2.0*PSI*ABS(R0)*TANL
        IF(ABS(ZT).GT.WZMXIT) GOTO 50
C
C Find wire number at the intersection.
C First note cell size and phi shift at the beginning of the 1st cell.
        DLN  = TWOPI/FLOAT(NWIRIT(LAY))
        SHFN = PHWRIT(LAY) - DLN*0.5
C
        PHIT = PHIT - SHFN
        PHIT = AMOD(PHIT+TWOPI,TWOPI)
        IWIRE = INT(PHIT/DLN) + IWIRIT(LAY) + 1
C See if this wire is alive.
C   Loop over the list of dead wires.
        IF (KIWST.GT.0) THEN
          DO 20 I = 1,LROWS(KIWST)
            IF (IWIRE.EQ.ITABL(KIWST,I,JIWSIW)) THEN
              IFLG = ITABL(KIWST,I,JIWSFL)
              IF(IFLG.EQ.1.OR.IFLG.EQ.4) GOTO 50
            ENDIF
   20     CONTINUE
        END IF
C Note that this wire should have produced a coordinate on the track.
        IV(LAY) = 1
   50 CONTINUE
  999 CONTINUE
      END
