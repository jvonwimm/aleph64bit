      SUBROUTINE MUIDF(NEXP,NFIR,IM1,IM2,N10,N03,MULT,ISHAD,RAPP,
     $                  ANG,IDF)
C-----------------------------------------------------------------------
C
CKEY MUONID MUON / INTERNAL
C
C!    calculates the muon id flag
C!    authors: D.Cinabro    10-JUN-1990
C!    revised: G.Taylor     20-APR-1992
C!             G.Taylor      5-OCT-1992
C!             Split flag 15 into 15 and 10 add arguements RAPP,DIST
C!
C!    input  : NEXP  /I = num of hcal dig hits expected
C!             NFIR  /I = num of hcal dig hits fired
C!             IM1   /I = num of muon chamber hits in first layer
C!             IM2   /I = num of muon chamber hits in second layer
C!             N10   /I = num of hcal dig hits fired in last 10 exp
C!             N03   /I = num of hcal dig hits fired in last 3 exp
C!             MULT  /I = excess digital multiplicity in last 10
C!                          hcal planes(*100)
C!             ISHAD /I = shadowing flag (=0 if best candidate)
C!             RAPP  /F = dist/cut muon chamber
C!             ANG   /F = ang/cut muon chamber
C!
C!    output : IDF/I = muon id flag
C=======================================================================
#ifndef DOC
C
      LOGICAL HCALF
C
C Cut values for official ID
C
      DATA XN23 /0.4/            ! Cut on number fired/Number exp
      DATA NC10 /5/              ! Number in last 10 expected
      DATA NC03 /1/              ! Number in last 3 expected
      DATA MUL /205/           ! Maximum hit multiplicity in
                                 ! last 10 planes
      DATA MULH /150/           ! Maximum Heavy Flavor hit multiplicity
                                 ! in last 10 planes
      DATA NEXM /10/             ! Minimum expected hits Heav. Flavour
      DATA NPLAC/2/              ! min numb of planes in mu-chambers
C-----------------------------------------------------------------------
      IDF = 0
      IMT=IM1+IM2
C
      IF (NEXP.NE.0) THEN
        RAT = FLOAT(NFIR)/FLOAT(NEXP)
      ELSE
        RAT = 0.
        IF (NFIR.GE.1) RAT = 1.
      ENDIF
C
      IF (MULT.LE.MUL) THEN
        HCALF = RAT.GE.XN23.AND.N10.GE.NC10
        IF (HCALF.AND.N03.GE.NC03)  IDF=1
        IF (.NOT.HCALF.AND.IMT.GE.NPLAC) IDF=2
        IF (HCALF.AND.IMT.GE.1) IDF=3
      ENDIF
C
C More complex Heavy flavor ID
C
      IF (MULT.LE.MULH) THEN
        HCALF=RAT.GE.XN23.AND.N10.GE.NC10.AND.NEXP.GE.NEXM
        IF (HCALF.AND.N03.GE.NC03) IDF=IDF+10
        IF (.NOT.HCALF.AND.IMT.GE.NPLAC) IDF=12
        IF (HCALF.AND.IMT.GE.1) IDF=13
        IF (IM1.GE.1.AND.IM2.GE.1) THEN
          IF(HCALF)THEN
            IDF=14
          ELSE
            IDF=15
          ENDIF
        ENDIF
      ENDIF
      IF(IDF.EQ.15.AND.(RAPP.GT..5.OR.ANG.GT..15)) IDF=10
C
C Deal with shadow candidates
C
      IF (ISHAD.LT.0) THEN
        IDF = -ABS(IDF)
      ENDIF
      RETURN
      END
#endif
