      INTEGER FUNCTION RTLSUM(LROCEX, IW)
C-----------------------------------------------------------------------
C! summarize RTLO banks in RTLS on error
C --
C -- inputs:
C --   LROCEX (integer)
C --     bit mask telling which readout controllers should be there
C --     usually PARAMETER (LROCEX = 8159), i.e. 1FDFhex
C --   IW(*)
C --     BOS array
C
C -- output:
C --   bos bank RTLS in case of inconsistent RTLO banks
C --   RTLO banks are dropped except RTLO # 0
C --
C -- description:
C -- check consistency of RTLO banks in raw data:
C --  o  are all of them there that are expected (parameter LROCEX)?
C --        the ROCs expected are specified using bits in LROCEX
C --        (bits 0..13):
C --        main EB, LVL1trigger, LV2trigger, VDET, ITC, TPC, ECAL, HCAL
C --        SATR, LCAL, BCAL, TPCA, TPCB
C --        in addition .GE. 17 pad TPPs per TPC endcap are required
C --        (note that the TPC main EB generally should NOT be there)
C --  o  do all contain the same trigger mask and event number?
C --
C --
C -- everything fine:
C --   drop all RTLO but main EB RTLO (with bank number 0)
C -- else
C --   build RTLS bank, bank number 0 and drop all RTLO banks but RTLO #
C --
C -- bank format for RTLS:
C -- ..............................................................
C --   1      i  Number of Colums = 60
C --   2      i  Number of rows = 2
C -- ..............................................................
C --    1  EM  i  trigger number main EB
C --    2  E1  i     "      "    level 1 trigger
C --    3  E2  i     "      "    level 2 trigger
C --    4  EV  i     "      "    VDET
C --    5  EI  i     "      "    ITC
C --    6  ET  i     "      "    TPC main EB
C --    7  EE  i     "      "    ECAL
C --    8  EH  i     "      "    HCAL
C --    9  ES  i     "      "    SATR
C --   10  EL  i     "      "    LCAL
C --   11  EC  i     "      "    BCAL
C --   12  EA  i     "      "    TPC side A
C --   13  EB  i     "      "    TPC side B
C -- 14-49     i     "      "    pad TPP of TPC sector # 1..36
C -- 50-60     i  reserved
C -- ..............................................................
C --   next row: ditto with trigger mask
C -- ..............................................................
C --
C?
C!======================================================================
#ifndef DOC
      IMPLICIT NONE
      INTEGER LROCEX, IW(*)

      INTEGER NAMIND, NBANK, NDROP
      EXTERNAL NAMIND, NBANK, MDROP
      EXTERNAL BLIST, BDROP, UCOPY2, VZERO
      EXTERNAL NSWAP, NDROP

      INTEGER IP, IP0

      INTEGER I, IROC
      INTEGER IEV, ITR, TA, TB, LROC, IRTLO, NARTLO
      LOGICAL OK

      INTEGER NEB, NROC, NROC0
C -- MAIN EB + 10 EBS + 2 TPP SIDE EB 36 TPPS + 11 RESERVED
      PARAMETER (NEB = 13,
     1           NROC = NEB+36+11,
     1           NROC0 = NROC-1
     1           )
      INTEGER RTLSLE
      PARAMETER (RTLSLE = NROC*2+2)
      INTEGER RTLOLE
      PARAMETER (RTLOLE = 12)
      INTEGER TRNUMO
      PARAMETER (TRNUMO = 3)
      INTEGER TRMSKO
      PARAMETER (TRMSKO = 4)
      INTEGER FBADDR(0:NROC0)
      DATA FBADDR/  0, 1285, 773, 1031, 1029, 1539, 1287, 1027,
     1  1035,  1289,  1033,  24592,  24584,
     1  8980,17172,25364,25361,17169, 8977,
     1  8982, 8981,17174,17173,25366,25365,
     1  25363,25362,17171,17170, 8979, 8978,
     1  33556,41748,49940,49937,41745,33553,
     1  33558,33557,41750,41749,49942,49941,
     1  49939,49938,41747,41746,33555,33554,
     1  11*-1
     1/
      IEV = 0
      ITR = 0
      TA = 0
      TB = 0
      LROC = 0
      OK = .TRUE.

      NARTLO = NAMIND('RTLO')
      IF (NARTLO .LE. 0) THEN
C -- NEVER HEARD ABOUT RTLO... THERE IS NOTHING WE CAN DO
        RTLSUM = 1
        RETURN
      END IF
      IP = IW(NARTLO)
C     DO WHILE(IP .GT. 0)
   10 CONTINUE
      IF (IP .LE. 0) GOTO 11
      IF (IEV .EQ. 0) IEV = IW(IP+TRNUMO)
      IF (ITR .EQ. 0) ITR = IW(IP+4)
      IF (IW(IP+TRNUMO) .NE. IEV .OR.
     1      IW(IP+TRMSKO) .NE. ITR) THEN
C -- INCONSISTENT EVENT NUMBER OR TRIGGER MASK
        OK = .FALSE.
        GOTO 1
      END IF

C -- SEARCH IN FASTBUS ADDRESS ARRAY FOR THIS BANK NUMBER
      IROC = 99999999
      DO 20 I = 0, NROC0
        IF(FBADDR(I) .EQ. IW(IP-2)) THEN
          IROC = I
          GOTO 21
        END IF
   20 CONTINUE
   21 CONTINUE
      IF (IW(IP-2) .EQ. 0) THEN
C -- THIS IS THE MAIN INTERFACE RTLO
        IRTLO = IP
      END IF
C -- COUNT RTLO'S FROM TPPS, SET BITS FOR EBS
      IF (IROC .LE. NEB-1) THEN
        LROC = IBSET(LROC, IROC)
      ELSE IF (IROC .LE. NEB+18-1) THEN
        TA = TA+1
      ELSE IF (IROC .LE. NEB+36-1) THEN
        TB = TB+1
      ELSE
C -- UNKNOWN RTLO BANK NUMBER - FUNNY, BUT NOT ALARMING... COULD BE A WI

      END IF
      IP = IW(IP-1)
C     END DO
      GOTO 10
   11 CONTINUE
    1 CONTINUE

C - CHECK IF EVERYTHING HAS BEEN FOUND
      OK =  OK .AND.
     1      TA .GE. 17 .AND.
     1      TB .GE. 17 .AND.
     1      IAND(LROC,LROCEX) .EQ. LROCEX

      IF (OK) THEN
C
C -----------------------------------------------------------------
C -- EVERYTHING IS FINE,
C -- DO NOT WRITE RTLS, DROP ALL RTLO BUT THE MAIN EB ONE
C -----------------------------------------------------------------
C
        CALL NSWAP('RTLO',0,'RTL9',0)
        CALL BDROP(IW,'RTLO')
        CALL NSWAP('RTL9',0,'RTLO',0)
C
C        CALL BLIST(IW, 'E-', 'RTLS')
C        CALL BDROP(IW, 'RTLO')
C        IP = NBANK('RTLO', 0, RTLOLE)
C        CALL UCOPY2(IW(IRTLO+1), IW(IP+1), RTLOLE)
C        CALL BLIST(IW, 'E+', 'RTLO')
        RTLSUM = 1
      ELSE
C
C -----------------------------------------------------------------
C -- SOMETHING WENT WRONG
C -- CREATE AND FILL RTLS BANK, DROP ALL RTLO BUT RTLO #0
C -----------------------------------------------------------------
C
        IP0 = NBANK('RTLS', 0, RTLSLE)
        IF (IP0 .LE. 0) THEN
C -- COULD NOT CREATE SUMMARY BANK RTLS
          RTLSUM = 0
          RETURN
        END IF
        CALL VZERO(IW(IP0+1), RTLSLE)
        CALL BLIST(IW, 'E+', 'RTLS')
C -- FILL SUMMARY BANK
        IW(IP0+1) = NROC
        IW(IP0+2) = 2
        IP0 = IP0+2
C -- GET TRIGGER NUMBER AND MASK FROM RTLO AND PUT IT INTO RTLS
        IP = IW(NARTLO)

C       DO WHILE(IP .GT. 0)
  100   CONTINUE
        IF (.NOT.(IP .GT. 0)) GOTO 101
C -- SEARCH IN FASTBUS ADDRESS ARRAYS FOR THIS BANK NUMBER
        IROC = 99999999
        DO 110 I = 0, NROC0
          IF(FBADDR(I) .EQ. IW(IP-2)) THEN
            IROC = I
            GOTO 111
          END IF
  110   CONTINUE
  111   CONTINUE

        IF (IROC .LE. NROC0) THEN
          IW(IP0+IROC+1) = IW(IP+TRNUMO)
          IW(IP0+NROC+IROC+1) = IW(IP+TRMSKO)
C -- DROP ALL BUT THE MAIN RTLO
          IF (IROC .NE. 0) THEN
            I = NDROP('RTLO', FBADDR(IROC))
          END IF
        ELSE
C -- UNKNOWN RTLO, FUNNY, DO NOT DROP BANK (E.G. IT COULD BE A WIRE TPP)

        END IF
        IP = IW(IP-1)
C       END DO
        GOTO 100
  101   CONTINUE
        RTLSUM = 0
      END IF

      RETURN
      END
#endif
