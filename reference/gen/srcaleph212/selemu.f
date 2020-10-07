      SUBROUTINE SELEMU (IFLAHM,IFLAMC,IFLACA,CLAS19)
C----------------------------------------------------------------------
CKEY EDIR DEF CLASS19
C! Muon identification class 19 code.
C-
C   Input  : None
C   Output : IFLAHM = Muon flag from HMAD bank
C            IFLAMC = Muon flag from MCAD bank
C            IFLACA = Muon flag based on MUCALO analysis
C            CLAS19 = class 19 logical flag
C-
C   Called by   : SELEVT
C   Calls  : MUCATR
C   Input banks : HMAD,MCAD,PFRF
C-
C                                       Author: R. Tenchini January 90
C----------------------------------------------------------------------
      SAVE
C --
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      PARAMETER(JHMANF=1,JHMANE=2,JHMANL=3,JHMAMH=4,JHMAIG=5,JHMAED=6,
     +          JHMACS=7,JHMAND=8,JHMAIE=9,JHMAIT=10,JHMAIF=11,
     +          JHMATN=12,LHMADA=12)
      PARAMETER(JMCANH=1,JMCADH=3,JMCADC=5,JMCAAM=7,JMCAAC=8,JMCATN=9,
     +          LMCADA=9)
C --
      COMMON/MUEFF/NHMAD,NHMAMB,NHMPOS,NMCAD,NMBOTH,NMSING,
     &             NMCALO,NHMEVT,NMCEVT,NCAEVT,NMUEVT,NMUAND
      PARAMETER(LENMAX=1000)
      DIMENSION IPLIS(LENMAX),ICLIS(LENMAX),ITLIS(LENMAX)
      PARAMETER(MAXL=100)
      DIMENSION NCAVEC(MAXL)
      DIMENSION ENER(1000),PT(1000)
      DIMENSION LISTEJ(300)
      CHARACTER*8 CNAM
      LOGICAL CLAS19
C --
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
C --
      IFLAHM=0
      IFLAMC=0
      IFLACA=0
      CLAS19 = .FALSE.
C --
      KHMAD=IW(NAMIND('HMAD'))
      IF(KHMAD.LE.0) GOTO 888
C --
      NMATRK = LROWS(KHMAD)
C --
C   Loop on charged tracks linked to HMAD
C --
      DO 10 NMT = 1,NMATRK
        IFMU = ITABL(KHMAD,NMT,JHMAIF)
        IF(IFMU.EQ.1) THEN
          IFLAHM = 1
          GOTO 999
        ENDIF
   10 CONTINUE
C --
  888 CONTINUE
      KMCAD=IW(NAMIND('MCAD'))
      IF(KMCAD.LE.0) GOTO 889
C --
      NMATRK = LROWS(KMCAD)
C --
C   Loop on charged tracks linked to MCAD
C --
      DO 20 NMT = 1,NMATRK
        MCAD1 = ITABL(KMCAD,NMT,JMCANH)
        MCAD2 = ITABL(KMCAD,NMT,JMCANH+1)
        IF(MCAD1.GE.1 .OR. MCAD2.GE.1) THEN
          IFLAMC = 1
          GOTO 999
        ENDIF
   20 CONTINUE
C --
  889 CONTINUE
      KPFRF=IW(NAMIND('PFRF'))
      IF(KPFRF.LE.0) GOTO 999
C --
      NMATRK = LROWS(KPFRF)
C --
C   MUCATR analysis
C --
      DO 30 NMT = 1,NMATRK
        CALL MUCATR(NMT,PR1OUT,PR2OUT,IFLAG)
        IF(IFLAG.EQ.1) THEN
          IFLACA = 1
          GOTO 999
        ENDIF
   30 CONTINUE
C --
999   CONTINUE
C --
      IF(IFLAHM.EQ.1 .OR. IFLAMC.EQ.1 .OR. IFLACA.EQ.1)
     &  CLAS19 = .TRUE.
C --
      RETURN
      END
