      SUBROUTINE X1PREP(IERR)
C----------------------------------------------------------------------
C!  - Prepare the bank XTDI used for the trigger analysis
C.
C.   Author   :- Alois Putzer           4-AUG-1989
C.               Martin Wunsch          4-AUG-1989
C.               Yves A. Maumary       20-DEC-1989 Adapted for ALEPHLIB
C!   Modified :- Yves A. Maumary       16-MAR-1990 for '90 run
C.
C.   Inputs:
C.        - none
C.
C.   Outputs:
C.        - IERR : INTEGER = 0 : creation and preparation of the bank
C.                               successful
C.                         = 1 : creation or preparation of the bank
C.                               failed
C.
C.   Libraries required: ALEPHLIB, BOS77, CERNLIB
C.
C.   Calls: ALTELL, AUBOS, BKFMT
C.
C?   Description
C?   ===========
C?   Creates the XTDI bank and prepares it (fill in names and set
C?   'filled' tag = 0)
C?
C.======================================================================
      SAVE
      EXTERNAL INTCHA,NAMIND
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
      COMMON/X1NAMC/NAXTBN,NAXTCN,NAXTDI,NAXTEB,NAXSGE,NAXSHI,
     &              NAXSSC,NAX1AD,NAX1SC,NAX1TH,NASIXA,NASIX2,NASIFO,
     &              NAX1RG,NAX1IP,NAX1TV
      PARAMETER(LROWDI=36,LROWSC=23)
C     144 = LROWDI*4, 92 = LROWSC*4
      CHARACTER*144 CHXTDI, CHXTD
      CHARACTER*92  CHX1SC, CHX1S
      PARAMETER(CHXTDI='TRG1TRG2TRGAHCT1HCT2HCT3HCT4HWT1HWT2HWT3'//
     &                 'HWT4LCW1LCW2LCW3LCW4EWT1EWT2EWT3EWT4ITCT'//
     &                 'TPCTLCT1LCT2LCT3LCT4ETOTHCW1HCW2HCW3HCW4'//
     &                 'EWU1EWU2EWU3EWU4ITSPTPSP')
      PARAMETER(CHX1SC='HCT1HCT2HCT3HCT4HCW1HCW2HCW3HCW4ECT1ECT2'//
     &                 'ECT3ECT4ECW1ECW2ECW3ECW4LCT1LCT2LCW1ETT1'//
     &                 'ITC TPC TRB ')
      PARAMETER(JXTDNA=1,JXTDFI=2,JXTDBP=3,LXTDIA=5)
      CHARACTER*4 RNAME
      LOGICAL FIRST
      DATA FIRST /.TRUE./
C
      IERR = 0
      IF(FIRST)THEN
C - fill strings CHXTD and CHX1S from PARAMETER statement
C   CHXTDI and CHX1SC and then use these strings to please
C   IBM compiler
        CHXTD = CHXTDI
        CHX1S = CHX1SC
        FIRST = .FALSE.
      ENDIF
C
C - Create bank XTDI
      LTDI = LXTDIA*LROWDI + LMHLEN
      CALL AUBOS('XTDI',1,LTDI,INDXS,IGARB)
      IF(IGARB.EQ.2)THEN
        CALL ALTELL('X1PREP: no space to book XTDI',0,'RETURN')
        IERR = 1
        RETURN
      ENDIF
      CALL BKFMT('XTDI','2I,(A,4I)')
C - Prepare bank XTDI
      NAXTDI = NAMIND('XTDI')
      INDXS = IW(NAXTDI)
      IF(INDXS.NE.0)THEN
        IW(INDXS+LMHCOL) = LXTDIA
        IW(INDXS+LMHROW) = LROWDI
        IPOIN = INDXS + LMHLEN
C -- Fill row names for bank XTDI and reset fill tag
        DO 10 J = 1,LROWDI
          RNAME = CHXTD((J-1)*4+1:J*4)
          IW(IPOIN+JXTDNA) = INTCHA(RNAME)
          IW(IPOIN+JXTDFI) = 0
          IPOIN = IPOIN + LXTDIA
   10   CONTINUE
      ELSE
        IERR = 1
      ENDIF
      RETURN
      END
