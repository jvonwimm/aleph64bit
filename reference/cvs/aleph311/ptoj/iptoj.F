      SUBROUTINE IPTOJ(LIST,IER)
C-----------------------------------------------------------------------
CKEY PTOJ ITC
C! Change ITC POT banks to JULIA format.
C!
C!    Author     :- I. Tomalin    88/08/25
C!    Modified   :- I. Tomalin    88/11/17
C!    Modified   :- J. Sedgbeer   89/03/03 Modified from ITUPDI to be
C!                                         an ALEPHLIB routine.
C!    Modified   :- J. Sedgbeer   89/04/11
C!    Modified   :- J. Sedgbeer   91/01/07 Get JULIA version no. from
C!                                RHAH bank. Use this - tidy code.
C!    Modified   :- J. Sedgbeer   92/01/30 Remove obsolete code.
C!
C!    Input:
C!      LIST   /C    : BOS event list:
C!                         if LIST(2:2).eq.'-' then drop POT banks
C!      commons:     /BCS/     for banks  PIDI and FRTL
C!                   /ITWICC/  ITC Geometry - filled by subr. IGEOMW
C!                   /IZFECC/  New Z front end params. Filled by IFECON
C!                   /IRFECC/  New R-phi f-end params. Filled by IFECON
C!      params:      FRTLJJ
C!                   IDIGJP
C!                   ALCONS
C!
C!    Output:
C!      IER    /I    : = 0 successful
C!                     = 1 input bank is empty or does not exist
C!                     = 2 not enough space
C1                     =-1 O.K. but garbage collection
C!      IDIG bank of ITC digitisings.
C!      FICL bank track coordinate list. Only created if FRTL exists.
C!
C! IPTOJ:
C! Recreate the IDIG bank from the packed digitisings (PIDI) bank.
C! The IDIG bank will be ordered according to increasing wire number.
C! Also create/overwrite FICL bank if FRTL bank exists.
C! N.B. MUST CALL IRDDAF before using this routine (to fill ITC
C! geometry and front-end commons.
C!
C?  If first then Set name indices and bank formats
C?  Check for non-empty PIDI bank.
C?  If FRTL bank exists then
C?    find total number of ITC coords assoc. with tracks, NUSED.
C?  else
C?    set NUSED = 0
C?  endif
C?  Create IDIG bank
C?  If FRTL bank exists then Create FICL bank
C?  Create a work bank for relation between old and new order (IADRIW).
C?  Loop over the packed digitisings.
C?    Unpack information about each digitising
C?    Load this into bank IDIG
C?    If FRTL exists then fill the relation work bank (IADRIW)
C?  End Loop
C?  Loop over digits
C?    Move digi. info. to correct place in bank IDIG
C?    If non-empty FRTL exists then fill the FICL bank
C?  Endloop
C?  Compress the IDIG bank.
C?  Set Jlist and Plist.
C?  Add Jlist to S list, drop Plist if required.
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C I/O commons and parameters
#include "bcs.h"
#include "alcons.h"
#include "itwicc.h"
#include "izfecc.h"
#include "irfecc.h"
#include "frtljj.h"
#include "idigjp.h"
      EXTERNAL NAMIND,INTCHA
C Dummy common block to save work bank indices.
      COMMON/IADUMB/IADRIW
      CHARACTER*(*) LIST, PLIST*4, JLIST*4
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
C
      IF(FIRST) THEN
        FIRST = .FALSE.
        NPIDI = NAMIND('PIDI')
        NIDIG = NAMIND('IDIG')
        NFRTL = NAMIND('FRTL')
        NFICL = NAMIND('FICL')
        CALL BKFMT('IDIG','I')
        CALL BKFMT('FICL','I')
      ENDIF
C
C Check for PIDI bank
C
      IER = 1
      JPIDI = IW(NPIDI)
      IF(JPIDI.EQ.0) GOTO 999
      NDIG = LROWS(JPIDI)
      IF(NDIG.EQ.0) GOTO 999
C
C Check for FRTL bank. Find total # of hits assoc. with tracks, NUSED.
C (If no FRTL bank then can still unpack digis. but cannot create FICL)
C
      NUSED = 0
      JFRTL = IW(NFRTL)
      IF(JFRTL.GT.0) THEN
        DO 10 I=1,LROWS(JFRTL)
          NUSED = NUSED + ITABL(JFRTL,I,JFRTNI) + ITABL(JFRTL,I,JFRTNE)
   10   CONTINUE
      ENDIF
C
C Create IDIG bank
C Use IGARB to note if garbage collection done.
C
      IGARB = 0
      CALL AUBOS('IDIG',0,LMHLEN+MWIRIT,JIDIG,IER)
      IF(IER.EQ.2) GOTO 999
      IF(IER.EQ.1) THEN
        IGARB = 1
        JPIDI = IW(NPIDI)
      ENDIF
      IW(JIDIG+LMHCOL) = 1
      IW(JIDIG+LMHROW) = NDIG
C
C If non-empty FRTL exists then Create new FICL bank and fill header.
C If no space then drop previously created IDIG bank before exiting.
C
      IF(NUSED.GT.0) THEN
        CALL AUBOS('FICL',0,LMHLEN+NUSED,JFICL,IER)
        IF(IER.EQ.2) THEN
          CALL BDROP(IW,'IDIG')
          GOTO 999
        ENDIF
        IF(IER.EQ.1) THEN
          IGARB = 1
          JPIDI = IW(NPIDI)
          JIDIG = IW(NIDIG)
        ENDIF
        IW(JFICL+LMHCOL) = 1
        IW(JFICL+LMHROW) = NUSED
      ENDIF
C
C Create a work bank showing where each digit in the reordered PIDI
C bank came from in the old one.
C
      IER = 2
      IADRIW = 0
      CALL WBANK(IW,IADRIW,LMHLEN+MWIRIT,*997)
      IW(IADRIW+LMHCOL) = 1
      IW(IADRIW+LMHROW) = MWIRIT
C
C-----------------------------------------------------------------------
C Loop over the digitisings.
C
      DO 20 N=1,NDIG
C Unpack the PIDI information for this digitising.
        NCODE = IW(JPIDI+LMHLEN+N)
        IWIRE = IBITS(NCODE,IBITWN,NBITWN)
        IAMB  = IBITS(NCODE,IBITAM,NBITAM)
C Clear bits used in PIDI only
        NCODE = IBCLR(NCODE,IBITAM)
        NCODE = IBCLR(NCODE,IBITVS)
C
C Fill the IDIG bank, ordering according to increasing wire number (but
C leaving some gaps).
        IW(JIDIG+LMHLEN+IWIRE) = NCODE
C Note the old address in the PIDI bank of this digit (if it was
C associated with a track).
        IF (N.GT.NUSED) GOTO 20
        IW(IADRIW+LMHLEN+IWIRE) = N
C Put the coord. ambiguity information into the IADRIW bank as well.
        IF(IAMB.EQ.1) IW(IADRIW+LMHLEN+IWIRE) = -IW(IADRIW+LMHLEN+IWIRE)
   20 CONTINUE
C
C Loop over the digits, removing the gaps and filling in the
C pointer block.
C
      IOFF = 0
      DO 40 N=1,MWIRIT
        ITEST = IW(JIDIG+LMHLEN+N)
        IF (ITEST.EQ.0) GOTO 40
        IOFF = IOFF + 1
        IW(JIDIG+LMHLEN+IOFF) = ITEST
C If non-empty FRTL exists then fill the FICL bank with the digit's
C new address.
        IF(NUSED.LE.0) GOTO 40
        IOLDA = IW(IADRIW+LMHLEN+N)
        IF (IOLDA.NE.0) IW(JFICL+LMHLEN+ABS(IOLDA)) = SIGN(IOFF,IOLDA)
   40 CONTINUE
C
C-----------------------------------------------------------------------
C Compress the IDIG bank.
C
      IER = 2
      CALL AUBOS('IDIG',0,LMHLEN+NDIG,JIDIG,IER)
      IF (IER.EQ.1) IGARB = 1
C
C Set Jlist and Plist. If no FRTL bank then Jlist is just 'IDIG'
C otherwise Jlist is 'IDIGFICL'
C
      PLIST = 'PIDI'
      JLIST = 'IDIG'
      IF(NUSED.GT.0) JLIST = JLIST(1:LNBLNK(JLIST)) // 'FICL'
C
C Add Jlist to S list, drop Plist if required - see macro JPLIST.
#include "jplist.h"
C
C If garbage collection then set error flag to -1
C
      IER = 0
      IF(IGARB.EQ.1) IER = -1
      GOTO 998
C-----------------------------------------------------------------------
C Error return from WBANK. Drop unfilled banks before exiting.
C
  997 CALL BDROP(IW,'IDIG')
      CALL BDROP(IW,'FICL')
C
C Drop the work banks.
  998 IF (IADRIW.NE.0) CALL WDROP(IW,IADRIW)
  999 CONTINUE
      END
#endif
