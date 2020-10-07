#if defined(ALEPH_DEC) &&  ! defined(UNIX)
      SUBROUTINE EPINIT
C ------------------------------------------------------------
C epio 1.68 version from cernlib 92b
C -------------------------------------------------------------
C. INITIALIZES EP FORMAT READ AND WRITE PACKAGE
      COMMON/EPCOMM/NMUNIT,NWUNIT,NCONT,ISTART,LASTUT,LREF,LIST(350)
      EQUIVALENCE (NOUTUT,LIST(8))
      COMMON/EPVAXN/EPVXUN(10)
      CHARACTER*255 EPVXUN
C--- NO. OF BITS / MACHINE WORD , LOGICAL OUTPUT UNIT (PRINTER DEF)
      DATA NBIT/32/,NOUT/6/
C ++++++++++++++++++ FLR additions to RETURN after the 1st call +++++
      SAVE IFI
      DATA IFI /0/
      IF (IFI.EQ.-1) THEN
         CALL EPBLIN
         CALL EPHEAD
      ENDIF
      IF (IFI.EQ.1) RETURN
      IFI = 1
C +++++++++++++++++ end of FLR additions ++++++++++++++++++++++++++++++
C--- Monitor package usage.
      CALL UMLOG('EPIO','*')
C--- MAX. NO. OF UNITS SUPPORTED SIMULTANEOUSLY
      NMUNIT=10
C--- NO. OF CONTROL WORDS / UNIT
      NWUNIT=33
C--- NO. OF OVERALL CONTROL WORDS
      NCONT=8
C--- ZERO LAST USER UNIT AND LAST INTERNAL UNIT
      LASTUT=0
      LREF=0
C--- SET DEFAULTS FOR OVERALL CONTROL WORDS
      LIST(1)=0
      LIST(2)=100
      LIST(3)=0
      LIST(4)=NBIT
      LIST(5)=180
      LIST(6)=12
      LIST(7)=9
      LIST(8)=NOUT
*
*---- BLANK DEFAULT FILE NAMES
*
      DO 10  I=1,NMUNIT
  10  EPVXUN(I)=' '
      RETURN
      END
#endif
