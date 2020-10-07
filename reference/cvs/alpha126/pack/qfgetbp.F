      SUBROUTINE QFGET_BP (IRUN, IEVT, ICHUNK, VTX, VTX_ERR, VTX_SIZE)
C ----------------------------------------------------------------------
CKEY FILL BEAMSPOT / INTERNAL
C!  Chunk-by-chunk beam position and size
C - Interface to the ALEPHLIB routines getlep/getbp  J. Boucrot 24-9-98
C - Modified A. Ealet 12-6-99  to smear beam error uncertainties
C            in MCarlo datasets using datacard SIBE
C
C - Input:
C   IRUN         / I  Run number
C   IEVT         / I  Event number
C
C - Input and output:
C   ICHUNK       / I  Previous chunk number; set this to 0 for the
C                       first call.  It is updated by the subroutine,
C                       and the subsequent call to QFGET_BP should be
C                       made with the returned value.  If ICHUNK is
C                       returned -1, no BP information exists for this
C                       run/event.
C
C - Output:
C   VTX(3)       / R  (x,y,z) position of luminous region centroid (cm)
C                       Notice: VTX(3) is always set equal to zero.
C
C   VTX_ERR(3)   / R  Estimated uncertainties on VTX (cm)
C                       Notice: VTX_ERR(3) is always set equal to 1.
C
C   VTX_SIZE(3)  / R  (x,y,z) rms size of luminous region (cm),
C                       averaged over the year/run period
C
C
C   Real data:
C   The beam position x and y and their uncertainties returned in VTX
C   and VTX_ERR are taken from the ALPB bank in the run record, or
C   from the beam.position file if JULIA version < 275.03.  The sizes
C   of the luminous region returned in VTX_SIZE are taken from the
C   ALRP database bank and are average values for the year (or run
C   period).
C
C   Monte Carlo:
C   The beam position x and y returned in VTX are generated so as to
C   simulate the desired beam size distribution.  The primary vertex
C   position of the event is extracted from truth information and an
C   offset is added according to the size of the luminous region.
C   The smearing corresponding to the average chunk-by-chunk beam
C   position uncertainty (VTX_ERR) is included.  The "desired" beam
C   size distribution is by default taken from measurements of the
C   actual LEP conditions for the geometry year of the Monte Carlo
C   file.  This default may be changed by means of data cards as
C   explained below.  The horizontal size of the luminous region,
C   sigma_x, is thrown at random from the measured distribution, which
C   is parametrized as the sum of two Gaussians (not a delta function).
C   The uncertainties in x and y returned in VTX_ERR, as well as the
C   three sizes returned in VTX_SIZE, are taken from the ALRP database
C   bank and are average values for the year.
C
C   Customizing the luminous region size for Monte Carlo:
C   As mentioned above, the default behavior is to use the luminous
C   region size parameters for the geometry year of the Monte Carlo
C   file.  The BPER card allows the user to override the default.  The
C   parameters from the year(s) specified on this card are then used;
C   each event is assigned at random to a particular year.  The years
C   (in the LEP 1 era) are weighted according to the numbers of qqbar
C   events in the MAYB/PERF runs in the VD run selection.  Different
C   weights may be specified by means of the BPWT card.  (This card is
C   mandatory for specifying the relative weights of LEP 2 periods,
C   if two or more periods are selected with BPER.)  Finally, arbitrary
C   beam sizes may be simulated by means of the BSIZ card.
C
C   The BPER, BPWT, and BSIZ cards have no effect when real data is
C   analyzed.
C
C   Syntax:
C
C   BPER [per1 [per2 [per3...]]]
C
C     where per1,per2,per3... are run periods as specified in the
C     first column of ADBR.  Years may be given as "1992" or "92"
C     or "9200".  Other run periods have the form "YYMM", such as
C     "9510".  A list of valid run periods is printed (and
C     execution terminates) if the BPER card is given with invalid
C     or no run periods.
C
C   BPWT [iwt1 [iwt2 [iwt3...]]]
C
C     where iwt1,iwt2,iwt3... are *integer* weights for the periods
C     listed on the BPER card.  An error results if there is no
C     accompanying BPER or if it has a different number of tokens.
C     The weights may have an arbitrary normalization.
C
C   BSIZ sigma_x sigma_y [sigma_z]
C
C     where sigma_x and sigma_y are floating point numbers giving the
C     desired sizes of the luminous region in cm.  The BSIZ card
C     overrides all values in ALRP.  The value of sigma_z, if given,
C     is returned in VTX_SIZE(3) but has no other effect.  If sigma_z
C     is not given, the value 1 is returned in VTX_SIZE(3).
C
C     Notes: the beam position uncertainty is set to zero if BSIZ is
C     given.  BPER/BPWT cards are ignored if BSIZ is given.
C
C   Examples:
C
C   1. To simulate 1993 and 1994 beam size conditions:
C      BPER 1993 1994
C
C   2. To simulate 1993 and 1994 with equal weights:
C      BPER 1993 1994
C      BPWT   50   50
C
C   3. To simulate a luminous region size of 120 x 7 microns:
C      BSIZ 0.0120 0.0007
C
C-----------------------------------------------------------------------
#ifndef DOC
#include "qcde.h"
      PARAMETER ( NRMC = 2000 )
      REAL GRAN(9),XMC_PERR(4)
      REAL VTX(3), VTX_ERR(3), VTX_SIZE(3), XYZ(3), DXYZ(3)
      COMMON / GETFLGS /  IGETEN , IGETXY
      COMMON / GETLVOU /  ICHNNK , VT_SIZE(3)
      DATA ISM / 0 /
C-----------------------------------------------------------------------
C Once per event, call the ALEPHLIB routine GETLEP
C which computes the best possible known value of the LEP energy
C and then calls the event-chunk beam position calculation routine GETBP
C
C ALPHA variables QELEP and KRINLF are filled here :
C
      CALL GETLEP(IRUN,IFOUN,KRINLF,NV,QELEP,XYZ,DXYZ)
C For backward-compatibility for old LEP1 (REAL DATA) processings :
      CALL GETOFS(IRUN,QDBOFS)
C
C ALPHA variables for the beam position:
C
      QVXNOM=XYZ(1)
      QVYNOM=XYZ(2)
      QVZNOM=XYZ(3)
      QVXNSG=DXYZ(1)
      QVYNSG=DXYZ(2)
      QVZNSG=DXYZ(3)
C      
C Fill variables corresponding to the event-chunk beam position
C only if the GETBP routine called by GETLEP was successful (IGETXY=4) :
C 
      ICHUNK=-1
      IF (IGETXY.NE.4) GO TO 990
C
C Output arguments of this routine :
C
      ICHUNK=ICHNNK
      DO 10 K=1,3
         VTX(K)=XYZ(K)
         VTX_ERR(K)=DXYZ(K)
         VTX_SIZE(K)=VT_SIZE(K)
 10   CONTINUE
C
C For MC events, if data card SIBE present, smear the
C beam position uncertainties:
C
 990  IF (IRUN.GT.NRMC) GO TO 999
      ISM=ISM+1
      JSIBE=IW(NAMIND('SIBE'))
      IF (JSIBE.GT.0) THEN
         CALL RNORML(GRAN,9)
         xmc_perr(1) = RW(JSIBE+1)*1E-03
         xmc_perr(2) = RW(JSIBE+2)*1E-03
         xmc_perr(3) = RW(JSIBE+3)*1E-03
         xmc_perr(4) = RW(JSIBE+4)*1E-03
         VTX_ERR(1) = XMC_PERR(1) + XMC_PERR(2)*GRAN(6)
         VTX_ERR(2) = XMC_PERR(3) + XMC_PERR(4)*GRAN(7)
         IF (ISM.EQ.1)      write(IW(6),*)
     +   '_QFGET_BP: XY Beam position uncertainties smeared using SIBE'
      ELSE
           IF (ISM.EQ.1)    write(IW(6),*)
     +   '_QFGET_BP: Use fixed XY beam position uncertainties'
      ENDIF
C      
 999  RETURN
      END
#endif
