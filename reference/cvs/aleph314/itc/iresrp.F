      REAL FUNCTION IRESRP(LAY,DDS,CALP)
C-----------------------------------------------------------------------
C! Get ITC R-phi coordinate resolution
C!
CKEY ITC
C!  Author  :- J. Sedgbeer   24/10/91
C!  Modified:- J. Sedgbeer   12/02/92 Make the default values of the
C!                           r-phi resol. those of 1990 data (as the
C!                           IRRF bank is not in ADBS890 !).
C!
C!    Input:
C!      LAY     /I  :  Itc layer number (1-8)
C!      DDS     /R  :  Drift-distance to coordinate (signed)
C!      CALP    /R  :  Cosine of entrance angle of track thru cell.
C!
C!      commons:       /BCS/ for bank IRRF. Note that it is assumed
C!                           IRRF has already exists in /BCS/
C!                           (i.e. IRDDAF previously called)
C!                     /ITWICC/ ITC geometry - for cell width
C!      parameters:    IRFFJJ
C!
C!    Output:
C!      IRESRP  /R  : ITC r-phi resolution (cm)
C!
C!      ** N.B. **
C!      IRESRP is > 0  if all O.K.
C!      If IRRF bank is missing then the nominal r-phi resolution is
C!      taken to be the 1990 data values and IRESRP is set NEGATIVE.
C!      The 1990 data values are used because if IRRF is
C!      missing it is probably because 8990 data is being read and
C!      hence ADBS8990  - IRRF is NOT in this DAF.
C!
C? Check for IRRF bank
C? If no IRRF bank then
C?    set nominal resolution
C? else
C?    get resolution parameters for layer LAY from IRRF bank
C?    calculate resolution for drift distance DDS
C?    limit to maximum/minimum allowed
C? endif
C? Correct resolution for entrance angle
C?
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
C I/O commons and parameters
#include "bcs.h"
#include "itwicc.h"
#include "irrfjj.h"
C-----------------------------------------------------------------------
      EXTERNAL NAMIND
      INTEGER NAMIND,LAY,NIRRF,JIRRF,KK,IC
      REAL DDS,CALP,RESOL,DD,CA
      LOGICAL FIRST
      REAL RES90(10)
      DATA RES90/
     +    0.1716E-1,-0.3747E-2, -0.1828E-1,  0.4138E-3,  0.3073E-1,
     +    0.1754E-1, 0.9952E-3, -0.2411E-1, -0.2671E-1,  0.5513E-1/
      DATA FIRST/.TRUE./
C-----------------------------------------------------------------------
#include "bmacro.h"
C-----------------------------------------------------------------------
C Initialise
      IF(FIRST) THEN
        FIRST = .FALSE.
        NIRRF = NAMIND('IRRF')
      ENDIF
C
C scaled drift distance - limit to 1.3
      DD = 2.0*ABS(DDS)/CELWIT(LAY)
      IF(DD.GT.1.3) DD = 1.3
C
C Check for IRRF bank
C
      JIRRF = IW(NIRRF)
C
C If no IRRF bank set default resolution
      IF(JIRRF.LE.0) THEN
C Pointer to coeffs. in RES90
        IC = 1
        IF(DDS.LT.0.0) IC = 6
C
        RESOL = RES90(IC) + RES90(IC+1)*DD + RES90(IC+2)*DD**2 +
     +                   RES90(IC+3)*DD**3 + RES90(IC+4)*DD**4
C limit resol. to something reasonable
        IF(RESOL.GT.0.0600) RESOL = 0.0600
        IF(RESOL.LT.0.0050) RESOL = 0.0050
C Set negative to indicate missing IRRF
        RESOL = -RESOL
C
C else get resolution from IRRF bank
      ELSE
        KK = KROW(JIRRF,LAY)
C Pointer to coeffs. in IRRF bank
        IC = JIRRCP
        IF(DDS.LT.0.0) IC = JIRRCN
C
        RESOL = RW(KK+IC) + RW(KK+IC+1)*DD + RW(KK+IC+2)*DD**2 +
     +                   RW(KK+IC+3)*DD**3 + RW(KK+IC+4)*DD**4
C limit resol. to allowed maximum/minimum
        IF(RESOL.GT.RW(KK+JIRRMX)) RESOL = RW(KK+JIRRMX)
        IF(RESOL.LT.RW(KK+JIRRMN)) RESOL = RW(KK+JIRRMN)
      ENDIF
C
C Correct for entrance angle - limit to about 85 degrees max.
C                              (i.e. cos about 0.1)
      CA = ABS(CALP)
      IF(CA.LT.0.1) CA = 0.1
      IRESRP = RESOL/CA
C
      END
#endif
