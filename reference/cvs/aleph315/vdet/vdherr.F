      SUBROUTINE VDHERR(IWAFA,PV,PU,PW,SIGNU,SIGNW,UERR,WERR)
C
C! Compute the VDET hit error given wafer information and track direction
CKEY VDET JULIA
C
C  This uses a simple theoretical model to compute the hit errors
C  given the signal/noise, and other parameters (see VDET ALIGNMENT
C  ALEPHNOTE of 1991).  Dave Brown, 7-2-91
C
C  INPUT;  IWAFA =  Decimal wafer address
C          PV,PU,PW = track momentum vector at wafer in wafer frame
C          SIGNU,SIGNW = Signal/noise on U,W side
C OUTPUT;  UERR,WERR = hit errors in U,W direction
C
#ifndef DOC
      SAVE
#include "vdgeos.h"
      INTEGER IWAFA,ILAY,IWAF,IPHI,IVIEW,ITYPE
      REAL RPITU,FPITU,RPITW,FPITW,PV,PU,PW
      REAL SIGNU,SIGNW,UERR,WERR
      REAL COSW, SINW, COSU, SINU
      REAL UERRP,UERRN,WERRP,WERRN
C
C  Shower spread factor- this should be computed from the pulseheight,
C  but for now we take a nominal value.  We also need sqrt(12).
C
      REAL SPRED,ROT12
      DATA SPRED/1.5/
      DATA ROT12/3.46410/
C
C  Unpack the wafer address, and find the wafer type
C
      CALL VADEWA(IWAFA,ILAY,IWAF,IPHI,IVIEW)
      ITYPE = NSLOGM(IPHI,ILAY)
C
C  Get the readout and floating strip pitches, U and W direction
C
      RPITU = VDPPIT(ITYPE)
      RPITW = VDZPIT(ITYPE)
      FPITU = VDSTPH(ITYPE)
      FPITW = VDSTZE(ITYPE)
C
C  Compute the cosine/sines
C
      COSW = SQRT( MAX(1./(1.+ (PW/PV)**2),0.0) )
      SINW = SQRT( MAX(1. - COSW**2,0.0) )
      COSU = SQRT( MAX(1./(1.+ (PU/PV)**2),0.0) )
      SINU = SQRT( MAX(1. - COSU**2,0.0) )
C
C  Resolution due to strip pitch
C
      UERRP = FPITU*COSU/ROT12
      WERRP = FPITW*COSW/ROT12
C
C  Resolution due to noise
C
      UERRN = (RPITU/SIGNU)*
     &  ( (1.0 + SPRED*SINU)/SQRT(1.+SINU**2 + SINW**2) )
      WERRN = (RPITW/SIGNW)*
     &  ( (1.0 + SPRED*SINW)/SQRT(1.+SINU**2 + SINW**2) )
C
C  Resolution due to alignment (NOT YET IMPLEMENTED)
C
C
C  Add in quadrature
C
      UERR = SQRT( UERRP**2 + UERRN**2)
      WERR = SQRT( WERRP**2 + WERRN**2)
C
      RETURN
      END
#endif
