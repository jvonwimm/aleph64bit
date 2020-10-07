      subroutine VDMKCE
C ---------------------------------------------------------------------------
C!  Makes the Chip efficiency map file VDCE (similar to the old VDEM)
C!  from VDPR (which defines the efficiency periods and weights) and
C!  VDCM (which gives the efficiencies of the chips that have non-default
C!  efficiency)
C!
C - Author         Manoj Thulasidas  3-Jul-1998
C
C   Input    VDPR and VDCM banks
C   Output   VDCE Bank
C
C
C      Add protections for garbage collections.
C                   07-Oct-1999 Thomas Kachelhoffer
C
C ---------------------------------------------------------------------------
#ifndef DOC
      implicit none
C  globals
#include "bcs.h"
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  arguments
C  functions
      EXTERNAL NAMIND
      INTEGER NAMIND
C  locals
C  number of chips per view
      INTEGER MXCHIP
      PARAMETER ( MXCHIP = 384 )
      INTEGER NAVDPR, KVDPR, NAVDCM, KVDCM, NVDCE, NDATA, IGARB, IVDCE,
     $   IPER, JLAY, IFAC, IMOD, IVIEW, ICHIP, JCHIP, KRVDCE, IFIRST,
     $   KVDCE, NVDCM, IVDCM
      REAL EFF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#include "vdcmjj.h"
#include "vdprjj.h"
#include "vdcejj.h"
#include "bmacrod.h"
#include "bmacro.h"
C============================================================================
      NAVDPR = NAMIND('VDPR')
      KVDPR = IW(NAVDPR)
      IF (KVDPR.LE.0) THEN
        CALL RERROR('VDMKCM',1,
     $     'Failed to read database bank VDPR. VDET efficiencies 100%')
        GOTO 999
      ENDIF
      NAVDCM = NAMIND('VDCM')
      KVDCM = IW(NAVDCM)
C VDCE will have same number of rows as VDPR
      NVDCE = LROWS(KVDPR)
C each row is LVDCEA (=771) elements long
      NDATA = NVDCE*LVDCEA + LMHLEN
      CALL ALBOS('VDCE',0,NDATA,KVDCE,IGARB)
      IF ( IGARB .EQ. 2 ) THEN
        CALL RERROR('VDMKCE',1,
     $     'Failed to create database bank VDCE.')
        GOTO 999        
      ENDIF
      IF ( IGARB .EQ. 1 ) THEN
        NVDCE = LROWS( KVDCE )
        KVDPR = IW(NAVDPR)
        KVDCM = IW(NAVDCM)
      ENDIF
      IW(KVDCE+LMHCOL) = LVDCEA
      IW(KVDCE+LMHROW) = NVDCE
C fill the VDCE bank with default values
      DO IVDCE = 1, NVDCE
        KRVDCE = KROW(KVDCE,IVDCE)
        IW(KRVDCE+JVDCPR) = ITABL(KVDPR,IVDCE,JVDPWT)
C for view=1
        EFF = RTABL(KVDPR,IVDCE,JVDPE1)
        IFIRST = KRVDCE+JVDCEF
        CALL VFILL(RW(IFIRST),MXCHIP,EFF)
C for view=2
        EFF = RTABL(KVDPR,IVDCE,JVDPE2)
        IFIRST = IFIRST+MXCHIP
        CALL VFILL(RW(IFIRST),MXCHIP,EFF)
      ENDDO
C  Now loop over the anomolous efficiencies
      IF (KVDCM.GT.0) THEN
        NVDCM = LROWS(KVDCM)
        DO IVDCM = 1, NVDCM
          IPER = ITABL(KVDCM,IVDCM,JVDCPE)
          JLAY = ITABL(KVDCM,IVDCM,JVDCJL)
          IFAC = ITABL(KVDCM,IVDCM,JVDCIF)
          IMOD = ITABL(KVDCM,IVDCM,JVDCIM)
          IVIEW = ITABL(KVDCM,IVDCM,JVDCVW)
          ICHIP = ITABL(KVDCM,IVDCM,JVDCCH)
          CALL VDCHNB(JLAY, IFAC, IMOD, IVIEW, ICHIP, JCHIP)
          KRVDCE = KROW(KVDCE,IPER)
          RW(KRVDCE+JVDCEF-1+JCHIP) = RTABL(KVDCM,IVDCM,JVDCEC)
        ENDDO
      ENDIF
 999  return
      end
#endif
