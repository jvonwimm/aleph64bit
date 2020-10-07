      SUBROUTINE VTRURL(IKOD,IBN,ICLS,NHXYZ,NRVS,NMAX,
     &                  NMATC,ITIND,NSTR,QFRAC,IERR)
C ----------------------------------------------------------------------
C
C!  Supplies TRUTH information for reconstructed hits from VFPH
C - Author: Alain Bonissent (Nov 1994)
C! Modified November 1996, handle properly strips which 
C!                belong to two clusters (after splitting)
CKEY V_DECK VDET
C
C  INPUTS:
C  ------
C IKOD = Type of relation wanted
C        1 or any nb = Relation to VDHT
C        2 = Relation to FRFT
C IBN    - Bank number for VFPH and VFHL linking
C ICLS  - Hit number
C NHXYZ = Row number in hits bank of the hit
C NRVS  = NR of the relevant VTRS bank
C NMAX  = max number of allowed matched tracks
C
C OUTPUTS:
C --------
C
C NMATC:   Number of matching tracks
C ITIND:   Index of matched tracks
C NSTR  = Array of number of strips in which 
C         each track has deposited charge
C QFRAC = Charge deposited by each track (array)
C IERR  = return code (0=OK)
C
C    Banks used: VFPH,VFHL,FPOI,FKIN,VTRS
C-----------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
      INTEGER ITIND(NMAX),NSTR(NMAX)
      DIMENSION QFRAC(NMAX)
      PARAMETER(NSTPMX=50)
#include "vfhljj.h"
#include "vdhtjj.h"
#include "fkinjj.h"
#include "fverjj.h"
#include "fpoijj.h"
#include "vtrsjj.h"
#include "vspljj.h"
C
C  IMASK is for masking off irrelevant bits in the hit address from VFHL
C     INTEGER IMASK/Z0003FC00/
C      DATA IMASK/261120/
C
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      SAVE NAVDHT, NAFPOI, NAFKIN, NAVTRS
C
C  Inline functions
C
#include "bmacro.h"
C
      IF(FIRST) THEN
      FIRST=.FALSE.
         NAVDHT = NAMIND('VDHT')
         NAFPOI = NAMIND('FPOI')
         NAFKIN = NAMIND('FKIN')
         NAVTRS = NAMIND('VTRS')
      ENDIF
C  Initialization
      CALL VZERO(ITIND, NMAX)
      NMATC=0
      ISTRD =0
      IERR=0
C Access VSPL bank of split clusters
      KVSPL = NLINK('VSPL',NRVS)
      IFRS=0
      ILSS=9999
      ISHF = 0
      ISHL = 0
      IF(KVSPL.NE.0)THEN
        NVSPL=LROWS(KVSPL)
        DO IVSPL=1,NVSPL
           JCLU=ITABL(KVSPL,IVSPL,JVSPNH)
           IF(JCLU.EQ.NHXYZ)THEN
              IFRS = ITABL(KVSPL,IVSPL,JVSPFS)
              ILSS = ITABL(KVSPL,IVSPL,JVSPLS)
              ISHF = ITABL(KVSPL,IVSPL,JVSPSF)
              ISHL = ITABL(KVSPL,IVSPL,JVSPSL)
           ENDIF
        ENDDO
      ENDIF
C
C Link up with strips to tracks relation banks
      KVTRS=IW(NAVTRS)
C Link up with cluster banks
      KVFHL=NLINK('VFHL',IBN)
      KVFPH=NLINK('VFPH',IBN)
      IF(KVFHL.EQ.0.OR.KVFPH.EQ.0.OR.KVTRS.EQ.0) THEN
         IERR=1
         GO TO 999
      ENDIF
      NVTRS = LROWS(KVTRS)
C
C Link up with FKIN and FPOI banks
C
      KFPOI = IW(NAFPOI)
      KFKIN = IW(NAFKIN)
      IF((KFPOI.LE.0.OR.KFKIN.LE.0).AND.IKOD.EQ.2) THEN
         IERR=1
      GO TO 999
      ENDIF
      KVDHT = IW(NAVDHT)
      IF(KVDHT.LE.0.AND.IKOD.EQ.2) THEN
         IERR=1
      GO TO 999
      ENDIF
C
C  Check for nonsense
      NVFHL=LROWS(KVFHL)
      IF (ICLS.GT.NVFHL) THEN
C     PRINT *,' TRUTH LIES: IERR,ICLS,NHIT ',IERR,ICLS,NHIT
         IERR=2
         GO TO 999
      ENDIF
C
C  ---Find offset to strip addresses, pulse heights in the VF banks
C      DO 20 NC=1,ICLS-1
C         ICODE=ITABL(KVFHL,NC,JVFHHA)
C         CALL VADDUN(ICODE,NST,ILAY,IWAF,IPHI,IVIE,ISTR)
C         ISTRD=ISTRD+NST
C  20  CONTINUE
      ICODE=ITABL(KVFHL,ICLS,JVFHHA)
C
C  Unpack strip address from VFHL
      CALL VUNADD(ICODE,NSTP,ILAY,IWAF,IPHI,IVIE,ISTR)
C
C Make corrected NSTP in case of split cluster
C
      ISTR = MAX(ISTR,IFRS)
C
C  And mask off some bits irrelevant for matching
      CALL VPKADD(ICODEX,0,ILAY,IWAF,IPHI,IVIE,ISTR-1)
C      ICODEX=IAND(ICODE,IMASK)+ISTR
C
C Check for more nonsense
      IF(NSTP.GT.NSTPMX) THEN
         IERR=2
         GO TO 999
      ENDIF
C
C Make corrected NSTP in case of split cluster
C
      NSTP = MIN(NSTP,ILSS-IFRS+1)
C
C Gather pulse heights : Adj. strip number to agree w/online conv.
C
      DO 300 ISTP=1,NSTP
      ICODE = ICODEX+ISTP
C
C Loop over list of relations looking for matches
C
      DO 310 IVTRS=1,NVTRS
         ICORL = ITABL(KVTRS,IVTRS,JVTRAD)
         IF(ICODE.EQ.ICORL)THEN
C Here, we found the same strip in cluster and relations bank
C Get the track number from VDHT bank
            IVDHT = ITABL(KVTRS,IVTRS,JVTRHT)
            ITKL=IVDHT
            IF(IKOD.EQ.2)THEN
               IF(ITKL.GT.0) ITKL = ITABL(KVDHT,IVDHT,JVDHTN)
               IF(ITKL.GT.0) ITKL = ITABL(KFPOI,ITKL,JFPOIP)
            ENDIF
C IF ITKL =0 This is noise, most likely a delta ray
               DO 320 IMATC=1,NMATC
               IF(ITIND(IMATC).EQ.ITKL)THEN
C Here, we find that this relation already exists : this track
C contributes to more than 1 strip in the cluster, we update
                  QFRAC(IMATC)=QFRAC(IMATC)+RTABL(KVTRS,IVTRS,JVTRCH)
                  NSTR(IMATC)=NSTR(IMATC)+1
                  GO TO 310
               ENDIF
  320          CONTINUE
C This track has not yet been found for this cluster
C we have to create a new entry in the arrays
               IF(NMATC.LT.NMAX)THEN
                  NMATC=NMATC+1
                  ITIND(NMATC)=ITKL
                  QFRAC(NMATC)=RTABL(KVTRS,IVTRS,JVTRCH)
                  NSTR(NMATC)=1
               ELSE
                  IERR=3
                  GO TO 999
               ENDIF
         ENDIF
  310 CONTINUE
  300 CONTINUE
C
C Now, we divide by the total cluster charge, to make percentages
C
C      DO 400 IMATC=1,NMATC
C  400 QFRAC(IMATC)=QFRAC(IMATC)/QTOT
  999 CONTINUE
      RETURN
      END
#endif
