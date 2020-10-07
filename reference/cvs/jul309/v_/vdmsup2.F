      SUBROUTINE VDMSUP2(ITRK,NH,WAFER,UHIT,WHIT, IGArg)
C----------------------------------------------------------------------
C! Update face extrapolation bank VDMS for true VDET hit position
CKEY VDET TRACK
C
C   Author : Paul Rensing 7-7-95
C         Based on VDMSUP from ALEPHLIB 207
C
C   VDMS bank for the requested track is copied to bank# 0, and
C     updated for VDET hit information, coming either from given hits or
C     VDCO.
C
C   Inputs:
C     INTEGER ITRK     - FRFT track number
C     INTEGER NH       - number of VDET hits; 0 means use VDCO
C     INTEGER WAFER(*) - wafer number of hits
C     REAL UHIT(*)     - the VDET u co-ordinate of the hit (wafer local!)
C     REAL WHIT(*)     - the VDET w co-ordinate of the hit (wafer local!)
C
C   Outputs: 
C     INTEGER IGArg    - non 0 means a garbage collection was performed
C     VDMS, 0 bank
C----------------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C
C  Inputs
C
      INTEGER ITRK, NH
      INTEGER WAFER(*)
      REAL UHIT(*), WHIT(*)
      INTEGER IGARG
C
C  Global variables
C
#include "bcs.h"
C
#include "vdcojj.h"
#include "vdmsjj.h"
C
C maximum rows in VDMS
C
      INTEGER MXVDMS
      PARAMETER (MXVDMS = 20)
C
C  Local variables
C
      INTEGER NamVDCO
      REAL VDMSRL
      INTEGER IFLAG,IWAFID
      REAL UC,WC
      INTEGER KVDMS0,I,IPNTN,IPNTO,IND(10)
      INTEGER IVDCO,JVDCO,IVDMS,JVDMS,KVDMS
      INTEGER IMOD(4),JMOD
      INTEGER IHIT,NHIT,IGARB
      INTEGER ILAY,IWAF,IPHI,IVIEW
      INTEGER JLAY,JWAF,JPHI,JVIEW
      INTEGER NVDCO,NVDMS,IROW,JROW,JTRK,ITYPE
      REAL RADIUS,PHI,XYZ(3),VUW(3),FPOS(2,4),RAD(10)
      REAL ZCENT(6),XYZAB(3),ABC(3)
      INTEGER   NWMOD,NWFAC,VNRWAF,VNRMOD
      INTEGER IWFF,JWAB,IRET
C
      SAVE ZCENT, NamVDCO
C
C  Functions
C
      INTEGER NAMIND,NLINK,VABCXY
      external NAMIND, NLINK, VABCXY

#include "bmacrod.h"
C
C  Data statements
C
      DATA NamVDCO/0/
C
C  Statement functions
C
#include "bmacro.h"
C
C  First time, get the name indicies
C
      IF (NamVDCO .le. 0) THEN
         NamVDCO=NAMIND('VDCO')
C
         NWFAC = VNRWAF()*VNRMOD()
         ABC(1)=0.0
         ABC(2)=0.0
         ABC(3)=0.0
         NWMOD = NWFAC/2
         DO IWFF = 1,NWFAC
           IF (IWFF.LE.NWMOD) THEN
             JWAB = NWMOD-IWFF+1
           ELSE
             JWAB = IWFF
           ENDIF
           IRET = VABCXY(ABC,JWAB,XYZAB)
           ZCENT(IWFF)=XYZAB(3)
         ENDDO
      END IF
      IGArg = 0
C
C  Drop the previous VDMS,0 bank
C
      KVDMS0 = NLINK('VDMS',0)
      IF (KVDMS0.GT.0) IW(KVDMS0 + LMHROW) = 0
C
C  Find the VDMS bank corresponding to this track
C
      JVDMS = NLINK('VDMS',ITRK)
      IF (JVDMS.GT.0) THEN
         NVDMS=LROWS(JVDMS)
      ELSE
         NVDMS=0
      END IF
C
C  Find the VDCO bank
C
      JVDCO = IW(NamVDCO)
C
C  Create bank 10000
C
      KVDMS = NLINK('VDMS',10000)
      IF (KVDMS .LE. 0) THEN
         CALL AUBOS('VDMS',10000,MXVDMS*LVDMSA+LMHLEN,KVDMS,IGARB)
         IF (KVDMS .LE. 0) THEN
            CALL ALTELL('Cant make VDMS bank',1,'RETURN')
            RETURN
         END IF
         IF (IGARB .NE. 0) THEN
            IGArg = IGARB
            JVDMS = NLINK('VDMS',ITRK)
         END IF
      ENDIF
C
C  Copy the existing bank to number 10000
C
      IF (JVDMS .GT. 0) THEN
         CALL UCOPY(IW(JVDMS+1),IW(KVDMS+1),
     &        MIN(NVDMS,MXVDMS-4)*LVDMSA+LMHLEN)
      ELSE
C
C  If we are making a new bank, preset the mini-header
C
         IW(KVDMS+1)=LVDMSA
         IW(KVDMS+2)=0
      END IF
C
C  Parse on which hit bank to use; 0 means VDCO, >0 means uses listed hits
C
      IF (NH .GT. 0) THEN
C
C  Loop over the hits for this row
C
         NHIT = NH
         DO IHIT = 1, NHIT
C
C  Get wafer number and decode; take the Z wafer
C
            IMOD(IHIT) = WAFER(IHIT)
            CALL VADEWA(IMOD(IHIT),ILAY,IWAF,IPHI,IVIEW)
C
C  Get local coordinates; transform the W into 'face' coordinates
C
            FPOS(1,IHIT) = UHIT(IHIT)
            FPOS(2,IHIT) = WHIT(IHIT) + ZCENT(IWAF)
         END DO
         
      ELSE IF (JVDCO .GT. 0) THEN
        NVDCO = LROWS(JVDCO)
C
C Loop over VDCO entries
C
        NHIT = 0
        DO IROW = 1, NVDCO
C
C  Check for track number match; if so, store the hit position
C
          JTRK = ITABL(JVDCO,IROW,JVDCTN)
          IF (ITRK .EQ. JTRK) THEN
             NHIT=NHIT+1
C
C  Get wafer number; decode
C
            IMOD(NHIT) = ITABL(JVDCO,IROW,JVDCWI)
            CALL VADEWA(IMOD(NHIT),ILAY,IWAF,IPHI,IVIEW)
C
C  Get global position
C
            RADIUS = RTABL(JVDCO,IROW,JVDCR0)
            PHI    = RTABL(JVDCO,IROW,JVDCPH)
            XYZ(3) = RTABL(JVDCO,IROW,JVDCZ0)
            XYZ(1) = RADIUS*COS(PHI)
            XYZ(2) = RADIUS*SIN(PHI)
C
C  Convert to local coordinates
C
            CALL VGWFVU(IMOD(NHIT),XYZ,VUW)
C
C  Convert U AND W to 'FACE' coordinates
C
            FPOS(1,NHIT) = VUW(2)
            FPOS(2,NHIT) = VUW(3)+ZCENT(IWAF)
          END IF
        END DO
      ELSE
C
C  No VDET hits from any source
C
         NHIT=0
      END IF
C
C  Loop over the hits
C
      DO IHIT = 1, NHIT
C
C  Decode the module address of this hit
C
         CALL VADEWA(IMOD(IHIT),ILAY,IWAF,IPHI,IVIEW)
C
C  Loop over the VDMS rows to find the matching face
C
         DO JROW=1,NVDMS
C
C  Decode this extrapolation address; check for a layer, phi match
C
            JMOD = ITABL(KVDMS,JROW,JVDMWI)
            CALL VADEWA(JMOD,JLAY,JWAF,JPHI,JVIEW)
            IF (JLAY .EQ. ILAY .AND. JPHI .EQ. IPHI) THEN
C
C  Match; overwrite the U and W coordinates, and the wafer ID
C  Set the flag bit for this extrapolation
C
               RW(KROW(KVDMS,JROW)+JVDMUC)=FPOS(1,IHIT)
               RW(KROW(KVDMS,JROW)+JVDMWC)=FPOS(2,IHIT)
               IW(KROW(KVDMS,JROW)+JVDMWI)=IMOD(IHIT)
               IW(KROW(KVDMS,JROW)+JVDMFL)=2
C
C  Escape from the loop
C
               GOTO 300
            END IF
         END DO
C
C  Error if we got to here; hit does not match any extrapolation
C
         CALL ALTELL('No VDMS match',3,'RETURN')
 300     CONTINUE
      END DO
C
C now add rows in VDMS 10000 for support rings and carbon fibre tube
C
      CALL VDMSEC(ITRK)
C
C copy VDMS 10000 to VDMS 0 and order the rows
C
      KVDMS0 = NLINK('VDMS',0)
      IF (KVDMS0 .LE. 0) THEN
         CALL AUBOS('VDMS',0,MXVDMS*LVDMSA+LMHLEN,KVDMS0,IGARB)
         IF (KVDMS0 .LE. 0) RETURN
         IF (IGARB .NE. 0) Then
            KVDMS = NLINK('VDMS',10000)
            IGArg = IGARB
         END IF
      ENDIF
      IW(KVDMS0+1) = IW(KVDMS+1)
      IW(KVDMS0+2) = IW(KVDMS+2)
      
      
      DO I = 1, LROWS(KVDMS)
         RAD(I) = RTABL(KVDMS,I,JVDMRA)
      END DO
      IF (LROWS(KVDMS).GT.0) CALL SORTZV(RAD,IND,LROWS(KVDMS),1,1,0)
      DO I = 1, LROWS(KVDMS)
         IPNTO = KROW(KVDMS,IND(I))+1
         IPNTN = KROW(KVDMS0,I)+1
         CALL UCOPY(IW(IPNTO),IW(IPNTN),LVDMSA)
      END DO
      IW(KVDMS+2)=0
      
C now put the correct amount of multiple scattering into VDMS 0
      DO I = 1,LROWS(KVDMS0)
         IFLAG = ITABL(KVDMS0,I,JVDMFL)
         UC = RTABL(KVDMS0,I,JVDMUC)
         WC = RTABL(KVDMS0,I,JVDMWC)
         IWAFID = ITABL(KVDMS0,I,JVDMWI)
         IF (IWAFID .NE. -1) RW(KVDMS0+LMHLEN+
     $        (I-1)*IW(KVDMS0+1)+JVDMSG) = VDMSRL(UC,WC,IWAFID)
      END DO
      
      RETURN
      END
#endif
