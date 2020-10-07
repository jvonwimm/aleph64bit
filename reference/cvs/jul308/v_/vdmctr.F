      SUBROUTINE VDMCTR(IERR)
C ------------------------------------------------------------
C!  Compress Monte carlo truth information
C A.Bonissent - 950317
C! Modified November 1996, handle properly strips which 
C!                belong to two clusters (after splitting)
CKEY V_DECK VDET
C   Input banks : VTRS Galeph strips to KINE tracks relation
C                 VDCO Reconstructed 3D hit clusters
C                 VDXY, VDZT 2D reconstructed hit clusters
C                 FKIN Compressed kinematic tracks built in Julia
C                 FPOI Relation between KINE and FKIN tracks
C                 VFHL, VFPH  Julia pulseheight banks
C
C   Output banks : VDFK, VUFK Relations between
C                             reconstructed hits VDCO, VDXY, VDZT
C                             and FKIN tracks
C
C   RETURN CODES: IERR=-1,-2 MISSING BANKS,
C                      -3 : Could not booK VDFK,
C                      -4 : NMAX was too small
C                IERR > 0 : same error occured in VUMCTR
C                           when dealing with unassociated hits
C                       0 OK
C     =========================================
#ifndef DOC
#include "bcs.h"
#include "vdcojj.h"
#include "vcpljj.h"                                                                                                 
#include "vdxyjj.h"
#include "vdztjj.h"
#include "vdfkjj.h"
#include "vdhtjj.h"
C
      PARAMETER(NMAX=20)
      DIMENSION ITMP(NMAX),ITMZ(NMAX),NSTRP(NMAX),NSTRZ(NMAX)
      DIMENSION QFRP(NMAX),QFRZ(NMAX)
      DIMENSION ITM(2*NMAX),NSTP(2*NMAX),NSTZ(2*NMAX)
      DIMENSION QFP(2*NMAX),QFZ(2*NMAX)
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      SAVE NAFKIN,NAVDCO
C
C  Inline functions
C
#include "bmacro.h"
      IF(FIRST) THEN
         NAVDCO=NAMIND('VDCO')
         NAFKIN=NAMIND('FKIN')
         FIRST=.FALSE.
      ENDIF
C
      IERR=0
C
C Delete old existing bank
C
      CALL BDROP(IW,'VDFK')
C
C initialize some indices
C
      KVDCO = IW(NAVDCO)
      KFKIN = IW(NAFKIN)
C
      IF(KVDCO.LE.0.OR.KFKIN.LE.0) THEN
         IERR=-1
         GO TO 999
      ENDIF
C
C Get number of FKIN tracks
      NTRK = LROWS(KFKIN)
C
C Now that we have the delta rays simulated, some hits may be attached
C to no track, i.e. track 0. Therefore we have one more track than rows
C in the FKIN bank (possibly)
      NTRK=NTRK+1
C
C  get #hits in VDCO bank
C
      NVDCO=LROWS(KVDCO)
C
C Create new relations bank with max possible size
      NREL = NVDCO*NTRK
      NDATA = NREL*LVDFKA+LMHLEN
      CALL AUBOS('VDFK',0,NDATA,KVDFK,IGARB)
      IF(IGARB.GE.2)THEN
         IERR = -3
         GO TO 999
      ELSE IF(IGARB.EQ.1)THEN
         KVDCO = IW(NAVDCO)
         KFKIN = IW(NAFKIN)
      ENDIF
C
      IW(KVDFK+LMHCOL) = LVDFKA
      IW(KVDFK+LMHROW) = 0
      ITRK0 = 0
C
C  loop over VDCO hits, find list of matched tracks
C
      DO 110 IVDCO=1,NVDCO
       IP=0
       IZ=0
       NMATP=0
       NMATZ=0
C  get Julia track number
       ITRCK=ITABL(KVDCO,IVDCO,JVDCTN)
C
       IF (ITRCK.NE.ITRK0) THEN
         ITRK0 = ITRCK
C  link bank VCPL
         KVCPL=NLINK('VCPL',ITRCK)
         IF(KVCPL.LE.0) GO TO 100
         JHIT = 0
       ENDIF
C
C  increment hit# in VCPL bank
       JHIT=JHIT + 1
C  get bank# of VDXY,VDZT bank
       NUMXY=ITABL(KVCPL,JHIT,JVCPXB)
       NUMZT=ITABL(KVCPL,JHIT,JVCPZB)
C
C   and link them
       KVDXY=NLINK('VDXY',NUMXY)
       KVDZT=NLINK('VDZT',NUMZT)
C
       IERP=0
       IERZ=0
       IF(KVDXY.GT.0) THEN
C   got the banks, now get hit#
          IP=ITABL(KVCPL,JHIT,JVCPNX)
C  we have enough to finally look for the truth
          IF (IP.GT.0) THEN
            NRVS = NUMXY+1
            NTP = ITABL(KVDXY,IP,JVDXIH)
            NBA = ITABL(KVDXY,IP,JVDXIW)
            QTOT = RTABL(KVDXY,IP,JVDXPH)
            CALL VTRURL(2,NBA,NTP,IP,NRVS,NMAX,
     &        NMATP,ITMP,NSTRP,QFRP,IERP)
C
C Now, we divide by the total cluster charge, to make percentages
C
            DO III=1,NMATP
               QFRP(III)=QFRP(III)/QTOT
            ENDDO
          ENDIF
       ENDIF
C
       IF(KVDZT.GT.0) THEN
C  now the same for Z clusters
          IZ=ITABL(KVCPL,JHIT,JVCPNZ)
          IF (IZ.GT.0) THEN
             NRVS = NUMZT
             NTZ=ITABL(KVDZT,IZ,JVDZIH)
             NBA=ITABL(KVDZT,IZ,JVDZIW)
             QTOT = RTABL(KVDZT,IZ,JVDZPH)
             CALL VTRURL(2,NBA,NTZ,IZ,NRVS,NMAX,
     &        NMATZ,ITMZ,NSTRZ,QFRZ,IERZ)
C
C Now, we divide by the total cluster charge, to make percentages
C
            DO III=1,NMATZ
               QFRZ(III)=QFRZ(III)/QTOT
            ENDDO
          ENDIF
       ENDIF
       IF(IERP.EQ.1.OR.IERZ.EQ.1)THEN
          IERR=-2
          GO TO 999
       ENDIF
C
  100 CONTINUE
C
      NMAT = 0
C
C First, copy the relations in xy view
C
      DO 50 IMATP=1,NMATP
      ITM(IMATP) = ITMP(IMATP)
      NSTP(IMATP) = NSTRP(IMATP)
      QFP(IMATP) = QFRP(IMATP)
      NSTZ(IMATP) = 0
      QFZ(IMATP) = 0.
   50 CONTINUE
      NMAT = NMATP
C
C Now, update for the relations in ZT view
C
      DO 60 IMATZ=1,NMATZ
      DO 61 IMAT=1,NMAT
      IF(ITM(IMAT).EQ.ITMZ(IMATZ))THEN
         NSTZ(IMAT) = NSTRZ(IMATZ)
         QFZ(IMAT) = QFRZ(IMATZ)
         GO TO 60
      ENDIF
   61 CONTINUE
      NMAT = NMAT+1
      ITM(NMAT) = ITMZ(IMATZ)
      NSTZ(NMAT) = NSTRZ(IMATZ)
      QFZ(NMAT) = QFRZ(IMATZ)
      NSTP(NMAT) = 0
      QFP(NMAT) = 0.
   60 CONTINUE
C
C  Put information in VDFK bank
C
      DO 70 IMAT=1,NMAT
      NVDFK = LROWS(KVDFK)+IMAT
      KLINE = KROW(KVDFK,NVDFK)
      RW(KLINE+JVDFPC) = QFZ(IMAT)
      RW(KLINE+JVDFPC+1) = QFP(IMAT)
      IW(KLINE+JVDFSC) = NSTZ(IMAT)
      IW(KLINE+JVDFSC+1) = NSTP(IMAT)
      IW(KLINE+JVDFFK) = ITM(IMAT)
      IW(KLINE+JVDFVD) = IVDCO
   70 CONTINUE
      IW(KVDFK+LMHROW) = LROWS(KVDFK)+NMAT
  110 CONTINUE
C
  999 CONTINUE
      CALL AUBPRS('VDFK')
      CALL VUMCTR(IERU)
      IF(IERU.NE.0)THEN
         IERR=-IERU
      ENDIF
      RETURN
      END
#endif
