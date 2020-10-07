      SUBROUTINE FREFIT(IRET)
C
C----------------------------------------------------------------------
C! Refit existing tracks (for DST-->DST reprocessing in JULIA)
C!
C!   Author   :- Robert P. Johnson     17-OCT-1990
C!
C!   Inputs:  Banks FRFT, FRTL, FTCL, FICL, FVCL  with NR=0
C!            (Note, the above banks are dropped on output)
C!            Bank TPCO with NR=0
C!            Banks ICCO, VDCO coordinate banks
C!
C!   Outputs: Banks FRFT, FRTL, FTCL, FICL, FVCL  with NR=1
C!            (These banks contain the newly fitted tracks)
C!            Bank TPCO with NR=1 with errors recalculated
C!            IRET       /I      Return code
C!                               0= everything OK
C!                               1= input banks not found
C!                               2= no room in BOS for output banks
C!                               3= input banks are not consistant
C!
C!   Libraries required: ALEPHLIB
C!
C!   Description
C!   ===========
C!   Tracks are fit to coordinates in TPCO, ICCO and VDCO using the
C!   coordinate lists in FTCL, FICL, and FVCL (NR=0).  The helix
C!   parameters in FRFT NR=0 are used as starting points for the fit.
C!   The results are place in FRFT NR=1.  Likewise, the new coordinate
C!   lists (which for now at least are the same as the old) are output
C!   in banks with NR=1.  If a track fails the fit, then the old fit
C!   values simply are copied to the output banks.
C!   Modified :- E. Lancon             26-AUG-1992
C!        Set number of VDET points to 0 in FRTL
C?
C!======================================================================
#ifndef DOC
C
#include "bcs.h"
#include "frftjj.h"
#include "frtljj.h"
#include "itcojj.h"
#include "vdcojj.h"
#include "rconds.h"
C
      LOGICAL LGARB,FIRST
      DIMENSION HP(6),COV(21),EE0(21)
      DATA FIRST/.TRUE./
C
C------------------ Statement Function Definitions ---------------------
C
#include "bmacro.h"
C
C-----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST=.FALSE.
        NTPCO=NAMIND('TPCO')
        NICCO=NAMIND('ICCO')
        NVDCO=NAMIND('VDCO')
        NITCO=NAMIND('ITCO')
      ENDIF
C
C++   Check whether the input banks exist
C
      IFRFT=NLINK('FRFT',0)
      IFRTL=NLINK('FRTL',0)
      IFTCL=NLINK('FTCL',0)
      IFICL=NLINK('FICL',0)
      IF (IFRFT.EQ.0 .OR. IFRTL.EQ.0
     &            .OR. (IFTCL.EQ.0.AND.IFICL.EQ.0)) THEN
        IRET=1
        GO TO 999
      ENDIF
C
C++   Recalculate the coordinate errors
C
      CALL FTPCER(IER)
C
C++   Drop the output banks if they already exist
C
      KFRFT=NLINK('FRFT',1)
      IF (KFRFT.NE.0) KFRFT=NDROP('FRFT',1)
      KFRTL=NLINK('FRTL',1)
      IF (KFRTL.NE.0) KFRTL=NDROP('FRTL',1)
      KFTCL=NLINK('FTCL',1)
      IF (KFTCL.NE.0) KFTCL=NDROP('FTCL',1)
      KFICL=NLINK('FICL',1)
      IF (KFICL.NE.0) KFICL=NDROP('FICL',1)
      KFVCL=NLINK('FVCL',1)
      IF (KFVCL.NE.0) KFVCL=NDROP('FVCL',1)
C
C++   Create the output bank for the new fit
C
      IW(1)=1
      LEN= LROWS(IFRFT)*LCOLS(IFRFT)+LMHLEN
      CALL AUBOS('FRFT',1,LEN,KFRFT,IGARB)
      IF (IGARB.EQ.2) THEN
        IRET=2
        CALL RERROR('FREFIT',1,'No room to create the FRFT bank.')
        GO TO 999
      ENDIF
      IW(KFRFT+LMHCOL)=LCOLS(IFRFT)
      IW(KFRFT+LMHROW)=LROWS(IFRFT)
      IF (IGARB.NE.0) THEN
        IFRFT=NLINK('FRFT',0)
        IFRTL=NLINK('FRTL',0)
        IFTCL=NLINK('FTCL',0)
        IFICL=NLINK('FICL',0)
      ENDIF
      IFVCL=NLINK('FVCL',0)
      KTPCO=IW(NTPCO)
      KICCO=IW(NICCO)
      KITCO=IW(NITCO)
      KVDCO=IW(NVDCO)
C
C++   Loop over all the existing tracks
C
      DO 700 ITK=1,LROWS(IFRFT)
C
C?   Here one produces FRFT banks without VDET points so one should drop
C?   any VDET information from previous processing.
C
        IW (KROW(IFRTL,ITK)+JFRTIV) = 0
        IW (KROW(IFRTL,ITK)+JFRTNV) = 0

C
C++     Get the number of coordinates in each detector and check if
C++     the correspond coordinate list bank is available.
C
        NTPC=ITABL(IFRTL,ITK,JFRTNT)
        IF (NTPC.GT.0 .AND. (IFTCL.EQ.0.OR.KTPCO.EQ.0)) THEN
          CALL RERROR('FREFIT',3,'The FTCL or TPCO bank is missing.')
          IRET=3
          GO TO 999
        ENDIF
        NITC=ITABL(IFRTL,ITK,JFRTNI)
        IF (NITC.GT.0
     &        .AND. (IFICL.EQ.0.OR.KICCO.EQ.0.OR.KITCO.EQ.0)) THEN
          CALL RERROR('FREFIT',4,
     *         'The FICL or ICCO or ITCO bank is missing.')
          IRET=3
          GO TO 999
        ENDIF
        NVDT=ITABL(IFRTL,ITK,JFRTNV)
        IF (NVDT.GT.0 .AND. (IFVCL.EQ.0.OR.KVDCO.EQ.0)) THEN
          CALL RERROR('FREFIT',5,'The FVCL or VDCO bank is missing.')
          IRET=3
          GO TO 999
        ENDIF
        IOITC=ITABL(IFRTL,ITK,JFRTII)
        IOTPC=ITABL(IFRTL,ITK,JFRTIT)
        IOVDT=ITABL(IFRTL,ITK,JFRTIV)
C
C++     Call the Kalman filter fit if TPC points are present
C
        IF (NTPC.GT.0) THEN
          IOPT=9
          NMULT=0
          CALL UFTTRA(ITK,FIELRC,RW(KROW(IFRFT,ITK)+JFRFIR),
     &                RW(KROW(IFRFT,ITK)+JFRFC2),
     &                NTPC,NITC,NVDT,IW(IFTCL+LMHLEN+IOTPC+1),
     &                IW(IFICL+LMHLEN+IOITC+1),IW(IFVCL+LMHLEN+IOVDT+1),
     &                HP,COV,CHI2,NDOF)
          IF (CHI2.GT.1.0E10) THEN
            IERR=4
          ELSE
            IERR=0
          ENDIF
        ELSEIF (NITC.GT.0) THEN
C
C++       Use the circle fit if there are no TPC points
C
          IOPT=2
          NMULT=0
          CALL UFITMS(NTPC,NITC,NVDT,IW(IFTCL+LMHLEN+IOTPC+1),
     &         IW(IFICL+LMHLEN+IOITC+1),IW(IFVCL+LMHLEN+IOVDT+1),
     &         IOPT,NMULT,29.6,0.027,HP,EE0,COV,CHI2,IERR)
C
C++       Calculate the number of degrees of freedom of this fit
C
          NNOIT=0
          DO 86 I=1,NITC
            IC=IABS(IW(IFICL+LMHLEN+IOITC+I))
            IF (RTABL(KITCO,IC,JITCSZ).GT.900.) NNOIT=NNOIT+1
   86     CONTINUE
          NNOVD=0
          DO 87 I=1,NVDT
            IC=IW(IFVCL+LMHLEN+IOVDT+I)
            IF (RTABL(KVDCO,IC,JVDCSZ).GT.4.) NNOVD=NNOVD+1
            IF (RTABL(KVDCO,IC,JVDCSR).GT.4.) NNOVD=NNOVD+1
   87     CONTINUE
          IF (NVDT.LT.2) THEN
            NSOFF=3
          ELSE
            NSOFF=5
          ENDIF
          NDOF=2*(NITC+NVDT)-NSOFF - NNOIT - NNOVD
          IF (NDOF.LE.1) IERR=9
        ELSE
          IERR=9
        ENDIF
        IF (IERR.GE.4) THEN
C
C++       The fit failed.  Copy the old fit values to the new FRFT bank
C
          DO 367 I=1,LCOLS(IFRFT)
            IW(KROW(KFRFT,ITK)+I)=ITABL(IFRFT,ITK,I)
  367     CONTINUE
        ELSE
C
C++       Successful fit, copy results into the new FRFT bank
C
          DO 60 I=1,6
            RW(KROW(KFRFT,ITK)+JFRFIR-1+I)=HP(I)
   60     CONTINUE
          DO 70 I=1,21
            RW(KROW(KFRFT,ITK)+JFRFEM-1+I)=COV(I)
   70     CONTINUE
          RW(KROW(KFRFT,ITK)+JFRFC2)=CHI2
          IW(KROW(KFRFT,ITK)+JFRFDF)=NDOF
          IW(KROW(KFRFT,ITK)+JFRFNO)=NMULT*100+IOPT*10+IERR
        ENDIF
  700 CONTINUE
      IRET=0
C
C++   Drop the old fit bank
C
      IFRFT=NDROP('FRFT',0)
C
C++   Change the NRs of the old pointer banks, so they become the output
C
C      KFRTL=MSWAP(IW,'FRTL',0,'FRTL',1)
C      IF (IFTCL.NE.0) KFTCL=MSWAP(IW,'FTCL',0,'FTCL',1)
C      IF (IFICL.NE.0) KFICL=MSWAP(IW,'FICL',0,'FICL',1)
C      IF (IFVCL.NE.0) KFVCL=MSWAP(IW,'FVCL',0,'FVCL',1)
C
  999 CONTINUE
C
C++   Drop all the banks if a fatal error has occured
C
      IF (IRET.GT.1) THEN
        CALL BDROP(IW,'FRFTFRTLFTCLFICLFVCL')
        CALL RERROR('FREFIT',6,'Fatal error: dropping all track banks.')
      ENDIF
      END
#endif
