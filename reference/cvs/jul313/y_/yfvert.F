      SUBROUTINE YFVERT(NT,ITLIS,ERRD,ERRZ,IER)
C----------------------------------------------------------
C!   -Perform vertex fit using specified set of tracks
C!    Author   : M. Mermikides,  Sept., 1987
C!    Modified :M.A.Ciocci 16/4/89 from 216 julia version
C!    Modified :- E. Lancon             14-FEB-1992
C!          To make it work with FRFT,FRTL
C!    Modified :- E. Lancon              1-OCT-1992
C!          return if diagonal ERMXV are <0
C!    Modified :- M. Cattaneo           30-OCT-2000
C!          Return if UXSFIT fails
C!
C!    INPUT:
C!             NT         I/   No of tracks to be used in vertex fit
C!             ITLIS(NT)  I/   Row indices in TGFT of tracks to be used
C!             ERRD(NT)   F/   Error on d0 for each track
C!             ERRZ(NT)   F/   Error on Z0 for each track
C!
C!    Description
C!    ===========
C!        For the time being we use a 3-D vertex fitter derived
C!    from D. Saxon (see my note) to find the position and
C!    associated error matrix of the point closest to all tracks
C!    in space.   The results of the vertex fit are stored in
C!    the bank YVXL. The tracks are refitted to include the vertex
C!    point and the updated parameters are stored in bank YTRL.
C!    Please note that this strategy is provisional, and will be
C!    eventually replaced by a something better.
#ifndef DOC
#include "bcs.h"
#include "tpnamc.h"
#include "yynamc.h"
#include "ytrljj.h"
#include "yvxljj.h"
#include "frftjj.h"
#include "frtljj.h"
#include "tpcojj.h"
#include "tparac.h"
#include "tpgpar.h"
#include "tpccon.h"
      LOGICAL FIRST
C
      DIMENSION ITLIS(NT),ERRD(NT),ERRZ(NT)
C
C Working arrays for vertex fitter
      DIMENSION TPAR(6,40),XYZV(3),ERMXV(6)
C Working arrays for track fitter
      DIMENSION XYZT(3,35),ERXY(35),ERZ(35),PARF(6),ERMXT(21)
C
      PARAMETER (NWVXL = 14, NDVXL= 5*NWVXL+2, NXVXL= 3*NWVXL+2)
      PARAMETER (NWTRL = 13, NDTRL=25*NWTRL+2, NXTRL=10*NWTRL+2)
C
C Parameters for vertex fitter   ***** THESE SHOULD GO ON DATA BASE
      DATA NPAR/3/,RSCAT/30./,XCON/0.03/
C Cut on chisq/deg of freedom for accepting vertex fit
      DATA CHCUT/10./
      DATA FIRST / .TRUE. /
C
#include "bmacro.h"
C
      IF ( FIRST ) THEN
         FIRST = .FALSE.
         NAYVXL = NAMIND('YVXL')
         NAYTRL = NAMIND('YTRL')
         NAFRFT = NAMIND('FRFT')
         NAFRTL = NAMIND('FRTL')
         NAFTCL = NAMIND('FTCL')
         NATPCO = NAMIND('TPCO')
      ELSE
      ENDIF
      IER = 0
      KYVXL = IW(NAYVXL)
      KYTRL = IW(NAYTRL)
C
C  Book on first entry to this event the banks YVXL and YTRL for
C  storing results of vertex fit.
C
      IF (KYVXL.EQ.0) THEN
         IW(1) = 1
         CALL AUBOS('YVXL',0,NDVXL,KYVXL,IGARB)
         CALL BLIST(IW,'E+','YVXL')
         IF (IGARB.EQ.2) GO TO 810
         IW(KYVXL + LMHCOL) = NWVXL
         IW(KYVXL + LMHROW) = 0
      ENDIF
C
      IF (KYTRL.EQ.0) THEN
         IW(1) = 1
         CALL AUBOS('YTRL',0,NDTRL,KYTRL,IGARB)
         CALL BLIST(IW,'E+','YTRL')
         IF (IGARB.EQ.2) GO TO 820
         IW(KYTRL + LMHCOL) = NWTRL
         IW(KYTRL + LMHROW) = 0
         IF (IGARB.NE.0) KYVXL = IW(NAYVXL)
      ENDIF
C
C  Do vertex fit
C
      KFRFT = IW(NAFRFT)
C
      DO 80 I= 1,NT
         IT = ITLIS(I)
C M.A.Ciocci modified in accord with the new convenction
C         TPAR(1,I) = 1./RW(KROW(KTGFT,IT) + JTGFIR)
         TPAR(1,I) = -1./RW(KROW(KFRFT,IT) + JFRFIR)
C M.A.Ciocci modified in accord with the new convenction
C         TPAR(2,I) = RW(KROW(KFRFT,IT) + JFRFD0)
C the following line is fixed as suggested by A.Finch
         TPAR(2,I) = RW(KROW(KFRFT,IT) + JFRFD0)*SIGN(1.,-TPAR(1,I))
         TPAR(3,I) = RW(KROW(KFRFT,IT) + JFRFP0)
         TPAR(4,I) = RW(KROW(KFRFT,IT) + JFRFZ0)
         TPAR(5,I) = RW(KROW(KFRFT,IT) + JFRFTL)
         TPAR(6,I) = 0.
   80 CONTINUE
C
      CALL UVPFIT(NT,TPAR,ERRD,ERRZ,NPAR,  NIT,NDF,CHISQ,XYZV,ERMXV)
C
      CHPD = CHISQ/NDF
C
      IF (CHPD.GT.CHCUT.OR.NIT.LT.0) THEN
         IER= 3
         GO TO 999
      ENDIF
      IF ( ERMXV(1).LT.0. .OR. ERMXV(3).LT.0. .OR. ERMXV(6).LT.0. ) THEN
        IER = 3
        GOTO 999
      ENDIF
C
C  Save good fit in YVXL bank
C
      IF(NWVXL*(IW(KYVXL+LMHROW)+1)+LMHLEN.GT.IW(KYVXL)) THEN
         NDATA = IW(KYVXL) + NXVXL
         IW(1) = 1
         CALL AUBOS('YVXL',0,NDATA,KYVXL,IGARB)
         IF (IGARB.EQ.2) GO TO 810
         IF (IGARB.NE.0) THEN
            KYTRL=IW(NAYTRL)
         ENDIF
      ENDIF
      KVX = KNEXT(KYVXL)
C
      IW(KVX + JYVXVT) = 0
      RW(KVX + JYVXXP) = XYZV(1)
      RW(KVX + JYVXYP) = XYZV(2)
      RW(KVX + JYVXZP) = XYZV(3)
      DO 8 I=1,6
         RW(KVX + JYVXEM + I -1) = ERMXV(I)
    8 CONTINUE
      RW(KVX + JYVXCH) = CHISQ
      IW(KVX + JYVXDF) = NDF
      IW(KVX + JYVXOF) = IW(KYTRL+LMHROW)
      IW(KVX + JYVXNT) = NT
      IW(KYVXL+LMHROW) = IW(KYVXL+LMHROW) + 1
C
C  If vertex fit was successful, refit tracks incorporating the vertex
C  point just found in each track.
C
      IF (IER.NE.0) GO TO 999
C
      XYZT(1,1) = XYZV(1)
      XYZT(2,1) = XYZV(2)
      XYZT(3,1) = XYZV(3)
      ERXY(1) = SQRT(1./ERMXV(1)+1./ERMXV(3))
      ERZ(1) = SQRT(1./ERMXV(6))
C
C Extract hits from TPCO bank for fitter
C
      KFRTL = IW(NAFRTL)
      KFTCL = IW(NAFTCL)
      KTPCO = IW(NATPCO)
C
      DO 200 I=1,NT
         IT = ITLIS(I)
         NHIT = IW(KROW(KFRTL,IT) + JFRTNT)
         IOFF=KFTCL+LMHLEN+ITABL(KFRTL,I,JFRTIT)
         DO 210 IH = 1,NHIT
           IC=IW(IOFF+IH)
            R = RW(KROW(KTPCO,IC) + JTPCRV)
            PHI = RW(KROW(KTPCO,IC) + JTPCPH)
            XYZT(1,IH+1) = R*COS(PHI)
            XYZT(2,IH+1) = R*SIN(PHI)
            XYZT(3,IH+1) = RW(KROW(KTPCO,IC) + JTPCZV)
            ERXY(IH+1) = SQRT(RW(KROW(KTPCO,IC) + JTPCSR))
            ERZ(IH+1)  = SQRT(RW(KROW(KTPCO,IC) + JTPCSZ))
  210    CONTINUE
         NXYZ = NHIT + 1
C
         CALL UXSFIT(TPAR(1,I),NXYZ,XYZT,ERXY,ERZ,RSCAT,XCON,BFIELD,
     &            NIT,CHISQ,NPARF,PARF,ERMXT)
CMC Protect against failed fit
         IF( NIT .LT. 0 ) THEN
           IER = 3
           GOTO 999
         ENDIF
CMC
         NDF = 2*NXYZ - NPARF
C
C  Save good fit in YTRL bank
C
         IF(NWTRL*(IW(KYTRL+LMHROW)+1)+LMHLEN.GT.IW(KYTRL)) THEN
            NDATA = IW(KYTRL) + NXTRL
            IW(1) = 1
            CALL AUBOS('YTRL',0,NDATA,KYTRL,IGARB)
            IF (IGARB.EQ.2) GO TO 820
            IF (IGARB.NE.0) THEN
               KYVXL=IW(NAYVXL)
               KFRTL = IW(NAFRTL)
               KFTCL = IW(NAFTCL)
               KTPCO = IW(NATPCO)
            ENDIF
         ENDIF
         KTR = KNEXT(KYTRL)
C M.A.Ciocci modified in accord with the new convenction
C         RW(KTR + JYTRIR) = 1./PARF(1)
         RW(KTR + JYTRIR) = -1./PARF(1)
         RW(KTR + JYTRTL) = PARF(5)
         RW(KTR + JYTRP0) = PARF(3)
C M.A.Ciocci modified in accord with the new convenction
C         RW(KTR + JYTRD0) = PARF(2)
         RW(KTR + JYTRD0) = PARF(2)*SIGN(1.,-PARF(1))
         RW(KTR + JYTRZ0) = PARF(4)
C M.A.Ciocci modified in accord with the new convenction
C  THE INVERSE OF COOVARIANCE MATRIX IS RELIED
C  (SEE ALEPH NOTE) TO -Q/RHO,LAMBDA,PHI
         RW(KTR + JYTREM) = ERMXT(1)
         RW(KTR + JYTREM + 1) = ERMXT(11)
         RW(KTR + JYTREM + 2) = ERMXT(15)
         RW(KTR + JYTREM + 3) = ERMXT(4)
         RW(KTR + JYTREM + 4) = ERMXT(13)
         RW(KTR + JYTREM + 5) = ERMXT(6)
         IW(KTR + JYTRVN) = IW(KYVXL+LMHROW)
         IW(KTR + JYTRTN) = IT
         IW(KYTRL+LMHROW) = IW(KYTRL+LMHROW) + 1
  200 CONTINUE
      GO TO 999
C
  810 IER = 2
      CALL REPORT('YFVERT','Error booking/extending bank YVXL',0)
      GO TO 999
  820 IER = 2
      CALL REPORT('YFVERT','Error booking/extending bank YTRL',0)
  999 RETURN
      END
#endif
