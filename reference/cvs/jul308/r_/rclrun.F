      SUBROUTINE RCLRUN
C----------------------------------------------------------------------
C!  - Close this run
C!
C!  Created by   J. Bunn
C!
C!  Modified by  S. Orteu               Date  19-JAN-1987
C!   Modified :- E. Lancon             30-AUG-1993
C!            write SUMR banks with nr = run_number
C!               F. Ranjard            25-APR-1994
C!            remove call to XTRSFI
C!               M. Cattaneo              JUN-1996
C!            add call to BMSPOT
C?
C!======================================================================
#ifndef DOC
#include "rparac.h"
#include "bcs.h"
#include "ravtim.h"
#include "rstate.h"
#include "rstatr.h"
#include "rcurnt.h"
#include "rtimes.h"
#include "rlunit.h"
#include "rflags.h"
#include "jsumjj.h"
      EXTERNAL NLIST, NAMIND
      CHARACTER FNAME*120,FTYPE*8,FDEVI*80
      CHARACTER*4 BNAME, NLIST
C--------------------------------------------------------------------
C     TPC Summary, write monitoring bank
C
      IF(FDETRF(JULTP))CALL TPCRUN
C
C
C Call user's close run
C
      CALL USCRUN
C
      IF(FDETRF(JULBC))CALL BCCRUN
      IF(FDETRF(JULBO))CALL OMCRUN
      IF(FDETRF(JULSI))CALL SICRUN
      IF(FDETRF(JULVD))CALL VDCRUN
      CALL VBSCLR
      CALL OLSPOT(1)
      IF(FDETRF(JULFA))CALL RFALCR
C
      IF(FDETRF(JULLC))CALL LPRLUM
C
      NRUNRS(1) = NRUNRS(1) + 1
      IF (ANYORL) NRUNRS(2)=NRUNRS(2)+1
      NRUNRS(4) = NRUNRS(4) + NEVTRS(1)
      NRUNRS(5) = NRUNRS(5) + NEVTRS(2)
      NEVTRR(1) = NEVTRR(2)+NEVTRR(3)+NEVTRR(4)+NEVTRR(5)
      NRUNRR(6) = NRUNRR(6) + NEVTRR(2)
      NRUNRR(7) = NRUNRR(7) + NEVTRR(3)
      NRUNRR(8) = NRUNRR(8) + NEVTRR(4)
      NRUNRR(9) = NRUNRR(9)+ NEVTRR(5)
      TIMJRA    = TIMJRA + STIMRA
C
C compute mean time per event
C
      TPEVT = 0.
      IF (NEVTRS(1).GT.0) TPEVT = STIMRA/FLOAT(NEVTRS(1))
      NEVTRS(3) = NINT(TPEVT*1000.0)
C
C?   In case of POT-POT repro fill JSUM with some infos. from UMJS
C?   (Copy of previous JSUM bank saved in RINRUN)
C
      KJSUM = IW (NAMIND('JSUM'))
      KUMJS = IW (NAMIND('UMJS'))
      IF ( FREPRO .AND. KJSUM.NE.0 .AND. KUMJS.NE.0 ) THEN
C...  Raw Data Input size
        RW (KJSUM+LMHLEN+JJSULI) = RW (KUMJS+LMHLEN+JJSULI)
C...  Raw data Input size for Z events
        RW (KJSUM+LMHLEN+JJSUIZ) = RW (KUMJS+LMHLEN+JJSUIZ)
C...  Ecal Storeys killed by cleaning
        IW (KJSUM+LMHLEN+JJSUKB) = IW (KUMJS+LMHLEN+JJSUKB)
        IW (KJSUM+LMHLEN+JJSUKW) = IW (KUMJS+LMHLEN+JJSUKW)
C...  Vertex position
        IF ( .NOT.FDETRF(JULYR) ) THEN
          IW(KJSUM+LMHLEN+JJSUNV) = IW(KUMJS+LMHLEN+JJSUNV)
          RW(KJSUM+LMHLEN+JJSUXV) = RW(KUMJS+LMHLEN+JJSUXV)
          RW(KJSUM+LMHLEN+JJSUYV) = RW(KUMJS+LMHLEN+JJSUYV)
          RW(KJSUM+LMHLEN+JJSUZV) = RW(KUMJS+LMHLEN+JJSUZV)
          RW(KJSUM+LMHLEN+JJSUXS) = RW(KUMJS+LMHLEN+JJSUXS)
          RW(KJSUM+LMHLEN+JJSUYS) = RW(KUMJS+LMHLEN+JJSUYS)
          RW(KJSUM+LMHLEN+JJSUZS) = RW(KUMJS+LMHLEN+JJSUZS)
        ENDIF
C...  Number of Z
        IF ( .NOT.FDETRF(JULYR) .OR. .NOT.FDETRF(JULEC) .OR.
     &      .NOT.FDETRF(JULHC)  ) THEN
          IW(KJSUM+LMHLEN+JJSUNZ) = IW(KUMJS+LMHLEN+JJSUNZ)
          IW(KJSUM+LMHLEN+JJSUVZ) = IW(KUMJS+LMHLEN+JJSUVZ)
        ENDIF
      ENDIF
      IF (KUMJS.NE.0) CALL BDROP (IW,'UMJS')
C
C?   Write JSUM,... banks
C
C?   If SUMR card exist, write T list to SUMR file
C?   Else put end of job stuff to POT
C
      CALL BLIST(IW,'T=',SUMLIS (1:LNBLNK(SUMLIS)) )
C
C?   In case of reprocessing some banks (not redefined in this job
C?   taken from input )of SUMLIS have nr = run number
C?   as set by falcon, they should have nr = 0 as for normal processing
C?   falcon task will reset the bank number to the run number
C?   one does not need to do that outside falcon
C
      KTYP=IW(NAMIND('SUMR'))
      IF ( FREPRO .AND. KTYP.NE.0 ) THEN
        K = 0
   10   K = K + 1
        BNAME = NLIST (IW,K,'T')
        IF (BNAME.EQ.'    ') GOTO 11
        KNAME = IW(NAMIND(BNAME))
        IF ( KNAME.NE.0 ) IW(KNAME-2) = IRUNRC
        GOTO 10
      ENDIF
   11 CONTINUE
C
      IF(KTYP.NE.0)THEN
        CALL ACDARG('SUMR','NATIVE','A',FNAME,FTYPE,FDEVI,IER)
        CALL AOPENW(LSUMRL,FNAME,FTYPE,FDEVI,IER)
        CALL BWRITE(IW,LSUMRL,'T')
        CALL BWRITE(IW,LSUMRL,'0')
      ELSE
        DO 1 I=1,NWRTMX
          IF(JOUTRL(I).NE.NOOUTP)THEN
            CALL ABWSEL ('T')
          ENDIF
    1   CONTINUE
      ENDIF
      CALL BLIST ( IW, 'T=', '0')
C
C?   If PASS0 write TPC drift velocity bank
C?                  and VDET Hot Chanels bank
C
      IF ( FPASS0 ) CALL RCPAS0
C
C?   Time Spent In This Run
C
      CALL ALTIME(ARNFRT)
      TIME = ARNFRT-ARNSRT
C
C?  Print statistics
C
      CALL RPRSTA(TIME)
C
  999 CONTINUE
      END
#endif
