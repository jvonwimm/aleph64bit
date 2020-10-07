      SUBROUTINE RPOTCL
C-------------------------------------------
C!    Clean up the POT before output
C       J.Knobloch 16-OCT-1989
C!   Modified :- E. Lancon             14-JUN-1991
C!              remove dropping of VDET banks in case of less than two g
C!              tracks the code has been moved to RECEVT
C!               F. Ranjard            25-APR-1994
C!              remove call to XTRSEV
C!               M. Cattaneo           23-APR-1999
C!              drop BLUM on all but random triggers
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rcurnt.h"
#include "rflags.h"
      INTEGER RTLSUM
      INTEGER NAPFRF / 0 /
      LOGICAL TLCAL,TRAND,TPHYS,TSATR,FAIL,TSCAL(4)
#include "bmacro.h"
C
C     compress BCAL banks
      CALL BCPACK
C
C      are there tracks???
C
      IF( NAPFRF .EQ. 0 ) THEN
        NAPFRF = NAMIND('PFRF' )
      ENDIF

      NTRK=0
      KPFRF=IW(NAPFRF)
      IF(KPFRF.NE.0)THEN
        NTRK=IW(KPFRF+2)
      ENDIF
C    DROP VDET FOR 1989 data
      IF(IRUNRC.GT.4000.AND.IRUNRC.LT.5910)
     +                CALL BDROP(IW,'VDDI')
C
C      get trigger class
C
      CALL RTRIGF(IRUNRC,TLCAL,TRAND,TPHYS,TSATR,FAIL)
      IF(.NOT.FAIL)THEN
C
C    Dropping detector banks for lcal only triggers
C
        IF(TLCAL.AND..NOT.TRAND.AND..NOT.TPHYS.AND.NTRK.EQ.0)THEN
          JEBIRF(1)=IBSET(JEBIRF(1),30)
          CALL BDROP(IW,'DMJTPEIDPEPTPHCOPHHYPHST')
          CALL BDROP(IW,'PMSKPRPWPRTMPTNCX2DFETDI')
          CALL BDROP(IW,'HTUBPCHYPCPAPEHYPEST')
          CALL BDROP(IW,'PEWIPPDSPPOBPPRLPTBCX1ADECTE')
          CALL BDROP(IW,'FRIDHMADPIDIPFRFPFRTPTEXPYER')
          CALL BDROP(IW,'PYFRVPLHVHLSERRFMHITDHEA')
          CALL BDROP(IW,'HCTEMUDG')
        ENDIF
      ENDIF
C    Dropping detector banks for Sical only triggers
      CALL STRIGF(KRUN,TLCAL,TRAND,TPHYS,TSCAL,FAIL)
      IF(.NOT.FAIL)THEN
        IF(TSCAL(1).AND..NOT.TRAND.AND..NOT.TPHYS.AND..NOT.TLCAL.AND.
     $    NTRK.EQ.0)THEN
          JEBIRF(1)=IBSET(JEBIRF(1),30)
          CALL BDROP(IW,'DMJTPEIDPEPTPHCOPHHYPHST')
          CALL BDROP(IW,'PMSKPRPWPRTMPTNCX2DFETDI')
          CALL BDROP(IW,'HTUBPCHYPCPAPEHYPEST')
          CALL BDROP(IW,'PEWIPPDSPPOBPPRLPTBCX1ADECTE')
          CALL BDROP(IW,'FRIDHMADPIDIPFRFPFRTPTEXPYER')
          CALL BDROP(IW,'PYFRVPLHVHLSERRFMHITDHEA')
          CALL BDROP(IW,'HCTEMUDGLUPALOLE')
        ENDIF
      ENDIF
C
C    Dropping detector banks for all but random triggers
      IF(.NOT.FAIL)THEN
        IF( .NOT. TRAND ) THEN
          CALL BDROP(IW,'BLUM')
        ENDIF
      ENDIF

C     Drop RTLO banks except for the Nr=0
C
C         THE VALUE 6871 (=1AD7 HEX)
C         is asking for triggers,ITC,ECAL,HCAL,LCAL,TPC-a,TPC-b
C                       in the readout
C           this number should be derived from the raw data as
C           soon as available        (J.Knobloch 7-2-90)
      ISTAT=RTLSUM(6871,IW)
      RETURN
      END
#endif
