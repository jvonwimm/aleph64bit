      SUBROUTINE RWRUNH
C----------------------------------------------------------------------
C! Write Run record to output streams
C!
C!  Author       S. Orteu   5-MAR-1987
C!  Modified    J. Boucrot 28-MAR-1989
C!   Modified :- E. Lancon              9-APR-1991
C!                      JSUM removed From BDROP
C!   Modified :- E. Lancon             16-OCT-1991
C!                      JCON added to C list
C!   Modified :- E. Lancon              2-OCT-1991
C!                      TDVV not written to run record
C!   Modified :- E. Lancon             28-SEP-1993
C!                      Bug when handling JCAR bank
C!   Modified :- P. Comas              28-MAR-1996
C!                      RUNE added to the C list
C?
C!======================================================================
#ifndef DOC
#include "bcs.h"
#include "rflags.h"
#include "rlunit.h"
#include "rparac.h"
#include "rjulver.h"
      CHARACTER SLIST*80
      INTEGER  AGETDB,IRET
      EXTERNAL AGETDB
      LOGICAL FIRST
      DATA FIRST  / .TRUE. /
C--------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST=.FALSE.
        IVJUL=INT(RJVERS)
        IVCOR=NINT(100*RJVERS)
      ENDIF
C
      CALL BLIST(IW,'C+','RHAHJCAREGLOHWNOHNTOZPFRHSTDHT0CJCONPTUN')
C?   In any case do not write TDVV
      CALL BLIST(IW,'C-','TDVV')
C- Do not writte JSUM... banks on output in case of reprocessing
C- on FALCON only
      IF ( FREPRO .AND. IW(NAMIND('SUMR')).NE.0  )
     &  CALL BLIST(IW,'C-',SUMLIS (5:LNBLNK(SUMLIS)) )
C- re-add Run header banks in anycase
      CALL BLIST(IW,'C+','RUNRRUNHRUNE')
C
C?   Loop over all JCAR banks get the highest bank number : nrj ,
C?   create a JCAR bank with nr = nrj + 1 from ARJC bank (see RINJOB)
C
      KARJC = IW( NAMIND('ARJC') )
      KJCAR = IW( NAMIND('JCAR') )
      IF ( KARJC.GT.0 ) THEN
        NRJ   = -1
        NRA   = IW(KARJC-2)
        IF ( KJCAR.GT.0 ) THEN
  100     NRJ   = IW(KJCAR-2)
          KJCAR = IW(KJCAR-1)
          IF (KJCAR.NE.0) GOTO 100
        ENDIF
        IND = MSWAP (IW, 'ARJC', NRA, 'JCAR', NRJ+1 )
      ENDIF
C
C    create an artificial RUNH bank if missing
C
      KRUNR=IW(NAMIND('RUNR'))
      NRUN=IW(KRUNR+2)
      IF(KRUNR.NE.0.AND.IW(NAMIND('RUNH')).EQ.0)THEN
        NEXP=IW(KRUNR+1)
        CALL AUBOS('RUNH',0,5,KRUNH,IGARB)
        IF ( IGARB.EQ.2 ) THEN
          CALL RERROR ('RWRUNH',-1,'Not enough space to book RUNH !!!')
        ELSE
          CALL BLIST(IW,'C+','RUNH')
          IW(KRUNH+1)=NEXP
          IW(KRUNH+2)=NRUN
        ENDIF
      ENDIF
      IF (FREPRO) THEN
        CALL BDROP(IW,'HNTOHWNO')
        IRET = AGETDB('HNTOHWNO',-NRUN)
      ENDIF
C
      DO 1 I=1,NWRTMX
        IF (JOUTRL(I).NE.NOOUTP) THEN
          CALL BKRHAW('JULIA   ',IVJUL,IVCOR,I+1,IFAIL)
          IF (IFAIL.EQ.1) THEN
            CALL RERROR('RWRUNH',1,'No room to book RHAH')
          ENDIF
          IF (FDEBRF.AND.JDBDRF(JULSK).NE.0) CALL PRRHAH
          CALL ABWSEL ('C')
        ENDIF
    1 CONTINUE
      IF (FDEBRF.AND.JDBDRF(JULSK).GE.3) THEN
        WRITE(LOUTRL,'(A)') '0RWRUNH- List of banks written'
        CALL AUBLIS('C')
      ENDIF
      END
#endif
