*DK asijob
      SUBROUTINE ASIJOB
C ----------------------------------------------------------------------
C. - F.RANJARD - 850325
C! Job initialization
C.  Modified     B.Bloch                  May 2001
C         call ADBCHK to check against DB official production version
C. - called by   GALEPH
C. - calls       ASINIT, ASREDC, ASREAD, ASIMOD          from this .HLB
C.               ASIGEA, AGEOME, ASIPAC, USIJOB          from this . HLB
C.               AOPDBS, ADBVER                          from ALEPHLIB
C                GINIT, GPMATE, GPTMED, GPROTM, GPVOLU   from GEANT
C.               TIMED                                   from KERNLIB
C.               ADBCHK                                  from ALEPHLIB
C---------------------------------------------------------
#ifndef DOC
      SAVE
#include "iocom.h"
#include "jobcom.h"
#include "alfgeo.h"
#include "bcs.h"
#include "asimjj.h"
C ----------------------------------------------------------------------
C - Keep initial time
C
      TLIM = 3600.
      CALL TIMEST (TLIM)
      CALL TIMED(TIMEJO(1))
C
C - Initialize flags and constants
C
      CALL ASINIT
C
C - Read STEERING data cards to set necessary flags for this job
C
      CALL ASREDC
C
C - Build ASIM with date of the geometry
C
      CALL AUBOS ('ASIM',0,LASIMA+LMHLEN,JASIM,IGARB)
      IF (JASIM.EQ.0) CALL ALTELL ('ASIJOB: no space for ASIM bank',0,
     &                             'STOP')
      IW(JASIM+LMHCOL) = LASIMA
      IW(JASIM+LMHROW) = 1
      IW(JASIM+LMHLEN+JASIYM) = IDATJO
      CALL BKFMT ('ASIM','I')
C
C - Do not compress output file
C
      JCOMP = IW(NAMIND('COMP'))
      IF (JCOMP.EQ.0) THEN
         CALL AUBOS ('COMP',0,1,JCOMP,IGARB)
         IF (JCOMP.EQ.0) CALL ALTELL ('ASIJOB: no space for COMP bank',
     &                                 0,'STOP')
         IW(JCOMP+1) = INTCHA ('NONE')
      ENDIF
C
C - check coherence of data cards
C     you cannot read the KINEmatics and PROCess KINEmatics
      IF (MGETJO.NE.0 .AND. IPROJO(1).NE.0) THEN
        CALL ALTELL ('ASIJOB: GET and PROC KINE are incompatible',0,
     &                'STOP')
      ENDIF
C     check the mag.field level
      IF (ABS(ALFIEL) .GT. 20.) THEN
       WRITE(LOUTIO,*)' +++ASIJOB +++ the mag.field is greater than',
     &                ' 20KGauss. Is that normal ?'
      ENDIF
C
C     set parametrization flag and bank format
      FPARJO = ICECJO(5).EQ.2
      IF (FPARJO) CALL BKFMT ('CAPA','4I,F')
C
C - print geometry date
C
      WRITE (LOUTIO,*)' +++ASIJOB+++ geometry date ', IDATJO
C
C - Open DAF file
C
      LRDBIO = JUNIDB(0)
      TFILIO(4) = ' '
      CALL AOPDBS (TFILIO(4),IRETD)
      IF (IRETD.NE.0) THEN
        CALL AWERRC (LOUTIO,'AOPDBS',TFILIO(4),IRETD)
        CALL ALTELL('ASIJOB :DB not found reported by AOPDBS ',
     &        0,'STOP')
      ENDIF
      CALL ADBVER(IDAFJO,IDCHJO)
      call adbchk(idafjo)   ! check on version # for official prod
      IF (IDAFJO.LT.300) THEN
         WRITE(LOUTIO,*)' +++ASIJOB+++ DB ',IDAFJO,
     &   ' is NOT Y2K compliant - please use DB >= 300'
         CALL ALTELL('ASIJOB : DB not Y2K compliant ',0,'STOP')
      ENDIF 
C
C - read run record and get  the run #, PART, GDEC, ASIM banks
C
      CALL ASREAD
C
C - Initialize GEANT3 and get the GEANT version #
C
      CALL GINIT
C
C - Initialize various modules (detectors, trigger) depending on
C   the run # and the GEANT version #.
C
      CALL ASIMOD
C
C - Interface to GEANT3 : set GEANT3 flags
C
      CALL ASIGEA
C
C - Build the geometry in GEANT3
C
      CALL AGEOME
C
C - Initialize various packages used during this run
C
      CALL ASIPAC
C
C - print out materials , media , rotation matrices ,volumes
C
      IF (IPRIJO(19).EQ.1) THEN
        CALL GPMATE(0)
        CALL GPTMED(0)
        CALL GPROTM(0)
        CALL GPVOLU(0)
      ENDIF
C
C - write run record if required
C
      CALL ASIRUN
C
C - Call USER routine
      CALL USIJOB
C
      RETURN
      END
#endif
