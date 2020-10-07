        SUBROUTINE ECHTAG ( NETP1 , ITAG , TPAR )
C----------------------------------------------------------------------
C!  - Tags track quality
C!
C!    Authors : M.N. Minard , M. Pepe       26/02/89
C!    Modified: M.N  Minard                 04/01/91
C!
C! Input       NETP1 = Track number
C! Output      ITAG  = 1 Good track from vertex
C!                     3 Good track from secondary V0
C!                     101 or 103 too low momentum
C!                     others not a good track
C?
C!======================================================================
#ifndef DOC
#include "etp1jj.h"
#include "frftjj.h"
#include "frtljj.h"
#include "eftdjj.h"
#include "ectcom.h"
#include "rparac.h"
#include "bcs.h"
         EXTERNAL UFITQL
         INTEGER UFITQL
         DIMENSION TPAR(4)
#include "bosext.h"
         DATA   IFIRST / 0 /
#include "bmacro.h"
C
C-       At first pass read selection title from bank EFTD
C
         KEFTD = IW(NAMIND('EFTD'))
         IF ( IFIRST.EQ.0 ) THEN
           JEFTD = KEFTD +LMHLEN
           NPTMIN = IW (JEFTD+JEFTNP)
           D0MAX  = RW (JEFTD+JEFTD0)
           Z0MAX  = RW (JEFTD+JEFTZ0)
           PMIN   = RW (JEFTD+JEFTPM)
           PMAX   = RW (JEFTD+JEFTPX)
           CNSIG  = RW (JEFTD+JEFTCG)
           IFIRST = 1
         ENDIF
C
C-       Set flags to select track quality
C
         NAFRFT  = NAMIND('FRFT')
         KFRFT = IW (NAFRFT)
      IF (KFRFT.LE.0) THEN
        CALL RERROR('ECHTAG',1,'No FRFT Bank')
        GO TO 999
      ENDIF
         NAFRTL  = NAMIND('FRTL')
         KFRTL = IW (NAFRTL)
         IF (KFRTL.LE.0) THEN
           CALL RERROR('ECHTAG',1,'No FRTL Bank')
           GO TO 999
         ENDIF
         NAETP1 = NAMIND('ETP1')
         KETP1 = IW (NAETP1)
      IF (KETP1.LE.0) THEN
        CALL RERROR('ECHTAG',2,'No ETP1 Bank')
        GO TO 999
      ENDIF
C
         ITAG  = 99
         NOPT  = 0
C
C-       Select track
C
         JFRFT = KROW ( KFRFT,NETP1 )
         RIR   = RW   ( JFRFT+JFRFIR)
         D0    = RW   ( JFRFT+JFRFD0)
         Z0    = RW   ( JFRFT+JFRFZ0)
         C2    = RW   ( JFRFT+JFRFC2)
         NDF   = IW   ( JFRFT+JFRFDF)
         TLAM  = RW   ( JFRFT+JFRFTL)
C
C-       Calculate track # of points
C
         NPTS =  ITABL(KFRTL,NETP1,JFRTNT)+ITABL(KFRTL,NETP1,JFRTNR)
         TPAR(1) = FLOAT(NPTS)
         TPAR(2) = D0
         TPAR(3) = Z0
         TPAR(4) = C2
         IF ( NDF.GT.0 ) THEN
           TPAR(4) = C2/FLOAT(NDF)
         ENDIF
         PMOM  = 0.
         IF (ABS(RIR).GT.0 ) THEN
            PMOM =  AAAKEC*FILDEC*(1.+TLAM**2)/ABS(RIR)
         ENDIF
C
C
C-       Not a good track
C

         NOPT = UFITQL(NETP1)
         ERPMAX = 0.0013*PMOM**2
         IF ( NOPT.EQ.2.AND.PMOM.GT.PMAX+3*ERPMAX) NOPT = 4
         IF ( NOPT.EQ.2.AND.PMOM.LT.PMAX+3*ERPMAX) NOPT = 1
         IF (PMOM.LT.3*PMIN) NOPT = NOPT+100
         ITAG = NOPT
         IF(NOPT.NE.1.AND.NOPT.NE.3) THEN
          CALL HFILL(JULEF+7,FLOAT(NOPT),DUM,1.)
         ELSE
C
C-       Good track
C
          ITAG = NOPT
         ENDIF
C
 999  CONTINUE
         RETURN
         END
#endif
