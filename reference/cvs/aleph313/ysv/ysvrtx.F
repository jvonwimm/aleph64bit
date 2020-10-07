      SUBROUTINE YSVRTX(IER)
C-----------------------------------------
C!  Steering for secondary vertex search
C   Builds vertices, applies cuts on material regions,
C   creates an association list vertex->track, and
C   fills banks YNVH and YNLI with results.
C
C   Temporary fix: YSIN card disables the flagging of
C                  incoming tracks, to avoid crashing Dali.
C
C   IER = 0 if successful
C
C   Author   :- D. Casper 30-jun-1996
C   Modified :- D. Casper 30-sep-1997
C               Remove cut on max number of FRFT tracks
C
C   Input banks:  FRFT  Track fits
C                 YSCT  Vertex fitting parameters from Dbase
C                 PYER  Primary vertex (needed by routines called)
C
C   Output banks: YNLI  List of vertices found
C                 YNVH  Tracks identified with a vertex
C                 YSMO  Momentum and other information about secondaries
C
C=========================================
#ifndef DOC
C +
C Declarations.
C -

#include "frftjj.h"
#include "frtljj.h"
#include "ynvhjj.h"
#include "ynlijj.h"
#include "ysctjj.h"
#include "ysvxjj.h"
#include "ysvtjj.h"
#include "ysmojj.h"
#include "ysvcut.h"
#include "bcs.h"
#include "bosext.h"
      INTEGER  ALGTDB
      EXTERNAL ALGTDB
      SAVE NFRFT, NPYER, NYSCT, NYSVX, NYSVT, NYNVH, NFRTL, NYSIN
      DATA LRUN/0/, NYSCT/0/
      
#include "bmacro.h"


C + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
C Entry Point.
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      IF (NYSCT.EQ.0) THEN
        NFRFT = NAMIND('FRFT')
        NPYER = NAMIND('PYER')
        NYSCT = NAMIND('YSCT')
        NYSVX = NAMIND('YSVX')
        NYSVT = NAMIND('YSVT')
        NYNVH = NAMIND('YNVH')
        NFRTL = NAMIND('FRTL')
        NYSIN = NAMIND('YSIN')
        NYNLI = NAMIND('YNLI')
      ENDIF

C Drop output banks if they exist

      CALL BDROP(IW,'YNVHYNLIYSMO')

C Check input banks

      KFRFT = IW(NFRFT)
      KFRTL = IW(NFRTL)
      KPYER = IW(NPYER)
      IF (KFRFT.EQ.0 .OR. KFRTL.EQ.0 .OR. KPYER.EQ.0) THEN
        IER = 1
        GO TO 999
      ENDIF

C+ check constants

      CALL ABRUEV(KRUN,KEVT)
      IF (KRUN.NE.LRUN) THEN
        ISTAT = ALGTDB(JUNIDB(0),'YSCT',KRUN)
        IF (ISTAT .EQ. 0) THEN
            IER = 2
            GOTO 999
        ENDIF
        LRUN = KRUN
      ENDIF
      KYSCT = IW(NYSCT)
      IF (IW(KFRFT-2).EQ.2) THEN
          IYSCT = KROW(KYSCT,1)
      ELSE
          IYSCT = KROW(KYSCT,2)
      ENDIF
      YSCTPV = 10.**(RW(IYSCT+JYSCPV))
      YSCTSM = RW(IYSCT+JYSCSM)
      YSCTC2 = 10.**(RW(IYSCT+JYSCC2))
      YSCTRL = RW(IYSCT+JYSCRL)**2
      YSCTRH = RW(IYSCT+JYSCRH)**2
      YSCTDR = RW(IYSCT+JYSCDR)
      YSCTCF = 10.**(RW(IYSCT+JYSCCF))
      YSCTRM = RW(IYSCT+JYSCRM)
      YSCTCR = RW(IYSCT+JYSCS1)

C Check there are any charged tracks

      IF (LROWS(KFRFT).EQ.0) THEN
        IER = 4
        GO TO 999
      ENDIF

C Identify the vertices in this event

      CALL YSVBLD(IER)
      IF (IER.NE.0) GOTO 999

C Veto vertices which are not in material, if appropriate

      IF (IW(KFRFT-2).EQ.0) CALL YSVMAT

C Make the vertex->track association bank

      CALL YSVTBK(NVX,NTK)

C  Create the terse output banks

      CALL AUBOS('YNVH',0,LMHLEN + NTK*LYNVHA,KYNVH,IGARB)
      IF (IGARB.EQ.2) THEN
        IER = 5
        GO TO 999
      ENDIF
      CALL BLIST(IW,'E+','YNVH')
      IW(KYNVH + LMHCOL) = LYNVHA
      IW(KYNVH + LMHROW) = 0

      CALL AUBOS('YNLI',0,LMHLEN+NVX*LYNLIA,KYNLI,IGARB)
      IF (IGARB.EQ.1) THEN
        KYNVH = IW(NYNVH)
      ELSE IF (IGARB.EQ.2) THEN
        IER = 6
        GO TO 999
      ENDIF
      CALL BLIST(IW,'E+','YNLI')
      IW(KYNLI + LMHCOL) = LYNLIA
      IW(KYNLI + LMHROW) = 0
C
C+  Fill the output banks
C
      KYSVX = IW(NYSVX)
      DO WHILE (KYSVX .NE. 0 .AND. IW(KYSVX-2).LE.-2)
        DO I = 1, LROWS(KYSVX)
            IYNLI = KNEXT(KYNLI)
            CALL UCOPY(RW(KROW(KYSVX,I)+JYSVVX),RW(IYNLI+JYNLXP),3)
            RW(IYNLI+JYNLCH) = RTABL(KYSVX,I,JYSVC2)
            IW(KYNLI+LMHROW) = IW(KYNLI+LMHROW) + 1
        ENDDO
        KYSVX = IW(KYSVX-1)
      ENDDO
      KYSVT = IW(NYSVT)
      IVTX = 1
      CALL BLIST(IW,'E+','YSMO')
      DO WHILE (KYSVT .NE. 0 .AND. IW(KYSVT-2).LE.-2)
        NR = IABS(IW(KYSVT-2))
C+ Book the required number of new YSMO banks
        NBOOK = LROWS(KYSVT)/NR
        DO IBOOK = IVTX, IVTX+NBOOK-1
            CALL AUBOS('YSMO',IBOOK,LMHLEN+LYSMOA*NR,KYSMO,IGARB)
            IF (IGARB.EQ.2) THEN
                IER = 7
                RETURN
            ELSE IF (IGARB.EQ.1) THEN
                KYSVT = NLINK('YSVT',-ABS(NR))
                KYNVH = IW(NYNVH)
                KYNLI = IW(NYNLI)
            ENDIF
            IW(KYSMO+LMHCOL) = LYSMOA
            IW(KYSMO+LMHROW) = 0
        ENDDO
        KYSMO = NLINK('YSMO',IVTX)
        DO I = 1, LROWS(KYSVT)
C+ Fill YNVH
          IYNVH = KNEXT(KYNVH)
          ITRK = ITABL(KYSVT,I,JYSVTN)
C+ See if this track as identified as incoming or outgoing
          IW(IYNVH+JYNVVN) = IVTX
          IF(ITABL(KYSVT,I,JYSVIO).LT.0) IW(IYNVH+JYNVVN) = -IVTX
          IW(IYNVH+JYNVTN) = ITRK
          IW(KYNVH+LMHROW) = IW(KYNVH+LMHROW) + 1
C+ Fill YSMO
          IYSMO = KNEXT(KYSMO)
          CALL UCOPY(IW(KROW(KYSVT,I)+JYSVTN),IW(IYSMO+JYSMTN),LYSMOA)
          IW(KYSMO+LMHROW) = IW(KYSMO+LMHROW)+1
          IF (MOD(I,NR).EQ.0) THEN
            IVTX = IVTX + 1
            KYSMO = NLINK('YSMO',IVTX)
          ENDIF
        ENDDO
        KYSVT = IW(KYSVT-1)
      ENDDO
      IF (NVX .NE. LROWS(KYNLI) .OR. NTK.NE.LROWS(KYNVH)) THEN
        IER = 8
        GOTO 999
      ENDIF
      IER = 0
 999  CONTINUE

      CALL AUBPRS('YNVHYNLIYSMO')

      RETURN
      END
#endif
