      SUBROUTINE VDSMAP(IROMD,GOOD)
C----------------------------------------------------------------------
C!  - Performs the map from electronics channels to strip numbers
C!
C
C  This subroutine takes as input hits from a single wafer
C  and produces as output an edited version of same,
C  with address mapped onto the actual (integer) strip numbers,
C  unmapped channels removed, and special case strips flaged.
C  D. Brown, 9-4-90
C  Modified 16-1-94 by Dave Brown for the VDET upgrade
C  Modified March 1995 A. Bonissent, M. Thulasidas
C                 reorganise and debug
C
C  INPUT  :
C      IROMD   :  Readout module address
C  OUTPUT :
C      GOOD    :  Flag if everything went OK
C
C  Upgraded 13-1-94 for new vdet    D. Brown
C?
C!======================================================================
#ifndef DOC
C
C  Inputs
C
      INTEGER IROMD
      LOGICAL GOOD
C
C  Parameters
C
C
C  Global includes
C
#include "bcs.h"
#include "vcsgjj.h"
#include "vdflgs.h"
#include "vpecjj.h"
#include "vregjj.h"
#include "vganjj.h"
#include "vpesjj.h"
#include "vrecon.h"
C
C  Function calls
C
      INTEGER NLINK
      INTEGER NAMIND
      INTEGER VNELRM, VNSCRM
C
C  Local variables
C
      INTEGER KVCSG,NVCSG,NAVCSG
      INTEGER NCHAN,NSTRP, NROST
      INTEGER KLIN
      INTEGER ILAY,IZ,IPH,IVIEW
      INTEGER IRWFL,IVREG
      INTEGER IOFL,INFL
      INTEGER KVREG,KVPEC,KVGAN,KVPES
      INTEGER IVCSG,JSTRP,ISTRP
      INTEGER IVGAN,NVGAN,IFSGN,ILSGN
      INTEGER JFLAG,PCFLG,PSFLG
      INTEGER NVREG,IFCRG,ILCRG
      INTEGER IVPEC,NVPEC,PCHAN
      INTEGER NVPES,IVPES,PSTRP
      INTEGER OPULS,IPULS
      REAL GAIN
C
C  Variables for inline functions
C
      LOGICAL SUNMP,SZERO,OSUPP,OOVER,OEROR,FIRST
      INTEGER I,J
      DATA FIRST /.TRUE./
#include "bmacro.h"
C
C  Inline functions
C
      SUNMP(J) = IAND(J,VBUNMP).EQ.VBUNMP
      SZERO(J) = IAND(J,VBUNBD+VBUNUS).GT.0
      OSUPP(J) = IAND(J,OBSUPP).EQ.OBSUPP
      OOVER(J) = IAND(J,OBOVER).EQ.OBOVER
      OEROR(J) = IAND(J,OBEROR).EQ.OBEROR
      IF(FIRST)THEN
        FIRST=.FALSE.
        NAVCSG=NAMIND('VCSG')
      ENDIF
C
C  Start of code
C
      GOOD = .TRUE.
C
C Decode address,
C get the view and the number of readout channels
C
      CALL VADEWA(IROMD,ILAY,IZ,IPH,IVIEW)
      NCHAN = VNELRM(IVIEW)
      KVCSG=IW(NAVCSG)
      NVCSG=LROWS(KVCSG)
C
      IF(MCEVNT)THEN
C
C  In MC data, no mapping needs to be done.  Just copy over.
C
        DO IVCSG=1,NCHAN
          KLIN=KROW(KVCSG,IVCSG)
          RW(KLIN+JVCSMP)=ITABL(KVCSG,IVCSG,JVCSRP)
          IW(KLIN+JVCSMF)=ITABL(KVCSG,IVCSG,JVCSRF)
        ENDDO
        GOTO 999
      END IF
      DO 10 IVCSG=1,NCHAN
C
C Get the raw flag
C
        IRWFL = ITABL(KVCSG,IVCSG,JVCSRF)
        IF (IRWFL.NE.0)THEN
          JFLAG = VB100U
C
C  Translate the Online strip flags
C
          IF(OSUPP(IRWFL))JFLAG = IOR(JFLAG,VBSUPP) ! suppressed strip
          IF(OOVER(IRWFL))JFLAG = IOR(JFLAG,VBOVER) ! DAC overflow
C
C  Flag the linedriver; this is used in correcting for the linedriver
C  decay in VDET91 data
C
          IF(IVCSG-1.GE.NCHAN/2)JFLAG = IOR(JFLAG,VBLIN2)
C
C Fill in the full flag column
C
          IW(KROW(KVCSG,IVCSG)+JVCSFF)=JFLAG
          IW(KROW(KVCSG,IVCSG)+JVCSSA)=IVCSG
        ENDIF
 10   CONTINUE
C
C  Link mapping bank
C
      KVREG = NLINK('VREG',IROMD)
      IF(KVREG .EQ. 0)THEN
        CALL RERROR('VDSMAP',1,'Cant find mapping information bank')
        GOOD = .FALSE.
        GOTO 999
      END IF
C
C And apply channel to strip mapping
C
      NVREG=LROWS(KVREG)
      DO 20 IVREG=1,NVREG
        IFCRG = ITABL(KVREG,IVREG,JVREFC)
        ILCRG = ITABL(KVREG,IVREG,JVRELC)
        JSTRP = ITABL(KVREG,IVREG,JVRECS)
        JFLAG = ITABL(KVREG,IVREG,JVRERF)
        IF(IFCRG.LT.1.OR.ILCRG.GT.NCHAN)THEN
          CALL RERROR('VDSMAP',3,'Mapping bank confusion')
          GOOD = .FALSE.
          GOTO 999
        END IF
        DO 21 IVCSG=IFCRG,ILCRG
C
C  Skip empty channels
C
          IRWFL=ITABL(KVCSG,IVCSG,JVCSRF)
          IF(IRWFL.GT.0)THEN
C
C Compute flag in the region
C
            IOFL=ITABL(KVCSG,IVCSG,JVCSFF)  ! Old flag
            INFL=IOR(IOFL,JFLAG)            ! New flag
C
C Compute the strip address
C
C if the first strip in the region (JSTRP) is given to be zero
C the mapped strip (ISTRP) should be zero.  that is guaranteed
C if IFCRG=ILCRG, which may not be the case.
C
            IF (IFCRG.NE.ILCRG .AND. JSTRP.EQ.0) THEN
              ISTRP = 0
            ELSE
              ISTRP = JSTRP+IVCSG-IFCRG
            ENDIF
C
C And fill
C
            IW(KROW(KVCSG,IVCSG)+JVCSSA)=ISTRP
            IW(KROW(KVCSG,IVCSG)+JVCSFF)=INFL
          ENDIF
 21     CONTINUE
 20   CONTINUE
C
C  Link peculiar channel bank
C
      KVPEC = NLINK('VPEC',IROMD)
C
C  Peculiar banks are not required
C
      IF(KVPEC .NE. 0)THEN
        NVPEC = LROWS(KVPEC)
        DO 30 IVPEC=1,NVPEC
          IVCSG = ITABL(KVPEC,IVPEC,JVPECA)
C
C  Skip empty channels
C
          IRWFL=ITABL(KVCSG,IVCSG,JVCSRF)
          IF(IRWFL.GT.0)THEN
            JFLAG = ITABL(KVPEC,IVPEC,JVPESF)
C
C Compute flag for the peculiar strip
C
            IOFL=ITABL(KVCSG,IVCSG,JVCSFF)  ! Old flag
            INFL=IOR(IOFL,JFLAG)            ! New flag
C
C And fill
C
            IW(KROW(KVCSG,IVCSG)+JVCSFF)=INFL

          ENDIF
 30     CONTINUE
      END IF
C
C  Link the gain bank
C
      KVGAN = NLINK('VGAN',IROMD)
      IF(KVGAN .EQ. 0)THEN
        CALL RERROR('VDSMAP',2,'Cant find gain bank')
        GOOD = .FALSE.
        GOTO 999
      END IF
      NVGAN = LROWS(KVGAN)
      DO 50 IVGAN=1,NVGAN
        IFSGN = ITABL(KVGAN,IVGAN,JVGAFS)
        ILSGN = ITABL(KVGAN,IVGAN,JVGALS)
        GAIN = RTABL(KVGAN,IVGAN,JVGAWG)
        IF(IFSGN.LT.1.OR.ILSGN.GT.NCHAN)THEN
          CALL RERROR('VDSMAP',4,'Gain bank confusion')
          GOOD = .FALSE.
          GOTO 999
        END IF
        DO 51 IVCSG=IFSGN,ILSGN
C
C  Skip empty channels
C
          IRWFL=ITABL(KVCSG,IVCSG,JVCSRF)
          IF(IRWFL.GT.0)THEN
C
C And fill
C
            RW(KROW(KVCSG,IVCSG)+JVCSSG)=GAIN
          ENDIF
 51     CONTINUE
 50   CONTINUE
C
C-- the number of read-out strips [or strip channels per module]
C
      NROST = VNSCRM(IVIEW)
C
C  Calculate the pulseheight for the useable strips
C
      DO 60 IVCSG=1,NCHAN
C
C check on the raw flag rather than the full flag
C
        IRWFL=ITABL(KVCSG,IVCSG,JVCSRF)
        IF (IRWFL.NE.0) THEN
          JFLAG=ITABL(KVCSG,IVCSG,JVCSFF)
          IF(.NOT. SZERO(JFLAG))THEN
            GAIN=RTABL(KVCSG,IVCSG,JVCSSG)
            IPULS=ITABL(KVCSG,IVCSG,JVCSRP)
            OPULS = IPULS*GAIN
          ELSE
            OPULS = 0
C
            JFLAG = IOR(JFLAG,VBZERO)
          END IF
          ISTRP=ITABL(KVCSG,IVCSG,JVCSSA)
C
C  use only sensible istrp
C
          IF (ISTRP.GT.0 .AND. ISTRP.LE.NROST) THEN
            KLIN=KROW(KVCSG,ISTRP)
            RW(KLIN+JVCSMP) = OPULS
C
C  Set the flag for this channel/strip
C
            IW(KLIN+JVCSMF) = JFLAG
          ENDIF
        ENDIF
 60   CONTINUE
C
C  Link peculiar (hot channels) strip bank
C
      KVPES = NLINK('VPES',IROMD)
C
C  Peculiar banks are not required
C
      IF(KVPES .NE. 0)THEN
        NVPES = LROWS(KVPES)
        DO 40 IVPES=1,NVPES
          IVCSG = ITABL(KVPES,IVPES,JVPEHA)
C
C  Check to see if this hot strip was read out this event- IE, has a
C  flag>0.  This must be checked as opposed to PH=/0, as it's possible
C  to readout a PH of 0.
C
C  Skip empty channels
C
           IOFL=ITABL(KVCSG,IVCSG,JVCSMF)  ! Old flag
           IF(IOFL.GT.0)THEN
            JFLAG = ITABL(KVPES,IVPES,JVPESF)
C
C Compute flag for the peculiar strip
C
            INFL=IOR(IOFL,JFLAG)            ! New flag
C
C And fill
C
            IW(KROW(KVCSG,IVCSG)+JVCSMF)=INFL
           ENDIF
 40     CONTINUE
      END IF
 999  CONTINUE
      RETURN
      END
#endif
