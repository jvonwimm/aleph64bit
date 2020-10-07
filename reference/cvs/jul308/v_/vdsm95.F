      SUBROUTINE VDSM95(IROMD)
C----------------------------------------------------------------------
C!  - Performs the map from electronics channels to strip numbers
C!
C
C  This subroutine takes as input hits from a single readout module
C  and produces as output an edited version of same,
C  with address mapped onto the actual (integer) strip numbers,
C  unmapped channels removed, and special case strips flaged,
C  for each of the 3 wafers in a module.
C  1 electronic channel -> 2(view1) or 3 (view2) readout strips
C
C  A. Bonissent June 1995
C
C  INPUT  :
C      IROMD   :  Readout module address
C
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
#include "vganjj.h"
#include "vpesjj.h"
#include "vrecon.h"
C
C  Function calls
C
      INTEGER NLINK
      INTEGER NAMIND
      INTEGER VNELRM, VNSCRM, VNRDIR, VSCHAN, VJMODI, VFWAFF
C
C  Local variables
C
      INTEGER KVCSG,NVCSG,NAVCSG
      INTEGER NCHAN,NSTRP, NROST
      INTEGER KLIN
      INTEGER ILAY,IZ,IPH,IVIEW,IWAF
      INTEGER IRWFL,IVREG
      INTEGER IOFL,INFL
      INTEGER KVREG,KVPEC,KVGAN,KVPES
      INTEGER IVCSG,JSTRP(3),ISTRP
      INTEGER IVGAN,NVGAN,IFSGN,ILSGN
      INTEGER JFLAG,JFL,PCFLG,PSFLG
      INTEGER NVREG,IFCRG,ILCRG
      INTEGER IVPEC,NVPEC,PCHAN
      INTEGER NVPES,IVPES,PSTRP
      INTEGER OPULS,IPULS
      INTEGER ISTSB(3),IFLTC(3),IFLBE
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
C     Say if we want to use bonding maps
C
C
      IFLBE=1
      IF(MCEVNT)IFLBE=0
C
C Decode address,
C get the view and the number of readout channels
C
      CALL VADEWA(IROMD,ILAY,IZ,IPH,IVIEW)
      NCHAN = VNELRM(IVIEW)
      KVCSG=IW(NAVCSG)
      NVCSG=LROWS(KVCSG)
C
      DO 10 IVCSG=1,NCHAN
C
C Get the raw flag
C
        IRWFL = ITABL(KVCSG,IVCSG,JVCSRF)
        IF (IRWFL.NE.0)THEN
          JFL = VB100U
C
C  Translate the Online strip flags
C
          IF(OSUPP(IRWFL))JFL = IOR(JFL,VBSUPP) ! suppressed strip
          IF(OOVER(IRWFL))JFL = IOR(JFL,VBOVER) ! DAC overflow
C Fill in the full flag column
C
          IW(KROW(KVCSG,IVCSG)+JVCSFF)=JFL
          IW(KROW(KVCSG,IVCSG)+JVCSSA)=IVCSG
C
C We also set the gain by default, which is 1
C May be superseded by the content of VGAN bank if present
C JVCSSG, strip gain, should have been CG : channel gain
C
          RW(KROW(KVCSG,IVCSG)+JVCSSG)=1.
        ENDIF
 10   CONTINUE
C
C  Link peculiar (hot channels) strip bank
C
      KVPES = NLINK('VPES',IROMD)
C
C  Peculiar banks are not required
C
C
C For Vdet95, VHOT contains the addresses of the hot READOUT CHANNELS,
C in contrast to Vdet 91 where it was readout strip addresses
C
      IF(KVPES .NE. 0)THEN
        NVPES = LROWS(KVPES)
        DO 40 IVPES=1,NVPES
          IVCSG = ITABL(KVPES,IVPES,JVPEHA)
C
C Protect for nonsense in VHOT
C
          IF(IVCSG.LE.0)GO TO 40
C
C  Check to see if this hot channel was read out this event- IE, has a
C  flag>0.  This must be checked as opposed to PH=/0, as it's possible
C  to readout a PH of 0.
C
C  Skip empty channels
C
           IOFL=ITABL(KVCSG,IVCSG,JVCSFF)  ! Old flag
           IF(IOFL.GT.0)THEN
            JFL = ITABL(KVPES,IVPES,JVPESF)
C
C Compute flag for the peculiar strip
C
            INFL=IOR(IOFL,JFL)            ! New flag
C
C And fill
C
            IW(KROW(KVCSG,IVCSG)+JVCSFF)=INFL
           ENDIF
 40     CONTINUE
      END IF
C
C Perform the mapping
C
      IRET = VFWAFF(IZ,IMOD,IBID)
      IRET = VJMODI(ILAY,IPH,IMOD,JMOD)
      DO 20 IVCSG=1,NCHAN
C
C Get the raw flag
C
        IRWFL = ITABL(KVCSG,IVCSG,JVCSRF)
C
C  Skip empty channels
C
        IF (IRWFL.NE.0)THEN
          IRET = VSCHAN(IFLBE,JMOD,IVIEW,IVCSG,ISTSB,IFLTC)
          IF(IRET.EQ.1)THEN
C
C Compute flag in the region
C
            IOFL=ITABL(KVCSG,IVCSG,JVCSFF)  ! Old flag
            DO IWAF=1,3
              IF(ISTSB(IWAF).GT.0)THEN
               IFLT = IFLTC(IWAF)
C
C Interpret the fault code into a flag
C
               JFLAG=0
               IF(IFLT.EQ.1.OR.IFLT.EQ.2.OR.IFLT.EQ.3.OR.IFLT.EQ.4.OR.
     >            IFLT.EQ.7) JFLAG=VBSUPP
               INFL=IOR(IOFL,JFLAG)            ! New flag
               IMSTR = ISTSB(IWAF)
               IW(KROW(KVCSG,IVCSG)+JVCSSA+IWAF-1) = IMSTR
               IW(KROW(KVCSG,IMSTR)+JVCSMF+IWAF-1)=INFL
              ENDIF
            ENDDO
         ENDIF
        ENDIF
 20   CONTINUE
C
C  Link the gain bank
C
      KVGAN = NLINK('VGAN',IROMD)
C
C If gain bank is absent, it means that gains are 1., by default.
C Will always be the case for monte carlo
C This is a difference with respect to VDSMAP for old Vdet.
C That sort of verification is better done in VDMPIN, once per run
C
      IF(KVGAN .NE. 0)THEN
        NVGAN = LROWS(KVGAN)
        DO 50 IVGAN=1,NVGAN
          IFSGN = ITABL(KVGAN,IVGAN,JVGAFS)
          ILSGN = ITABL(KVGAN,IVGAN,JVGALS)
          GAIN = RTABL(KVGAN,IVGAN,JVGAWG)
          IF(IFSGN.LT.1.OR.ILSGN.GT.NCHAN)THEN
            CALL RERROR('VDSM95',4,'Gain bank confusion')
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
 51       CONTINUE
 50     CONTINUE
      ENDIF
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
          DO IWAF=1,3
            ISTRP=ITABL(KVCSG,IVCSG,JVCSSA+IWAF-1)
            JFL=ITABL(KVCSG,ISTRP,JVCSMF+IWAF-1)
            IF(.NOT. SZERO(JFL))THEN
              GAIN=RTABL(KVCSG,IVCSG,JVCSSG)
              IPULS=ITABL(KVCSG,IVCSG,JVCSRP)
              OPULS = IPULS*GAIN
            ELSE
              OPULS = 0
C
              JFL = IOR(JFL,VBZERO)
            END IF
C
C  use only sensible istrp
C
            IF (ISTRP.GT.0 .AND. ISTRP.LE.NROST) THEN
              KLIN=KROW(KVCSG,ISTRP)
              RW(KLIN+JVCSMP+IWAF-1) = OPULS
C
C  Set the flag for this channel/strip
C
              IW(KLIN+JVCSMF+IWAF-1) = JFL
            ENDIF
          ENDDO
        ENDIF
 60   CONTINUE
      IF(MCEVNT)THEN
        CALL VTRSUP(IROMD)
      ENDIF
 999  CONTINUE
      RETURN
      END
#endif
