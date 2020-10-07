      SUBROUTINE VINIRU
C--------------------------------------------------------------------
C!   Initialize the VDET run
C!   AUTHOR    : G. BAGLIESI   861007
C!   Input      : VDG1, VDRD, VDPA banks
C!   Output     : Filled VDPARM and VDGEOS commons
C!
C!    called by : RPRRUN
C!    calls     : VDIGEP
C!
C!    modifications:
C!    08/01/90   test for MC event (HGM)
C!    14-5-90    Rearrange DB logic, get module mapping banks,
C!               reduce bank lists, etc.  D. Brown
C!    4-2-91     Changes for 91 D. Brown
C!    16-4-93    get DB banks depending on setup code through ALGTDB
C!               instead of MDARD
C!
C!  Modified 16-1-94 by Dave Brown for the VDET upgrade
C!  Modified March 1995 A. Bonissent, M. Thulasidas
C!                 reorganise and debug
C!  Modified 16-9-96 by I. Tomalin:
C!              Set MKVDXY to .TRUE. if in PASS0 and using new VDET
C!              and PVGG card.
C!  Modified 3-7-98 by Manoj Thulasidas:
C!              Include VDPR and VDCM in the list of banks to be
C!              loaded with the set up code
C!
C--------------------------------------------------------------------
#ifndef DOC
#include "bcs.h"
#include "rcurnt.h"
#include "rlunit.h"
#include "rflags.h"
#include "vrecon.h"
#include "vprtnc.h"
#include "vrcnjj.h"
#include "vphojj.h"
#include "vtcejj.h"
#include "vhppjj.h"
C
C  Functions
C
      INTEGER  ALGTDB,GTSTUP,NAMIND,NBANK,VNELRM,VDGINI,VDYEAR
      EXTERNAL ALGTDB,GTSTUP,NAMIND,NBANK,VNELRM,VDGINI,VDYEAR
C
C  Local variables
C
      INTEGER IFLAG,JFLAG
      INTEGER IVS,KVPHO,KVRCN,KVTRP,KVTPA,KVTCE,KVDEM,KVHPP
      INTEGER IVIEW,IND

#include "bmacro.h"
C
C  Initialize some variables
C
      NEVNTS = 0
      DO IVIEW=1,2
        NRAWHT(IVIEW) = 0
        NFNLHT(IVIEW) = 0
        NMATHT(IVIEW) = 0
        NNOSHT(IVIEW) = 0
      END DO
C
C Get database banks
C
      IVS  = GTSTUP ('VD',IRUNRC)
      IFLAG = ALGTDB(LRCONS,
     $   'VPHOVRCNVTRPVTPAVTCEVDEMVHPPVDRLVDPRVDCM',-IVS)
      KVPHO = IW(NAMIND('VPHO'))
      KVRCN = IW(NAMIND('VRCN'))
      KVTRP = IW(NAMIND('VTRP'))
      KVTPA = IW(NAMIND('VTPA'))
      KVTCE = IW(NAMIND('VTCE'))
      KVDEM = IW(NAMIND('VDEM'))
      KVHPP = IW(NAMIND('VHPP'))
      IF(KVPHO.LE.0 .OR. KVRCN.LE.0 .OR. KVTRP.LE.0 .OR.
     &     KVTPA.LE.0 .OR. KVTCE.LE.0 .OR. KVDEM.LE.0 .OR.
     &     KVHPP.LE.0 )THEN
        CALL RERROR('VINIRU',1,'Failed to read database banks')
        GOTO 999
      END IF
C
C  Set control flags; overwrite if reprocessing  or PASS0
C
      IF ( FREPRO ) THEN
        MKVDXY = .TRUE.
        MKVFPH = .FALSE.
        MKVHOT = .FALSE.
      ELSE IF ( FPASS0 ) THEN
        MKVDXY = .FALSE.
        IVSET = GTSTUP('VD',IRUNRC)
        IF (IW(NAMIND('PVGG')).GT.0.AND.IVSET.GE.8)
     +  MKVDXY = .TRUE.
        MKVFPH = .TRUE.
        MKVHOT = .TRUE.
      ELSE
        MKVDXY = ITABL(KVRCN,1,JVRCMX).EQ.-1
        MKVFPH = ITABL(KVRCN,1,JVRCMF).EQ.-1
        MKVHOT = ITABL(KVRCN,1,JVRCMH).EQ.-1
      END IF
C
C  Unpack PH offset information
C
      IPHOFF = ITABL(KVPHO,1,JVPHPO)
C
C  Unpack the single strip noise and the line-driver decay factor
C
      SNOISE(1) = RTABL(KVTCE,1,JVTCWN)
      SNOISE(2) = RTABL(KVTCE,1,JVTCUN)
      DECFAC = RTABL(KVTCE,1,JVTCDF)
C
C  Final reclustering parameters
C
      MNPULS = ITABL(KVRCN,1,JVRCPU)
      MXUNBD = ITABL(KVRCN,1,JVRCMU)
      MXSIGM = ITABL(KVRCN,1,JVRCMS)
      FNLSUM = ITABL(KVRCN,1,JVRCFS)
C
C  Hit position parameters
C
      SEPSIG = RTABL(KVHPP,1,JVHPSS)
      POSSIG = RTABL(KVHPP,1,JVHPPS)
      ERRNOM(1) = RTABL(KVHPP,1,JVHPWE)
      ERRNOM(2) = RTABL(KVHPP,1,JVHPUE)
C
C  Common mode suppression parameters
C
      MINCMD = ITABL(KVRCN,1,JVRCMN)
      HCUTCM = RTABL(KVRCN,1,JVRCHC)
      LCUTCM = RTABL(KVRCN,1,JVRCLC)
C
C  Hot channel suppression parameters
C
      MXOCUP(1) = RTABL(KVRCN,1,JVRCMZ)
      MXOCUP(2) = RTABL(KVRCN,1,JVRCMP)
      NHOTMN   = 100
C
C Setup
C
      CALL VRDDAF(LRCONS,IRUNRC,IFLAG)
C
C Get alignment constants from database
C
      CALL VGRDAL(LRCONS,IRUNRC,IND)
C
      IF (IND .EQ. 0)THEN
        CALL RERROR('VINIRU',3,'Failed to read alignment banks')
      END IF
      IF (IFLAG .EQ. 0)THEN
        CALL RERROR('VINIRU',3,'Failed to initialize geometry')
      END IF
C
C  Maximum # of channels in a readout module
C
      NRCHAN = MAX(VNELRM(1),VNELRM(2))
C
C read maps, gains, and hot channels
C
      MCEVNT = IW(NAMIND('AJOB')).NE.0
        IFLAG = ALGTDB(LRCONS,'VMREVMPC',-IVS)
        JFLAG = ALGTDB(LRCONS,'VMGN',-IRUNRC)
        IF (IFLAG .EQ. 0 .OR. JFLAG.EQ. 0)THEN
          CALL RERROR('VINIRU',2,'Failed to initialize bond maps')
        END IF
        CALL VDMPIN
      IF(.NOT.MCEVNT)THEN
C
C  Load the common VRECON
C
      CALL VDRCUN
C
C  Process hot channel information
C
        CALL VHOTIN
C
C  Cable swaps
C
        JFLAG = ALGTDB(LRCONS,'VCAB',-IRUNRC)
        IF( JFLAG.EQ.0 )
     +    CALL RERROR('VINIRU',4,'Failed to read cable swap bank')
C
C  Generic constants
C
        IF(VDYEAR().NE.95)THEN
C Old Vdet
          CHNOFF(1) = 1      !  online chanels are offset to wrt bond ma
          CHNOFF(2) = 1      !  online chanels are offset to wrt bond ma
        ELSE
C New Vdet
          CHNOFF(1) = 0      !  offset is taken into account by
          CHNOFF(2) = 0      !  geography package
        ENDIF
C
C  linedriver decay
C
        IF(IVS.LE.3)THEN
          IDCLIN = 2           ! Line driver 2 affected
        ELSE IF (IVS.LE.6)THEN
          IDCLIN = 1           ! Line driver 1 affected
        ELSE
          IDCLIN = 0
        END IF
C
C  Fix the timing problem early in 1992
C
        IF(IVS.EQ.4.AND.IRUNRC.LE.14831)THEN
            DECFAC = .22
        END IF
C
C  For now, no 'special condition' flags are set, but once something
C  happens which needs special care for the reconstruction, the prefered
C  method will be to use this section to set one of the
C  10 condition flags CONFLAG and it's associated real value CONPAR.
C  These flags can be read in the appropriate reconstruction routine,
C  where appropriate action can be taken.  This avoids dispersing
C  time-dependent kludge constants througout the code, where they'll
C  be hard to find and fix later.  Note that a completely general
C  method for handling miscablings is already incorporated with the
C  VCAB bank.  Variations in gains, alignment, etc. are handled
C  by standard database banks.
C
C  ex;  IF(IRUNRC.GT.30100.AND.IRUNRC.LT.30200)THEN
C          CONFLG(1) = 2        !  Fix the pedestal offset in mx7 #2 chip
C          CONPAR(1) = 31.1     !  Offset value
C        END IF
C
      ELSE
C
C Generic MC special setup
C
        CHNOFF(1) = 0        !  In MC strips are readout as channels
        CHNOFF(2) = 0        !  In MC strips are readout as channels
        IF(VDYEAR().EQ.95)THEN
          CHNOFF(1) = 32
          CHNOFF(2) = 2
        ENDIF
        IDCLIN = 0              !  No line driver decay
        DECFAC = 0.0
      END IF
C
C  Cleanup bos
C
 999  CONTINUE
      CALL BGARB(IW)
      RETURN
      END
#endif
