      SUBROUTINE VDMPIN
C----------------------------------------------------------------------
C!  - Unpack VDET mapping and module banks used in hit reconstruction
C!
C
C  This subroutine is responsible for reading in the database files
C  which contain the module maping information of the ALEPH silicon
C  vertex detector, and translating these into a form usefull in the
C  reconstruction.  D. Brown, 10-4-90
C  Modified March 1995 D. Brown, A. Bonissent, M. Thulasidas
C                 reorganise and debug,
C                 use geom. package for Vdet 91 or 95
C  Modified June 1995 A. Bonissent
C                 VMGN was changed to contain also first and last
C                 channel in a region
C                 would work only with DAF > 198
C
C?
C!======================================================================
#ifndef DOC
#include "vrecon.h"
#include "vmgnjj.h"
#include "vmpcjj.h"
#include "vpecjj.h"
#include "vganjj.h"
C
C  Global includes
C
#include "bcs.h"
C
C  Functions
C
      INTEGER NBANK
      INTEGER NAMIND
      INTEGER VNSCRM, VNELRM, VDYEAR
C
C  Local variables
C
      INTEGER NAVMRE,NAVMGN,NVMRE,NVREG,NVMPC,NVPEC,NVGAN,IVMRE
      INTEGER NAVMPC
      INTEGER IROMD,IRONX,IVMPC,IVMGN,NEWMD
      INTEGER KVMRE,KVMPC,KVMGN
      INTEGER NWRD,NVMGN,MROW
      INTEGER IADDR,ILAY,IWFF,IFAC,IVIEW
      INTEGER ICHAN,NCHAN,LCHAN
      INTEGER KVREG,KVPEC,KVGAN

      LOGICAL FIRST
      DATA FIRST /.TRUE./
#include "bmacro.h"
      IF(FIRST)THEN
         FIRST=.FALSE.
         NAVMPC = NAMIND('VMPC')
         NAVMGN = NAMIND('VMGN')
      ENDIF
C
        CALL VUNADD(IADDR,NCHAN,ILAY,IWFF,IFAC,IVIEW,ICHAN)
        CALL VUNADD(IADDR,NCHAN,ILAY,IWFF,IFAC,IVIEW,ICHAN)
        CALL VUNADD(IADDR,NCHAN,ILAY,IWFF,IFAC,IVIEW,ICHAN)
C Build VMRE bank, depending on year
C
      IF(VDYEAR().EQ.91)THEN
        CALL VMKR90
      ELSE
        CALL VMKR95
      ENDIF
C
C  Find the banks
C
      KVMPC = IW(NAVMPC)
      KVMGN = IW(NAVMGN)
      IF( KVMPC .EQ. 0 )THEN
        CALL RERROR('VDMPIN',1,'Cant find database banks')
        GOTO 999
      END IF
C
C  Peculiar channel bank creation
C
      NVMPC = LROWS(KVMPC)
      IF(NVMPC.NE.0)THEN
        IADDR = ITABL(KVMPC,1,JVMPCA)
        CALL VUNADD(IADDR,NCHAN,ILAY,IWFF,IFAC,IVIEW,ICHAN)
        CALL VAENWA(IROMD,ILAY,IWFF,IFAC,IVIEW)
        NEWMD=1
      ENDIF
      DO IVMPC=1,NVMPC
        IF(NEWMD.EQ.1)THEN
          KVPEC = NBANK('VPEC',IROMD,LMHLEN+NVMPC*LVPECA)
          IF(KVPEC .EQ. 0)THEN
            CALL RERROR('VDMPIN',2,'Out of BOS space')
            GOTO 999
          END IF
          IW(KVPEC+LMHCOL) = LVPECA
          IW(KVPEC+LMHROW) = 0
          NEWMD=0
        END IF
        IW(KNEXT(KVPEC)+JVPECA) = ICHAN+1
        IW(KNEXT(KVPEC)+JVPESF) = ITABL(KVMPC,IVMPC,JVMPSF)
        IW(KVPEC+LMHROW) = LROWS(KVPEC) + 1
        IF(IVMPC.LT.NVMPC)THEN
          IADDR = ITABL(KVMPC,IVMPC+1,JVMPCA)
          CALL VUNADD(IADDR,NCHAN,ILAY,IWFF,IFAC,IVIEW,ICHAN)
          CALL VAENWA(IRONX,ILAY,IWFF,IFAC,IVIEW)
        ENDIF
        IF(IRONX.NE.IROMD.OR.IVMPC.EQ.NVMPC)THEN
          NVPEC = LROWS(KVPEC)
          KVPEC = NBANK('VPEC',IROMD,LMHLEN+NVPEC*LVPECA)
          IROMD=IRONX
          NEWMD=1
        END IF
      END DO
C
C  The same for the module gains.  The format of the separate
C  banks is however slightly different from the original
C
      IF(MCEVNT) GOTO 999
      IF(KVMGN .EQ. 0)THEN
        CALL RERROR('VDMPIN',1,'Cant find database banks')
        GOTO 999
      END IF
      NVMGN = LROWS(KVMGN)
      IF(NVMGN.NE.0)THEN
        IROMD = ITABL(KVMGN,1,JVMGWA)
        CALL VADEWA(IROMD,ILAY,IWFF,IFAC,IVIEW)
        LCHAN = ITABL(KVMGN,1,JVMGLC)+1
        IF(LCHAN.GT.VNELRM(IVIEW))LCHAN = VNELRM(IVIEW)
        ICHAN = ITABL(KVMGN,1,JVMGFC)+1
        IF(ICHAN.LT.1)ICHAN = 1
        NEWMD=1
      ENDIF
      DO IVMGN=1,NVMGN
        IF(NEWMD.EQ.1)THEN
            KVGAN = NBANK('VGAN',IROMD,LMHLEN+NVMGN*LVGANA)
            IF(KVGAN .EQ. 0)THEN
              CALL RERROR('VDMPIN',2,'Out of BOS space')
              GOTO 999
            END IF
            IW(KVGAN+LMHCOL) = LVGANA
            IW(KVGAN+LMHROW) = 0
            NEWMD=0
        END IF
        IW(KNEXT(KVGAN)+JVGAFS) = ICHAN
        IW(KNEXT(KVGAN)+JVGALS) = LCHAN
        RW(KNEXT(KVGAN)+JVGAWG) = RTABL(KVMGN,IVMGN,JVMGWG)
        IW(KVGAN+LMHROW) = LROWS(KVGAN) + 1
        IF(IVMGN.LT.NVMGN)THEN
          IRONX = ITABL(KVMGN,IVMGN+1,JVMGWA)
          CALL VADEWA(IRONX,ILAY,IWFF,IFAC,IVIEW)
          LCHAN = ITABL(KVMGN,IVMGN+1,JVMGLC)+1
          IF(LCHAN.GT.VNELRM(IVIEW))LCHAN = VNELRM(IVIEW)
          ICHAN = ITABL(KVMGN,IVMGN+1,JVMGFC)+1
          IF(ICHAN.LT.1)ICHAN = 1
        ENDIF
        IF(IRONX.NE.IROMD.OR.IVMGN.EQ.NVMGN)THEN
          NVGAN = LROWS(KVGAN)
          KVGAN = NBANK('VGAN',IROMD,LMHLEN+NVGAN*LVGANA)
          IROMD=IRONX
          NEWMD=1
        END IF
      ENDDO
C
C  Cleanup the BOS array- this is necessary, as a lot of named-bank
C  space has been moved around.
C
      CALL BDROP(IW,'VMPCVMGN')
      CALL BGARB(IW)
C
 999  CONTINUE
      RETURN
      END
#endif
