       SUBROUTINE VRVMBE
C ----------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Read and unpack bonding error banks  VMBE
C
C - Joe Rothberg, August 1995
C
C ---------------------------------------------------------------
#ifndef DOC
      IMPLICIT NONE
C ------------------------------------------------------------
#include "vglobl.h"
#include "vduefc.h"
#include "bcs.h"
C ----------------------------------------------
      INTEGER kvuec, nfirstz, nfirstp

      INTEGER nami, kvmbe, nvmbe, ivmbe, iad
      INTEGER ivmad, ivcod, ivtemp
      INTEGER ivad1, ivad2, ivview, ivbond, ivfault, ivparam
      INTEGER ismod
C
      INTEGER ten4, ten6, ten8
      PARAMETER(ten4=10000)
      PARAMETER(ten6=10**6)
      PARAMETER(ten8=10**8)
C
      INTEGER nump, numz, nume, ix, iv, ie, j, igarb

      CHARACTER*4 bname
      INTEGER lnvmbu, ihvmbu
C number of columns
      INTEGER nvmbur
      PARAMETER (nvmbur = 6)
      INTEGER NLINK
#include "bmacrod.h"
C ------------------------------------------------------------
#include "bmacro.h"
C ------------------------------------------------------------------
C bank VMBE
      DO ismod = 1, 80
         kvmbe  = 0
         kvmbe = NLINK('VMBE',ismod)

         IF(kvmbe .GT. 0) THEN
            nvmbe = LROWS(kvmbe)
            IF(nvmbe .GT. 0) THEN

C create bank for unpacked values
              bname = 'VMBU'
              lnvmbu = nvmbe*nvmbur + 2
              CALL AUBOS(bname,ismod,lnvmbu,ihvmbu,igarb)
              IW(ihvmbu+1)= nvmbur
              IW(ihvmbu+2)= nvmbe

C------------------------------------------------
              nump = 0
              numz = 0
              nume = 0
C -----------------------------------------------
              DO  ivmbe = 1, nvmbe
                ivmad = ITABL(kvmbe,ivmbe,1)
                ivcod = ITABL(kvmbe,ivmbe,2)

C unpack code word
                ivfault = MOD(ivcod,ten6)
                ivparam = ivcod/ten6

C unpack address word
C        original bank is given in terms of electronics channels
C          starting from 0
C
               ivad2 =  MOD(ivmad,ten4)
               ivad1 =  MOD(ivmad/ten4,ten4)

               ivtemp = ivmad/ten8

               ivbond = MOD(ivtemp,10)
               ivview = 1+(ivtemp-ivbond)/10
C -------------------------------------------------------
C store unpacked values
               IF(ivview .EQ. vviewp) THEN
                  nump = nump + 1
                  ix  = nump
               ELSE IF (ivview .EQ. vviewz) THEN
                  numz = numz + 1
                  ix = numz
               ELSE
                  GOTO 990
               ENDIF

C -------------------------------------------------
C fill new bank
             nume = nume + 1
             iad = ihvmbu + 2 + (nume-1)*nvmbur
             IW(iad +1) = ivview
             IW(iad +2) = ivad1
             IW(iad +3) = ivad2
             IW(iad +4) = ivbond
             IW(iad +5) = ivfault
             IW(iad +6) = ivparam

C -------------------------------------------------

 990         CONTINUE
C loop over rows in bank
          ENDDO
C ------------------------------------------------
          ENDIF
        ENDIF
      ENDDO

      RETURN
      END
#endif
