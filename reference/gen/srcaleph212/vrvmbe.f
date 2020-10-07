       SUBROUTINE VRVMBE
C ----------------------------------------------------
CKEY VDETDES INDEX / USER
C!  Read and unpack bonding error banks  VMBE
C
C - Joe Rothberg, August 1995
C
C ---------------------------------------------------------------
      IMPLICIT NONE
C ------------------------------------------------------------
C!    Parameters for VDET geometry package
C ----------------------------------------------------------------------
C
C     Labels for return codes:
C
      INTEGER VDERR, VDOK
      PARAMETER (VDERR = -1)
      PARAMETER (VDOK  = 1)
C
C     Labels for views:
C
      INTEGER VVIEWZ, VVIEWP
      PARAMETER (VVIEWZ = 1)
      PARAMETER (VVIEWP = 2)
C
C     Fixed VDET geometry parameters:
C
      INTEGER NVLAYR, NVMODF, NVVIEW, NPROMM, IROMAX
      PARAMETER (NVLAYR = 2)
      PARAMETER (NVMODF = 2)
      PARAMETER (NVVIEW = 2)
      PARAMETER (NPROMM = 1)
      PARAMETER (IROMAX = 4)
C
C     Array dimensions:
C
      INTEGER NVWMMX, NVWFMX, NVFLMX, NVFMAX, NVMMAX, NVWMAX
      INTEGER NVZRMX, NVPRMX
      PARAMETER (NVWMMX = 3)
      PARAMETER (NVWFMX = NVWMMX*NVMODF)
      PARAMETER (NVFLMX = 15)
      PARAMETER (NVFMAX = 24)
      PARAMETER (NVMMAX = NVFMAX*NVMODF)
      PARAMETER (NVWMAX = NVFMAX*NVWFMX)
      PARAMETER (NVZRMX = NVFMAX*IROMAX)
      PARAMETER (NVPRMX = NVMMAX*NPROMM)
C
C!   VDET Unconnected, extra channels; Face-module content
C ------------------------------------------------------------
      INTEGER VUECH, VEXCH, VIGBM
      INTEGER MAXFACE
      PARAMETER(MAXFACE=40)
      CHARACTER*4 FACEC
      INTEGER FACEN,MODNEG,MODPOS
c
      COMMON/VDUEFC/VUECH(2),VEXCH(2),VIGBM,
     >      FACEN(MAXFACE),FACEC(MAXFACE),
     >      MODNEG(MAXFACE),MODPOS(MAXFACE)
      INTEGER LMHLEN, LMHCOL, LMHROW
      PARAMETER (LMHLEN=2, LMHCOL=1, LMHROW=2)
C
      COMMON /BCS/   IW(1000)
      INTEGER IW
      REAL RW(1000)
      EQUIVALENCE (RW(1),IW(1))
C
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
      INTEGER LCOLS, LROWS, KROW, KNEXT, ITABL,LFRWRD, LFRROW
      REAL RTABL
      INTEGER ID, NRBOS, L
C ------------------------------------------------------------
C!    set of intrinsic functions to handle BOS banks
C - # of words/row in bank with index ID
      LCOLS(ID) = IW(ID+1)
C - # of rows in bank with index ID
      LROWS(ID) = IW(ID+2)
C - index of next row in the bank with index ID
      KNEXT(ID) = ID + LMHLEN + IW(ID+1)*IW(ID+2)
C - index of row # NRBOS in the bank with index ID
      KROW(ID,NRBOS) = ID + LMHLEN + IW(ID+1)*(NRBOS-1)
C - # of free words in the bank with index ID
      LFRWRD(ID) = ID + IW(ID) - KNEXT(ID)
C - # of free rows in the bank with index ID
      LFRROW(ID) = LFRWRD(ID) / LCOLS(ID)
C - Lth integer element of the NRBOSth row of the bank with index ID
      ITABL(ID,NRBOS,L) = IW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C - Lth real element of the NRBOSth row of the bank with index ID
      RTABL(ID,NRBOS,L) = RW(ID+LMHLEN+(NRBOS-1)*IW(ID+1)+L)
C
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
