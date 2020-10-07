

C=======================================================================
      function epad_e14(ipeco,imodule)
C-----------------------------------------------------------------------
C Get the pad energy of the peco cluster IPECO coming from module IMODULE
C Auxiliary to QFE14
C Author G. Taylor Oct 1997
C-----------------------------------------------------------------------
#include "qcde.h"
      PARAMETER (IDPK = 2**16)
      PARAMETER (JETDTL = 1)
      parameter(mx=500)
      dimension mxcol(228)
      dimension epad(mx,36)
      save epad,lrun,levt
      logical first
      data first/.true./
#include "bmacro.h"
C-----------------------------------------
      epad_e14=0.
C 
C find the limits of the module indices
C
      if (first) then
       first=.false.
       do i=1,228
        CALL ECRWRG(I,IRSS,MXCOL(i))
       enddo
       LEVT=0
       LRUN=0
      endif
C
C for a new event decode the storey info
C
      IF (KRUN.NE.LRUN.OR.KEVT.NE.LEVT) THEN
       LEVT=KEVT
       LRUN=KRUN
       call vzero(epad(1,1),mx*36)
C 
C link to the storey banks
C
       KETDI = IW(NAMIND('ETDI'))
       KPEST = IW(NAMIND('PEST'))
       if (Kpest.le.0.or.ketdi.le.0) go to 999
C
C loop over the storeys and sum the energy in each peco in each module
C
       CUTENER = 0.01           ! gampex value is 0.01
       npest=lrows(kpest)
       do ipest=1,npest
        ipc = itabl(kpest,ipest,5)
        if (ipc .gt. 0 ) then
         if (ipc .gt. mx ) go to 999
         epc = rtabl(kpest,ipest,2)
         ITO = ITABL(KPEST,IPEST,4)
         IF (ITO.gt.0) then
          K = ITABL(KPEST,IPEST,1)
          IWIN = ITABL(KETDI,ITO,JETDTL)
          I = IWIN / IDPK
          J = (IWIN - I * IDPK) / 4
          IF ( I.ge.1.and.I.le.228) then
           IF ( J.ge.1.and.J.le.mxcol(i)) then
            ENSTO = FLOAT (ITABL(KETDI,Ito,JETDTL+K)) / 1000000.
            ENSTO = ENSTO * ECETDI(I,J)
            IF (ENSTO.gt.CUTENER) then
             call emdtow(i,j,ksbd,kmodu,krg)
             module=kmodu+12*(ksbd-1)
             if (ipc .le. mx ) epad(ipc,module)=epad(ipc,module)+ensto
            endif
           endif
          endif
         endif
        endif
       enddo
      ENDIF
C
      if (ipeco.le.mx) epad_e14=epad(ipeco,imodule)
 999  return
      end
