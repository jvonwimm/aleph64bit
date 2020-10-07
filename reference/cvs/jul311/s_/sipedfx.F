      SUBROUTINE SIPEDFX(IRUN,IOLD,INEW)
C----------------------------------------------------------------------
C!  - SICAL Fix pedestal of specific runs
C!
C!   Author   :- B. Bloch                    28-OCT-1998
C!             warning : the units are MeV  not ADC counts !!!
C?
C!  - Input :  IRUN   Run number 
C!             IOLD   current value (in MeV) of ADC channel content
C!  - Output : INEW   corrected value if required
C?
C!======================================================================
#ifndef DOC
      save iruno,ifix
      real*4 xyz(3),dxyz(3)
      data iruno/0/
      if ( irun.eq.iruno) go to 100
      iruno = irun
      ifix = 0
      if ( (irun.ge.45508).and.(irun.le.45511) ) then
         ifix = -6
      elseif ( (irun.ge.46785).and.(irun.le.46789) ) then
         ifix =  3
      elseif ( (irun.ge.50118).and.(irun.le.50122) ) then
         ifix =  -15
      endif
 100  inew = iold + ifix
      return
      end
#endif
