      logical function xmrmp(dum)
C------------------------------------
C    B.Bloch June 2000 
C    function is false when during a miniramp
C                true  when outside a miniramp ( Energy is stable)
C---------------------------------------
      integer xhvbit
      parameter(ibit=18)
      iret = xhvbit(jhvst)
      xmrmp = btest(jhvst,(ibit-1))
      return
      end
