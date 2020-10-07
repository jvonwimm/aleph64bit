      FUNCTION COSFAC(POL)
C----------------------------------------------------------------
C! TPC resolution dependence on theta
C  Input  POL : polar angle ( degrees )
C
C  G.Ganis   21 Mar 1991
C----------------------------------------------------------------
      PARAMETER( QQPI= 3.141592 )
      DIMENSION XF(3),XC(3)
      DATA XF /1.,1.12,3./,XC /.75,.8,.93 /
C
      COSX= COS(POL*QQPI/180.)
      COSFAC=1.
      IF(COSX.LT.XC(1)) RETURN
      IF(COSX.GE.XC(1).AND.COSX.LT.XC(2)) THEN
       WX=(LOG10(XF(2))-LOG10(XF(1)))/(XC(2)-XC(1))
       WX= WX*(COSX-XC(1)) + LOG10(XF(1))
      ELSE IF(COSX.GE.XC(2)) THEN
       WX=(LOG10(XF(3))-LOG10(XF(2)))/(XC(3)-XC(2))
       WX= WX*(COSX-XC(2)) + LOG10(XF(2))
      END IF
C
      COSFAC= EXP(WX*LOG(10.))
C
      RETURN
      END
