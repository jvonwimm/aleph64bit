*DK iaprox
      SUBROUTINE IAPROX(XWI, YWI, VA, VB, VP)
C.
C...IAPROX  1.00  900426  14:54                        R.Beuselinck.
C.
C!  Find closest point to wire on track element.
C.
C.  Arguments:
C.  XWI [S,I,REAL] : X coordinate of wire.
C.  YWI [S,I,REAL] : Y coordinate of wire.
C.  VA  [V,I,REAL] : Start point on track element.
C.  VB  [V,I,REAL] : End point on track element.
C.  VP  [V,O,REAL] : Point closest to sense wire on track element.
C.
C?  The closest point to (XWI,YWI) on the line segment AB is found.
C?  This is normally the perpendicular distance to the line unless that
C?  point falls outside the limits of the given start and end points.
C?  In that case the closest of VA, VB is returned.
C.
C-----------------------------------------------------------------------
#ifndef DOC
      SAVE
      REAL XWI, YWI, VA(*), VB(*), VP(*)
      REAL ASQ, BSQ, CSQ, CFACT
C
      ASQ = (VB(1)-XWI)**2 + (VB(2)-YWI)**2
      BSQ = (VA(1)-XWI)**2 + (VA(2)-YWI)**2
      CSQ = (VB(1)-VA(1))**2 + (VB(2)-VA(2))**2
C
C--  Check whether perpendicular lies within AB.
C--
      IF (ABS(ASQ-BSQ).GT.CSQ) THEN
        IF (ASQ.LT.BSQ) THEN
          VP(1) = VB(1)
          VP(2) = VB(2)
          VP(3) = VB(3)
        ELSE
          VP(1) = VA(1)
          VP(2) = VA(2)
          VP(3) = VA(3)
        ENDIF
      ELSE
C
C--  Find the perpendicular from (XWI,YWI) to AB
C--
        CFACT = (BSQ+CSQ-ASQ)/(2.*CSQ)
        VP(1) = VA(1) + CFACT*(VB(1)-VA(1))
        VP(2) = VA(2) + CFACT*(VB(2)-VA(2))
        VP(3) = VA(3) + CFACT*(VB(3)-VA(3))
      ENDIF
      END
#endif
