*CD lccomd
      COMMON /LCCOMD/ XLSUP(2,2),XHSUP(2,2),YSUP(2,2),
     *                Y0CUT,DYCUT,Y5CUT,DY1,DY3,HUNIT
#if defined(DOC)
C     The common /LCCOMD/ contains constants
C     describing dead space in the LCAL average material
C
C     XLSUP(I,J) = Low X end of wire support J (I=even,odd plane)
C     XHSUP(I,J) = High X end of wire support J (I=even,odd plane)
C     YSUP(I,J)  = Y position of wire support J (I=even,odd plane)
C     Y0CUT      = Y intercept of inclined cathode boundary
C     DYCUT      = Slope of inclined cathode boundary
C     Y5CUT      = Low Y boundary at tube 5
C     DY1        = Dead distance from extrusion end
C     DY3        = Dead distance from center of wire supports
C     HUNIT      = Half the interpad space
#endif
