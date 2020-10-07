      SUBROUTINE X2BOOK
C
C ------------------------------------------------
C! Book the level 2 histograms
C
C Author : T. Medcalf  10/9/87
C
C? Uses HBOOK calls to book the histograms to show
C? track finding statistics.
C
C ------------------------------------------------
#ifndef DOC
      SAVE
C
      CALL HBOOK1(1140,'Total tracks found by trigger.$',100,0.,100.,0.)
      CALL HBOOK1(1141,'Inner sector track finding percentages.$',
     +  51,0.,102.,0.)
      CALL HBOOK1(1142,'Outer sector track finding percentages.$',
     +  51,0.,102.,0.)
C
      RETURN
      END
#endif
