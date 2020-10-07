      SUBROUTINE X1BOOK
C ----------------------------------------------------------------------
C.
C. - Author   : A. Putzer  - 95/01/15  FOR ALEPHLIB 204
C.
C! - Book Level1 Trigger Histograms
C.
C? - This routine initializes (books) the level1 trigger
C?   histograms which are of more general interest.
C.
C.
C. - Called by      XTBOOK                        from .GALEPH
C.
#ifndef DOC
      SAVE
C.
C.   L E V E L 1    H I S T O G R A M S
C.
      CALL HBOOK1(1101,'Trig. Segm. : Muon         Trig.  $',60,1.,61.,
     &   0.)
      CALL HBOOK1(1102,'Trig. Segm. : Char. Elmag. Trig   $',60,1.,61.,
     &   0.)
      CALL HBOOK1(1103,'Trig. Segm. : Neut. Elmag. Trig   $',60,1.,61.,
     &   0.)
      CALL HBOOK1(1109,'Trig. Segm. : Bhabha SICAL Trig   $',60,1.,61.,
     &   0.)
      CALL HBOOK1(1111,'Accepted Phys. Triggers           $',32,1.,33.,
     &   0.)
      RETURN
      END
#endif
