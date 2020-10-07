      SUBROUTINE ITBOOK
C-----------------------------------------------------------------------
C! Book ITC histograms
C!
C!   Author       :- J. Sedgbeer    87/03/02
C!   Modified     :- J. Sedgbeer    88/05/05
C!   Modified     :- J. Sedgbeer + I. Tomalin  89/06/05
C!
C!   called by : RBKHIS
C!   calls     : none
C!
C!   libraries:  HBOOK
C-----------------------------------------------------------------------
#ifndef DOC
#include "rparac.h"
#include "rflags.h"
C-----------------------------------------------------------------------
      JOFF = JULIT * 1000
C-----------------------------------------------------------------------
C Book histograms for ITC Prepare data.
C ITDI bank
C
      IF(JHISRF(JULIT).GE.2) THEN
        CALL HBOOK1(JOFF+ 1,'ITC: # of Digitisings/event'
     +                                            ,100,0.,1000.,0.)
        CALL HBOOK1(JOFF+ 2,'ITC: Wire Nos.of hit wires(layers 1-4)',
     +                                 48,  1.,385.,0.)
        CALL HBOOK1(JOFF+ 3,'ITC: Wire Nos.of hit wires(layers 5-8)',
     +                                 72,385.,961.,0.)
        CALL HBOOK1(JOFF+ 4,'ITC: TDC Spectrum (r-phi)',60,0.,600.,0.)
      END IF
C
      IF(JHISRF(JULIT).GE.3) THEN
        CALL HBOOK1(JOFF+11,'ITC: TDC Spectrum (r-phi) Layer 1'
     +                                              ,60,0.,600.,0.)
        CALL HBOOK1(JOFF+12,'ITC: TDC Spectrum (r-phi) Layer 2'
     +                                              ,60,0.,600.,0.)
        CALL HBOOK1(JOFF+13,'ITC: TDC Spectrum (r-phi) Layer 3'
     +                                              ,60,0.,600.,0.)
        CALL HBOOK1(JOFF+14,'ITC: TDC Spectrum (r-phi) Layer 4'
     +                                              ,60,0.,600.,0.)
        CALL HBOOK1(JOFF+15,'ITC: TDC Spectrum (r-phi) Layer 5'
     +                                              ,60,0.,600.,0.)
        CALL HBOOK1(JOFF+16,'ITC: TDC Spectrum (r-phi) Layer 6'
     +                                              ,60,0.,600.,0.)
        CALL HBOOK1(JOFF+17,'ITC: TDC Spectrum (r-phi) Layer 7'
     +                                              ,60,0.,600.,0.)
        CALL HBOOK1(JOFF+18,'ITC: TDC Spectrum (r-phi) Layer 8'
     +                                              ,60,0.,600.,0.)
      ENDIF
C
      IF(JHISRF(JULIT).GE.2) THEN
        CALL HBOOK1(JOFF+20,'ITC: Z-TDC Spectrum',60,0.,1200.,0.)
      END IF
C
      IF(JHISRF(JULIT).GE.3) THEN
        CALL HBOOK1(JOFF+21,'ITC: Z-TDC Spectrum. Layer 1'
     +                                             ,60,0.,1200.,0.)
        CALL HBOOK1(JOFF+22,'ITC: Z-TDC Spectrum. Layer 2'
     +                                             ,60,0.,1200.,0.)
        CALL HBOOK1(JOFF+23,'ITC: Z-TDC Spectrum. Layer 3'
     +                                             ,60,0.,1200.,0.)
        CALL HBOOK1(JOFF+24,'ITC: Z-TDC Spectrum. Layer 4'
     +                                             ,60,0.,1200.,0.)
        CALL HBOOK1(JOFF+25,'ITC: Z-TDC Spectrum. Layer 5'
     +                                             ,60,0.,1200.,0.)
        CALL HBOOK1(JOFF+26,'ITC: Z-TDC Spectrum. Layer 6'
     +                                             ,60,0.,1200.,0.)
        CALL HBOOK1(JOFF+27,'ITC: Z-TDC Spectrum. Layer 7'
     +                                             ,60,0.,1200.,0.)
        CALL HBOOK1(JOFF+28,'ITC: Z-TDC Spectrum. Layer 8'
     +                                             ,60,0.,1200.,0.)
      ENDIF
C
C ITCO bank
C
      IF(JHISRF(JULIT).GE.2) THEN
        CALL HBOOK1(JOFF+101,'ITC: No.of Coords/event',
     +                                              100,0.,1000.,0.)
      END IF
C
      IF(JHISRF(JULIT).GE.3) THEN
        CALL HBOOK1(JOFF+102,'ITC: coord. Phi values',35,0.,7.,0.)
        CALL HBOOK1(JOFF+103,'ITC: coord. Z values (cm)',
     +                                              50,-125.,125.,0.)
      ENDIF
C
C-----------------------------------------------------------------------
C Book histograms for ITC-TPC tracking.
C IGTL#1 bank.
C
      CALL HBOOK1(JOFF+201,'ITC: # of ITC-TPC(extended) tracks/event'
     +                                      ,50,0.,50.,0.)
      CALL HBOOK1(JOFF+202,'ITC: # ITC coords/ITC-TPC(extended) track'
     +                                      ,100,0.,100.,0.)
      CALL HBOOK1(JOFF+203,'ITC: Fraction of hits on each JULIA '//
     +'ITC-TPC(extended) track which were shared with its closest '
     +//'true MC track',100,0.0,1.0,0.0)
C
C-----------------------------------------------------------------------
C Book histograms for ITC tracking.
C IGTL#2 bank.
C
      CALL HBOOK1(JOFF+301,
     +   'ITC: # ITC tracks/event (no TPC assoc. track)'
     +                                      ,50,0.,50.,0.)
      CALL HBOOK1(JOFF+302,
     +   'ITC: # ITC coords/ITC track (no TPC assoc. track)'
     +                                      ,100,0.,100.,0.)
      CALL HBOOK1(JOFF+303,'ITC: Fraction of hits on each JULIA '//
     +'ITC track (no TPC assoc. track) which were shared with its '//
     +'closest true MC track',100,0.0,1.0,0.0)
C
C-----------------------------------------------------------------------
C Book histograms for ITC Monte Carlo tracks
C ITTD bank.
C
      IF(JHISRF(JULIT).GE.2) THEN
        CALL HBOOK1(JOFF+401,'ITC: # Monte Carlo true ITC tracks/event'
     +                                      ,100,0.,100.,0.)
        CALL HBOOK1(JOFF+402,'ITC: # digitisings/Monte Carlo track'
     +                                      ,100,0.,100.,0.)
      END IF
C-----------------------------------------------------------------------
      END
#endif
