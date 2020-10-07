*CD trkcom
      PARAMETER (LETRK=27, LITRK=11, LNTRK=10)
      COMMON /TRKCOM/   ITRKEL(LITRK), TRKELE(LETRK), TRKNXT(LNTRK)
     &                , FTRHAD
      LOGICAL FTRHAD
      COMMON /TRKKAR/   TRKVOL
      CHARACTER*4 TRKVOL
C
#if defined(DOC)
      track element (TRKELE defines the point before the step, TRKNXT after)
      ITRKEL      track element integer parameters
              1   track number
              2   stack-track number (=0 means "primary")
              3   vertex number
              4   particle type
              5   volume name  (hollerith)
              6   volume number
              7   = 0 not in a selected sensitive volume
                  > 0 in a selected sensitive volume
              8   = 1 when entering a new volume or starting a new
                      particle ( step length = 0.)
                    2 when leaving a volume
                    3 when leaving the experimental set-up
                    0 otherwise
              9   = 1 when track looses its identity
                    2 when energy below cut
                    3 when shower is produced
              10   slot# of the volume
              11   tracking type

      TRKELE      starting point of the current step
            (1-7) X, Y, Z, cosA, cosB, cosC, P   before the step
              8   total energy  before the step
              9   track length before the step
             10   Time Of Flight before the step
             11   length of the step to be done
             12   energy lost during this step
             13   mass
             14   charge
           15-17   X, Y, Z offsets of the cumulative transformation
                   from the ALEPH system to the detector system.
           18-27   rotation matrix elements for the cumulative
                   tranformation from the ALEPH to the local system .
                   TRKELE(27)=0. indicates the null rotation.

      TRKNXT      end point of the current step
            (1-7) X, Y, Z, cosA, cosB, cosC, P  after the step
              8   total energy after the step
              9   track length after the step
             10   T.O.F. after the step
       FTRHAD     hadronic interaction flag
       TRKVOL     volume name  (character*4)
C
#endif
