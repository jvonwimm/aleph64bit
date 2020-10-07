      INTEGER MXSATW,IDUMMY,MNSPTW,JTHRTW,JCPAKW,MXMATW,IALGTW,NPRETW
      INTEGER NPSTTW,JTOLTW,KTRNTW,MXTNTW,MXRNTW,MNRATW
      REAL    TRMXTW,ZWINTW,SPACTW,AVPLTW,SLPLTW,CTMNTW,SLPZTW,RNCRTW
      REAL    TPACKW,RTHRTW,RSMSTW,WRMXTW,ZWSATW,ADCMTW,ZALOTW,DZNLTW
      REAL    THRSTW
      LOGICAL FSECTW,FZERTW,SPLITW
      COMMON/TWIRCT/ MXSATW,WRTLTW,TRMXTW,ZWINTW,SPACTW,IDUMMY,MNSPTW,
     &               JTHRTW,AVPLTW,SLPLTW,CTMNTW,FSECTW,SLPZTW,RNCRTW,
     &               JCPAKW,TPACKW,MXMATW,IALGTW,NPRETW,NPSTTW,RTHRTW,
     &               RSMXTW,WRMXTW,ZWSATW,JTOLTW,KTRNTW,ADCMTW,FZERTW,
     &               ZALOTW,MXTNTW,MXRNTW,DZNLTW,SPLITW,THRSTW,MNRATW
#if defined(DOC)
C! Parameters and cuts for analysis of TPC wire data
C
C   Parameters for wire pulse analysis (TWIRES)
C     JCPAKW =  Number to divide charge by when packing
C     TPACKW =  Number to multiply time by when packing
C     MXSATW =  Maximum number of saturated samples in a pulse before
C               the pulse-height sum is set to saturation (in the
C               case of algorithm 0)
C     JTHRTW =  Threshold used for counting pulse length and summing
C               pulse height.  Only samples above JTHRTW are counted.
C     AVPLTW =  Expected maximum length of wire pulse at zero dip angle
C               and zero drift length
C     SLPLTW =  Slope of increase in pulse length with dip angle
C     SLPZTW =  Slope of increase in pulse length with drift distance
C     IALGTW =  Algorithm choice for the reduction
C     NPRETW =  Number of presamples
C     NPSTTW =  Number of postsamples
C     RTHRTW =  Fractional threshold for time estimates
C     JTOLTW =  Maximum dip allowed in wire pulse-height distribution
C     WRMXTW =  Maximum rms width squared of a good wire pulse
C     SPLITW =  .TRUE. to try to split double pulses (use TRDWP3)
C     THRSTW =  Fractional threshold in TRDWP3 to get times of pulse
C               leading and trailing edges
C     MNRATW =  10 times minimum ratio of peak to valley ph in TRDWP3
C   Parameters for dE/dx (TRKWRA and TRKELS)
C     WRTLTW =  Required distance inside ends of wire for a track match
C     TRMXTW =  Maximum turning angle of track from its origin to
C               the wire crossing point
C     CTMNTW =  Minimum cosine of angle of track with wire in x-y plane
C     ZWINTW =  Z window cut for adding wire pulse to a track.  This
C               should always be less than or equal to SPACTW.
C     SPACTW =  Minimum z separation of wire pulses to avoid "bad" flag.
C               This should always be greater than or equal to ZWINTW.
C     MNSPTW =  Minimum number of dE/dx samples on track for TELS bank
C     FSECTW =  Flag:  set true to use TPCO point to determine which
C               sectors are crossed by each track (fast method)
C     RNCRTW =  Maximum change in dx from one sample to the next
C               allowed for using Taylor expansion for the logarithm.
C     MXMATW =  Maximum number of tracks allowed to match a hit
C               before the hit is rejected completely.  In any case,
C               if more than one track matches the hit, the hit is
C               labelled 'bad'
C     RSMXTW =  Maximum residual of the wire hit from the track for it
C               to be associated with the track
C     ZWSATW =  Minimum distance in time to a saturated pulse with
C               occurs at a shorter drift time
C     FZERTW =  .TRUE. to ignore zero pulses (below threshold hits)
C     KTRNTW =  Percentage of lowest pulse-height hits to drop when
C               calculating the truncated mean             (8)
C     ADCMTW =  ADC value to which the pulse height is set if the
C               hit was below the TPC threshold            (7)
C     ZALOTW =  Distance beyond TPC central membrane up to which
C               track intersections will still be counted.  (0.2)
C     MXTNTW =  Maximum number of track intersections with zero
C               hits on a wire for wire to be considered live (3)
C     MXRNTW =  Maximum allowed hits below threshold in a row (4)
C     DZNLTW =  Extra distance in z following the end of a pulse
C               throughout which no tracks are allowed to pick up
C               below threshold ph=zero hits (in cm).
C
C
C
C-----------------------------------------------------------------------
#endif
