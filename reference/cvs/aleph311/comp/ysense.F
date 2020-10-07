        SUBROUTINE YSENSE(PSI_P,PSI_IN,PSI_OUT,PSI_S,ISENSE)
****************************************************************
C!  Determine the sense of direction of a track
C
C           D.Casper    10-Aug-1997
C
C           Input:      PSI_P   turning angle of track at primary vertex
C                       PSI_IN  turning angle of track at innermost coordinate
C                       PSI_OUT turning angle of track at outermost coordinate
C                       PSI_S   turning angle of track at secondary vertex
C
C                       NOTE: these quantities should be computed by YHELIX
C
C           Output:     ISENSE  = -1 if error; 0-3 are "normal" returns in
C                                 all cases:
C                                 0  is reserved.  This routine never returns
C                                    a code of zero; it is intended to be set
C                                    later for tracks which are finally id'ed
C                                    as the incoming track.  Note that
C                                    changing a class 2 track into class 0
C                                    also implies a reversal of the track's
C                                    momentum.
C                                 1  track leaves secondary going outward
C                                    (or enters going inward)
C                                 2  track enters secondary going outward
C                                    (or leaves going inward)
C                                 3  track leaves secondary going outward
C                                    (or enters going inward)
C                                 4  track passes through secondary
C                                    (in either direction)
C                                 Return codes > 4 indicate a problem
C                                 if the "primary" is the true event
C                                 vertex near the origin.  See below for
C                                 details.  Note that it is impossible to
C                                 distinguish here between tracks which
C                                 *really* go inward or outward without
C                                 more information.  The caller must ultimately
C                                 decide; for example, it would normally be rare
C                                 for an radially inward track to create a
C                                 secondary vertex, hence class 1 and 3 are
C                                 probably daughter tracks of a secondary
C                                 vertex.  Likewise, if a class 2 track is
C                                 consistent with the primary vertex,
C                                 the caller may decide it is actually the
C                                 incoming track (code 0 is reserved for
C                                 this situation).
C
C   This routine is simplified by carefully computing psi with YHELIX, which
C   should treat vertices on spiral branches or behind the primary correctly.
C   There are a number of implicit assumptions which are checked for
C   validity before going ahead.  First, a track's coordinates must lie
C   at positive values of psi, with psi increasing with radius; this
C   should happen by construction when fitting the helix.  Since the
C   track may not come from the primary vertex, no requirement is made on
C   psi_primary.  The track *is* supposed to pass through the secondary
C   vertex, and hence the secondary must fall in a reasonable range of turning
C   angles; from -pi to +3pi/2 (psi=0 is where the track parameters are
C   defined and the track is closest to (0,0); psi=-pi corresponds to a half
C   turn before that, psi=pi is where the helix turns over, and thus the
C   psi < 3pi/2 requirement allows a vertex somewhat beyond the turn-around).
C
C   Subject to these constraints the following cases (i.e. ordering of
C   the turning angles are possible:
C
C 1  p s i o     The expected case; track originates from secondary
C 2  p i o s     Also reasonable; track ends at secondary
C 3  s p i o     Track created at secondary and loops past primary
C 4  p i s o     Track created at primary and passes through secondary;
C                could happen for a delta-ray emission...
C 5  s i o p *   Track created at secondary and ends at the primary 
C 6  s i p o *   Track created at secondary and loops through the primary 
C 7  i o p s *   Track ends at the primary 
C 8  i o s p *   Track ends at the secondary
C 9  i p o s *   Track passes through primary and ends at secondary
C 10 i s o p *   Track passes through secondary and ends at primary
C 11 i p s o *   Track passes through primary and then secondary
C 12 i s p o *   Track passes through secondary and then primary
C   
C   Cases marked with a star(*) indicate a problem if the primary is
C   at or near the origin (i.e. primary event vertex), since normally
C   the main event vertex/origin should fall before any coordinates (psi~0)
C   
C
***************************************************************
#ifndef DOC
#include "alcons.h"

C Validate input

        CALL ABRUEV(KRUN,KEVT)
        ISENSE = -1
        IF (PSI_IN.LT.0 .OR. PSI_OUT.LT.0) THEN
C          TYPE *,'YSENSE - negative psi_in/psi_out?!',
C     &                      krun,kevt,psi_in,psi_out
          RETURN
        ENDIF
        IF (PSI_IN.GE.PSI_OUT) THEN
C          TYPE *,'YSENSE - psi_in >= psi_out?!',
C     &          krun,kevt,psi_in,psi_out
          RETURN
        ENDIF
C        IF (ABS(PSI_P).GT.PI/2.) THEN
C          TYPE *,'YSENSE - psi_primary out of range?!',
C     &          krun,kevt,psi_p,psi_in,psi_out,psi_s
C          RETURN
C        ENDIF
        IF (PSI_S.LT.-PI .OR. PSI_S.GT.1.5*PI) THEN
C          TYPE *,'YSENSE - psi_secondary out of range?!',
C     &          krun,kevt,psi_p,psi_in,psi_out,psi_s
          RETURN
        ENDIF

C Deal with the easiest cases first - the primary, secondary, and coordinates
C all follow each other nicely

C p s i o
        IF (PSI_P.LE.PSI_S .AND. PSI_S.LE.PSI_IN) THEN
            ISENSE = 1
            RETURN
        ENDIF
C p i o s
        IF (PSI_P.LE.PSI_IN .AND. PSI_OUT.LE.PSI_S) THEN
            ISENSE = 2
            RETURN
        ENDIF
C s p i o
        IF (PSI_S.LE.PSI_P .AND. PSI_P.LE.PSI_IN) THEN
            ISENSE = 3
            RETURN
        ENDIF
C p i s o
        IF (PSI_P.LE.PSI_IN .AND. PSI_IN.LE.PSI_S 
     &      .AND. PSI_S.LE.PSI_OUT) THEN
            ISENSE = 4
        ENDIF

C Now the cases we don't expect...

C s i o p
        IF (PSI_S.LE.PSI_IN .AND. PSI_OUT.LE.PSI_P) THEN
            ISENSE = 5
            RETURN
        ENDIF
C s i p o
        IF (PSI_S.LE.PSI_IN .AND. PSI_IN.LE.PSI_P
     &      .AND. PSI_P.LE.PSI_OUT) THEN
            ISENSE = 6
            RETURN
        ENDIF
C i o p s
        IF (PSI_OUT.LE.PSI_P .AND. PSI_P.LE.PSI_S) THEN
            ISENSE = 7
            RETURN
        ENDIF
C i o s p
        IF (PSI_OUT.LE.PSI_S .AND. PSI_S.LE.PSI_P) THEN
            ISENSE = 8
            RETURN
        ENDIF
C i p o s
        IF (PSI_IN.LE.PSI_P .AND. PSI_P.LE.PSI_OUT
     &      .AND. PSI_OUT.LE.PSI_S) THEN
            ISENSE = 9
            RETURN
        ENDIF
C i s o p
        IF (PSI_IN.LE.PSI_S .AND. PSI_S.LE.PSI_OUT
     &      .AND. PSI_OUT.LE.PSI_P) THEN
            ISENSE = 10
            RETURN
        ENDIF
C i p s o
        IF (PSI_IN.LE.PSI_P .AND. PSI_P.LE.PSI_S
     &      .AND. PSI_S.LE.PSI_OUT) THEN
            ISENSE = 11
            RETURN
        ENDIF
C i s p o
        IF (PSI_IN.LE.PSI_S .AND. PSI_S.LE.PSI_P
     &      .AND. PSI_P.LE.PSI_OUT) THEN
            ISENSE = 12
            RETURN
        ENDIF

        RETURN
        END
#endif
