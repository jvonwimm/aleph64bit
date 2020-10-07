#if defined(ALEPH_DEC) &&  ! defined(UNIX)
      SUBROUTINE BLO32W( source, n1, target, n2, n3 )
C
C?   SOURCE is an array of 16 bit words
C?   TARGET is an array of 32 bits words
C?   This routine fills N3 (32b) words starting at TARGET(N2)
C?   from SOURCE(N1). (16b) words are NOT swapped.
C
      INTEGER*2 SOURCE(1)
      INTEGER   TARGET(1)
      INTEGER N1, N2 N3
      CALL lib$movc3( 4*n3, source(n1), target(n2) )
      RETURN
      END
#endif
