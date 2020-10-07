/*
C! on Unix put an underscore at end of C routines 
 */

#if defined(VAX)
#  define FORT_CALL(name) name
#else
/* for UNIX, append an underscore */
#  define FORT_CALL(name) name ## _
#endif
