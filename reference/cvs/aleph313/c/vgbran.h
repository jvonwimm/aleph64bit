/*
C! Definitions for VGLOB branch and bound C routines 
CKEY VGLOB VDET TRACK
 */

#ifndef _VGBRAN_H_
#define _VGBRAN_H_

#ifdef VAX
#  define FORT_CALL(name) name
#else
/* for UNIX, append an underscore */
#  define FORT_CALL(name) name ## _
#endif

#include "lpkit.h"

#define NHITDIM 4

/*
 * this number must match the definition by the same name in vgbran.f !!! 
 */
#define GRKSCF 1000

/* 
 * this structure mirrors the pattern "bank" in vgbran.f 
 * (see indpat variable) 
 */
struct VGBPAT 
{
   int trk;
   int hit[NHITDIM];
   float obj;
};

/*
 * load the lprec structure with the problem
 */
lprec *vldlps(int ntrk, const int nhits[NHITDIM],
	      const int nUseHit[][NHITDIM], 
	      int npat, const struct VGBPAT *pat );

/*
 * solve the problem with branch and bound.
 *  *** CALLED FROM FORTRAN ***
 */
void FORT_CALL(vgbslv) (const int *ntrk, const int nhits[NHITDIM], 
			const int nUseHit[][NHITDIM], 
			const int *npat, const struct VGBPAT *pat,
			int *nsoln, int *soln, float *objSoln );


#endif
