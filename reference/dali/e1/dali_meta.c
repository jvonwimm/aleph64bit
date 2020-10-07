/*
 * DALI_META.C contains routines for metafile generation. Can be called
 * from both DALI_UIS and DALI_X.
 * Bjorn S. Nilsson, May 1992.
 */
#if defined(__DECC) && defined(__VMS)
#define fgetname decc$fgetname
#define cuserid  decc$cuserid
#endif /* DECC on VMS BSN */
#ifdef __hpux
#define _HPUX_SOURCE
#define unix
#define __unix__
#endif /* HP */
#if defined(__unix) && !defined(__unix__)
#define __unix__
#endif /* For SGI */

#ifndef VAXC
#include <unistd.h>
#endif
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <math.h>
#include <string.h>

#ifdef VMS
#include <iodef.h>
#include <trmdef.h>
#include <ssdef.h>
#endif /* VMS */

#include "dali_defs.h"
#include "commons.h"
#include "ps_abbrevs.h"

/*
 * Global variables and structures.
 */
logical debug_mask[30] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
char m_line[257];
float PSwidth_unit = 1.0;
char tmeta[80] = " ";
FILE *fmeta;
/*
 * On VMS, tot_meta_chars sums the # bytes written. On Ultrix it is the sum
 * of the fprintf return values, which should be zero.
 */
int tot_meta_chars = 0;
int meta_BW = -1; /* 0 for black, 1 for white */
int meta_COL = -1; /* 1 if Colour data is recorded in meta file */

/*
 * Static variables holding info private to dali_meta.
 */
static logical verticaltext = False;
static float font_scale[6];
static int meta_BW_div;
static int meta_BW_clip = -1;
static int meta_BW_higher, meta_BW_lower;
static int meta_writing_errors = 0;
static max_tot_meta_chars = 0;
#define max_meta_marks 50
static int number_of_meta_marks;
static fpos_t meta_marks[max_meta_marks];
static int meta_marks_chars[max_meta_marks];
static int meta_fg_store = -1;
static int prev_linewidth = -1;
static int clip_on = 0;
static char clip_line[121];
static int clip_foreground;
static logical debugm = False;
/*
 * Function prototypes - only those explicitly required appear below.
 */
void text_to_PS();
void mprintf();
void write_colors_to_meta();
void mdglevl();
void dgtint();

void mdgclpa(
	int*   lclp,
	float* h1cl,
	float* v1cl,
	float* h2cl,
	float* v2cl
)
/*
 * Define a clipping rectangle.
 */
{
	if (debugm) {
		sprintf(m_line,"%%dgclpa iaredo:%d", dopr1c.iaredo);
		mprintf(m_line, 1);
	}
	sprintf(clip_line,
		"gsave %s %.1f %.1f %s %.1f %.1f %s %.1f %.1f %s %.1f %.1f %s closepath clip",
		PS_newpath,
		*h1cl, *v1cl, PS_moveto,
		*h1cl, *v2cl, PS_lineto,
		*h2cl, *v2cl, PS_lineto,
		*h2cl, *v1cl, PS_lineto);
	clip_on = 1;
	if (meta_COL > 0) {
		char line[12];
		if (*lclp < 0 || *lclp >= ddefcn.numcdd) {
			printf("\nWarning: color index %d is out of range", *lclp);
			fflush(stdout);
		} else {
			sprintf(line, " setcolor%02d", *lclp);
			strcat(clip_line, line);
		}
	}
	if (ddefco.ltabdd[*lclp] <= meta_BW_div) {
/*
 * Lower range of indices.
 */
		meta_BW_clip = meta_BW_lower;
	} else {
/*
 * Higher range of indices.
 */
		meta_BW_clip = meta_BW_higher;
	}
}

void mdgalph(
	int   *leva,
	float *halef,
	float *valef,
	int *ascent
)
{
	mdglevl(leva);
	if (debugm) {
		sprintf(m_line,"%%dgalph iaredo:%d", dopr1c.iaredo);
		mprintf(m_line, 1);
	}
	if (meta_COL <= 0 && meta_BW == 1) return;
	sprintf(m_line,"F01 (ALEPH) %d %d %s", (int) *halef,
#ifdef Previous
		(int) *valef - *ascent, PS_moveto_show);
#else
		(int) *valef, PS_moveto_show);
#endif
	mprintf(m_line, 1);
}

void mdgpoin(
	float *hpoi,
	float *vpoi
)
{
	if (debugm) {
		sprintf(m_line,"%%dgpoin iaredo:%d", dopr1c.iaredo);
		mprintf(m_line, 1);
	}
	if (meta_COL <= 0 && meta_BW == 1) return;
	sprintf(m_line, "%s %.1f %.1f %s", PS_newpath, *hpoi, *vpoi, PS_point);
	mprintf(m_line, 1);
}

void mdgplot(
	int   *n,
	float h[],
	float v[]
)
{
	int index;

	if (debugm) {
		sprintf(m_line,"%%dgplot iaredo:%d", dopr1c.iaredo);
		mprintf(m_line, 1);
	}
	if (meta_COL <= 0 && meta_BW == 1) return;
	for (index = 0; index < *n; index++) {
		sprintf(m_line,
			"%s %.1f %.1f %s 0 0 currentlinewidth 0 360 arc closepath fill",
			PS_newpath, h[index], v[index], PS_moveto);
		mprintf(m_line, 1);
	}
}

void mdgdash(
	int   *n,
	int dashes[]
)
{
	if (debugm) {
		sprintf(m_line,"%%dgdash iaredo:%d", dopr1c.iaredo);
		mprintf(m_line, 1);
	}
	if (meta_COL <= 0 && meta_BW == 1) return;
	if (*n ==0) {
		sprintf(m_line, "[] 0 %s", PS_setdash);
	} else {
		int ns = abs(*n);
		int index;
		char m_temp[80];

		sprintf(m_line, "[%d", dashes[0]);
		for (index=1; index<ns; index++) {
			sprintf(m_temp, " %d", dashes[index]);
			strcat(m_line, m_temp);
		}
		sprintf(m_temp, "] 0 %s", PS_setdash);
		strcat(m_line, m_temp);
	}
	mprintf(m_line, 1);
}

void mdgdraw(
	int   *n,
	float h[],
	float v[]
)
{
	int index;

	if (debugm) {
		sprintf(m_line,"%%dgdraw iaredo:%d", dopr1c.iaredo);
		mprintf(m_line, 1);
	}
	if (meta_COL <= 0 && meta_BW == 1) return;
	sprintf(m_line, "%s %.1f %.1f %s", PS_newpath, h[0], v[0], PS_moveto);
	mprintf(m_line, 1);
	for (index = 1; index < *n; index++) {
		sprintf(m_line, "%.1f %.1f %s", h[index], v[index], PS_lineto);
		mprintf(m_line, 0);
	}
	sprintf(m_line, "%s", PS_stroke);
	mprintf(m_line, 0);
}

void mdgtext(
	float *htxt,
	float *vtxt,
	FortranString *text,
	int   *n,
	int   *ascent
)
{
	unsigned char *m_temp = (unsigned char *) calloc(*n + 2, sizeof(char));
	int x, y;

#ifdef vms
	text_to_PS(text->dsc$a_pointer, *n, m_temp);
#else
	text_to_PS(text, *n, m_temp);
#endif /* VMS character */
	if (debugm) {
		sprintf(m_line,"%%dgtext %s iaredo:%d", m_temp, dopr1c.iaredo);
		mprintf(m_line, 1);
	}
	if (meta_COL <= 0 && meta_BW == 1) return;

	if (verticaltext) {
		x = (int) *htxt + dtxthm + *ascent;
		y = (int) *vtxt + dtxtvm;
		sprintf(m_line,"FMV (%s) %d %d %s", m_temp, x, y, PS_moveto_show_vert);
	} else {
		x = (int) *htxt + dtxthm;
		y = (int) *vtxt + dtxtvm - *ascent;
		sprintf(m_line,"FM (%s) %d %d %s", m_temp, x, y, PS_moveto_show);
	}
	mprintf(m_line, 1);
}

void mdgtxtg1(
	float *htxt,
	float *vtxt,
	FortranString *text,
	int *n,
	int *nposgr,
	int *ntotgr
)
/*
 * Dummy at the moment. Insert here for DALI meta file.
 */
{
}

void mdgtxtg2(
	int *htxt,
	int *vtxt,
	FortranString *text,
	int   *ascent
)
{
	char m_temp[10];
	int x, y;
	int xfudge, yfudge;

#ifdef vms
	text_to_PS(text->dsc$a_pointer, 1, m_temp);
#else
	text_to_PS(text, 1, m_temp);
#endif /* VMS character */
	if (debugm) {
		sprintf(m_line,"%%dgtxtg %s iaredo:%d", m_temp, dopr1c.iaredo);
		mprintf(m_line, 1);
	}
	if (meta_COL <= 0 && meta_BW == 1) return;
	if (dtvccc.fx11dt) {
		xfudge = dtxthm;
		yfudge = dtxtvm;
	} else {
/*
 * This is a temporary fix to place UIS characters correctly.
 */
		xfudge = -dtxthm;
		yfudge = 0;
	}
	if (verticaltext) {
		x = (int) *htxt + xfudge + *ascent;
		y = (int) *vtxt + yfudge;
		sprintf(m_line,"FMV (%s) %d %d %s", m_temp, x, y, PS_moveto_show_vert);
	} else {
		x = (int) *htxt + xfudge;
		y = (int) *vtxt + yfudge - *ascent;
		sprintf(m_line,"FM (%s) %d %d %s", m_temp, x, y, PS_moveto_show);
	}
	mprintf(m_line, 1);
}

void mdgtdir(
	int *jdir
)
{
	if (*jdir == 90) {
		verticaltext = True;
	} else {
		verticaltext = False;
	}
}

void mdglevl(
	int *l
)
{
	meta_fg_store = *l;
	verticaltext = False;
	if (debugm) {
		sprintf(m_line, "%%dglevl %d %d iaredo:%d", *l, ddefco.ltabdd[*l], dopr1c.iaredo);
		mprintf(m_line, 1);
	}
	if (meta_COL > 0) {
/*
 * Change colour with a setcolorxx command to be defined in the beginning
 * of the PostScript file.
 */
		if (*l < 0 || *l >= ddefcn.numcdd ) {
			printf("\nWarning: color index %d is out of range", *l);
			fflush(stdout);
		} else {
			sprintf(m_line, "setcolor%02d", *l);
			mprintf(m_line, 1);
		}
	}
	if (meta_COL >= 0) return;
	if (ddefco.ltabdd[*l] <= meta_BW_div) {
/*
 * Lower range of indices.
 */
		if (meta_BW != meta_BW_lower) {
			meta_BW = meta_BW_lower;
			sprintf(m_line, "%d setgray", meta_BW_lower);
			mprintf(m_line, 1);
		}
	} else {
/*
 * Higher range of indices.
 */
		if (meta_BW != meta_BW_higher) {
			meta_BW = meta_BW_higher;
			sprintf(m_line, "%d setgray", meta_BW_higher);
			mprintf(m_line, 1);
		}
	}
}

void mdglevs(
	int *l0,
	int *foreg
)
{
	if (debugm) {
		sprintf(m_line,"%%dglevs %d %d iaredo:%d", *l0, *foreg, dopr1c.iaredo);
		mprintf(m_line, 1);
	}
	if (meta_COL > 0) {
/*
 * Change colour with a setcolorxx command to be defined in the beginning
 * of the PostScript file.
 */
		int ls = (*l0 < 0) ? meta_fg_store : *l0;

		if (ls < 0 || ls >= ddefcn.numcdd) {
			printf("\nWarning: color index %d is out of range", ls);
			fflush(stdout);
		}
		sprintf(m_line, "setcolor%02d", ls);
		mprintf(m_line, 1);
	}
	if (meta_COL >= 0) return;
	if (*foreg <= meta_BW_div) {
/*
 * Lower range of indices.
 */
		if (meta_BW != meta_BW_lower) {
			meta_BW = meta_BW_lower;
			sprintf(m_line, "%d setgray", meta_BW_lower);
			mprintf(m_line, 1);
		}
	} else {
/*
 * Higher range of indices.
 */
		if (meta_BW != meta_BW_higher) {
			meta_BW = meta_BW_higher;
			sprintf(m_line, "%d setgray", meta_BW_higher);
			mprintf(m_line, 1);
		}
	}
}

void mdgarcl(
	int *n,
	float h[],
	float v[],
	int *narc
)
{
	int index;

	if (debugm) {
		sprintf(m_line,"%%dgarcl iaredo:%d", dopr1c.iaredo);
		mprintf(m_line, 1);
	}
/*
 * The next is possibly wrong. Try setgray instead.
 */
	if (meta_COL <= 0 && meta_BW_clip == 1) return;
	if (meta_COL == 0 && meta_BW == 1) return;
	if (clip_on != 1) {
		printf("\nWARNING: Clipping has not been defined."); fflush(stdout);
	} else {
		mprintf(clip_line, 1);
	}
	if (meta_COL < 0) {
		sprintf(m_line, "0 setgray");
		mprintf(m_line, 1);
	}
	sprintf(m_line, "%s %.1f %.1f %s", PS_newpath, h[0], v[0], PS_moveto);
	mprintf(m_line, 0);
	for (index = 1; index < *narc; index++) {
		sprintf(m_line, "%.1f %.1f %s", h[index], v[index], PS_lineto);
		mprintf(m_line, 0);
	}
	sprintf(m_line, "fill");
	mprintf(m_line, 0);
	if (clip_on == 1) {
		sprintf(m_line, "grestore");
		mprintf(m_line, 0);
	} else if (meta_COL < 0) {
		sprintf(m_line, "%d setgray", meta_BW);
		mprintf(m_line, 0);
	}
}

void mdgarea(
	int *n,
	float h[],
	float v[]
)
{
	int index;

	if (debugm) {
		sprintf(m_line,"%%dgarea iaredo:%d", dopr1c.iaredo);
		mprintf(m_line, 1);
	}
	if (meta_COL <= 0 && meta_BW == 1) return;
	sprintf(m_line, "%s %.1f %.1f %s", PS_newpath, h[0], v[0], PS_moveto);
	mprintf(m_line, 1);
	for (index = 1; index < *n; index++) {
		sprintf(m_line, "%.1f %.1f %s", h[index], v[index], PS_lineto);
		mprintf(m_line, 0);
	}
	sprintf(m_line, "fill");
	mprintf(m_line, 0);
}

void mdgcler(
	float *hc1,
	float *hc2,
	float *vc1,
	float *vc2
)
{
	if (debugm) {
		sprintf(m_line,"%%dgcler iaredo:%d", dopr1c.iaredo);
		mprintf(m_line, 1);
	}
/*
 *	if (meta_BW == 1)
 */
		 return;
/*!!
	sprintf(m_line, "1 setgray");
	mprintf(m_line, 1);
	sprintf(m_line,
		"%s %.1f %.1f %s %.1f %.1f %s %.1f %.1f %s %.1f %.1f %s", PS_newpath,
		*hc1, *vc1, PS_moveto,
		*hc1, *vc2, PS_lineto,
		*hc2, *vc2, PS_lineto,
		*hc2, *vc1, PS_lineto);
	mprintf(m_line, 0);
	sprintf(m_line, "closepath fill");
	mprintf(m_line, 0);
	if (meta_BW >= 0) {
		sprintf(m_line,"%d setgray", meta_BW);
		mprintf(m_line, 0);
	}
!!*/
}

int make_PS_prolog(
	int kind,
	int bound_x,
	int bound_y
)
{
/*
 * Open output file and Write PostScript prolog. This is modelled after
 * DECW$CAPTURE file. Just so that you know who really made it.
 * kind is 1 for BW screen dumps, 2 for BW metafile, 3 for colour screen
 * dumps and 4 for colour metafile.
 * Some of the PostScript comment fixed fields could be subject to change.
 */
	char node[64], line[257], *cp;
	static char *months[] = {
		"Jan", "Feb", "Mar", "Apr", "May", "Jun",
		"Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
	};
	int ip, node_len, deb_chars;
	time_t time_now;
	struct tm *timev;

	time_now = time((time_t *) NULL);
	timev = localtime(&time_now);

	if ((tmeta[0] == ' ') || (tmeta[0] == '\0'))
		strcpy(tmeta,"dali_bw.ps");
	fmeta = fopen(tmeta, "w");
	if (fmeta == NULL) {
		printf("\nCould not open file %s. No action taken.\n", tmeta);
		fflush(stdout);
		return 0;
	} else {
#ifdef VMS
		if (fgetname(fmeta, line) == NULL) strcpy(line, tmeta);
#else
/*
 * Can't this be done smarter?
 */
		if ((tmeta[0] == '~') || (tmeta[0] == '/')) {
			(void) strcpy(line, tmeta);
		} else {
			(void) getcwd(line, sizeof(line));
			(void) strcat(line, "/");
			(void) strcat(line, tmeta);
		}
#endif /* VMS */
	}
	meta_writing_errors = 0;
/*
 * Header.
 */
	max_tot_meta_chars = 0;
	tot_meta_chars = fprintf(fmeta,"%%!PS-Adobe-2.0 EPSF-1.2\n");
	tot_meta_chars += fprintf(fmeta,"%%%%Title: %s\n", line);
	switch (kind) {
		case 1: tot_meta_chars +=
					fprintf(fmeta, "%%%%Creator: DALI PS mono dump V1.0 10-Nov-1991\n");
				break;
		case 2: tot_meta_chars +=
					fprintf(fmeta, "%%%%Creator: DALI PS mono meta V1.1 21-Nov-1991\n");
				break;
		case 3: tot_meta_chars += fprintf(fmeta,
					"%%%%Creator: DALI PS colour dump V1.2 21-May-1992\n");
				break;
		case 4: tot_meta_chars += fprintf(fmeta,
					"%%%%Creator: DALI PS colour meta V1.1 20-May-1992\n");
				break;
	}
	ip = 1900 + timev->tm_year;
	if (timev->tm_year < 90) ip = 2000 + timev->tm_year;
	tot_meta_chars += fprintf(fmeta,
		"%%%%CreationDate: %d-%s-%d %02d:%02d:%02d\n",
		timev->tm_mday, months[timev->tm_mon], ip,
		timev->tm_hour, timev->tm_min, timev->tm_sec);
#ifdef vms
/*
 * node::user is VMS-specific.
 */
	cp = getenv("SYS$NODE");
	if (cp != NULL) {
		strcpy(node, cp);
		cp = cuserid((char *)NULL);
		tot_meta_chars += fprintf(fmeta,"%%%%For: %s%s", node, cp);
	} else {
		tot_meta_chars += fprintf(fmeta,"%%%%For: ? ");
	}
#else
	node_len = sizeof(node);
	if (gethostname(node, node_len) == 0) {
		cp = (char *)cuserid((char *)NULL);
/*
 * Fixup CERN node names missing the .cern.ch
 */
		if ((strncmp(node, "dxal", 4) == 0) && (strchr(node, '.') == NULL))
			strcat(node, ".cern.ch");
		tot_meta_chars += fprintf(fmeta,"%%%%For: %s@%s", cp, node);
	} else {
		tot_meta_chars += fprintf(fmeta,"%%%%For: ? ");
	}
#endif /* vms */

	if (devtic.ievtde[0] > 0) {
		tot_meta_chars += fprintf(fmeta, " Aleph run %d event %d\n",
			devtic.irunde[0], devtic.ievtde[0]);
	} else {
		tot_meta_chars += fprintf(fmeta, " No event displayed.\n");
	}
	tot_meta_chars += fprintf(fmeta,"%%%%Pages: 1\n");
	tot_meta_chars += fprintf(fmeta,"%%%%BoundingBox:41 18 572 774\n");
	tot_meta_chars += fprintf(fmeta,"%%%%EndComments\n");
	tot_meta_chars += fprintf(fmeta,"%%%%EndProlog\n");
/*
 * Page setup. Note the small extra header to identify the owner.
 */
	tot_meta_chars += fprintf(fmeta,"%%%%Page: 1 1\n");
	tot_meta_chars += fprintf(fmeta,
		"%%%%Comment: For inclusion in LaTeX etc, comment the next 7 lines.\n");
	tot_meta_chars += fprintf(fmeta,"15 dict begin\n");
	tot_meta_chars += fprintf(fmeta,"/temp-save save def\n");
	tot_meta_chars += fprintf(fmeta,
		"9 /Times-Roman findfont exch scalefont setfont\n");
	strncpy(line, dcftvt.tfildc, 8); /* Version */
	line[8] = '\0';
	tot_meta_chars += fprintf(fmeta,
		"300 820 moveto (Made on %d-%s-%d %02d:%02d:%02d by %s with %s) show\n",
		timev->tm_mday, months[timev->tm_mon], ip,
		timev->tm_hour, timev->tm_min, timev->tm_sec, cp, line);
	tot_meta_chars += fprintf(fmeta,
		"300 810 moveto (Filename: %s) show\n", tmeta);
	tot_meta_chars += fprintf(fmeta,"572 18 translate\n");
	tot_meta_chars += fprintf(fmeta,"90 rotate\n");
	tot_meta_chars += fprintf(fmeta,
		"%%%%Comment: You may adjust the size of the picture by changing the scale.\n");
	tot_meta_chars += fprintf(fmeta,
		"%%%%Comment: Note that the BoundingBox above should then also be changed.\n");
	tot_meta_chars += fprintf(fmeta,"756 531 scale\n");
	switch (kind) {
		case 1:
		tot_meta_chars += fprintf(fmeta,"/scale { pop pop } bind def\n");
		tot_meta_chars += fprintf(fmeta,"/showpage {} def\n");
		tot_meta_chars += fprintf(fmeta,"%%%%BeginDocument: DALI screendump\n");
		tot_meta_chars += fprintf(fmeta,"%%!PS-Adobe-2.0 EPSF-1.2\n");
		tot_meta_chars += fprintf(fmeta,"%%%%Title: Bitmap Image\n");
		tot_meta_chars += fprintf(fmeta,"%%%%Creator: DALI PS mono dump\n");
		break;

		case 2:
		tot_meta_chars += fprintf(fmeta,"/showpage {} def\n");
		tot_meta_chars += fprintf(fmeta,
			"%%%%BeginDocument: DALI PostScript metafile\n");
		tot_meta_chars += fprintf(fmeta,"%%!PS-Adobe-2.0 EPSF-1.2\n");
		tot_meta_chars += fprintf(fmeta,"%%%%Title: DALI vector graphics\n");
		tot_meta_chars += fprintf(fmeta,"%%%%Creator: DALI PS mono meta\n");
		break;

		case 3:
		tot_meta_chars += fprintf(fmeta,"/scale { pop pop } bind def\n");
		tot_meta_chars += fprintf(fmeta,"/showpage {} def\n");
		tot_meta_chars += fprintf(fmeta,"%%%%BeginDocument: DALI screendump\n");
		tot_meta_chars += fprintf(fmeta,"%%!PS-Adobe-2.0 EPSF-1.2\n");
		tot_meta_chars += fprintf(fmeta,"%%%%Title: Bitmap Image\n");
		tot_meta_chars += fprintf(fmeta,"%%%%Creator: DALI PS colour dump\n");
		break;

		case 4:
		tot_meta_chars += fprintf(fmeta,"/showpage {} def\n");
		tot_meta_chars += fprintf(fmeta,
			"%%%%BeginDocument: DALI colour PostScript metafile\n");
		tot_meta_chars += fprintf(fmeta,"%%!PS-Adobe-2.0 EPSF-1.2\n");
		tot_meta_chars += fprintf(fmeta,"%%%%Title: DALI vector graphics\n");
		tot_meta_chars += fprintf(fmeta,"%%%%Creator: DALI PS colour meta\n");
		break;
	}
	tot_meta_chars += fprintf(fmeta,"%%%%BoundingBox: 0 0 %d %d\n",
		bound_x, bound_y);
	tot_meta_chars += fprintf(fmeta,"%%%%Pages: 1 1\n");
	tot_meta_chars += fprintf(fmeta,"%%%%EndComments\n");
	tot_meta_chars += fprintf(fmeta,"/DALI_dict 100 dict def\n");
	tot_meta_chars += fprintf(fmeta,"DALI_dict begin\n");
	tot_meta_chars += fprintf(fmeta,"/inch {72 mul} def end\n");
	tot_meta_chars += fprintf(fmeta,"%%%%EndProlog\n");
/*
 * Start of page proper.
 */
	tot_meta_chars += fprintf(fmeta,"%%%%Page: 1 1\n");
	tot_meta_chars += fprintf(fmeta,"save\n");
	tot_meta_chars += fprintf(fmeta,"DALI_dict begin\n");

	return 1;
}

void make_PS_postamble()
{
/*
 * Write the postamble to the PostScript metafile and close it.
 */
	fpos_t deb_chars;

	tot_meta_chars += fprintf(fmeta,"restore showpage end\n");
	tot_meta_chars += fprintf(fmeta,"%%%%Trailer\n");
	tot_meta_chars += fprintf(fmeta,"\n");
	tot_meta_chars += fprintf(fmeta,"%%%%EndDocument\n");
	tot_meta_chars += fprintf(fmeta,
		"%%%%Comment: For inclusion in LaTeX etc, comment out what is after here.\n");
	tot_meta_chars += fprintf(fmeta,"temp-save restore\n");
	tot_meta_chars += fprintf(fmeta,"end\n");
	tot_meta_chars += fprintf(fmeta,"showpage\n");
	tot_meta_chars += fprintf(fmeta,"%%%%Trailer\n");
	tot_meta_chars += fprintf(fmeta,"%%End-of-file\n");
#ifdef ultrix
	if (tot_meta_chars == 0) {
/*
 * Status is OK for Ultrix, find total number of characters written.
 */
		fgetpos(fmeta, &tot_meta_chars);
	}
#else
	fgetpos(fmeta, &deb_chars);
#endif /* ultrix */
	if (max_tot_meta_chars > tot_meta_chars) {
		int i, totc;

		for (i = 0; i < 3; i++) {
			tot_meta_chars += fprintf(fmeta, " \n");
		}
		tot_meta_chars +=
			fprintf(fmeta, "%% You may wish to remove the rest of the file !\n");
		for (i = 0; i < 3; i++) {
			tot_meta_chars += fprintf(fmeta, " \n");
		}
		totc = tot_meta_chars;
		while (totc < max_tot_meta_chars) {
			totc += fprintf(fmeta,"%%Ignore                                                                        \n");
		}
		printf("\nNote: Due to editing the meta file %s", tmeta);
		printf("\ncontains irrelevant lines at the end. You may want");
		printf("\nto remove these with a real editor.");
#ifdef ultrix
		fflush(stdout);
		if (tot_meta_chars == 0) {
/*
 * Status is OK for Ultrix, find total number of characters written.
 */
			fgetpos(fmeta, &tot_meta_chars);
		}
#endif /* ultrix */
	}
	fclose(fmeta);
/*
 * Nullify everything to make it look fresh.
 */
	fmeta = NULL;
	number_of_meta_marks = 0;
	duiscc.fmetdu = False;
}

void dgmkme(
int *iprnt
)
{
/*
 * Insert a mark in the meta file, i.e., record byte offset.
 */
#ifdef vms
	long ipos;
#else
	fpos_t ipos;
#endif

	if (fmeta == NULL) {
		printf("\nNo open meta file to mark."); fflush(stdout);
		return;
	}
	if (number_of_meta_marks == max_meta_marks) {
		printf("\nMeta file mark table is full.\n"); fflush(stdout);
		return;
	}
	number_of_meta_marks++;
	tot_meta_chars +=
		fprintf(fmeta, "\n%%Mark number %d\n", number_of_meta_marks);
	if (fgetpos(fmeta, &meta_marks[number_of_meta_marks - 1]) != 0) {
		printf("\nMark insertion in meta file failed.\n"); fflush(stdout);
		return;
	}
#ifdef vms
	ipos = tot_meta_chars;
#else
	fgetpos(fmeta,&ipos);
#endif /* vms */
	meta_marks_chars[number_of_meta_marks - 1] = ipos;
	if (*iprnt > 0) printf("\nMark number %d inserted at offset %d.",
		number_of_meta_marks, ipos);
}

void dgedme()
{
/*
 * Edit the metafile. Only function now is to remove characters at the end
 * starting at a mark recorded in dgmkme.
 */
	int mark, lmark, i;

	if (fmeta == NULL) {
		printf("\nNo open meta file to edit."); fflush(stdout);
		return;
	}
	if (number_of_meta_marks == 0) {
		printf("\nThere are no marks in the meta file.");
		printf("\nNo action taken."); fflush(stdout);
		return;
	}
	printf("\nThere are %d marks in the meta file. Byte offsets:\n",
		number_of_meta_marks);
	for (i = 0; i < number_of_meta_marks; i++) {
		printf(" %d", meta_marks_chars[i]);
	}
	printf("\nFrom which one will you erase? [%d] ", number_of_meta_marks);
	fflush(stdout);
	dgtint(&mark, &lmark);
	if (lmark <= 0) mark = number_of_meta_marks;
	if ((mark > number_of_meta_marks) || (mark <= 0)) {
		printf("\nNo such mark, no action taken.\n");
		return;
	} else {
		if (fsetpos(fmeta, &meta_marks[mark - 1]) != 0) {
			printf("\nCould not reposition meta file %s.\n", tmeta);
			fflush(stdout);
			return;
		}
		number_of_meta_marks = mark;
		if (max_tot_meta_chars < tot_meta_chars)
			max_tot_meta_chars = tot_meta_chars;
		tot_meta_chars = meta_marks_chars[number_of_meta_marks - 1];
		printf("\nFile pointer was repositioned."); fflush(stdout);
	}
}

void dgname(
#ifdef vms
	FortranString *tmet
#else
	FortranString *tmet,
	int tmet_l
#endif /* VMS character */
)
{
/*
 * This routine sets the name of the metafile.
 */
	int index;
#ifdef vms
	int tmet_l = tmet->dsc$w_length;
#endif /* VMS */

	if (meta_COL != (-1)) {
		printf("\nYou cannot rename the meta file.");
		printf("\n%s is already open.", tmeta); fflush(stdout);
		return;
	}
	for (index = 0; index < tmet_l; index++) {
#ifdef vms
		tmeta[index] = (char) tmet->dsc$a_pointer[index];
#else
		tmeta[index] = (char) tmet[index];
#endif /* VMS character */
		if(tmeta[index] == ' ') {
			tmeta[index] = '\0';
			break;
		}
	}
	if (tmet_l < sizeof(tmeta)) tmeta[tmet_l] = '\0';
}

void dgqume(
	int *nchar,
#ifdef vms
	FortranString *tmet
#else
	FortranString *tmet,
	int *tmetl
#endif /* vms */
)
{
/*
 * This routine returns information on metafile activity.
 */
	char line[128];

	if (fmeta == NULL) {
		*nchar = -1;
		return;
	}
#ifdef ultrix
	fgetpos(fmeta,nchar);
#else
	*nchar = tot_meta_chars;
#endif /* ultrix */
#ifdef vms
	if (fgetname(fmeta, line) == NULL) strcpy(line, tmeta);
#else
/*
 * Can't this be done smarter?
 */
	if ((tmeta[0] == '~') || (tmeta[0] == '/')) {
		(void) strcpy(line, tmeta);
	} else {
		(void) getcwd(line, sizeof(line));
		(void) strcat(line, "/");
		(void) strcat(line, tmeta);
	}
#endif /* vms */
	if (tmeta[0] != ' ') {
#ifdef vms
		strncpy((char *) tmet->dsc$a_pointer, line, tmet->dsc$w_length);
#else
		strncpy((char *) tmet, line, *tmetl);
#endif /* VMS character */
	}
}

void dgopme(
	int *kind,
	int *pr_default
)
{
/*
 * Initialize PostScript metafile. kind is 2 for BW metafile
 * and 4 for colour metafile.
 * pr_default is 1, 2 or 3 if default printer should be set to bw, gray or
 * colour.
 * xsize and ysize are dimensions in pixels used for calculating the
 * bounding box. At the moment we assume default sizes.
 */
	int i1;
	static int xsize = 992, ysize = 696;

	debugm = debug_mask[(int)('m'-'a')];
	if (make_PS_prolog(*kind, ((xsize)*72)/300+1, ((ysize)*72)/300+1) == 0)
		return;
	tot_meta_chars += fprintf(fmeta, "/xmin 1 def\n");
	tot_meta_chars += fprintf(fmeta, "/xmax %d def\n", xsize);
	tot_meta_chars += fprintf(fmeta, "/ymin 1 def\n");
	tot_meta_chars += fprintf(fmeta, "/ymax %d def\n", ysize);
	tot_meta_chars += fprintf(fmeta,
		"1 xmax xmin sub div 1 ymax ymin sub div scale\n");
	tot_meta_chars += fprintf(fmeta, "/LT/lineto load def\n");
	tot_meta_chars += fprintf(fmeta, "/MV/moveto load def\n");
	tot_meta_chars += fprintf(fmeta, "/NP/newpath load def\n");
	tot_meta_chars += fprintf(fmeta, "/S/stroke load def\n");
	tot_meta_chars += fprintf(fmeta, "/SD/setdash load def\n");
	tot_meta_chars += fprintf(fmeta, "/MS {moveto show} bind def\n");
	tot_meta_chars += fprintf(fmeta,
		"/MSV {moveto gsave 90 rotate show grestore} bind def\n");
	tot_meta_chars += fprintf(fmeta,
		"/PO {ourpointsize 0 360 arc closepath fill} bind def\n");
/*
 * Setup for font encoding.
 */
	tot_meta_chars += fprintf(fmeta, "mark/ISOLatin1Encoding\n");
	tot_meta_chars += fprintf(fmeta,
		"8#000 1 8#001{StandardEncoding exch get}for /emdash/endash\n");
	tot_meta_chars += fprintf(fmeta,
		"8#004 1 8#025{StandardEncoding exch get}for /quotedblleft/quotedblright\n");
	tot_meta_chars += fprintf(fmeta,
		"8#030 1 8#054{StandardEncoding exch get}for /minus 8#056 1 8#217\n");
	tot_meta_chars += fprintf(fmeta,
		"{StandardEncoding exch get}for/dotlessi 8#301 1 8#317{StandardEncoding\n");
	tot_meta_chars += fprintf(fmeta,
		"exch get}for/space/exclamdown/cent/sterling/currency/yen/brokenbar/section\n");
	tot_meta_chars += fprintf(fmeta,
		"/dieresis/copyright/ordfeminine/guillemotleft/logicalnot/hyphen/registered\n");
	tot_meta_chars += fprintf(fmeta,
		"/macron/degree/plusminus/twosuperior/threesuperior/acute/mu/paragraph\n");
	tot_meta_chars += fprintf(fmeta,
		"/periodcentered/cedilla/onesuperior/ordmasculine/guillemotright/onequarter\n");
	tot_meta_chars += fprintf(fmeta,
		"/onehalf/threequarters/questiondown/Agrave/Aacute/Acircumflex/Atilde\n");
	tot_meta_chars += fprintf(fmeta,
		"/Adieresis/Aring/AE/Ccedilla/Egrave/Eacute/Ecircumflex/Edieresis/Igrave\n");
	tot_meta_chars += fprintf(fmeta,
		"/Iacute/Icircumflex/Idieresis/Eth/Ntilde/Ograve/Oacute/Ocircumflex/Otilde\n");
	tot_meta_chars += fprintf(fmeta,
		"/Odieresis/multiply/Oslash/Ugrave/Uacute/Ucircumflex/Udieresis/Yacute/Thorn\n");
	tot_meta_chars += fprintf(fmeta,
		"/germandbls/agrave/aacute/acircumflex/atilde/adieresis/aring/ae/ccedilla\n");
	tot_meta_chars += fprintf(fmeta,
		"/egrave/eacute/ecircumflex/edieresis/igrave/iacute/icircumflex/idieresis\n");
	tot_meta_chars += fprintf(fmeta,
		"/eth/ntilde/ograve/oacute/ocircumflex/otilde/odieresis/divide/oslash/ugrave\n");
	tot_meta_chars += fprintf(fmeta,
		"/uacute/ucircumflex/udieresis/yacute/thorn/ydieresis\n");
	tot_meta_chars += fprintf(fmeta, "256 array astore def cleartomark\n");
	tot_meta_chars += fprintf(fmeta, "\n");
	tot_meta_chars += fprintf(fmeta,
		"/encodefont{findfont dup maxlength dict begin{1 index/FID ne{def}{pop\n");
	tot_meta_chars += fprintf(fmeta,
		"pop}ifelse}forall/Encoding exch def dup/FontName exch def currentdict\n");
	tot_meta_chars += fprintf(fmeta, "definefont end}def\n");
	tot_meta_chars += fprintf(fmeta, "\n");
	tot_meta_chars += fprintf(fmeta, "/$/ISOLatin1Encoding load def\n");
	tot_meta_chars += fprintf(fmeta, "/&/encodefont load def\n");
	tot_meta_chars += fprintf(fmeta, "/F/findfont load def\n");
	tot_meta_chars += fprintf(fmeta, "/o/scalefont load def\n");
	tot_meta_chars += fprintf(fmeta, "/f/setfont load def\n");
/*
 * Define fonts.
 */
	tot_meta_chars += fprintf(fmeta,
		"/FM {/Courier-ISO $ /Courier & pop /Courier-ISO F 13.3 o f} bind def\n");
	tot_meta_chars += fprintf(fmeta,
		"/F01 {/Courier-Bold-ISO $ /Courier-Bold & pop /Courier-Bold-ISO F 26.6 o f} bind def\n");
	tot_meta_chars += fprintf(fmeta,
		"/FG {13.3 /Symbol findfont exch scalefont setfont} bind def\n");
	tot_meta_chars += fprintf(fmeta,
		"/FMV {/Courier-ISO $ /Courier & pop /Courier-ISO F 13.3 o f} bind def\n");
	tot_meta_chars += fprintf(fmeta,
		"/FGV {13.3 /Symbol findfont exch scalefont setfont} bind def\n");
	tot_meta_chars += fprintf(fmeta,"\n");
	tot_meta_chars += fprintf(fmeta,"/ourpointsize 0.5 def\n");
	tot_meta_chars += fprintf(fmeta,"\n");
	if (duiscc.indvcm > 2) {
		switch (*pr_default) {
			case 1:
				tot_meta_chars += fprintf(fmeta,"/colordisplay false def\n");
				tot_meta_chars += fprintf(fmeta,"/bwdisplay    true  def\n");
				tot_meta_chars += fprintf(fmeta,"/graydisplay  false def\n");
				break;
			case 2:
				tot_meta_chars += fprintf(fmeta,"/colordisplay false def\n");
				tot_meta_chars += fprintf(fmeta,"/bwdisplay    false def\n");
				tot_meta_chars += fprintf(fmeta,"/graydisplay  true  def\n");
				break;
			case 3: default:
				tot_meta_chars += fprintf(fmeta,"/colordisplay true  def\n");
				tot_meta_chars += fprintf(fmeta,"/bwdisplay    false def\n");
				tot_meta_chars += fprintf(fmeta,"/graydisplay  false def\n");
				break;
		}
	} else {
		tot_meta_chars += fprintf(fmeta,"/bwdisplay    true def\n");
	}

	prev_linewidth = -1;
	duiscc.fmetdu = True;
	meta_BW = -1;
	if (*kind == 4) {
		meta_COL = 0;
		meta_BW = 1;
		write_colors_to_meta();
	} else if (duiscc.indvcm == 2) {
/*
 * Black and white.
 */
		meta_BW_div = 0;
		meta_BW_higher = 1;
		meta_BW_lower = 0;
		tot_meta_chars +=
			fprintf(fmeta, "/ourlinewidth {0.0 mul setlinewidth} bind def\n");
	} else {
/*
 * Colour.
 */
		meta_BW_div = ddefco.ltabdd[6];
		meta_BW_higher = 0;
		meta_BW_lower = 1;
		tot_meta_chars +=
			fprintf(fmeta, "/ourlinewidth {0.0 mul setlinewidth} bind def\n");
	}
	number_of_meta_marks = 0;

	if (debugm) printf("\nOpened meta file %s", tmeta); fflush(stdout);
}

void dgrsme()
{
/*
 * Resume writing to meta file. Must be called before any drawing operations
 * are recorded.
 */
	int iprnt;

	iprnt = (debugm) ? 1 : 0;
	switch(meta_COL) {
		case 1:
			printf("\nPostScript metafile recording is already active.");
			break;
		case 0:
			meta_COL = 1;
			dgmkme(&iprnt);
			break;
		default:
			printf("\nYou must open a metafile first. No action taken.");
			break;
	}
	fflush(stdout);
}

void dgspme()
{
/*
 * Suspend writing to meta file. The file is left opened, but no drawing
 * operations are recorded.
 */
	switch(meta_COL) {
		case 1:
			meta_COL = 0;
			tot_meta_chars += fprintf(fmeta, "\n");
			break;
		case 0:
			printf("\nPostScript metafile recording is already suspended.");
			break;
		default:
			printf("\nNo metafile has been opened. No action taken.");
			break;
	}
	fflush(stdout);
}

void dgcsme()
{
/*
 * Close PostScript metafile.
 */
	char line[80];
	int l, l1;

	if (fmeta == NULL) {
		printf("\nThere is no open metafile. No action taken.\n");
		fflush(stdout);
		return;
	}
	tot_meta_chars += fprintf(fmeta, "\n");
	make_PS_postamble();
	sprintf(line, "%s closed (%d characters.)", tmeta, tot_meta_chars);
	printf("\n%s", line);
	if ((l=strlen(line)) < 52) {
#ifdef vms
		l1 = l+1;
#else
		l1 = l;
#endif /* VMS */
	} else {
		l1 = 1;
		printf("\n");
	}
	for (l=l1; l<52; l++)
		printf(" ");
	printf(":");
	meta_COL = -1;
	meta_BW  = -1;
	tot_meta_chars = 0;
	max_tot_meta_chars = 0;
	fflush(stdout);
}

void dgcome(
#ifdef vms
	FortranString *tcom
#else
	FortranString *tcom,
	int tcoml
#endif /* vms */
)
/*
 * This routine inserts a comment in the metafile.
 */
{
#ifdef vms
	unsigned char *cp;
	int tcoml;
#endif /* vms */
	int index;

	if (fmeta == NULL) return;

	for (index=0; index<256; index++) {
		m_line[index] = '\0';
	}
	tot_meta_chars += fprintf(fmeta, "\n");
	strcpy(m_line, "%%DALI-comment: ");
#ifdef vms
	tcoml = tcom->dsc$w_length;
	cp = tcom->dsc$a_pointer + tcoml - 1;
	for (index=0; index<tcom->dsc$w_length; index++) {
		if ((int)*cp > 32) break;
		tcoml = tcoml--; cp--;
	}
	strncat(m_line, (char *)tcom->dsc$a_pointer, tcoml);
#else
	strncat(m_line, (char *)tcom, tcoml);
#endif /* vms */
	mprintf(m_line, 1);
}

void dgfime()
{
/*
 * Called at normal program termination.
 */
	if (fmeta != NULL) dgcsme();
}

void mprintf(
	char line[],
	int new
)
{
/*
 * Output routine for PostScript metafile. If new == 1, start a new line.
 * Otherwise continue the present line within an 80 character record limit.
 */
#define DANGEROUS_META_SIZE 1000000
	static char beep[] = {0x07, 0x07, 0x07, 0x00};
	static int line_meta_chars = 0;
	char pr_buf[128];
	int l, cc;
	long lenter, ipos;

#ifdef ultrix
	fgetpos(fmeta, &lenter);
#else
	lenter = tot_meta_chars;
#endif /* vms */
	if (prev_linewidth != ddeco3.dlindd) {
		(void) sprintf(pr_buf, "%f ourlinewidth", ddeco3.dlindd * PSwidth_unit);
		tot_meta_chars += fprintf(fmeta, "\n%s", pr_buf);
		line_meta_chars = strlen(pr_buf);
		prev_linewidth = ddeco3.dlindd;
	}

	l = strlen(line);
	if (new == 1) {
		tot_meta_chars += fprintf(fmeta, "\n");
		line_meta_chars = 0;
	}
	if ((line_meta_chars + l > 79) && (line_meta_chars != 0)) {
		tot_meta_chars += fprintf(fmeta, "\n");
		line_meta_chars = 0;
	}
	if (line_meta_chars != 0) {
		cc = fprintf(fmeta, " ");
		tot_meta_chars += cc;
		line_meta_chars++;
	}
	cc = fprintf(fmeta, "%s", line);
#ifdef vms
	if (cc <= 0) {
#else
	if (cc < 0) {
#endif /* vms */
		meta_writing_errors++;
		if ((meta_writing_errors <= 5) && (meta_writing_errors > 0)) {
			printf("\n%sWARNING: There are errors writing metafile", beep);
			fflush(stdout);
		}
	}
	tot_meta_chars += cc;
	line_meta_chars += strlen(line);

/*
 * Check if the file is growing beyond bounds.
 */
#ifdef ultrix
	fgetpos(fmeta, &ipos);
#else
	ipos = tot_meta_chars;
#endif /* ultrix */
	if ((lenter < DANGEROUS_META_SIZE) && ipos >= DANGEROUS_META_SIZE) {
		printf("\n%sWARNING: Your metafile is growing beyond %d bytes.", beep,
			DANGEROUS_META_SIZE);
		printf("\nYou should seriously consider closing it."); fflush(stdout);
	}
}

void write_colors_to_meta()
/*
 * Define PostScript colour operators for meta file.
 */
{
	int index;
/*
 * Define switches colordisplay, bwdisplay and graydisplay. In the PostScript
 * file to be printed only one of those must be true and the others false.
 * We also define an operator ourlinewidth. This makes it possible to set
 * different linewidths for colour, graytone and B/W resp.
 */
	if (duiscc.indvcm > 2) {
		tot_meta_chars += fprintf(fmeta,
			"\n/colordisplay where { pop } { /colordisplay false def } ifelse\n");
		tot_meta_chars += fprintf(fmeta,
			"/bwdisplay where { pop } { /bwdisplay false def } ifelse\n");
		tot_meta_chars += fprintf(fmeta,
			"/graydisplay where { pop } { /graydisplay false def } ifelse\n");
	} else {
		tot_meta_chars += fprintf(fmeta,
			"/bwdisplay where { pop } { /bwdisplay true def } ifelse\n");
	}

	if (duiscc.indvcm > 2) {
/*
 * Define the RGB colours used.
 */
		tot_meta_chars += fprintf(fmeta,
			"colordisplay { %% Display in color.\n");
		tot_meta_chars +=
			fprintf(fmeta, "/ourlinewidth {0.5 mul setlinewidth} bind def\n");
		for (index = 0; index < ddefcn.numcdd; index++) {
			if (fabs(ddefco.rdcodd[index]-ddefco.grcodd[index]) +
			    fabs(ddefco.blcodd[index]-ddefco.grcodd[index]) > 0.01) {
				tot_meta_chars += fprintf(fmeta,
				"/setcolor%02d  {%6.4f %6.4f %6.4f setrgbcolor} bind def\n", index,
				ddefco.rdcodd[index], ddefco.grcodd[index], ddefco.blcodd[index]);
			} else {
				tot_meta_chars += fprintf(fmeta,
				"/setcolor%02d  {%6.4f setgray} bind def\n", index, ddefco.blcodd[index]);
			}
		}
		tot_meta_chars += fprintf(fmeta, "} if\n");
	}

/*
 * Define corresponding black&white choices.
 */
	tot_meta_chars += fprintf(fmeta, "bwdisplay { %% Display in black-and-white.\n");
	tot_meta_chars +=
		fprintf(fmeta, "/ourlinewidth {0.0 mul setlinewidth} bind def\n");
	for (index = 0; index <= dusdac.areadu; index++) {
		tot_meta_chars += fprintf(fmeta,
			"/setcolor%02d  {1.0000 setgray} bind def\n", index);
	}
	for (index = dusdac.areadu+1; index < ddefcn.numcdd; index++) {
		tot_meta_chars += fprintf(fmeta,
			"/setcolor%02d  {0.0000 setgray} bind def\n", index);
	}
	tot_meta_chars += fprintf(fmeta, "} if\n");

	if (duiscc.indvcm > 2) {
/*
 * Define colours as graylevels.
 */
		tot_meta_chars += fprintf(fmeta, "graydisplay { %% Display using graylevels.\n");
		tot_meta_chars +=
			fprintf(fmeta, "/ourlinewidth {0.0 mul setlinewidth} bind def\n");
		tot_meta_chars += fprintf(fmeta,
			"/setcolor00  {0.9500 setgray} bind def\n");
		for (index = 1; index <= dusdac.areadu; index++) {
			tot_meta_chars += fprintf(fmeta,
				"/setcolor%02d  {1.0000 setgray} bind def\n", index);
		}
		tot_meta_chars += fprintf(fmeta,
				"/setcolor06  {0.9500 setgray} bind def\n");
		for (index = dusdac.areadu+1; index < ddefcn.numcdd; index++) {
			tot_meta_chars += fprintf(fmeta,
				"/setcolor%02d  {0.0000 setgray} bind def\n", index);
		}
		tot_meta_chars += fprintf(fmeta, "} if\n");
	}
}

void mdgalgr
(
	int *iwind,
	int *lev,
	char *tlet,
	int *idir,
	float *h1,
	float *v1,
	float *h2,
	float *v2,
	FortranString *text,
	int *ascent
)
{
	unsigned char cout[2];
	int xfudge, yfudge;

#ifdef vms
	cout[0] = text->dsc$a_pointer[0];
#else
	cout[0] = text[0];
#endif /* VMS character */
	cout[1] = '\0';

	if (debugm) {
		sprintf(m_line,"%%dgalgr %s iaredo:%d", cout, dopr1c.iaredo);
		mprintf(m_line, 1);
	}
	if (cout[0] == ' ') return;
	if (meta_COL <= 0 && meta_BW == 1) return;
	if (dtvccc.fx11dt) {
		xfudge = dtxthm;
		yfudge = dtxtvm;
	} else {
/*
 * This is a temporary fix to place UIS characters correctly.
 */
		xfudge = -dtxthm;
		yfudge = 0;
	}
	if (*idir == 0) {
		sprintf(m_line,"FG (%s) %d %d %s", cout, (int) *h1 + xfudge,
			(int) *v1 + yfudge - *ascent, PS_moveto_show);
	} else {
		sprintf(m_line,"FGV (%s) %d %d %s", cout, (int) *h1 + xfudge + *ascent,
			(int) *v1 + yfudge, PS_moveto_show_vert);
	}
	mprintf(m_line, 1);
}

void text_to_PS(
	unsigned char cin[],
	int  n,
	unsigned char *cout
)
{
/*
 * Convert the n character string cin to a PostScript string, thereby
 * escaping \, ( and ).
 */
	int index;

	for (index = 0; index < n; index++) {
		if ((int) cin[index] > 127) {
/*
 * Convert to octal to please 7-bit transmission lines.
 */
			char tchar[5];
			sprintf(tchar, "%03o", (int) cin[index]);
			*cout = '\\';	cout++;
			*cout = tchar[0];	cout++;
			*cout = tchar[1];	cout++;
			*cout = tchar[2];	cout++;
			continue;
		}
		switch (cin[index]) {
			case '\\':
			case '(':
			case ')':
				*cout = '\\';	cout++;
				break;
			default:
				break;
		}
		*cout = cin[index];		cout++;
	}
	*cout = '\0';
}
void mdgtxtm(
	int *level,
	int *itsiz,
	FortranString *text,
	float *sangl,
	float *hret,
	float *vret,
	float *height,
	int *nitems,
	logical text_s[],
#ifdef VMS
	FortranString text_chars[],
#else
	FortranString *text_chars[],
#endif
	int text_nchars[]
)
{
	unsigned char tempc[101];
	int index;
/*
 * PS metafile.
 */
	if (debugm) {
		sprintf(m_line,"%%dgtxtf");
		mprintf(m_line, 1);
	}
	if (meta_COL <= 0 && meta_BW == 1) return;
	sprintf(m_line, "%d %d %s gsave", (int) *hret, (int) *vret, PS_moveto);
	mprintf(m_line, 1);
	mdglevl(level);
	if ((int) *sangl != 0) {
		sprintf(m_line, "%d rotate", (int) *sangl);
		mprintf(m_line, 0);
	}
	sprintf(m_line, "/FN {/Times-Roman-ISO $ /Times-Roman & pop /Times-Roman-ISO F %f o f } def",
		*height);
	mprintf(m_line, 1);
	sprintf(m_line, "/FS {%f /Symbol findfont exch scalefont setfont} def",
		*height);
	mprintf(m_line, 1);
/*
 * Then output the text.
 */
	for (index = 0; index < *nitems; index++) {
		if (text_s[index]) {
			sprintf(m_line, "FS");
		} else {
			sprintf(m_line, "FN");
		}
		if (index == 0) {
			mprintf(m_line, 1);
		} else {
			mprintf(m_line, 0);
		}
#ifdef vms
		text_to_PS(text_chars[index].dsc$a_pointer, text_nchars[index], tempc);
#else
		text_to_PS(text_chars[index], text_nchars[index], tempc);
#endif /* VMS character */
		sprintf(m_line, "(%s) show", tempc);
		mprintf(m_line, 0);
	}

	sprintf(m_line, "grestore");
	mprintf(m_line, 0);

	return;
}
