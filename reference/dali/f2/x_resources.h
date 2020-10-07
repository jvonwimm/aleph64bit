/*
 * DALI resources definitions.
 */
#define XtNdebug "debug"
#define XtCDebug "Debug"
#define XtNmainfont "mainfont"
#define XtCMainfont "Mainfont"
#define XtNhelpfont "helpfont"
#define XtCHelpfont "Helpfont"
#define XtNihelpfont "italhelpfont"
#define XtCiHelpfont "ItalHelpfont"
#define XtNgreekfont "greekfont"
#define XtCGreekfont "Greekfont"
#define XtNfont00 "font00"
#define XtCFont00 "Font00"
#define XtNfont01 "font01"
#define XtCFont01 "Font01"
#define XtNfontn1 "fontn1"
#define XtCFontn1 "Fontn1"
#define XtNfontn2 "fontn2"
#define XtCFontn2 "Fontn2"
#define XtNfontn3 "fontn3"
#define XtCFontn3 "Fontn3"
#define XtNfontn4 "fontn4"
#define XtCFontn4 "Fontn4"
#define XtNfontn5 "fontn5"
#define XtCFontn5 "Fontn5"
#define XtNfontn6 "fontn6"
#define XtCFontn6 "Fontn6"
#define XtNfonts1 "fonts1"
#define XtCFonts1 "Fonts1"
#define XtNfonts2 "fonts2"
#define XtCFonts2 "Fonts2"
#define XtNfonts3 "fonts3"
#define XtCFonts3 "Fonts3"
#define XtNfonts4 "fonts4"
#define XtCFonts4 "Fonts4"
#define XtNfonts5 "fonts5"
#define XtCFonts5 "Fonts5"
#define XtNfonts6 "fonts6"
#define XtCFonts6 "Fonts6"
#define XtNmetalinewidth "metalinewidth"
#define XtCMetalinewidth "Metalinewidth"
#define XtNdeph "displaydepth"
#define XtCdeph "Displaydepth"
#define XtNgray  "displayingray"
#define XtCgray  "Displayingray"

typedef struct {
	Pixel fg, bg;
	String debug;
	String mainfont;
	String helpfont;
	String italhelpfont;
	String greekfont;
	String font00;
	String font01;
	String fontn1;
	String fontn2;
	String fontn3;
	String fontn4;
	String fontn5;
	String fontn6;
	String fonts1;
	String fonts2;
	String fonts3;
	String fonts4;
	String fonts5;
	String fonts6;
	String metawidthunit;  /* This should be float, but DECW hasn't this type*/
	String displaydepth;
	String displayingray;
} ApplicationData, *ApplicationDataPtr;

static XtResource resources[] = {
	{XtNforeground, XtCForeground, XtRPixel, sizeof(Pixel),
		XtOffset(ApplicationDataPtr, fg), XtRString, (XtPointer)"Black"},
	{XtNbackground, XtCBackground, XtRPixel, sizeof(Pixel),
		XtOffset(ApplicationDataPtr, bg), XtRString, (XtPointer)"White"},
	{(String)XtNdebug, 	(String)XtCDebug, XtRString, sizeof(String),
		XtOffset(ApplicationDataPtr, debug), XtRString, (XtPointer)"="},
	{(String)XtNmainfont, 	(String)XtCMainfont, XtRString, sizeof(String),
		XtOffset(ApplicationDataPtr, mainfont), XtRString,
		(XtPointer)"-misc-fixed-medium-r-normal--13-120-75-75-c-70-iso8859-1"},
/*		(XtPointer)"-DEC-Terminal-Medium-R-Normal--14-140-75-75-C-80-ISO8859-1"}, */
/*		(XtPointer)"-Adobe-Courier-Medium-R-Normal--12-120-75-75-M-70-ISO8859-1"}, */
	{(String)XtNhelpfont, 	(String)XtCHelpfont, XtRString, sizeof(String),
		XtOffset(ApplicationDataPtr, helpfont), XtRString,
		(XtPointer)"-misc-fixed-medium-r-normal--13-120-75-75-c-70-iso8859-1"},
/*		(XtPointer)"-DEC-Terminal-Medium-R-Normal--14-140-75-75-C-80-ISO8859-1"}, */
/*		(XtPointer)"-Adobe-Courier-Medium-R-Normal--12-120-75-75-M-70-ISO8859-1"}, */
	{(String)XtNihelpfont, 	(String)XtCiHelpfont, XtRString, sizeof(String),
		XtOffset(ApplicationDataPtr, italhelpfont), XtRString,
		(XtPointer)"-Adobe-Courier-Medium-O-Normal--12-120-75-75-M-70-ISO8859-1"},
	{(String)XtNgreekfont, 	(String)XtCGreekfont, XtRString, sizeof(String),
		XtOffset(ApplicationDataPtr, greekfont), XtRString,
		(XtPointer)"-Adobe-Symbol-Medium-R-Normal--14-1*0-*-*-P-85-ADOBE-FONTSPECIFIC"},
	{(String)XtNfont00, 	(String)XtCFont00, XtRString, sizeof(String),
		XtOffset(ApplicationDataPtr, font00), XtRString,
		(XtPointer)"-Adobe-Courier-Medium-R-Normal--24-*-75-75-M-150-ISO8859-1"},
/*		(XtPointer)"-DEC-Terminal-Medium-R-Normal--28-280-75-75-C-160-ISO8859-1"}, */
	{(String)XtNfont01, 	(String)XtCFont01, XtRString, sizeof(String),
		XtOffset(ApplicationDataPtr, font01), XtRString,
		(XtPointer)"-Adobe-Courier-Bold-R-Normal--24-240-75-75-M-150-ISO8859-1"},
/*		(XtPointer)"-DEC-Terminal-Bold-R-Normal--28-280-75-75-C-160-ISO8859-1"}, */
	{(String)XtNfontn1, 	(String)XtCFontn1, XtRString, sizeof(String),
		XtOffset(ApplicationDataPtr, fontn1), XtRString,
		(XtPointer)"-*-Times-Medium-R-Normal--10-1*-*-*-P-*-ISO8859-1"},
	{(String)XtNfontn2, 	(String)XtCFontn2, XtRString, sizeof(String),
		XtOffset(ApplicationDataPtr, fontn2), XtRString,
		(XtPointer)"-*-Times-Medium-R-Normal--12-1*-*-*-P-*-ISO8859-1"},
	{(String)XtNfontn3, 	(String)XtCFontn3, XtRString, sizeof(String),
		XtOffset(ApplicationDataPtr, fontn3), XtRString,
		(XtPointer)"-*-Times-Medium-R-Normal--14-1*-*-*-P-*-ISO8859-1"},
	{(String)XtNfontn4, 	(String)XtCFontn4, XtRString, sizeof(String),
		XtOffset(ApplicationDataPtr, fontn4), XtRString,
		(XtPointer)"-*-Times-Medium-R-Normal--18-1*-*-*-P-*-ISO8859-1"},
	{(String)XtNfontn5, 	(String)XtCFontn5, XtRString, sizeof(String),
		XtOffset(ApplicationDataPtr, fontn5), XtRString,
		(XtPointer)"-*-Times-Medium-R-Normal--24-2*-*-*-P-*-ISO8859-1"},
	{(String)XtNfontn6, 	(String)XtCFontn6, XtRString, sizeof(String),
		XtOffset(ApplicationDataPtr, fontn6), XtRString,
		(XtPointer)"-*-Times-Medium-R-Normal--34-*-*-*-P-*-ISO8859-1"},
	{(String)XtNfonts1, 	(String)XtCFonts1, XtRString, sizeof(String),
		XtOffset(ApplicationDataPtr, fonts1), XtRString,
		(XtPointer)"-*-Symbol-Medium-R-Normal--10-1*-*-*-P-*-ADOBE-FONTSPECIFIC"},
	{(String)XtNfonts2, 	(String)XtCFonts2, XtRString, sizeof(String),
		XtOffset(ApplicationDataPtr, fonts2), XtRString,
		(XtPointer)"-*-Symbol-Medium-R-Normal--12-1*-*-*-P-*-ADOBE-FONTSPECIFIC"},
	{(String)XtNfonts3, 	(String)XtCFonts3, XtRString, sizeof(String),
		XtOffset(ApplicationDataPtr, fonts3), XtRString,
		(XtPointer)"-*-Symbol-Medium-R-Normal--14-1*-*-*-P-*-ADOBE-FONTSPECIFIC"},
	{(String)XtNfonts4, 	(String)XtCFonts4, XtRString, sizeof(String),
		XtOffset(ApplicationDataPtr, fonts4), XtRString,
		(XtPointer)"-*-Symbol-Medium-R-Normal--18-1*-*-*-P-*-ADOBE-FONTSPECIFIC"},
	{(String)XtNfonts5, 	(String)XtCFonts5, XtRString, sizeof(String),
		XtOffset(ApplicationDataPtr, fonts5), XtRString,
		(XtPointer)"-*-Symbol-Medium-R-Normal--24-2*-*-*-P-*-ADOBE-FONTSPECIFIC"},
	{(String)XtNfonts6, 	(String)XtCFonts6, XtRString, sizeof(String),
		XtOffset(ApplicationDataPtr, fonts6), XtRString,
		(XtPointer)"-*-Symbol-Medium-R-Normal--34-*-*-*-P-*-ADOBE-FONTSPECIFIC"},
	{(String)XtNmetalinewidth,	(String)XtCMetalinewidth,	XtRString,	sizeof(String),
		XtOffset(ApplicationDataPtr, metawidthunit),	XtRString,
	        (XtPointer)"1.0"},
	{(String)XtNdeph,	(String)XtCdeph,	XtRString,	sizeof(String),
		XtOffset(ApplicationDataPtr, displaydepth),	XtRString,
	        (XtPointer)"-1"},
	{(String)XtNgray,	(String)XtCgray,	XtRString,	sizeof(String),
		XtOffset(ApplicationDataPtr, displayingray),	XtRString,
	        (XtPointer)"-1"}
};

	ApplicationData defdata;
