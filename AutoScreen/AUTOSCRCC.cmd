/* PROJECT     : AB UTILITIES (LIC:XXXXXX)                                  */
/* PROGRAMMER  : ASHISH BAGADDEO                                            */
/* DATE        : 01/01/2014                                                 */
/* DESCRIPTION : COMMAND FILE                                               */
/* � COPYRIGHT : ICORE�            MAILTO: SUPPORT@ICORE.CO.IN              */
/*--------------------------------------------------------------------------*/
             CMD        PROMPT('AUTO SCREEN GENERATOR')
             PARM       KWD(PGMPRFX) TYPE(*CHAR) LEN(8) MIN(1) +
                          ALWUNPRT(*NO) PROMPT('Program Prefix')
             PARM       KWD(LIBL) TYPE(*CHAR) LEN(10) PROMPT('LIBRARY')
