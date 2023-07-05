     �* Project     : AB Utilities                                         *
     �* Programmer  : Ashish Bagaddeo                                      *
     �* Date        : 00/00/0000                                           *
     �* Description : Auto Generate Screens                                *
     �*                                                                    *
     �* � Copyright  : iCORE�            mailto: support@icore.co.in       *
     �**********************************************************************
     H Debug Option(*NoDebugIO)
     H CopyRight('iCORE� - Ashish Bagaddeo')

     �* File Specification
     FAUTOSCRD  CF   E             WORKSTN
     FAUTOSCRAF1IF   E           K DISK    Rename(AUTOSCRAR:F1)
     FAUTOSCRAF2IF   E           K DISK    Rename(AUTOSCRAR:F2)
     FAUTOSCRAF3IF   E           K DISK    Rename(AUTOSCRAR:F3)
     FAUTOSCRAF4IF   E           K DISK    Rename(AUTOSCRAR:F4)

     �* Variable Declaration

     DVarDs1           DS
     D ChkKey                        10    Inz(*Blanks)
     D wCommand                     300    Inz(*Blanks)
     D wAEScr                        10    Inz(*Blanks)
     D wDecLen                        2    Inz(*Blanks)
     D wRecName                      10    Inz(*Blanks)
     D wFldTyp                        1    Inz(*Blanks)
     D wLineData                     80    Inz(*Blanks)
     D wCurText                      16    Inz(*Blanks)
     D wRepText                      16    Inz(*Blanks)
     D wCurTextL                     60    Inz(*Blanks)
     D wRepTextL                     60    Inz(*Blanks)
     D wToFile                       10    Inz(*Blanks)
     D wkHUPos                        2    Inz(*Blanks)
     D wkVUPos                        2    Inz(*Blanks)
     D wkUPSAnd                       3    Inz(*Blanks)
     D wCurFldLenC                    2    Inz(*Blanks)
     D wCmdLen                       15P 5 Inz(*Zeros)
     D SFLRRN                         4  0 Inz(*Zeros)
     D wCurFldCnt                     3  0 Inz(*Zeros)
     D wCurFldLen                     2  0 Inz(*Zeros)
     D wTotSubCnt                     2  0 Inz(*Zeros)
     D wIntTest                      21  8 Inz(*Zeros)
     D wLineSeq                       6  2 Inz(*Zeros)
     D wLineDate                      6  0 Inz(*Zeros)
     D wFstSnd                        1  0 Inz(*Zeros)
     D wVPosAdd                       2  0 Inz(*Zeros)
     D wScrRecNo                      2  0 Inz(*Zeros)
     D wScrCnt                        2  0 Inz(*Zeros)
     D wCurLineVPos                   2S 0 Inz(*Zeros)
     D wCurLineVPosL                  2S 0 Inz(*Zeros)
     D wCurLineVPosR                  2S 0 Inz(*Zeros)
     D wFldTxtLen                     5P 0 Inz(*Zeros)
     D wTrimOpt                       1    Inz(*Blanks)
     D wkExLen                        2S 0 Inz(*Zeros)
     D wkPOSUPSCnt                    2S 0 Inz(*Zeros)
     D i                              2S 0 Inz(*Zeros)

     �* Arrays

     �* Indicators
     D wFileFnd                        N   Inz(*Off)
     D wCorrect                        N   Inz(*Off)
     D wNextRec                        N   Inz(*Off)
     D wNextChk                        N   Inz(*Off)
     D wGt80                           N   Inz(*Off)
     D wTrimFlg                        N   Inz(*Off)
     D wPosDone                        N   Inz(*Off)
     D WCmdOk                          N   Inz(*Off)

     D InKyPtr         S               *   Inz(%ADDR(*IN))
     DSubFileInd       DS            99    Based(InKyPtr)
     D SflDsp                          N   Overlay(SubFileInd: 30)
     D SflClr                          N   Overlay(SubFileInd: 31)
     D SflEnd                          N   Overlay(SubFileInd: 32)

     �* Constants
     DUCase            C                   CONST('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
     DLCase            C                   CONST('abcdefghijklmnopqrstuvwxyz')


     �* Data Structures
     D wkUPSDS         DS                  DIM(9) QUALIFIED
     D SUBSCR                        80
     D SUBSQL                        80
     D FLDLENC                        2

     D*ApiErrorDs      DS                  INZ QUALIFIED
     D* BYTESPROV                    10I 0 INZ(%SIZE(APIERRORDS.MSGDATA))
     D* BYTESAVAL                    10I 0
     D* MSGID                         7A
     D* FILLER                        1A
     D* MSGDATA                     256A


     �* Field Option DS Array
     DFldOptDS         DS                  DIM(500) Qualified
     DFLDNAM                         10
     DFLDTYP                          1
     DFLDLEN                          5
     DFLDDEC                          2
     DFLDSCR                          1
     DFLDAET                         15
     DFLDKEY                          1
     DFLDDFT                         16
     DFLDSUB                          1
     DFLDSFT                         16
     DFLDUPS                          1
     DFLDVAL                          1
     DFLDMIN                         16
     DFLDMAX                         16
     DFLDVA1                         16
     DFLDVA2                         16
     DFLDVA3                         16
     DFLDVA4                         16
     DFLDREC                          2  0

     DScrDS            DS
     DFLDNAM                         10
     DFLDSCR                          1
     DFLDAET                         15
     DFLDKEY                          1
     DFLDDFT                         16
     DFLDSUB                          1
     DFLDSFT                         16
     DFLDUPS                          1
     DFLDVAL                          1
     DFLDMIN                         16
     DFLDMAX                         16
     DFLDVA1                         16
     DFLDVA2                         16
     DFLDVA3                         16
     DFLDVA4                         16

     �* Subfile Data Structure (Fields)
     DScreenDS         DS
     DFILENAM                        10
     DLIBRARY                        10
     DSPROJNM                        16
     DSAUTH                          16
     DSDATE                          10
     DSDESC                          48
     DSSCRN                          50

     �* Cursor Data Structure (Fields)
     DSrcFileFDS       DS
     DSFldNm                         10
     DSFldLn                          9  0
     DSFldDc                          9  0
     DSFldTy                          1
     DSFldTx                         50
     DSFldTo                          9  0

     �* Prototypes
      /Copy RPGUTILS,CPYBK

     D AUTOSCRP        PR                  EXTPGM('AUTOSCRP')
     D inPgmPrfx                      8
     D FndFlg                          N

     D AUTOSCRP        PI
     D inPgmPrfx                      8
     D FndFlg                          N

      /Free

         //�----------------------------------------------------------
         //�                      Program Starts                     -
         //�----------------------------------------------------------

         //�Set Options
            Exec SQL
                 SET OPTION COMMIT = *NONE;

         //�Show File Screen
            DoU *IN03 or *IN12;

              *In22  = *Off;

           //�Set Current Date
              SDATE = %CHAR(%DATE);
              Write HEADER;
              Write FOOTER;
              ExFmt GETFIL;

              ErrMsg = *Blanks;
              *In18 = *Off;

              If *IN03 or *IN05;
                ExSr $ExitPgm;
              EndIf;

              If FILENAM = *Blanks Or LIBRARY = *Blanks Or inPgmPrfx = *Blanks;
                ErrMsg = 'FileName/Library/Pgm Prefix Not Entered';
                Iter;
              EndIf;

              ExSr $ChkFile;

              If wFileFnd   = *On;
                FILENAM = %XLate(LCase:UCase:FILENAM);
                LIBRARY = %XLate(LCase:UCase:LIBRARY);
             //�Get Field Options for each field
                ExSr $GetFldOptions;

             //�Build ADD/EDIT Screen
                ExSr $BuildAddEditScr;

             //�Build ADD/EDIT Program
                ExSr $BuildAddEditPgm;

             //�Build Subfile Screen
                ExSr $BuildSubFileScr;

             //�Build Display Screen
                ExSr $BuildSubFilePgm;

             //�Compile Screen & Program
                //ExSr $Compile;

                If ERRMSG = *Blanks;

               //�ExitProgram
                  ExSr $ExitPgm;

                  ErrMsg = 'Screen build successful for file ' + FILENAM;
                  Clear ScreenDS;
                EndIf;


              Else;
                ErrMsg = 'File/Library Not Found';
              EndIf;

            EndDo;

         //�Exit Program
            ExSr $ExitPgm;


         //�----------------------------------------------------------
         //�                        Subroutines                      -
         //�----------------------------------------------------------

         //�----------------------------------------
         // $BuildAddEditScr - Build Add/Edit Screen
         //�----------------------------------------
            BegSr $BuildAddEditScr;

           //�Initialize
              wLineSeq  = *Zeros;
              wLineDate = *Zeros;
              wToFile   = 'ADDEDITSCR';

           //�Build Screen

           //�Description
              wAEScr = '100DESC';

              SetLL wAEScr AUTOSCRAF1;
              ReadE(E) wAEScr AUTOSCRAF1;
              DoW Not %EOF(AUTOSCRAF1);
                wLineData = *Blanks;
                wLineData = AUTOLINE;
                ExSr $UpdateDfts;
                ExSr $AddLineToFile;

                ReadE(E) wAEScr AUTOSCRAF1;

              EndDo;

           //�Header
              wAEScr = '101HEAD';
              SetLL wAEScr AUTOSCRAF1;
              ReadE(E) wAEScr AUTOSCRAF1;
              DoW Not %EOF(AUTOSCRAF1);
                wLineData = *Blanks;
                wLineData = AUTOLINE;
                ExSr $UpdateDfts;
                ExSr $AddLineToFile;

                ReadE(E) wAEScr AUTOSCRAF1;
              EndDo;

           //�Data
              wFstSnd       = 1;
              wScrRecNo     = 0;
              wCurFldCnt    = 1;
              wCurLineVPosL = 5;
              wCurLineVPosR = 5;
              wNextChk      = *On;

              DoW FldOptDS(wCurFldCnt ).FLDNAM <> *Blanks;

              wGt80 = *Off;

              //If %REM(wCurFldCnt :10) = 0 Or wCurFldCnt  = 1;
              If wCurFldCnt  = 1 Or wNextRec = *On;

                  wScrRecNo += 1;
               //�Data Header
                  wAEScr = '102RECH';
                  SetLL wAEScr AUTOSCRAF1;
                  ReadE(E) wAEScr AUTOSCRAF1;
                  DoW Not %EOF(AUTOSCRAF1);
                    wLineData = *Blanks;
                    wLineData = AUTOLINE;

                 //�Set Record Format Name
                    If wScrRecNo > 9;
                      wRecName = 'DSPREC' + %Trim(%Char(wScrRecNo));
                    Else;
                      wRecName = 'DSPREC' + '0' + %Trim(%Char(wScrRecNo));
                    EndIf;

                    ExSr $UpdateDfts;
                    ExSr $AddLineToFile;

                    ReadE(E) wAEScr AUTOSCRAF1;

                  EndDo;

                  wFstSnd       = 1;
                  wNextRec      = *Off;
                  wCurLineVPosL = 5;
                  wCurLineVPosR = 5;

                EndIf;

                If FldOptDS(wCurFldCnt ).FLDTYP = 'Z';

                  wFldTyp = 'Z';
                  wAEScr  = '103DATAA';

                ElseIf FldOptDS(wCurFldCnt ).FLDTYP = 'L';

                  wFldTyp = 'L';
                  wAEScr  = '103DATAA';

                ElseIF FldOptDS(wCurFldCnt ).FLDTYP = 'S' Or
                       FldOptDS(wCurFldCnt ).FLDTYP = 'P' Or
                       FldOptDS(wCurFldCnt ).FLDTYP = 'F';

                  If %Int(FldOptDS(wCurFldCnt ).FLDDEC) > 9;
                    wDecLen = FldOptDS(wCurFldCnt ).FLDDEC;
                  Else;
                    wDecLen = '0' + %Trim(FldOptDS(wCurFldCnt ).FLDDEC);
                  EndIf;

                  wFldTyp = 'Y';
                  wAEScr  = '103DATAA';

                Else;

                  wFldTyp = 'A';
                  wDecLen = *Blanks;
                  wAEScr  = '103DATAA';

                EndIf;

                SetLL wAEScr AUTOSCRAF1;
                ReadE(E) wAEScr AUTOSCRAF1;
                DoW Not %EOF(AUTOSCRAF1);

                  wLineData = AUTOLINE;

               //�Move to next record if insufficient lines
                  If %Int(FldOptDS(wCurFldCnt ).FLDLEN) > 20
                     And wNextChk = *On;
                     wNextChk = *Off;
                    If %Int(FldOptDS(wCurFldCnt ).FLDLEN) / 80 >
                       20 - wCurLineVPosL;
                      wNextRec = *On;
                      Leave;
                    ElseIf ((%Int(FldOptDS(wCurFldCnt ).FLDLEN) / 20 >
                           20 - wCurLineVPosL) And wFstSnd = 1 ) Or
                           ((%Int(FldOptDS(wCurFldCnt ).FLDLEN) / 20 >
                           20 - wCurLineVPosR) And wFstSnd = 2 );
                      wNextRec = *On;
                      Leave;
                    EndIf;

                  EndIf;

                  If AUTOLSEQ = 1;

                 //�Handle if Field > 20 chars
                    If %Int(FldOptDS(wCurFldCnt ).FLDLEN) > 20
                      Or (%Int(FldOptDS(wCurFldCnt ).FLDLEN) > 18
                          And wFldTyp = 'Y');
                      If %Int(FldOptDS(wCurFldCnt ).FLDLEN) > 80
                         Or wFldTyp = 'Y' Or wFldTyp = 'Z';
                        wGt80 = *On;
                        If wFstSnd = 2;
                          wFstSnd = 1;
                          wCurLineVPosL += 1;
                        EndIf;
                      EndIf;
                    EndIf;

                 //�Set Text Data
                    wFldTxtLen =%Len(%Trim(FldOptDS(wCurFldCnt ).FLDAET));
                    %SubSt(wLineData:46:wFldTxtLen) =
                                       %Trim(FldOptDS(wCurFldCnt ).FLDAET);

                 //�Set Vertical Position
                    If wFstSnd = 1;
                      If wCurLineVPosL > 9;
                        %SubSt(wLineData:40:2) = %Char(wCurLineVPosL);
                      Else;
                        %SubSt(wLineData:41:1) = %Char(wCurLineVPosL);
                      EndIf;
                    Else;
                      If wCurLineVPosR > 9;
                        %SubSt(wLineData:40:2) = %Char(wCurLineVPosR);
                      Else;
                        %SubSt(wLineData:41:1) = %Char(wCurLineVPosR);
                      EndIf;
                    EndIf;

                 //�Set Horizontal Position
                    If wFstSnd = 1;
                      %SubSt(wLineData:43:2) = ' 1';
                    Else;
                      %SubSt(wLineData:43:2) = '41';
                    EndIf;

                  ElseIf AUTOLSEQ = 2;

                 //�Set Field Name
                    %SubSt(wLineData:19:10) = FldOptDS(wCurFldCnt ).FLDNAM;

                 //�Set Field Type (Text or Numeric)
                 // %SubSt(wLineData:35:1) = wFldTyp;

                 //�Set Output Type (B or O)
                    If FldOptDS(wCurFldCnt ).FLDSCR = 'Y';
                      %SubSt(wLineData:38:1) = 'B';
                    Else;
                      %SubSt(wLineData:38:1) = 'O';
                    EndIf;

                    If %Int(FldOptDS(wCurFldCnt ).FLDLEN) > 20 Or
                      (%Int(FldOptDS(wCurFldCnt ).FLDLEN) > 18 And
                       wFldTyp = 'Y');
                      If %Int(FldOptDS(wCurFldCnt ).FLDLEN) > 80
                         Or wFldTyp = 'Y' Or wFldTyp = 'Z';
                        If wFstSnd = 2;
                          wFstSnd = 1;
                          wCurLineVPosL += 1;
                        EndIf;

                        If wFldTyp <> 'Y' And wFldTyp <> 'Z' And wFldTyp <> 'L';
                          %SubSt(wLineData:45:10) = 'CNTFLD(60)';
                          wVPosAdd =
                          (%Int(FldOptDS(wCurFldCnt ).FLDLEN) / 60) + 1;
                        EndIf;

                      Else;

                        %SubSt(wLineData:45:10) = 'CNTFLD(20)';
                        wVPosAdd =
                        (%Int(FldOptDS(wCurFldCnt ).FLDLEN) / 20) + 1;

                      EndIf;

                    Else;
                      wVPosAdd = 1;
                    EndIf;

                 //�Set Vertical
                    If wFstSnd = 1;
                      If wCurLineVPosL > 9;
                        %SubSt(wLineData:40:2) = %Char(wCurLineVPosL);
                      Else;
                        %SubSt(wLineData:41:1) = %Char(wCurLineVPosL);
                      EndIf;
                    Else;
                      If wCurLineVPosR > 9;
                        %SubSt(wLineData:40:2) = %Char(wCurLineVPosR);
                      Else;
                        %SubSt(wLineData:41:1) = %Char(wCurLineVPosR);
                      EndIf;
                    EndIf;

                 //�Set Horizontal
                    If wFstSnd = 1;
                      %SubSt(wLineData:43:2) = %Char(19);
                    Else;
                      %SubSt(wLineData:43:2) = %Char(59);
                    EndIf;

                  EndIf;

                  ExSr $AddLineToFile;
                  FldOptDS(wCurFldCnt ).FLDREC = wScrRecNo;

                  ReadE(E) wAEScr AUTOSCRAF1;

                EndDo;

                If wNextRec = *On;
                  Iter;
                EndIf;

                If wFstSnd = 1;
                  wCurLineVPosL += wVPosAdd;
                Else;
                  wCurLineVPosR += wVPosAdd;
                EndIf;

                If wGt80 = *On;
                  wCurLineVPosL +=1;
                  wCurLineVPosR = wCurLineVPosL;
                EndIf;
                If wCurLineVPosR > 20 And wCurLineVPosL > 20;
                  wNextRec = *On;
                EndIf;

                If wCurLineVPosR >= wCurLineVPosL;
                  wFstSnd = 1;
                Else;
                  wFstSnd = 2;
                EndIf;

             //�Add underline and Protected Fields
                If FldOptDS(wCurFldCnt ).FLDSCR = 'Y';

                  wAEScr = '106PROC';
                  Chain(E) (wAEScr:1) AUTOSCRAF1;
                  If %Found(AUTOSCRAF1);
                    wLineData = AUTOLINE;
                  EndIf;
                  ExSr $AddLineToFile;

                  wAEScr = '105UNDL';
                  Chain(E) (wAEScr) AUTOSCRAF1;
                  If %Found(AUTOSCRAF1);
                    wLineData = AUTOLINE;
                  EndIf;
                  ExSr $AddLineToFile;

                Else;

                  wAEScr = '106PROC';
                  Chain(E) (wAEScr:2) AUTOSCRAF1;
                  If %Found(AUTOSCRAF1);
                    wLineData = AUTOLINE;
                  EndIf;
                  ExSr $AddLineToFile;

                EndIf;

                wCurFldCnt  += 1;
                wNextChk = *On;
              EndDo;

           //�Footer
              wAEScr  = '107FOOT';
              SetLL wAEScr AUTOSCRAF1;
              ReadE(E) wAEScr AUTOSCRAF1;
              DoW Not %EOF(AUTOSCRAF1);
                wLineData = *Blanks;
                wLineData = AUTOLINE;
                ExSr $AddLineToFile;

                ReadE(E) wAEScr AUTOSCRAF1;

              EndDo;


            EndSr;

         //�------------------------------------------
         // $BuildAddEditPgm - Build Add/Edit Program
         //�------------------------------------------
            BegSr $BuildAddEditPgm;

           //�Initialize
              wLineSeq  = *Zeros;
              wLineDate = *Zeros;
              wToFile   = 'ADDEDITPGM';

           //�Build ADD/EDIT Program

           //�Description & Declarations
              wAEScr = '100DECL';

              SetLL wAEScr AUTOSCRAF2;
              ReadE(E) wAEScr AUTOSCRAF2;
              DoW Not %EOF(AUTOSCRAF2);
                wLineData = *Blanks;
                wLineData = AUTOLINE;
                ExSr $UpdateDfts;
                ExSr $AddLineToFile;
                ReadE(E) wAEScr AUTOSCRAF2;

              EndDo;

           //�Main Line Start
              wAEScr = '100MAIN';
              SetLL wAEScr AUTOSCRAF2;
              ReadE(E) wAEScr AUTOSCRAF2;
              DoW Not %EOF(AUTOSCRAF2);
                wLineData = *Blanks;
                wLineData = AUTOLINE;
                ExSr $AddLineToFile;

                ReadE(E) wAEScr AUTOSCRAF2;

              EndDo;

           //�Total Screen Numbers
              wAEScr = '100TSCR';
              SetLL wAEScr AUTOSCRAF2;
              ReadE(E) wAEScr AUTOSCRAF2;
              DoW Not %EOF(AUTOSCRAF2);
                wLineData = *Blanks;
                wLineData = AUTOLINE;
                ExSr $UpdateDfts;
                ExSr $AddLineToFile;

                ReadE(E) wAEScr AUTOSCRAF2;

              EndDo;

           //�Main Line End
              wAEScr = '101MAIN';
              SetLL wAEScr AUTOSCRAF2;
              ReadE(E) wAEScr AUTOSCRAF2;
              DoW Not %EOF(AUTOSCRAF2);
                wLineData = *Blanks;
                wLineData = AUTOLINE;
                ExSr $AddLineToFile;

                ReadE(E) wAEScr AUTOSCRAF2;

              EndDo;

           //�Subroutine Start
              wAEScr = '102SUBR';
              SetLL wAEScr AUTOSCRAF2;
              ReadE(E) wAEScr AUTOSCRAF2;
              DoW Not %EOF(AUTOSCRAF2);
                wLineData = *Blanks;
                wLineData = AUTOLINE;
                ExSr $AddLineToFile;

                ReadE(E) wAEScr AUTOSCRAF2;

              EndDo;

           //�Show Screen Code
              wAEScr = '103SCR';
              SetLL (wAEScr:1) AUTOSCRAF2;
              ReadE(E) (wAEScr:1) AUTOSCRAF2;
              DoW Not %EOF(AUTOSCRAF2);
                wLineData = *Blanks;
                wLineData = AUTOLINE;
                ExSr $AddLineToFile;

                ReadE(E) (wAEScr:1) AUTOSCRAF2;

              EndDo;

              wAEScr = '103SCR';
              wScrCnt = 1;
              DoW wScrCnt <= wScrRecNo;
                SetLL (wAEScr:2) AUTOSCRAF2;
                ReadE(E) (wAEScr:2) AUTOSCRAF2;
                DoW Not %EOF(AUTOSCRAF2);
                  wLineData = *Blanks;
                  wLineData = AUTOLINE;

                  ExSr $UpdateDfts;
                  ExSr $AddLineToFile;

                  ReadE(E) (wAEScr:2) AUTOSCRAF2;

                EndDo;

                wScrCnt += 1;

              EndDo;

              wAEScr = '103SCR';
              SetLL (wAEScr:3) AUTOSCRAF2;
              ReadE(E) (wAEScr:3) AUTOSCRAF2;
              DoW Not %EOF(AUTOSCRAF2);
                wLineData = *Blanks;
                wLineData = AUTOLINE;
                ExSr $AddLineToFile;

                ReadE(E) (wAEScr:3) AUTOSCRAF2;

              EndDo;

           //�Validate Screen Code
              wAEScr = '104VAL';
              SetLL (wAEScr:1) AUTOSCRAF2;
              ReadE(E) (wAEScr:1) AUTOSCRAF2;
              DoW Not %EOF(AUTOSCRAF2);
                wLineData = *Blanks;
                wLineData = AUTOLINE;
                ExSr $AddLineToFile;

                ReadE(E) (wAEScr:1) AUTOSCRAF2;

              EndDo;

              wAEScr = '104VAL';
              wScrCnt = 1;
              DoW wScrCnt <= wScrRecNo;
                SetLL (wAEScr:2) AUTOSCRAF2;
                ReadE(E) (wAEScr:2) AUTOSCRAF2;
                DoW Not %EOF(AUTOSCRAF2);
                  wLineData = *Blanks;
                  wLineData = AUTOLINE;
                  ExSr $UpdateDfts;
                  ExSr $AddLineToFile;

                  ReadE(E) (wAEScr:2) AUTOSCRAF2;

                EndDo;

                wScrCnt += 1;

              EndDo;

              wAEScr = '104VAL';
              SetLL (wAEScr:3) AUTOSCRAF2;
              ReadE(E) (wAEScr:3) AUTOSCRAF2;
              DoW Not %EOF(AUTOSCRAF2);
                wLineData = *Blanks;
                wLineData = AUTOLINE;
                ExSr $AddLineToFile;

                ReadE(E) (wAEScr:3) AUTOSCRAF2;

              EndDo;

           //�Fetch Record
              wAEScr = '105FTH';
              SetLL wAEScr AUTOSCRAF2;
              ReadE(E) wAEScr AUTOSCRAF2;
              DoW Not %EOF(AUTOSCRAF2);
                wLineData = *Blanks;
                wLineData = AUTOLINE;
                ExSr $UpdateDfts;
                ExSr $AddLineToFile;

                ReadE(E) wAEScr AUTOSCRAF2;

              EndDo;

           //�Validate Screen Subroutines
              wAEScr = '106VALCD';
              wScrCnt = 1;
              DoW wScrCnt <= wScrRecNo;
                SetLL (wAEScr) AUTOSCRAF2;
                ReadE(E) (wAEScr) AUTOSCRAF2;
                DoW Not %EOF(AUTOSCRAF2);
                  wLineData = *Blanks;
                  wLineData = AUTOLINE;
                  ExSr $UpdateDfts;
                  ExSr $AddLineToFile;

                  ReadE(E) wAEScr AUTOSCRAF2;

                EndDo;

                wScrCnt += 1;

              EndDo;

           //�Add Subroutine
              wAEScr = '106ADDCD';
              SetLL wAEScr AUTOSCRAF2;
              ReadE(E) wAEScr AUTOSCRAF2;
              DoW Not %EOF(AUTOSCRAF2);
                wLineData = *Blanks;
                wLineData = AUTOLINE;
                ExSr $UpdateDfts;
                ExSr $AddLineToFile;

                ReadE(E) wAEScr AUTOSCRAF2;

              EndDo;

           //�Update Subroutine
              wAEScr = '106UPDCD';
              SetLL wAEScr AUTOSCRAF2;
              ReadE(E) wAEScr AUTOSCRAF2;
              DoW Not %EOF(AUTOSCRAF2);
                wLineData = *Blanks;
                wLineData = AUTOLINE;
                ExSr $UpdateDfts;
                ExSr $AddLineToFile;

                ReadE(E) wAEScr AUTOSCRAF2;

              EndDo;

           //�Delete Subroutine
              wAEScr = '107DLTCD';
              SetLL wAEScr AUTOSCRAF2;
              ReadE(E) wAEScr AUTOSCRAF2;
              DoW Not %EOF(AUTOSCRAF2);
                wLineData = *Blanks;
                wLineData = AUTOLINE;
                ExSr $UpdateDfts;
                ExSr $AddLineToFile;

                ReadE(E) wAEScr AUTOSCRAF2;

              EndDo;

           //�End Subroutine
              wAEScr = '109END';
              SetLL wAEScr AUTOSCRAF2;
              ReadE(E) wAEScr AUTOSCRAF2;
              DoW Not %EOF(AUTOSCRAF2);
                wLineData = *Blanks;
                wLineData = AUTOLINE;
                ExSr $AddLineToFile;

                ReadE(E) wAEScr AUTOSCRAF2;

              EndDo;

            EndSr;

         //�----------------------------------------
         // $BuildSubFileScr - Build Subfile Screen
         //�----------------------------------------
            BegSr $BuildSubFileScr;

           //�Initialize
              wLineSeq  = *Zeros;
              wLineDate = *Zeros;
              wToFile   = 'SUBFILESCR';

           //�Build Screen

           //�Description
              wAEScr = '100DESC';

              SetLL wAEScr AUTOSCRAF3;
              ReadE(E) wAEScr AUTOSCRAF3;
              DoW Not %EOF(AUTOSCRAF3);
                wLineData = *Blanks;
                wLineData = AUTOLINE;
                ExSr $UpdateDfts;
                ExSr $AddLineToFile;

                ReadE(E) wAEScr AUTOSCRAF3;

              EndDo;

           //�Header
              wAEScr = '101HEAD';
              SetLL wAEScr AUTOSCRAF3;
              ReadE(E) wAEScr AUTOSCRAF3;
              DoW Not %EOF(AUTOSCRAF3);
                wLineData = *Blanks;
                wLineData = AUTOLINE;
                ExSr $UpdateDfts;
                ExSr $AddLineToFile;

                ReadE(E) wAEScr AUTOSCRAF3;

              EndDo;

              wAEScr = '102HEAD';
              SetLL wAEScr AUTOSCRAF3;
              ReadE(E) wAEScr AUTOSCRAF3;
              DoW Not %EOF(AUTOSCRAF3);
                wLineData = *Blanks;
                wLineData = AUTOLINE;
              //ExSr $UpdateDfts;
                ExSr $AddLineToFile;

                ReadE(E) wAEScr AUTOSCRAF3;

              EndDo;

              wAEScr = '103HEAD';
              SetLL wAEScr AUTOSCRAF3;
              ReadE(E) wAEScr AUTOSCRAF3;
              DoW Not %EOF(AUTOSCRAF3);
                wLineData = *Blanks;
                wLineData = AUTOLINE;
              //ExSr $UpdateDfts;
                ExSr $AddLineToFile;

                ReadE(E) wAEScr AUTOSCRAF3;

              EndDo;

           //�Data

              wAEScr = '104DATA';
              SetLL wAEScr AUTOSCRAF3;
              ReadE(E) wAEScr AUTOSCRAF3;
              DoW Not %EOF(AUTOSCRAF3);
                wLineData = *Blanks;
                wLineData = AUTOLINE;
              //ExSr $UpdateDfts;
                ExSr $AddLineToFile;

                ReadE(E) wAEScr AUTOSCRAF3;

              EndDo;

           //�Set Subfile Fields
              wTotSubCnt    = 5;
              wCurFldCnt    = 1;
              wCurLineVPos  = 10;
              wkPOSUPSCnt   = *Zeros;
              wkUPSAnd      = *Blanks;

           //�Check if to be included in SubFile
              DoW FldOptDS(wCurFldCnt ).FLDNAM <> *Blanks;

                If FldOptDS(wCurFldCnt ).FLDSUB = 'Y';

                  wAEScr = '105DATA';
                  Chain(E) wAEScr AUTOSCRAF3;
                  wLineData = *Blanks;
                  wLineData = AUTOLINE;

               //�Check if crossing screen limit
                  If %Len(%Trim(FldOptDS(wCurFldCnt ).FLDSFT)) >
                     %Int(FldOptDS(wCurFldCnt ).FLDLEN);
                     If wTotSubCnt  + 1 +
                        %Len(%Trim(FldOptDS(wCurFldCnt ).FLDSFT)) > 79;
                        Leave;
                     Else;
                        wCurFldLen  = %Len(%Trim(FldOptDS(wCurFldCnt ).FLDSFT));
                     EndIf;
                  Else;
                     If wTotSubCnt  + 1 +
                        %INT(FldOptDS(wCurFldCnt ).FLDLEN) > 79;
                       Leave;
                     Else;
                       wCurFldLen  = %Int(FldOptDS(wCurFldCnt ).FLDLEN);
                     EndIf;
                  EndIf;

                  wTotSubCnt  += 1;

               //�Check if numeric or alpha numeric
                  If FldOptDS(wCurFldCnt ).FLDTYP = 'S' Or
                     FldOptDS(wCurFldCnt ).FLDTYP = 'P' Or
                     FldOptDS(wCurFldCnt ).FLDTYP = 'F';

                    If %Int(FldOptDS(wCurFldCnt ).FLDDEC) > 9;
                      wDecLen = FldOptDS(wCurFldCnt ).FLDDEC;
                    Else;
                      wDecLen = '0' + %Trim(FldOptDS(wCurFldCnt ).FLDDEC);
                    EndIf;

                    wFldTyp = 'Y';
                    wAEScr  = '105DATA';

                  ElseIf FldOptDS(wCurFldCnt ).FLDTYP = 'Z';

                    wFldTyp = 'Z';
                    wDecLen = *Blanks;
                    wAEScr  = '105DATA';

                  ElseIf FldOptDS(wCurFldCnt ).FLDTYP = 'L';

                    wFldTyp = 'L';
                    wDecLen = *Blanks;
                    wAEScr  = '105DATA';

                  Else;

                    wFldTyp = 'A';
                    wDecLen = *Blanks;
                    wAEScr  = '105DATA';

                  EndIf;

               //�Set Field Name
                  %SubSt(wLineData:19:10) = FldOptDS(wCurFldCnt ).FLDNAM;

               //�Set Output Type (O)
                  %SubSt(wLineData:38:1) = 'O';

               //�Set Veritical Position
                  %SubSt(wLineData:40:2) = '10';

               //�Set Horizontal Position
                  If wTotSubCnt  > 9;
                    %SubSt(wLineData:43:2) = %Char(wTotSubCnt );
                    wkHUPos = %Char(wTotSubCnt );
                  Else;
                    %SubSt(wLineData:43:2) = '0' + %Char(wTotSubCnt );
                    wkHUPos = '0' + %Char(wTotSubCnt );
                  EndIf;

                  //�Add Position Info To DS
                  If FldOptDS(wCurFldCnt).FLDUPS = 'Y' And wkPOSUPSCnt < 10;

                    wkPOSUPSCnt += 1;

                    If wkPOSUPSCnt > 1;
                      wkUPSAnd = 'AND';
                    EndIf;

                    If wCurFldLen > 9;
                      wCurFldLenC = FldOptDS(wCurFldCnt).FLDLEN;
                    Else;
                      wCurFldLenC = '0' + %TRIM(FldOptDS(wCurFldCnt).FLDLEN);
                    EndIf;

                    wkUPSDS(wkPOSUPSCnt).FLDLENC = wCurFldLenC;

                    wkUPSDS(wkPOSUPSCnt).SUBSCR = '     A            POS0' +
                               %CHAR(wkPOSUPSCnt) +
                               '         ' + wCurFldLenC + '   B 07' +
                               ' ' + wkHUPos   ;

                    wkUPSDS(wkPOSUPSCnt).SUBSQL =
                    '     C+    ' + wkUPSAnd +
                    ' (:POS0' + %CHAR(wkPOSUPSCnt)  +
                    ' = '' '' OR ' + %TRIM(FldOptDS(wCurFldCnt ).FLDNAM) +
                    ' >= :POS0' + %CHAR(wkPOSUPSCnt) + ')';

                  EndIf;

                  ExSr $AddLineToFile;

                  wTotSubCnt  += wCurFldLen ;

                EndIf;

                wCurFldCnt  += 1;

              EndDo;

           //�End Of SRCSFL
              wAEScr = '107DATA';
              SetLL wAEScr AUTOSCRAF3;
              ReadE(E) wAEScr AUTOSCRAF3;
              DoW Not %EOF(AUTOSCRAF3);
                wLineData = *Blanks;
                wLineData = AUTOLINE;
              //ExSr $UpdateDfts;
                ExSr $AddLineToFile;

                ReadE(E) wAEScr AUTOSCRAF3;

              EndDo;

           //�Set Position Data
		            If wkPOSUPSCnt > 0;
			             wAEScr = '107POSA';

			             For i = 1 To wkPOSUPSCnt;

                  wLineData = wkUPSDS(i).SUBSCR;
                  ExSr $AddLineToFile;

                  wAEScr = '107POSB';
                  Chain(E) (wAEScr:62) AUTOSCRAF3;
                  wLineData = *Blanks;
                  wLineData = AUTOLINE;

                  ExSr $AddLineToFile;

                EndFor;
				
			           EndIf;

           //�Set Field Headings
              wTotSubCnt    = 5;
              wCurFldCnt    = 1;
              wCurLineVPos  = 10;
              wPosDone      = *Off;

           //�Check if to be included in SubFile
              DoW FldOptDS(wCurFldCnt ).FLDNAM <> *Blanks;

                If FldOptDS(wCurFldCnt ).FLDSUB = 'Y';

                  wAEScr = '108DATA';
                  Chain(E) (wAEScr:65) AUTOSCRAF3;
                  wLineData = *Blanks;
                  wLineData = AUTOLINE;

               //�Check if crossing screen limit
                  If %Len(%Trim(FldOptDS(wCurFldCnt ).FLDSFT)) >
                     %Int(FldOptDS(wCurFldCnt ).FLDLEN);
                     If wTotSubCnt  + 1 +
                        %Len(%Trim(FldOptDS(wCurFldCnt ).FLDSFT)) > 79;
                        Leave;
                     Else;
                        wCurFldLen  = %Len(%Trim(FldOptDS(wCurFldCnt ).FLDSFT));
                     EndIf;
                  Else;
                     If wTotSubCnt  + 1 +
                        %INT(FldOptDS(wCurFldCnt ).FLDLEN) > 79;
                       Leave;
                     Else;
                       wCurFldLen  = %Int(FldOptDS(wCurFldCnt ).FLDLEN);
                     EndIf;
                  EndIf;

                  wTotSubCnt  += 1;

               //�Set Horizontal Position
                  If wTotSubCnt  > 9;
                    %SubSt(wLineData:43:2) = %Char(wTotSubCnt );
                  Else;
                    %SubSt(wLineData:43:2) = '0' + %Char(wTotSubCnt );
                  EndIf;

               //�Set Heading Text for field
                  %SubSt(wLineData:46) =
                    %Trim(FldOptDS(wCurFldCnt ).FLDSFT) + '''';

                  ExSr $AddLineToFile;

               //�Write Colour Line
                  Chain(E) (wAEScr:66) AUTOSCRAF3;
                  wLineData = *Blanks;
                  wLineData = AUTOLINE;
                  ExSr $AddLineToFile;

                  wTotSubCnt  += wCurFldLen ;

                EndIf;
                wCurFldCnt  += 1;

              EndDo;

           //�Confirm Window
              wAEScr = '109CONF';
              SetLL wAEScr AUTOSCRAF3;
              ReadE(E) wAEScr AUTOSCRAF3;
              DoW Not %EOF(AUTOSCRAF3);
                wLineData = *Blanks;
                wLineData = AUTOLINE;
              //ExSr $UpdateDfts;
                ExSr $AddLineToFile;

                ReadE(E) wAEScr AUTOSCRAF3;

              EndDo;

           //�Footer
              wAEScr  = '109FOOT';
              SetLL wAEScr AUTOSCRAF3;
              ReadE(E) wAEScr AUTOSCRAF3;
              DoW Not %EOF(AUTOSCRAF3);
                wLineData = *Blanks;
                wLineData = AUTOLINE;
                ExSr $AddLineToFile;

                ReadE(E) wAEScr AUTOSCRAF3;

              EndDo;


            EndSr;

         //�-----------------------------------------
         // $BuildSubFilePgm - Build Subfile Program
         //�-----------------------------------------
            BegSr $BuildSubFilePgm;

           //�Initialize
              wLineSeq  = *Zeros;
              wLineDate = *Zeros;
              wToFile   = 'SUBFILEPGM';

           //�Build Program

           //�Header
              wAEScr = '100HEAD';

              SetLL wAEScr AUTOSCRAF4;
              ReadE(E) wAEScr AUTOSCRAF4;
              DoW Not %EOF(AUTOSCRAF4);
                If AUTOLSEQ = 26;
                  For i = 1 to wkPOSUPSCnt;
                    wLineData = AUTOLINE;
                    %SubSt(wLineData:15:1) = %Char(i);
                    %SubSt(wLineData:38:2) = wkUPSDS(i).FLDLENC;
                    ExSr $AddLineToFile;
                  EndFor;

                Else;
                  wLineData = *Blanks;
                  wLineData = AUTOLINE;
                  ExSr $UpdateDfts;
                  ExSr $AddLineToFile;

                EndIf;

                ReadE(E) wAEScr AUTOSCRAF4;

              EndDo;
           //�Cursor Desc
              wAEScr = '101HEAD';
              SetLL wAEScr AUTOSCRAF4;
              Chain(E) (wAEScr:66) AUTOSCRAF4;
              If %Found(AUTOSCRAF4);
                wLineData = *Blanks;
                wLineData = AUTOLINE;
                ExSr $UpdateDfts;
                ExSr $AddLineToFile;
              EndIf;

           //�Header Continue...
              wAEScr = '102HEAD';
              SetLL wAEScr AUTOSCRAF4;
              ReadE(E) wAEScr AUTOSCRAF4;
              DoW Not %EOF(AUTOSCRAF4);
                wLineData = *Blanks;
                wLineData = AUTOLINE;
                ExSr $UpdateDfts;
                ExSr $AddLineToFile;

                ReadE(E) wAEScr AUTOSCRAF4;

              EndDo;

           //�Main Processing
              wAEScr = '103MAIN';
              SetLL wAEScr AUTOSCRAF4;
              ReadE(E) wAEScr AUTOSCRAF4;
              DoW Not %EOF(AUTOSCRAF4);

                Select;
                  When AUTOLSEQ = 168;
                    For i = 1 to wkPOSUPSCnt;
                      Chain(E) (wAEScr:168) AUTOSCRAF4;
                      wLineData = AUTOLINE;
                      %SubSt(wLineData:32:1) = %Char(i);
                      ExSr $AddLineToFile;
                      Read AUTOSCRAF4;
                      wLineData = AUTOLINE;
                      %SubSt(wLineData:29:1) = %Char(i);
                      ExSr $AddLineToFile;
                    EndFor;
                    Read AUTOSCRAF4;

                  When AUTOLSEQ = 175;
                    For i = 1 to wkPOSUPSCnt;
                      Chain(E) (wAEScr:175) AUTOSCRAF4;
                      wLineData = AUTOLINE;
                      %SubSt(wLineData:29:1) = %Char(i);
                      %SubSt(wLineData:41:1) = %Char(i);
                      ExSr $AddLineToFile;
                      Read AUTOSCRAF4;
                      wLineData = AUTOLINE;
                      %SubSt(wLineData:32:1) = %Char(i);
                      %SubSt(wLineData:40:1) = %Char(i);
                      ExSr $AddLineToFile;
                      Read AUTOSCRAF4;
                      wLineData = AUTOLINE;
                      ExSr $AddLineToFile;
                      Read AUTOSCRAF4;
                      wLineData = AUTOLINE;
                      ExSr $AddLineToFile;
                    EndFor;

                  Other;

                    wLineData = *Blanks;
                    wLineData = AUTOLINE;
                    ExSr $UpdateDfts;
                    ExSr $AddLineToFile;

                EndSl;

                ReadE(E) wAEScr AUTOSCRAF4;

              EndDo;

              wAEScr = '105MAIN';
              SetLL wAEScr AUTOSCRAF4;
              ReadE(E) wAEScr AUTOSCRAF4;
              DoW Not %EOF(AUTOSCRAF4);
                wLineData = *Blanks;
                wLineData = AUTOLINE;
                ExSr $UpdateDfts;
                ExSr $AddLineToFile;

                ReadE(E) wAEScr AUTOSCRAF4;

              EndDo;

              wAEScr = '106MAIN';
              SetLL wAEScr AUTOSCRAF4;
              ReadE(E) wAEScr AUTOSCRAF4;
              DoW Not %EOF(AUTOSCRAF4);
                wLineData = *Blanks;
                wLineData = AUTOLINE;
                ExSr $AddLineToFile;

                ReadE(E) wAEScr AUTOSCRAF4;

              EndDo;
              wAEScr = '108MAIN';
              SetLL wAEScr AUTOSCRAF4;
              ReadE(E) wAEScr AUTOSCRAF4;
              DoW Not %EOF(AUTOSCRAF4);
                wLineData = *Blanks;
                wLineData = AUTOLINE;
                ExSr $AddLineToFile;

                ReadE(E) wAEScr AUTOSCRAF4;

              EndDo;

           //�Load Cursor Data

              wAEScr = '109FILE';
              SetLL wAEScr AUTOSCRAF4;
              Chain(E) wAEScr AUTOSCRAF4;
              If %Found(AUTOSCRAF4);
                wLineData = *Blanks;
                wLineData = AUTOLINE;
                %SubSt(wLineData:15:10) = FILENAM;
                ExSr $AddLineToFile;
              EndIf;

              If wkPOSUPSCnt > 0;
              wAEScr = '110MAIN';
                SetLL wAEScr AUTOSCRAF4;
                Chain(E) wAEScr AUTOSCRAF4;
                If %Found(AUTOSCRAF4);
                  wLineData = *Blanks;
                  wLineData = AUTOLINE;
                  ExSr $AddLineToFile;
                EndIf;
              EndIf;

           //�Set Subfile Fields
              wCurFldCnt    = 1;

           //�Set Position Data
              For i = 1 To wkPOSUPSCnt;

                wLineData = wkUPSDS(i).SUBSQL;
                ExSr $AddLineToFile;

              EndFor;

           //�End Of Program
              wAEScr = '112MAIN';
              SetLL wAEScr AUTOSCRAF4;
              ReadE(E) wAEScr AUTOSCRAF4;
              DoW Not %EOF(AUTOSCRAF4);
                wLineData = *Blanks;
                wLineData = AUTOLINE;
                ExSr $AddLineToFile;

                ReadE(E) wAEScr AUTOSCRAF4;

              EndDo;

            EndSr;

         //�----------------------------------------
         // $Compile - Compile Display And Program
         //�----------------------------------------
            BegSr $Compile;

           //�DLTOVR Files
              wCommand  = 'DLTOVR FILE(*ALL)';

              wCmdLen  = %Len(wCommand );

              OCBDS.Prompt = '0';
              APIErrorDS.MsgId = *Blanks;

              CallP(E) PRUNCMD(wCommand     :
                               wCmdLen      :
                               OCBDS        :
                               %Len(OCBDS)  :
                               'CPOP0100'   :
                               ChangedCmd   :
                               CmdAvailLen  :
                               CmdChangeLen :
                               APIErrorDS  );

           //�Compile Subfile Display
              wCommand  = 'CRTDSPF FILE(AUTOSUBD) +
                           SRCFILE(*LIBL/AUTOSCRF) SRCMBR(AUTOSUBD) +
                           REPLACE(*YES)';
              wCmdLen  = %Len(wCommand );
              OCBDS.Prompt = '0';
              APIErrorDS.MsgId = *Blanks;

              CallP(E) PRUNCMD(wCommand     :
                               wCmdLen      :
                               OCBDS        :
                               %Len(OCBDS)  :
                               'CPOP0100'   :
                               ChangedCmd   :
                               CmdAvailLen  :
                               CmdChangeLen :
                               APIErrorDS  );

           //�Compile Add/Edit Display
              wCommand  = 'CRTDSPF FILE(AUTOTMPD) +
                           SRCFILE(*LIBL/AUTOSCRF) SRCMBR(AUTOTMPD) +
                           REPLACE(*YES)';
              wCmdLen  = %Len(wCommand );

              OCBDS.Prompt = '0';
              APIErrorDS.MsgId = *Blanks;

              CallP(E) PRUNCMD(wCommand     :
                               wCmdLen      :
                               OCBDS        :
                               %Len(OCBDS)  :
                               'CPOP0100'   :
                               ChangedCmd   :
                               CmdAvailLen  :
                               CmdChangeLen :
                               APIErrorDS  );

           //�Compile Subfile Program
              wCommand  = 'CRTSQLRPGI OBJ(AUTOSPGP) +
                           SRCFILE(*LIBL/AUTOSCRF) SRCMBR(AUTOSPGP) +
                           OBJTYPE(*PGM) REPLACE(*YES)';
              wCmdLen  = %Len(wCommand );

              OCBDS.Prompt = '0';
              APIErrorDS.MsgId = *Blanks;
              CallP(E) PRUNCMD(wCommand     :
                               wCmdLen      :
                               OCBDS        :
                               %Len(OCBDS)  :
                               'CPOP0100'   :
                               ChangedCmd   :
                               CmdAvailLen  :
                               CmdChangeLen :
                               APIErrorDS  );

           //�Compile Add/Edit Program
              wCommand  = 'CRTSQLRPGI OBJ(AUTOTMPP) +
                           SRCFILE(*LIBL/AUTOSCRF) SRCMBR(AUTOTMPP) +
                           OBJTYPE(*PGM) REPLACE(*YES)';
              wCmdLen  = %Len(wCommand );

              OCBDS.Prompt = '0';
              APIErrorDS.MsgId = *Blanks;

              CallP(E) PRUNCMD(wCommand     :
                               wCmdLen      :
                               OCBDS        :
                               %Len(OCBDS)  :
                               'CPOP0100'   :
                               ChangedCmd   :
                               CmdAvailLen  :
                               CmdChangeLen :
                               APIErrorDS  );

              If APIErrorDS.MsgId <> *Blanks;
              // Some Error Occurred
                 WCmdOk = *Off;
              Else;
              // No Errors
                 WCmdOk   = *On;
              EndIf;

            EndSr;

         //�----------------------------------------
         // $UpdateDfts - Update Defaults
         //�----------------------------------------
            BegSr $UpdateDfts;

              wTrimOpt = *Blanks;

           //�Project
              wCurText = '==PROJECTNM==';
              wRepText = sPROJNM;
              ExSr $RplDefaults;

           //�Program Name
              wCurText = '==PROGRAMMP==';
              wRepText = %Trim(inPgmPrfx) + 'MP';
              ExSr $RplDefaults;

           //�Program Name
              wCurText = '==PROGRAMSP==';
              wRepText = %Trim(inPgmPrfx) + 'SP';
              ExSr $RplDefaults;

           //�Programmer
              wCurText = '==PROG=======';
              wRepText = sAUTH;
              ExSr $RplDefaults;

           //�Project Reference
              wCurText = '==PROJECT====';
              wRepText = sPROJCD;
              ExSr $RplDefaults;

           //�Date
              wCurText = '==DATE=======';
              wRepText = sDATE;
              ExSr $RplDefaults;

           //�Description
              wCurTextL = '==DESC=======';
              wRepTextL = sDESC + '�*';
              ExSr $RplLngDefaults;

           //�Screen Name
              wCurTextL = '==SCREENNM===';
              wRepTextL = ''''  + %Trim(SSCRN) + '''' + ';';
              ExSr $RplLngDefaults;

           //�Record Format
              wCurText = 'DSPREC=';
              wRepText = wRecName;
              ExSr $RplDefaults;

           // wCurText = '==Screen Name Place==';
           // wRepText = SSCRN;
           // ExSr $RplDefaults;

           //�File Name
              wTrimOpt = 'T';

              wCurText = '==FILE==';
              wRepText = FILENAM;
              ExSr $RplDefaults;

              wCurText = '==FILE====';
              wRepText = FILENAM;
              ExSr $RplDefaults;

           //�File Name
              wCurText = 'TS=';
              wRepText = %Char(wScrRecNo) + ';';
              ExSr $RplDefaults;

           //�File Name
              wCurText = 'CS=';
              If wScrCnt > 9;
                wRepText = %Char(wScrCnt);
              Else;
                wRepText = '0' + %Char(wScrCnt);
              EndIf;
              ExSr $RplDefaults;

           //�Program Name - Main
              wCurText = '==PGMMP===';
              wRepText = %Trim(inPgmPrfx) + 'MP';
              ExSr $RplDefaults;

           //�Program Name - Sub
              wCurText = '==PGMSP===';
              wRepText = %Trim(inPgmPrfx) + 'SP';
              ExSr $RplDefaults;

              wTrimOpt = 'E';

           //�Main Pgm Name
              wCurText = '==PGMMP$$$';
              wkExLen  = 10;
              wRepText = %Trim(inPgmPrfx) + 'MP';
              ExSr $RplDefaults;

           //�Main Pgm Display
              wCurText = '==PGMMD$$$';
              wkExLen  = 10;
              wRepText = %Trim(inPgmPrfx) + 'MD';
              ExSr $RplDefaults;

           //�Subfile Pgm Name
              wCurText = '==PGMSP$$$';
              wRepText = %Trim(inPgmPrfx) + 'SP';
              ExSr $RplDefaults;

           //�Subfile Pgm Display
              wCurText = '==PGMSD$$$';
              wRepText = %Trim(inPgmPrfx) + 'SD';
              ExSr $RplDefaults;

              wCurText = '==FILE$$$$';
              wRepText = FILENAM;
              ExSr $RplDefaults;

            EndSr;

         //�----------------------------------------
         // $ChkFile - Check Files
         //�----------------------------------------
            BegSr $ChkFile;

              ExSr $ChkExists;

            EndSr;

         //�----------------------------------------
         // $GetFldOptions - Get Field Options
         //�----------------------------------------
            BegSr $GetFldOptions;

            //�Clear DS
               Clear FldOptDS;

            //�Declare Cursor & Open Cursor
               ExSr $DecCurSor;

               *In22  = *On;
               wCurFldCnt  = 0;

               ExSr $FetchNxt;
               DoW SQLCOD = 0;

                 wCurFldCnt  += 1;
                 Clear ScrDS;

                 FLDNAM = SFldNm;
                 FLDLEN = %Char(SFldLn);
                 FLDDEC = %Char(SFldDc);
                 FLDDSC = SFldTX;
                 FLDCURC = %Char(wCurFldCnt ) + '/' + %Char(SFldTO);
                 FLDSCR = 'Y';
                 FLDUPS = 'N';
                 FLDKEY = 'N';
                 FLDSUB = 'N';
                 FLDVAL = 'N';

                 If %Len(%Trim(SFldTX)) > 0 And %Len(%Trim(SFldTX)) < 16;
                   FLDAET = SFldTX;
                 Else;
                   FLDAET = FLDNAM;
                 EndIf;

                 *IN66 = *On;

                 Select;
                 When SFldTy = 'A';
                    FLDTYP = 'A - Char';
                    *IN66 = *Off;
                 When SFldTy = 'S';
                    FLDTYP = 'S - Decimal';
                    *IN66 = *Off;
                 When SFldTy = 'P';
                    FLDTYP = 'P - Decimal';
                    *IN66 = *Off;
                 When SFldTy = 'B';
                    FLDTYP = 'B - Binary';
                 When SFldTy = 'F';
                    FLDTYP = 'F - Float';
                 When SFldTy = 'L';
                    FLDTYP = 'L - Date';
                 When SFldTy = 'T';
                    FLDTYP = 'T - Time';
                 When SFldTy = 'Z';
                    FLDTYP = 'TimeStamp';
                 When SFldTy = 'H';
                    FLDTYP = 'H - Hexa';
                 When SFldTy = 'J';
                    FLDTYP = 'J - DBCS';
                 When SFldTy = 'E';
                    FLDTYP = 'E - DBCS';
                 When SFldTy = 'O';
                    FLDTYP = 'O - DBCS';
                 When SFldTy = 'G';
                    FLDTYP = 'G - DBCS';
                 When SFldTy = '5';
                    FLDTYP = '5 - BinChar';
                 EndSl;

                 wCorrect  = *Off;

                 DoU wCorrect  = *On;

                   If *in18 = *Off;

                     Write Header;
                     Write Footer;
                     ExFmt GETFLD;

                     If *In03 = *On Or *In12 = *On;
                       ExSr $ExitPgm;
                     EndIf;

                     ERRMSG = *Blanks;

                     ExSr $ChkCorrect;

                   Else;

                     wCorrect  = *On;

                   EndIf;

                 EndDo;

                 ExSr $SaveInDS;

                 ExSr $FetchNxt;

               EndDo;


            // Close Cursor
               ExSr $CloseCur;

            EndSr;

         //�----------------------------------------
         // $ChkCorrect - Check if correct data
         //�----------------------------------------
            BegSr $ChkCorrect;

           //�Display In Screen
              If FLDSCR <> 'Y' and FLDSCR <> 'N';
                ERRMSG = 'Invalid Display In Screen. Y or N';
              EndIf;

           //�Default Value
              If FLDSCR = 'N' AND FLDDFT = *Blanks;
                ERRMSG = 'Default Value required';
              EndIf;

           //�Display In Sbfile
              If FLDSUB <> 'Y' AND FLDSUB <> 'N';
                ERRMSG = 'Invalid Display In Sbfile. Y or N';
              EndIf;

           //�Subfile Scr Text
              If FLDSUB = 'Y' AND FLDSFT = *Blanks;
                ERRMSG = 'Subfile Scr Text required';
              EndIf;

           //�Add/Edit Scr Text
              If FLDSCR = 'Y' AND FLDAET = *Blanks;
                ERRMSG = 'Add/Edit Scr Text required';
              EndIf;

           //�Validation Req?
              If FLDVAL <> 'Y' AND FLDVAL <> 'N';
                ERRMSG = 'Invalid Validation Req?.   Y or N';
              EndIf;

           //�Valid Values
              If FLDVAL = 'Y' AND FLDVA1 = *Blanks AND
                 FLDMIN = *Blanks AND FLDMAX = *Blanks;
                ERRMSG = 'Valid or Min-Max Value required';
              EndIf;

           //�Valid Values
              If FLDSUB <> 'Y' AND FLDUPS = 'Y';
                ERRMSG = 'Use Position Only For SubFile Fields';
              EndIf;

           //�Numeric Field
              If SFldTy = 'S' Or SFldTy = 'P' Or SFldTy = 'F';
                If FLDMIN <> *Blanks;
                  Monitor;
                    wIntTest = %INT(FLDMIN);
                  On-Error;
                    ERRMSG = 'Invalid Minimum Value';
                  EndMon;
                EndIf;

                If FLDMIN <> *Blanks;
                  Monitor;
                    wIntTest = %INT(FLDMAX);
                  On-Error;
                    ERRMSG = 'Invalid Maximum Value';
                  EndMon;
                EndIf;

                If FLDVA2 <> *Blanks;
                  Monitor;
                    wIntTest = %INT(FLDVA1);
                  On-Error;
                    ERRMSG = 'Invalid Valid Value 1';
                  EndMon;
                EndIf;
                If FLDVA2 <> *Blanks;
                  Monitor;
                    wIntTest = %INT(FLDVA2);
                  On-Error;
                    ERRMSG = 'Invalid Valid Value 2';
                  EndMon;
                EndIf;

                If FLDVA3 <> *Blanks;
                  Monitor;
                    wIntTest = %INT(FLDVA3);
                  On-Error;
                    ERRMSG = 'Invalid Valid Value 3';
                  EndMon;
                EndIf;

                If FLDVA4 <> *Blanks;
                  Monitor;
                    wIntTest = %INT(FLDVA4);
                  On-Error;
                    ERRMSG = 'Invalid Valid Value 4';
                  EndMon;
                EndIf;

              EndIf;

           //�Text Field
              If SFldTy <> 'S' And SFldTy <> 'P' And SFldTy <> 'F';
                If FLDMIN <> *Blanks Or FLDMAX <> *Blanks;
                  ERRMSG = 'Minimum Value Or Maximum Value not allowed';
                EndIf;
              EndIf;

              If ERRMSG = *Blanks;
                wCorrect  = *On;
              EndIf;


            EndSr;

         //�----------------------------------------
         // $SaveInDS - Save Data in Data Structure
         //�----------------------------------------
            BegSr $SaveInDS;

              FldOptDS(wCurFldCnt ).FLDNAM = FLDNAM;
              FldOptDS(wCurFldCnt ).FLDTYP = SFldTy;
              FldOptDS(wCurFldCnt ).FLDLEN = FLDLEN;
              FldOptDS(wCurFldCnt ).FLDDEC = FLDDEC;
              FldOptDS(wCurFldCnt ).FLDSCR = FLDSCR;
              FldOptDS(wCurFldCnt ).FLDAET = FLDAET;
              FldOptDS(wCurFldCnt ).FLDKEY = FLDKEY;
              FldOptDS(wCurFldCnt ).FLDDFT = FLDDFT;
              FldOptDS(wCurFldCnt ).FLDSUB = FLDSUB;
              FldOptDS(wCurFldCnt ).FLDSFT = FLDSFT;
              FldOptDS(wCurFldCnt ).FLDVAL = FLDVAL;
              FldOptDS(wCurFldCnt ).FLDMIN = FLDMIN;
              FldOptDS(wCurFldCnt ).FLDMAX = FLDMAX;
              FldOptDS(wCurFldCnt ).FLDVA1 = FLDVA1;
              FldOptDS(wCurFldCnt ).FLDVA2 = FLDVA2;
              FldOptDS(wCurFldCnt ).FLDVA3 = FLDVA3;
              FldOptDS(wCurFldCnt ).FLDVA4 = FLDVA4;
              FldOptDS(wCurFldCnt ).FLDREC = *Zeros;
              FldOptDS(wCurFldCnt ).FLDUPS = FLDUPS;

            EndSr;

         //�----------------------------------------
         //�$RplDefaults - Replace defaults
         //�----------------------------------------
            BegSr $RplDefaults;

              If wTrimOpt = 'T';
                EXEC SQL
                   SELECT REPLACE(:wLineData,
                          TRIM(:wCurText), TRIM(:wRepText))
                     INTO :wLineData
                   FROM SYSIBM/SYSDUMMY1;

              ElseIf wTrimOpt = 'E';
                EXEC SQL
                   SELECT REPLACE(:wLineData,
                          SUBSTRING(:wCurText,1,10),
                          SUBSTRING(:wRepText,1,10))
                     INTO :wLineData
                   FROM SYSIBM/SYSDUMMY1;

              ElseIf wTrimOpt = ' ';
                EXEC SQL
                   SELECT REPLACE(:wLineData,
                          :wCurText, :wRepText)
                     INTO :wLineData
                   FROM SYSIBM/SYSDUMMY1;
              EndIf;

            EndSr;

         //�---------------------------------------------
         //�$RplLngDefaults - Replace Long Text defaults
         //�---------------------------------------------
            BegSr $RplLngDefaults;

                EXEC SQL
                   SELECT REPLACE(:wLineData,
                          TRIM(:wCurTextL), TRIM(:wRepTextL))
                     INTO :wLineData
                   FROM SYSIBM/SYSDUMMY1;

            EndSr;

         //�----------------------------------------
         //�$WriteLineToFile - Write Line to File
         //�----------------------------------------
            BegSr $AddLineToFile;

               wLineSeq += 1;

               Select;

               When wToFile = 'ADDEDITSCR';
                 Exec SQL
                   INSERT INTO AUTOSCRF
                   VALUES(:wLineSeq, :wLineDate, :wLineData);

               When wToFile = 'ADDEDITPGM';
                 Exec SQL
                   INSERT INTO AUTOPGMF
                   VALUES(:wLineSeq, :wLineDate, :wLineData);

               When wToFile = 'SUBFILESCR';
                 Exec SQL
                   INSERT INTO AUTOSUBF
                   VALUES(:wLineSeq, :wLineDate, :wLineData);

               When wToFile = 'SUBFILEPGM';
                 Exec SQL
                   INSERT INTO AUTOSPGF
                   VALUES(:wLineSeq, :wLineDate, :wLineData);

               EndSl;

            EndSr;
         //�----------------------------------------
         //�$ExitPgm - Exit Program
         //�----------------------------------------
            BegSr $ExitPgm;

               *InLr = *On;
               Return;

            EndSr;

      /End-Free
     �*  // -------------------------------------
     �*  // $DecCurSor - Declare and Open Cursor
     �*  // -------------------------------------
     C     $DecCurSor    BegSr

     C/EXEC SQL
     C+  CLOSE SFILECURF
     C/END-EXEC

     C/EXEC SQL
     C+  DECLARE SFILECURF CURSOR FOR
     C+  SELECT WHFLDI,
     C+  CASE WHEN WHFLDD <> 0 THEN WHFLDD ELSE WHFLDB END,
     C+  WHFLDP, WHFLDT, WHFTXT, WHFLDN
     C+  FROM QTEMP/FLDREFF
     C+  WHERE WHFILE = :FILENAM AND
     C+  (:LIBRARY = ' ' Or WHLIB = :LIBRARY)
     C+  FOR FETCH ONLY
     C/END-EXEC

     C/EXEC SQL
     C+  OPEN SFILECURF
     C/END-EXEC

     C                   EndSr

     �*  // ------------------------------------------
     �*  // $FetchNxt - Fetch next record from cursor
     �*  // ------------------------------------------
     C     $FetchNxt     BegSr

     C/EXEC SQL
     C+  FETCH SFILECURF INTO :SrcFileFDS
     C/END-EXEC

     C                   EndSr

     �*  // ------------------------------------------
     �*  // $CloseCur - Close Cursor
     �*  // ------------------------------------------
     C     $CloseCur     BegSr


     C/EXEC SQL
     C+  CLOSE SFILECURF
     C/END-EXEC

     C                   EndSr

     �*  // ----------------------------------------------
     �*  // $ChkExists - Check If already exists in QTEMP
     �*  // ----------------------------------------------
     C     $ChkExists    BegSr

     C/EXEC SQL
     C+  SELECT WHFLDI INTO :ChkKey FROM QTEMP/FLDREFF
     C+  WHERE WHFILE = :FILENAM AND
     C+  (:LIBRARY = ' ' Or WHLIB = :LIBRARY)
     C+  FETCH FIRST ROW ONLY
     C/END-EXEC

      /fREE
          //If ChkKey = *Blanks;

               wCommand   = 'DSPFFD FILE(' + %Trim(LIBRARY) + '/' +
                             %Trim(FILENAM) +
                            ') OUTPUT(*OUTFILE) OUTFILE(QTEMP/FLDREFF) +
                              OUTMBR(*FIRST *REPLACE)';
               wCmdLen   = %Len(wCommand  );
               CallP(E) RunCmd(wCommand  :wCmdLen  );

          //EndIf;

      /End-Free
     C/EXEC SQL
     C+  SELECT WHFLDI INTO :ChkKey FROM QTEMP/FLDREFF
     C+  WHERE WHFILE = :FILENAM AND
     C+  (:LIBRARY = ' ' Or WHLIB = :LIBRARY)
     C+  FETCH FIRST ROW ONLY
     C/END-EXEC

      /fREE
            If ChkKey = *Blanks;
              wFileFnd   = *Off;
            Else;
              wFileFnd   = *On;
            EndIf;

      /End-Free
     C                   EndSr
     �**********************************************************************
