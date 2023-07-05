     �***********************************************************************   
      * (C) COPYRIGHT 2017, ASHISH                                              
      * ALL RIGHTS RESERVED                                                     
      *                                                                         
      *     * * * * *  C I T I  C O N F I D E N T I A L  * * * * *              
      * THIS PROGRAM IS AN UNPUBLISHED WORK FULLY PROTECTED BY THE UNITED       
      * STATES COPYRIGHT LAWS AND IS CONSIDERED A TRADE SECRET BELONGING TO     
      * THE COPYRIGHT HOLDER.                                                   
     �************************************************************************* 
      *                                                                       * 
      * ===================================================================== * 
      *� Program Name     �: ==PROGRAMSP==                                   �* 
      *� Author           �: ==PROG=======                                   �* 
      *� Date             �: ==DATE=======                                   �* 
      *� Project          �: ==PROJECTNM==                                   �* 
      *� Work list        �: ==PROJECT====                                   �* 
      *� Description      �: ==DESC=======                                   �* 
      * �                                                                    �* 
      * ===================================================================== * 
      *                        Change History                                 * 
      * =====================================================================�* 
      *� Author           �:�                                                �* 
      *� Date             �:�                                                �* 
      *� Project Reference�:�                                                �* 
      *� Work list        �:�                                                �* 
      *� Description      �:�                                                �* 
      *� Modification tag �:�                                                �* 
      *                                                                       * 
      * ===================================================================== * 
      *                                                                         
      ************************************************************************* 
     H Debug Option(*NoDebugIO)                                                 
                                                                                
     �* Display File Specification                                              
     F==PGMSD$$$CF   E             WORKSTN SFILE(SRCSFL:SFLRRN)                 
                                                                                
     �* File Specification                                                      
     �*NA                                                                       
                                                                                
     �* Variable Declaration                                                    
     DVarDs1           DS                                                       
     D Wk_Command                   300    Inz(*Blanks)                         
     D WkScrName                     50    Inz(*Blanks)                         
     D Wk_UsgPgm                     10    Inz(*Blanks)                         
     D Wk_Prompt                      1    Inz(*Blanks)                         
     D Wk_CurOpt                     10    Inz(*Blanks)                         
     D Wk_RepOpt                     10    Inz(*Blanks)                         
     D WkMode                         3    Inz(*Blanks)                         
     D PrvPOS01                      10    Inz(*Blanks)                         
     D PrvRELRCD                      5S 0 Inz(*Zeros)                          
     D WkStartPos                     2  0 Inz(*Zeros)                          
     D Wk_CmdLen                     15P 5 Inz(*Zeros)                          
     D SFLRRN                         4  0 Inz(*Zeros)                          
     D Wk_Count                       4  0 Inz(*Zeros)                          
     D Wk_TmpRRN                      4  0 Inz(*Zeros)                          
     D WkRecNo         S             10  0 Inz(*Zeros)                          
     D FndFlg          S               N   Inz(*Off)                            
     D InAuth          S               N   Inz(*On)                             
     D F3Flg           S               N   Inz(*Off)                            
                                                                                
     �* Indicators                                                              
     D Wk_ResetDsp     S               N   Inz(*Off)                            
     D Wk_ErrFlg       S               N   Inz(*Off)                            
     D Wk_CmdOk        S               N   Inz(*Off)                            
     D wkCHGPOS        S               N   Inz(*Off)                            
     D InKyPtr         S               *   Inz(%ADDR(*IN))                      
                                                                                
     DSubFileInd       DS            99    Based(InKyPtr)                       
     D SIfChk                          N   Overlay(SubFileInd: 22)              
     D SflDsp                          N   Overlay(SubFileInd: 30)              
     D SflClr                          N   Overlay(SubFileInd: 31)              
     D SflEnd                          N   Overlay(SubFileInd: 32)              
                                                                                
     �* Constants                                                               
     DUCase            C                   CONST('ABCDEFGHIJKLMNOPQRSTUVWXYZ')  
     DLCase            C                   CONST('abcdefghijklmnopqrstuvwxyz')  
     DQ                C                   CONST('''')                          
                                                                                
     �* Data Structures                                                         
                                                                                
     D ApiErrorDs      DS                  INZ QUALIFIED                        
     D  BYTESPROV                    10I 0 INZ(%SIZE(APIERRORDS.MSGDATA))       
     D  BYTESAVAL                    10I 0                                      
     D  MSGID                         7A                                        
     D  FILLER                        1A                                        
     D  MSGDATA                     256A                                        
                                                                                
     �* Program Status Data Structure                                           
     D PSDS           SDS                                                       
     D PDSMsgID               40     46                                         
     D USER                  254    263                                         
                                                                                
     �* Cursor Data Structure                                                   
     DCCURDS         E DS                  EXTNAME('==FILE==  ')                
     DA                              10A                                        
     DP                               2  0                                      
     DD                                Z                                        
     DSRECNO                         10  0                                      
                                                                                
     �* Prototypes                                                              
     D ==PGMSP$$$      PR                  EXTPGM('==PGMSP===')                 
                                                                                
     D ==PGMMP$$$      PR                  EXTPGM('==PGMMP===')                 
     D Mode                           3                                         
     D RecNo                         10  0                                      
     D ErrFlg                          N                                        
                                                                                
     D ==PGMSP$$$      PI                                                       
                                                                                
      /Free                                                                     
                                                                                
         //�----------------------------------------------------------          
         //�                      Program Starts                     -          
         //�----------------------------------------------------------          
                                                                                
         //�Set SQL Options                                                     
            EXEC SQL                                                            
              SET OPTION COMMIT = *NONE, CLOSQLCSR = *ENDMOD;                   
                                                                                
            EXEC SQL                                                            
              WHENEVER SQLERROR CONTINUE;                                       
                                                                                
         // Screen Name                                                         
            wkScrName = ==SCREENNM===                                           
            wkStartPos = (50 - %Len(%Trim(wkScrName))) / 2;                     
            If wkStartPos < 1;                                                  
              wkStartPos = 1;                                                   
            EndIf;                                                              
            %SubSt(inScrn:wkStartPos) = %Trim(wkScrName);                       
                                                                                
         //�Initialize Variables                                                
            *In41 = *On;                                                        
                                                                                
         //�Load Subfile & Display                                              
            ExSr $LoadSF;                                                       
                                                                                
         //�Exit Program                                                        
            ExSr $ExitPgm;                                                      
                                                                                
                                                                                
         //�----------------------------------------------------------          
         //�                        Subroutines                      -          
         //�----------------------------------------------------------          
                                                                                
         //�----------------------------------------                            
         //�$LoadSF - Load Subfile                                              
         //�----------------------------------------                            
            BegSr $LoadSF;                                                      
                                                                                
            //�Declare Cursor & Open Cursor                                     
               ExSr $DecCurSor;                                                 
                                                                                
            //�Find all source files and display                                
               Exsr $Display;                                                   
                                                                                
            EndSr;                                                              
                                                                                
         //�----------------------------------------                            
         //�$Display - Display Screen                                           
         //�----------------------------------------                            
            BegSr $Display;                                                     
                                                                                
               ExSr $LoadSubFile;                                               
                                                                                
               Wk_ReSetDsp = *Off;                                              
               DoU *In03 or *In12;                                              
                  If PrvRELRCD > 0;                                             
                     If PrvRELRCD > SFLRRN;                                     
                        RCDNBR = SFLRRN;                                        
                     Else;                                                      
                        RCDNBR = PrvRELRCD;                                     
                     EndIf;                                                     
                     If Wk_ReSetDsp = *On;                                      
                        Wk_ReSetDsp = *Off;                                     
                        PrvRELRCD = 0;                                          
                     EndIf;                                                     
                  EndIf;                                                        
                                                                                
                  Write HEADER;                                                 
                  Write FOOTER;                                                 
                  ExFmt SRCSFLCTL;                                              
                                                                                
                  If *In03 <> *On And *In12 <> *On;                             
                                                                                
                     If SflRRN = 0 And (Not *In06 And Not *In05);               
                        Iter;                                                   
                     EndIf;                                                     
                                                                                
                     ERRMSG = *Blanks;                                          
                                                                                
                  //�When F5 is pressed (Refresh)                               
                     If *In05 = *On;                                            
                        PrvRELRCD = RELRCD;                                     
                        PrvPOS01 = *Blanks;                                     
                        POS01   = *Blanks;                                      
                                                                                
                        ExSr $LoadSF;                                           
                     EndIf;                                                     
                                                                                
                  //�When Position Option is Selected                           
                     If POS01 <> PrvPOS01;                                      
                        PrvPOS01 = POS01;                                       
                        wkCHGPOS = *On;                                         
                     EndIf;                                                     
                                                                                
                     If wkCHGPOS = *On;                                         
                        ExSr $LoadSF;                                           
                        wkCHGPOS = *Off;                                        
                        Iter;                                                   
                     EndIf;                                                     
                                                                                
                  //�When F6 is pressed                                         
                     If *In06 = *On;                                            
                        If *In41 = *Off;                                        
                           ERRMSG = 'Option Not Available!!!';                  
                           Iter;                                                
                        EndIf;                                                  
                                                                                
                        wkMode = 'ADD';                                         
                        wkRecNo = 0;                                            
                        Wk_ErrFlg = *Off;                                       
                        CallP(E) ==PGMMP===(wkMode:wkRecNo:Wk_ErrFlg);          
                                                                                
                        ExSr $LoadSF;                                           
                        Iter;                                                   
                     EndIf;                                                     
                                                                                
                  //�When Option is taken against record                        
                     ReadC SRCSFL;                                              
                     If %EOF Or RELRCD <> 0;                                    
                        PrvRELRCD = RELRCD;                                     
                     ElseIf Not %EOF;                                           
                        PrvRELRCD = SFLRRN;                                     
                     EndIf;                                                     
                                                                                
                     DoW Not %EOF;                                              
                        SOPT = %Trim(SOPT);                                     
                                                                                
                        Select;                                                 
                        When SOPT = *Blanks;                                    
                             //Do Nothing                                       
                    //� When SOPT <> '5' And *In41 = *Off;                      
                    //�      ERRMSG = 'Option Not Available!!!';                
                    //�      Leave;                                             
                        When SOPT = '3';                                        
                           wkMode = 'ADD';                                      
                           wkRecNo = SRecNo;                                    
                           Wk_ErrFlg = *Off;                                    
                           CallP(E) ==PGMMP===(wkMode:wkRecNo:Wk_ErrFlg);       
                                                                                
                           ExSr $LoadSF;                                        
                           Iter;                                                
                        When SOPT = '2' Or SOPT = '5';                          
                           ExSr $EVRecord;                                      
                        When SOPT = '4';                                        
                           wkRecNo = SRecNo;                                    
                           ExSr $DltRecord;                                     
                           ExSr $LoadSf;                                        
                           Wk_ResetDsp = *On;                                   
                           Iter;                                                
                                                                                
                        Other;                                                  
                           ERRMSG = 'Invalid Option!!!';                        
                                                                                
                        EndSl;                                                  
                                                                                
                        SOPT = *Blanks;                                         
                        Update(E) SRCSFL;                                       
                                                                                
                        ReadC SRCSFL;                                           
                                                                                
                     EndDo;                                                     
                                                                                
                  Else;                                                         
                     F3Flg = *In03;                                             
                     ExSr $ExitPgm;                                             
                  EndIf;                                                        
                                                                                
               EndDo;                                                           
                                                                                
            EndSr;                                                              
                                                                                
         //�----------------------------------------                            
         //�$LoadSubFile - Load Subfile                                         
         //�----------------------------------------                            
            BegSr $LoadSubFile;                                                 
                                                                                
               SflClr = *On;                                                    
               Write SRCSFLCTL;                                                 
               SflClr = *Off;                                                   
                                                                                
               SflRRN = 0;                                                      
                                                                                
               ExSr $FetchNxt;                                                  
               DoW SQLCOD = 0 And SflRRN < 9999;                                
                                                                                
                                                                                
                  SOPT = *Blanks;                                               
                  FLDXXXX01 = CFLDXXXX01;                                       
                                                                                
                  SflRRN += 1;                                                  
                  Write SRCSFL;                                                 
                  CLEAR SRCSFL;                                                 
                  ExSr $FetchNxt;                                               
                  SIfChk = *Off;                                                
                                                                                
               EndDo;                                                           
                                                                                
            //�Close Cursor                                                     
               ExSr $CloseCur;                                                  
                                                                                
               If SflRRN > 0;                                                   
                  SflDsp = *On;                                                 
                  SflEnd = *On;                                                 
                  FndFlg = *On;                                                 
                  RCDNBR = 1;                                                   
               Else;                                                            
                  SflDsp = *Off;                                                
                  SflEnd = *Off;                                                
                  FndFlg = *Off;                                                
                  ERRMSG = 'No Record Found!!!';                                
               EndIf;                                                           
                                                                                
            EndSr;                                                              
                                                                                
         //�---------------------------------                                   
         //�$EVRecord - Edit/View Object                                        
         //�---------------------------------                                   
            BegSr $EVRecord;                                                    
                                                                                
               wkRecNo = SRecNo;                                                
                                                                                
               If SOPT = '2';                                                   
                  wkMode = 'UPD';                                               
               Else;                                                            
                  wkMode = 'DSP';                                               
               EndIf;                                                           
                                                                                
               CallP(E) ==PGMMP===(wkMode:wkRecNo:Wk_ErrFlg);                   
                                                                                
            EndSr;                                                              
                                                                                
         //�---------------------------------                                   
         //�$DltRecord - Delete Object                                          
         //�---------------------------------                                   
            BegSr $DltRecord;                                                   
                                                                                
               SCONFRM = 'N';                                                   
               ExFmt CONFIRMW;                                                  
               If SCONFRM = 'Y';                                                
                  wkMode = 'DLT';                                               
                  CallP(E) ==PGMMP===(wkMode:wkRecNo:Wk_ErrFlg);                
               EndIf;                                                           
                                                                                
            EndSr;                                                              
                                                                                
         //�------------------------                                            
         //�$ExitPgm - Exit Program                                             
         //�------------------------                                            
            BegSr $ExitPgm;                                                     
                                                                                
               *InLr = *On;                                                     
               Return;                                                          
                                                                                
            EndSr;                                                              
                                                                                
      /End-Free                                                                 
     �*  //�-------------------------------------                               
     �*  //�$DecCurSor - Declare and Open Cursor                                
     �*  //�-------------------------------------                               
     C     $DecCurSor    BegSr                                                  
                                                                                
     C/EXEC SQL                                                                 
     C+  CLOSE SCUR                                                             
     C/END-EXEC                                                                 
                                                                                
     C/EXEC SQL                                                                 
     C+  DECLARE SCUR     CURSOR FOR                                            
     C+  SELECT A.*                                                             
     C+         A.FLD1,                                                         
     C+        ,RRN(A)                                                          
     C+  FROM SFILENAMEX A                                                      
     C+  WHERE                                                                  
     C+      (:POS01 = ' ' OR A.POSFLDXX01 >= :POS01)                           
     C+  ORDER BY 1                                                             
     C+  FOR FETCH ONLY                                                         
     C/END-EXEC                                                                 
                                                                                
     C/EXEC SQL                                                                 
     C+  OPEN SCUR                                                              
     C/END-EXEC                                                                 
                                                                                
     C                   EndSr                                                  
                                                                                
     �*  //�------------------------------------------                          
     �*  //�$FetchNxt - Fetch next record from cursor                           
     �*  //�------------------------------------------                          
     C     $FetchNxt     BegSr                                                  
                                                                                
     C/EXEC SQL                                                                 
     C+  FETCH SCUR     INTO :CCURDS                                            
     C/END-EXEC                                                                 
                                                                                
     C                   EndSr                                                  
                                                                                
     �*  //�------------------------------------------                          
     �*  //�$CloseCur - Close Cursor                                            
     �*  //�------------------------------------------                          
     C     $CloseCur     BegSr                                                  
                                                                                
     C/EXEC SQL                                                                 
     C+  CLOSE SCUR                                                             
     C/END-EXEC                                                                 
                                                                                
     C                   EndSr                                                  
                                                                                
