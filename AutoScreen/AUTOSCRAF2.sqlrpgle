     �*************************************************************************
      * (C) COPYRIGHT 2017, ASHISH                                        �    
      * ALL RIGHTS RESERVED                                                 �  
      *                                                                     �  
      *     * * * * *  C I T I  C O N F I D E N T I A L  * * * * *          �  
      * THIS PROGRAM IS AN UNPUBLISHED WORK FULLY PROTECTED BY THE UNITED   �  
      * STATES COPYRIGHT LAWS AND IS CONSIDERED A TRADE SECRET BELONGING TO �  
      * THE COPYRIGHT HOLDER.                                               �  
     �*************************************************************************
      * �                                                                 � � *
      * ===================================================================== *
      *� Program Name     �:�==PROGRAMMP==                                � ��*
      *� Author           �: ==PROG=======                                � ��*
      *� Date             �:�==DATE=======                                � ��*
      *� Project          �: ==PROJECTNM==                                � ��*
      *� Work list        �:�==PROJECT====                                � ��*
      *� Description      �:�==DESC=======                                � ��*
      *�                                                                  � ��*
      * ===================================================================== *
      *�                       Change History                             � ��*
      * =====================================================================�*
      *� Author           �:�                                             � ��*
      *� Date             �:�                                             � ��*
      *� Project Reference�:�                                             � ��*
      *� Work list        �:�                                             � ��*
      *� Description      �:�                                             � ��*
      *� Modification tag �:�                                             � ��*
      * =====================================================================�*
      *                                                                        
      *************************************************************************
     H Debug Option(*NoDebugIO)                                                
                                                                               
     �*�File Specification                                                     
     F==PGMMD$$$CF   E             WORKSTN                                     
     F==FILE$$$$IF   E             DISK                                        
                                                                               
     �*�Variable Declaration                                                   
     DVarDs1           DS                                                      
     D Wk_Command                   300    Inz(*Blanks)                        
     D WkScrName                     50    Inz(*Blanks)                        
     D WkStartPos                     2  0 Inz(*Zeros)                         
     D inValidScrNo                   2  0 Inz(*Zeros)                         
     D wCurScrNo                      2  0 Inz(*Zeros)                         
     D wScrRecNo                      2  0 Inz(*Zeros)                         
     D wRecNo                        10  0 Inz(*Zeros)                         
     D Wk_CmdLen                     15P 5 Inz(*Zeros)                         
     D Wk_TStamp                       Z                                       
                                                                               
     �*�Indicators                                                             
     D Wk_SuccessFlg                   N   Inz(*Off)                           
     D FndFlg                          N   Inz(*Off)                           
     D wDispOnlyFlg                    N   Inz(*Off)                           
     D wValidFlg                       N   Inz(*Off)                           
     D wAllValid                       N   Inz(*Off)                           
     D InKyPtr         S               *   Inz(%ADDR(*IN))                     
                                                                               
     D PgmInd          DS            99    Based(InKyPtr)                      
     D wAddUpd                         N   Overlay(PgmInd: 17)                 
     D wPrvScr                         N   Overlay(PgmInd: 19)                 
     D wNxtScr                         N   Overlay(PgmInd: 20)                 
     D wDspOnly                        N   Overlay(PgmInd: 31)                 
                                                                               
     �*�Constants                                                              
     DUCase            C                   CONST('ABCDEFGHIJKLMNOPQRSTUVWXYZ') 
     DLCase            C                   CONST('abcdefghijklmnopqrstuvwxyz') 
                                                                               
     �*�Data Structures                                                        
     D FileDS        E DS                  EXTNAME(==FILE==     )              
                                                                               
     �*�Program Status Data Structure                                          
     D PSDS           SDS                                                      
     D USER                  254    263                                        
                                                                               
     �*�Prototypes                                                             
     D ==PGMMP$$$      PR                  EXTPGM('==PGMMP===')                
     D Mode                           3                                        
     D RecNo                         10  0                                     
     D ErrorFlg                        N                                       
                                                                               
                                                                               
     D ==PGMMP$$$      PI                                                      
     D inMode                         3                                        
     D inRecNo                       10  0                                     
     D ErrFlg                          N                                       
                                                                               
      /Free                                                                    
                                                                               
         //�----------------------------------------------------------         
         //�                      Program Starts                     -         
         //�----------------------------------------------------------         
                                                                               
         //�Set SQL Options                                                    
            Exec SQL                                                           
               SET OPTION COMMIT = *NONE, CLOSQLCSR = *ENDMOD;                 
                                                                               
            EXEC SQL                                                           
              WHENEVER SQLERROR CONTINUE;                                      
                                                                               
         //�Set Total Screens                                                  
            wScrRecNo = TS=                                                    
                                                                               
         // Screen Name                                                        
            wkScrName = ==SCREENNM===                                          
            wkStartPos = (50 - %Len(%Trim(wkScrName))) / 2;                    
            If wkStartPos < 1;                                                 
              wkStartPos = 1;                                                  
            EndIf;                                                             
            %SubSt(inScrn:wkStartPos) = %Trim(wkScrName);                      
                                                                               
         //�Set Input RRN Number                                               
            If %DEC(inRecNo) < 0;                                              
              wRecNo = 1;                                                      
            Else;                                                              
              wRecNo = inRecNo;                                                
            EndIf;                                                             
                                                                               
         //�Set Display Parameters                                             
            ExSr $SetDspParm;                                                  
                                                                               
         //�Process Display Screens                                            
            ExSr $ProcessDisplay;                                              
                                                                               
         //�Exit Program                                                       
            ExSr $ExitPgm;                                                     
                                                                               
                                                                               
         //�----------------------------------------------------------         
         //�                        Subroutines                      -         
         //�----------------------------------------------------------         
                                                                               
         //�----------------------------------------                           
         //�$SetDspParm - Set Display Parameters                               
         //�----------------------------------------                           
            BegSr $SetDspParm;                                                 
                                                                               
           //�Clear Fields                                                     
              Clear FileDS;                                                    
                                                                               
           //�Set Display to First Screen                                      
              If wCurScrNo = 0;                                                
                wCurScrNo = 1;                                                 
              EndIf;                                                           
                                                                               
           //�Set Display Only (Protected Fields) to Off                       
              wDispOnlyFlg = *Off;                                             
                                                                               
              If inMode = 'ADD';                                               
                If wRecNo > 0;                                                 
                  ExSr $Fetch;                                                 
                EndIf;                                                         
                wAllValid = *Off;                                              
              ElseIf inMode = 'DLT' Or inMode = 'DSP' Or inMode = 'UPD';       
             //�Fetch record from file using RRN passed                        
                ExSr $Fetch;                                                   
                wDispOnlyFlg = *On;                                            
                wAllValid = *On;                                               
              EndIf;                                                           
                                                                               
           //�Fetch record from file using RRN passed                          
              If inMode = 'DLT' Or inMode = 'DSP';                             
                wDspOnly = *On;                                                
              EndIf;                                                           
                                                                               
           //�In case of Display, don't show Confirm Function                  
              If inMode = 'DSP';                                               
                *In41 = *On;                                                   
              EndIf;                                                           
                                                                               
            EndSr;                                                             
                                                                               
         //�------------------------------------------                         
         //�$ProcessDisplay - Process Display Screens                          
         //�------------------------------------------                         
            BegSr $ProcessDisplay;                                             
                                                                               
              DoU *In12 Or *In03;                                              
                                                                               
             //�Choose which screen to display                                 
                ExSr $ShowScreen;                                              
                                                                               
                If *In12 = *Off And *In03 = *Off;                              
                                                                               
                  ERRMSG = *Blanks;                                            
                                                                               
                  Select;                                                      
                                                                               
                  When InMode = 'UPD' Or InMode = 'ADD';                       
                                                                               
                 //�Validate screen fields                                     
                    ExSr $ValidateScr;                                         
                                                                               
                    If wValidFlg = *Off;                                       
                      inValidScrNo = wCurScrNo;                                
                    Else;                                                      
                                                                               
                      If wAddUpd = *On And wAllValid = *On;                    
                        If inMode = 'ADD';                                     
                       //�Add record to file                                   
                          ExSr $Add;                                           
                          Clear FileDS;                                        
                          ErrMsg = 'Record Added Successfully!';               
                          wCurScrNo = 1;                                       
                        Else;                                                  
                       //�Update record                                        
                          ExSr $Update;                                        
                          ErrMsg = 'Record Updated Successfully!';             
                          wCurScrNo = 1;                                       
                        EndIf;                                                 
                      EndIf;                                                   
                                                                               
                    EndIf;                                                     
                                                                               
                  When InMode = 'DSP' Or InMode = 'DLT';                       
                                                                               
                 //�Delete record                                              
                    If InMode = 'DLT' And wAddUpd = *On;                       
                      ExSr $Delete;                                            
                      ExSr $ExitPgm;                                           
                    EndIf;                                                     
                                                                               
                 //�Choose screen                                              
                    If wPrvScr = *On And wCurScrNo > 1;                        
                      wCurScrNo -= 1;                                          
                    ElseIf wCurScrNo < wScrRecNo And wPrvScr = *Off;           
                      wCurScrNo +=1;                                           
                    EndIf;                                                     
                                                                               
                  EndSl;                                                       
                                                                               
                Else;                                                          
                  ExSr $ExitPgm;                                               
                EndIf;                                                         
                                                                               
              EndDo;                                                           
                                                                               
            EndSr;                                                             
                                                                               
         //�----------------------------------                                 
         //�$ShowScreen - Show Screen                                          
         //�----------------------------------                                 
            BegSr $ShowScreen;                                                 
                                                                               
           //�If First Screen Don't show Previous                              
              If wCurScrNo = 1;                                                
                *In51 = *On;                                                   
              Else;                                                            
                *In51 = *Off;                                                  
              EndIf;                                                           
                                                                               
           //�If Last Screen Don't show Next                                   
              If wCurScrNo = wScrRecNo;                                        
                *In52 = *On;                                                   
              Else;                                                            
                *In52 = *Off;                                                  
              EndIf;                                                           
                                                                               
              Write HEADER;                                                    
              Write FOOTER;                                                    
                                                                               
              Select;                                                          
                                                                               
                When wCurScrNo = CS=;                                          
                  ExFmt DspRecCS=;                                             
                                                                               
              EndSl;                                                           
                                                                               
            EndSr;                                                             
                                                                               
         //�----------------------------------                                 
         //�$ValidateScr - Validate Screens                                    
         //�----------------------------------                                 
            BegSr $ValidateScr;                                                
                                                                               
              wValidFlg = *Off;                                                
                                                                               
           //�Select Validate Subroutine according to screen                   
              Select;                                                          
                                                                               
                When wCurScrNo = CS=;                                          
                  ExSr $ValidateCS=;                                           
                                                                               
              EndSl;                                                           
                                                                               
           //�Check if Valid                                                   
              If wValidFlg = *On;                                              
                                                                               
                If wPrvScr = *On And wCurScrNo > 1;                            
                  wCurScrNo -= 1;                                              
                ElseIf wCurScrNo < wScrRecNo And wPrvScr = *Off;               
                  wCurScrNo +=1;                                               
                ElseIf wCurScrNo = wScrRecNo And inMode = 'ADD';               
                  wAllValid = *On;                                             
                EndIf;                                                         
                                                                               
              Else;                                                            
                  wAllValid = *Off;                                            
              EndIf;                                                           
                                                                               
            EndSr;                                                             
                                                                               
         //�---------------------------------                                  
         //�$Fetch - Fetch Record                                              
         //�---------------------------------                                  
            BegSr $Fetch;                                                      
                                                                               
               Exec SQL                                                        
                  SELECT * INTO :FileDS                                        
                  FROM ==FILE==   A                                            
                  WHERE RRN(A) = :wRecNo;                                      
                                                                               
               If SQLCOD < 0 or SQLCOD = 100;                                  
                 ERRMSG  = 'Record not found';                                 
               EndIf;                                                          
                                                                               
            EndSr;                                                             
                                                                               
         //�---------------------------------                                  
         //�$ValidateCS= - Validate Screen CS=                                 
         //�---------------------------------                                  
            BegSr $ValidateCS=;                                                
                                                                               
               wValidFlg = *On;                                                
               ERRMSG  = *Blanks;                                              
                                                                               
            EndSr;                                                             
                                                                               
         //�---------------------------------                                  
         //�$Add - Add Data                                                    
         //�---------------------------------                                  
            BegSr $Add;                                                        
                                                                               
              Exec SQL                                                         
                 INSERT INTO ==FILE==                                          
                 VALUES :FileDS;                                               
                                                                               
              If SQLCOD < 0;                                                   
                ERRMSG = 'Error Occured while adding data';                    
              Else;                                                            
                ERRMSG = *Blanks;                                              
              EndIf;                                                           
                                                                               
            EndSr;                                                             
                                                                               
         //�---------------------------------                                  
         //�$Update - Update Data                                              
         //�---------------------------------                                  
            BegSr $Update;                                                     
                                                                               
              Exec SQL                                                         
                 UPDATE ==FILE==   A                                           
                 SET ROW = :FileDS                                             
                 WHERE RRN(A) = :wRecNo;                                       
                                                                               
              If SQLCOD < 0;                                                   
                ERRMSG = 'Error Occured while updating data';                  
              Else;                                                            
                ERRMSG = *Blanks;                                              
              EndIf;                                                           
                                                                               
            EndSr;                                                             
                                                                               
         //�---------------------------------                                  
         //�$Delete - Delete Data                                              
         //�---------------------------------                                  
            BegSr $Delete;                                                     
                                                                               
              Exec SQL                                                         
                 DELETE FROM ==FILE==   A                                      
                 WHERE RRN(A) = :wRecNo;                                       
                                                                               
              If SQLCOD < 0;                                                   
                ERRMSG = 'Error Occured while deleteting data';                
              Else;                                                            
                ERRMSG = *Blanks;                                              
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
                                                                               
