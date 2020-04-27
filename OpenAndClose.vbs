'   a .vbs file is just a text file containing visual basic code that has the extension renamed from .txt  to .vbs

'Write Excel.xls  Sheet's full path here
strPath = "C:\Users\CONNO\OneDrive\Documents\GitHub\COVID\USCOVID_TimeSeries.xlsx" 

'Create an Excel instance and set visibility of the instance
Set objApp = CreateObject("Excel.Application") 
objApp.Visible = True 

'Open workbook; Run Macro; Save Workbook with changes; Close; Quit Excel
Set wbToRun = objApp.Workbooks.Open(strPath) 

wbToRun.Save 
wbToRun.Close 
objApp.Quit 

'Leaves an onscreen message!
MsgBox strPath & " vbs successfully completed!",         vbInformation 
'