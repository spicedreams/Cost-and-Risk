Option Explicit

Dim objShell, objFSO, strCurrentDir, strDriveLetter
Dim strAppRoot, strRScript, strAppDir, strDataDir, strCommand
Dim strVirtualDrive, strRPortableSource, strAppSource

Set objShell = CreateObject("WScript.Shell")
Set objFSO = CreateObject("Scripting.FileSystemObject")

' ==============================================================================
' 1. CONFIGURATION SECTION
' Easily adjust the drive letter and path substitutions here
' ==============================================================================
strVirtualDrive    = "R:"
strRPortableSource = "C:\Users\ghar115\professional\risk\cost-risk\R\portable-r-4.6.1-win-x64"
strAppSource       = "C:\Users\ghar115\professional\risk\cost-risk"
' ==============================================================================

' 2. Determine where this script is currently being executed from
strCurrentDir = objFSO.GetParentFolderName(WScript.ScriptFullName)
strDriveLetter = UCase(objFSO.GetDriveName(strCurrentDir))

' 3. Scenario Handling: Did the user copy this to their local PC (e.g., C: drive)?
' (Commented out for local testing since test paths reside on C:)
' If strDriveLetter = "C:" Then
'     MsgBox "It looks like you copied this launcher to your local computer (e.g., Desktop)." & vbCrLf & vbCrLf & _
'            "To run the Forecaster, this script must remain in the shared project folder. " & _
'            "If you want a shortcut on your Desktop, right-click this file in the network drive, " & _
'            "select 'Send to' -> 'Desktop (create shortcut)'.", _
'            vbCritical, "Invalid Execution Location"
'     WScript.Quit
' End If

' 4. Construct the Drive Mapping at Run Time
' Remove the subst mapping if it already exists (prevents errors on subsequent runs)
objShell.Run "cmd.exe /c subst " & strVirtualDrive & " /D", 1, True
' ,0,True means hide the window & any errors, ,1,True means show it
' Create the subst mapping (wait on return = True so mapping finishes before continuing)
objShell.Run "cmd.exe /c subst " & strVirtualDrive & " " & Chr(34) & strRPortableSource & Chr(34), 1, True

' Define the target App directory path
strAppDir = strVirtualDrive & "\App"

' Create directory junction for App (mklink /J)
' Check if the folder already exists to prevent mklink from throwing an error
If Not objFSO.FolderExists(strAppDir) Then
    objShell.Run "cmd.exe /c mklink /J " & Chr(34) & strAppDir & Chr(34) & " " & Chr(34) & strAppSource & Chr(34), 0, True
End If

' 5. Application Paths
strAppRoot = strVirtualDrive
strRScript = Chr(34) & strAppRoot & "\bin\Rscript.exe" & Chr(34)

' 6. Relative Read/Write Data Path (Constantly-named subdirectory)
strDataDir = strCurrentDir & "\Forecaster_Data"

' 7. Construct the execution command
' This sets an R environment variable (PROJECT_DATA_DIR) so Shiny knows where to look for/create the DB
strCommand = strRScript & " -e ""Sys.setenv(PROJECT_DATA_DIR='" & Replace(strDataDir, "\", "/") & "'); " & _
             "shiny::runApp('" & Replace(strAppDir, "\", "/") & "', launch.browser=TRUE)"""

' 8. Execute silently (0 = hide window, False = don't wait for completion)
' objShell.Run strCommand, 0, False
objShell.Run "cmd.exe /k """ & strCommand & """", 1, True

Set objShell = Nothing
Set objFSO = Nothing
