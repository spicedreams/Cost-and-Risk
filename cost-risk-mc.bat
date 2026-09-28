@echo off
:: Install RStudio from Software Center- it goes into "C:\Program Files\R"
:: and is more likely to be safe from Windows Defender
:: May need to exclude from Windows Defender like
:: Windows Settings; Privacy & security; open Windows Security; Virus & threat protection; Virus & threat protection settings; manage settings; Exclusions; Add or remove exclusions

c:
cd \Users\ghar115\professional\risk\cost-risk
:: Find the drive letter this script is currently running from
set "SCRIPT_DIR=%~dp0"
echo Loading %SCRIPT_DIR%\cost-risk-mc

:: Launch the portable R executable and pass your main script
"C:\Program Files\R\R-4.6.1\bin\x64\RScript.exe" -e "shiny::runApp(port = 3296, host = '127.0.0.1', launch.browser = TRUE)"

