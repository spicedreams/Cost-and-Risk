@echo off
:: Install RStudio from Software Center- it goes into "C:\Program Files\R"
:: and is more likely to be safe from Windows Defender
c:
cd \Users\ghar115\professional\risk\cost-risk
:: Find the drive letter this script is currently running from
set "SCRIPT_DIR=%~dp0"
echo "Loading script from "%SCRIPT_DIR%

:: Launch the portable R executable and pass your main script
"C:\Program Files\R\R-4.6.1\bin\x64\\RScript.exe" "%SCRIPT_DIR%\cost-risk-mc.R"
:: pause

:: need to exclude from Windows Defender like
:: Windows Settings; Privacy & security; open Windows Security; Virus & threat protection; Virus & threat protection settings; manage settings; Exclusions; Add or remove exclusions

