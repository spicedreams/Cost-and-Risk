@echo off
:: Find the drive letter this script is currently running from
set "SCRIPT_DIR=%~dp0"

:: Launch the portable R executable and pass your main script
"%SCRIPT_DIR%\bin\x64\Rscript.exe" "%SCRIPT_DIR%\App\cost-risk-mc.R"

pause

:: C:\Users\ghar115\professional\risk\cost-risk>R\portable-r-4.6.1-win-x64\bin\RScript.exe cost-risk-mc.R