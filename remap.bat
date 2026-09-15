@echo off
if "%2"=="" (
  echo. Call with: %0 drive: destination-direcory
  exit/b
)
if not exist "%2\." (
  echo. The destination directory does not exist
  exit/b
)
SET drive=%1\\
SET dest=%2
SET dest=%dest:\=\\%
subst | findstr /I /R /C:"^%drive%: => %dest%$" 1> nul
if errorlevel 1 (
  echo. %1-drive not mapped to %2
  echo. remapping %1
  subst %1 /D 1> nul
  subst %1 %2
) ELSE (
  echo. %1-drive already mapped to %2
)
