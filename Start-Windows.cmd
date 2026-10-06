@echo off
cd /d "%~dp0"
powershell.exe -NoProfile -ExecutionPolicy Bypass -File "%~dp0scripts\start_windows.ps1" %*
set "survcast_exit=%errorlevel%"
if not "%survcast_exit%"=="0" pause
exit /b %survcast_exit%
