@echo off
REM run_debug.bat - Run the RBiblioSynth debug and verification
REM Run this from the project root directory

echo ============================================
echo RBiblioSynth Debug and Verification Script
echo ============================================
echo.

REM Check if R is available
where Rscript >nul 2>&1
if %ERRORLEVEL% neq 0 (
    echo ERROR: Rscript not found in PATH
    echo Please install R and add it to your PATH, or run from RStudio
    echo.
    echo Alternative: Open RStudio and run:
    echo   source("debug_m1.R")
    echo.
    pause
    exit /b 1
)

echo Running debug_m1.R...
echo.

Rscript debug_m1.R

echo.
echo ============================================
echo Debug complete. Check output above for errors.
echo ============================================
pause