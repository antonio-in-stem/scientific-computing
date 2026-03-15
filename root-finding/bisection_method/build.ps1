# 1. Force the script to run from its own directory
$PSScriptRoot = Split-Path -Parent $MyInvocation.MyCommand.Definition
Set-Location $PSScriptRoot

# 2. Define paths
$BIN_DIR = "bin"
$SRC_DIR = "src"
$OUT_DIR = "out"
$EXE_NAME = "biseccion.exe"
$EXE_PATH = Join-Path $PSScriptRoot "$BIN_DIR\$EXE_NAME"

# 3. Create folders if they don't exist
if (!(Test-Path $BIN_DIR)) { New-Item -ItemType Directory -Path $BIN_DIR }
if (!(Test-Path $OUT_DIR)) { New-Item -ItemType Directory -Path $OUT_DIR }

Write-Host "--- Compiling Fortran Motor ---" -ForegroundColor Cyan

# 4. Compile all source files in src/
gfortran "$PSScriptRoot\$SRC_DIR\*.f95" -o "$EXE_PATH"

# 5. Check if compilation was successful
if ($?) {
    Write-Host " [OK] Compilation successful." -ForegroundColor Green
    
    # Clean up module files in src to keep it tidy
    Get-ChildItem -Path "$PSScriptRoot\$SRC_DIR" -Filter *.mod | Remove-Item
    
    Write-Host "--- Launching Execution ---" -ForegroundColor Yellow
    
    # 6. Execute using the ampersand operator to handle variables correctly
    & "$EXE_PATH"
} else {
    Write-Host " [ERROR] Compilation failed." -ForegroundColor Red
}