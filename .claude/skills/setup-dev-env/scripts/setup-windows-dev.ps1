# SUEWS Windows Development Environment Setup
# Run in PowerShell (as Administrator recommended for MSYS2)
#
# Usage:
#   .\setup-windows-dev.ps1              # Full setup
#   .\setup-windows-dev.ps1 -SkipMSYS2   # Skip MSYS2 if already installed
#   .\setup-windows-dev.ps1 -SkipQGIS    # Skip QGIS prompt

param(
    [switch]$SkipMSYS2,
    [switch]$SkipQGIS
)

$ErrorActionPreference = "Continue"

Write-Host "========================================" -ForegroundColor Cyan
Write-Host "SUEWS Windows Dev Environment Setup" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

# Helper functions
function Test-CommandExists {
    param($Command)
    $null -ne (Get-Command $Command -ErrorAction SilentlyContinue)
}

function Add-ToUserPath {
    param($PathToAdd)
    $currentPath = [Environment]::GetEnvironmentVariable("PATH", "User")
    if ($currentPath -notlike "*$PathToAdd*") {
        [Environment]::SetEnvironmentVariable("PATH", "$currentPath;$PathToAdd", "User")
        $env:PATH = "$env:PATH;$PathToAdd"
        Write-Host "  Added to PATH: $PathToAdd" -ForegroundColor Green
    }
}

# 1. PowerShell Execution Policy
Write-Host "[1/7] Checking PowerShell execution policy..." -ForegroundColor Yellow
$policy = Get-ExecutionPolicy -Scope CurrentUser
if ($policy -eq "Restricted") {
    try {
        Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser -Force -ErrorAction SilentlyContinue
        Write-Host "  Set to RemoteSigned" -ForegroundColor Green
    } catch {
        Write-Host "  Could not change (may need admin). Current: $policy" -ForegroundColor Yellow
    }
} else {
    Write-Host "  OK ($policy)" -ForegroundColor Green
}

# 2. Git
Write-Host "[2/7] Checking Git..." -ForegroundColor Yellow
if (Test-CommandExists "git") {
    $gitVer = git --version
    Write-Host "  Installed: $gitVer" -ForegroundColor Green
} else {
    Write-Host "  Installing Git..." -ForegroundColor Gray
    winget install Git.Git --accept-package-agreements --accept-source-agreements --silent
    $env:PATH = "$env:PATH;C:\Program Files\Git\cmd"
    Write-Host "  Git installed" -ForegroundColor Green
}

# 3. GitHub CLI
Write-Host "[3/7] Checking GitHub CLI..." -ForegroundColor Yellow
if (Test-CommandExists "gh") {
    $ghVer = (gh --version | Select-Object -First 1)
    Write-Host "  Installed: $ghVer" -ForegroundColor Green
} else {
    Write-Host "  Installing gh CLI..." -ForegroundColor Gray
    winget install GitHub.cli --accept-package-agreements --accept-source-agreements --silent
    Write-Host "  gh installed - run 'gh auth login' to authenticate" -ForegroundColor Yellow
}

# 4. Python
Write-Host "[4/7] Checking Python..." -ForegroundColor Yellow
if (Test-CommandExists "python") {
    $pyVer = python --version 2>&1
    Write-Host "  Installed: $pyVer" -ForegroundColor Green
} else {
    Write-Host "  Installing Python 3.12..." -ForegroundColor Gray
    winget install Python.Python.3.12 --accept-package-agreements --accept-source-agreements --silent
    Write-Host "  Python 3.12 installed" -ForegroundColor Green
}

# 5. uv (Python package manager)
Write-Host "[5/7] Checking uv..." -ForegroundColor Yellow
if (Test-CommandExists "uv") {
    $uvVer = uv --version
    Write-Host "  Installed: $uvVer" -ForegroundColor Green
} else {
    Write-Host "  Installing uv..." -ForegroundColor Gray
    winget install astral-sh.uv --accept-package-agreements --accept-source-agreements --silent
    Write-Host "  uv installed" -ForegroundColor Green
}

# 6. MSYS2 + Fortran toolchain
Write-Host "[6/7] Checking MSYS2 + gfortran..." -ForegroundColor Yellow
$msys2Path = "C:\msys64"
if ($SkipMSYS2) {
    Write-Host "  Skipped (--SkipMSYS2)" -ForegroundColor Gray
} elseif (Test-Path "$msys2Path\ucrt64\bin\gfortran.exe") {
    Write-Host "  Installed at $msys2Path" -ForegroundColor Green
    Add-ToUserPath "$msys2Path\ucrt64\bin"
} else {
    Write-Host "  MSYS2 not found. Installing..." -ForegroundColor Gray

    if (-not (Test-Path $msys2Path)) {
        Write-Host "  Downloading MSYS2..." -ForegroundColor Gray
        $msys2Installer = "$env:TEMP\msys2-installer.exe"
        Invoke-WebRequest -Uri "https://github.com/msys2/msys2-installer/releases/download/2024-01-13/msys2-x86_64-20240113.exe" -OutFile $msys2Installer
        Write-Host "  Installing MSYS2 (this takes a few minutes)..." -ForegroundColor Gray
        Start-Process -FilePath $msys2Installer -ArgumentList "install --root $msys2Path --confirm-command" -Wait -NoNewWindow
    }

    Write-Host "  Installing Fortran toolchain via pacman..." -ForegroundColor Gray
    & "$msys2Path\usr\bin\bash.exe" -lc "pacman -Syu --noconfirm"
    & "$msys2Path\usr\bin\bash.exe" -lc "pacman -S --noconfirm mingw-w64-ucrt-x86_64-gcc-fortran mingw-w64-ucrt-x86_64-meson mingw-w64-ucrt-x86_64-ninja"

    Add-ToUserPath "$msys2Path\ucrt64\bin"
    Write-Host "  MSYS2 + gfortran installed" -ForegroundColor Green
}

# 7. VS Code (optional)
Write-Host "[7/7] Checking VS Code..." -ForegroundColor Yellow
if (Test-CommandExists "code") {
    Write-Host "  Installed" -ForegroundColor Green
} else {
    Write-Host "  Installing VS Code..." -ForegroundColor Gray
    winget install Microsoft.VisualStudioCode --accept-package-agreements --accept-source-agreements --silent
    Write-Host "  VS Code installed" -ForegroundColor Green
}

# QGIS (optional, for plugin testing)
if (-not $SkipQGIS) {
    Write-Host ""
    Write-Host "[Optional] QGIS LTR (for UMEP plugin testing)..." -ForegroundColor Yellow
    $qgisPath = "C:\Program Files\QGIS*"
    if (Test-Path $qgisPath) {
        Write-Host "  QGIS already installed" -ForegroundColor Green
    } else {
        Write-Host "  QGIS requires manual installation:" -ForegroundColor Yellow
        Write-Host "    1. Download: https://download.osgeo.org/osgeo4w/v2/osgeo4w-setup.exe" -ForegroundColor Gray
        Write-Host "    2. Run Express Install -> QGIS LTR" -ForegroundColor Gray
    }
}

Write-Host ""
Write-Host "========================================" -ForegroundColor Cyan
Write-Host "Setup Complete!" -ForegroundColor Green
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Next steps:" -ForegroundColor Yellow
Write-Host "  1. Close and reopen terminal (for PATH changes)" -ForegroundColor White
Write-Host "  2. gh auth login (authenticate with GitHub)" -ForegroundColor White
Write-Host "  3. Clone SUEWS:" -ForegroundColor White
Write-Host "       git clone https://github.com/UMEP-dev/SUEWS.git" -ForegroundColor Gray
Write-Host "  4. Build SUEWS:" -ForegroundColor White
Write-Host "       cd SUEWS" -ForegroundColor Gray
Write-Host "       uv venv" -ForegroundColor Gray
Write-Host "       .venv\Scripts\activate" -ForegroundColor Gray
Write-Host "       uv pip install -e .[dev]" -ForegroundColor Gray
Write-Host "       make test" -ForegroundColor Gray
Write-Host ""
