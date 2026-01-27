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

$ErrorActionPreference = "Stop"

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
    $pathArray = $currentPath -split ";" | Where-Object { $_ -ne "" }
    if ($PathToAdd -notin $pathArray) {
        [Environment]::SetEnvironmentVariable("PATH", "$currentPath;$PathToAdd", "User")
        $env:PATH = "$env:PATH;$PathToAdd"
        Write-Host "  Added to PATH: $PathToAdd" -ForegroundColor Green
    }
}

$wingetAvailable = Test-CommandExists "winget"
if (-not $wingetAvailable) {
    Write-Host "winget is not available; installs that rely on winget will be skipped." -ForegroundColor Yellow
    Write-Host "Install App Installer from Microsoft Store, or use manual downloads:" -ForegroundColor Yellow
    Write-Host "  Git: https://git-scm.com/download/win" -ForegroundColor Gray
    Write-Host "  GitHub CLI: https://cli.github.com/" -ForegroundColor Gray
    Write-Host "  Python: https://www.python.org/downloads/windows/" -ForegroundColor Gray
    Write-Host "  uv: https://docs.astral.sh/uv/" -ForegroundColor Gray
    Write-Host "  VS Code: https://code.visualstudio.com/Download" -ForegroundColor Gray
    Write-Host ""
}

# 1. PowerShell Execution Policy
Write-Host "[1/7] Checking PowerShell execution policy..." -ForegroundColor Yellow
$policy = Get-ExecutionPolicy -Scope CurrentUser
if ($policy -eq "Restricted") {
    try {
        Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser -Force -ErrorAction Stop
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
    if (-not $wingetAvailable) {
        Write-Host "  winget not available; please install Git manually (see URLs above)." -ForegroundColor Yellow
    } else {
        Write-Host "  Installing Git..." -ForegroundColor Gray
        try {
            & winget install Git.Git --accept-package-agreements --accept-source-agreements --silent
            if ($LASTEXITCODE -ne 0) {
                throw "winget exit code $LASTEXITCODE"
            }
            $env:PATH = "$env:PATH;C:\Program Files\Git\cmd"
            Write-Host "  Git installed" -ForegroundColor Green
        } catch {
            Write-Host "  Git install failed: $_" -ForegroundColor Red
            Write-Host "  Install manually: https://git-scm.com/download/win" -ForegroundColor Yellow
        }
    }
}

# 3. GitHub CLI
Write-Host "[3/7] Checking GitHub CLI..." -ForegroundColor Yellow
if (Test-CommandExists "gh") {
    $ghVer = (gh --version | Select-Object -First 1)
    Write-Host "  Installed: $ghVer" -ForegroundColor Green
} else {
    if (-not $wingetAvailable) {
        Write-Host "  winget not available; please install GitHub CLI manually (see URLs above)." -ForegroundColor Yellow
    } else {
        Write-Host "  Installing gh CLI..." -ForegroundColor Gray
        try {
            & winget install GitHub.cli --accept-package-agreements --accept-source-agreements --silent
            if ($LASTEXITCODE -ne 0) {
                throw "winget exit code $LASTEXITCODE"
            }
            Write-Host "  gh installed - run 'gh auth login' to authenticate" -ForegroundColor Yellow
        } catch {
            Write-Host "  GitHub CLI install failed: $_" -ForegroundColor Red
            Write-Host "  Install manually: https://cli.github.com/" -ForegroundColor Yellow
        }
    }
}

# 4. Python
Write-Host "[4/7] Checking Python..." -ForegroundColor Yellow
if (Test-CommandExists "python") {
    $pyVer = python --version 2>&1
    Write-Host "  Installed: $pyVer" -ForegroundColor Green
} else {
    if (-not $wingetAvailable) {
        Write-Host "  winget not available; please install Python manually (see URLs above)." -ForegroundColor Yellow
    } else {
        Write-Host "  Installing Python 3.12..." -ForegroundColor Gray
        try {
            & winget install Python.Python.3.12 --accept-package-agreements --accept-source-agreements --silent
            if ($LASTEXITCODE -ne 0) {
                throw "winget exit code $LASTEXITCODE"
            }
            Write-Host "  Python 3.12 installed" -ForegroundColor Green
        } catch {
            Write-Host "  Python install failed: $_" -ForegroundColor Red
            Write-Host "  Install manually: https://www.python.org/downloads/windows/" -ForegroundColor Yellow
        }
    }
}

# 5. uv (Python package manager)
Write-Host "[5/7] Checking uv..." -ForegroundColor Yellow
if (Test-CommandExists "uv") {
    $uvVer = uv --version
    Write-Host "  Installed: $uvVer" -ForegroundColor Green
} else {
    if (-not $wingetAvailable) {
        Write-Host "  winget not available; please install uv manually (see URLs above)." -ForegroundColor Yellow
    } else {
        Write-Host "  Installing uv..." -ForegroundColor Gray
        try {
            & winget install astral-sh.uv --accept-package-agreements --accept-source-agreements --silent
            if ($LASTEXITCODE -ne 0) {
                throw "winget exit code $LASTEXITCODE"
            }
            Write-Host "  uv installed" -ForegroundColor Green
        } catch {
            Write-Host "  uv install failed: $_" -ForegroundColor Red
            Write-Host "  Install manually: https://docs.astral.sh/uv/" -ForegroundColor Yellow
        }
    }
}

# 6. MSYS2 + Fortran toolchain
Write-Host "[6/7] Checking MSYS2 + gfortran..." -ForegroundColor Yellow
$msys2Path = "C:\msys64"
if ($SkipMSYS2) {
    Write-Host "  Skipped (-SkipMSYS2)" -ForegroundColor Gray
} elseif (Test-Path "$msys2Path\ucrt64\bin\gfortran.exe") {
    Write-Host "  Installed at $msys2Path" -ForegroundColor Green
    Add-ToUserPath "$msys2Path\ucrt64\bin"
} else {
    Write-Host "  MSYS2 not found. Installing..." -ForegroundColor Gray

    if (-not (Test-Path $msys2Path)) {
        Write-Host "  Downloading MSYS2..." -ForegroundColor Gray
        $msys2Installer = "$env:TEMP\msys2-installer.exe"
        # Pinned fallback URL (last verified: 2024-01-13). Update periodically.
        $msys2InstallerUrl = "https://github.com/msys2/msys2-installer/releases/download/2024-01-13/msys2-x86_64-20240113.exe"
        $usingFallback = $true
        try {
            $release = Invoke-RestMethod -Uri "https://api.github.com/repos/msys2/msys2-installer/releases/latest" -Headers @{ "User-Agent" = "SUEWS-setup-script" }
            $asset = $release.assets | Where-Object { $_.name -match "^msys2-x86_64-.*\.exe$" } | Select-Object -First 1
            if ($null -ne $asset) {
                $msys2InstallerUrl = $asset.browser_download_url
                $usingFallback = $false
            }
        } catch {
            Write-Host "  Could not query latest MSYS2 release; using pinned fallback installer." -ForegroundColor Yellow
            Write-Host "  Note: Fallback may be outdated. Check https://www.msys2.org/ for latest version." -ForegroundColor Yellow
        }
        try {
            Invoke-WebRequest -Uri $msys2InstallerUrl -OutFile $msys2Installer -ErrorAction Stop
        } catch {
            Write-Host "  Download failed: $_" -ForegroundColor Red
            Write-Host "  Please download MSYS2 manually from https://www.msys2.org/ or https://github.com/msys2/msys2-installer/releases" -ForegroundColor Yellow
            return
        }
        Write-Host "  Installing MSYS2 (this takes a few minutes)..." -ForegroundColor Gray
        try {
            $installProcess = Start-Process -FilePath $msys2Installer -ArgumentList "install --root $msys2Path --confirm-command" -Wait -NoNewWindow -PassThru
            if ($installProcess.ExitCode -ne 0) {
                throw "MSYS2 installer exit code $($installProcess.ExitCode)"
            }
            # Clean up downloaded installer
            Remove-Item $msys2Installer -Force -ErrorAction SilentlyContinue
        } catch {
            Write-Host "  MSYS2 installation failed: $_" -ForegroundColor Red
            Write-Host "  Please download MSYS2 manually from https://www.msys2.org/" -ForegroundColor Yellow
            return
        }
    }

    Write-Host "  Installing Fortran toolchain via pacman..." -ForegroundColor Gray
    try {
        & "$msys2Path\usr\bin\bash.exe" -lc "pacman -Syu --noconfirm"
        if ($LASTEXITCODE -ne 0) {
            throw "pacman -Syu failed with exit code $LASTEXITCODE"
        }
        & "$msys2Path\usr\bin\bash.exe" -lc "pacman -S --noconfirm mingw-w64-ucrt-x86_64-gcc-fortran mingw-w64-ucrt-x86_64-meson mingw-w64-ucrt-x86_64-ninja"
        if ($LASTEXITCODE -ne 0) {
            throw "pacman -S failed with exit code $LASTEXITCODE"
        }
    } catch {
        Write-Host "  MSYS2 package installation failed: $_" -ForegroundColor Red
        Write-Host "  You may need to re-run this step after fixing MSYS2." -ForegroundColor Yellow
        return
    }

    Add-ToUserPath "$msys2Path\ucrt64\bin"
    Write-Host "  MSYS2 + gfortran installed" -ForegroundColor Green
}

# 7. VS Code (optional)
Write-Host "[7/7] Checking VS Code..." -ForegroundColor Yellow
if (Test-CommandExists "code") {
    Write-Host "  Installed" -ForegroundColor Green
} else {
    if (-not $wingetAvailable) {
        Write-Host "  winget not available; please install VS Code manually (see URLs above)." -ForegroundColor Yellow
    } else {
        Write-Host "  Installing VS Code..." -ForegroundColor Gray
        try {
            & winget install Microsoft.VisualStudioCode --accept-package-agreements --accept-source-agreements --silent
            if ($LASTEXITCODE -ne 0) {
                throw "winget exit code $LASTEXITCODE"
            }
            Write-Host "  VS Code installed" -ForegroundColor Green
        } catch {
            Write-Host "  VS Code install failed: $_" -ForegroundColor Red
            Write-Host "  Install manually: https://code.visualstudio.com/Download" -ForegroundColor Yellow
        }
    }
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
