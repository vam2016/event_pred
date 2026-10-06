$ErrorActionPreference = "Stop"
$launchArgs = $args
$root = Split-Path -Parent $PSScriptRoot
Set-Location -LiteralPath $root
$found = Get-Command Rscript.exe -CommandType Application -ErrorAction SilentlyContinue
$rscript = if ($found) { $found.Source } else { $null }
if (-not $rscript) {
  foreach ($key in @('HKCU:\SOFTWARE\R-core\R','HKLM:\SOFTWARE\R-core\R','HKLM:\SOFTWARE\WOW6432Node\R-core\R')) {
    $registered = Get-ItemProperty -LiteralPath $key -ErrorAction SilentlyContinue
    if ($registered -and $registered.InstallPath) {
      $candidate = Join-Path $registered.InstallPath 'bin\Rscript.exe'
      if (Test-Path -LiteralPath $candidate) { $rscript=$candidate; break }
    }
  }
}
if (-not $rscript) {
  $bases = @($env:ProgramFiles,${env:ProgramFiles(x86)},$env:LOCALAPPDATA) | Where-Object { $_ }
  foreach ($base in $bases) {
    foreach ($folder in @((Join-Path $base 'R'),(Join-Path $base 'Programs\R'))) {
      $installs = Get-ChildItem -LiteralPath $folder -Directory -ErrorAction SilentlyContinue | Where-Object { $_.Name -match '^R-\d+\.\d+\.\d+' } | Sort-Object { [version]([regex]::Match($_.Name,'\d+\.\d+\.\d+').Value) } -Descending
      foreach ($install in $installs) {
        $candidate = Join-Path $install.FullName 'bin\Rscript.exe'
        if (Test-Path -LiteralPath $candidate) { $rscript=$candidate; break }
      }
      if ($rscript) { break }
    }
    if ($rscript) { break }
  }
}
if (-not $rscript) { throw 'R was not found. Install R from https://cran.r-project.org/bin/windows/base/ and retry.' }
& $rscript --vanilla (Join-Path $root 'scripts\start_core.R') @launchArgs
exit $LASTEXITCODE
