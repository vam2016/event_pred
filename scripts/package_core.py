"""Assemble core source and recorded checks; packaging itself runs no tests."""
from pathlib import Path
import hashlib
import json
import re
import shutil
import stat
import zipfile

root = Path(__file__).resolve().parent.parent
version = "1.0.0-rc.1"
output = root.parent / f"SurvCast-v{version}.zip"
modules = set()

def include_sources(path):
    for name in re.findall(r'source\("(R/[^\"]+)"', path.read_text()):
        if name not in modules:
            modules.add(name)
            include_sources(root / name)

include_sources(root / "app_core.R")
paths = set(modules) | {
    "R/_disable_autoload.R", ".gitignore",
    "Start-Mac.command", "Start-Windows.cmd", "Start-Linux.sh",
    "scripts/start_core.R", "scripts/start_windows.ps1", "docs/LOCAL_START.md",
    "app_core.R", "scripts/install_core.R", "scripts/package_core.py",
    "www/style.css", "www/handbook.css", "www/core-mathjax.js", "www/MathJax-LICENSE.txt",
    "docs/CORE_HANDBOOK.md", "docs/CORE_DEVELOPMENT_STATUS.md",
    "docs/V1_0_RELEASE_PLAN.md", "docs/V2_0_DEVELOPMENT_PLAN.md",
    "docs/CORE_VALIDATION.md", "scripts/check_core.R",
    "scripts/check_core_browser.cjs", "scripts/replay_core_downloads.R",
    "validation/core_check_results.json", "validation/core_browser_results.json",
    "validation/core_launch_results.json", "validation/core_replay_results.json", "validation/core_portable_results.json",
    "validation/core_development_status.json", "validation/core_github_sync.json",
}
files = {name: (root / name).read_bytes() for name in sorted(paths)}
files["app.R"] = (root / "app_core.R").read_bytes()
files["README.md"] = (root / "docs/README_CORE.md").read_bytes()
files["Dockerfile"] = (root / "Dockerfile.core").read_bytes()
files[".dockerignore"] = (root / "Dockerfile.core.dockerignore").read_bytes()
status = json.loads((root / "validation/core_development_status.json").read_text())
manifest = {
    "platform": "SurvCast", "title": "生存事件预测与模拟工作台",
    "version": version, "status": status["status"],
    "checks_run": status["checks_run"], "release_ready": status["release_ready"],
    "modules": sorted(modules),
    "files": [{"path": name, "sha256": hashlib.sha256(data).hexdigest()}
              for name, data in sorted(files.items())],
}
files["core-source-manifest.json"] = (json.dumps(manifest, ensure_ascii=False, indent=2)+"\n").encode()
with zipfile.ZipFile(output, "w", zipfile.ZIP_DEFLATED) as archive:
    for name, data in sorted(files.items()):
        info = zipfile.ZipInfo("SurvCast/"+name)
        info.create_system = 3
        mode = 0o755 if name in {"Start-Mac.command","Start-Linux.sh"} else 0o644
        info.external_attr = (stat.S_IFREG | mode) << 16
        info.compress_type = zipfile.ZIP_DEFLATED
        archive.writestr(info, data)
shutil.copyfile(output, root.parent / f"event_pred-v{version}-core.zip")
print(json.dumps({"artifact": str(output), "files_written": len(files), "recorded_validation": status["status"], "packaging_runs_tests": False}, ensure_ascii=False))
