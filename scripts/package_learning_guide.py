"""Version learning inputs or package an already verified offline guide.
Run from the source root. --manifest-only updates source hashes before verification.
"""
import argparse
import hashlib
import json
from pathlib import Path
import shutil
import zipfile

ROOT = Path.cwd()
OUTPUT = ROOT / 'reports/learning-v2'
MANIFEST = ROOT / 'docs/learning-assets/guide-manifest.json'
def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()
def source_files():
    paths = [Path('docs/SurvCast_STATISTICIAN_GUIDE.md'), Path('docs/LEARNING_V2_ADDENDUM.md'),
             Path('docs/learning-guide.css'), Path('docs/CORE_CASES.md'), Path('app_core.R'), Path('www/core-mathjax.js')]
    paths += sorted(Path('docs/learning-assets').glob('*')) + sorted(Path('docs/learning-history').glob('*'))
    paths += sorted(Path('scripts').glob('*learning*'))
    # Bind the mathematical/runtime inputs, including any future core modules.
    paths += sorted(Path('R').glob('*.R'))
    return sorted({p for p in paths if p.is_file() and p != MANIFEST.relative_to(ROOT)})

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--manifest-only', action='store_true')
    args = parser.parse_args()
    if args.manifest_only:
        manifest = dict(schema='survcast-learning-guide-2', guide_version='2.0-dev', authored_date='2026-10-11',
                        verified_engine_baseline='98fe9b1263c22a55cede8eb1060c805d5f67b91e',
                        chapters=34, original_chapters=28, legacy_worked_cases=4, current_core_cases=4,
                        narrative_scenarios=6, illustrations=7, self_tests=12,
                        scope='Learning artifact; synthetic cases; not an application candidate or deployed release',
                        files_sha256={str(p): sha(p) for p in source_files()})
        MANIFEST.write_text(json.dumps(manifest, ensure_ascii=False, indent=2)+'\n')
        print('Updated versioned learning source manifest')
        return
    manifest = json.loads(MANIFEST.read_text())
    for name, digest in manifest['files_sha256'].items():
        if sha(Path(name)) != digest:
            raise RuntimeError('Source changed after manifest: '+name)
    html = OUTPUT / 'SurvCast_STATISTICIAN_GUIDE.html'
    numeric = json.loads(Path('validation/v2_learning_results.json').read_text())
    browser = json.loads(Path('validation/v2_learning_browser_results.json').read_text())
    for result in (numeric, browser):
        if result['status'] != 'passed' or result['html_sha256'] != sha(html) or result['manifest_sha256'] != sha(MANIFEST):
            raise RuntimeError('Missing or stale verification; rerun numerical and offline browser checks')
    folder = OUTPUT / 'SurvCast-learning-2.0-dev'
    if folder.exists():
        shutil.rmtree(folder)
    folder.mkdir()
    paths = [Path('docs/SurvCast_STATISTICIAN_GUIDE.md'), Path('docs/LEARNING_V2_ADDENDUM.md'), Path('docs/learning-guide.css'), Path('docs/CORE_CASES.md')]
    paths += sorted(Path('docs/learning-assets').glob('*')) + sorted(Path('docs/learning-history').glob('*'))
    paths += sorted(Path('scripts').glob('*learning*'))
    paths += [Path('validation/v2_learning_results.json'), Path('validation/v2_learning_browser_results.json'), Path('validation/v2_learning_figures_results.json')]
    for p in paths:
        target = folder/p
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(p, target)
    shutil.copy2(html, folder/html.name)
    shutil.copytree(OUTPUT/'cases', folder/'cases')
    (folder/'README.md').write_text('''# SurvCast 2.0-dev 学习资料

打开 SurvCast_STATISTICIAN_GUIDE.html 离线阅读34章、7图和12个自测。
前28章来源及历史检查保存在docs/learning-history，本次检查保存在validation。
原A–D与新增四个核心案例不同；六个使用场景是流程说明。

复现需要同版本SurvCast源码与R依赖，本包不包含应用运行源码。
将一个cases子目录的JSON/R脚本复制到同版本源码根目录，运行Rscript survcast_case_replay.R。
不同案例分别执行，同名输出可能覆盖。原案例、HTML构建、核对、绘图与打包脚本均在scripts。
完整源文件清单/校验见docs/learning-assets/guide-manifest.json。
本包不构成2.0应用候选包、正式发布、目标环境验收或临床方法校准。
''')
    contents = {str(p.relative_to(folder)): sha(p) for p in sorted(folder.rglob('*')) if p.is_file()}
    (folder/'package-files.json').write_text(json.dumps(contents, ensure_ascii=False, indent=2)+'\n')
    archive = OUTPUT/'SurvCast-learning-2.0-dev.zip'
    with zipfile.ZipFile(archive, 'w', zipfile.ZIP_DEFLATED) as z:
        for p in sorted(folder.rglob('*')):
            if p.is_file(): z.write(p, str(Path(folder.name)/p.relative_to(folder)))
    with zipfile.ZipFile(archive) as z:
        if z.testzip() is not None: raise RuntimeError('ZIP integrity failure')
        for name, digest in contents.items():
            if hashlib.sha256(z.read(folder.name+'/'+name)).hexdigest() != digest:
                raise RuntimeError('ZIP entry differs: '+name)
    report = dict(status='passed', files=len(contents)+1, verified_entries=len(contents),
                  package_sha256=sha(archive), html_sha256=sha(html), manifest_sha256=sha(MANIFEST),
                  package='reports/learning-v2/SurvCast-learning-2.0-dev.zip',
                  scope='Every ZIP payload matches verified learning folder; application source intentionally separate')
    Path('validation/v2_learning_package_results.json').write_text(json.dumps(report, ensure_ascii=False, indent=2)+'\n')
    print('Verified learning ZIP:', archive, 'entries:', report['files'])
if __name__ == '__main__': main()
