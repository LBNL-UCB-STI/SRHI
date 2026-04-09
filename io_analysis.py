#!/usr/bin/env python3
"""
Scan R scripts to extract:
- libraries used (library, require)
- input files (read_csv, st_read, etc.)
- output files (write_csv, st_write, etc.)
- URLs (http://, https://, ftp://)
- referenced files (any string ending with .csv, .shp, .rds, etc.)

Prints JSON to stdout, and a library summary to stderr.
"""

import os
import re
import sys
import json
import textwrap

from pathlib import Path
from collections import Counter, deque, defaultdict

# ----------------------------------------------------------------------
# Regular expressions (same as before)
# ----------------------------------------------------------------------

LIB_PATTERN = re.compile(
    r'\b(?:library|require)\s*\(\s*["\']?([^"\')\s]+)["\']?\s*\)'
)

INPUT_FUNCTIONS = [
    'read_csv', 'read.csv', 'read_csv2', 'read_delim',
    'read_rds', 'readRDS', 'st_read', 'read_sf',
    'fread', 'vroom', 'read_excel', 'load', 'fromJSON'
]
INPUT_PATTERN = re.compile(
    r'\b(' + '|'.join(INPUT_FUNCTIONS) + r')\s*\(\s*["\']([^"\']+)["\']',
    re.IGNORECASE
)

OUTPUT_FUNCTIONS = [
    'write_csv', 'write.csv', 'write_rds', 'saveRDS',
    'st_write', 'write_sf', 'fwrite', 'save', 'toJSON'
]

OUTPUT_PATTERN = re.compile(
    r'\b(' + '|'.join(OUTPUT_FUNCTIONS) + r')\s*\(\s*(?:[^,]+,\s*)?["\']([^"\']+)["\']',
    re.IGNORECASE
)

URL_PATTERN = re.compile(r'https?://[^\s"\'<>]+|ftp://[^\s"\'<>]+', re.IGNORECASE)

REF_EXTENSIONS = r'\.(csv|shp|rds|geojson|RData|Rda|Rds|tiff|tif|nc|grd|kml|gpkg)'
REF_PATTERN = re.compile(
    r'["\']([^"\']*?(?:' + REF_EXTENSIONS + r'))["\']',
    re.IGNORECASE
)

# ----------------------------------------------------------------------
# Extraction functions
# ----------------------------------------------------------------------

def extract_libraries(content):
    libs = set()
    for match in LIB_PATTERN.finditer(content):
        pkg = match.group(1).strip().split(',')[0].strip()
        if pkg:
            libs.add(pkg)
    return sorted(libs)

def strip_comments(content):
    """Remove R comments: for each line, delete from first '#' to end."""
    lines = content.split('\n')
    stripped = []
    for line in lines:
        pos = line.find('#')
        if pos != -1:
            line = line[:pos]
        stripped.append(line)
    return '\n'.join(stripped)

def extract_input_files(content):
    content_clean = strip_comments(content)
    inputs = []
    for match in INPUT_PATTERN.finditer(content_clean):
        path = match.group(2).strip()
        if path:
            inputs.append(path)
    return inputs

def extract_output_files(content):
    content_clean = strip_comments(content)
    outputs = []
    for match in OUTPUT_PATTERN.finditer(content_clean):
        path = match.group(2).strip()
        if path:
            outputs.append(path)
    return outputs

def extract_urls(content):
    # Also strip comments to avoid URLs inside comments
    content_clean = strip_comments(content)
    urls = set()
    for match in URL_PATTERN.finditer(content_clean):
        urls.add(match.group(0))
    return sorted(urls)

def extract_referenced_files(content):
    refs = set()
    for match in REF_PATTERN.finditer(content):
        path = match.group(1).strip()
        if not re.match(r'https?://', path, re.IGNORECASE):
            refs.add(path)
    return sorted(refs)

def scan_r_file(file_path):
    try:
        with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
            content = f.read()
    except Exception as e:
        print(f"Error reading {file_path}: {e}")
        return None

    r_file_analysis = {
        'lib': extract_libraries(content),
        'inp_file': extract_input_files(content),
        'out_file': extract_output_files(content),
        'url': extract_urls(content),
        'ref_file': extract_referenced_files(content)
    }

    return r_file_analysis

def find_data_files_as_map(root_dir: str) -> dict[str, str]:
    root = Path(root_dir).resolve()
    extensions = {".csv", ".shp", ".rdata"}
    file_map = {}
    for p in root.rglob("*"):
        if p.suffix.lower() in extensions:
            file_map[p.name] = str(p)
    return file_map


def resolve_script_order_simple(file_to_params, raw_file_to_path):
    """
    Determine runnable script order and identify missing input files.
    A script is blocked if any input is missing (raw file not found or intermediate file not produced).
    Blocked scripts cannot be run, and any script that depends on a blocked script is also blocked.

    Args:
        file_to_params (dict): keys = script relative path (e.g., "prep_data.R")
                               values = dict with 'inp_file' (list of input paths)
                                         and 'out_file' (list of output paths)
        raw_file_to_path (dict): keys = raw file basename (e.g., "counties.shp")
                                 values = full path (not used, only existence matters)

    Returns:
        dict with:
            'run_order' : list of script paths in executable order (only non‑blocked scripts)
            'missing_files' : dict { file_path: [list of scripts needing it] }
            'blocked_scripts' : list of scripts that cannot be run
    """
    # 1. Map each output file to the script that produces it
    output_to_script = {}
    for script, params in file_to_params.items():
        for out in params.get('out_file', []):
            output_to_script[out] = script

    # 2. Build dependency graph (script -> scripts it depends on)
    graph = {script: set() for script in file_to_params}
    reverse_graph = {script: set() for script in file_to_params}   # script -> scripts that depend on it
    missing_per_script = defaultdict(list)   # any input that is neither raw nor produced

    for script, params in file_to_params.items():
        for inp in params.get('inp_file', []):
            inp_basename = os.path.basename(inp)
            if inp_basename in raw_file_to_path:
                # Raw file exists – no dependency
                continue
            producer = output_to_script.get(inp)
            if producer is not None:
                graph[script].add(producer)
                reverse_graph[producer].add(script)
            else:
                # Neither raw nor produced – missing
                missing_per_script[script].append(inp)

    # 3. Identify initially blocked scripts (have missing raw or missing intermediate)
    blocked = set()
    for script, params in file_to_params.items():
        # Check for missing raw files (by basename) – they are not in missing_per_script yet
        for inp in params.get('inp_file', []):
            inp_basename = os.path.basename(inp)
            if inp_basename not in raw_file_to_path and inp not in output_to_script:
                blocked.add(script)
                break
        # Also if any missing intermediate (already captured in missing_per_script)
        if script in missing_per_script and missing_per_script[script]:
            blocked.add(script)

    # 4. Propagate blockage: any script that depends on a blocked script becomes blocked
    queue = deque(blocked)
    while queue:
        b = queue.popleft()
        for dependent in reverse_graph.get(b, []):
            if dependent not in blocked:
                blocked.add(dependent)
                queue.append(dependent)

    # 5. Determine run order for non‑blocked scripts (topological sort)
    # Filter graph to only non‑blocked nodes
    unblocked_scripts = [s for s in file_to_params if s not in blocked]
    # Build in-degree only for unblocked scripts, ignoring edges from/to blocked scripts
    in_degree = {script: 0 for script in unblocked_scripts}
    for script in unblocked_scripts:
        for dep in graph[script]:
            if dep not in blocked:
                in_degree[script] += 1

    queue = deque([s for s in unblocked_scripts if in_degree[s] == 0])
    run_order = []
    while queue:
        script = queue.popleft()
        run_order.append(script)
        for dependent in reverse_graph[script]:
            if dependent in blocked:
                continue
            in_degree[dependent] -= 1
            if in_degree[dependent] == 0:
                queue.append(dependent)

    # If there are cycles among unblocked scripts, some may not be scheduled (but we ignore for simplicity)

    # 6. Aggregate all missing files (from all scripts, including blocked)
    all_missing = defaultdict(list)
    for script, missing_list in missing_per_script.items():
        for m in missing_list:
            all_missing[m].append(script)

    return {
        'run_order': run_order,
        'missing_files': dict(all_missing),
        'blocked_scripts': sorted(blocked)
    }

# ----------------------------------------------------------------------
# Main
# ----------------------------------------------------------------------

def main():
    root_path = Path('.').resolve()
    raw_path = Path('.').resolve()

    raw_file_to_path = find_data_files_as_map(raw_path)

    file_to_params = {}
    all_libraries = []  # collect every library occurrence (with duplicates) for summary

    output_to_r_script = {}

    for r_file in root_path.rglob('*.R'):
        rel_path = r_file.relative_to(root_path)
        info = scan_r_file(r_file)
        if info:
            file_to_params[str(rel_path)] = info
            all_libraries.extend(info['lib'])

    # Build output JSON
    if len(sys.argv) > 1 and 'json' in sys.argv[1]:
        output_data = []
        for f_name, info in file_to_params.items():
            json_dict = {
                'file': f_name,
                'urls': info['url'],
                'input_files': info['inp_file'],
                'output_files': info['out_file'],
                # 'libraries': info['lib'],
                # 'referenced_files': info['ref_file']
            }
            json_dict = {k: v for k, v in json_dict.items() if v}
            output_data.append(json_dict)

        # Print JSON to stdout
        print(json.dumps(output_data, indent=2, ensure_ascii=False))

    elif len(sys.argv) > 1 and 'order' in sys.argv[1]:
        print("\n" + "="*60)
        print("Which R scripts can be run, which are not, what files are missing")
        print("="*60)

        result = resolve_script_order_simple(file_to_params, raw_file_to_path)
        print("\nScripts that can run (in order):")
        for script in result['run_order']:
            print(f"  {script}")

        print("\nBlocked scripts (cannot run):")
        for script in result['blocked_scripts']:
            print(f"  {script}")

        if result['missing_files']:
            print("\nMissing files (neither raw nor produced):")
            for f, scripts in result['missing_files'].items():
                print(f"  {f} needed by: {', '.join(scripts)}")

    elif len(sys.argv) > 1 and 'libs' in sys.argv[1]:
        if all_libraries:
            # Generate the content of requirements.R
            lib_list = set(all_libraries)
            libs_quoted = '", "'.join(lib_list)
            req_script = textwrap.dedent(f'''\
                #!/usr/bin/env Rscript
                # requirements.R
                # This script installs all R packages used in the scanned project.
                # Run it with: Rscript requirements.R 2>&1 | tee install.log
    
                options(repos = c(CRAN = "https://cloud.r-project.org"))
    
                install.packages(c("{libs_quoted}"))
            ''')

            # Print to stderr so it doesn't interfere with JSON output
            print("\n" + "="*60)
            print("REQUIREMENTS.R FILE CONTENT (copy and paste into requirements.R)")
            print("="*60)
            print(req_script)
            print("\n" + "="*60)
            print("\nInstructions:")
            print("1. Save the above content as 'requirements.R'")
            print("2. Run: Rscript requirements.R 2>&1 | tee install.log")
            print("   This will save all output (including errors) to install.log")
            print("   while also showing it in the terminal.")
            print("="*60)
        else:
            print("No library calls found.")
            print("\n" + "="*60)
            print(f"Scanned {len(file_to_params)} R files.")
            print("="*60)
    else:
        print("Usage: python io_analysis.py json|order|libs")

if __name__ == '__main__':
    main()