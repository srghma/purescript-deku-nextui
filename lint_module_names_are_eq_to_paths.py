#!/usr/bin/env python3

# reads lines of file
# skip empty lines or lines starting with a comment "-- "
# first line should be "module My.Module.Name where"
# checks that module name is equal to path to file from the root

import sys
import subprocess
import re
import pprint

# Read lines until we find a line with the module declaration
def return_first_line_of_code(path_to_file):
    with open(path_to_file, "r") as f:
        # Skip empty lines or lines starting with "--"
        for line in f:
            stripped_line = line.strip()
            if stripped_line and not stripped_line.startswith("--"):
                line_with_a_module_name = stripped_line
                return line_with_a_module_name

def replace_the_first_line_that_is_eq_to(path_to_file, line_to_replace, line_to_replace_with):
    """
    Replaces the first occurrence of a specific line in a file with a new line.

    Args:
      path_to_file: The path to the file.
      line_to_replace: The line to be replaced.
      line_to_replace_with: The line to replace with.
    """
    try:
        with open(path_to_file, "r") as f:
            lines = f.readlines()
        found = False
        with open(path_to_file, "w") as f:
            for line in lines:
                if line.strip() == line_to_replace.strip() and not found:
                    f.write(line_to_replace_with + "\n")
                    found = True
                else:
                    f.write(line)
        if not found:
            print(f"Warning: Line '{line_to_replace}' not found in file '{path_to_file}'. No replacement was done.")
    except FileNotFoundError:
        print(f"Error: File '{path_to_file}' not found.")
    except Exception as e:
         print(f"An error ocurred: {e}")

def lint_module_names_eq_to_path(root, path_to_file_relative_to_root):
    path_to_file = root + "/" + path_to_file_relative_to_root
    try:
        line_with_a_module_name = return_first_line_of_code(path_to_file)
    except FileNotFoundError:
        print(f"Error: File '{path_to_file}' not found.")
        return False

    if not line_with_a_module_name or not line_with_a_module_name.startswith("module "):
        print(f"Error: No module declaration found in {path_to_file}")
        return False

    # Remove the root prefix and the '.purs' extension, then convert to module path format
    expected_module_name_from_path = path_to_file_relative_to_root.replace(".purs", "").replace("/", ".")

    # Split the line, replace the element on index 1, and reconstruct the line
    expected_line__words = line_with_a_module_name.split(" ")
    if len(expected_line__words) > 1:
        expected_line__words[1] = expected_module_name_from_path
    expected_line = " ".join(expected_line__words)

    # Check if the module name matches the expected name
    if line_with_a_module_name == expected_line:
        return True
    else:
        print(f"Error: '{line_with_a_module_name}' != '{expected_line}' for {path_to_file_relative_to_root}")
        replace_the_first_line_that_is_eq_to(path_to_file, line_with_a_module_name, expected_line)
        return False

def quote_args_if_neeeded(args):
    quoted_args = []
    for arg in args:
        if " " in arg or "*" in arg:
            quoted_args.append(f'"{arg}"')
        else:
            quoted_args.append(arg)
    return quoted_args

def filter_out_matching(array, array_of_regexps):
    return [x for x in array if not any(re.match(r, x) for r in array_of_regexps)]

def get_all_purs_files(root):
    # Build the base command
    command = ["git", "ls-files", "*.purs"]

    print(f"Running command: {' '.join(quote_args_if_neeeded(command))}")

    # Get all `.purs` files tracked by git under the root directory
    result = subprocess.run(
        ["git", "ls-files", "*.purs"],
        text=True,
        capture_output=True,
        check=True,
        cwd=root,
    )
    purs_files = result.stdout.strip().splitlines()
    return purs_files

def lint_module_names_eq_to_path_root(root, purs_files):
    # Lint each file found
    n_total = len(purs_files)
    n_ok = 0
    n_fail = 0
    ok = True
    for purs_file in purs_files:
        result = lint_module_names_eq_to_path(root, purs_file)
        if result:
            n_ok += 1
        else:
            n_fail += 1
            ok = False

    return ok, n_total, n_ok, n_fail

# check that all files are from `list_of_known_root_dirs` and generate a dict { known_root_dir => relative_path_to_files }
# example:
# ```python
# known_files_dict, unknown_files_list = filepaths_to_dict_of_root_and_relative_paths(["src"], ["src/Data/Vect.purs", "unknown/Foo.purs"])
# assert known_files_dict == { "src": ["Data/Vect.purs"] }
# assert unknown_files_list == ["unknown/Foo.purs"]
# ```
def filepaths_to_dict_of_root_and_relative_paths(list_of_known_root_dirs, filepaths):
    def find_root_or_this_file(list_of_known_root_dirs, filepath):
        for root in list_of_known_root_dirs:
            if filepath.startswith(root + "/"):
                return root
        return False

    known_files_dict = {}
    unknown_files_list = []
    for filepath in filepaths:
        root_or_a_filepath = find_root_or_this_file(list_of_known_root_dirs, filepath)
        if root_or_a_filepath == False:
            unknown_files_list.append(filepath)
        else:
            relative_filepath = filepath[len(root_or_a_filepath)+1:]
            if root_or_a_filepath not in known_files_dict:
                known_files_dict[root_or_a_filepath] = []
            known_files_dict[root_or_a_filepath].append(relative_filepath)

    return known_files_dict, unknown_files_list

def main(list_of_regexes_to_exclude_files, list_of_known_root_dirs):
    # Get all `.purs` files tracked by git under the root directory
    try:
        purs_files = get_all_purs_files(".")
    except subprocess.CalledProcessError:
        print(f"Error: Unable to retrieve `.purs` files from cwd")
        sys.exit(1)

    purs_files_non_excluded = filter_out_matching(
        purs_files,
        list_of_regexes_to_exclude_files
    )

    # print(f"Found {len(purs_files_non_excluded)} non-excluded files")
    # print(f"Files: {purs_files_non_excluded}")

    # check that all files are from list_of_known_root_dirs and generate a dict { known_root_dir => relative_path_to_files }
    known_files_dict, unknown_files_list = filepaths_to_dict_of_root_and_relative_paths(
        list_of_known_root_dirs,
        purs_files_non_excluded
    )

    # print(f"Files: {pprint.pformat(known_files_dict)}")

    if len(unknown_files_list) > 0:
        print(f"Error: Unknown files, add them to `list_of_known_root_dirs` or `list_of_regexes_to_exclude_files`: {unknown_files_list}")
        sys.exit(1)

    # Iterate over each root directory and find `.purs` files
    ok = True
    for root, relative_path_to_files_in_root in known_files_dict.items():
        root_ok, n_total, n_ok, n_fail = lint_module_names_eq_to_path_root(root, relative_path_to_files_in_root)
        if root_ok:
            print(f"Linting successful for {root}, {n_ok}/{n_total} files OK")
        else:
            ok = False
            print(f"Error: Linting failed for {root}, {n_fail}/{n_total} files failed")

    if not ok:
      sys.exit(1)

if __name__ == "__main__":
    list_of_regexes_to_exclude_files = [
        r"^.spago/([^/]+/){1,7}[^/]+\.purs$",
    ]
    list_of_known_root_dirs = [
        "src",
        "test",
    ]
    main(list_of_regexes_to_exclude_files, list_of_known_root_dirs)
