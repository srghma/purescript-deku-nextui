#!/bin/bash

# Set the source directory where your .tsx files are located
source_dir="/home/srghma/projects/purescript-deku-nextui/src/app"

# Check if the source directory exists
if [ ! -d "$source_dir" ]; then
  echo "Error: Source directory '$source_dir' does not exist."
  exit 1
fi

# Find all .tsx files within the source directory and its subdirectories.
find "$source_dir" -type f -name "*.tsx" | while IFS= read -r file; do

  # Get the relative path of the file within the source directory.
  relative_path="${file#"$source_dir"}"
  # Remove the first slash.
  relative_path="${relative_path:1}"

  # Get the directory portion of the relative path.
  file_dir=$(dirname "$relative_path")

  # Get the filename of the .tsx file, without extension.
  filename=$(basename "$relative_path" .tsx)

  # Create the new destination directory path.
  dest_dir_path="$source_dir/$file_dir/$filename"

  # Create the new file path as page.tsx
  new_file="$dest_dir_path/page.tsx"

  # Print a message to know what we are doing.
  echo "Moving '$file' to '$new_file'"

  # Create destination folder.
  mkdir -p "$dest_dir_path"

  # Move and rename the file.
  mv "$file" "$new_file"

done

echo "All .tsx files have been moved into their respective directories with page.tsx names."
