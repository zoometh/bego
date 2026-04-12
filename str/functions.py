
#%%
# Export Windows folder structure to CSV (to be used for image management in Bego)

from pathlib import Path
import csv


def export_folder_structure_to_csv(
    source_root: str,
    output_csv: str,
) -> None:
    """
    Export the directory tree under source_root to a CSV file.
    Only folders are exported, not files.

    The CSV contains:
    - relative_path: path relative to source_root
    """

    source = Path(source_root)

    if not source.exists():
        raise FileNotFoundError(f"Source folder does not exist: {source}")

    if not source.is_dir():
        raise NotADirectoryError(f"Source is not a directory: {source}")

    folders = []

    for path in source.rglob("*"):
        if path.is_dir():
            relative_path = path.relative_to(source).as_posix()
            folders.append(relative_path)

    folders.sort()

    output_path = Path(output_csv)
    output_path.parent.mkdir(parents=True, exist_ok=True)

    with output_path.open("w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        writer.writerow(["relative_path"])
        for folder in folders:
            writer.writerow([folder])

    print(f"Exported {len(folders)} folders to: {output_path}")


if __name__ == "__main__":
    export_folder_structure_to_csv(
        source_root=r"D:/(ARCHIVES)/Bego/Base de donnees/Images",
        output_csv=r"C:/Users/TH282424/Rprojects/bego/str/images_structure.csv",
    )