from dataclasses import dataclass
from pathlib import Path
import re
from collections import defaultdict
import matplotlib.pyplot as plt
import sys

POSTS_DIRECTORY: Path = Path("article/_posts/")

def _read_file_contents(file_path: Path) -> str:
    with file_path.open() as f:
        return f.read()

def _compute_word_count(contents: str) -> int:
    return len(contents.split())

@dataclass(frozen=True)
class Post:
    file_path: Path
    year: int
    month: int
    day: int
    slug: str
    contents: str
    word_count: int

    @classmethod
    def from_file_path(cls, file_path: Path) -> 'Post':
        pattern = r'(\d{4})-(\d{2})-(\d{2})-(.+)\.md'
        match = re.match(pattern, file_path.name)
        if match:
            year, month, day, slug = match.groups()
            contents = _read_file_contents(file_path)
            word_count = _compute_word_count(contents)
            return cls(file_path, int(year), int(month), int(day), slug, contents, word_count)
        else:
            raise ValueError(f"Invalid file name format: {file_path.name}")


def get_posts_from_directory(directory_path: Path) -> list[Post]:
    posts = []
    for file_path in directory_path.glob("*.md"):
        post = Post.from_file_path(file_path)
        posts.append(post)
    return posts

def plot_words_per_month(posts: list[Post]) -> None:
    # Initialize a defaultdict with int (default: 0)
    words_per_month = defaultdict(int)

    # Iterate through the posts and aggregate the word count by month
    for post in posts:
        month_str = f"{post.year}-{post.month:02d}"
        words_per_month[month_str] += post.word_count

    # Get the earliest and latest months
    earliest_month = min(words_per_month)
    latest_month = max(words_per_month)

    # Fill the missing months with 0 word count
    current_month = earliest_month
    while current_month <= latest_month:
        words_per_month[current_month] += 0
        year, month = map(int, current_month.split('-'))
        month += 1
        if month > 12:
            month = 1
            year += 1
        current_month = f"{year}-{month:02d}"

    # Sort the months in ascending order
    sorted_months = sorted(words_per_month.keys())

    # Create a bar chart using matplotlib
    plt.figure(figsize=(12, 6))
    plt.bar(sorted_months, [words_per_month[month] for month in sorted_months])
    plt.xticks(fontsize=8, rotation=90, ha='right')
    plt.xlabel("Month")
    plt.ylabel("Words Written")
    plt.title("Words per Month")
    plt.tight_layout()
    # Save the plot as a PNG file
    plt.savefig("words_per_month.png", dpi=300)

def main():
    if len(sys.argv) != 2:
        raise ValueError("Must provide at least one argument.")
    if sys.argv[1] == "words_per_month":
        posts = get_posts_from_directory(POSTS_DIRECTORY)
        plot_words_per_month(posts)
    else:
        raise ValueError("Unknown command.")

if __name__ == "__main__":
    try:
        main()
    except Exception as e:
        print(e)