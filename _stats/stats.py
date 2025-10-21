import re
import sys
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path

import matplotlib.pyplot as plt

POSTS_DIRECTORY: Path = Path("../article/_posts/")


def _read_file_contents(file_path: Path) -> str:
    with file_path.open() as f:
        return f.read()


def _compute_word_count(contents: str) -> int:
    return len(contents.split())


@dataclass(frozen=True, eq=True, order=True)
class YearMonth:
    """
    A year-month.
    """

    year: int
    month: int

    def __post_init__(self):
        assert 1 <= self.month <= 12

    def next_month(self) -> "YearMonth":
        if self.month == 12:
            return YearMonth(year=self.year + 1, month=1)
        else:
            return YearMonth(year=self.year, month=self.month + 1)


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
    def from_file_path(cls, file_path: Path) -> "Post":
        pattern = r"(\d{4})-(\d{2})-(\d{2})-(.+)\.md"
        match = re.match(pattern, file_path.name)
        if match:
            year, month, day, slug = match.groups()
            contents = _read_file_contents(file_path)
            word_count = _compute_word_count(contents)
            return cls(
                file_path=file_path,
                year=int(year),
                month=int(month),
                day=int(day),
                slug=slug,
                contents=contents,
                word_count=word_count,
            )
        else:
            raise ValueError(f"Invalid file name format: {file_path.name}")

    def year_month(self) -> YearMonth:
        return YearMonth(year=self.year, month=self.month)


def earliest_month(posts: list[Post]) -> YearMonth:
    return sorted(posts, key=lambda p: p.year_month())[0].year_month()


def latest_month(posts: list[Post]) -> YearMonth:
    return sorted(posts, key=lambda p: p.year_month())[-1].year_month()


def all_months(posts: list[Post]) -> list[YearMonth]:
    first: YearMonth = earliest_month(posts)
    last: YearMonth = latest_month(posts)
    months: list[YearMonth] = []
    current: YearMonth = first
    months.append(current)
    while not (current == last):
        current = current.next_month()
        months.append(current)
    return months


def get_posts_from_directory(directory_path: Path) -> list[Post]:
    posts: list[Post] = []
    for file_path in directory_path.glob("*.md"):
        post = Post.from_file_path(file_path)
        posts.append(post)
    return posts


def plot_words_per_month(posts: list[Post]) -> None:
    # Initialize a defaultdict with int (default: 0)
    words_per_month: defaultdict[str, int] = defaultdict(int)

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
        year, month = map(int, current_month.split("-"))
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
    plt.xticks(fontsize=8, rotation=90, ha="right")
    plt.xlabel("Month")
    plt.ylabel("Words Written")
    plt.title("Words per Month")
    plt.tight_layout()
    # Save the plot as a PNG file
    plt.savefig("words_per_month.png", dpi=300)


def plot_cumulative_words_per_month(posts: list[Post]) -> None:
    # Initialize a defaultdict with int (default: 0)
    words_per_month: defaultdict[str, int] = defaultdict(int)
    cumulative_words_per_month: defaultdict[str, int] = defaultdict(int)

    # Iterate through the posts and aggregate the word count by month
    for post in posts:
        month_str = f"{post.year}-{post.month:02d}"
        words_per_month[month_str] += post.word_count

    # Get the earliest and latest months
    earliest_month = min(words_per_month)
    latest_month = max(words_per_month)

    # Fill the missing months with 0 word count
    current_month = earliest_month
    cumulative_total = 0
    while current_month <= latest_month:
        cumulative_total += words_per_month[current_month]
        cumulative_words_per_month[current_month] = cumulative_total
        year, month = map(int, current_month.split("-"))
        month += 1
        if month > 12:
            month = 1
            year += 1
        current_month = f"{year}-{month:02d}"

    # Sort the months in ascending order
    sorted_months = sorted(cumulative_words_per_month.keys())

    # Create a bar chart using matplotlib
    plt.figure(figsize=(12, 6))
    plt.bar(
        sorted_months, [cumulative_words_per_month[month] for month in sorted_months]
    )
    plt.xticks(fontsize=8, rotation=90, ha="right")
    plt.xlabel("Month")
    plt.ylabel("Cumulative Words Written")
    plt.title("Cumulative Words per Month")
    plt.tight_layout()
    # Save the plot as a PNG file
    plt.savefig("cumulative_words_per_month.png", dpi=300)


def main():
    if len(sys.argv) != 2:
        raise ValueError("Must provide at least one argument.")

    posts = get_posts_from_directory(POSTS_DIRECTORY)
    if sys.argv[1] == "words_per_month":
        plot_words_per_month(posts)
    elif sys.argv[1] == "cumulative_words_per_month":
        plot_cumulative_words_per_month(posts)
    else:
        raise ValueError("Unknown command.")


if __name__ == "__main__":
    try:
        main()
    except Exception as e:
        print(e)
