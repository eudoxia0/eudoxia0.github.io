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
class YearQuarter:
    """
    A year-quarter.
    """

    year: int
    quarter: int

    def __post_init__(self):
        assert 1 <= self.quarter <= 4

    def next_quarter(self) -> "YearQuarter":
        if self.quarter == 4:
            return YearQuarter(year=self.year + 1, quarter=1)
        else:
            return YearQuarter(year=self.year, quarter=self.quarter + 1)


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

    def year_quarter(self) -> YearQuarter:
        return YearQuarter(year=self.year, quarter=(self.month - 1) // 3 + 1)


def get_posts_from_directory(directory_path: Path) -> list[Post]:
    posts: list[Post] = []
    for file_path in directory_path.glob("*.md"):
        post = Post.from_file_path(file_path)
        posts.append(post)
    return posts


def _quarter_str(yq: YearQuarter) -> str:
    return f"{yq.year}-Q{yq.quarter}"


def _fill_quarters(values: defaultdict[str, int]) -> list[str]:
    """
    Given a dict keyed by quarter strings, fill in any missing quarters
    between the earliest and latest with 0, and return the sorted keys.
    """
    earliest_year, earliest_quarter = map(
        lambda s: int(s.replace("Q", "")), min(values).split("-")
    )
    latest_year, latest_quarter = map(
        lambda s: int(s.replace("Q", "")), max(values).split("-")
    )
    current = YearQuarter(year=earliest_year, quarter=earliest_quarter)
    last = YearQuarter(year=latest_year, quarter=latest_quarter)
    while current <= last:
        values[_quarter_str(current)] += 0
        current = current.next_quarter()
    return sorted(values.keys())


def plot_words_per_quarter(posts: list[Post]) -> None:
    words_per_quarter: defaultdict[str, int] = defaultdict(int)
    for post in posts:
        words_per_quarter[_quarter_str(post.year_quarter())] += post.word_count

    sorted_quarters = _fill_quarters(words_per_quarter)

    plt.figure(figsize=(24, 12))
    plt.bar(sorted_quarters, [words_per_quarter[q] for q in sorted_quarters])
    plt.xticks(fontsize=8, rotation=90, ha="right")
    plt.xlabel("Quarter")
    plt.ylabel("Words Written")
    plt.title("Words per Quarter")
    plt.tight_layout()
    plt.savefig("words.png", dpi=300)


def plot_cumulative_words_per_quarter(posts: list[Post]) -> None:
    words_per_quarter: defaultdict[str, int] = defaultdict(int)
    cumulative_words_per_quarter: defaultdict[str, int] = defaultdict(int)
    for post in posts:
        words_per_quarter[_quarter_str(post.year_quarter())] += post.word_count

    sorted_quarters = _fill_quarters(words_per_quarter)

    cumulative_total = 0
    for quarter in sorted_quarters:
        cumulative_total += words_per_quarter[quarter]
        cumulative_words_per_quarter[quarter] = cumulative_total

    plt.figure(figsize=(24, 12))
    plt.bar(
        sorted_quarters, [cumulative_words_per_quarter[q] for q in sorted_quarters]
    )
    plt.xticks(fontsize=8, rotation=90, ha="right")
    plt.xlabel("Quarter")
    plt.ylabel("Cumulative Words Written")
    plt.title("Cumulative Words per Quarter")
    plt.tight_layout()
    plt.savefig("cumulative_words.png", dpi=300)


def plot_posts_per_quarter(posts: list[Post]) -> None:
    posts_per_quarter: defaultdict[str, int] = defaultdict(int)
    for post in posts:
        posts_per_quarter[_quarter_str(post.year_quarter())] += 1

    sorted_quarters = _fill_quarters(posts_per_quarter)

    plt.figure(figsize=(24, 12))
    plt.bar(sorted_quarters, [posts_per_quarter[q] for q in sorted_quarters])
    plt.xticks(fontsize=8, rotation=90, ha="right")
    plt.xlabel("Quarter")
    plt.ylabel("Posts Written")
    plt.title("Posts per Quarter")
    plt.tight_layout()
    plt.savefig("posts.png", dpi=300)


def plot_cumulative_posts_per_quarter(posts: list[Post]) -> None:
    posts_per_quarter: defaultdict[str, int] = defaultdict(int)
    cumulative_posts_per_quarter: defaultdict[str, int] = defaultdict(int)
    for post in posts:
        posts_per_quarter[_quarter_str(post.year_quarter())] += 1

    sorted_quarters = _fill_quarters(posts_per_quarter)

    cumulative_total = 0
    for quarter in sorted_quarters:
        cumulative_total += posts_per_quarter[quarter]
        cumulative_posts_per_quarter[quarter] = cumulative_total

    plt.figure(figsize=(24, 12))
    plt.bar(
        sorted_quarters, [cumulative_posts_per_quarter[q] for q in sorted_quarters]
    )
    plt.xticks(fontsize=8, rotation=90, ha="right")
    plt.xlabel("Quarter")
    plt.ylabel("Cumulative Posts Written")
    plt.title("Cumulative Posts per Quarter")
    plt.tight_layout()
    plt.savefig("cumulative_posts.png", dpi=300)


def main():
    if len(sys.argv) != 2:
        raise ValueError("Must provide at least one argument.")

    posts = get_posts_from_directory(POSTS_DIRECTORY)
    if sys.argv[1] == "words":
        plot_words_per_quarter(posts)
    elif sys.argv[1] == "cumulative_words":
        plot_cumulative_words_per_quarter(posts)
    elif sys.argv[1] == "posts":
        plot_posts_per_quarter(posts)
    elif sys.argv[1] == "cumulative_posts":
        plot_cumulative_posts_per_quarter(posts)
    else:
        raise ValueError("Unknown command.")


if __name__ == "__main__":
    try:
        main()
    except Exception as e:
        print(e)
