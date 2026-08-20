import argparse

import matplotlib.pyplot as plt
import numpy as np
from matplotlib.colors import LinearSegmentedColormap

# Arguments.
parser = argparse.ArgumentParser()
parser.add_argument("mode", choices=["sigmoid", "takeoff"])
args = parser.parse_args()
is_sigmoid: bool = args.mode == "sigmoid"
is_takeoff: bool = args.mode == "takeoff"

# Start of the X axis.
X_MIN: int = 1800

# End of the X axis.
X_MAX: int = 2100

# Present day, present time.
PRESENT: int = 2026

# Year where the sigmoid stops.
CURVE_END: int = 2026 if is_sigmoid else 2030

# Height of the limit of formalizability as a fraction of the Y axis.
LIMIT: float = 0.55

# Y-intercept of the curve.
BASELINE = 0.08

# Ceiling of the curve at CURVE_END.
CEILING = 0.50

# Years after PRESENT for the takeoff curve to approach the top of the Y axis.
TAKEOFF_YEARS = 5

# Font.
plt.rcParams["font.family"] = "serif"
plt.rcParams["font.serif"] = ["TeX Gyre Termes"]

# Example tasks.
Y_LABELS = [
    "Plowing",
    "Weaving",
    "Arithmetic",
    "Data entry",
    "Travel booking",
    "Making music",
    "Customer support",
    "Writing essays",
    "Arguing online",
]


# Draw the curve of progress.
MIDPOINT = 1890
STEEPNESS = 0.035
t = np.linspace(X_MIN, CURVE_END, 500)
sigmoid = 1 / (1 + np.exp(-STEEPNESS * (t - MIDPOINT)))
# rescale so the curve runs from BASELINE up toward CEILING
y = BASELINE + (CEILING - BASELINE) * (sigmoid - sigmoid[0]) / (1 - sigmoid[0])
fig, ax = plt.subplots(figsize=(8, 5))

# Soft green gradient fill for the Human-Complete Region, darker at the top
# and lighter toward the Limit of Formalizability.
green_gradient = LinearSegmentedColormap.from_list(
    "human_complete", ["#a3d1a3", "#eef7ee"]
)
gradient = np.linspace(0, 1, 256).reshape(-1, 1)
ax.imshow(
    gradient,
    cmap=green_gradient,
    aspect="auto",
    origin="upper",
    extent=(X_MIN, X_MAX, LIMIT, 1),
    alpha=0.6,
    zorder=0,
)

ax.plot(t, y, color="tab:blue", linewidth=1.5)

if is_takeoff:
    # Steep exponential takeoff continuing from the end of the sigmoid,
    # asymptotically approaching the top of the Y axis within a few years.
    k = -np.log(0.01) / TAKEOFF_YEARS
    t_takeoff = np.linspace(CURVE_END, X_MAX, 500)
    y_takeoff = 1.0 - (1.0 - y[-1]) * np.exp(-k * (t_takeoff - CURVE_END))
    ax.plot(t_takeoff, y_takeoff, color="tab:red", linewidth=1.5, linestyle="--")

# Draw the "Limit of Formalizability".
ax.axhline(LIMIT, linestyle=":", color="black", linewidth=1)
ax.text(
    X_MIN + 5,
    LIMIT + 0.015,
    "Limit of Formalizability",
    ha="left",
    va="bottom",
    fontsize=11,
    style="italic",
)

# Vertical line at the present day.
ax.axvline(PRESENT, linestyle="--", color="gray", linewidth=0.5)

# Axes.
ax.set_xlim(X_MIN, X_MAX)
ax.set_ylim(0, 1)
ax.set_xlabel("Time")

# Example tasks.
yticks = np.linspace(0.075, 0.925, len(Y_LABELS))
ax.set_yticks(yticks)
ax.set_yticklabels(Y_LABELS)

# X ticks: round half-centuries plus the present day.
xticks = sorted(set(np.arange(X_MIN, X_MAX + 1, 50)) | {PRESENT})
ax.set_xticks(xticks)

# Style.
ax.spines[["top", "right"]].set_visible(True)
ax.grid(axis="y", linewidth=0.3, alpha=0.4)

# Region labels.
label_x: int = 1950
ax.text(
    label_x,
    0.78,
    "Human-Complete Region",
    ha="center",
    va="center",
    fontsize=13,
    style="italic",
    color="black",
    alpha=0.6,
)
ax.text(
    label_x,
    0.27,
    "Formalizable Region",
    ha="center",
    va="center",
    fontsize=13,
    style="italic",
    color="black",
    alpha=0.6,
)

# Render.
out_name = "graph1.png" if is_sigmoid else "graph2.png"
fig.savefig(out_name, dpi=450, bbox_inches="tight")
