"""
python increase_contrast.py /Users/oliver/code/zed/assets/themes/one/one.json --saturation 1.8 --value 1.1 > themes/high.json
"""

import re
import json
import colorsys

SATURATION_FACTOR = 1.2
VALUE_FACTOR = 1.1


def increase_contrast(color):
    # Extract RGB values from the color string
    r, g, b, a = (
        int(color[1:3], 16) / 255,
        int(color[3:5], 16) / 255,
        int(color[5:7], 16) / 255,
        int(color[7:9], 16) / 255,
    )

    # Convert RGB to HSV
    h, s, v = colorsys.rgb_to_hsv(r, g, b)

    # Increase contrast by adjusting saturation and value
    s = min(1.0, s * SATURATION_FACTOR)  # Increase saturation by 20%
    v = min(1.0, v * VALUE_FACTOR)  # Increase value by 10%

    # Convert HSV back to RGB
    r, g, b = colorsys.hsv_to_rgb(h, s, v)

    # Convert RGB values back to hexadecimal format
    r, g, b = int(r * 255), int(g * 255), int(b * 255)
    return f"#{r:02x}{g:02x}{b:02x}{int(a * 255):02x}"


def increase_theme_contrast(theme):
    theme["name"] = f"{theme['name']} (High Contrast)"
    for k, v in theme["style"]["syntax"].items():
        v["color"] = increase_contrast(v["color"])
    return theme


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="Increase contrast of a theme")
    parser.add_argument("theme_path", type=str, help="Path to the theme file")
    parser.add_argument(
        "--saturation", type=float, default=1.2, help="Factor to increase saturation by"
    )
    parser.add_argument(
        "--value", type=float, default=1.1, help="Factor to increase value by"
    )
    args = parser.parse_args()

    SATURATION_FACTOR = args.saturation
    VALUE_FACTOR = args.value
    theme_path = args.theme_path

    with open(theme_path, "r") as file:
        theme = json.load(file)

    theme["name"] = f"{theme['name']} (High Contrast)"

    theme["themes"] = [
        increase_theme_contrast(subtheme) for subtheme in theme["themes"]
    ]

    print(json.dumps(theme, indent=4))
