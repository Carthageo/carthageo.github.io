# Trading Posts across the Indian Ocean

Interactive map showing European and Asian trading posts (factories) around the Indian Ocean from 1498 to 2000.

## Features

- D3.js map with Natural Earth projection
- Time slider with autoplay to browse years 1498–2000
- Flag markers for each empire controlling a trading post
- Filter panel to show/hide empires
- Zoom & pan

## Data

- `data/trading_posts_final.csv` — trading posts with coordinates, empire, and date range
- `config/flags.json` — flag icon rules per empire and period
- `flags/` — SVG flag files

## Usage

Serve the folder with any local HTTP server:

```
npx serve .
```

Then open `http://localhost:3000` in a browser.

## Sources

EFIO, 2026.
