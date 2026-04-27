/* =========================
   CONFIGURATION
   ========================= */

const WIDTH = window.innerWidth;
const HEIGHT = window.innerHeight;
const FLAG_SIZE = 20;

/* =========================
   PROJECTION & PATH
   ========================= */

const projection = d3.geoNaturalEarth1()
  .scale(WIDTH / 3)
  .center([72, 0])
  .translate([WIDTH / 2, HEIGHT / 2]);

const path = d3.geoPath().projection(projection);

/* =========================
   SVG
   ========================= */

const svg = d3.select("#map")
  .append("svg")
  .attr("width", WIDTH)
  .attr("height", HEIGHT);

// Fond océan
svg.append("rect")
  .attr("width", WIDTH)
  .attr("height", HEIGHT)
  .attr("fill", "#82C0C9");

// Groupe unique pour le zoom (land + graticule + flags)
const gZoom = svg.append("g").attr("class", "zoomable");

// Pays
const gLand = gZoom.append("g").attr("class", "land");

// Graticules
const graticule = d3.geoGraticule();
const gGraticule = gZoom.append("path")
  .datum(graticule())
  .attr("d", path)
  .attr("fill", "none")
  .attr("stroke", "#fff")
  .attr("stroke-width", 0.4)
  .attr("stroke-opacity", 0.4)
  .attr("stroke-dasharray", "4 3");

// Drapeaux (au-dessus de tout)
const gFlags = gZoom.append("g").attr("class", "flags");

/* =========================
   TOOLTIP
   ========================= */

const tooltip = d3.select("#tooltip");

/* =========================
   DONNÉES GLOBALES
   ========================= */

let tradingPosts = [];
let flagConfig = {};
let currentYear = 1498;
let currentK = 1;
const activeEmpires = new Set();
const panelFlagImages = new Map();

/* =========================
   CHARGEMENT DES DONNÉES
   ========================= */

Promise.all([
  fetch("https://cdn.jsdelivr.net/npm/world-atlas@2/countries-110m.json").then(r => r.json()),
  d3.csv("data/trading_posts_final.csv"),
  fetch("config/flags.json").then(r => r.json())
]).then(([world, csv, flags]) => {

  // --- Pays ---
  const countries = topojson.feature(world, world.objects.countries);
  gLand.selectAll("path")
    .data(countries.features)
    .enter()
    .append("path")
    .attr("d", path)
    .attr("fill", "#ce824b")
    .attr("stroke", "#fff")
    .attr("stroke-width", 0.5);

  // --- Trading posts ---
  tradingPosts = csv.map(d => ({
    name: d.Nom,
    altNames: d["Autres Noms"],
    lon: +d.Longitude,
    lat: +d.Latitude,
    empire: d.Empire,
    startYear: +d.start_year,
    endYear: +d.end_year
  }));

  flagConfig = flags;

  // Initialiser les empires actifs et le panneau de filtre
  const empires = [...new Set(tradingPosts.map(d => d.empire))];
  empires.forEach(e => activeEmpires.add(e));
  buildFilterPanel(empires);

  // Précréer tous les marqueurs (invisibles par défaut)
  buildAllMarkers();
  updateMap(currentYear);
});

/* =========================
   RÉSOLUTION DU DRAPEAU
   ========================= */

function getFlagFile(empire, year) {
  const rules = flagConfig[empire];
  if (!rules) return null;

  const rule = rules.find(r => year >= r.from && year < r.to);
  return rule ? `flags/${rule.icon}` : null;
}

/* =========================
   PRÉCRÉATION DES MARQUEURS
   ========================= */

function buildAllMarkers() {
  const r = FLAG_SIZE / 2;

  tradingPosts.forEach(d => {
    const [x, y] = projection([d.lat, d.lon]);
    const firstRule = flagConfig[d.empire]?.[0];
    const flagFile = firstRule ? `flags/${firstRule.icon}` : null;

    const g = gFlags.append("g")
      .attr("class", "marker")
      .attr("transform", `translate(${x}, ${y})`)
      .datum(d)
      .style("display", "none");

    const content = g.append("g")
      .attr("class", "mc")
      .attr("transform", `scale(${1 / currentK})`);

    if (flagFile) {
      content.append("image")
        .attr("href", flagFile)
        .attr("width", FLAG_SIZE)
        .attr("height", FLAG_SIZE)
        .attr("x", -r)
        .attr("y", -r)
        .attr("preserveAspectRatio", "xMidYMid slice")
        .style("clip-path", "circle(50%)");

      content.append("circle")
        .attr("class", "flag-border")
        .attr("r", r)
        .attr("fill", "none")
        .attr("stroke", "#fff")
        .attr("stroke-width", 1.5);
    } else {
      content.append("circle")
        .attr("class", "flag-border")
        .attr("r", 4)
        .attr("fill", "#e74c3c")
        .attr("stroke", "#fff")
        .attr("stroke-width", 1);
    }

    // interactions
    g.on("mouseover", () => {
      tooltip.style("opacity", 1)
        .html(`<strong>${d.name}</strong>
               ${d.altNames ? `<em>${d.altNames}</em><br>` : ""}
               ${d.empire}<br>
               ${d.startYear} – ${d.endYear}`);
    })
    .on("mousemove", (event) => {
      tooltip
        .style("left", (event.pageX + 12) + "px")
        .style("top", (event.pageY - 28) + "px");
    })
    .on("mouseout", () => {
      tooltip.style("opacity", 0);
    });
  });
}

/* =========================
   MISE À JOUR DE LA CARTE
   ========================= */

function updateMap(year) {
  currentYear = year;
  updateFilterFlags(year);

  gFlags.selectAll(".marker").each(function (d) {
    const visible = d.startYear <= year && d.endYear >= year && activeEmpires.has(d.empire);
    const el = d3.select(this);
    el.style("display", visible ? null : "none");

    // Mettre à jour le drapeau si visible
    if (visible) {
      const img = el.select("image");
      if (img.size()) {
        const flagFile = getFlagFile(d.empire, year);
        if (flagFile) img.attr("href", flagFile);
      }
    }
  });
}

/* =========================
   SLIDER TEMPOREL & AUTOPLAY
   ========================= */

const slider = document.getElementById("yearSlider");
const label = document.getElementById("yearLabel");
const playBtn = document.getElementById("playBtn");

label.textContent = slider.value;

slider.addEventListener("input", () => {
  const year = Number(slider.value);
  label.textContent = year;
  updateMap(year);
});

// --- Autoplay ---
let playing = false;
let playInterval = null;
const PLAY_SPEED_MS = 120; // ms entre chaque année

function togglePlay() {
  if (playing) {
    clearInterval(playInterval);
    playInterval = null;
    playBtn.innerHTML = '<i class="fa-solid fa-play"></i>';
  } else {
    if (Number(slider.value) >= 2000) {
      slider.value = 1498;
    }
    playBtn.innerHTML = '<i class="fa-solid fa-pause"></i>';
    playInterval = setInterval(() => {
      let year = Number(slider.value) + 1;
      if (year > 2000) {
        year = 2000;
        togglePlay();
        return;
      }
      slider.value = year;
      label.textContent = year;
      updateMap(year);
    }, PLAY_SPEED_MS);
  }
  playing = !playing;
}

playBtn.addEventListener("click", togglePlay);

/* =========================
   FILTRE PAR EMPIRE
   ========================= */

const filterBtn = document.getElementById("filterBtn");
const filterPanel = document.getElementById("filterPanel");

filterBtn.addEventListener("click", () => {
  filterPanel.classList.toggle("open");
});

function buildFilterPanel(empires) {
  const panel = document.getElementById("filterPanel");
  panel.innerHTML = "";

  empires.forEach(empire => {
    const label = document.createElement("label");

    const cb = document.createElement("input");
    cb.type = "checkbox";
    cb.checked = true;
    cb.addEventListener("change", () => {
      if (cb.checked) {
        activeEmpires.add(empire);
      } else {
        activeEmpires.delete(empire);
      }
      updateMap(currentYear);
    });

    label.appendChild(cb);

    const img = document.createElement("img");
    img.className = "flag-icon";
    label.appendChild(img);
    panelFlagImages.set(empire, img);

    const span = document.createElement("span");
    span.textContent = empire;
    label.appendChild(span);

    panel.appendChild(label);
  });

  updateFilterFlags(currentYear);
}

function updateFilterFlags(year) {
  panelFlagImages.forEach((img, empire) => {
    const flagFile = getFlagFile(empire, year);
    const firstRule = flagConfig[empire]?.[0];
    const fallback = firstRule ? `flags/${firstRule.icon}` : null;
    const src = flagFile || fallback;
    if (src) {
      img.src = src;
      img.style.display = "";
    } else {
      img.style.display = "none";
    }
  });
}

/* =========================
   ZOOM & PAN
   ========================= */

const scaleStr = new Map();

const zoom = d3.zoom()
  .scaleExtent([1, 12])
  .on("zoom", (event) => {
    const { transform } = event;
    currentK = transform.k;
    // 1 seule mise à jour DOM pour tout le contenu géo
    gZoom.attr("transform", transform);
    // 1 attribut par marqueur (counter-scale)
    const inv = `scale(${1 / transform.k})`;
    gFlags.selectAll(".mc").attr("transform", inv);
  });

svg.call(zoom);

/* =========================
   REDIMENSIONNEMENT
   ========================= */

window.addEventListener("resize", () => {
  const w = window.innerWidth;
  const h = window.innerHeight;

  svg.attr("width", w).attr("height", h);
  svg.select("rect").attr("width", w).attr("height", h);

  projection.scale(w / 3).center([72, 0]).translate([w / 2, h / 2]);

  // Redessiner les paths géo
  gLand.selectAll("path").attr("d", path);
  gGraticule.attr("d", path);

  // Repositionner les drapeaux
  gFlags.selectAll(".marker").each(function (d) {
    const [x, y] = projection([d.lon, d.lat]);
    d3.select(this).attr("transform", `translate(${x}, ${y})`);
  });
});
