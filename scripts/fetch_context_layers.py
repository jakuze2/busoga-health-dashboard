"""
fetch_context_layers.py
Context layers for the Busoga atlas map, from OpenStreetMap (Overpass API).
One-off; re-run occasionally (OSM changes slowly). Data (c) OpenStreetMap
contributors, ODbL. Run from the project root:

    python scripts/fetch_context_layers.py

Writes app/data/geo/osm_{roads,water,rivers,schools,markets,towns,health}.geojson,
clipped to the Busoga boundary (app/data/geo/region.geojson, from R/01_metadata.R).
"""
import json, time, requests
from shapely.geometry import shape, mapping, Point, LineString, Polygon
from shapely.ops import unary_union

OVERPASS = ["https://overpass-api.de/api/interpreter",
            "https://overpass.kumi.systems/api/interpreter"]
OUT = "app/data/geo/"

region = unary_union([shape(f["geometry"]) for f in json.load(open(OUT + "region.geojson"))["features"]])
clip = region.buffer(0.01)
minx, miny, maxx, maxy = clip.bounds
BB = f"{miny},{minx},{maxy},{maxx}"

def overpass(body):
    q = f"[out:json][timeout:600][bbox:{BB}];({body});out tags geom({BB});"
    for attempt in range(6):
        url = OVERPASS[attempt % len(OVERPASS)]
        try:
            r = requests.post(url, data={"data": q}, timeout=900,
                              headers={"User-Agent": "BusogaHealthForum-dashboard/1.0"})
            if r.status_code == 200:
                return r.json()["elements"]
            print("  overpass", r.status_code, "retrying")
        except requests.RequestException as e:
            print("  overpass error", str(e)[:80], "retrying")
        time.sleep(20 * (attempt + 1))
    raise SystemExit("Overpass failed: " + body[:60])

def geom(el):
    if el["type"] == "node":
        return Point(el["lon"], el["lat"])
    if el["type"] == "way" and "geometry" in el:
        pts = [(p["lon"], p["lat"]) for p in el["geometry"] if p]
        if len(pts) >= 4 and pts[0] == pts[-1]:
            return Polygon(pts)
        return LineString(pts) if len(pts) >= 2 else None
    if el["type"] == "relation":
        outers = []
        for m in el.get("members", []):
            if m.get("role") == "outer" and m.get("geometry"):
                pts = [(p["lon"], p["lat"]) for p in m["geometry"] if p]
                if len(pts) >= 2:
                    outers.append(LineString(pts))
        if not outers:
            return None
        from shapely.ops import polygonize, linemerge
        polys = list(polygonize(linemerge(unary_union(outers))))
        return unary_union(polys) if polys else unary_union(outers)
    return None

def write(name, feats, as_point=False, simplify=None):
    out = []
    for el in feats:
        g = geom(el)
        if g is None or g.is_empty:
            continue
        if as_point and g.geom_type != "Point":
            g = g.representative_point()
        if not g.intersects(clip):
            continue
        if g.geom_type != "Point":
            g = g.intersection(clip)
            if simplify:
                g = g.simplify(simplify, preserve_topology=True)
            if g.is_empty:
                continue
        t = el.get("tags", {})
        props = {k: t.get(k) for k in ("name", "highway", "amenity", "place", "waterway", "shop",
                                       "natural", "isced:level", "school:level", "operator:type") if t.get(k)}
        if name == "schools":
            props["category"] = school_category(t)
        out.append({"type": "Feature", "properties": props, "geometry": mapping(g)})
    json.dump({"type": "FeatureCollection", "features": out},
              open(OUT + f"osm_{name}.geojson", "w", encoding="utf-8"))
    print(f"osm_{name}: {len(out)} features")
    if name == "schools":
        import collections
        print("  ", collections.Counter(f["properties"]["category"] for f in out).most_common())

import re
def school_category(t):
    """Education level from OSM tags first (amenity, isced:level, school:level), then the name."""
    a = t.get("amenity", "")
    nm = (t.get("name") or "").lower()
    lvl = " ".join(t.get(k) or "" for k in ("isced:level", "school:level", "school")).lower()
    if a == "university" or "university" in nm:
        return "University"
    if (a == "kindergarten"
            or re.search(r"nursery|kindergarten|day ?care|pre-?primary|pre-?school|\becd\b|baby class", nm)
            or re.search(r"\b0\b|pre-?primary|kindergarten|nursery", lvl)):
        return "Pre-primary"
    if re.search(r"technical|vocational|\bvti\b|polytechnic|\bbtvet\b|\btrade\b|farm school", nm):
        return "Technical / vocational"
    # "X College" in Uganda is very often a secondary school, so only these patterns mean tertiary
    if (a == "college"
            or re.search(r"institute|college of|school of (nursing|health|midwif)|teachers.? college|\bptc\b|seminary", nm)
            or re.search(r"\b[5-8]\b|tertiary|higher", lvl)):
        return "Tertiary college / institute"
    if (re.search(r"secondary|\bs\.? ?s\.?\b|\bsss\b|high school|\bcollege\b|\bseed\b|comprehensive", nm)
            or re.search(r"\b[23]\b|secondary", lvl)):
        return "Secondary"
    if (re.search(r"primary|\bp/?s\b|\bp\.s\.?|junior|\bupe\b|\bc/?u\b", nm)
            or re.search(r"\b1\b|primary", lvl)):
        return "Primary"
    return "Unclassified"

only = __import__("sys").argv[1:]
layers = [
    ("roads",   'way["highway"~"^(trunk|primary|secondary|tertiary)$"];', False, 0.0003),
    ("water",   'way["natural"="water"];relation["natural"="water"];way["water"];', False, 0.0005),
    ("rivers",  'way["waterway"~"^(river|canal)$"];', False, 0.0003),
    ("schools", 'node["amenity"~"^(school|kindergarten|college|university)$"];'
                'way["amenity"~"^(school|kindergarten|college|university)$"];', True, None),
    ("markets", 'node["amenity"="marketplace"];way["amenity"="marketplace"];', True, None),
    ("towns",   'node["place"~"^(city|town|suburb)$"];', True, None),
    ("health",  'node["amenity"~"^(hospital|clinic|doctors)$"];way["amenity"~"^(hospital|clinic|doctors)$"];'
                'node["healthcare"];', True, None),
    ("commerce", 'node["amenity"~"^(bank|atm|fuel|marketplace|bureau_de_change|money_transfer|microfinance)$"];'
                 'way["amenity"~"^(bank|fuel|marketplace)$"];'
                 'node["shop"~"^(supermarket|wholesale|agrarian|hardware|farm|mobile_phone|money_lender)$"];'
                 'way["shop"~"^(supermarket|wholesale|agrarian|hardware)$"];', True, None),
]
for name, body, as_point, simp in layers:
    if only and name not in only:
        continue
    print("fetching", name)
    write(name, overpass(body), as_point, simp)
    time.sleep(5)
