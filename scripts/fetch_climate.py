"""
fetch_climate.py
Climate and environment inputs for Busoga (no accounts needed):
  - CHIRPS v2.0 monthly rainfall, 0.05 deg, Jan 1981 to the latest month, Busoga box, one NetCDF
    (UCSB Climate Hazards Center, served by the IRI/LDEO Climate Data Library)
  - ERA5-Land daily maximum temperature at each district centroid: 2011 to last month
    (2011-2020 is the baseline for heat days)                      [Open-Meteo archive API]
  - CAMS global PM2.5, hourly, district centroids, from Sep 2022     [Open-Meteo air-quality API]
  - ECMWF SEAS5 seasonal forecast, ~6 months, all members, districts [Open-Meteo seasonal API]
Every Open-Meteo response is cached per point in data/climate/cache/, so a run stopped by the
free-tier rate limits resumes where it left off. Run from the project root after R/01_metadata.R:
    python scripts/fetch_climate.py            # incremental (re-fetches the recent period)
Then Rscript R/04b_climate.R builds app/data/climate.rds and outlook.rds.
"""
import json, os, time, datetime as dt
import requests
from shapely.geometry import shape

OUT = "data/climate/"; CACHE = OUT + "cache/"
os.makedirs(CACHE, exist_ok=True)
S = requests.Session(); S.headers["User-Agent"] = "BusogaHealthForum-dashboard/1.0"
today = dt.date.today()
last_month_end = today.replace(day=1) - dt.timedelta(days=1)

def get(url, params, tries=10):
    for k in range(tries):
        try:
            r = S.get(url, params=params, timeout=600)
            if r.status_code == 200:
                return r.json()
            msg = r.text[:100]
            wait = 3700 if "Hourly" in msg else 70 if "Minutely" in msg else 30 * (k + 1)
            if "Daily" in msg:
                raise SystemExit("Open-Meteo daily limit reached; re-run tomorrow (cached parts are kept).")
            print(f"    {r.status_code} {msg} - waiting {wait}s", flush=True)
        except requests.RequestException as e:
            print("    error", str(e)[:80], flush=True); wait = 30
        time.sleep(wait)
    raise SystemExit("failed: " + url)

def cached(name, url, params, max_age_days=None):
    f = CACHE + name + ".json"
    if os.path.exists(f):
        age = (time.time() - os.path.getmtime(f)) / 86400
        if max_age_days is None or age < max_age_days:
            return json.load(open(f))
    j = get(url, params); json.dump(j, open(f, "w")); time.sleep(12)
    return j

# ---- 1. CHIRPS monthly rainfall (Busoga box) ------------------------------------------------
f_chirps = OUT + "chirps_busoga_monthly.nc"
if not os.path.exists(f_chirps) or (time.time() - os.path.getmtime(f_chirps)) / 86400 > 20:
    url = ("https://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/.v2p0/.monthly/.global/.precipitation/"
           "X/32.75/34.05/RANGEEDGES/Y/-1.05/1.55/RANGEEDGES/data.nc")
    for k in range(5):
        try:
            r = S.get(url, timeout=1800)
            if r.status_code == 200 and r.content[:3] == b"CDF":
                open(f_chirps, "wb").write(r.content); break
        except requests.RequestException as e:
            print("    chirps error", str(e)[:80])
        time.sleep(60)
    print("CHIRPS:", os.path.getsize(f_chirps), "bytes")

# ---- district points -----------------------------------------------------------------------
dis = []
for f in json.load(open("app/data/geo/district.geojson", encoding="utf-8"))["features"]:
    p = shape(f["geometry"]).representative_point()
    dis.append((f["properties"]["uid"], f["properties"]["name"], round(p.y, 4), round(p.x, 4)))

# ---- 2. ERA5-Land daily Tmax, 2011 to last month ---------------------------------------------
rows = ["uid,date,tmax_c"]
for uid, name, lat, lon in dis:
    base = cached(f"era5_{uid}_2011_2020", "https://archive-api.open-meteo.com/v1/archive", dict(
        latitude=lat, longitude=lon, start_date="2011-01-01", end_date="2020-12-31",
        daily="temperature_2m_max", models="era5_land", timezone="Africa/Kampala"))
    recent = cached(f"era5_{uid}_2021_now", "https://archive-api.open-meteo.com/v1/archive", dict(
        latitude=lat, longitude=lon, start_date="2021-01-01", end_date=last_month_end.isoformat(),
        daily="temperature_2m_max", models="era5_land", timezone="Africa/Kampala"), max_age_days=20)
    for j in (base, recent):
        d = j["daily"]
        rows += [f"{uid},{t},{x}" for t, x in zip(d["time"], d["temperature_2m_max"]) if x is not None]
    print("ERA5-Land Tmax:", name, flush=True)
open(OUT + "era5land_tmax_district.csv", "w").write("\n".join(rows))

# ---- 3. CAMS PM2.5 -------------------------------------------------------------------------
rows = ["uid,date,pm25_daily"]
for uid, name, lat, lon in dis:
    j = cached(f"cams_{uid}", "https://air-quality-api.open-meteo.com/v1/air-quality", dict(
        latitude=lat, longitude=lon, start_date="2022-09-01", end_date=last_month_end.isoformat(),
        hourly="pm2_5", domains="cams_global", timezone="Africa/Kampala"), max_age_days=20)
    day = {}
    for t, v in zip(j["hourly"]["time"], j["hourly"]["pm2_5"]):
        if v is not None: day.setdefault(t[:10], []).append(v)
    rows += [f"{uid},{k},{sum(v) / len(v):.2f}" for k, v in sorted(day.items()) if len(v) >= 18]
    print("CAMS PM2.5:", name, flush=True)
open(OUT + "cams_pm25_district.csv", "w").write("\n".join(rows))

# ---- 4. ECMWF SEAS5 seasonal forecast ------------------------------------------------------
rows = ["uid,month,member,rain_mm,tmax_c"]
for uid, name, lat, lon in dis:
    d = cached(f"seas5_{uid}", "https://seasonal-api.open-meteo.com/v1/seasonal", dict(
        latitude=lat, longitude=lon, daily="precipitation_sum,temperature_2m_max", forecast_days=183,
        timezone="Africa/Kampala"), max_age_days=6)["daily"]
    members = sorted({k.split("_member")[1] for k in d if "_member" in k})
    for mem in members:
        pr = d.get(f"precipitation_sum_member{mem}"); tx = d.get(f"temperature_2m_max_member{mem}") or [None] * len(pr or [])
        if not pr: continue
        mon = {}
        for t, r, x in zip(d["time"], pr, tx):
            a = mon.setdefault(t[:7], [0.0, [], 0]); a[2] += 1
            if r is not None: a[0] += r
            if x is not None: a[1].append(x)
        for m, a in sorted(mon.items()):
            if a[2] >= 25:
                rows.append(f"{uid},{m.replace('-', '')},{mem},{a[0]:.2f},{(sum(a[1]) / len(a[1])) if a[1] else ''}")
    print("SEAS5:", name, flush=True)
open(OUT + "seas5_outlook_district.csv", "w").write("\n".join(rows))
open(OUT + "fetched.txt", "w").write(today.isoformat())
print("climate inputs complete")
