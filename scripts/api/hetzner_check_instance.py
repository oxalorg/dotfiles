#!/usr/bin/env python3
"""
Check where a Hetzner Cloud server type (e.g., cx32) is currently available.

# /// script
# dependencies = [
#   "requests",
#   "rich",
# ]
# ///


Usage:
  export HETZNER_TOKEN=...
  python hcloud_check_availability.py --type cx32
"""

import argparse
import os
import sys
import time
from typing import Dict, List, Optional, Tuple

import requests

API_BASE = "https://api.hetzner.cloud/v1"
TIMEOUT = 15  # seconds


class HCloudAPIError(RuntimeError):
    pass


def _headers(token: str) -> Dict[str, str]:
    return {"Authorization": f"Bearer {token}"}


def _get_all(token: str, path: str, key: str) -> List[dict]:
    """
    Fetch all pages for a collection endpoint returning {key: [...], meta: {pagination: ...}}
    """
    results: List[dict] = []
    page = 1
    while True:
        url = f"{API_BASE}{path}"
        params = {"page": page, "per_page": 50}
        try:
            resp = requests.get(url, headers=_headers(token), params=params, timeout=TIMEOUT)
        except requests.RequestException as e:
            raise HCloudAPIError(f"Network error fetching {path}: {e}") from e
        if resp.status_code != 200:
            raise HCloudAPIError(f"HTTP {resp.status_code} fetching {path}: {resp.text}")
        data = resp.json()
        items = data.get(key, [])
        results.extend(items)
        pag = (data.get("meta") or {}).get("pagination") or {}
        next_page = pag.get("next_page")
        if not next_page:
            break
        page = next_page
        # brief pause to be polite
        time.sleep(0.05)
    return results


def find_server_type(token: str, name: str) -> Optional[dict]:
    """
    Find a server type by name (case-insensitive exact match). Returns the server type dict or None.
    """
    server_types = _get_all(token, "/server_types", "server_types")
    for st in server_types:
        if str(st.get("name", "")).lower() == name.lower():
            return st
    return None


def build_dc_index(datacenters: List[dict]) -> Dict[int, dict]:
    """
    Index datacenters by id for quick lookup.
    """
    return {dc["id"]: dc for dc in datacenters}


def main() -> int:
    parser = argparse.ArgumentParser(description="Check Hetzner Cloud server type availability by location/datacenter.")
    parser.add_argument(
        "--type",
        "-t",
        required=True,
        help="Server type name (e.g., cx32). Note: classic 'cx*' types are Intel; 'cpx*' are AMD; 'cax*' are ARM.",
    )
    parser.add_argument(
        "--show-supported",
        action="store_true",
        help="Also show datacenters/locations where the type is supported but not currently available.",
    )
    args = parser.parse_args()

    token = os.environ.get("HETZNER_TOKEN") or os.environ.get("HCLOUD_TOKEN")
    if not token:
        print("Error: please set HETZNER_TOKEN (or HCLOUD_TOKEN) environment variable.", file=sys.stderr)
        return 2

    try:
        st = find_server_type(token, args.type)
        if not st:
            print(f"Server type '{args.type}' not found. Tip: check exact name (e.g., cx32, cpx32, cax31).", file=sys.stderr)
            return 1

        st_id = st["id"]
        st_desc = st.get("description") or ""
        print(f"Found server type: {st['name']} (id={st_id}) {f'- {st_desc}' if st_desc else ''}")

        datacenters = _get_all(token, "/datacenters", "datacenters")

        available_rows: List[Tuple[str, str, str]] = []  # (dc_name, location_name, city/country)
        supported_not_available: List[Tuple[str, str, str]] = []

        for dc in datacenters:
            loc = dc.get("location") or {}
            location_name = loc.get("name") or "unknown-location"
            city_country = ", ".join([s for s in [loc.get("city"), loc.get("country")] if s])
            dc_name = dc.get("name") or f"dc-{dc.get('id')}"

            server_types_info = dc.get("server_types") or {}
            available_ids = set(server_types_info.get("available") or [])
            supported_ids = set(server_types_info.get("supported") or [])

            if st_id in available_ids:
                available_rows.append((dc_name, location_name, city_country))
            elif args.show-supported and st_id in supported_ids:
                supported_not_available.append((dc_name, location_name, city_country))

        if available_rows:
            print("\nCurrently AVAILABLE in:")
            for dc_name, loc_name, city_country in sorted(available_rows, key=lambda r: (r[1], r[0])):
                print(f"  - {loc_name:<8} | {dc_name:<10} | {city_country}")
        else:
            print("\nThis server type is not currently available in any datacenter.")

        if args.show-supported and supported_not_available:
            print("\nSupported (but not currently available) in:")
            for dc_name, loc_name, city_country in sorted(supported_not_available, key=lambda r: (r[1], r[0])):
                print(f"  - {loc_name:<8} | {dc_name:<10} | {city_country}")

        # Helpful note on locations:
        # Common Hetzner Cloud locations: nbg1 (Nuremberg), fsn1 (Falkenstein), hel1 (Helsinki), ash (Ashburn), hil (Hillsboro).
        # The 'location' object in each datacenter provides city/country for clarity.

        return 0

    except HCloudAPIError as e:
        print(f"API error: {e}", file=sys.stderr)
        return 3
    except KeyboardInterrupt:
        return 130


if __name__ == "__main__":
    raise SystemExit(main())