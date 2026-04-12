#%%
# Figures à franges (FF) par roche

import json
from pathlib import Path
from typing import Any, List, Dict

import psycopg2
from psycopg2.extras import RealDictCursor


def get_roches_with_total_ff(
    config_path: str = r"C:/Users/TH282424/Rprojects/bego/doc/conf/db_config.json"
) -> List[Dict[str, Any]]:
    """
    Connects to PostgreSQL using credentials stored in a JSON file
    and returns the query results as a list of dictionaries.

    Expected JSON structure:
    {
        "host": "157.136.252.188",
        "port": 5432,
        "database": "bego",
        "user": "username",
        "password": "password"
    }
    """
    config_file = Path(config_path)

    if not config_file.exists():
        raise FileNotFoundError(f"Config file not found: {config_file}")

    with config_file.open("r", encoding="utf-8") as f:
        db_config = json.load(f)

    query = """
        SELECT
            roches.idroche,
            roches.alphalabel,
            COUNT(roches.zone) AS total_ff,
            roches.plan,
            roches.image,
            roches.geom
        FROM roches, figures
        WHERE figures.prem LIKE 'R'
          AND figures.deux LIKE 'f'
          AND figures.idroche = roches.idroche
        GROUP BY roches.idroche, roches.alphalabel, roches.plan, roches.image, roches.geom
        ORDER BY total_ff DESC;
    """

    conn = None
    try:
        conn = psycopg2.connect(
            host=db_config["host"],
            port=db_config["port"],
            dbname=db_config["database"],
            user=db_config["user"],
            password=db_config["password"],
        )

        with conn.cursor(cursor_factory=RealDictCursor) as cur:
            cur.execute(query)
            results = cur.fetchall()

        return [dict(row) for row in results]

    finally:
        if conn is not None:
            conn.close()
            
results = get_roches_with_total_ff()

for row in results:
    print(row["idroche"], row["alphalabel"], row["total_ff"])

#%%