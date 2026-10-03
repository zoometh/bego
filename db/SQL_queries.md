## Roches

### géométries

#### verifications

Géométries qui ne rentre pas dans le RGF93

```sql
SELECT
    idroche,
    ST_X(ST_GeometryN(geom, 1)) AS x,
    ST_Y(ST_GeometryN(geom, 1)) AS y
FROM roches
WHERE geom IS NOT NULL
  AND (
       ST_X(ST_GeometryN(geom, 1)) < -378305
    OR ST_X(ST_GeometryN(geom, 1)) > 1320649
    OR ST_Y(ST_GeometryN(geom, 1)) < 6005281
    OR ST_Y(ST_GeometryN(geom, 1)) > 7235613
  );
```

Vue + access, `roches_gs` pour la distinguer de la table `roches`

```sql
CREATE OR REPLACE VIEW roches_gs AS
SELECT idroche as id, 
alphalabel as num,
nom, 
zone, 
groupe, 
roche, 
geom
FROM roches;
WHERE idroche NOT LIKE '4.3.4C'

GRANT USAGE ON SCHEMA public TO geoserver_user;
GRANT SELECT ON public.roches_gs TO geoserver_user;
```

### counts

Les comptes par type de gravures sont dans la section **figures**

```sql
CREATE OR REPLACE VIEW roches__figunfig_tot AS
 SELECT roches.idroche as id,
    roches.alphalabel as num,
    roches.nom,
    count(figures.num) AS total_fig,
    geom
  FROM roches, figures
  WHERE roches.idroche::text = figures.idroche::text
  AND roches.idroche NOT LIKE '4.3.4C'
  GROUP BY roches.idroche, roches.nom, roches.x, roches.y
  ORDER BY (count(figures.num)) DESC;

GRANT SELECT ON public.roches__figunfig_tot TO geoserver_user;
```

Mettre la pkey de la vue dans la table `gt_pk_metadata_table` (pour QGIS)

```sql
INSERT INTO public.gt_pk_metadata_table
    (table_schema, table_name, pk_column, pk_column_idx, pk_policy)
VALUES
    ('public', 'roches__figunfig_tot', 'id', 1, 'assigned');
```

## Figures
> ⚠️ MAJUSCULE _vs_ minuscule dans le nom de la vue

Comprend les superpositions

```sql
CREATE OR REPLACE VIEW roches_ff_tot AS
 SELECT roches.idroche as id,
    roches.alphalabel as num,
    roches.nom,
    roches.img_roche,
    roches.img_face,
    count(figures.num) AS total_fig,
    geom
  FROM roches, figures
  WHERE roches.idroche::text = figures.idroche::text AND
  figures.prem='R' AND figures.deux='f'
  GROUP BY roches.idroche, roches.nom, roches.x, roches.y
  ORDER BY (count(figures.num)) DESC;

comment on view roches_ff_tot is 'Figures a franges';

GRANT SELECT ON public.roches_ff_tot TO geoserver_user;

INSERT INTO public.gt_pk_metadata_table
    (table_schema, table_name, pk_column, pk_column_idx, pk_policy)
VALUES
    ('public', 'roches_ff_tot', 'id', 1, 'assigned');
```

### superpositions

<!-- Servies sur l'API (pas le GS) -->

```sql
CREATE OR REPLACE VIEW superpositions AS
 SELECT idsuper as id,
    labelfig_1 as fig1,
    labelfig_1 as fig2,
    relation as fig1_to_fig2,
    description,
    img_superposition,
  FROM superpositions
  ORDER BY labelfig_1 DESC;
```
