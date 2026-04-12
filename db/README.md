
## DB

### vérifications

* faces sans roches

```sql
SELECT f.*
FROM faces f
LEFT JOIN roches r ON r.idroche = f.idroche
WHERE r.idroche IS NULL;
```

* figures sans faces

```sql
SELECT g.*
FROM figures g
LEFT JOIN faces f ON f.idface = g.idface
WHERE f.idface IS NULL;
```