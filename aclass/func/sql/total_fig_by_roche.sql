create or replace view total_fig_by_roche as
select roches.idroche, nom, x, y, count(figures.num) as total_fig
from roches, figures
where roches.idroche = figures.idroche
group by roches.idroche, nom, x, y
order by total_fig desc