
Create Table "Florida Insurance" (
	policy_id INTEGER,
	statecode INTEGER,
	county INTEGER,
	eq_site_limit INTEGER,
	hu_site_limit INTEGER,
	fl_site_limit INTEGER,
	tiv_2011 INTEGER,
	tiv_2012 INTEGER,
	eq_site_deductible INTEGER,
	fl_site_deductible INTEGER,
	fr_site_deductible INTEGER,
	point_latitude INTEGER,
	point_longitude INTEGER,
	line INTEGER,
	construction INTEGER,
	point_granularity INTEGER
)
.mode.csv
.import FL_insurance_sample.csv
.print
sELECT * FROM Florida Insurance LIMIT 10;
.print
SELECT count(distinct county_id) from Florida Insurance
.print 'Average appreciation'
SELECT AVG(tiv_2012) FROM Florida Insurance;
SELECT AVG(tiv_2011) FROM Florida Insuarnce;
SELECT AVG(tiv_2012-tiv_2011);
.print''
.print Frequency Table
SELECT construction, COUNT(*) FROM Florida Insurance GROUP BY construction
