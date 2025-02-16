-- Open database
.open ':memory:'

-- Read in the florida insurance file
CREATE TABLE florida_insurance (
    policyID TEXT,
    statecode TEXT,
    county TEXT,
    eq_site_limit NUMERIC,
    hu_site_limit NUMERIC,
    fl_site_limit NUMERIC,
    fr_site_limit NUMERIC,
    tiv_2011 NUMERIC,
    tiv_2012 NUMERIC,
    eq_site_deductible NUMERIC,
    hu_site_deductible NUMERIC,
    fl_site_deductible NUMERIC,
    fr_site_deductible NUMERIC,
    point_latitude NUMERIC,
    point_longitude NUMERIC,
    line TEXT,
    construction TEXT,
    point_granularity INTEGER
	);
	
.mode csv
.import /home/ouecon010/DScourseS25/ProblemSets/PS3/FL_insurance_sample.csv florida_insurance
	
-- Print out the first 10 rows of the data set
SELECT *
FROM florida_insurance
LIMIT 10;

-- List which counties are in the sample
SELECT DISTINCT county
FROM florida_insurance
ORDER BY county;

-- Compute average property appreciation from 2011 to 2012
SELECT avg(tiv_2012 - tiv_2011) as avg_appreciation FROM florida_insurance;

-- Create frequency table of the construction variable 
SELECT 
    construction,
    COUNT(*) as count,
    ROUND(COUNT(*) * 100.0 / (SELECT COUNT(*) FROM florida_insurance), 2) as percentage
FROM florida_insurance
GROUP BY construction
ORDER BY count DESC;