## Create database

### Design model and create database in MySQL-workbench

Follow the instruction in file `sql_summary.md` section `Forward engineering` to create a diagram, which is saved as `geography_model.mwv` and proceed to create the database called `geography`. Copy or save the SQL script that creates the database for future use.

```mysql
-- MySQL Workbench Forward Engineering

SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0;
SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0;
SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='TRADITIONAL,ALLOW_INVALID_DATES';

-- -----------------------------------------------------
-- Schema geography
-- -----------------------------------------------------

-- -----------------------------------------------------
-- Schema geography
-- -----------------------------------------------------
CREATE SCHEMA IF NOT EXISTS `geography` DEFAULT CHARACTER SET utf8 ;
USE `geography` ;

-- -----------------------------------------------------
-- Table `geography`.`State`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `geography`.`State` (
  `state_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `fips` CHAR(2) NOT NULL,
  `name` VARCHAR(45) NOT NULL,
  `abbr` CHAR(2) NOT NULL,
  `lon` FLOAT NULL,
  `lat` FLOAT NULL,
  UNIQUE INDEX `FIPS_UNIQUE` (`fips` ASC),
  UNIQUE INDEX `name_UNIQUE` (`name` ASC),
  UNIQUE INDEX `abbr_UNIQUE` (`abbr` ASC),
  PRIMARY KEY (`state_id`),
  UNIQUE INDEX `state_id_UNIQUE` (`state_id` ASC))
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `geography`.`Metro`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `geography`.`Metro` (
  `metro_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `fips` CHAR(5) NOT NULL,
  `name` VARCHAR(128) NOT NULL,
  `lon` FLOAT NULL,
  `lat` FLOAT NULL,
  PRIMARY KEY (`metro_id`),
  UNIQUE INDEX `metro_id_UNIQUE` (`metro_id` ASC))
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `geography`.`County`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `geography`.`County` (
  `county_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `fips` CHAR(3) NOT NULL,
  `name` VARCHAR(128) NOT NULL,
  `state_id` INT UNSIGNED NOT NULL,
  `metro_id` INT UNSIGNED NULL,
  `lon` FLOAT NULL,
  `lat` FLOAT NULL,
  PRIMARY KEY (`county_id`),
  INDEX `fk_County_State1_idx` (`state_id` ASC),
  UNIQUE INDEX `county_id_UNIQUE` (`county_id` ASC),
  INDEX `fk_County_Metro1_idx` (`metro_id` ASC),
  CONSTRAINT `fk_County_State1`
    FOREIGN KEY (`state_id`)
    REFERENCES `geography`.`State` (`state_id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_County_Metro1`
    FOREIGN KEY (`metro_id`)
    REFERENCES `geography`.`Metro` (`metro_id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `geography`.`Place`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `geography`.`Place` (
  `place_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `fips` CHAR(5) NOT NULL,
  `name` VARCHAR(128) NOT NULL,
  `state_id` INT UNSIGNED NOT NULL,
  `lon` FLOAT NULL,
  `lat` FLOAT NULL,
  PRIMARY KEY (`place_id`),
  UNIQUE INDEX `place_id_UNIQUE` (`place_id` ASC),
  CONSTRAINT `fk_Place_State2`
    FOREIGN KEY (`state_id`)
    REFERENCES `geography`.`State` (`state_id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `geography`.`County_Subdivision`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `geography`.`County_Subdivision` (
  `cousub_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `fips` CHAR(5) NOT NULL,
  `name` VARCHAR(128) NOT NULL,
  `state_id` INT UNSIGNED NOT NULL,
  `county_id` INT UNSIGNED NOT NULL,
  `metro_id` INT UNSIGNED NOT NULL,
  `lon` FLOAT NULL,
  `lat` FLOAT NULL,
  PRIMARY KEY (`cousub_id`),
  INDEX `fk_Town_State1_idx` (`state_id` ASC),
  INDEX `fk_Town_County1_idx` (`county_id` ASC),
  UNIQUE INDEX `cousub_id_UNIQUE` (`cousub_id` ASC),
  INDEX `fk_County_Subdivision_Metro1_idx` (`metro_id` ASC),
  CONSTRAINT `fk_Town_State1`
    FOREIGN KEY (`state_id`)
    REFERENCES `geography`.`State` (`state_id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_Town_County1`
    FOREIGN KEY (`county_id`)
    REFERENCES `geography`.`County` (`county_id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT `fk_County_Subdivision_Metro1`
    FOREIGN KEY (`metro_id`)
    REFERENCES `geography`.`Metro` (`metro_id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


SET SQL_MODE=@OLD_SQL_MODE;
SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;
SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS;
```

## Fill in the `geography` database using R

To Build the SQL database of `geography` using 2010 decennial census

```R
library(data.table)
library(magrittr)
library(DBI)
library(totalcensus)
library(stringr)

# connect to database
con <- dbConnect(odbc::odbc(), "geography")
# list tables
# dbListTables(con)
# list field in table rental
# dbListFields(con, "County")
```

### Insert values from R

To write a dataframe to a database table, the dataframe must satisfy these requirements:

- Column names of the dataframe must be a subset of those of the database table.
- The columns of database table that have no default values or auto_increment must present in the dataframe.
- No need to be in the same order.

### State table
```R
# get the data
state <- read_decennial(2010, "US", summary_level = "state")
state_sql <- state[, .(fips = str_sub(GEOID, 8, 9),
                       name = NAME,
                       abbr = state,
                       population, lon, lat,
                       geoid = GEOID,
                       geoid_short = str_extract(GEOID, "[0-9]+$"))]

# in case of resetting the primary key to start from 1, from mysql command line
# ALTER TABLE State AUTO_INCREMENT=1;
dbWriteTable(con, "State", state_sql, append = TRUE)

# check the table from mysql command line and get id and abbr for later use
state_db <- dbReadTable(con, "State") %>%
    setDT() %>%
    .[, .(state_id, state = abbr)]
```

### Metro table
```R
metro <- read_decennial(2010, "US", summary_level = "310")
metro_sql <- metro[, .(fips = str_sub(GEOID, 8, 12),
                       name = NAME, 
                       population, lon, lat,
                       geoid = GEOID,
                       geoid_short = str_extract(GEOID, "[0-9]+$"))]

dbWriteTable(con, "Metro", metro_sql, append = TRUE)

metro_db <- dbReadTable(con, "Metro") %>%
    setDT() %>%
    .[, .(metro_id, metro_fips = fips)]
```


### Place table
```R
# get data. state is still in abbr not in state_id
place <- read_decennial(2010, "US", summary_level = "place")
place_sql <- place[, .(fips = str_sub(GEOID, 10, 14),
                       name = str_remove(NAME, ",[^,]+$"),
                       state, population, lon, lat,
                       geoid = GEOID,
                       geoid_short = str_extract(GEOID, "[0-9]+$"))] %>%
    # convert state to state_id
    state_db[., on = .(state)] %>%
    # reorder column to match Place table
    .[, .(fips, name, state_id, population, lon, lat, geoid, geoid_short)]

# write to table
dbWriteTable(con, "Place", place_sql, append=TRUE)
```


### County table
```R
county <- read_decennial(2010, "US", geo_headers = "CBSA", 
                        summary_level = "county")
county_sql <- county[, .(fips = str_sub(GEOID, 10, 12),
                       name = NAME,
                       state, 
                       metro_fips = CBSA,
                       population, lon, lat,
                       geoid = GEOID,
                       geoid_short = str_extract(GEOID, "[0-9]+$"))] %>%
    state_db[., on = .(state)] %>%
    metro_db[., on = .(metro_fips)] %>%
    .[, .(fips, name, state_id, metro_id, population, lon, lat, geoid, geoid_short)]

dbWriteTable(con, "County", county_sql, append = TRUE)

county_db <- dbReadTable(con, "County") %>%
    setDT() %>%
    .[, .(county_id, county_geoid_short = geoid_short)]
```


### County_Subdivision table

```R
cousub <- read_decennial(2010, "US", geo_headers = "CBSA", 
                        summary_level = "060")

cousub_sql <- cousub[, .(fips = str_sub(GEOID, 12, 16),
                         name = NAME,
                         population, lon, lat, state,
                       geoid = GEOID,
                       geoid_short = str_extract(GEOID, "[0-9]+$"),
                       metro_fips = CBSA)] %>%
    .[, county_geoid_short := str_sub(geoid_short, 1, 5)] %>%
    state_db[., on = .(state)] %>%
    metro_db[., on = .(metro_fips)] %>%
    county_db[., on = .(county_geoid_short)] %>%
    .[, .(fips, name, population, state_id, county_id, metro_id, lon, lat,
          geoid, geoid_short)]
    
dbWriteTable(con, "County_Subdivision", cousub_sql, append = TRUE)
```

### disconnect from a database
```R
dbDisconnect(con)
```

