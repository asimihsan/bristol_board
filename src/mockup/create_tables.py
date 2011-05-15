# ----------------------------------------------------------------------
# Copyright (c) 2011 Asim Ihsan (asim dot ihsan at gmail dot com)
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
# File: bristol_board/src/mockup/create_tables.py
#
# Assume that the PostgreSQL database is empty and create all
# the tables from scratch.
#
# Refer to bristol_board/doc/database_model.[vsd/png].
# ----------------------------------------------------------------------

from __future__ import with_statement
import os
import sys
import psycopg2
import logging
from string import Template
import datetime
import uuid
import pprint

# ----------------------------------------------------------------------
#   Logging.
# ----------------------------------------------------------------------
APP_NAME = 'create_tables'
logger = logging.getLogger(APP_NAME)
logger.setLevel(logging.DEBUG)
ch = logging.StreamHandler()
ch.setLevel(logging.DEBUG)
formatter = logging.Formatter('%(asctime)s - %(name)s - %(message)s')
ch.setFormatter(formatter)
logger.addHandler(ch)
logger = logging.getLogger(APP_NAME)
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
#   Constants to leave alone.
# ----------------------------------------------------------------------
DATABASE_NAME = "database"
DATABASE_USERNAME = "ubuntu"
DATABASE_PASSWORD = "password"
DATABASE_HOST = "localhost"
DATABASE_PORT = 5432

TEMPL_DB_CONNECT = Template("dbname=${dbname} user=${user} password=${password}")

# ----------------------------------------------------------------------
# Tables.
# ----------------------------------------------------------------------
# template
DROP_TEMPLATE_TABLE = """DROP TABLE IF EXISTS template;"""
CREATE_TEMPLATE_TABLE = """CREATE TABLE template (template_id UUID PRIMARY KEY,
                                                 ipad_contents TEXT NOT NULL,
                                                 android_contents TEXT,
                                                 html_contents TEXT NOT NULL);"""

# institution
DROP_INSTITUTION_TABLE = """DROP TABLE IF EXISTS institution;"""
CREATE_INSTITUTION_TABLE = """CREATE TABLE institution (institution_id UUID PRIMARY KEY,
                                                        name TEXT NOT NULL,
                                                        api_key UUID,
                                                        api_key_expiry TIMESTAMP);"""

# role
DROP_ROLE_TABLE = """DROP TABLE IF EXISTS role;"""
CREATE_ROLE_TABLE = """CREATE TABLE role (role_id UUID PRIMARY KEY,
                                          role_name TEXT NOT NULL,
                                          privileges HSTORE,
                                          obligations HSTORE);"""

# printout
DROP_PRINTOUT_TABLE = """DROP TABLE IF EXISTS printout;"""
CREATE_PRINTOUT_TABLE = """CREATE TABLE printout (printout_id UUID PRIMARY KEY,
                                                  revision_id UUID NOT NULL,
                                                  user_id UUID NOT NULL,
                                                  datetime_printed TIMESTAMP NOT NULL);"""

# user
DROP_USER_TABLE = """DROP TABLE IF EXISTS articheck_user;"""
CREATE_USER_TABLE = """CREATE TABLE articheck_user (user_id UUID PRIMARY KEY,
                                                    institution_id UUID NOT NULL,
                                                    username TEXT NOT NULL,
                                                    password TEXT NOT NULL,
                                                    role_id UUID NOT NULL,
                                                    email_address TEXT NOT NULL);"""

# annotation
DROP_ANNOTATION_TABLE = """DROP TABLE IF EXISTS annotation;"""
CREATE_ANNOTATION_TABLE = """CREATE TABLE annotation (annotation_id UUID PRIMARY KEY,
                                                      user_id UUID NOT NULL,
                                                      datetime_edited TIMESTAMP NOT NULL,
                                                      photograph_id UUID NOT NULL);"""

# condition report
DROP_CONDITION_REPORT_TABLE = """DROP TABLE IF EXISTS condition_report;"""
CREATE_CONDITION_REPORT_TABLE = """CREATE TABLE condition_report (revision_id UUID PRIMARY KEY,
                                                                  condition_report_id UUID NOT NULL,
                                                                  art_id UUID NOT NULL,
                                                                  user_id UUID NOT NULL,
                                                                  datetime_edited TIMESTAMP NOT NULL,
                                                                  contents TEXT NOT NULL,
                                                                  tags HSTORE,
                                                                  template_id UUID NOT NULL);"""

# note
DROP_NOTE_TABLE = """DROP TABLE IF EXISTS note;"""
CREATE_NOTE_TABLE = """CREATE TABLE note (note_id UUID PRIMARY KEY,
                                          revision_id UUID,
                                          user_id UUID,
                                          contents TEXT,
                                          datetime_edited TIMESTAMP);"""

# photograph
DROP_PHOTOGRAPH_TABLE = """DROP TABLE IF EXISTS photograph;"""
CREATE_PHOTOGRAPH_TABLE = """CREATE TABLE photograph (photograph_id UUID PRIMARY KEY,
                                                      user_id UUID NOT NULL,
                                                      s3_path TEXT,
                                                      datetime_taken TIMESTAMP NOT NULL,
                                                      condition_report_id UUID NOT NULL,
                                                      hash TEXT NOT NULL,
                                                      file_system_path TEXT);"""
                                    
# art
DROP_ART_TABLE = """DROP TABLE IF EXISTS art;"""
CREATE_ART_TABLE = """CREATE TABLE art (art_id UUID PRIMARY KEY,
                                        host_institution_art_id TEXT);"""

INSERT_STATEMENTS = [DROP_TEMPLATE_TABLE,
                     CREATE_TEMPLATE_TABLE,
                     DROP_INSTITUTION_TABLE,
                     CREATE_INSTITUTION_TABLE,
                     DROP_ROLE_TABLE,
                     CREATE_ROLE_TABLE,
                     DROP_PRINTOUT_TABLE,
                     CREATE_PRINTOUT_TABLE,
                     DROP_USER_TABLE,
                     CREATE_USER_TABLE,
                     DROP_ANNOTATION_TABLE,
                     CREATE_ANNOTATION_TABLE,
                     DROP_CONDITION_REPORT_TABLE,
                     CREATE_CONDITION_REPORT_TABLE,
                     DROP_NOTE_TABLE,
                     CREATE_NOTE_TABLE,
                     DROP_PHOTOGRAPH_TABLE,
                     CREATE_PHOTOGRAPH_TABLE,
                     DROP_ART_TABLE,
                     CREATE_ART_TABLE]
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
# Indexes.
# ----------------------------------------------------------------------
INDEX_CONDITION_REPORT_ID_ON_CONDITION_REPORT = """CREATE INDEX condition_report_id_on_condition_report on condition_report(condition_report_id);"""
INDEX_CONDITION_REPORT_ID_ON_PHOTOGRAPH = """CREATE INDEX condition_report_id_on_photograph on photograph(condition_report_id);"""
INDEX_REVISION_ID_ON_PRINTOUT = """CREATE INDEX revision_id_on_printout on printout(revision_id);"""
INDEX_PHOTOGRAPH_ID_ON_ANNOTATION = """CREATE INDEX photograph_id_on_annotation on annotation(photograph_id);"""
INDEX_API_KEY_ON_INSTITUTION = """CREATE INDEX api_key_on_institution on institution(api_key);"""
INDEX_USERNAME_ON_ARTICHECK_USER = """CREATE INDEX username_on_articheck_user on articheck_user(username);"""

INDEX_STATEMENTS = [INDEX_CONDITION_REPORT_ID_ON_CONDITION_REPORT,
                    INDEX_CONDITION_REPORT_ID_ON_PHOTOGRAPH,
                    INDEX_REVISION_ID_ON_PRINTOUT,
                    INDEX_PHOTOGRAPH_ID_ON_ANNOTATION,
                    INDEX_API_KEY_ON_INSTITUTION,
                    INDEX_USERNAME_ON_ARTICHECK_USER]
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
# Foreign key constraints.
# ----------------------------------------------------------------------
FOREIGN_KEY_STATEMENTS = []
# ----------------------------------------------------------------------

ALL_STATEMENTS = INSERT_STATEMENTS + INDEX_STATEMENTS + FOREIGN_KEY_STATEMENTS                  

# ----------------------------------------------------------------------
# insert_dummy_data() commands.
# ----------------------------------------------------------------------
INSERT_INSTITUTION = "INSERT INTO institution (institution_id, name, api_key, api_key_expiry) VALUES (%s, %s, %s, %s);"
RANDOM_INSTITUTION = "SELECT institution_id FROM institution ORDER BY RANDOM() LIMIT 1;"

INSERT_ROLE = "INSERT INTO role (role_id, role_name) VALUES (%s, %s);"
SELECT_CONSERVATOR = "SELECT role_id FROM role WHERE role_name = 'conservator';"

INSERT_USER = "INSERT INTO articheck_user (user_id, institution_id, username, password, role_id, email_address) VALUES (%s, %s, %s, crypt(%s, gen_salt('bf', 8)), %s, %s);"
# ----------------------------------------------------------------------
def insert_dummy_data(cur):
    logger = logging.getLogger("%s.insert_dummy_data" % (APP_NAME, ))
    logger.info("entry")
    
    NUMBER_INSTITUTIONS = 5
    NUMBER_USERS = 5
    
    logger.info("Inserting institutions...")
    for i in xrange(NUMBER_INSTITUTIONS):
        institution_id = uuid.uuid4()
        name = "Institution %s" % (i, )
        institution_id = str(uuid.uuid4())
        api_key = str(uuid.uuid4())
        api_key_expiry = (datetime.datetime.now() + datetime.timedelta(days=365)).replace(microsecond=0).isoformat(" ")
        args = (institution_id, name, api_key, api_key_expiry)
        logger.debug("institution insert args:\n%s" % (pprint.pformat(args), ))
        cur.execute(INSERT_INSTITUTION, args)
    
    logger.info("Inserting roles...")
    cur.execute(INSERT_ROLE, (str(uuid.uuid4()), "registrar"))
    cur.execute(INSERT_ROLE, (str(uuid.uuid4()), "conservator"))
    
    logger.info("Inserting users...")    
    for i in xrange(NUMBER_USERS):        
        cur.execute(RANDOM_INSTITUTION)        
        institution_id = cur.fetchone()[0]
        
        cur.execute(SELECT_CONSERVATOR)
        role_id = cur.fetchone()[0]
        
        user_id = str(uuid.uuid4())
        username = "user%s" % (i, )
        password = "pass%s" % (i, )
        email_address = "user@host.com"
        
        args = (user_id, institution_id, username, password, role_id, email_address)
        logger.debug("user insert args:\n%s" % (pprint.pformat(args), ))
        cur.execute(INSERT_USER, args)        

if __name__ == "__main__":
    logger.info("Starting main.  args: %s" % (sys.argv[1:], ))
    
    logger.debug("Opening database connection and cursor...")
    conn = psycopg2.connect(TEMPL_DB_CONNECT.substitute(dbname=DATABASE_NAME,
                                                        user=DATABASE_USERNAME,
                                                        password=DATABASE_PASSWORD,
                                                        host=DATABASE_HOST,
                                                        port=DATABASE_PORT))
    conn.set_isolation_level(psycopg2.extensions.ISOLATION_LEVEL_AUTOCOMMIT)
    cur = conn.cursor()    
    logger.debug("Opened database connection and cursor.")        
    try:
        for statement in ALL_STATEMENTS:
            logger.info("Executing: %s" % (statement, ))
            cur.execute(statement)        
        insert_dummy_data(cur)
    finally:
        logger.debug("Closing database connection and cursor...")
        conn.commit()
        cur.close()
        conn.close()
    