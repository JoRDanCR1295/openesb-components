--Note that all the names used in this file are prefixed with BPELSE_USER. If you 
--want to create a user with different name and want that user to have separate 
--tablespace, etc, simply find and replace BPELSE_USER with the desired user name.

-- before create postgresql objects need check locale avalibility
-- locale -a | grep ru_RU.UTF-8


--You need to login to PostgreSQL as postgres to run these commands (actually, all of
--these commands can be run using the system user except the last set as noted). 
--If you are using pgsql, on command prompt enter:
--pgsql

create user esb_bpelse_owner with
    password 'se#passwd#bpel';

create tablespace esb_bpelse_space
    owner esb_bpelse_owner
    location '/var/lib/postgresql/9.0/main/esb/bpelse';

create database esb_bpelse_db with
    owner esb_bpelse_owner
    tablespace esb_bpelse_space
    encoding 'unicode'
    LC_CTYPE 'ru_RU.UTF-8'
    LC_COLLATE 'ru_RU.UTF-8'
    template template0;

\c esb_bpelse_db

create schema esb_bpelse_owner AUTHORIZATION esb_bpelse_owner;
