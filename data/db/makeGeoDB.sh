#!/bin/bash

# let's assume we are the postgres user or have access to it.

COMMAND=$1
DBNAME=$2
DBROLE=$3
DBPASS=$3
SCHEMAFILE=$5

DBHOST="dbs.int.citiviz.com"
DBPORT="5432"
ADMINROLE="citiviz"
ADMINPASS="citiviz"

PSQLARGS="-h ${DBHOST} -p ${DBPORT}"
PSQLADMIN="${PSQLARGS} -U ${ADMINROLE}"
PSQLUSER="${PSQLARGS} -U ${DBROLE}"

mkdir -p ~/.tmp
PGPASSFILE=$(mktemp ~/.tmp/.pgpassXXX)

mkpassfile() {
  echo "${DBHOST}:${DBPORT}:*:${ADMINROLE}:${ADMINPASS}" > ${PGPASSFILE}
  echo "${DBHOST}:${DBPORT}:*:${DBROLE}:${DBPASS}" >> ${PGPASSFILE}
  cat ${PGPASSFILE}
  export PGPASSFILE
}

rmpassfile() {
  rm ${PGPASSFILE}
  unset PGPASSFILE
}

dump() {
        echo $PGPASSFILE
	set -vx
	pg_dump ${PSQLADMIN} -Fc "${DBNAME}" > "${DBNAME}$(date +%H%m%d-%H%M).pgsql.dump"
	set +vx
}

drop() {
	set -vx
	dropdb ${PSQLADMIN} "$DBNAME"
	dropuser ${PSQLADMIN} "$DBROLE"
	set +vx
}

create() {
	set -vx
        createuser ${PSQLADMIN} -D -e -E -i -l -R -S "${DBROLE}"
	createdb ${PSQLADMIN} -O "${DBROLE}" -E UTF-8 "${DBNAME}"
	cat << EOF | psql ${PSQLADMIN} "${DBNAME}"
ALTER USER ${DBROLE} WITH PASSWORD '${DBPASS}';
CREATE EXTENSION postgis;
CREATE EXTENSION postgis_topology;
ALTER SCHEMA public OWNER TO ${DBROLE};
ALTER TABLE public.spatial_ref_sys OWNER TO ${DBROLE};
ALTER SCHEMA topology OWNER TO ${DBROLE};
ALTER TABLE topology.topology OWNER TO ${DBROLE};
ALTER TABLE topology.layer OWNER TO ${DBROLE};
ALTER SEQUENCE topology.topology_id_seq OWNER TO ${DBROLE};
EOF
	cat "${SCHEMAFILE}" | psql ${PSQLUSER} "${DBNAME}"
	set +vx
}

populate() {
	set -vx
	echo "Not implemented"
	set +vx
}

cleanup() {
	set -vx
	echo "Not implemented"
	set +vx
}

usage() {
	cat << EOF
Usage: $0 <command> <dbname> <dbrole> <rolepass> <schema_file>

commands:
	dump: 		make a backup of the database
	drop: 		drop both user and database
	create: 	create user and database, add postgis extensions and create the schema
	populate:       fill the database with data
	cleanup: 	remove data from the database
	all: 	        dump && cleanup && create && populate
EOF
}

if [ $# -lt 5 ]; then
	usage
        rmpassfile
	exit 1
fi

mkpassfile
case $COMMAND in
	dump)
		dump
		;;
	drop)
		drop
		;;
	create)
		create
		;;
	populate)
		populate
		;;
	cleanup)
		cleanup
		;;
	all)
		dump
		drop
		create
		populate
		;;
	*)
		usage
		;;
esac
rmpassfile
