#!/bin/bash

# let's assume we are the postgres user or have access to it.

COMMAND=$1
DBNAME=$2
DBROLE=$3
SCHEMAFILE=$4


dump() {
	set -vx
	pg_dump -Fc "${DBNAME}" > "${DBNAME}$(date +%H%m%d-%H%M).pgsql.dump"
	set +vx
}

drop() {
	set -vx
	dropdb "$DBNAME"
	dropuser "$DBROLE"
	set +vx
}

create() {
	set -vx
	createuser -D -e -E -i -l -R -S "${DBROLE}"
	createdb -O "${DBROLE}" -E UTF-8 "${DBNAME}"
	psql -U postgres "${DBNAME}" -c "CREATE EXTENSION postgis; CREATE EXTENSION postgis_topology;"
	cat << EOF | psql -U postgres "${DBNAME}"
ALTER SCHEMA public OWNER TO ${DBROLE};
ALTER TABLE public.spatial_ref_sys OWNER TO ${DBROLE};
ALTER SCHEMA topology OWNER TO ${DBROLE};
ALTER TABLE topology.topology OWNER TO ${DBROLE};
ALTER TABLE topology.layer OWNER TO ${DBROLE};
ALTER SEQUENCE topology.topology_id_seq OWNER TO ${DBROLE};
EOF
	cat "${SCHEMAFILE}" | psql -U "${DBROLE}" "${DBNAME}"
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
Usage: $0 <command> <dbname> <dbrole> <schema_file>

commands:
	dump: 		make a backup of the database
	drop: 		drop both user and database
	create: 	create user and database, add postgis extensions and create the schema
	populate: fill the database with data
	cleanup: 	remove data from the database
	all: 			dump && cleanup && create && populate
EOF
}

if [ $# -lt 4 ]; then
	usage
	exit 1
fi

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
