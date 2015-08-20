#!/bin/bash

SCRIPT_PATH="${BASH_SOURCE[0]}";
if ([ -h "${SCRIPT_PATH}" ]) then
  while([ -h "${SCRIPT_PATH}" ]) do SCRIPT_PATH=`readlink "${SCRIPT_PATH}"`; done
fi
pushd . > /dev/null
cd `dirname ${SCRIPT_PATH}` > /dev/null
SCRIPT_PATH=`pwd`;
popd  > /dev/null

FULL_PATH="${SCRIPT_PATH}/$(basename ${BASH_SOURCE[0]})"

sourcedb="refresh_dev"
nofetch="false"

if [[ -n $1 ]]; then
  sourcedb=$1

  if [[ -n $2 ]]; then
    nofetch=$2
  fi
fi

#echo "Getting data from $sourcedb DB"

die() {
  echo $1
  exit 1
}

cd "$(dirname "$0")"

if [[ -e $HOME/.pgpass ]]; then
  read sourcehost sourceport sourcedbName sourceusername \
    <<<$(perl -ne "if(\$_ !~ /#/ && \$_ =~ /:$sourcedb:/) { s/:/ /g; s/ [^ ]*\$//; print; }" $HOME/.pgpass)

  if [[ -z $sourcehost || -z $sourceport || -z $sourcedbName || -z $sourceusername ]]; then
    die "Missing source parameters from the pgpass file"
  else # requires password in pgpass file

    fetch() {
      if [[ $nofetch != "nofetch" ]]; then
        #echo "Fetching $2"
        psql -h $sourcehost -d $sourcedbName -U $sourceusername -p $sourceport \
          -c "$1" > "$2"
      fi
    }


    # TODO: need to get logo, make it into media, and add to vfile as cover
    # Also get description and add as vfile caption
    fetch "COPY (
            SELECT slug FROM tv_shows
          ) TO STDOUT WITH CSV;" /tmp/tvSlugs.csv \
      && awk -F "\"*,\"*" '{print "https://www.vfiles.com/tv/" $1}' /tmp/tvSlugs.csv


    fetch "COPY (
            SELECT slug FROM contests
          ) TO STDOUT WITH CSV;" /tmp/contestSlugs.csv \
      && awk -F "\"*,\"*" '{print "https://www.vfiles.com/contests/" $1}' /tmp/contestSlugs.csv


    fetch "COPY (
            SELECT   p.username
              FROM vendors v
              JOIN people p ON p.id = v.person_id
          ) TO STDOUT WITH CSV;" /tmp/vendorSlugs.csv \
      && awk -F "\"*,\"*" '{print "https://www.vfiles.com/profile/" $1 "/shop"}' /tmp/vendorSlugs.csv



    fetch "COPY (
            SELECT   slug
              FROM shop_collections
          ) TO STDOUT WITH CSV;" /tmp/shopCollectionSlugs.csv \
      && awk -F "\"*,\"*" '{print "https://www.vfiles.com/shop/collections/" $1 "/all"}' /tmp/shopCollectionSlugs.csv


    fetch "COPY (SELECT username FROM people) TO STDOUT WITH CSV;" /tmp/userIds.csv \
      && awk -F "\"*,\"*" '{print "https://www.vfiles.com/profile/" $1}' /tmp/userIds.csv \
      && awk -F "\"*,\"*" '{print "https://www.vfiles.com/profile/" $1 "/vfiles"}' /tmp/userIds.csv \
      && awk -F "\"*,\"*" '{print "https://www.vfiles.com/profile/" $1 "/mentions"}' /tmp/userIds.csv \
      && awk -F "\"*,\"*" '{print "https://www.vfiles.com/profile/" $1 "/likes"}' /tmp/userIds.csv #\
      #&& awk -F "\"*,\"*" '{print "https://www.vfiles.com/profile/" $1 "/followers"}' /tmp/userIds.csv \
      #&& awk -F "\"*,\"*" '{print "https://www.vfiles.com/profile/" $1 "/followees"}' /tmp/userIds.csv

    fetch "COPY (SELECT id FROM vfiles) TO STDOUT WITH CSV;" /tmp/vfileIds.csv \
      && awk -F "\"*,\"*" '{print "https://www.vfiles.com/vfiles/" $1}' /tmp/vfileIds.csv

    fetch "COPY (SELECT media_id, vfile_id FROM media_vfiles) TO STDOUT WITH CSV;" /tmp/mediaVfileIds.csv \
      && awk -F "\"*,\"*" '{print "https://www.vfiles.com/vfiles/" $2 "/media/" $1}' /tmp/mediaVfileIds.csv


  fi
else
  die "pgpass file not present"
fi
