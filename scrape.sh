#! /bin/bash

curl 'https://api-public.stoerungsauskunft.de/api/v1/public/outages?SectorType=1' -H 'Authorization: Basic ZnJvbnRlbmQ6ZnJvbnRlbmQ=' | jq .[]
