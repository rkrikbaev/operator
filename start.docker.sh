#!/bin/bash


## start as docker container

docker run --name operator --restart always -p 8015:8015 \
-v /var/run/docker.sock:/var/run/docker.sock \
-v ~/services/operator-service/service:/application \
--network service_network fpcloud/operator-service-amd64:latest
