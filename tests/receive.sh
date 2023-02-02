#!/bin/bash
HOST=$1
PORT=$2
ID=$3

curl --location --request POST 'http://$HOST:$PORT/predict' \
--header 'Content-Type: application/json' \
--data-raw '{ 
    "task_id": "'$ID'", 
    "model_point": "almaty4", 
    "model_type": "tf_model", 
    "model_config": { "window":96 }, 
    "model_uri": { 
        "experiment_id": "571625146127493926", 
        "run_id": "6776c0c6dda044bd8f120d2875463883" 
        }, 
    "metadata":null, 
    "period": null, 
    "dataset":null 
    }'