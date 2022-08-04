
# Operator
## Project structure


src
    -> logs
    -> middleware
        __init__.py
        context.py
    -> resources
        Predict.py
        Test.py
    -> schemas
        __init__.py
        request.json
    __init__.py
    app.py
    logger.py
    logic.py
    prophet.py
    service.py
    tfx.api.py
test
    __init__.py
    test_app.py
.dockerignore
.env
.docker-compose.yaml
Dockerfile
README.md
requirements.txt



## start as docker container

docker run --name operator --restart always -p 8015:8015 -v /var/run/docker.sock:/var/run/docker.sock -v ~/services/operator-service/service:/application --network service_network fpcloud/operator-service-amd64:latest