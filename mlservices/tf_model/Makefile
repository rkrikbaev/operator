run:
	docker run -it --rm -v /opt/operator/mlservices/tf_model:/application -v /opt/modelregistry/mlruns:/opt/mlruns -p 8005:8005 -e LOG_LEVEL=DEBUG -e TRACKING_SERVER='http://138.68.70.41:5000' -e TIMEOUT=1000 rkrikbaev/tf-model:latest
build:
	docker build -t rkrikbaev/tf-model:latest . -f Dockerfile.app 