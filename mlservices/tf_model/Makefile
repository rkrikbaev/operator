run:
	docker run --rm -v /opt/operator/mlservices/tf_model:/application -p 8005:8005 -e LOG_LEVEL=DEBUG -e TRACKING_SERVER='http://138.68.70.41:5000' rkrikbaev/tf-model:latest