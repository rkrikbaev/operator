run:
	docker run --rm -v /opt/operator/mlservices/tf_model/mlruns:/application/mlruns -p 8005:8005 -e TRACKING_SERVER='http://138.68.70.41:5000' -e LOG_LEVEL=DEBUG rkrikbaev/tf-model:latest