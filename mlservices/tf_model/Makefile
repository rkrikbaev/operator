run:
	docker run --rm -v ~/operator/mlservices/tf_lstm_model/mlruns:/application/mlruns -p 8005:8005 -e TRACKING_SERVER='http://138.68.70.41:5000' -e LOG_LEVEL=DEBUG rkrikbaev/tf_lstm:latest