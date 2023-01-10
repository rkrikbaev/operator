test:
	docker run --rm -it -v ~/operator/mlservices/tf_lstm_model:/opt/service -p 8055:8005 -e LOG_LEVEL=DEBUG rkrikbaev/tf-env:latest /opt/service python wsgi.py