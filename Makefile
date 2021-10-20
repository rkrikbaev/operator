build:
	docker build -t rkrikbaev/servie-operator:v1.1.4 .
run:
	docker run -it --rm -v ~/operator/service:/service \
						-v /var/run/docker.sock:/var/run/docker.sock \
						-p 8005:8005 \
						rkrikbaev/servie-operator:v1.1.4