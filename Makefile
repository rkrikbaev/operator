build:
	docker build -t rkrikbaev/fp-operator:v1.0 .
run:
	docker run -it --rm -v /Users/rustamkrikbayev/operator/app:/application \
						-v /var/run/docker.sock:/var/run/docker.sock \
						-p 8015:8015 \
						--network=service_network \
						rkrikbaev/fp-operator:v1.0