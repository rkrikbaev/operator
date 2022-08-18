docker:
	docker run --rm -it \
	--name jupyter-notebook \
	-p 8888:8888 \
	-v ${shell pwd}/project:/home/project \
	prophet-notebook:latest \
	jupyter notebook --allow-root --no-browser --ip='0.0.0.0' --port=8888

install:
	python3 -m venv .venv
	source .venv/bin/activate
	curl -L https://tljh.jupyter.org/bootstrap.py \
    | sudo python - \
    --admin admin:admin \
	--show-progress-page \
	--user-requirements-txt-url https://raw.githubusercontent.com/rkrikbaev/operator/dev/jupyter/requirements.txt