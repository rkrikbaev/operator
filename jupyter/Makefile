run:
        docker run --rm -it \
		--name jupyter-notebook \
		-p 8888:8888 \
		-v ${shell pwd}/project:/home/project \
		prophet-notebook:latest \
		jupyter notebook --allow-root --no-browser --ip='0.0.0.0' --port=8888