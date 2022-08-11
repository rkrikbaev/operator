
MYSQL_DATABASE='db'
MYSQL_USER='mlflow'
MYSQL_PASSWORD='Pa55word!'
MYSQL_ROOT_PASSWORD=root

build:
	docker build -t rkrikbaev/fp-operator:v1.0 .
run:
	docker run -it --rm -v /Users/rustamkrikbayev/operator/app:/application \
						-v /var/run/docker.sock:/var/run/docker.sock \
						-p 8015:8015 \
						--network=service_network \
						rkrikbaev/fp-operator:v1.0

mlflow:
        mlflow server \
		—backend-store-uri mysql+pymysql://mlflow:strongpassword@db:3306/db \
		—default-artifact-root /root/model-training/jupyter/project/mlflow/ \
		—host 0.0.0.0
jupyter:
        docker run --rm -dit -p 8888:8888 \
		-v /root/model-training/jupyter/project:/home/project \
		prophet-notebook:latest \
		jupyter notebook --allow-root --no-browser --ip='0.0.0.0' --port=8888
mysql:
        docker run --rm -it -p 3306:3306 \
        -e MYSQL_DATABASE=db \
        -e MYSQL_USER=mlflow \
        -e MYSQL_PASSWORD=AAA123! \
        -v dbdata:/var/lib/mysql \
        -v /usr/local/db_mlflow/conf.d:/etc/mysql/conf.d \
        --name mlflow_db mysql/mysql-server:5.7.28
stop:
        docker stop mlflow_db
        docker volume rm dbdata