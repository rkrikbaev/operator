FROM python:3.8.3

USER root
# ARG PROPHET_VER="1.0"

RUN apt-get update
RUN apt-get -y install \
    wget \
    git \
    libc-dev

RUN pip install --upgrade pip

RUN pip install ipython==7.5.0
RUN pip install prophet==1.1
RUN pip install jupyter
RUN pip install mlflow==1.24.0

WORKDIR /home/project

RUN jupyter notebook --allow-root --no-browser --ip='0.0.0.0' --port=8888