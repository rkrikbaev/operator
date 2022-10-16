FROM python:3.8

USER root

RUN apt-get update
RUN apt-get -y install \
    wget \
    git \
    libc-dev

RUN pip install --upgrade pip \
    && ipython==7.5.0 \
    && prophet==1.1 \
    && mlflow==1.24.0

RUN mkdir /etc/jupyterhub
    