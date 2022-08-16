FROM jupyterhub/jupyterhub:latest

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
RUN cp jupyterhub_config.py /etc/jupyterhub/

WORKDIR /home/project

CMD ["jupyterhub", "-f", "/etc/jupyterhub/jupyterhub_config.py"]