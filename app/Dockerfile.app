# Use an official Python runtime as a parent image
FROM python:3.9

LABEL Auth: Krikbayev Rustam 
LABEL Email: "rkrikbaev@gmail.com"
ENV REFRESHED_AT 2020-10-20

# Install any needed packages specified in requirements.txt
COPY ./requirements.txt .
RUN python -m pip install --upgrade pip
RUN pip install -r requirements.txt 

# Copy the current directory contents into the container at /app
RUN mkdir application
WORKDIR /application

COPY ./api.py .
COPY ./service.py .
COPY ./tasks.py .
COPY ./utils.py .