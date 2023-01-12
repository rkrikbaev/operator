# Use an official Python runtime as a parent image
FROM python:3.9

LABEL Auth: Krikbayev Rustam 
LABEL Email: "rkrikbaev@gmail.com"
ENV REFRESHED_AT 2023-01-12

# Install any needed packages specified in requirements.txt
COPY ./requirements.txt .
RUN python -m pip install --upgrade pip
RUN pip install -r requirements.txt 

# Copy the current directory contents into the container at /app
RUN mkdir application
WORKDIR /application
RUN mkdir logs

COPY ./api.py .
COPY ./model.py .
COPY ./utils.py .

ENTRYPOINT ["gunicorn", "-b", "0.0.0.0:8005", "api:api"]