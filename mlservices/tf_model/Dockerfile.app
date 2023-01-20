# Use an official Python runtime as a parent image
FROM rkrikbaev/tf-env:latest

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

COPY ./api.py .
COPY ./model.py .
COPY ./utils.py .

ENV LOG_LEVEL=DEBUG
ENV TIMEOUT=1000

ENTRYPOINT [ "gunicorn", "-b", "0.0.0.0:8005", "api:api", "--timeout", "1000", "--log-level", "debug" ]