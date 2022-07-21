# Use an official Python runtime as a parent image
FROM python:3.9

LABEL Auth: Krikbayev Rustam 
LABEL Email: "rkrikbaev@gmail.com"
ENV REFRESHED_AT 2020-10-20

COPY ./requirements.txt .

# Install any needed packages specified in requirements.txt
RUN pip install -r requirements.txt 

RUN mkdir app
WORKDIR /app

RUN mkdir logs

# Copy the current directory contents into the container at /app
COPY ./app .

# Make port 8015 available to the world outside this container
EXPOSE 8015

CMD ["sh", "entry_point.sh"]
