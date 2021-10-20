# Use an official Python runtime as a parent image
FROM python:3.9
# Set the working directory to /app
COPY ./requirements.txt .
# Install any needed packages specified in requirements.txt
#RUN pip install -r requirements.txt
RUN pip install docker==5.0.0 \
    pip install gunicorn \
    pip install python-dotenv
RUN pip install falcon==3.0.1
RUN pip install numpy==1.20.2
RUN pip install pandas==1.2.4
RUN pip install scikit-learn==0.24.2
RUN pip install scipy==1.6.3
RUN pip install sklearn


RUN mkdir service

WORKDIR /service

RUN mkdir logs

# Copy the current directory contents into the container at /app
COPY . .

# Make port 8000 available to the world outside this container
EXPOSE 8000
# Run app.py when the container launches
CMD ["python", "run.py"]
CMD ["gunicorn", "--reload", "-b", "0.0.0.0:8000", "app.app"