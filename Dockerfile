# Use an official Python runtime as a parent image
FROM python:3.9

LABEL Auth: Krikbayev Rustam 
LABEL Email: "rkrikbaev@gmail.com"
ENV REFRESHED_AT 2020-10-20


# Install any needed packages specified in requirements.txt
RUN pip install docker==5.0.0 && \
    pip install python-dotenv && \
    pip install falcon==3.0.1 && \
    pip install numpy==1.20.2 && \
    pip install pandas==1.2.4
    # pip install scikit-learn==0.24.2 && \
    # pip install scipy==1.6.3 && \
    # pip install sklearn

RUN mkdir application

WORKDIR /application

RUN mkdir logs

# Copy the current directory contents into the container at /app
COPY ./service .

# Make port 8001 available to the world outside this container
EXPOSE 8001

# Run app.py when the container launches
CMD ["python", "app.py", 8001]
