from app import api
# import os
import logging
from wsgiref.simple_server import make_server

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)
# port = os.getenv('APP_PORT', default=8015)

if __name__ == "__main__":

    with make_server("", 8015, api) as httpd:
        logger.debug("Listening Port 8005...")
        # Serve until process is killed
        httpd.serve_forever()