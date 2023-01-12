from api import api
import os

from utils import get_logger
logger = get_logger(__name__, loglevel='DEBUG')

# --------- For local debugging/test only ----------
from wsgiref.simple_server import make_server

if __name__ == "__main__":

    port = 8005
    with make_server("", int(port), api) as httpd:
        logger.debug(f"Listening Port {port}...")
        # Serve until process is killed
        httpd.serve_forever()
# --------- For local debugging/test only ----------

