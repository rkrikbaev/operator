from wsgiref.simple_server import make_server

from falcon import app

from config.logger import logger

import falcon
from resources.resources import *
from middleware.context import ContextMiddleware
import sys

import sys

class Server(falcon.API):

    def __init__(self):

        super(Server, self).__init__(
            middleware=[ContextMiddleware()]
        )

        # Create resources
        action = Prediction()

        # Build routes
        self.add_route("/action", action)
        # self.add_route("/test", test)

if __name__ == "__main__":

    api_app = Server()

    app_port = os.getenv('APP_PORT', default=8015)
    
    try:

        with make_server("", app_port, api_app) as httpd:
            logger.debug(f'Serving on port {app_port}...')
            httpd.serve_forever()
    
    except Exception as exc:
        logger.error(f'Address already in use {exc}')
