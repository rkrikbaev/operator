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
<<<<<<< HEAD
    app_port = sys.argv[1]
    with make_server('', int(app_port), api_app) as httpd:
        logger.debug(f'Serving on port {app_port}...')
        httpd.serve_forever()
=======

    app_port = 8001 if sys.argv[1] == None else sys.argv[1]

    with make_server("", app_port, api_app) as httpd:
        logger.debug(f'Serving on port {app_port}...')
        httpd.serve_forever()
>>>>>>> b5965ae32b2bb4185db5ad23f0b3bca3c2a03603
