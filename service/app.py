from wsgiref.simple_server import make_server

from config.logger import logger

import falcon
from resources.resources import *
from middleware.context import ContextMiddleware

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
    app_port = sys.argv[1]
    with make_server('', int(app_port), api_app) as httpd:
        logger.debug(f'Serving on port {app_port}...')
        httpd.serve_forever()
