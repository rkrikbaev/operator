from wsgiref.simple_server import make_server

from docopt import docopt
from config.logger import logger


import falcon
from resources.resources import *
from middleware.context import ContextMiddleware

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

    docopt(__doc__)

    api_app = Server()

    with make_server("", 8001, api_app) as httpd:
        logger.debug("Serving on port 8001...")
        httpd.serve_forever()
