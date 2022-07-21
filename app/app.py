import falcon
from resources.model import Predict, Health

import logging, sys
logging.basicConfig(stream=sys.stdout, level=logging.DEBUG,
                    format=f"%(asctime)s - [%(levelname)s] - %(name)s - (%(filename)s).%(funcName)s(%(lineno)d) - %(message)s")
logger = logging.getLogger(__name__)

api = falcon.App()

api.add_route('/predict/{task_id}', Predict())
api.add_route("/health", Health())
logger.debug('service started')