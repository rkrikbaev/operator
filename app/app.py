import falcon
from resources.model import Predict, Health
import os

from middleware.helper import logger
logger = logger(__name__)

api = falcon.App()

api.add_route('/predict/{task_id}', Predict())
api.add_route("/health", Health())

logger.debug('Application started.')