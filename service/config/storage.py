'''
pickleDB is lightweight, fast, and simple database based on the json module. And it's BSD licensed!
'''

import pickledb
from config.logger import logger

try:
    storage = pickledb.load('project.db', False)
except Exception as e:
    logger.error('error load db')