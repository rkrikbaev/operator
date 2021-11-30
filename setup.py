#!/usr/bin/env python

from setuptools import setup, find_packages

desc = ''
with open('README.rst') as f:
    desc = f.read()

setup(
    name='operator',
    version='1.0',
    description=('Operator for docketed python services '),
    long_description=desc,
    url='https://github.com/rkrikbaev/operator',
    author='Rustam Krikbaev',
    author_email='rkrikbaev@gmail.com',
    license='Apache v2',
    classifiers=[
        'Development Status :: 3 - Alpha',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: Apache Software License',
        'Programming Language :: Python :: 3.8',
        'Programming Language :: Python :: 3.9',
    ],

    keywords='',
    packages=find_packages(exclude=['contrib', 'docs', 'test*']),
    install_requires=[
        'falcon>=1.4.1',
        'docopt>=0.6.2',
        'jsonschema>=2.5.1'
    ],
    package_data={},
    data_files=[],
    entry_points={
        'console_scripts': [
            'operator = server.__main__:main'
        ],
    },
)
