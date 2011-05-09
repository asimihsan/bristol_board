# ----------------------------------------------------------------------
# Copyright (c) 2011 Asim Ihsan (asim dot ihsan at gmail dot com)
# Distributed under the MIT/X11 software license, see 
# http://www.opensource.org/licenses/mit-license.php.
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
# File: bristol_board/src/infrastructure/bootstrap.py
#
# Take a fresh Ubuntu install and set it up to support 
# Bristol board.  This will largely be installing source packages,
# setting up files, creating services.
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
#   Imports.
# ----------------------------------------------------------------------
from __future__ import with_statement
import os
import sys
import logging
import collections

from boto.ec2.connection import EC2Connection
from fabric.api import settings
from fabric.contrib.console import confirm
from fabric.operations import sudo, run
from fabric.contrib.files import append, uncomment, sed
from fabric.context_managers import cd
import fabric.network
import colorama
from colorama import Fore, Back, Style

# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
#   Logging.
# ----------------------------------------------------------------------
APP_NAME = 'bootstrap'
logger = logging.getLogger(APP_NAME)
logger.setLevel(logging.DEBUG)
ch = logging.StreamHandler()
ch.setLevel(logging.DEBUG)
formatter = logging.Formatter('%(asctime)s - %(name)s - %(message)s')
ch.setFormatter(formatter)
logger.addHandler(ch)
logger = logging.getLogger(APP_NAME)
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
#   Constants to change.
# ----------------------------------------------------------------------
REMOTE_HOST = "192.168.0.193"
REMOTE_USERNAME = "ubuntu"

# Set either REMOTE_PASSWORD or KEY_FILENAME, where the latter is a patch
# to an authorized RSA keyfile.  KEY_FILENAME is preferred.  Set whatever
# you don't want to use to None.
REMOTE_PASSWORD = "password" 
KEY_FILENAME = None
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
#   Constants to leave alone.
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------

def install_bare_essentials():
    logger = logging.getLogger("%s.install_bare_essentials" % (APP_NAME, ))
    logger.debug("entry.")
    uncomment(filename = r"/etc/apt/sources.list",
              regex = "deb http:\/\/archive.canonical.com\/ubuntu.*partner",
              use_sudo = True)
    uncomment(filename = r"/etc/apt/sources.list",
              regex = "deb-src http:\/\/archive.canonical.com\/ubuntu.*partner",
              use_sudo = True)    
    sudo("apt-get update")
    sudo("yes yes | apt-get upgrade")
    sudo("yes yes | apt-get install git mercurial build-essential unzip python-software-properties ruby curl python-dev htop")

def install_erlang():
    logger = logging.getLogger("%s.install_erlang" % (APP_NAME, ))
    logger.debug("entry.")
    sudo("yes yes | apt-get install curl m4 flex xsltproc fop libncurses5-dev unixodbc-dev")
    with cd("~"):        
        run("wget http://www.erlang.org/download/otp_src_R14B02.tar.gz")
        run("tar xvf otp_src_R14B02.tar.gz")
    with cd(r"~/otp_src_R14B02"):
        run("./configure")
        run("make")
        sudo("make install")
    with cd("~"):
        run("rm -rf otp_src_*")
    
def install_postgresql():
    logger = logging.getLogger("%s.install_postgresql" % (APP_NAME, ))
    logger.debug("entry.")
    sudo("yes yes | apt-get install postgresql postgresql-contrib")    
    
def setup_python():
    logger = logging.getLogger("%s.setup_python" % (APP_NAME, ))
    logger.debug("entry.")
    with cd("~"):        
        run("curl -O http://python-distribute.org/distribute_setup.py")    
        sudo("python distribute_setup.py")
        run("rm -f distribute*")
    sudo("easy_install -U httplib2 boto fabric colorama twisted pycrypto")        
    sudo("rm -rf /tmp/tmp*")
    
def setup_ntp():
    logger = logging.getLogger("%s.setup_ntp" % (APP_NAME, ))
    logger.debug("entry.")
    sudo("yes yes | apt-get install ntp")
    sudo("cp /etc/ntp.conf /etc/ntp.conf.backup")
    sed(filename = "/etc/ntp.conf",
        before = "server.*org",
        after = "",
        use_sudo = True)        
    sed(filename = "/etc/ntp.conf",
        before = "server.*com",
        after = "",
        use_sudo = True)        
    for server_line in ["server 0.uk.pool.ntp.org",
                        "server 1.uk.pool.ntp.org",
                        "server 2.uk.pool.ntp.org",
                        "server 3.uk.pool.ntp.org"]:
        append(filename = "/etc/ntp.conf",
               text = server_line,
               use_sudo = True)
    sudo("service ntp restart")
    
def install_haproxy():
    logger = logging.getLogger("%s.install_haproxy" % (APP_NAME, ))
    logger.debug("entry.")
    with cd("~"):        
        run("wget http://haproxy.1wt.eu/download/1.4/src/haproxy-1.4.15.tar.gz")
        run("tar xvf haproxy-1.4.15.tar.gz")
    with cd(r"~/haproxy-1.4.15"):
        run("make TARGET=linux26")
        sudo("make install")
    with cd("~"):
        run("rm -rf haproxy*")
    
def harden():
    logger = logging.getLogger("%s.harden" % (APP_NAME, ))
    logger.debug("entry.")    
    sed(filename = "/etc/ssh/sshd_config",
        before = "PermitRootLogin yes",
        after = "PermitRootLogin no",
        use_sudo = True)                
    sudo("yes yes | apt-get install ufw")
    sudo("ufw allow ssh")
    sudo("ufw allow 80/tcp")
    sudo("ufw allow 443/tcp")
    sudo("ufw default deny")
    sudo("ufw limit OpenSSH")
    sudo("yes yes | ufw enable")
    sudo("yes yes | apt-get install denyhosts")    
    sudo("cp /etc/denyhosts.conf /etc/denyhosts.conf.backup")
    sed(filename = "/etc/denyhosts.conf",
        before = "AGE_RESET_VALID=5d",
        after = "AGE_RESET_VALID=10m",
        use_sudo = True)
    
def call_fabric_function(function, *args, **kwds):
    if KEY_FILENAME is not None:
        with settings(host_string=REMOTE_HOST,
                      user=REMOTE_USERNAME,
                      key_filename=KEY_FILENAME):            
            function(*args, **kwds)
    else:
        assert(REMOTE_PASSWORD is not None)
        with settings(host_string=REMOTE_HOST,
                      user=REMOTE_USERNAME,
                      password=REMOTE_PASSWORD):    
            function(*args, **kwds)        
    
def main():
    logger.info("Starting main.")
    colorama.init()
    
    # ------------------------------------------------------------------
    #   What functions to call.  Uncomment / comment as you wish.
    #   In theory only call these functions once per installation,
    #   but safe, albeit wasteful, to run again.
    # ------------------------------------------------------------------
    functions_to_call = [ \
                         # install_bare_essentials,
                         # install_erlang,
                         install_haproxy
                         # setup_python,
                         # setup_ntp,
                         # install_postgresql,
                         # harden
                        ]
    # ------------------------------------------------------------------
    try:        
        for function_to_call in functions_to_call:
            call_fabric_function(function_to_call)        
    finally:
        logger.info("Cleanup.")
        logger.debug("Disconnect all SSH sessions...")
        fabric.network.disconnect_all()
    
if __name__ == "__main__":
    main()


