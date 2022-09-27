"""Racket profile for running deep/shallow/untyped lattices.

Instructions:
TBD"""

#
# NOTE: This code was machine converted. An actual human would not
#       write code like this!
#

# Import the Portal object.
import geni.portal as portal
# Import the ProtoGENI library.
import geni.rspec.pg as pg
# Import the Emulab specific extensions.
import geni.rspec.emulab as emulab

# Create a portal object,
pc = portal.Context()

# Create a Request object to start building the RSpec.
request = pc.makeRequestRSpec()

myshell="bash"
myhw="c220g1"
mytar="https://cs.brown.edu/people/bgreenma/rds-cloudlab.tar.gz"
myhome="/users/ben_g"
myinstall="%s/rds-cloudlab/install.sh" % myhome

for nodenum in range(0, 2):
    nn = request.RawPC("node-%s" % nodenum)
    nn.hardware_type = myhw
    nn.addService(pg.Install(mytar, myhome))
    nn.addService(pg.Execute(myshell, myinstall))


# Print the generated rspec
pc.printRequestRSpec(request)
