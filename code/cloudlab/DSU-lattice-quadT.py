"""Racket profile for running deep/shallow/untyped lattices. (Focus on quadT.)

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
myhw="m510"
mytar="https://cs.brown.edu/people/bgreenma/rds-cloudlab.tar.gz"
myhome="/users/ben_g"
myinstall="%s/rds-cloudlab/install.sh" % myhome

for nodenum in range(115):
    nn = request.RawPC("n%s" % nodenum)
    nn.hardware_type = myhw
    nn.addService(pg.Install(mytar, myhome))
    nn.setFailureAction('nonfatal')
    #nn.addService(pg.Execute(myshell, myinstall))


# Print the generated rspec
pc.printRequestRSpec(request)
