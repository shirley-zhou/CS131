#!/usr/bin/python
from twisted.protocols.basic import LineReceiver
from twisted.internet.protocol import ServerFactory, ClientFactory
from twisted.internet import reactor
from twisted.web.client import getPage
#from twisted.python.log import startLogging
import time
import urllib2
import json
import conf
import sys
import re

#startLogging(sys.stdout)

ALL_NEIGHBORS = {"Alford": ["Hamilton", "Welsh"], "Ball": ["Holiday", "Welsh"], "Hamilton": ["Holiday"], "Holiday": ["Ball", "Hamilton"], "Welsh": ["Alford", "Ball"]}

#maintain server data across connections
class Server(ServerFactory):
  def __init__(self, name):
    self.file = name + ".log"
    self.clientInfo = {}
    self.serverID = name
	
  #def buildProtocol(self, addr):
  #  return ServerProtocol() #alt: use protocol = ServerProtocol, default buildProtocol will instantiate

  def startFactory(self):
    self.fp = open(self.file, 'a') #a for append

  def log(self, line):
    self.fp.write(line + '\n')

  def stopFactory(self):
    self.fp.close()
	
#NOTE: some of this code is modelled on chatserver.py sample from Twisted docs
class ServerProtocol(LineReceiver): #created for each new connection, non persistent

  #def __init__(self, factory):
   # self.factory = factory #handled auto, by factory.protocol = ServerProtocol
	
  def connectionMade(self):
    self.factory.log("connection made")

  def connectionLost(self, reason):
    self.factory.log("connection closed")
	
  def propagate(self, line):
    for neighbor in ALL_NEIGHBORS[self.factory.serverID]:
      self.factory.log("connect with neighbor: " + neighbor)
      #based on example from https://stackoverflow.com/questions/3275004/how-to-write-a-twisted-server-that-is-also-a-client
      factory = SomeClientFactory(line)
      factory.protocol = SomeClientProtocol
      reactor.connectTCP("localhost", conf.PORT_NUM[neighbor], factory)
      self.factory.log("propagate: " + line)

  def handlejson(self, results, infoamt, orig_response):
    self.factory.log("returned json: " + results)
    data = json.loads(results)
    data["results"] = data["results"][0:infoamt]
    final = json.dumps(data)
    final = re.sub("\\n\\n", "\\n", final)
    response = orig_response + "\n" + final + "\n\n" #todo: clarify
    self.transport.write(response + '\n')
    self.factory.log("response to client: " + response)
    

  def lineReceived(self, line):
    self.factory.log("received: " + line)
    if len(line) == 0:
      response = '? ' + line
      self.transport.write(response + '\n')
      self.factory.log("error IAMAT input format, response: " + response)
      return

    words = line.split()

    #LOCATION ANNOUNCEMENT FROM CLIENT
    if words[0] == "IAMAT":
      if len(words) != 4:
        response = '? ' + line
	self.transport.write(response + '\n')
        self.factory.log("error IAMAT input format, response: " + response)
        return
      try:
        clientID = words[1]
        location = words[2] #latitude and longitude in ISO6709
        timestamp = float(words[3]) #time in POSIX time
        timediff = time.time() - timestamp
        if timediff >= 0:
          timediff = '+' + str(timediff)
      except:
        response = '? ' + line
	self.transport.write(response + '\n')
        self.factory.log("error IAMAT input format, response: " + response)
        return
	  
      if clientID not in self.factory.clientInfo or timestamp > self.factory.clientInfo[clientID][1]:
        self.factory.log("add client info")
        response = "AT "+ self.factory.serverID + " " + timediff + " " + ' '.join(words[1 :])
        self.factory.clientInfo[clientID] = [location, timestamp, response] #list or tuple?
        self.propagate(response)
      else:
        self.factory.log("do not update client info")
        return
	  
    #LOCATION QUERY FROM CLIENT
    elif words[0] == "WHATSAT":
      if len(words) != 4:
        response = '? ' + line
	self.transport.write(response + '\n')
        self.factory.log("error WHATSAT input format, response: " + response)
        return
      try:
        clientID = words[1]
        radius = words[2]
        infoamt = int(words[3])
      except:
        response = '? ' + line
	self.transport.write(response + '\n')
        self.factory.log("error WHATSAT input format, response: " + response)
        return
		
      if clientID not in self.factory.clientInfo or int(radius) < 0 or int(radius) > 50 or infoamt <= 0 or infoamt > 20: #ERROR
        response = "? " + line
        self.transport.write(response + '\n')
        self.factory.log("requesting location of unknown client, response: " + response)
        return

      location = self.factory.clientInfo[clientID][0].replace('-', ' -').replace('+', ' +')
      latlong = location.split()
      location = latlong[0] + ',' + latlong[1]
      timestamp = self.factory.clientInfo[clientID][1]
      orig_response = self.factory.clientInfo[clientID][2]
        
      places = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?" + "location=" + location + "&radius=" + radius + "&key=" + conf.API_KEY
      #from https://stackoverflow.com/questions/2720484/use-twisteds-getpage-as-urlopen
      #also from https://stackoverflow.com/questions/7891062/how-to-pass-extra-arguments-to-callback-register-functions-with-twisted-python-a
      d = getPage(places) #TODO: fix this use getPage, urllib2.urlopen
      d.addCallback(callback = lambda x:(self.handlejson(x, infoamt, orig_response)))
      self.factory.log("querying Places API: " + places)
      return
	
    #RECEIVED RELAYED RESPONSE FROM ANOTHER SERVER
    elif words[0] == "AT":
      if len(words) != 6:
        response = '? ' + line
        self.transport.write(response + '\n')
        self.factory.log("error AT input format, response: " + response)
        return

      try:
        clientID = words[3]
        location = words[4] #latitude and longitude in ISO6709
        timestamp = float(words[5]) #time in POSIX time 
      except:
        response = '? ' + line
        self.transport.write(response + '\n')
        self.factory.log("error AT input format, response: " + response)
        return

      if clientID not in self.factory.clientInfo or timestamp > self.factory.clientInfo[clientID][1]:
        self.factory.log("update/add client info")
        self.factory.clientInfo[clientID] = [location, timestamp, line] #list or tuple?
        self.propagate(line)
      else:
        self.factory.log("do not update client info")
      return
		
    else:
      response = "? " + line
	  
    self.transport.write(response + '\n')
    self.factory.log("response to client: " + response)

class SomeClientFactory(ClientFactory):
  def __init__(self, msg):
    self.msg = msg
	
class SomeClientProtocol(LineReceiver):
  def connectionMade(self):
    self.sendLine(self.factory.msg)

def main():
  if len(sys.argv) != 2:
    print "takes 1 arg: server name"

  #check sys.argv for server name, then set up server neighbor list in switch
  server = sys.argv[1]
  
  if server not in conf.PORT_NUM:
    print "invalid server name arg 1"
  
  factory = Server(server)
  factory.protocol = ServerProtocol
	
  reactor.listenTCP(conf.PORT_NUM[server], factory)
  reactor.run()

# this only runs if the module was *not* imported
if __name__ == '__main__':
    main()