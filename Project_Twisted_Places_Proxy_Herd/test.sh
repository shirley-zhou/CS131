(
echo -e "IAMAT kiwi.cs.ucla.edu +34.068930-118.445127 1479413884.392014450";
sleep 2;
) | telnet localhost 11980

sleep 2

(
echo -e "WHATSAT kiwi.cs.ucla.edu 10 5";
sleep 2;
) | telnet localhost 11981

(
echo -e "IAMAT kiwi.cs.ucla.edu +34.068930-118.445127 1479413884.0";
sleep 2;
) | telnet localhost 11983

sleep 3

pkill -f 'python2 server.py Holiday'

(
echo -e "IAMAT kiwi.cs.ucla.edu +38.0-110.0 1479413890.392014450";
sleep 2;
) | telnet localhost 11982

sleep 3

(
echo -e "WHATSAT kiwi.cs.ucla.edu 10 5";
sleep 2;
) | telnet localhost 11982

(
echo -e "WHATSAT kiwi.cs.ucla.edu 10 5";
sleep 2;
) | telnet localhost 11984

sleep 1

pkill -f python2