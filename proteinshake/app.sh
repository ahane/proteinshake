# -XMx4g = 4 GB of memory, -ea = enable assertions
#java -Xmx4g -ea -Dfile.encoding=UTF-8 -classpath $cp
#!/bin/bash

#scala -cp target/classes eu.hanefeld.ba.App
#the above doesnt seem to know where to find the factorie dependencies
#it would be nice to run it with java like in the factorie fac.sh
mvn scala:run -DmainClass=eu.hanefeld.ba.App


