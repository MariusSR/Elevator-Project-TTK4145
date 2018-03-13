#Start ElevatorServer
pkill ElevatorServer
gnome-terminal -e /home/student/.cargo/bin/ElevatorServer

#Compile and run init_node:start()
cd Desktop/project-ssm/src
erlc *.erl && erl -s init_node start
