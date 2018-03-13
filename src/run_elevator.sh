# Start ElevatorServer
pkill ElevatorServer
~/.cargo/bin/ElevatorServer & disown

# Compile and move *.beam files to ebin
cd ~/Desktop/project-ssm/src
erlc *.erl;
mkdir -p ~/Desktop/project-ssm/ebin
mv *.beam ../ebin/;

# Run init_node:start()
cd ~/Desktop/project-ssm/ebin/
erl -s init_node start

# Make shure to kill ElevatorServer
pkill ElevatorServer